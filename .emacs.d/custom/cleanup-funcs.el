;;; clean-up-funcs.el --- clean up c function args
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Monday, March 28, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-03-28 17:42:34 dharms>
;; Modified by: Dan Harms
;; Keywords: c++

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun remove-leading-whitespace (start end)
  "Remove a region's leading whitespace.
Region is delimited by START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (looking-at "\\s-")
        (replace-match "" nil nil)))))

(defun remove-trailing-whitespace (start end)
  "Remove a region's trailing whitespace.
Region is delimited by START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-max))
      (backward-char)
      (while (looking-at "\\s-")
        (replace-match "" nil nil)
        (backward-char)))))

(defun remove-embedded-newlines (start end)
  "Remove a region's embedded newlines.
Region is delimited by START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
        (replace-match "" nil nil)))))

(defun cleanup-func-param-spacing (start end is-decl)
  "Clean up spacing of a single function parameter."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      ;; remove multiple consecutive whitespace
      (while (re-search-forward "\\s-\\{2,\\}" nil t)
        (replace-match " " nil nil))
    (goto-char (point-min))
    ;; remove whitespace before punctuation or parentheses
    (while (re-search-forward "\\s-+\\(\\s.\\|\\s\(\\|\\s\)\\)" nil t)
      (replace-match "\\1" nil nil))
    (goto-char (point-min))
    ;; remove whitespace after punctuation or parentheses
    (while (re-search-forward "\\(\\s.\\|\\s\(\\|\\s\)\\)\\s-+" nil t)
      (replace-match "\\1" nil nil))
    (goto-char (point-min))
    (if is-decl
         ;; for declarations, add a space if not present before parameter name
         (let ((identifier "\\sw\\|_\\|:"))
           (while (re-search-forward (concat "\\(.*?\\)\\s-?\\(\\(?:"
                                             identifier
                                             "\\)+\\)\\s-*$") nil t)
             (replace-match "\\1 \\2" nil nil)))))))

(defun clean-up-func-param (start end indent do-spacing is-decl)
  "Cleans up one (comma-separated) param of a function declaration."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      ;; if we're later indenting, dispose of old newlines
      (if indent
          (remove-embedded-newlines (point-min)(point-max)))
      (when do-spacing
        (cleanup-func-param-spacing (point-min)(point-max) is-decl)
        (remove-leading-whitespace (point-min)(point-max))
        (remove-trailing-whitespace (point-min)(point-max))))))

(defun clean-up-func-params (start end indent is-decl should-comment)
  "Does the actual work of cleaning up spacing and (optionally) indentation of a function declaration"
  (let ((saved nil))
    (save-excursion
      (save-restriction
        (narrow-to-region start end) ; separate by ',' and process each parameter
        (goto-char (point-min))
        (setq saved (cons (point) saved))
        (let ((start (point-min)))
          (catch 'break
            (while (<= (point) (point-max))
              (cond
               ((or (looking-at ",") (eq (point) (point-max)))
                (progn
                  (goto-char
                   (save-excursion
                     (save-restriction
                       (narrow-to-region start (point))
                       (goto-char (point-min))
                       ;; isolate any initializer
                       (if (re-search-forward "=" nil t)
                           (let ((is-quoted nil))
                             (backward-char)
                             (clean-up-func-param (point-min)(point) indent t is-decl)
                             ;; ensure spaces around the '='
                             (insert " ")
                             (forward-char)
                             (insert " ")
                             ;; don't strip whitespace or otherwise clean up quoted strings
                             (save-excursion
                               (when (re-search-forward
                                      "\\s-*\\(\\s\"+\\)\\(.*\\)\\(\\s\"+\\)\\s-*" nil t)
                                 (setq is-quoted t)
                                 (replace-match "\\1\\2\\3" nil nil)))
                             (clean-up-func-param (point) (point-max) indent
                                                  (not is-quoted) nil)
                             (if should-comment
                                 ;; begin comment before the '='
                                 (comment-region (- (point) 3) (point-max)))
                             (point-max))
                         (clean-up-func-param (point-min) (point-max) indent t is-decl)
                         (point-max))
                         )))
                   ;; save this point to insert newline later
                   (setq saved (cons (point) saved))
                   ;; stop at region end
                   (if (eq (point) (point-max))
                       (throw 'break nil))
                   (forward-char) ; at end of buffer this would quit
                   (insert " ")
                   (setq start (point))))
                ((looking-at "\\s\(")
                 (forward-list 1))      ;skip past parentheses
                (t (forward-char))
               ))))))
      (if indent
          (mapc (lambda(pos) ; points later in buffer are processed first
                  (progn
                    (goto-char pos)
                    (newline-and-indent)))
                saved)
        ;; if not inserting newlines (maintaining those present), we may need to re-indent
        (indent-region start end))))

(provide 'cleanup-funcs)
;;; clean-up-funcs.el ends here
