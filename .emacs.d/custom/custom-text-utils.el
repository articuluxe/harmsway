;;; custom-text-utils.el --- custom text manipulation utilities
;; Copyright (C) 2016-2017, 2021  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Thursday, April 14, 2016
;; Version: 1.0
;; Modified Time-stamp: <2021-03-16 10:59:58 dharms>
;; Modified by: Dan Harms
;; Keywords: text

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

(defun my/indent-line-relative()
  "Indent current line according to previous line."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (delete-horizontal-space)
    (indent-relative-maybe)
    ))

(defun jump-to-matching-paren() "Go to matching paren" (interactive)
  (if (looking-at "\\s\(")
      (forward-list 1)
    (backward-char)
    (if (looking-at "\\s\)")
        (progn
          (forward-char 1)
          (forward-list -1))
      (forward-char 1))))

(defun highlight-paren-right()
  "search forward for a parenthesized sexp and set region if found"
  (interactive)
  (let ((pos (search-forward-regexp "\\s\(" nil t)))
    (when pos
      (set-mark-command nil)
      (backward-char)
      (forward-list 1)
      (backward-char)
      (setq deactivate-mark nil))))

(defun highlight-paren-left()
  "search backward for a parenthesized sexp and set region if found"
  (interactive)
  (let ((pos (search-backward-regexp "\\s\)" nil t)))
    (when pos
      (set-mark-command nil)
      (forward-char)
      (forward-list -1)
      (forward-char)
      (setq deactivate-mark nil))))

(defun highlight-enclosing-paren(&optional arg)
  "assume point is bounded by paren and set region to that exp"
  (interactive "P")
  (if arg
      (let ((pos (search-forward-regexp "\\s\)" nil t)))
        (when pos
          (backward-char)
          (set-mark-command nil)
          (forward-char)
          (forward-list -1)
          (forward-char)
          (setq deactivate-mark nil)))
    (let ((pos (search-backward-regexp "\\s\(" nil t)))
      (when pos
        (forward-char)
        (set-mark-command nil)
        (backward-char)
        (forward-list 1)
        (backward-char)
        (setq deactivate-mark nil)))))

(defun enclose-by-braces (left right)
  "Insert symbols LEFT and RIGHT around a region or point."
  (interactive "r")
  (if (use-region-p) ; act on region
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char end)
          (insert right)
          (goto-char start)
          (insert left)))
    (progn ; act around point
      (insert left right)
      (backward-char 1))))
(defun enclose-by-braces-paren ()
  "Insert parentheses around a region or point."
  (interactive)
  (enclose-by-braces ?( ?) ))
(defun enclose-by-braces-bracket ()
  "Insert brackets around a region or point."
  (interactive)
  (enclose-by-braces ?[ ?] ))
(defun enclose-by-braces-brace ()
  "Insert braces around a region or point."
  (interactive)
  (enclose-by-braces ?{ ?} ))
(defun enclose-by-braces-caret ()
  "Insert carets around a region or point."
  (interactive)
  (enclose-by-braces ?< ?> ))

(defun highlight-current-sexp (&optional arg)
  "Highlight the current sexp around point"
  (interactive "P")
  (let ((n (if arg arg 1)))
    (unless (looking-at "\\_<")
      (backward-sexp n))
    (set-mark-command nil)
    (forward-sexp n)
    (setq deactivate-mark nil)))

(defun align-repeat-regexp (start end regexp)
  "Repeat alignment for regexp"
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun clean-up-buffer (begin end &optional arg)
  "Clean up all or a portion of a buffer.
Looks at either the entire buffer or the region delimited by
BEGIN and END.  Cleaning up entails: indenting, removing tabs,
and deleting trailing whitespace.  With optional prefix argument
ARG supplied and non-nil, newline sequences of more than 3 are
shortened to 2."
  (interactive (if (use-region-p)
                   (list (region-beginning)
                         (region-end)
                         current-prefix-arg)
                 (list nil nil current-prefix-arg)))
  (save-restriction
    (narrow-to-region (or begin (point-min))
                      (or end (point-max)))
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (if indent-tabs-mode
        (tabify (point-min) (point-max))
      (untabify (point-min) (point-max)))
    (when arg
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\n\\{3,\\}" nil t)
          (replace-match "\n\n"))))))

(provide 'custom-text-utils)
;;; custom-text-utils.el ends here
