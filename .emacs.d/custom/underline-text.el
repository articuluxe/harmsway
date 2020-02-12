;;; underline-text.el --- Underline text helper
;; Copyright (C) 2020  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Wednesday, February 12, 2020
;; Version: 1.0
;; Modified Time-stamp: <2020-02-12 14:24:56 dan.harms>
;; Modified by: Dan Harms
;; Keywords: tools

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
;; Copyright 2015 Boruch Baum <boruch_baum@gmx.com>, GPL3+ license
;; cf.  `nicholasvanhorn.com/posts/underline-text-in-emacs.html'

;;; Code:

(defun underline-text (arg)
  "Inserts a line under the current line, filled with a default
underline character `='. If point had been at the end of the
line, moves point to the beginning of the line directly following
the underlining. It does not underline the line's leading
whitespace, trailing whitespace, or comment symbols. With prefix `C-u'
prompts user for a custom underline character. With prefix `C-u
C-u', does not underline whitespace embedded in the line."
  (interactive "p")
  (let* ((original-point (point))
         (underline-char
          (replace-regexp-in-string "[[:cntrl:][:space:]]" "="
                                    (if (= arg 1)
                                        "="
                                      (char-to-string
                                       (read-char "What character to underline with?")))))
         (original-point-is-eol
          (when (looking-at "$") t))
         (original-point-is-eob
          (= original-point (point-max))))
    (beginning-of-line)
    (unless
        (when (looking-at "[[:space:]]*$")
          (beginning-of-line 0)
          (when (looking-at "[[:space:]]*$")
            (goto-char original-point)
            (message "nothing to do")))
      (insert
       (buffer-substring (line-beginning-position) (line-end-position))
       "\n")
      (save-restriction
        (narrow-to-region
         (progn
           (goto-char (1- (re-search-forward "[^[:space:]]" nil t)))
           (cond
            ((looking-at ";+")   (match-end 0))
            ((looking-at "#+")   (match-end 0))
            ((looking-at "//+")  (match-end 0))
            ((looking-at "/\\*+") (match-end 0))
            (t (point))))
         (1+ (progn
               (goto-char (line-end-position))
               (re-search-backward "[^[:space:]]" nil t))))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (if (= arg 16)
            (while (re-search-forward "[^[:space:]]" nil t)
              (replace-match underline-char nil))
          (re-search-forward "[^[:space:]]" nil t)
          (goto-char (1- (point)))
          (while (re-search-forward "." nil t)
            (replace-match underline-char nil)))
        (widen))
      (if original-point-is-eob
          (goto-char (point-max))
        (if original-point-is-eol
            (goto-char (re-search-forward "^"))
          (goto-char original-point))))))

(provide 'underline-text)
;;; underline-text.el ends here
