;;; read-file.el --- read a file into a list of lines
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, June  9, 2017
;; Version: 1.0
;; Modified Time-stamp: <2018-03-30 17:04:27 dharms>
;; Modified by: Dan Harms
;; Keywords: file input
;; Package-Requires: ((emacs "25"))

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
;; Read a file into a list of lines.  Optionally do further processing per
;; line.
;;

;;; Code:
(require 'seq)
(require 'subr-x)

;;;###autoload
(defun read-file-into-lines (file)
  "Read FILE into a list of strings, one per line."
  (interactive "fFile: ")
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n")))

;;;###autoload
(defun read-file-transform (lines &rest forms)
  "Process LINES as directed.
Evaluate FORMS per line via `thread-first'."
  (let ((lst (mapcar (lambda (line)
                       (eval `(thread-first line ,@forms)))
                     lines)))
    lst))
    ;; (if strip-empty
    ;;     (seq-remove 'string-empty-p lst)
    ;;   lst)))

(defun read-file-strip-hash-comment (str)
  "Strip comments from STR starting with `#' to end of string."
  (if (string-match "#.*$" str)
      (replace-match "" nil nil str)
    str))

;; (defun read-file-into-list-of-lines (file)
;;   "Read FILE into a list of strings split line by line."
;;   (interactive "f")
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (split-string (buffer-string) "\n" t)))

;; (defun mytest (&rest forms)
;;   (interactive)
;;   (mapcar (lambda (x)
;;             (eval
;;              `(thread-first x ,@forms)
;;              )
;;             )
;;           (split-string " 123 # 456 # 789 " "#")))

;; (defun mystr (x) (string-width x))

;; (mytest 'string-trim 'mystr)
;; (3 3 3)

;; ("123" "456" "789")


(provide 'read-file)
;;; read-file.el ends here
