;;; full-edit.el --- open all interesting files recursively
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Tuesday, March 29, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-11-22 17:16:01 dharms>
;; Modified by: Dan Harms
;; Keywords:

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

(require 'seq)

(defvar full-edit-accept-patterns
  '( "\\.cpp$" "\\.cc$" "\\.cxx$" "\\.c$" "\\.C$"
     "\\.h$" "\\.hh$" "\\.hpp$" "\\.hxx$" "\\.H$"
     "\\.sh$" "\\.py$" "\\.sql$" "\\.java$" "\\.in$"
     "\\.proto$" "\\.el$" "\\.cs$"
     "^CMakeLists.txt$" "\\.cmake$"
     "^Makefile$" "^makefile$"
     )
  "List of regexps which `full-edit' will open.")
(defvar full-edit-reject-patterns
  '( "\\.exe$" "\\.pdb$" "\\.obj$"
     )
  "List of regexps which `full-edit' will ignore.")

(defun full-edit-test-list-for-string(list regex)
  "Check if a list contains a string by regexp."
  (let ((lst list)
        curr)
    (catch 'found
      (while lst
        (setq curr (car lst))
        (if (string-match curr regex)
            (throw 'found t)
          (setq lst (cdr lst))))
      nil)))

(defun full-edit-gather-all-files(dir reporter &optional symbolic)
  "Gather a list of filenames recursively below a directory.  Results are
  filtered via `full-edit-accept-patterns' and `full-edit-reject-patterns'."
  (let* ((all-results
          (directory-files
           dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)" t))
         (files (seq-remove 'file-directory-p all-results))
         (dirs (seq-filter 'file-directory-p all-results))
         (result '()))
    (unless symbolic
      (setq files (seq-remove 'file-symlink-p files))
      (setq dirs (seq-remove 'file-symlink-p dirs)))
    (mapc (lambda(file)
            (and
             (full-edit-test-list-for-string
              full-edit-accept-patterns
              (file-name-nondirectory file))
             (not (full-edit-test-list-for-string
                   full-edit-reject-patterns
                   (file-name-nondirectory file)))
             (setq result (cons file result))
             (progress-reporter-update reporter)
             ))
          files)
    (mapc (lambda(dir)
            (setq
             result
             (nconc
              result
              (full-edit-gather-all-files dir reporter symbolic))))
          dirs)
    result
    ))

(defun full-edit-open-file-list(files)
  "Find (open) each of a list of filenames."
  (let* ((i 0)
         (len (length files))
         (reporter (make-progress-reporter "Opening files..." 0 len)))
    (mapc (lambda(file)
            (find-file-noselect file)
            (setq i (+ i 1))
            (progress-reporter-update reporter i)
            ) files)
    (progress-reporter-done reporter)))

(defun full-edit(root &optional arg)
  "Find (open) all files recursively below a directory.
   With optional prefix argument, will follow symbolic targets."
  (interactive
   `(,(read-directory-name "Full-Edit Directory: " nil nil t)
     ,current-prefix-arg))
  (if root
      (let* ((reporter (make-progress-reporter "Gathering files..."))
             (files (full-edit-gather-all-files (expand-file-name root)
                                                reporter arg)))
        (progress-reporter-done reporter)
        (full-edit-open-file-list files)
        )
    (message "No directory given")))

(provide 'full-edit)
;;; full-edit.el ends here
