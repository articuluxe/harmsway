;;; custom-environment.el --- custom environmental utilities
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Friday, April 15, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-05-26 16:52:08 dan.harms>
;; Modified by: Dan Harms
;; Keywords: environment utils

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

(defun read-file-into-list-of-lines (file)
  "Read FILE into a list of strings split line by line."
  (interactive)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun load-environment-variables-from-file (file)
  "Load each line from FILE, of the form `var=val'.
For each line, sets environment variable `var' equal to `val'."
  (interactive "fChoose the file: ")
  (mapc (lambda(line)
          (when (string-match "\\(.+\\)=\\(.+\\)" line)
            (setenv (match-string-no-properties 1 line)
                    (substitute-in-file-name
                     (match-string-no-properties 2 line)))))
        (read-file-into-list-of-lines file)))

(defun load-environment-variable-from-file (var file &optional sep)
  "Load into the environment variable VAR each line from the FILE.
SEP can be a separator."
  (interactive)
  (unless sep (setq sep path-separator))
  (setenv var (concat (mapconcat 'convert-standard-filename
                                 (read-file-into-list-of-lines file)
                                 sep) sep (getenv var))))

(defun my/load-environment-variables-from-file (dir &optional append-exec-path)
  "Update a series of predetermined environment variables according
to several files that may be present under DIR.  If
APPEND_EXEC_PATH is non-nil, the existing exec-path will have any
new elements prepended to it; otherwise, the default is to set
the final element of `exec-path' to the exec-directory.  The
point is that subsequent calls may not want to overwrite the
final element."
  (let ((path-file (concat dir "path"))
        (include-file (concat dir "include"))
        (lib-file (concat dir "lib"))
        (libpath-file (concat dir "libpath")))
    ;; check for any additional environment variables
    (if (file-exists-p path-file)
        (progn
          (load-environment-variable-from-file "PATH" path-file)
          ;; replicate path (delimiter-separated string of paths) into
          ;; exec-path (list of paths); by convention, ends in exec-dir
          (setq exec-path (append
                           (read-file-into-list-of-lines path-file)
                           (if append-exec-path
                               exec-path
                             (list (convert-standard-filename exec-directory)))))))
    (if (file-exists-p include-file)
        (load-environment-variable-from-file "INCLUDE" include-file))
    (if (file-exists-p lib-file)
        (load-environment-variable-from-file "LIB" lib-file))
    (if (file-exists-p libpath-file)
        (load-environment-variable-from-file "LIBPATH" libpath-file))
    ))

(provide 'custom-environment)
;;; custom-environment.el ends here
