;;; remove-files.el --- Remove files according to a description
;; Copyright (C) 2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, July  9, 2019
;; Modified Time-stamp: <2019-07-11 08:59:20 dharms>
;; Modified by: Dan Harms
;; Keywords: harmsway utils tools

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
;; Removes git lock files.
;;

;;; Code:
(require 'ivy)
(require 'seq)

(defun remove-files-action (x)
  "Remove file X."
  (let ((file (cdr x)))
    (delete-file file)))

;;;###autoload
(defun remove-files-regexp (regexp &optional root)
  "Present option to remove files under ROOT matching REGEXP.
ROOT defaults to `default-directory'."
  (let* ((root (or root default-directory))
         (files (directory-files-recursively root regexp)))
    (if (seq-empty-p files)
        (message "No files found")
      (ivy-read "Remove file: "
                (mapcar (lambda (file)
                          (cons (file-relative-name file root) file))
                        files)
                :action #'remove-files-action
                :caller 'remove-files-gather))))

(provide 'remove-files)
;;; remove-files.el ends here
