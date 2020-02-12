;;; insert-with-completion.el --- Insert with completion
;; Copyright (C) 2020  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Wednesday, February 12, 2020
;; Version: 1.0
;; Modified Time-stamp: <2020-02-12 14:38:12 dan.harms>
;; Modified by: Dan Harms
;; Keywords: tools
;; Package-Requires: ((emacs "25.1"))


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
(require 'read-file)

(defun insert-with-completion (strings)
  "Insert one of STRINGS into the current buffer."
  (insert (completing-read "Insert: " strings nil t)))

(defun insert-with-completion-from-file (file)
  "Insert a string selected from FILE into the current buffer."
  (interactive "FFile: ")
  (insert-with-completion
   (seq-remove
    #'string-empty-p
    (read-file-transform
     (read-file-into-lines file)
     #'read-file-strip-hash-comment
     #'string-trim))))

(provide 'insert-with-completion)
;;; insert-with-completion.el ends here
