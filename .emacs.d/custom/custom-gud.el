;;; custom-gud.el --- custom gud utilities
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Tuesday, April 12, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-04-12 17:49:44 dharms>
;; Modified by: Dan Harms
;; Keywords: gud gdb coding

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


(defun my/launch-gdb()
  "Launch gdb automatically in the test directory."
  (interactive)
  (let ((root (profile-current-get 'project-root-dir))
        (prefix (profile-current-get 'remote-prefix))
        (sub-dirs (profile-current-get 'debug-sub-dirs))
        exec-dir exec)
    (when root
      (setq exec-dir
            (concat
             prefix root
             (cond ((eq (length sub-dirs) 1) (car sub-dirs))
                   ((null sub-dirs) "")
                   (t (funcall my/choose-func
                               sub-dirs "Debug dir:"))))))
    (unless (and exec-dir (file-exists-p exec-dir))
      (setq exec-dir default-directory))
    (setq exec (read-file-name "Debug executable: " exec-dir nil t))
    (gdb (concat "gdb -i=mi " exec))))

(provide 'custom-gud)
;;; custom-gud.el ends here
