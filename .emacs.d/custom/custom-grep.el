;;; custom-grep.el --- a custom grep
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Tuesday, April 12, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-04-14 09:23:09 dan.harms>
;; Modified by: Dan Harms
;; Keywords: grep coding

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

(defun my/grep (&optional arg)
  "Provides a wrapper around grep to provide convenient shortcuts
to adjust the root directory.  With a prefix ARG of 64 (C-u C-u
C-u), or if the variable 'project-root-dir is not defined in the
current profile, the search directory will be chosen
interactively by the user using ido.  With a prefix arg of 16
`C-uC-u', use the current directory.  With a prefix arg of 4
`C-u', the directory will be chosen interactively by the user
among the src directories configured using the current profile.
Otherwise, use the first src directory configured in the
profile."
  (interactive "p")
  (let* ((root (profile-current-get 'project-root-dir))
         (dirs (profile-current-get 'grep-dirs))
         (first (cdr (car dirs)))
         (prompt "Grep root: ")
         (dir
          (cond ((or (null root) (null dirs) (= arg 64))
                 (read-directory-name prompt nil nil t))
                ((= arg 16) ".")
                ((= arg 4) (funcall my/choose-func dirs prompt))
                (t first)))
         (remote (file-remote-p dir))
         (search-string
          (if (region-active-p)
              (buffer-substring (region-beginning) (region-end))
            (thing-at-point 'symbol t))))
    (when remote                        ;remove remote prefix if present
      (setq dir
            (replace-regexp-in-string (regexp-quote remote) "" dir)))
    (unless (file-remote-p default-directory)
      (setq dir
            ;; some variants of grep don't handle relative paths
            ;; (but expand-file-name doesn't work remotely)
            (expand-file-name dir)))
    (require 'grep)
    (grep-apply-setting
     'grep-command
     (concat "find -P "
             ;; some greps dislike trailing slashes
             (directory-file-name dir)
             " \"(\" -name \"*moc_*\" -o -name \"*qrc_*\" \")\" "
             "-prune -o -type f \"(\" -name \"*.cpp\" -o -name \"*.h\" "
             "-o -name \"*.cc\" -o -name \"*.hh\" -o -name \"*.cxx\" "
             "-o -name \"*.hxx\" -o -name \"*.h\" -o -name \"*.c\" "
             "-o -name \"*.H\" -o -name \"*.C\" -o -name \"*.hpp\" "
             "-o -name \"*.in\" -o -name \"*.ac\" -o -name \"*.el\" "
             "-o -name \"*.sql\" -o -name \"*.py\" -o -name \"*.proto\" "
             "-o -name \"*.sh\" -o -name \"*.cs\" -o -name \"*.dart\" "
             "\")\" -print0 | xargs -0 grep -Isn "
             (shell-quote-argument search-string)
             ))
    (command-execute 'grep)))

(provide 'custom-grep)
;;; custom-grep.el ends here
