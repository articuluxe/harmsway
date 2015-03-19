;; -*- Mode: Emacs-Lisp -*-
;; gen-tags.el --- Generate TAGS files
;; Copyright (C) 2015   (dan.harms)
;; Author:  <dan.harms@xrtrading.com>
;; Created: Wednesday, March 18, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-03-19 17:37:56 dan.harms>
;; Keywords: etags, ctags

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

;; Commentary:

;;

;; Code:

(defvar gen-tags-exe
  (cond ((file-exists-p "/bin/ctags") "/bin/ctags")
        ((file-exists-p "/usr/bin/ctags") "/usr/bin/ctags")
        (t "ctags")))
(defvar ctags-cpp-kinds "+l")
(defvar ctags-cpp-options (concat "--c++-kinds=" ctags-cpp-kinds
                                  " --file-scope=no"
                                  " --tag-relative=no"))
(defvar gen-tags-alist)
  '(("xr-common" "snap/xr-common/")
    )

(defun generate-tags ()
  (gen-tags-internal gen-tags-exe (nth 1 gen-tags-alist)
                     "tags" (cdr (cdr gen-tags-alist)))
  )

(defun gen-tags-internal (exec src-sub-dir dest-sub-dir arg-list)
  "Generate a tags file."
  (interactive)
  (let ((default-directory
          (concat
           (profile-current-get 'remote-prefix)
           (profile-current-get 'project-root-dir)
           src-sub-dir))
        (dest (concat
               (profile-current-get 'remote-prefix)
               (profile-current-get 'project-root-dir)
               (or dest-sub-dir "tags")))
        (process))
    ;; todo: create remote-prefix/project-root-dir/dest-sub-dir if necessary
    (message "Generating tags for %s into %s..."
             default-directory dest)
    (setq process (apply 'start-file-process
                         "generate TAGS" (get-buffer-create " *gen-TAGS*")
                         (append '("/bin/sh" "-c" exec) arg-list)))
;                         '("/bin/sh" "-c" exec)))
    (set-process-sentinel
     process
     (lambda (proc change)
       (when (string-match "\\(finished\\|exited\\)" change)
;         (kill-buffer " *gen-TAGS*")
         (message "gen-TAGS done")))
     )))

(provide 'gen-tags)

;; gen-tags.el ends here
