;; -*- Mode: Emacs-Lisp -*-
;; gen-tags.el --- Generate TAGS files
;; Copyright (C) 2015   (dan.harms)
;; Author:  <dan.harms@xrtrading.com>
;; Created: Wednesday, March 18, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-03-20 18:03:16 dan.harms>
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
'(("xr-common" "snap/xr-common/" ctags-cpp-options)
  )
(defvar gen-tags-target-sub-dir "tags/")
(defun generate-tags ()
  (if (profile-current-get 'project-name)
      (gen-tags-files gen-tags-alist)
    (error "Could not generate tags: no active profile")))

(defun gen-tags-files (alist)
  "Generate a series of tags files."
  (let ((local-dest-dir
         (concat
          (profile-current-get 'remote-prefix)
          (profile-current-get 'project-root-dir)
          gen-tags-target-sub-dir))
        (remote-dest-dir (profile-current-get 'tags-dir))
        )
    (message "local-dest-dir:%s remote-dest-dir:%s"
             local-dest-dir remote-dest-dir)
    (make-directory local-dest-dir t)
    (dolist element alist
            (gen-tags-file
             (map 'gen-tags-file
                  (nth 0 element)
                  (nth 1 element)
                  (cdr (cdr element)))))
    ))

(defun gen-tags-file (src-name src-sub-dir arg-list)
  "Generate a tags file."
  (interactive)
  (let* ((default-directory
           (concat
            (profile-current-get 'remote-prefix)
            (profile-current-get 'project-root-dir)
            src-sub-dir))
         (sub-name (concat src-name "-tags"))
         (local-dest-file (concat local-dest-dir sub-name))
         (remote-dest-file (concat remote-dest-dir sub-name))
         (msg process))
    (setq msg (format "Generating tags for %s into %s..."
                      default-directory local-dest-file))
    ;; todo: create remote-prefix/project-root-dir/dest-sub-dir if necessary
    (message msg)
    (setq process (apply 'start-file-process
                         "generate TAGS" (get-buffer-create " *gen-TAGS*")
                         (append '("/bin/sh" "-c" gen-tags-exe) arg-list)))
                                        ;                         '("/bin/sh" "-c" exec)))
    (set-process-sentinel
     process
     (lambda (proc change)
       (when (string-match "\\(finished\\|exited\\)" change)
                                        ;         (kill-buffer " *gen-TAGS*")
         ;; todo: copy-file (locally) to tags-dir
         (message (concat msg "done."))))
     )))

(provide 'gen-tags)

;; gen-tags.el ends here
