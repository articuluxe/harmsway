;; -*- Mode: Emacs-Lisp -*-
;; gen-tags.el --- Generate TAGS files
;; Copyright (C) 2015   (dan.harms)
;; Author:  <dan.harms@xrtrading.com>
;; Created: Wednesday, March 18, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-03-22 20:03:38 dharms>
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
(defvar gen-tags-ctags-cpp-kinds "+l")
(defvar gen-tags-ctags-cpp-options
  (list
   (concat "--c++-kinds=" gen-tags-ctags-cpp-kinds)
   "--file-scope=no"
   "--tag-relative=no"))
(defvar gen-tags-alist
  (list
;   (append (list "xr-common" "snap/xr-common/") ctags-cpp-options))
   (append (list "punch" "punch/" "/usr/local/bin/ctags";; gen-tags-exe
                 "-Re")
           gen-tags-ctags-cpp-options)
   (append (list "clib" "/opt/local/include/gcc48/c++/"
                 "/usr/local/bin/ctags" "-Re"
                 "--language-force=c++"
                 "-h=\".h.H.hh.hpp.hxx.h++.inc.def.\"")
           gen-tags-ctags-cpp-options)
   ))
(defvar gen-tags-target-sub-dir "tags/")
(defvar gen-tags--iter nil)
(defvar gen-tags--buffer nil)
(defvar gen-tags--remote nil
  "Are we generating tags for a remote source repository?")
(defun gen-tags-generate-tags () (interactive)
       (unless (profile-current-get 'project-name)
         (error "Could not generate tags: no active profile"))
       (setq gen-tags--buffer
             (get-buffer-create " *gen-TAGS*"))
       (setq gen-tags--iter gen-tags-alist)
       (gen-tags-first-file))

(defvar gen-tags--msg)
(defvar gen-tags--local-dest-file nil)
(defvar gen-tags--remote-dest-dir nil)
(defvar gen-tags--local-dest-dir nil)

(defun gen-tags-first-file ()
  "Generate a series of tags files."
  (setq gen-tags--remote-dest-dir (profile-current-get 'tags-dir))
  (setq gen-tags--local-dest-dir
        (concat
         (profile-current-get 'remote-prefix)
         (profile-current-get 'project-root-dir)
         gen-tags-target-sub-dir))
  (if gen-tags--iter
      (progn
        (message "local-dest-dir:%s remote-dest-dir:%s"
                 gen-tags--local-dest-dir gen-tags--remote-dest-dir)
        (make-directory gen-tags--local-dest-dir t)
        (gen-tags-next-file)
        )
    (with-current-buffer gen-tags--buffer
      (insert (now) "Starting to generate tags."))
    ))
      ;; (setq curr (car gen-tags--iter))
      ;; (set
      ;; (mapc (lambda(element)
      ;;         (gen-tags-next-file
      ;;          (nth 0 element)
      ;;          (nth 1 element)
      ;;          (cdr (cdr element))))
      ;;       alist)))

(defun gen-tags-next-file ()
  "Generate a tags file."
  (interactive)
  (if gen-tags--iter
    (let* ((src-name (nth 0 (car gen-tags--iter)))
           (src-dir (nth 1 (car gen-tags--iter)))
           (arg-list (cdr (cdr (car gen-tags--iter))))
           (default-directory
             (if (file-name-absolute-p src-dir)
                 src-dir
               (concat
                (profile-current-get 'project-root-dir)
                src-dir)))
           (sub-name (concat src-name "-tags"))
;           (local-dest-file
           (remote-dest-file (concat gen-tags--remote-dest-dir sub-name))
           process args)
      (if gen-tags--remote
          (setq default-directory
                (concat (profile-current-get 'remote-prefix)
                        default-directory)))
      (setq gen-tags--local-dest-file (concat gen-tags--local-dest-dir sub-name))
      (setq gen-tags--msg (format "Generating tags for %s into %s..."
                                  default-directory gen-tags--local-dest-file))
      ;; todo: create remote-prefix/project-root-dir/dest-sub-dir if necessary
      (message gen-tags--msg)
      (message "arg-list is %s" arg-list)
      (setq args
            (append arg-list
                    (list "-f" gen-tags--local-dest-file
                          default-directory)))
      (message "all args are %s" args)
      ;; (setq drh
      ;;                      (append (list "-Re") arg-list
      ;;                              (list "-f" gen-tags--local-dest-file
      ;;                                    default-directory)
      ;;                              ))
      ;; /bin/sh -c "<script>" requires its argument (the script) be
      ;; quoted by strings; and apply expects a list as its last argument,
      ;; to be flattened out when the process is called.  Hence the
      ;; massaging of the input below to be a list containing a single item:
      ;; a string of all arguments to be passed, starting with the executable.
      (setq process (apply 'start-file-process
                           "generate TAGS"
                           gen-tags--buffer
;                           (get-buffer-create " *gen-TAGS*")
                           "/bin/sh" "-c"
                           (list (mapconcat 'identity args " "))
                                        ;                                          (append (list exe) args) " "))

                           ;; (apply 'start-file-process "mine" (get-buffer-create "mine")
                           ;;        "/bin/sh" "-c" (list (mapconcat 'identity '("ls" "-l" "-h") " ")))

                           ;; (setq process (apply 'start-file-process
                           ;;                      "generate TAGS" (get-buffer-create " *gen-TAGS*")
                           ;;                      "/bin/sh" "-c" exe
                           ;;                      args
                           ;; (append arg-list
                           ;;         (list "-f" gen-tags--local-dest-file
                           ;;               default-directory))

                           ;; (append (list "-Re") arg-list
                           ;;         (list "-f" gen-tags--local-dest-file
                           ;;               default-directory)
                           ))
                                        ;                         '("/bin/sh" "-c" exec)))
      (set-process-sentinel
       process
       (lambda (proc change)
         (when (string-match "\\(finished\\|exited\\)" change)
                                        ;         (kill-buffer " *gen-TAGS*")
           ;; todo: copy-file (locally) to tags-dir
           (message (concat gen-tags--msg "done."))
           (message "Done generating %s" gen-tags--local-dest-file)
;           (if
           (setq gen-tags--iter (cdr gen-tags--iter))
           (gen-tags-next-file))
         )))
    (with-current-buffer gen-tags--buffer
      (insert (now) "Done generating tags."))
    ))

(provide 'gen-tags)

;; gen-tags.el ends here
