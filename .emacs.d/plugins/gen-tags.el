;; -*- Mode: Emacs-Lisp -*-
;; gen-tags.el --- Generate TAGS files
;; Copyright (C) 2015   (dan.harms)
;; Author:  <dan.harms@xrtrading.com>
;; Created: Wednesday, March 18, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-03-25 00:14:37 dharms>
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
  (cond ((file-exists-p "/usr/local/bin/ctags") "/usr/local/bin/ctags")
        ((file-exists-p "/bin/ctags") "/bin/ctags")
        ((file-exists-p "/usr/bin/ctags") "/usr/bin/ctags")
        (t "ctags"))
  "The ctags executable.")
(defvar gen-tags-ctags-cpp-kinds "+l" "Default ctags cpp-kinds options.")
(defvar gen-tags-ctags-cpp-options
  (list
   (concat "--c++-kinds=" gen-tags-ctags-cpp-kinds)
   "--file-scope=no"
   "--tag-relative=no")
  "Default ctags cpp options.")
(defvar gen-tags-alist '()
  "A list whose every element is a sub-list specifying how to generate a
TAGS file.")
(defvar gen-tags-target-sub-dir "tags/"
  "Default sub-dir in which to put generated tag files.")
(defvar gen-tags-copy-remote nil)
(defvar gen-tags--iter nil "Current item being processed.")
(defvar gen-tags--total-num 0 "Total number of TAGS files to create.")
(defvar gen-tags--curr-num 0 "Current number of TAGS file being created.")
(defvar gen-tags--buffer nil "gen-TAGS buffer.")
(defvar gen-tags--remote nil
  "Are tags being generating for a remote source repository?")
(defvar gen-tags--msg)
(defvar gen-tags--intermediate-dest-dir nil
  "An intermediate staging location for each TAGS file being generated.
This is useful for generating TAGS on a remote server.")
(defvar gen-tags--intermediate-dest-file nil
  "The intermediate staging file for each TAGS file that is generated.
This is useful for generating TAGS on a remote server.  On a local server,
this will be the same as the tags-dir.")
(defvar gen-tags--final-dest-dir nil
  "The final destination for the TAGS files being generated.")
(defvar gen-tags--final-dest-file nil
  "The final file being written for the current TAGS generation.")

(defun gen-tags-generate-tags () (interactive)
       (unless (profile-current-get 'project-name)
         (error "Could not generate tags: no active profile"))
       (gen-tags--first-file))

(defun gen-tags--on-start ()
  "Called when TAGS generation begins."
  (message "local-dest-dir:%s remote-dest-dir:%s"
           gen-tags--intermediate-dest-dir gen-tags--final-dest-dir)
  (with-current-buffer gen-tags--buffer
    (insert (format "TAGS generation started at %s %s.\n\n"
            (today) (now)))))

(defun gen-tags--on-finish ()
  "Called when TAGS generation completes."
  (with-current-buffer gen-tags--buffer
    (insert (format "TAGS generation finished at %s %s.\n\n\n"
            (today) (now)))))

(defun gen-tags--first-file ()
  "Generate a series of tags files."
  (setq gen-tags--buffer (get-buffer-create " *gen-TAGS*"))
  (setq gen-tags--total-num (length gen-tags-alist))
  (setq gen-tags--iter gen-tags-alist)
  (setq gen-tags--remote (profile-current-get 'remote-prefix))
  (setq gen-tags--final-dest-dir (profile-current-get 'tags-dir))
  (if gen-tags--remote
      ;; we're generating TAGS on a remote host, so set up a
      ;; staging area for generation, before we copy them to the
      ;; local destination.
      (progn
        (setq gen-tags--intermediate-dest-dir
              (concat (profile-current-get 'project-root-dir)
                      gen-tags-target-sub-dir))
        (make-directory (concat gen-tags--remote
                                gen-tags--intermediate-dest-dir) t))
    ;; else everything is local, so set up our variables in order to
    ;; generate output directly into the final destination.
    (setq gen-tags--intermediate-dest-dir
          gen-tags--final-dest-dir))
  (make-directory gen-tags--final-dest-dir t)
  (gen-tags--on-start)
  (gen-tags--try-gen-next-file)
  )

(defun gen-tags--try-gen-next-file ()
  "Generate a tags file."
  (if gen-tags--iter
      (gen-tags--gen-next-file)
    (gen-tags--on-finish)))

(defun gen-tags--gen-next-file ()
  "Generate TAGS for the current element."
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
         process args)
    (setq gen-tags--intermediate-dest-file
          (concat gen-tags--intermediate-dest-dir sub-name))
    ;; this won't be used in the local scenario
    (setq gen-tags--final-dest-file
          (concat gen-tags--final-dest-dir sub-name))
    (setq gen-tags--msg (format "Generating tags for %s into %s..."
                                default-directory
                                gen-tags--intermediate-dest-file))
    (with-current-buffer gen-tags--buffer
      (insert gen-tags--msg))

    (message "arg-list is %s" arg-list)
    (setq args
          (append arg-list
                  (list "-f" gen-tags--intermediate-dest-file
                        default-directory)))
    (message "all args are %s" args)

    ;; if remote, we need the remote prefix
    (when gen-tags--remote
      (setq default-directory
            (concat gen-tags--remote default-directory)))
    ;; /bin/sh -c "<script>" requires its argument (the script) be
    ;; quoted by strings; and `apply' expects a list as its last argument,
    ;; to be flattened out when the process is called.  Hence the
    ;; massaging of the input below to be a list containing a single item:
    ;; a string of all arguments to be passed, starting with the executable.
    (setq process (apply 'start-file-process
                         "generate TAGS"
                         gen-tags--buffer
                         "/bin/sh" "-c"
                         (list (mapconcat 'identity args " "))
                         ))
    (set-process-sentinel
     process
     (lambda (proc change)
       (when (string-match "\\(finished\\|exited\\)" change)
                                        ;         (kill-buffer " *gen-TAGS*")
         (with-current-buffer gen-tags--buffer
           (insert "done.\n"))
         (when (and gen-tags--remote gen-tags-copy-remote)
           (copy-file
            (concat gen-tags--remote
                    gen-tags--intermediate-dest-file)
            gen-tags--final-dest-file t))
         (setq gen-tags--iter (cdr gen-tags--iter))
         (setq gen-tags--curr-num (1+ gen-tags--curr-num))
         (gen-tags--try-gen-next-file))
       )))
  )

(provide 'gen-tags)

;; gen-tags.el ends here
