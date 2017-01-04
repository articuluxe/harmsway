;;; profile.el --- manage profiles
;; Copyright (C) 2016, 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, November  3, 2016
;; Version: 1.0
;; Modified Time-stamp: <2017-01-03 17:32:57 dharms>
;; Modified by: Dan Harms
;; Keywords: profiles project

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
;; This project is based in part on profiles.el by Sylvain Bougerel, from
;; 2009.

;;

;;; Code:

(require 'f)
(require 'tramp)

(defvar prof-obarray
  (let ((intern-obarray (make-vector 7 0)))
    (intern "default" intern-obarray)
    intern-obarray)
  "Array of profile objects.")
(defvar prof-path-alist '()
  "Alist of pairs of strings (REGEXP . PROFILE-NAME).
A profile is used for a file if the filename matches REGEXP.  In the case
of no matches, the default profile is instead used.")
(defvar prof-local (intern-soft "default" prof-obarray))
(defvar prof-current nil)

(defun prof-p (prof)
  "Return non-nil if PROF is a profile."
  (intern-soft prof prof-obarray))

(defun prof-put (profile property value)
  "Put into PROFILE the PROPERTY with value VALUE."
  (let ((p (intern-soft profile prof-obarray)))
    (if p (put p property value)
      (error "Invalid profile %s" profile))))

(defun prof-get (profile property &optional inhibit-polymorphism)
  "Get from PROFILE the value associated with PROPERTY."
  (let ((p (intern-soft profile prof-obarray))
        parent parentname)
    (if p
        (or (get p property)
            (and (not inhibit-polymorphism)
                 parentname
                 (setq parentname (get p :parent))
                 (setq parent (intern-soft parentname prof-obarray))
                 (prof-get parent property)))
      (error "Invalid profile %s" profile))))

(defun prof-define (profile &rest plist)
  "Create or replace a profile named PROFILE.
Add to it the property list PLIST."
  (let ((p (intern profile prof-obarray)))
    (setplist p plist)))

(defun prof-define-derived (profile parent &rest plist)
  "Create or replace a profile named PROFILE.
Its parent is PARENT.  Add to it the property list PLIST."
  (let ((p (intern profile prof-obarray)))
    (setplist p (append (list :parent parent) plist))))

(defun prof-find-path-alist (&optional filename)
  "Scan `prof-path-alist' for an entry to match FILENAME."
  (assoc-default
   (or filename (buffer-file-name) (buffer-name))
   prof-path-alist 'string-match))

(defun prof-soft-reset ()
  "Reset the current profile.
This does not otherwise remove any profiles from memory."
  (interactive)
  ;; kill-local-variable insufficient due to permanent-local property
  (setq prof-current nil)
  (setq prof-local (default-value 'prof-local)))

(defun prof-hard-reset (&optional profile)
  "Remove all traces of PROFILE."
  (interactive)
  (prof--remove-prof-from-alist profile)
  (prof--remove-prof profile)
  (prof-soft-reset))

(defun prof--remove-prof (profile)
  "Delete the profile PROFILE, which can be a symbol or string (name)."
  (unintern profile prof-obarray))

(defun prof--remove-prof-from-alist (profile)
  "Remove profile PROFILE from the internal data structure."
  (setq prof-path-alist
        (seq-remove
         (lambda (elt)
           ;; string-equal handles a symbol using its print-name
           (string-equal (cdr elt) profile))
         prof-path-alist)))

(defun prof-find-file-upwards-helper (path file)
  "Helper function to search upward from PATH for FILE."
  (let* ((parent (file-name-directory path))
         files)
    (cond
     ;; parent of ~ is nil, parent of / is itself
     ;; This terminating condition accounts for both
     ((or (null parent) (equal parent (directory-file-name parent)))
      nil)
     ((setq files (directory-files parent t file))
      (car files))                      ;found
     (t (prof-find-file-upwards-helper
         (directory-file-name parent) file)))))

(defun prof-find-file-upwards (dir file)
  "Recursively search upward from DIR for FILE.
Return path to file or nil if not found."
  (interactive)
  (prof-find-file-upwards-helper (or dir default-directory) file))

(defun prof-find-file-dir-upwards (file)
  "Recursively search upward for FILE.
Return that file's directory or nil if not found."
  (interactive)
  (let ((file (prof-find-file-upwards nil file)))
    (when file (file-name-directory file))))

(defun prof--find-root (dir &optional absolute)
  "Search for the project root, starting from DIR and moving up the file tree.
Returns a cons (file, dir) containing the project file and its parent
directory, if found, else nil.  If ABSOLUTE is non-nil, the path, if found,
will be absolute.  Profile files can look like any of the following:
`.eprof', `my.eprof', `.my.eprof'."
  (let (root file)
    (setq root
          (if (functionp 'locate-dominating-file)
              (locate-dominating-file
               dir
               (lambda (parent)
                 (setq file
                       (car (directory-files parent t "\\sw+\\.e?prof$")))))
            (prof-find-file-upwards dir "\\sw+\\.e?prof$")))
    (when root
      (if absolute
          (cons file (expand-file-name root))
        (cons file root)))))

(defun prof--compute-basename (name)
  "Return basename of profile located at NAME.
For example, given a profile file `.mybase.eprof', the basename would be
`mybase'."
  (let ((base (file-name-base name)))
    (when (string-match "\\.?\\(.+\\)" base)
      (match-string-no-properties 1 base))))

(defun prof--compute-remote-props (dir)
  "Compute the remote properties associated with DIR.
DIR may be remote."
  (and dir (file-remote-p dir)
       (with-parsed-tramp-file-name dir file
         `( ,file-host ,file-localname
                       ,(tramp-make-tramp-file-name
                         file-method file-user file-host "")))))

(defun prof--compute-stem (prof)
  "Compute a profile PROF's stem.
This is useful in regexp-matching.  The profile's root-dir is
probably a relative path, possibly including a `~' that
represents the user's home directory."
  (replace-regexp-in-string "~/" "" (prof-get prof :root-dir)))

(defun prof--loaded (prof)
  "A profile PROF has been loaded.
This may or may not be for the first time."
  (unless (prof-get prof :inited)
    (prof-put prof :inited t)
    (prof--inited prof))
  (unless (eq prof prof-current)
    ;; todo: call hooks for profile change
    (setq prof-current prof))
  )

(defun prof--log-profile-loaded (prof)
  "Log a profile PROF upon initialization."
  (let ((name (symbol-name prof)))
    (unless (string-equal name "default")
      (message "Loaded profile %s (project %s) at %s"
               name
               (prof-get prof :project-name)
               (prof-get prof :root-dir)))))

(defun prof--safe-funcall (prof property &rest rem)
  "Call a function as defined in the PROPERTY of profile PROF.
The function must exist and be bound."
  (let ((func (intern-soft
               (prof-get prof property))))
    (and func (fboundp func) (funcall func rem))))

(defun prof--inited (prof)
  "Initialize a profile PROF."
  (prof--log-profile-loaded prof)

  ;; todo
  (prof--safe-funcall prof :init)
  )

(defadvice find-file-noselect-1
    (before before-find-file-no-select-1 activate)
  (prof--file-opened buf filename))

(defun prof--file-opened (buffer filename)
  "Initialize a profile, if necessary, for BUFFER, visiting FILENAME."
  (with-current-buffer buffer
    (make-local-variable 'prof-local)
    (put 'prof-local 'permanent-local t)
    (setq prof-local
          (intern-soft (prof-find-path-alist
                        (expand-file-name filename))
                       prof-obarray))
    (let* ((root (prof--find-root (file-name-directory filename) t))
           (root-file (car root))
           (root-dir (cdr root))
           (remote-props (prof--compute-remote-props root-dir))
           remote-host remote-localname remote-prefix basename)
      (when remote-props
        (setq remote-host (car remote-props))
        (setq remote-localname (cadr remote-props))
        (setq remote-prefix (caddr remote-props)))
      (when (and root root-file root-dir
                 (string-match "\\.[er]prof$" root-file)
                 (not (string-equal root-dir
                                    (prof-get prof-local :root-dir))))
        ;; a new profile, not yet inited
        (load-file root-file)
        (setq basename (prof--compute-basename root-file))
        (when remote-props
          (setq root-dir remote-localname))
        (setq prof-path-alist (cons (cons root-dir basename)
                                    prof-path-alist))
        (setq prof-local
              (intern-soft (prof-find-path-alist
                            (expand-file-name filename))
                           prof-obarray))
        (unless (prof-get prof-local :root-dir)
          (prof-put prof-local :root-dir root-dir))
        ;; change to absolute if necessary: in case the profile listed
        ;; root-dir as relative
        (when (f-relative? (prof-get prof-local :root-dir))
          (prof-put prof-local :project-name
                    (f-long (prof-get prof-local :root-dir))))
        (unless (prof-get prof-local :project-name)
          (prof-put prof-local :project-name basename))
        (unless (prof-get prof-local :remote-host)
          (prof-put prof-local :remote-host remote-host))
        (unless (prof-get prof-local :remote-prefix)
          (prof-put prof-local :remote-prefix remote-prefix))
        (unless (prof-get prof-local :root-stem)
          (prof-put prof-local :root-stem
                    (prof--compute-stem prof-local)))
        )
      (prof--loaded prof-local)
      )))

(provide 'profile)
;;; profile.el ends here
