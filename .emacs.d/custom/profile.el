;;; profile.el --- manage profiles
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, November  3, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-12-09 17:53:12 dharms>
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

(defun prof-find-file-upwards (dir file-to-find)
  "Recursively search upward for file; returns path to file or nil if not found."
  (interactive)
  (let*
      ((find-file-r
        (lambda (path)
          (let* ((parent (file-name-directory path))
                 files)
            (cond
             ((or (null parent) (equal parent (directory-file-name parent))) nil)
             ((setq files (directory-files parent t file-to-find))
              (car files))              ;found
             ;; parent of ~ is nil, parent of / is itself
             ;; This terminating condition accounts for both
             (t (funcall find-file-r
                         (directory-file-name parent))))))))
    (funcall find-file-r (or dir default-directory))))

(defun prof-find-file-dir-upwards (file-to-find)
  "Recursively search upward for file; returns file's directory or nil if not found."
  (interactive)
  (let ((file (prof-find-file-upwards file-to-find)))
    (if file (file-name-directory file) nil)))

(defun prof--find-root (dir &optional absolute)
  "Search for the project root, starting from DIR and moving up the file tree.
Returns a cons (file, dir) containing the project file and its parent
directory, if found, else nil.  If ABSOLUTE is non-nil, the path, if found,
will be absolute.  Profile files can look like any of the following:
`.eprof', `my.eprof', `.my.eprof'."
  (let (root file)
    (setq root
          (if (<= 24 emacs-major-version)
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

(defun prof--on-prof-activated (profile)
  "A profile PROFILE has been activated."
  (unless (eq profile prof-current)
    (setq prof-current profile)
    ))

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
                        (expand-file-name filename)) prof-obarray))
    (let* ((root (prof--find-root (file-name-directory filename) t))
           (root-file (car root))
           (root-dir (cdr root))
           ))
    ))

(provide 'profile)
;;; profile.el ends here
