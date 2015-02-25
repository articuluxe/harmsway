;; -*- Mode: Emacs-Lisp; lexical-binding: nil -*-
;;; profiles+.el --- Extensions to 'profiles.el'
;; Copyright (C) 2015  Dan Harms
;; Created: Thursday, February 19, 2015
;; Time-stamp: <2015-02-24 19:28:19 dharms>

;; Author: Dan Harms
;; Keywords:

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

(require 'profiles)
(setq profile-path-alist-file nil)

;;;###autoload
(defun profile-define-derived (profile parent &optional name mail &rest plist)
  "Create or replace PROFILE with NAME and MAIL.  PROFILE, NAME and MAIL are
   all required to be string values.  Optional argument PLIST is a property
   list.  The new profile shares the properties of its parent, unless it
   chooses to override any of them."
  (setplist (intern profile profile-obarray)
            (append (list 'name name 'mailing-address mail 'parent parent)
                    plist)))

;;;###autoload
(defun profile-lookup-property-polymorphic (profile property)
  "Lookup PROPERTY in PROFILE.  If not found, and PROFILE has a
parent, lookup PROPERTY in one of its parents."
  (let ((val (get profile property))
        (parent-name (get profile 'parent)))
    (or val
        (and parent-name
             (intern-soft parent-name profile-obarray)
             (profile-lookup-property-polymorphic
              (intern-soft parent-name profile-obarray) property)))))

;;;###autoload
(defun profile-current-get (property &optional ignore-parent)
  "Return the value of PROPERTY for the current profile `profile-current'.
The returned property is not evaluated.  This overrides the function in
`profiles.el'."
  (let ((val (get profile-current property)))
    (or val
        (and (null ignore-parent)
             (profile-lookup-property-polymorphic profile-current property)))))

;;;###autoload
(defun profile-current-funcall (property)
  "Return the function call of PROPERTY's value for the current
profile `profile-current'."
  (funcall (get profile-current property)))

;;;###autoload
(defun profile-find-profile-basename (name)
  "Given a typical profile file such as `.mybase.profile', returns the
basename, such as `mybase'."
  (when
      (string-match "\\.?\\(\\sw+\\)$" (file-name-base name))
  (match-string 1 (file-name-base name))))

(defvar profile--root-file)
;;;###autoload
(defun profile-find-root (dir)
  "Searches for the project root as may be defined in current profile,
starting from DIR and moving up the directory tree.  Profile files can look
like any of the following: `.profile', `my.profile', `.my.profile', `.root',
`.git'."
  (let ((root
         (locate-dominating-file
          dir
          (lambda(parent)
            (let ((res (directory-files parent t "\\.profile$")))
              (when res
                (setq profile--root-file (car res))))))))
    (unless root
      (setq root
            (locate-dominating-file dir ".root")
            profile--root-file ".root"))
    (unless root
      (setq root
            (locate-dominating-file dir ".git")
            profile--root-file ".git"))
    (if root
        (cons profile--root-file (expand-file-name root))
      (setq profile--root-file nil))))

;; called when a profile is loaded
(defun profile--on-loaded () "Initializes a loaded profile."
       (when (and c-buffer-is-cc-mode
                  profile-current
                  (profile-current-get 'funcall))
         (profile-current-funcall 'funcall)))

(add-hook 'find-file-hook 'profile--on-loaded)

;; override the advice
(defadvice find-file-noselect-1
    (before before-find-file-noselect-1 activate)
  "Set the buffer local variable `profile-current' right after the creation
of the buffer."
  (with-current-buffer buf
    (make-local-variable 'profile-current)
    (put 'profile-current 'permanent-local t)
    (setq profile-current
          (intern-soft (profile-find-path-alist
                        (expand-file-name filename)) profile-obarray))
    (let* ((root (profile-find-root (file-name-directory filename)))
           (curr (profile-current-get 'project-root))
           (root-file (car root))
           (root-dir (cdr root))
           (profile-basename (profile-find-profile-basename root-file)))
      (when root-dir
        (unless (string-equal root-dir (profile-current-get 'project-root))
          ;; this profile has not been loaded before
          (if (string-match "\\.profile$" root-file)
              (progn                    ;load the new profile
                (load-file root-file)
                ;; update the path alist to activate any new profiles
                (setq profile-path-alist
                      (cons (cons
                             (file-relative-name root-dir "~")
                             profile-basename) profile-path-alist))
                (setq profile-current
                      (intern-soft (profile-find-path-alist
                                    (expand-file-name filename))
                                   profile-obarray))
                (unless (profile-current-get 'project-root)
                  (profile-current-put 'project-root root-dir))
                (unless (profile-current-get 'project-name)
                  (profile-current-put 'project-name profile-basename)))
            ;; new profile, but not from a .profile
            (profile-current-put 'project-root root-dir)
            ))))))

(provide 'profiles+)

;;; profiles+.el ends here
