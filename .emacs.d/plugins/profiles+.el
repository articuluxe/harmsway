;; -*- Mode: Emacs-Lisp -*-
;; profiles+.el --- Extensions to 'profiles.el'
;; Copyright (C) 2015  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-03-15 23:59:54 dharms>
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

;; Commentary:

;;

;; Code:

(require 'profiles)
(require 'tramp)

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
(defun profile-current-funcall (property project-root-dir)
  "Return the function call of PROPERTY's value for the current
profile `profile-current'."
  (funcall (get profile-current property) project-root-dir))

;;;###autoload
(defun profile-find-profile-basename (name)
  "Given a typical profile file such as `.mybase.profile', returns the
basename, such as `mybase'."
  (when
      (string-match "\\.?\\(\\sw+\\)$" (file-name-base name))
  (match-string 1 (file-name-base name))))

(defvar profile--root-file)
;;;###autoload
(defun profile-find-root (dir &optional absolute)
  "Searches for the project root as may be defined in current profile,
starting from DIR and moving up the directory tree.  Profile files can look
like any of the following: `.eprof', `my.eprof', `.my.eprof', `.root',
`.git'."
  (let ((root
         (locate-dominating-file
          dir
          (lambda(parent)
            (let ((res (directory-files parent t "\\sw+\\.[er]prof$")))
              (when res
                (setq profile--root-file (car res))))))))
    (unless root
      (setq root
            (locate-dominating-file dir ".root")
            profile--root-file ".root"))
    ;; (unless root
    ;;   (setq root
    ;;         (locate-dominating-file dir ".git")
    ;;         profile--root-file ".git"))
    (if root
        (if absolute
            (cons profile--root-file (expand-file-name root))
          (cons profile--root-file root))
      (setq profile--root-file nil))))

(defun profile--real-file-name (filename)
  "Return the tag's correct destination file, FILENAME.  This may prepend a
remote prefix."
  (message "tags looking up real-file-name %s" filename)
  (if (file-name-absolute-p filename) filename
    (concat (profile-current-get 'project-root-dir)
            (profile-current-get 'src-sub-dir)
            filename)))

(setq etags-select-real-file-name 'profile--real-file-name)

;; called when a file is opened and assigned a profile
(defun profile--on-file-opened () "Initialize a file after opening and
assigning a profile."
       (when (and profile-current
                  (profile-current-get 'on-file-open)
                  (fboundp (intern-soft
                            (profile-current-get 'on-file-open))))
         (profile-current-funcall 'on-file-open
                                  (profile-current-get 'project-root-dir))))

(add-hook 'find-file-hook 'profile--on-file-opened)

;; (defvar profile-local-tags-dir-root "~/src/tags/")
;; (defun profile-remote-host (file)
;;   (if (tramp-tramp-file-p file)
;;       (tramp-file-name-host (tramp-dissect-file-name file))
;;     nil))
;; (defun profile-local-tags-dir (file root)
;;   (let ((host (profile-remote-host file)))
;;     (if host
;;         (concat (replace-regexp-in-string "\\/" "!" root t nil)
;;                 "/" profile-local-tags-dir-root host "/"
;;                 "/"
;;                 ) nil)))

(defun profile--compute-tags-dir ()
  "Helper function that computes where a project's local TAGS live."
  (let ((base (profile-current-get 'tags-sub-dir))
        (dir default-directory))
    (when (tramp-tramp-file-p dir)
      (profile-current-put 'remote-host
                           (tramp-file-name-host
                            (tramp-dissect-file-name dir))))
    (concat (profile-current-get 'project-root-dir)
            (or base "tags/"))))

(defun profile--compute-project-stem (root-dir)
  "Helper function that computes a project's stem, useful in regular
expression matching.  The presumption is that the ROOT-DIR is a relative
path, possibly including a `~' representing the user's home directory."
  (replace-regexp-in-string "~" "" root-dir))

;; called when a profile is initialized
(defun profile--on-profile-init ()
  "Initialize a loaded profile."
  (let ((root (profile-current-get 'project-root-dir)))
    (when (and profile-current
               (profile-current-get 'on-profile-init)
               (fboundp (intern-soft
                         (profile-current-get 'on-profile-init)))
               (not (profile-current-get 'profile-inited)))
      (unless (profile-current-get 'tags-dir)
        (profile-current-put 'tags-dir (profile--compute-tags-dir)))
      (message "drh on profile init tags-dir %s"
               (profile-current-get 'tags-dir))
      (profile-current-put 'project-root-stem
                           (profile--compute-project-stem root))
      (profile-current-funcall 'on-profile-init root)
      (profile-current-put 'profile-inited t))))

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
           (curr (profile-current-get 'project-root-dir))
           (root-file (car root))
           (root-dir (cdr root))
           profile-basename)
      (message "profile-dbg: root is %s, curr is %s" root curr) ;drh
      (when (and root root-file root-dir)
        (setq profile-basename (profile-find-profile-basename root-file))
        (unless (string-equal root-dir (profile-current-get 'project-root-dir))
          ;; this profile has not been loaded before
          (message "profile-dbg root-dir was %s, not %s" root-dir
                   (profile-current-get 'project-root-dir)) ;drh
          (if (string-match "\\.[er]prof$" root-file)
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
                (unless (profile-current-get 'project-root-dir)
                  (profile-current-put 'project-root-dir root-dir))
                (unless (profile-current-get 'project-name)
                  (profile-current-put 'project-name profile-basename))
                (add-to-list 'sml/replacer-regexp-list
                             (list (concat "^"
                                           (abbreviate-file-name
                                            (profile-current-get
                                             'project-root-dir)))
                                   (concat ":"
                                           (upcase
                                            (profile-current-get
                                             'project-name))
                                           ":")) t)
                (message "Loaded profile %s located at %s"
                         (profile-current-get 'project-name)
                         (profile-current-get 'project-root-dir))
                )
            ;; new profile, but not from a .profile
            (profile-current-put 'project-root-dir root-dir)
            )))
      (profile--on-profile-init))))

(provide 'profiles+)

;;; profiles+.el ends here
