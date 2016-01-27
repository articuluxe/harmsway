;; -*- Mode: Emacs-Lisp -*-
;; profiles+.el --- Extensions to 'profiles.el'
;; Copyright (C) 2015, 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-01-27 16:40:54 dan.harms>
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

;; disable the base class file
(setq profile-path-alist-file nil)

;;;###autoload
(defun profile-define-derived (profile parent &optional name mail &rest plist)
  "Create PROFILE with NAME and MAIL.  Unlike `profile-define',
if there is an existing profile with the same NAME, it is NOT
overridden (the existing plist is left alone).  This is to
support project files situated in project hierarchies.  PROFILE,
NAME and MAIL are all required to be string values.  Optional
argument PLIST is a property list.  The new profile shares the
properties of its parent, unless it chooses to override any of
them."
  (unless (intern-soft profile profile-obarray)
    (setplist (intern profile profile-obarray)
              (append (list 'name name 'mailing-address mail 'parent parent)
                      plist))))

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
(defun profile-open-dired-on-dir ()
  "Open a dired buffer on one of the directories listed in the profile.
These could come from various sources."
  (interactive)
  (let* ((cats (list "src" "build" "debug"))
         (cat (funcall my/choose-func cats "Choose among:"))
         choices dir path)
    (cond ((string= cat "src")
           (setq choices (mapcar
                          (lambda(dir)
                            (if (consp dir)
                                (car dir)
                              dir))
                          (profile-current-get 'grep-dirs))))
          ((string= cat "build")
           (setq choices (mapcar
                          'car
                          (profile-current-get 'build-sub-dirs))))
          ((string= cat "debug")
           (setq choices (profile-current-get 'debug-sub-dirs))))
    (unless choices
      (error "No %s directories available" cat))
    (setq dir (funcall my/choose-func choices "Open dired: "))
    (when dir
      (setq path
            (concat
             (profile-current-get 'remote-prefix)
             (unless (file-name-absolute-p dir)
               (profile-current-get 'project-root-dir))
             dir))
      (if (file-readable-p path)
          (dired path)
        (error "%s does not exist!" path)))))

;;;###autoload
(defun profile-find-profile-basename (name)
  "Given a typical profile file such as `.mybase.eprof', returns the
basename, such as `mybase'."
  (let ((base (file-name-base name)))
    (when
      (string-match "\\.?\\(.*\\)" base)
    (match-string 1 base))))

;;;###autoload
(defun profile-collect-include-files (alist &optional prepend-remote)
  "Extract the include directories from ALIST, which is in the format of a
list of lists of properties, see `ctags-alist'. Return a list of the
results."
  (mapcar (lambda(path)
            (let ((include
                   (if (file-name-absolute-p path)
                       path
                     (concat
                      (profile-current-get 'project-root-dir) path))))
              (setq path
                    (if prepend-remote
                        (concat (profile-current-get 'remote-prefix)
                                include)
                      include))))
          (mapcar 'cadr alist)))

;;;###autoload
(defun profile-set-include-files ()
  "Set useful include file settings for use in programming modes,
according to the current profile."
    (profile-current-put 'include-files
                         (profile-collect-include-files
                          (profile-current-get 'ctags-alist)))
    (profile-current-put 'include-ff-files
                         ;; ff-search-directories doesn't need a trailing
                         ;; slash
                         (mapcar 'directory-file-name
                                 (profile-collect-include-files
                                  (profile-current-get 'ctags-alist) t)))
    )

;;;###autoload
(defun profile-collect-grep-dirs ()
  "Extract the list of include directories according to the current
profile."
  (mapcar (lambda (path)
            (let ((full
                   (if (file-name-absolute-p path)
                       path
                     (concat
                      (profile-current-get 'project-root-dir) path))))
              (cons path full)))
          (mapcar 'cadr (profile-current-get 'ctags-alist))))

;;;###autoload
(defun profile-set-grep-dirs ()
  "Set include directory settings useful for grep, according to the
current profile."
  (profile-current-put 'grep-dirs
                       (append
                        (profile-collect-grep-dirs)
                        (list (profile-current-get 'project-root-dir)))))

;;;###autoload
(defun profile-collect-sml-regexps (alist)
  "Extract from ALIST, which is in the format of a list of lists of
properties, see `ctags-alist', a list of cons cells representing a
modeline replacement pair for sml, see `sml/replacer-regexp-list'."
  (mapcar (lambda(elt)
            (let ((path (cadr elt))
                  (title (car elt)))
              (cons (if (file-name-absolute-p path)
                        path
                      (concat
                       (profile-current-get 'project-root-dir) path))
                    (concat (upcase title) ":"))))
          alist))

;;;###autoload
(defun profile-set-mode-line-from-include-files ()
  "Set useful mode line abbreviations, see `sml/replacer-regexp-alist',
according to the current profile."
  (let ((sml-alist (profile-collect-sml-regexps
                    (profile-current-get 'ctags-alist))))
    (mapc (lambda (elt)
            (add-to-list 'sml/replacer-regexp-list
                         (list (car elt) (cdr elt)) t))
          sml-alist)))

;;;###autoload
(defun profile-set-sml-and-registers-from-build-sub-dirs ()
  "Set the sml mode line according to the build-sub-dirs setting of the
current profile.  See `sml/replacer-regexp-alist'.  Also optionally set
some convenience registers to access the build-sub-dirs."
  (let ((sml-alist (profile-current-get 'build-sub-dirs)))
    (mapc (lambda (elt)
            (let* ((dir (car elt))
                   (name (or (cadr elt)
                             (concat (upcase
                                      (directory-file-name dir)) ":")))
                   (reg (caddr elt)))
              (unless (= 0 (length dir))
                (add-to-list 'sml/replacer-regexp-list
                             (list dir name) t))
              (and reg (characterp reg)
                   (set-register
                    reg (cons 'file (concat
                                     (profile-current-get 'remote-prefix)
                                     (profile-current-get 'project-root-dir)
                                     dir))))
              ))
          sml-alist))
  ;; also, set a global register to go to the root dir
  (set-register ?r (cons 'file
                         (concat (profile-current-get 'remote-prefix)
                                 (profile-current-get 'project-root-dir))))
  ;; and another to go to the first (priveleged) src-dir
  (when (< 0 (length (profile-current-get 'grep-dirs)))
    (set-register
     ?c (cons 'file (cdr (car (profile-current-get 'grep-dirs))))))
  )

;;;###autoload
(defun profile-on-c-file-open (project-root)
  "Helper function to perform the typical actions desired when a
c-language file is opened and a profile is active.  These typical
actions include setting include directories."
  (when c-buffer-is-cc-mode
    (set (make-local-variable 'achead:include-directories)
         (profile-current-get 'include-files)))
  (setq ff-search-directories
        ;; current dir, include dirs, project root
        (append '(".")
                (profile-current-get 'include-ff-files)
                (list
                 `,(concat
                    (profile-current-get 'remote-prefix)
                    (directory-file-name (profile-current-get 'project-root-dir)))))))

(defvar profile--root-file nil)
;;;###autoload
(defun profile-find-root (dir &optional absolute)
  "Searches for the project root as may be defined in current profile,
starting from DIR and moving up the directory tree.  Profile files can look
like any of the following: `.eprof', `my.eprof', `.my.eprof'."
  (let ((root
         (if (<= 24 emacs-major-version)
             (locate-dominating-file
              dir
              (lambda(parent)
                (let ((res (directory-files parent t "\\sw+\\.eprof$")))
                  (when res
                    (setq profile--root-file (car res))))))
           (find-file-upwards dir "\\sw+\\.eprof$")
           )))
    (if root
        (if absolute
            (cons profile--root-file (expand-file-name root))
          (cons profile--root-file root))
      (setq profile--root-file nil))))

(defun profile--real-file-name (filename)
  "Return the tag's correct destination file, FILENAME.  This may prepend a
remote prefix."
  (let ((file (if (file-name-absolute-p filename)
                  filename
                (concat (profile-get tag-lookup-target-profile
                                     'project-root-dir)
                        (profile-get tag-lookup-target-profile
                                     'src-sub-dir)))))
    (concat (profile-get tag-lookup-target-profile 'remote-prefix)
            file)))
(setq etags-select-real-file-name 'profile--real-file-name)

(defun profile--insert-file-name (filename tag-file-path)
  (if (file-name-absolute-p filename)
      filename
    (concat (profile-get tag-lookup-target-profile
                         'project-root-dir)
            (profile-get tag-lookup-target-profile
                         'src-sub-dir)
            filename)
    ))
(setq etags-select-insert-file-name 'profile--insert-file-name)

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

(defun profile--compute-remote-properties (dir)
  "Compute the properties associated with DIR, a (possibly remote) filename."
  (when (file-remote-p dir)
    (with-parsed-tramp-file-name dir file
      `( ,file-host ,file-localname
                   ,(tramp-make-tramp-file-name
                     file-method file-user file-host "")
                   ))))

(defun profile--compute-remote-subdir-stem ()
  "Helper function that computes a remote project's stem in a format
useful for uniquely naming the local TAGS directory."
  (concat
   (replace-regexp-in-string
    "/\\|\\\\" "!" (profile-current-get 'remote-host) t t)
   "!"
   (replace-regexp-in-string
    "/\\|\\\\" "!" (profile-current-get 'project-root-stem))))

(defun profile--compute-tags-dir (dir)
  "Helper function that computes where a project's local TAGS live."
  (let ((base (or (getenv "EMACS_TAGS_DIR") "~"))
        (sub (or (profile-current-get 'tags-sub-dir) ".tags/"))
        dest-dir)
    (unless dir (setq dir default-directory))
    (when (not (tramp-tramp-file-p dir))
      ;; in the local case, set our base according to the project
      (setq base (profile-current-get 'project-root-dir)))
    (setq dest-dir (concat (file-name-as-directory base) sub))
    (if (tramp-tramp-file-p dir)
        (concat dest-dir
                (file-name-as-directory
                 (profile--compute-remote-subdir-stem)))
      dest-dir)))

(defun profile--compute-project-stem (root-dir)
  "Helper function that computes a project's stem, useful in regular
expression matching.  The presumption is that the ROOT-DIR is a relative
path, possibly including a `~' representing the user's home directory."
  (replace-regexp-in-string "~/" "" root-dir))

;; log a profile upon initialization
(defun profile--log-profile ()
  (let ((name (symbol-name profile-current)))
    (unless (string-equal name "default")
      (message "Loaded profile %s (project %s) at %s"
               name
               (profile-current-get 'project-name)
               (profile-current-get 'project-root-dir)
               ))))

;; called when a profile is initialized
(defun profile--on-profile-init (remote-host remote-prefix)
  "Initialize a loaded profile."
  (let ((root (profile-current-get 'project-root-dir)))
    (when (and profile-current
               (not (profile-current-get 'profile-inited t)))
      ;; run this init code once per profile loaded
      (when remote-host
        (profile-current-put 'remote-host remote-host))
      (when remote-prefix
        (profile-current-put 'remote-prefix remote-prefix))
      (when root
        (unless (profile-current-get 'project-root-stem)
          (profile-current-put 'project-root-stem
                               (profile--compute-project-stem root))))
      (when root
        (unless (profile-current-get 'tags-dir)
          (profile-current-put
           'tags-dir
           (profile--compute-tags-dir
            (concat remote-prefix root)))))
      (profile--log-profile)
      ;; if there's a valid init function, call it
      (when (and (profile-current-get 'on-profile-init)
                 (fboundp (intern-soft
                           (profile-current-get 'on-profile-init))))
        (profile-current-funcall 'on-profile-init root))
      (profile-current-put 'profile-inited t))))

;; override the advice from `profiles.el'
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
           (remote-properties
            (profile--compute-remote-properties
             (or root-dir default-directory)))
           remote-host
           remote-localname remote-prefix
           profile-basename)
      (when remote-properties
        (setq remote-host (car remote-properties))
        (setq remote-localname (cadr remote-properties))
        (setq remote-prefix (caddr remote-properties)))
      (when (and root root-file root-dir
                 (string-match "\\.eprof$" root-file)
                 (not (string-equal root-dir
                                    (profile-current-get 'project-root-dir))))
        ;; apparently this is a new profile not yet loaded into the path
        (load-file root-file)
        ;; todo: this doesn't work; when remote, project-root-dir is
        ;; absolute, and abbreviate-file-name doesn't transform remote
        ;; names
        (when remote-properties
          (setq root-dir                ;HACK!!!!!
                (replace-regexp-in-string
                 "/home/dan.harms" ;(shell-command "echo ~")
                 "~" remote-localname t)))
        (setq profile-basename
              (profile-find-profile-basename root-file))
        ;; update the path alist to activate any new profiles
        (add-to-list 'profile-path-alist
                     (cons (file-relative-name root-dir "~")
                           profile-basename))
        (setq profile-current
              (intern-soft (profile-find-path-alist
                            (expand-file-name filename))
                           profile-obarray))
        (unless (profile-current-get 'project-root-dir)
          (profile-current-put 'project-root-dir root-dir))
        (unless (profile-current-get 'project-name)
          (profile-current-put 'project-name profile-basename))
        )
      (profile--on-profile-init remote-host remote-prefix))))

(provide 'profiles+)

;;; profiles+.el ends here
