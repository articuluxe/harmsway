;; profiles+.el --- Extensions to 'profiles.el'
;; Copyright (C) 2015, 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-05-19 07:46:47 dharms>
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

(defun profile-soft-reset ()
  "Reset the current profile to the default value.
This does not otherwise remove the profile itself from memory."
  (interactive)
  ;; kill-local-variable won't do it due to permanent-local property
  (setq profile-current
        (default-value 'profile-current)))

(defun profile--remove-profile (profile)
  "Delete the profile PROFILE, which can be a symbol or string (name)."
  (unintern profile profile-obarray))

(defun profile--delete-profile-from-alist (profile)
  "Remove a profile PROFILE from the internal data structure."
  (setq profile-path-alist
        (seq-remove
         (lambda(elt)
           ;; string-equal handles a symbol using its print-name
           (string-equal (cdr elt) profile))
         profile-path-alist)))

(defun profile-hard-reset ()
  "Remove all traces of current profile."
  (interactive)
  (profile--delete-profile-from-alist profile-current)
  (profile--remove-profile profile-current)
  (profile-soft-reset)
  )

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
            (if (and path (f-absolute? path))
                path
              (concat (profile-current-get 'project-root-dir) path)))
          (mapcar 'cadr (profile-current-get 'ctags-alist))))

;;;###autoload
(defun profile-set-grep-dirs ()
  "Set include directory settings useful for grep, according to the
current profile."
  (profile-current-put 'grep-dirs
                       (delete-dups
                        (append
                         (profile-collect-grep-dirs)
                         (list (profile-current-get 'project-root-dir))))))

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
                       (f-short (profile-current-get 'project-root-dir)) path))
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
     ?c (cons 'file (car (profile-current-get 'grep-dirs)))))
  )

(defun profile--gather-compiler-includes (compiler)
  "Return a list of include directories for COMPILER.  They will be absolute."
  (let ((cmd (concat "echo | " compiler " -v -x c++ -E - 2>&1 | "
                     "grep -A 20 starts | grep include | grep -v search")))
    (split-string (shell-command-to-string cmd))))
(defvar profile-clang-standard-version "c++14")
(defvar profile-gcc-standard-version "c++14")

;;;###autoload
(defun profile-on-c-file-open (project-root)
  "Helper function to perform the typical actions desired when a
c-language file is opened and a profile is active.  These typical
actions include setting include directories."
  (when (and (boundp 'c-buffer-is-cc-mode) c-buffer-is-cc-mode)
    (set (make-local-variable 'achead:include-directories)
         (profile-current-get 'include-files))
    ;; set 'compiler-include-dirs for ac-clang
    (when (executable-find "clang")
      (or (profile-current-get 'compiler-include-dirs)
          (profile-current-put
           'compiler-include-dirs
           (mapcar (lambda(x) (concat "-I" x))
                   (profile--gather-compiler-includes
                    (or (getenv "CXX") "g++")))))
      (set (make-local-variable 'ac-clang-flags)
           (append
            `(,(concat "-std=" profile-clang-standard-version)
              "-code-completion-macros" "-code-completion-patterns")
            (mapcar (lambda(x) (concat "-I" (expand-file-name x)))
                    (profile-current-get 'include-files))
            (list `,(concat "-I"
                            (profile-current-get 'remote-prefix)
                            (directory-file-name
                             (expand-file-name
                              (profile-current-get 'project-root-dir)))))
            (profile-current-get 'compiler-include-dirs)
            )))
    ;; set flycheck for c++
    (when (eq major-mode 'c++-mode)
      (if (executable-find "clang")
          (progn                        ;clang
            (set (make-local-variable 'flycheck-clang-language-standard)
                 profile-clang-standard-version)
            (set (make-local-variable 'flycheck-clang-standard-library)
                 "libc++")
            (set (make-local-variable 'flycheck-clang-include-path)
                 (profile-current-get 'include-files))
            (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
            )
        ;; gcc
        (set (make-local-variable 'flycheck-gcc-language-standard)
             profile-gcc-standard-version)
        (set (make-local-variable 'flycheck-gcc-include-path)
             (profile-current-get 'include-files))
        (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
        )))
  (setq ff-search-directories
        ;; current dir, include dirs, project root
        (append '(".")
                (profile-current-get 'include-ff-files)
                (list
                 `,(concat
                    (profile-current-get 'remote-prefix)
                    (directory-file-name
                     (expand-file-name
                      (profile-current-get 'project-root-dir))))))))

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
    (message "drh profile-find-root found root:%s, dir:%s, absolute:%s" root dir absolute)
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
(defun profile--on-file-opened ()
  "Initialize a file after opening and assigning a profile."
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

(defun profile--abbreviate-remote-root (remote-name)
  "Abbreviate REMOTE-NAME, a remote project root, as necessary.
It is usually preferable to have a short project prefix.  This
may just come down to substituting `~' for the home directory.
Note that `abbreviate-file-name' doesn't work for remote paths."
  (let ((home
         (string-trim (shell-command-to-string "echo ~"))))
    (replace-regexp-in-string home "~" remote-name t)))

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
      (message "drh set tags-dir:%s" (profile-current-get 'tags-dir))
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
  (profile--init buf filename))

(defun profile--init (buffer filename)
  "Initialize a profile, in BUFFER, visiting FILENAME."
  (interactive "bChoose a buffer: \nfChoose a file: ")
  (with-current-buffer buffer
    (make-local-variable 'profile-current)
    (put 'profile-current 'permanent-local t)
    (setq profile-current
          (intern-soft (profile-find-path-alist
                        (expand-file-name filename)) profile-obarray))
    (message "drh profile-init filename:%s dir:%s" filename (file-name-directory filename))
    (let* ((root (profile-find-root (file-name-directory filename) t)) ;drh
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
        ;; apparently this is a new profile not yet initialized
        (load-file root-file)
        (setq profile-basename
              (profile-find-profile-basename root-file))
        (message "drh Loading profile host:%s prefix:%s remotename:%s ~:%s HOME:%s root-dir:%s regexp:%s"
                 remote-host remote-prefix remote-localname
                 (shell-command-to-string "echo ~")
                 (getenv "HOME") root-dir (file-relative-name root-dir "~"))
        (add-to-list 'profile-path-alist (cons root-dir profile-basename))
        (setq profile-current
              (intern-soft (profile-find-path-alist
                            (expand-file-name filename))
                           profile-obarray))
        (unless (profile-current-get 'project-root-dir)
          (profile-current-put 'project-root-dir root-dir))
        ;; in case root-dir was listed in the .eprof file as relative
        (when (f-relative? (profile-current-get 'project-root-dir))
          (profile-current-put 'project-name (f-long (profile-current-get 'project-root-dir))))
        (unless (profile-current-get 'project-name)
          (profile-current-put 'project-name profile-basename))
        )
      (profile--on-profile-init remote-host remote-prefix))))

(provide 'profiles+)

;;; profiles+.el ends here
