;; profiles+.el --- Extensions to 'profiles.el'
;; Copyright (C) 2015, 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-06-10 11:15:20 dan.harms>
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

(require 'f)
(require 'profiles)
(require 'tramp)
(require 'custom-environment)
(require 'bookmark+)

;; disable the base class file
(setq profile-path-alist-file nil)

(defun profile-define-derived (profile parent &optional name mail &rest plist)
  "Create PROFILE with PARENT as parent, and NAME and MAIL.
Unlike `profile-define', if there is an existing profile with the
same NAME, it is NOT overridden (the existing plist is left
alone).  This is to support project files situated in project
hierarchies.  PROFILE, NAME and MAIL are all required to be
string values.  Optional argument PLIST is a property list.  The
new profile shares the properties of its parent, unless it
chooses to override any of them."
  (unless (intern-soft profile profile-obarray)
    (setplist (intern profile profile-obarray)
              (append (list 'name name 'mailing-address mail 'parent parent)
                      plist))))

(defun profile-lookup-property-polymorphic (profile property)
  "Lookup in PROFILE the value of PROPERTY.
If not found, and PROFILE has a parent, lookup PROPERTY in one of
its parents."
  (let ((val (get profile property))
        (parent-name (get profile 'parent)))
    (or val
        (and parent-name
             (intern-soft parent-name profile-obarray)
             (profile-lookup-property-polymorphic
              (intern-soft parent-name profile-obarray) property)))))

(defun profile-current-get (property &optional ignore-parent)
  "Return the value of PROPERTY for current profile `profile-current'.
IGNORE_PARENT should be non-nil to avoid checking for the
property up the inheritance tree.  The returned property is not
evaluated.  This overrides the function in `profiles.el'."
  (let ((val (get profile-current property)))
    (or val
        (and (null ignore-parent)
             (profile-lookup-property-polymorphic profile-current property)))))

(defun profile-current-funcall (property project-root-dir)
  "Return function call of PROPERTY for `profile-current'.
PROJECT-ROOT-DIR is the root dir of the profile."
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

(defun profile-hard-reset (&optional profile)
  "Remove all traces of PROFILE."
  (interactive)
  (let ((prof (or profile profile-current)))
    (profile--delete-profile-from-alist prof)
    (profile--remove-profile prof)
    (profile-soft-reset)
    ))

(define-error 'profile-error "Profile+ error")
(define-error 'profile-error-non-fatal "Profile load stopped" 'profile-error)
(define-error 'profile-error-aborted "Profile load aborted" 'profile-error)

(defvar profile--ignore-load-errors nil
  "Internal variable is non-nil if user desires errors to be skipped.")

(defun profile--query-error (profile err)
  "Ask user what to do if error ERR occurs while loading PROFILE."
  (interactive)
  (let ((buf (get-buffer-create "*Profile Error*")))
    (with-current-buffer buf
      (erase-buffer)
      (toggle-truncate-lines -1)
      (insert "An error occurred while loading profile \""
              (symbol-name profile)
              "\":\n\n"
              err
              "\n\nWould you like to continue loading this profile?  Please select:\n\n"
              " (y) Continue loading profile, ignoring this error\n"
              " (!) Continue loading profile, ignoring this and future errors\n"
              " (n) Stop loading profile\n"
              " (a) Abort loading of profile, and revert profile load\n"
              ))
    (pop-to-buffer buf)
    (let ((choices '(?y ?n ?a ?!))
          (prompt "Please type y, n, ! or a: ")
          ch)
      (while (null ch)
        (setq ch (read-char-choice prompt choices)))
      (quit-window t)
      (cond ((eq ch ?n)
             (signal 'profile-error-non-fatal err))
            ((eq ch ?a)
             (profile-hard-reset profile)
             (signal 'profile-error-aborted
                     (format "Aborted (and reset) profile \"%s\" (%s)"
                             (symbol-name profile) err)))
            ((eq ch ?!)
             (setq profile--ignore-load-errors t))
            ))
  nil))

(defun profile-load-environment-file (name)
  "Load environment from file NAME, if it exists in the profile's root."
  (interactive)
  (let* ((root (profile-current-get 'project-root-dir))
         (prefix (profile-current-get 'remote-prefix))
         (file (concat prefix root name)))
    (setenv "REPO_ROOT" root)
    (when (f-exists? file)
      (load-environment-variables-from-file file))))

(defun profile-load ()
  "Load a profile after its data structures have been set up."
  (interactive)
  (let ((profile profile-current)
        (name (symbol-name profile-current)))
    (condition-case err
        (progn
          (profile-validate-include-files profile)
          (gen-tags-set-tags-table name)
          (profile-set-include-files name)
          (profile-set-grep-dirs name)
          (profile-set-mode-line-from-include-files name)
          (profile-set-sml-and-registers-from-build-sub-dirs name)
          (bmkp-switch-bookmark-file-create
           (concat
            (profile-get name 'remote-prefix)
            (profile-get name 'project-root-dir)
            "repo.bmk") t)
          )
      ('profile-error-non-fatal
       (profile-put name 'profile-inited nil)
       (message "Stopped loading profile \"%s\" (%s)" name (cdr err)))
      ((profile-error-aborted profile-error)
       (ignore-errors
         (profile-put name 'profile-inited nil))
       (error (cdr err)))
    )))

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

(defun profile-find-profile-basename (name)
  "Return profile NAME's basename.
Given a typical profile file such as `.mybase.eprof', returns `mybase'."
  (let ((base (file-name-base name)))
    (when
        (string-match "\\.?\\(.*\\)" base)
      (match-string 1 base))))

(defun profile-validate-include-files (profile)
  "Validate the set of include files of profile PROFILE."
  (interactive)
  (let* ((name (symbol-name profile))
         (remote (profile-get name 'remote-prefix))
         (root (profile-get name 'project-root-dir))
         (lst (profile-get name 'ctags-alist))
         entry path)
    (setq profile--ignore-load-errors nil)
    (setq lst
          (seq-filter (lambda (elt)
                        (setq entry (cadr elt))
                        (setq path
                              (concat remote
                                      (concat
                                       (when (or (zerop (length entry))
                                                 (f-relative? entry))
                                         root)
                                       entry)))
                        (cond ((null entry) nil)
                              ((f-exists? path) path)
                              (profile--ignore-load-errors nil)
                              (t
                               (profile--query-error
                                profile
                                (format "%s does not exist!" path)))))
                      lst))
    (profile-put name 'ctags-alist lst)))

(defun profile-collect-include-files (lst profile)
  "Extract include directories from LST, for profile PROFILE.
LST is in the format of a list of lists of properties, see
`ctags-alist'.  Return a list of the results.  Each entry is a
cons cell (`fullpath' . `path') where `fullpath' prepends a
remote prefix, if one exists."
  (let ((remote (profile-get profile 'remote-prefix))
        (root (profile-get profile 'project-root-dir))
        res)
    (mapcar (lambda(path)
              (setq res
                    (concat
                     (when (or (null path) (f-relative? path)) root)
                     path))
              (cons (concat remote res) res))
            (mapcar 'cadr lst))))

(defun profile-set-include-files (profile)
  "Set useful include file settings for use in programming modes.
The current profile is PROFILE."
  (let ((lst (profile-collect-include-files
              (profile-get profile 'ctags-alist) profile)))
    (profile-put profile 'include-files (mapcar 'cdr lst))
    (profile-put profile 'include-ff-files
                         ;; ff-search-directories doesn't need a trailing
                         ;; slash
                         (mapcar 'directory-file-name
                                 (mapcar 'car lst)))))

(defun profile-collect-grep-dirs (lst root)
  "Extract list of include directories suitable for `grep' actions.
LST is a list in the format of `ctags-alist'.  ROOT is the root
directory of the current profile."
  (mapcar (lambda (path)
            (if (and path (f-absolute? path))
                path
              (concat root path)))
          (mapcar 'cadr lst)))

(defun profile-set-grep-dirs (profile)
  "Set include directory settings useful for `grep'.
PROFILE is the current profile."
  (let ((root (profile-get profile 'project-root-dir)))
    (profile-put profile 'grep-dirs
                 (delete-dups
                  (append
                   (profile-collect-grep-dirs
                    (profile-get profile 'ctags-alist)
                    root)
                   (list root))))))

(defun profile-collect-sml-regexps (lst profile root)
  "Extract a list of cons cells representing a modeline replacement pair.
LST is a list of lists of properties, see `ctags-alist'.  PROFILE is the
current profile.  ROOT is the current profile root.  The return value
will be a list cons cells, see `sml/replacer-regexp-list'."
  (let (path title)
    (mapcar (lambda(elt)
              (setq path (cadr elt))
              (setq title (car elt))
              (cons
               (when path
                 (if (f-absolute? path) path
                   (f-short path)))
               (concat (upcase title) ":")))
            lst)))

(defun profile-set-mode-line-from-include-files (profile)
  "Set useful mode line abbreviations according to profile PROFILE.
See `sml/replacer-regexp-alist'."
  (let* ((root (profile-get profile 'project-root-dir))
         (sml-alist (profile-collect-sml-regexps
                     (profile-get profile 'ctags-alist)
                     profile root)))
    (mapc (lambda (elt)
            (when (car elt)
              (add-to-list 'sml/replacer-regexp-list
                           (list (car elt) (cdr elt)) t)))
          sml-alist)))

(defun profile-set-sml-and-registers-from-build-sub-dirs (profile)
  "Set sml modeline according to `build-sub-dirs' setting of profile PROFILE.
See `sml/replacer-regexp-alist'.  Also optionally set some
convenience registers to access the build-sub-dirs."
  (let ((sml-alist (profile-get profile 'build-sub-dirs))
        (root (profile-get profile 'project-root-dir))
        (remote (profile-get profile 'remote-prefix)))
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
                    reg (cons 'file (concat remote root dir))))
              ))
          sml-alist)
    ;; also, set a global register to go to the root dir
    (set-register ?r (cons 'file (concat remote root)))
    ;; and another to go to the first (priveleged) src-dir
    (when (< 0 (length (profile-get profile 'grep-dirs)))
      (set-register
       ?c (cons 'file (car (profile-get profile 'grep-dirs)))))))

(defun profile--gather-compiler-includes (compiler)
  "Return a list of include directories for COMPILER.  They will be absolute."
  (let ((cmd (concat "echo | " compiler " -v -x c++ -E - 2>&1 | "
                     "grep -A 20 starts | grep include | grep -v search")))
    (split-string (shell-command-to-string cmd))))
(defvar profile-clang-standard-version "c++14")
(defvar profile-gcc-standard-version "c++14")

(defun profile-on-c-file-open (project-root)
  "Initialize settings for a c-language file, given PROJECT-ROOT.
These typical actions include setting include directories."
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

(defun profile-find-root (dir &optional absolute)
  "Search for project root, starting from DIR and moving up the file tree.
ABSOLUTE if non-nil will return an absolute path.  Profile files
can look like any of the following: `.eprof', `my.eprof',
`.my.eprof'."
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
  (when (and dir (file-remote-p dir))
    (with-parsed-tramp-file-name dir file
      `( ,file-host ,file-localname
                    ,(tramp-make-tramp-file-name
                      file-method file-user file-host "")
                    ))))

(defun profile--compute-remote-subdir-stem ()
  "Compute a remote project's stem.
Format is useful for uniquely naming the local TAGS directory."
  (concat
   (replace-regexp-in-string
    "/\\|\\\\" "!" (profile-current-get 'remote-host) t t)
   "!"
   (replace-regexp-in-string
    "/\\|\\\\" "!" (profile-current-get 'project-root-stem))))

(defun profile--compute-tags-dir (dir)
  "Compute where a project's local TAGS live, with root DIR."
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
  "Compute a project's stem, useful in regular expression matching.
ROOT-DIR describes the profile's base directory.  The presumption
is that the ROOT-DIR is a relative path, possibly including a `~'
representing the user's home directory."
  (replace-regexp-in-string "~/" "" root-dir))

(defun profile--log-profile ()
  "Log a profile upon initialization."
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
  "Initialize a loaded profile.
REMOTE-HOST and REMOTE-PREFIX, if non-nil, pertain to the remote
path."
  (let ((root (profile-current-get 'project-root-dir)))
    (when (and profile-current
               (not (profile-current-get 'profile-inited t)))
      ;; run this init code once per profile loaded
      (profile-current-put 'profile-inited t)
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
        (profile-current-funcall 'on-profile-init root)))))

;; override the advice from `profiles.el'
(defadvice find-file-noselect-1
    (before before-find-file-noselect-1 activate)
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
    (let* ((root (profile-find-root (file-name-directory filename) t))
           (curr (profile-current-get 'project-root-dir))
           (root-file (car root))
           (root-dir (cdr root))
           (remote-properties
            (profile--compute-remote-properties
             root-dir))
           remote-host
           remote-localname remote-prefix
           profile-basename)
      (when remote-properties
        (setq remote-host (car remote-properties))
        (setq remote-localname (cadr remote-properties))
        (setq remote-prefix (caddr remote-properties)))
      (when (and root root-file root-dir
                 (string-match "\\.[er]prof$" root-file)
                 (not (string-equal root-dir
                                    (profile-current-get 'project-root-dir))))
        ;; apparently this is a new profile not yet initialized
        (load-file root-file)
        (setq profile-basename
              (profile-find-profile-basename root-file))
        (when remote-properties
          (setq root-dir remote-localname))
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
