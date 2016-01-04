;; -*- Mode: Emacs-Lisp -*-
;; init.el --- Initialization file
;; Copyright (C) 2015, 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Friday, February 27, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-01-04 12:41:22 dan.harms>
;; Modified by: Dan Harms
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; load-path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (defvar my/user-directory (expand-file-name
                             (if (boundp 'user-emacs-directory)
                                 user-emacs-directory
                               "~/.emacs.d/")))
  (defvar my/scratch-directory (concat my/user-directory "etc/"))
  (defvar my/elisp-directory (concat my/user-directory "elisp/"))
  (defvar my/plugins-directory (concat my/user-directory "plugins/"))
  (add-to-list 'load-path my/plugins-directory)
  (add-to-list 'load-path my/elisp-directory)
  (add-to-list 'load-path (concat my/user-directory "modes/"))
  (add-to-list 'load-path (concat my/user-directory "custom/"))
  )
(defvar my/user-settings
  (concat my/user-directory "settings/user/" user-login-name))
(load my/user-settings t)

(set-register ?~ (cons 'file "~/"))
(set-register ?\C-i (cons 'file user-init-file)) ;edit init file
(set-register ?\C-d (cons 'file "~/Documents"))
(set-register ?\C-k (cons 'file "~/Desktop"))
(set-register ?\C-w (cons 'file "~/Downloads"))
(set-register ?\C-s (cons 'file "~/src"))
(set-register ?\C-h (cons 'file "~/src/harmsway"))
(set-register ?\C-e (cons 'file "~/src/harmsway/.emacs.d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-save ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/autosave-dir (concat my/user-directory "autosaves/"))
(unless (file-directory-p my/autosave-dir)
  (make-directory my/autosave-dir t))
(setq auto-save-file-name-transforms
      `((".*" ,(concat my/autosave-dir "\\1") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/backup-dir
  (concat my/user-directory "backups/" (format-time-string "%Y-%m-%d")))
(unless (file-directory-p my/backup-dir)
  (make-directory my/backup-dir t))
(setq backup-directory-alist `(("." . ,my/backup-dir)))
(setq delete-by-moving-to-trash t)
(setq backup-by-copying t
      version-control t
      delete-old-versions t
      kept-old-versions 0               ;oldest versions to keep
      kept-new-versions 10
      auto-save-timeout 60
      auto-save-interval 0              ;disable autosaves due to input events
      )

;; Suppress GNU startup message
(setq inhibit-startup-message t)
(setq inhibit-default-init t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq use-dialog-box nil)
(setq gc-cons-threshold 20000000)
(setq kill-do-not-save-duplicates t)
(file-name-shadow-mode 1)
(setq enable-recursive-minibuffers t)
(setq isearch-allow-scroll t)
;;  truncate long lines
(setq-default truncate-lines t)
;; search is case-sensitive by default
(setq-default case-fold-search nil)
;; interactive regexp-search space character stands only for 1 char
(setq-default search-whitespace-regexp nil)
;; default tab width
(setq-default tab-width 4)
;; Show selections
(transient-mark-mode 1)
;; show current function
(which-function-mode t)
;; Insertion while text is selected deletes the selected text
(delete-selection-mode 1)
;; winner mode
(winner-mode 1)
;; append unique parent directory to buffers of same name
(toggle-uniquify-buffer-names)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; don't try to create "other files"
(setq ff-always-try-to-create nil)
;; Preserve line position on scroll
(setq scroll-preserve-screen-position t)
;; Ignore case when completing file names
(setq read-file-name-completion-ignore-case nil)
(show-paren-mode t)
(size-indication-mode 1)
;; don't add new-lines to end of buffer on scroll
(setq next-line-add-newlines nil)
;; allow converting regions to upper/lower case
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; disable nuisances
(put 'overwrite-mode 'disabled t)
(fset 'yes-or-no-p 'y-or-n-p)
;; reuse frames
(setq-default display-buffer-reuse-frames t)

;; visual settings
(menu-bar-mode -1)
(setq-default fill-column 78)
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
;; default colors
;; (set-foreground-color "white")
;; (set-background-color "black")
(set-cursor-color "yellow")
(set-mouse-color "white")
(mouse-avoidance-mode 'cat-and-mouse)
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(setq ring-bell-function (lambda() ()))
(when (display-graphic-p)
  (global-unset-key "\C-z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fonts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(defun my/syntax-color-hex-values()
  "Syntax color text of the form #FF1100 in a buffer.
Cf. `http://ergoemacs.org/emacs/emacs_CSS_colors.html'."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[AaBbCcDdEeFf[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; key-bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "<f1>"))
(global-set-key [(next)] 'scroll-up-line)
(global-set-key [(prior)] 'scroll-down-line)
(global-set-key "\C-x\C-r" (lambda()(interactive)(revert-buffer nil t)))
(global-set-key "\C-caa" 'align)
(global-set-key "\C-car" 'align-repeat-regexp)
(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key "\C-c5" 'toggle-truncate-lines)
(global-set-key "\C-c " 'whitespace-mode)
(global-set-key "\C-cf" 'font-lock-fontify-buffer)
(global-set-key "\e\es" 'speedbar)
(global-set-key "\e\eo" 'speedbar-get-focus)
(global-set-key "\M-sf" 'ff-find-other-file)
(global-set-key (kbd "M-#") 'sort-lines)
(global-set-key (kbd "C-#") 'sort-paragraphs)
(global-set-key "\C-xw" 'write-region)

;; This horrible hack gets around a "reference to free variable" warning,
;; I believe due to a defadvice referring to `filename' in the original
;; code being advised.  But I couldn't find where.
;; More recent emacsen seem to handle the error.
(when (version< emacs-version "24.3")
  (defvar filename nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dash ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (when (version< "24.3" emacs-version)
    (require 'dash)
    (eval-after-load "dash" '(dash-enable-font-lock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; s ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (version< "24.3" emacs-version)
  (require 's))

(require 'deferred)
(require 'concurrent)
(require 'epc)
(require 'epcs)

;;;;;;; FUNCTIONS ;;;;;;;
;; ; man-page lookups
;; (defun openman () "lookup man page" (interactive)
;;    (manual-entry (current-word)))
;; ; convert tabs to spaces
;; (defun untabify-buffer () "untabify current buffer" (interactive)
;;    (save-excursion
;;      (untabify (point-min) (point-max))))
;; ; shell command on region
;; (defun shell-command-on-buffer (cmd) (interactive "sShell command on buffer: ")
;;   (shell-command-on-region (point-min) (point-max) cmd t t
;; ))



(load-library "utils")
(load-library "compiling")
(load-library "coding")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'log-viewer)
(require 'csv-mode)
(require 'cmake-mode)
(require 'cmake-font-lock)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
;; work around bug in cc-mode in emacs 24.4
;; see debbugs.gnu.org/db/18/18845.html
(eval-and-compile
  (when (< emacs-major-version 24)
    (add-to-list 'load-path (concat my/elisp-directory "compat/24/0/-/")))
  (when (< emacs-major-version 25)
    (add-to-list 'load-path (concat my/elisp-directory "compat/25/0/-/")))
  )
(eval-when-compile
  (if (and (= emacs-major-version 24) (= emacs-minor-version 4))
      (require 'cl)))
(if (= emacs-major-version 23)
	(progn
	  (autoload 'protobuf-mode "protobuf-mode" "Major mode for editing protobuf files." t)
	  (autoload 'csharp-mode "csharp-mode" "Major mode for editing csharp files." t)
	  )
  (require 'protobuf-mode)
  (require 'csharp-mode)
  )

(require 'dart-mode)
(require 'dos)
(require 'dos-indent)
(require 'folio-mode)
(require 'folio-electric)
(require 'markdown-mode)
(require 'pos-tip)
(require 'qt-pro)
(when (version< emacs-version "23")
  (require 'json-mode))
(require 'nhexl-mode)
;; git
(require 'gitignore-mode)
(require 'gitconfig-mode)
(require 'gitattributes-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rainbow ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (version< "24.3" emacs-version)
  (require 'rainbow-mode)
  ;; enable for emacs-lisp-mode
  (add-to-list 'rainbow-html-colors-major-mode-list 'emacs-lisp-mode)
  (add-to-list 'rainbow-x-colors-major-mode-list 'emacs-lisp-mode)
  (add-to-list 'rainbow-ansi-colors-major-mode-list 'emacs-lisp-mode)
  (add-to-list 'rainbow-r-colors-major-mode-list 'emacs-lisp-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rotate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rotate)
(define-key ctl-x-4-map "l" 'rotate-layout)
(define-key ctl-x-4-map "w" 'rotate-window)
(define-key ctl-x-4-map "h" 'rotate:even-horizontal)
(define-key ctl-x-4-map "\C-h" 'rotate:main-horizontal)
(define-key ctl-x-4-map "v" 'rotate:even-vertical)
(define-key ctl-x-4-map "\C-v" 'rotate:main-vertical)
(define-key ctl-x-4-map "t" 'rotate:tiled)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ibuffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ibuffer)
;; Newer "tabulated list mode" in buff-menu.el breaks buffer-menu+ 21.0.
;; We'll shadow the current buff-menu.el with our local emacs-23 version.
(when (version<= "24.2" emacs-version)
  (load "buff-menu.el"))
(require 'buff-menu+)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-x\S-b" 'electric-buffer-list)
;; human-readable sizes
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1048576)
    (format "%7.1fM" (/ (buffer-size) 1048576.0)))
   ((> (buffer-size) 131072)
    (format "%7.0fk" (/ (buffer-size) 1024.0)))
   ((> (buffer-size) 1024)
    (format "%7.1fk" (/ (buffer-size) 1024.0)))
   (t (format "%8d" (buffer-size)))))
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)
        (mark modified read-only " "
              (size-h 9 -1 :right)
              " "
              (name 26 -1))
        (mark modified read-only " "
              (size-h 9 -1 :right)
              " "
              (filename-and-process 26 -1))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; select
(require 'etags-select)
(setq tags-revert-without-query t)
(require 'gen-tags)
(global-set-key "\C-ct" 'gen-tags-generate-tags)
(defvar tag-lookup-target-profile nil
  "The working profile in effect when a tag is first looked up.")
(defun my/store-profile ()
  (setq tag-lookup-target-profile (symbol-name profile-current)))
(global-set-key "\M-." (lambda()(interactive)
                         (my/store-profile)
                         (etags-select-find-tag)))
(global-set-key [?\C-\M-.] (lambda()(interactive)
                             (my/store-profile)
                             (etags-select-find-tag-at-point)))
;; show stack
(require 'etags-stack)
(global-set-key "\C-c\C-t" 'etags-stack-show)

;; table
(require 'etags-table)
;; we store our tags in a specific directory
(setq etags-table-search-up-depth nil)

;; This (unused) snippet overrides tag lookup for standard tags, not
;; etags-select.  This would (untested) make tag lookup tramp-aware.  See
;; emacs.stackexchange.com/questions/53/ctags-over-tramp
;; (defun my/etags-file-of-tag (&optional relative)
;;   (save-excursion
;;     (re-search-backward "\f\n\\([^\n]+\\),[0-9]*\n")
;;     (let ((str (convert-standard-filename
;;                 (buffer-substring (match-beginning 1) (match-end 1)))))
;;       (if relative str
;;         (let ((basedir (file-truename default-directory)))
;;           (if (file-remote-p basedir)
;;               (with-parsed-tramp-file-name basedir nil
;;                 (message "drh *** str=%s basedir=%s result=%s"
;;                          str basedir
;;                          (expand-file-name
;;                           (apply 'tramp-make-tramp-file-name
;;                                  (list method user host str hop))))
;;                 (expand-file-name (apply 'tramp-make-tramp-file-name
;;                                          (list method user host str hop))))
;;             (expand-file-name str basedir)))))))
;; (setq file-of-tag-function 'my/etags-file-of-tag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yascroll ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yascroll)
(global-yascroll-bar-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; copyright ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'copyright)
;; copyright-update is added to my/before-save-hook below
(setq copyright-query nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hide-lines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'hide-lines)
(global-set-key "\C-c\\" 'hide-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; line-comment-banner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'line-comment-banner)
(global-set-key [?\C-c?\C-/] 'line-comment-banner)
(add-hook 'c-mode-common-hook
          (lambda() (make-local-variable 'comment-fill)
            (setq comment-fill "*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; list-register ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'list-register)
(global-set-key (kbd "\C-xrv") 'list-register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; discover-my-major ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'discover-my-major)
(global-set-key (kbd "C-h C-m") 'discover-my-major)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; iedit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'iedit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; aes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aes)
(setq aes-always-ask-for-passwords nil)
(setq aes-enable-plaintext-password-storage t)
(setq aes-delete-passwords-after-idle 0)
(aes-enable-auto-decryption)
(defvar my/aes-default-group "  default")
(add-hook 'aes-path-passwd-hook (lambda (path) my/aes-default-group))
;; if the environment variable is not defined, we will be prompted
;; for the password
(setq aes--plaintext-passwords
      (let ((pwd (or (getenv "EMACS_PWD") "nil")))
        (list (cons my/aes-default-group pwd))))
(global-set-key "\C-c0z" 'aes-toggle-encryption)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; idle-highlight ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'idle-highlight-mode)
(setq idle-highlight-idle-time 10)
;(add-hook 'prog-mode-hook #'idle-highlight-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ascii ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ascii)
(global-set-key "\M-sa" 'ascii-display)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'my/git-keymap)
(global-set-key "\M-sm" 'my/git-keymap)
(eval-and-compile
  (add-to-list 'load-path (concat my/plugins-directory "magit/lisp/")))
(unless (version< emacs-version "24.4")
  (eval-and-compile (setq magit-need-cygwin-noglob nil))
  (require 'with-editor)
  (require 'magit)
  (setq magit-revert-buffers nil
        inhibit-magit-revert t
        magit-completing-read-function 'magit-ido-completing-read
        magit-status-buffer-name-format "*magit: %b*"
        magit-log-show-margin t
        magit-popup-show-common-commands nil
        magit-log-show-refname-after-summary nil
        magit-no-confirm '()
        magit-diff-refine-hunk t
        )
  (global-magit-file-buffer-mode 1)
  ;; add ido shortcut
  (if (version< emacs-version "25.1")
      (add-hook
       'ido-setup-hook
       (lambda() (define-key ido-completion-map
                   (kbd "C-x g") 'ido-enter-magit-status)))
    (define-key ido-common-completion-map
      (kbd "C-x g") 'ido-enter-magit-status))
  ;; git commands
  (define-key my/git-keymap "g" 'magit-status)
  (define-key my/git-keymap "\M-g" 'magit-dispatch-popup)
  ;; view arbitrary blobs
  (define-key my/git-keymap "f" 'magit-find-file)
  (define-key my/git-keymap "4f" 'magit-find-file-other-window)
  ;; show all commits that touch current file
  (define-key my/git-keymap "h" 'magit-log-buffer-file)
  (define-key my/git-keymap "y" 'magit-cherry)
  ;; to see all differences, even those automatically merged
  (define-key my/git-keymap "e" 'ediff-merge-revisions-with-ancestor)
  (define-key my/git-keymap "m" 'magit-toggle-margin)
  (define-key my/git-keymap "b" 'magit-blame)
  ;; unstage all changes (like SU but forces HEAD)
  (define-key my/git-keymap "U" 'magit-unstage-all)
  (define-key my/git-keymap "s" 'magit-stage-file)
  (define-key my/git-keymap "u" 'magit-unstage-file)
  ;; soft reset; hard reset can use C-u x
  (define-key my/git-keymap "r" 'magit-reset-soft)
  ;; add argument --no-merges to log
  (magit-define-popup-switch 'magit-log-popup
    ?m "Omit merge commits" "--no-merges")
  (setq magit-log-arguments (cons "--no-merges"
                                  magit-log-arguments))
  ;; show status buffer alone
  (setq magit-post-display-buffer-hook
        (lambda ()
          (when (derived-mode-p 'magit-status-mode)
            (delete-other-windows))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git-timemachine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (version< emacs-version "24.4")
  (require 'git-timemachine)
  (define-key my/git-keymap "t" 'git-timemachine)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shell-pop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'shell-pop)
(global-set-key (kbd "<f1>") 'shell-pop)
(global-set-key "\C-c1" 'shell-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shackle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'shackle)
(setq shackle-default-ratio 0.4)
(setq shackle-select-reused-windows t)
(setq shackle-rules
      '(
        (occur-mode :select nil)
        (grep-mode :select nil)
        ("*Help*" :select t)
        (completion-list-mode :select nil)
        (compilation-mode :select nil)
        ("*Shell Command Output*" :select t); :align right)
        ("COMMIT_EDITMSG" :select t)
        )
      shackle-default-rule '(:select nil)
      )
(shackle-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hl-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'hl-line+)
(global-set-key "\M-sl" 'hl-line-flash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; crosshairs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'crosshairs)
(global-set-key "\M-sL" 'crosshairs-flash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; beacon ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'beacon)
(beacon-mode 1)
(add-to-list 'beacon-dont-blink-major-modes 'etags-select-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bookmark+ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (add-to-list 'load-path (concat my/plugins-directory "bookmark+/")))
(setq bookmark-default-file (concat my/user-directory "bookmarks")
      bmkp-bmenu-state-file (concat my/user-directory
                                    "emacs-bmk-bmenu-state.el")
      bookmark-save-flag nil
      )
(require 'bookmark+)
(global-set-key [f7] 'bmkp-previous-bookmark)
(global-set-key "\C-c7" 'bmkp-previous-bookmark)
(global-set-key [f8] 'bmkp-next-bookmark)
(global-set-key "\C-c8" 'bmkp-next-bookmark)
(add-hook 'after-init-hook
          (lambda ()
            (unless (> (length command-line-args) 1)
              (bookmark-bmenu-list)
              (switch-to-buffer "*Bookmark List*"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; savehist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'savehist)
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring compile-history)
      savehist-file (concat my/user-directory "history")
      savehist-save-minibuffer-history t
      history-length 50
      history-delete-duplicates t
      )
(put 'minibuffer-history 'history-length 100)
(put 'kill-ring 'history-length 25)
(savehist-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; recentf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 12
      recentf-save-file (concat my/user-directory "recentf"))
(setq recentf-exclude '( "-tags\\'" "ido\.last\\'" ))
(recentf-mode 1)
(defvar uniquify-recentf-func 'recentf-open-files
  "Select recent files from `recentf' via a completion function.")
(defun my/call-recentf-func () (interactive) (funcall uniquify-recentf-func))
(global-set-key "\er" 'my/call-recentf-func)
(when (version< "24.3" emacs-version)
  (require 'uniquify-recentf)
  (setq uniquify-recentf-func 'uniquify-recentf-ido-recentf-open))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(setq ido-save-directory-list-file (concat my/user-directory "ido-last"))
(setq ido-max-prospects 25)
;; (setq ido-enable-flex-matching t)
;; (setq ido-case-fold t)
(setq ido-auto-merge-work-directories-length -1) ;disables auto-merge
(add-to-list 'ido-work-directory-list-ignore-regexps tramp-file-name-regexp)
;; ask before reusing an existing buffer
(setq-default ido-default-buffer-method 'maybe-frame)
(setq-default ido-default-file-method 'maybe-frame)
(ido-mode 1)
(defvar my/completion-framework-alist (list
                                       (cons "ido" 'my/activate-ido))
  "An alist of completion frameworks to choose among.  Each value is a
 cons cell (`description' . `activation-function' ).")

;; sort files by descending modified time (except remotely, which is dog-slow)
(defun ido-sort-mtime()
  (unless (tramp-tramp-file-p default-directory)
    (setq ido-temp-list
          (sort ido-temp-list
                (lambda (a b)
                  (time-less-p
                   (sixth (file-attributes (concat ido-current-directory b)))
                   (sixth (file-attributes (concat ido-current-directory a)))))))
    ;; (ido-to-end
    ;;  (delq nil (mapcar (lambda (x)
    ;;                      (and (char-equal (string-to-char x) ?.) x))
    ;;                    ido-temp-list)))
    ))
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

(when (< emacs-major-version 24)
  ;; use ido to switch modes when smex is not available
  (global-set-key (kbd "M-x")
                  (lambda() (interactive)
                    (call-interactively
                     (intern
                      (ido-completing-read
                       "M-x " (all-completions "" obarray 'commandp)))))))
;; for recentf
(unless (featurep 'uniquify-recentf)
  (defun recentf-ido-find-file()
    "Find a recent file using ido."
    (interactive)
    (let ((file (ido-completing-read
                 "Choose recent file:"
                 recentf-list nil t)))
      (when file (find-file file))))
  (global-set-key "\er" 'recentf-ido-find-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ace-window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'avy)
(require 'ace-window)
(global-set-key "\M-p" 'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; swiper/ivy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (version< emacs-version "24.1")
  (eval-and-compile
    (add-to-list 'load-path (concat my/plugins-directory "swiper/")))
  (require 'swiper)
  (global-set-key "\M-ss" 'swiper)
  (define-key isearch-mode-map (kbd "C-o") 'swiper-from-isearch)
  (require 'ivy)
  (setq ivy-wrap t)
  (add-to-list 'my/completion-framework-alist
               (cons "ivy" 'my/activate-ivy))
  (global-set-key "\e\eii" 'ivy-resume)
  (setq counsel-find-file-ignore-regexp "\\.elc$")
  (setf (cdr (assoc 'counsel-M-x ivy-initial-inputs-alist)) "")
  (require 'counsel)
  ;; fallback to basic find-file
  (define-key counsel-find-file-map "\C-x\C-f"
    (lambda ()
      (interactive)
      (ivy-set-action
       (lambda (x)
         (let ((completing-read-function 'completing-read-default))
           (call-interactively 'find-file))))
      (ivy-done)))
  ;; make matches appear fancy
  (unless (version< emacs-version "24.5")
    (setq ivy-display-style 'fancy))
  )

(defun my/activate-ido ()
  "Activate ido as a completion framework."
  (when (boundp 'ivy-mode)
    (ivy-mode 0))
  ;; magit
  (setq magit-completing-read-function 'magit-ido-completing-read)
  ;; recentf
  (setq uniquify-recentf-func 'uniquify-recentf-ido-recentf-open)
  ;; M-x
  (global-set-key (kbd "M-x") 'smex)
  (if (boundp 'ivy-mode)
      (global-set-key "\e\ex" 'counsel-M-x)
    (global-set-key "\e\ex" 'execute-extended-command))
  (ido-mode 1)
  (message "Using ido for completion"))

(defun my/activate-ivy ()
  "Activate ivy as a completion framework."
  (when (boundp 'ido-mode)
    (ido-mode 0))
  ;; magit
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; recentf
  (setq uniquify-recentf-func 'uniquify-recentf-ivy-recentf-open)
  ;; M-x
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key "\e\ex" 'smex)
  (ivy-mode 1)
  (message "Using ivy for completion"))

;; choose the completion framework in use
(defun my/choose-completion ()
  "Choose among completion frameworks. Uses the completion framework
 specified by `my/choose-func'. Current choices are ido and ivy."
  (interactive)
  (funcall (funcall my/choose-func my/completion-framework-alist
                    "Completion framework:")))
(global-set-key "\C-c\C-e" 'my/choose-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (<= 24 emacs-major-version)
  (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))
(if (boundp 'ivy-mode)
    (global-set-key "\e\ex" 'counsel-M-x)
  ;; the old M-x
  (global-set-key "\e\ex" 'execute-extended-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; imenu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I prefer imenu-anywhere to return imenu results only for the current
;; buffer, not all open buffers.
(defvar imenu-anywhere-buffer-list-function
  (lambda() (list (current-buffer))))
(require 'imenu-anywhere)
(global-set-key "\C-cj" 'imenu-anywhere)

(require 'imenu-scan)

(require 'popup-imenu)
(setq popup-imenu-position 'point)
(global-set-key "\C-c\C-j" 'popup-imenu)

;; ;; TODO: this regexp has false positives, but at least handles when
;; ;; functions have whitespace (i.e. in namespaces).  I don't know which
;; ;; is better.  The answer may be to not use imenu at all.
;; ;; (require 'cc-mode+)
;; (defvar my/imenu-generic-expression
;;   `((nil
;;      ,(concat
;;        "\\s-*"                     ; beginning of line is required
;;        "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
;;        "\\([a-zA-Z0-9_:]+[ \t]+\\)?"        ; type specs; there can be no
;;        "\\([a-zA-Z0-9_:]+[ \t]+\\)?"        ; more than 3 tokens, right?

;;        "\\("                            ; last type spec including */&
;;        "[a-zA-Z0-9_:]+"
;;        "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)" ; either pointer/ref sign or whitespace
;;        "\\)?"                             ; if there is a last type spec
;;        "\\s-+\\("                      ; name; take that into the imenu entry
;;        "[a-zA-Z0-9_:~]+"          ; member function, ctor or dtor...
;;                                         ; (may not contain * because then
;;                                         ; "a::operator char*" would become "char*"!)
;;        "\\|"
;;        "\\([a-zA-Z0-9_:~]*::\\)?operator"
;;        "[^a-zA-Z1-9_][^(]*"             ; ...or operator
;;        " \\)"
;;        "[ \t]*([^)]*)[ \t\n]*[^;]"    ; require something other than a ; after
;;                                         ; the (...) to avoid prototypes.  Can't
;;                                         ; catch cases with () inside the parentheses
;;                                         ; surrounding the parameters
;;                                         ; (like "int foo(int a=bar()) {...}"

;;        ) 6)
;;     ("Class"
;;      ,(concat
;;        "\\s-*"                       ; beginning of line is required
;;        "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
;;        "class[ \t]+"
;;        "\\([a-zA-Z0-9_]+\\)"            ; this is the string we want to get
;;        "[ \t]*[:{]"
;;        ) 2)))

;; (add-hook 'c++-mode-hook
;;           (lambda() (setq imenu-generic-expression
;;                           my/imenu-generic-expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; powerline ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (add-to-list 'load-path (concat my/plugins-directory "powerline/"))
  (require 'powerline)
  ;; does not interact with rich-minority mode: try delight.el?
  ;; (powerline-default-theme)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rich-minority ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (version<= "24.3" emacs-version)
  ;; this dependency actually comes from smart-mode-line, which uses
  ;; rich-minority.
  (require 'rich-minority)
  (rich-minority-mode 1)
  (setq rm-blacklist
        '(" AC" " yas" " Undo-Tree" " Abbrev" " Guide" " Hi" " $" " ,"
          " Ifdef" " Rbow" " ivy" " ElDoc" " (*)" " wg" " ⛓"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smart-mode-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (version<= "24.3" emacs-version)
  (eval-and-compile
    (add-to-list 'load-path (concat my/plugins-directory "smart-mode-line/")))
  (require 'smart-mode-line)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; undo-tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)
(global-undo-tree-mode)
;; reset the undo tree history (useful after reverting buffer)
(global-set-key "\C-cu" (lambda()(interactive)(setq buffer-undo-tree nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; goto-chg ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'goto-chg)
(global-set-key [?\C-.] 'goto-last-change)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tramp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")
(when (boundp 'my/user-name)
  (setq tramp-default-user my/user-name))
(setq tramp-auto-save-directory my/autosave-dir)
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp tramp-file-name-regexp))

(defvar my/remote-host-list '())
(defun my/connect-to-remote-host(&optional arg)
  "Connect to a remote host from `my/remote-host-list'."
  (interactive "P")
  (let ((hosts
         (mapcar (lambda (plist)
                   (let* ((delim (char-to-string ?:))
                          (stem (plist-get plist :host))
                          (user (or (unless arg
                                      (plist-get plist :user))
                                    my/user-name))
                          (pwd (plist-get plist :password))
                          (desc (plist-get plist :description))
                          (category (plist-get plist :category))
                          (display (concat user delim stem))
                          (connect (concat
                                    "/"
                                    tramp-default-method
                                    ":" user "@" stem ":~")))
                     (when category
                       (setq display
                             (concat category delim display)))
                     (when desc
                       (setq display
                             (concat display delim desc)))
                     (and stem (cons display connect))))
                 my/remote-host-list))
        result cell)
    (setq result
          (funcall my/choose-func
                   (mapcar 'car hosts)
                   "Remote host: "))
    (when result
      (setq cell (assoc result hosts))
      (find-file (cdr cell)))))
(global-set-key [f6] 'my/connect-to-remote-host)
(global-set-key "\C-c6" 'my/connect-to-remote-host)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-src-fontify-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (C . t)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-x)                      ; C-x C-j now runs 'dired-jump
(require 'dired-details+)
(require 'dired-sort)
(setq-default dired-listing-switches "-alhvGg")
(setq dired-details-initially-hide nil)
(put 'dired-find-alternate-file 'disabled nil)
;; next window's dired window used as target for current window operations
(setq dired-dwim-target t)
;; search only in filenames
(setq dired-isearch-filenames t)

(defun my/dired-sort()
  "Toggle sorting in dired buffers."
  (interactive)
  ( let ((type
          (funcall
           my/choose-func
           '( "size" "extension" "ctime" "utime" "time" "name")
           "Sort by:")))
    ;; on os x, extension (X) not supported;
    ;; also, ctime means time file status was last changed
    (cond ((string= type "size") (dired-sort-size))
          ((string= type "extension") (dired-sort-extension))
          ((string= type "ctime") (dired-sort-ctime))
          ((string= type "utime") (dired-sort-utime))
          ((string= type "time") (dired-sort-time))
          ((string= type "name") (dired-sort-name))
          (t (error "unknown dired sort %s" type)))))
(define-key dired-mode-map "`" 'my/dired-sort)

(defadvice shell-command
    (after shell-in-new-buffer (command &optional output-buffer error-buffer))
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(ad-activate 'shell-command)

;; launch command
(defun dired-launch-command() (interactive)
       (dired-do-shell-command
        (case system-type
          (darwin "open")
          (gnu/linux "open")
          ) nil (dired-get-marked-files t current-prefix-arg)))

(add-hook 'dired-load-hook
          (lambda()
            (define-key dired-mode-map (kbd "<prior>") 'dired-up-directory)
            (define-key dired-mode-map "l" 'dired-launch-command)
            ))

;; easily go to top or bottom
(defun dired-back-to-top()
  (interactive)
  (let ((sorting-by-date (string-match-p dired-sort-by-date-regexp
                                         dired-actual-switches)))
    (goto-char (point-min))
    (dired-next-line
     (if sorting-by-date 2 4))))
(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom() (interactive)
       (goto-char (point-max))
       (dired-next-line -1))
(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(defun my-dired-do-command (command)
  "Run command on marked files. Any files not already open will be opened.
 After this command has been run, any buffers it's modified will remain
 open and unsaved."
  (interactive "Run on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sunrise-commander ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (add-to-list 'load-path (concat my/plugins-directory "sunrise/"))
  (require 'sunrise-commander)
  (require 'sunrise-x-tree)
  (require 'sunrise-x-w32-addons)
  (global-set-key "\C-c0s" 'sunrise)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; neotree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'neotree)
(global-set-key "\C-c0n" 'neotree-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; diff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only highlight current chunk
(setq-default ediff-highlight-all-diffs 'nil
              ediff-keep-variants nil
              )
;; don't use a separate control frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; toggle between control frame and control window
(add-hook 'ediff-keymap-setup-hook
          (lambda()
            (define-key ediff-mode-map "t" 'ediff-toggle-multiframe)
            ))

;; ensure wide display does not persist after quitting ediff
(defvar ediff-last-windows nil "Last ediff window configuration.")
(defun ediff-restore-windows ()
  "Restore window configuration to `ediff-last-windows'."
  (set-window-configuration ediff-last-windows)
  (remove-hook 'ediff-after-quit-hook-internal
               'ediff-restore-windows))
(defadvice ediff-buffers (around ediff-restore-windows activate)
  (setq ediff-last-windows (current-window-configuration))
  (add-hook 'ediff-after-quit-hook-internal 'ediff-restore-windows)
  ad-do-it)

(defun my/toggle-ediff-wide-display()
  "Turn off wide-display mode (if enabled) before quitting ediff."
  (interactive)
  (when ediff-wide-display-p
    (ediff-toggle-wide-display)))
(add-hook 'ediff-cleanup-hook
          (lambda ()
            (my/toggle-ediff-wide-display)
            (ediff-janitor t nil)
            ))

;; resume prior window configuration
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; better colors in older versions
(when (version< emacs-version "24.3")
  (eval-after-load 'diff-mode '(progn
                                 (set-face-attribute 'diff-added nil
                                                     :foreground "white"
                                                     :background "blue"
                                                     )
                                 (set-face-attribute 'diff-removed nil
                                                     :foreground "white"
                                                     :background "red3"
                                                     )
                                 (set-face-attribute 'diff-changed nil
                                                     :foreground "white"
                                                     :background "purple"
                                                     )
                                 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ediff-trees ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ediff-trees)
(define-prefix-command 'my/ediff-trees-keymap)
(global-set-key "\C-ce" 'my/ediff-trees-keymap)
(define-key my/ediff-trees-keymap (kbd "n") 'ediff-trees-examine-next)
(define-key my/ediff-trees-keymap (kbd "p") 'ediff-trees-examine-previous)
(define-key my/ediff-trees-keymap (kbd "\C-n") 'ediff-trees-examine-next-regexp)
(define-key my/ediff-trees-keymap (kbd "\C-p") 'ediff-trees-examine-previous-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; diff-hl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (add-to-list 'load-path (concat my/plugins-directory "diff-hl/")))
(require 'diff-hl)
(require 'diff-hl-flydiff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git-gutter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq git-gutter:lighter "")
(setq git-gutter:hide-gutter t)
(setq git-gutter:diff-option "-w")
(require 'git-gutter)
(global-git-gutter-mode 1)
(global-set-key "\C-xvp" 'git-gutter:previous-hunk)
(global-set-key "\C-xvn" 'git-gutter:next-hunk)
(global-set-key "\C-xvd" 'git-gutter:popup-hunk)
(global-set-key "\C-xvt" 'git-gutter:toggle)
(global-set-key "\C-xvs" 'git-gutter:stage-hunk)
(global-set-key "\C-xvr" 'git-gutter:revert-hunk)
(global-set-key "\C-xvc" 'git-gutter:clear)
(global-set-key "\C-xvu" 'git-gutter:update-all-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shebang ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'shebang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; point-undo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'point-undo)
(global-set-key [?\C-,] 'point-undo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; framemove ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'windmove)
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)
;;(setq windmove-wrap-around t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; frame-cmds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this also loads 'frame-fns
(require 'frame-cmds)
(global-set-key [(meta up)]                    'move-frame-up)
(global-set-key [(meta down)]                  'move-frame-down)
(global-set-key [(meta left)]                  'move-frame-left)
(global-set-key [(meta right)]                 'move-frame-right)
(global-set-key [(meta shift ?v)]              'move-frame-to-screen-top)
(global-set-key [(control shift ?v)]           'move-frame-to-screen-bottom)
(global-set-key [(control shift prior)]        'move-frame-to-screen-left)
(global-set-key [(control shift next)]         'move-frame-to-screen-right)
(global-set-key [(control shift home)]         'move-frame-to-screen-top-left)
(global-set-key [(control meta down)]          'enlarge-frame)
(global-set-key [(control meta right)]         'enlarge-frame-horizontally)
(global-set-key [(control meta up)]            'shrink-frame)
(global-set-key [(control meta left)]          'shrink-frame-horizontally)
(global-set-key [(control ?x) (control ?z)]    'iconify-everything)
(global-set-key [(control ?z)]                 'iconify/show-frame)
;(global-set-key [mode-line mouse-3]            'mouse-iconify/show-frame)
;(global-set-key [mode-line C-mouse-3]          'mouse-remove-window)
(global-set-key [(control meta ?z)]            'show-hide)
;(global-set-key [vertical-line C-down-mouse-1] 'show-hide)
;(global-set-key [C-down-mouse-1]               'mouse-show-hide-mark-unmark)
(substitute-key-definition 'delete-window      'remove-window global-map)
(define-key ctl-x-map "o"                      'other-window-or-frame)
(define-key ctl-x-4-map "1"                    'delete-other-frames)
(define-key ctl-x-5-map "h"                    'show-*Help*-buffer)
(substitute-key-definition 'delete-window      'delete-windows-for global-map)
(define-key global-map "\C-xt."                'save-frame-config)

(defun my/tile-frames-vertically()
  "Tile frames vertically. You can restore prior frame position via going to
register \\C-l."
  (interactive)
  (save-frame-config)
  (tile-frames-vertically))
(global-set-key "\e\ev" 'my/tile-frames-vertically)
(defun my/tile-frames-horizontally()
  "Tile frames horizontally. You can restore prior frame position via going to
register \\C-l."
  (interactive)
  (save-frame-config)
  (tile-frames-horizontally))
(global-set-key "\e\eh" 'my/tile-frames-horizontally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; workgroups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'workgroups)
(setq wg-default-buffer "*Bookmark List*")
;; doesn't work, isn't needed? (setq wg-restore-position t)
;; TODO: set initial string to "( -<{ }>- )"
(setq wg-query-for-save-on-emacs-exit nil)
(define-key wg-map (kbd "<left>") 'wg-switch-left)
(define-key wg-map (kbd "<right>") 'wg-switch-right)
(workgroups-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; workgroups2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'workgroups2)
;; (setq wg-prefix-key "\C-z")
;; ;; (define-key workgroups-mode-map (kbd "<left>") 'wg-switch-left)
;; ;; (define-key workgroups-mode-map (kbd "<right>") 'wg-switch-right)
;; (workgroups-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; color-theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (add-to-list 'load-path (concat my/plugins-directory "color-theme/"))
  (add-to-list 'load-path (concat my/plugins-directory "color-theme/themes/")))
(require 'color-theme)
(color-theme-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; palette ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'palette)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smerge ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun try-smerge()
  (save-excursion
    (goto-char (point-min))
    (smerge-mode
     (if (re-search-forward "^<<<<<<<" nil t) 1 0))))
(add-hook 'find-file-hook 'try-smerge t)
(add-hook 'after-save-hook (lambda() (if (smerge-mode) (try-smerge))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multi-term ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'multi-term)
;(setq multi-term-program "/bin/tcsh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bash-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'bash-completion)
(bash-completion-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vlf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq large-file-warning-threshold 100000000) ;100MB
(eval-and-compile
  (add-to-list 'load-path (concat my/plugins-directory "vlf/")))
(require 'vlf-setup)
(setq vlf-batch-size 10000000)          ;10MB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; profiles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'profiles+)
(profile-define "default" "dharms" "danielrharms@gmail.com"
                ;; relative path to makefiles
                'build-sub-dirs '((""))
                ;; relative path to debug executables (under project-root-dir
                ;; and build-sub-dir)
                ;; 'debug-sub-dirs '("tests/")
                ;; specific compiler invocation command
                'compile-sub-command "make"
                )
(profile-set-default "default")
(global-set-key "\C-c0d" 'profile-open-dired-on-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rtags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (add-to-list 'load-path (concat my/plugins-directory "rtags/")))
(require 'rtags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (add-to-list 'load-path (concat my/plugins-directory "auto-complete/")))
(require 'auto-complete)
;; user dictionary
(add-to-list 'ac-user-dictionary-files
             (concat my/scratch-directory "user-dict"))
;; mode/extension directory (in addition to "plugins/auto-complete/dict")
(add-to-list 'ac-dictionary-directories
             (concat my/scratch-directory "dict/"))
(mapc (lambda(mode)
        (add-to-list 'ac-modes mode))
      '(sql-mode nxml-mode cmake-mode folio-mode protobuf-mode
                 python-mode dos-mode gud-mode sh-mode
                 makefile-mode makefile-automake-mode makefile-gmake-mode
                 autoconf-mode gdb-script-mode awk-mode
                 mock-mode))
(require 'auto-complete-config)
(ac-config-default)
(setq-default ac-sources (append '(ac-source-filename) ac-sources))
(setq ac-use-menu-map t)
(setq ac-auto-start t)
(setq ac-ignore-case nil)
(define-key ac-mode-map (kbd "M-/") 'auto-complete)
;(define-key ac-mode-map (kbd "<lwindow> TAB") 'auto-complete)
(define-key ac-mode-map (kbd "\C-c TAB") (lambda()(interactive)
                                           (setq ac-auto-start
                                                 (null ac-auto-start))))
(setq ac-menu-height 20)
(require 'rtags-ac)
(setq rtags-completions-enabled t)
(rtags-enable-standard-keybindings c-mode-base-map)
(require 'auto-complete-etags)
(require 'auto-complete-nxml)
;; c-headers
(require 'auto-complete-c-headers)
(setq achead:include-patterns (list
                               "\\.\\(h\\|hpp\\|hh\\|hxx\\|H\\)$"
                               "/[a-zA-Z-_]+$"
                               ))
;; doesn't work...
;; (setq achead:ac-prefix
;;       "#?\\(?:include\\|import\\)\\s-*[<\"]\\s-*\\([^\"<>' \t\r\n]+\\)")
(setq achead:include-directories '("."))

(add-hook 'c-mode-common-hook
          (lambda()
            (set (make-local-variable 'ac-auto-start) nil)
            ;; we'll define a special key event for yasnippet
            (setq ac-sources (remove 'ac-source-yasnippet ac-sources))
            (setq ac-sources (remove 'ac-source-gtags ac-sources))
            (add-to-list 'ac-sources 'ac-source-etags)
            (add-to-list 'ac-sources 'ac-source-c-headers)
            ) t)                       ;append to hook list to take effect
                                       ;after ac-config-default
(add-hook 'protobuf-mode-hook
          (lambda()
            (setq ac-sources (add-to-list 'ac-sources 'ac-source-etags))))
(defun my/rtags-complete() (interactive)
       (auto-complete '(ac-source-rtags)))
(global-set-key (kbd "\C-c r TAB") 'my/rtags-complete)
(defun my/expand-imenu() (interactive)
       (auto-complete '(ac-source-imenu)))
(global-set-key "\C-c0j" 'my/expand-imenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; YASnippet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (add-to-list 'load-path (concat my/plugins-directory "yasnippet/")))
(require 'yasnippet)
(add-to-list 'safe-local-variable-values '(require-final-newline . nil))
(setq yas-snippet-dirs (list
                        (concat my/scratch-directory "snippets/")
                        (concat my/plugins-directory "yasnippet/snippets/")))
(yas-global-mode 1)
(setq yas-prompt-functions '(
                             yas-ido-prompt
                             yas-x-prompt
                             ;; yas-completing-prompt
                             ;; yas-no-prompt
                             ))
;; disable TAB key to activate a snippet (I prefer TAB merely indent)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-keymap [(tab)] nil)
;; (define-key yas-keymap (kbd "TAB") nil)
;; (define-key yas-keymap [(shift tab)] nil)
;; (define-key yas-keymap [backtab] nil)
;; add our own keybindings
(define-key yas-minor-mode-map "\C-cse" 'yas-expand)
(define-key yas-minor-mode-map "\C-csi" 'yas-insert-snippet)
(define-key yas-minor-mode-map "\C-csn" 'yas-new-snippet)
(define-key yas-minor-mode-map "\C-csv" 'yas-visit-snippet-file)
(define-key yas-minor-mode-map "\C-cs?" 'yas-describe-tables)

;; integrate with auto-complete
(defun my/expand-yasnippet() (interactive)
       (auto-complete '(ac-source-yasnippet)))
(global-set-key [backtab] 'my/expand-yasnippet)
(global-set-key [(shift tab)] 'my/expand-yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-global-modes
      '(emacs-lisp-mode python-mode dart-mode sh-mode))
(require 'flycheck-pos-tip)
(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; headers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(autoload 'auto-update-file-header "header2")
(require 'header2)
(add-hook 'write-file-functions 'auto-update-file-header)

;; last modification time
(defun my/update-last-modifier ()
  "Update header line indicating identity of last modifier."
  (delete-and-forget-line)
  (insert (format " %s" (let ((name (user-full-name)))
                          (if (and name (not (string= name "")))
                              name
                            (user-login-name))))))
;; use my own function, because delete-trailing-whitespace prevents a
;; space after the colon
(register-file-header-action "Modified by[ \t]*:" 'my/update-last-modifier)

;; file name
(defun my/update-file-name ()
  "Update the line that indicates the file name."
  (beginning-of-line)
  ;; Verify looking at a file name for this mode.
  (when (looking-at (concat (regexp-quote (header-prefix-string)) " *\\([^ ]+\\) +\\-\\-"))
    (goto-char (match-beginning 1))
    (delete-region (match-beginning 1) (match-end 1))
    (insert (file-name-nondirectory (buffer-file-name)))))

(register-file-header-action "^[ \t]*.+ *\\([^ ]+\\) +\\-\\-" 'my/update-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'autoinsert)
(require 'auto-insert-choose+)
(setq auto-insert 'other)
(setq auto-insert-directory (concat my/scratch-directory "auto-insert/"))
;; list of different templates to choose from
;; c++
(defvar auto-insert-c-header-alist '())
(defvar auto-insert-c-impl-alist '())
(auto-insert-choose+-add-entry 'auto-insert-c-header-alist "template.h")
(auto-insert-choose+-add-entry 'auto-insert-c-impl-alist "template.cpp")
;; autoconf
(defvar auto-insert-autoconf-alist '())
(auto-insert-choose+-add-entry 'auto-insert-autoconf-alist
                               "configure-standard.ac")
(auto-insert-choose+-add-entry 'auto-insert-autoconf-alist
                               "configure-library.ac")
;; Makefile.am
(defvar auto-insert-makefile-am-alist '())
(auto-insert-choose+-add-entry 'auto-insert-makefile-am-alist
                               "Makefile-toplevel.am")
(auto-insert-choose+-add-entry 'auto-insert-makefile-am-alist
                               "Makefile-library.am")
(auto-insert-choose+-add-entry 'auto-insert-makefile-am-alist
                               "Makefile-executable.am")

;; The "normal" entries (using auto-insert) can list the file name and
;; the yas-expand helper.  If you want to be able to choose among
;; different templates per mode or file extension, then use the
;; auto-insert-choose+ functionality: populate an alist per file type
;; with the different templates, then associate a lambda with a defun
;; that selects between them: completion, ido, popup.
(setq auto-insert-alist
      '(
        ;; profiles
        (("\\.eprof$" . "Profiles") .
         ["template.eprof" auto-insert-choose-yas-expand])
        (("\\.rprof$" . "Remote Profiles") .
         ["template.rprof" auto-insert-choose-yas-expand])
        ;; lisp
        ((emacs-lisp-mode . "Emacs Lisp") .
         ["template.el" auto-insert-choose-yas-expand])
        ;; sh
        ((sh-mode . "Sh") .
         ["template.sh" auto-insert-choose-yas-expand])
        ;; dos
        ((dos-mode . "Dos") .
         ["template.bat" auto-insert-choose-yas-expand])
        ;; python
        ((python-mode . "Python") .
         ["template.py" auto-insert-choose-yas-expand])
        ;; CMake
        (("CMakeLists.txt" . "CMake") .
         ["template.cmake" auto-insert-choose-yas-expand])
        ;; autoconf
        ((autoconf-mode . "Autoconf")
         lambda nil (auto-insert-choose-and-call-popup
                     auto-insert-autoconf-alist))
        ;; makefile-automake
        ((makefile-automake-mode . "Makefile-Automake")
         lambda nil (auto-insert-choose-and-call-popup
                     auto-insert-makefile-am-alist))
        ;; c headers
        (("\\.\\(h\\|hh\\|H\\|hpp\\|hxx\\)$" . "c++")
         lambda nil (auto-insert-choose-and-call-popup
                     auto-insert-c-header-alist))
        ;; c impl
        (("\\.\\(cpp\\|cc\\|C\\|c\\|cxx\\)$" . "c++")
         lambda nil (auto-insert-choose-and-call-popup
                     auto-insert-c-impl-alist))
        ))
(global-set-key "\C-cst" 'auto-insert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; popup-kill-ring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'popup-kill-ring)
(global-set-key "\C-\M-y" 'popup-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;; popup-global-mark-ring ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'popup-global-mark-ring)
(global-set-key "\e\ey" 'popup-global-mark-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; zop-to-char ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'zop-to-char)
(global-set-key "\M-z" 'zop-to-char)
(global-set-key "\M-Z" 'zop-up-to-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; speedbar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'speedbar-mode-hook
          (lambda()
            (when (display-graphic-p)
              (setq-default gdb-speedbar-auto-raise t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; htmlize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'htmlize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; awk-it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'awk-it)
(global-set-key "\C-c0aa" 'awk-it)
(global-set-key "\C-c0ap" 'awk-it-with-separator)
(global-set-key "\C-c0as" 'awk-it-single)
(global-set-key "\C-c0ag" 'awk-it-single-with-separator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; list-environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'list-environment)
(global-set-key "\C-c0e" 'list-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; guide-key ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (version< "24.3" emacs-version)
  (require 'guide-key)
  (guide-key-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; os ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((system (car (reverse (split-string (symbol-name system-type)
                                           "\\/" t))))
       (os-dir (concat my/user-directory "settings/os/" system "/"))
       (system-file (concat os-dir system))
       (path-file (concat os-dir "path"))
       (include-file (concat os-dir "include"))
       (lib-file (concat os-dir "lib"))
       (libpath-file (concat os-dir "libpath"))
       )
  ;; load os file
  (load system-file)
  ;; check for any additional environment variables
  (if (file-exists-p path-file)
      (progn
        (load-environment-variable-from-file "PATH" path-file)
        ;; replicate path (delimiter-separated string of paths) into
        ;; exec-path (list of paths); by convention, ends in exec-dir
        (setq exec-path (append
                         (read-file-into-list-of-lines path-file)
                         (list (convert-standard-filename exec-directory))))))
  (if (file-exists-p include-file)
      (load-environment-variable-from-file "INCLUDE" include-file))
  (if (file-exists-p lib-file)
      (load-environment-variable-from-file "LIB" lib-file))
  (if (file-exists-p libpath-file)
      (load-environment-variable-from-file "LIBPATH" libpath-file))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((gui-dir (concat my/user-directory "settings/gui/"))
      (gui window-system)
      gui-file)
  ;; load gui file
  (setq gui-file (concat gui-dir (if (null gui) "tty" (symbol-name gui))))
  (load gui-file)
  )

(setq-default frame-title-format
              '(:eval
                (format "%s@%s: %s %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name)
                        (if dired-directory
                            (concat "{" (buffer-name) "}")
                          (buffer-name))
                        (if (profile-current-get 'project-name)
                            ;; the parent profile "default" happens to
                            ;; have an empty 'project-name attribute
                            (concat
                             "("
                             (upcase (symbol-name profile-current))
                             ")")
                          "")           ;else empty if no project name
                        )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; site ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/load-site-file (name)
  "Load a site file associated with site NAME, and perform related
customization."
  (let* ((site-dir
          (file-name-as-directory
           (concat my/user-directory "settings/site/" name)))
         (site-file (concat site-dir name)))
    (when (file-exists-p site-file)
      ;; (setq site-name (file-name-base site-file))
      (load site-file))
    (setq yas-snippet-dirs (cons (concat site-dir "snippets/")
                                 yas-snippet-dirs))
    (yas-reload-all)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; host ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/host-plist '())
(let* ((system system-name)
       (hosts-dir (concat my/user-directory "settings/host/"))
       (hosts-file (concat hosts-dir "hosts"))
       (host-dir
        (file-name-as-directory
         (concat hosts-dir system)))
	   (host-file (concat host-dir system))
       (remote-hosts-file (concat my/user-directory "remote-hosts")))
  ;; load remote hosts file if present
  (when (file-exists-p remote-hosts-file)
    (load remote-hosts-file t))
  ;; load host file (if present)
  (if (file-exists-p host-file)
      (progn
        (load host-file t))
    ;; otherwise look for current host in hosts file
    (load hosts-file t)
    (mapc
     (lambda(plist)
       (and (string= (plist-get plist :host) system)
            (plist-get plist :site)
            (my/load-site-file (plist-get plist :site))))
     my/host-plist))
  )

(add-hook 'before-save-hook 'my/before-save-hook)
(defun my/before-save-hook() "Presave hook"
       (when (memq major-mode '(c++-mode emacs-lisp-mode perl-mode
                                         java-mode python-mode dos-mode
                                         nxml-mode protobuf-mode folio-mode
                                         sh-mode csharp-mode awk-mode
                                         gdb-script-mode
                                         gitignore-mode
                                         gitconfig-mode
                                         gitattributes-mode))
         (delete-trailing-whitespace)
         (copyright-update nil t)
         (time-stamp)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      (append '(("\\.C$"        . c++-mode)
                ("\\.cc$"       . c++-mode)
                ("\\.cpp$"      . c++-mode)
                ("\\.inl$"      . c++-mode)
                ("\\.c$"        . c++-mode)
                ("\\.H$"        . c++-mode)
                ("\\.hh$"       . c++-mode)
                ("\\.hpp$"      . c++-mode)
                ("\\.h$"        . c++-mode)
                ("\\.java$"     . java-mode)
                ("\\.pl$"       . perl-mode)
                ("\\.pm$"       . perl-mode)
                ("SConstruct"   . python-mode)
                ("SConscript"   . python-mode)
                ("\\.[Cc][Ss][Vv]$" . csv-mode)
                ("CMakeLists\\.txt$"   . cmake-mode)
                ("\\.cmake$"    . cmake-mode)
                ("\\.proto$"    . protobuf-mode)
                ("\\.folio$"    . folio-mode)
                ("\\.log$"      . log-viewer-mode)
                ("\\.pro$"      . qt-pro-mode)
                ("\\.otq$"      . conf-mode) ;one-tick-query files
                ("\\.cs$"       . csharp-mode)
                ("\\.bmk$"      . emacs-lisp-mode)
                ("README\\.md$" . gfm-mode)
                ("\\.md$"       . markdown-mode)
                ("\\.dart$"     . dart-mode)
                )
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sh-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'sh-mode-hook
          (lambda()
            (setq-default indent-tabs-mode nil)
            ;; (idle-highlight-mode 1)
            (define-key sh-mode-map "\r" 'reindent-then-newline-and-indent)
            (setq sh-basic-offset 3)
            (setq sh-indentation 3)
            (define-key sh-mode-map "\C-c\C-c" 'comment-region)
            (define-key sh-mode-map "\C-c\C-u" 'uncomment-region)
            (add-to-list 'flycheck-disabled-checkers 'sh-posix-dash)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dos-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'dos-mode-hook
          (lambda()
            (setq-default indent-tabs-mode nil)
            ;; (idle-highlight-mode 1)
            (define-key dos-mode-map "\r" 'reindent-then-newline-and-indent)
            (setq dos-basic-offset 3)
            (setq dos-indentation 3)
            (define-key dos-mode-map "\C-c\C-c" 'comment-region)
            (define-key dos-mode-map "\C-c\C-u" 'uncomment-region)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dart-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq dart-enable-analysis-server t)
;not sure this is needed (add-hook 'dart-mode-hook 'flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'nxml-mode-hook
          (lambda()
            (setq-default indent-tabs-mode nil)
            ;; (idle-highlight-mode 1)
            (define-key nxml-mode-map "\r" 'reindent-then-newline-and-indent)
            (define-key nxml-mode-map "\C-c\C-c" 'comment-region)
            (define-key nxml-mode-map "\C-c\C-u" 'uncomment-region)
            ))
(require 'mz-comment-fix)
;; the following is a hack to fix nested XML commenting in Emacs 24.
;; Note that 'comment-strip-start-length also exists for other modes if needed.
(add-to-list 'comment-strip-start-length (cons 'nxml-mode 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (add-to-list 'load-path (concat my/elisp-directory "emacs-jedi/"))
  (when (executable-find "python")
    (require 'virtualenvwrapper)
    ;(setq venv-location "?")
    ;; add jedi if installed
    (when (eq 0 (call-process "python" nil nil nil "-c" "import jedi"))
;      (setq jedi:setup-keys t)
      (setq jedi:complete-on-dot t)
      (setq jedi:tooltip-method '(popup))
      (require 'jedi)
      (require 'direx)
      (require 'jedi-direx)
      (add-hook 'jedi-mode-hook 'jedi-direx:setup)
      (add-hook 'python-mode-hook 'jedi:setup)
      (defun my/expand-jedi() (interactive)
             (auto-complete '(ac-source-jedi-direct)))
      )
    (unless (executable-find "flake8")
      (add-to-list 'flycheck-disabled-checkers 'python-flake8)
      (add-to-list 'flycheck-checkers 'python-pyflakes)
      (require 'flycheck-pyflakes)
      )))
(add-hook 'python-mode-hook
          (lambda()
            (setq-default indent-tabs-mode nil)
            (setq-local electric-indent-chars
                        (remq ?: electric-indent-chars))
            (setq forward-sexp-function nil)
            (define-key python-mode-map "\C-j" 'newline-and-indent)
            (define-key python-mode-map "\C-c\C-c" 'comment-region)
            (define-key python-mode-map "\C-c\C-u" 'uncomment-region)
            (define-key python-mode-map [?\C-\M-g] 'python-nav-forward-sexp)
            (define-key python-mode-map (kbd "\C-c RET")
              (lambda()(interactive)
                (compile (concat "python " (buffer-name)))))
            (when (featurep 'jedi)
              (define-key python-mode-map [(ctrl tab)] 'my/expand-jedi)
              (define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs-lisp-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            (define-key emacs-lisp-mode-map "\r" 'reindent-then-newline-and-indent)
            (define-key emacs-lisp-mode-map "\C-c\C-c" 'comment-region)
            (define-key emacs-lisp-mode-map "\C-c\C-u" 'uncomment-region)
            (define-key emacs-lisp-mode-map (kbd "\C-c RET")
              (lambda()(interactive)
                (byte-compile-file (buffer-file-name))))
            (if (featurep 'rainbow-mode)
                (rainbow-turn-on)
              (my/syntax-color-hex-values))
            ))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(require 'lisp-extra-font-lock)
(lisp-extra-font-lock-global-mode 1)
(require 'bytecomp)
(add-hook 'after-save-hook
          (lambda()
            (when (and
                   (eq major-mode 'emacs-lisp-mode)
                   (file-exists-p (byte-compile-dest-file (buffer-file-name)))
                   (not (string-match
                         "^\\.dir-locals.el$"
                         (file-name-nondirectory
                          (buffer-file-name)))))
              (save-excursion
                (byte-compile-file buffer-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; conf-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'conf-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            ;; (idle-highlight-mode 1)
            ;; conf-colon-mode still bound to "\C-c:"
            (local-unset-key "\C-c\C-c")
            ;; conf-unix-mode now bound to "\C-cu"
            (local-unset-key "\C-c\C-u")
            (define-key conf-mode-map "\C-cu" 'conf-unix-mode)
            (define-key conf-mode-map "\C-c\C-c" 'comment-region)
            (define-key conf-mode-map "\C-c\C-u" 'uncomment-region)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; awk-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'awk-mode-hook
          (lambda()
            (setq comment-start "#") (setq comment-end "")
            (define-key awk-mode-map "\C-c\C-c" 'comment-region)
            (define-key awk-mode-map "\C-c\C-u" 'uncomment-region)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; html-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'html-mode-hook
          (lambda()
            (if (featurep 'rainbow-mode)
                (rainbow-turn-on)
              (my/syntax-color-hex-values))
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; css-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'css-mode-hook
          (lambda()
            (if (featurep 'rainbow-mode)
                (rainbow-turn-on)
              (my/syntax-color-hex-values))
            ))

;; code ends here
