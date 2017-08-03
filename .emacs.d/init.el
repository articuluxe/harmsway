;; init.el --- Initialization file
;; Copyright (C) 2015-2017  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Friday, February 27, 2015
;; Modified Time-stamp: <2017-08-02 17:43:41 dharms>
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

;;; Commentary:

;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; load-path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-and-compile
  (defconst my/user-directory (expand-file-name
                               (if (boundp 'user-emacs-directory)
                                   user-emacs-directory
                                 "~/.emacs.d/")))
  (defconst my/scratch-directory (concat my/user-directory "etc/"))
  (defconst my/elisp-directory (concat my/user-directory "elisp/"))
  (defconst my/plugins-directory (concat my/user-directory "plugins/"))
  (setq load-path (append `(,my/plugins-directory
                            ,my/elisp-directory
                            ,(concat my/user-directory "modes/")
                            ,(concat my/user-directory "custom/"))
                          load-path))
  (setq load-path (append
                   `(
                     ,(concat my/user-directory "ext/proviso/")
                     ,(concat my/user-directory "ext/outrespace/")
                     ,(concat my/plugins-directory "multi-line/")
                     ,(concat my/plugins-directory "emacs-refactor/")
                     ,(concat my/plugins-directory "expand-region/")
                     ,(concat my/plugins-directory "multiple-cursors/")
                     ,(concat my/plugins-directory "elnode/")
                     ,(concat my/plugins-directory "magit/lisp/")
                     ,(concat my/plugins-directory "bookmark+/")
                     ,(concat my/plugins-directory "hydra/")
                     ,(concat my/plugins-directory "swiper/")
                     ,(concat my/plugins-directory "powerline/")
                     ,(concat my/plugins-directory "smart-mode-line/")
                     ,(concat my/plugins-directory "sunrise/")
                     ,(concat my/plugins-directory "diff-hl/")
                     ,(concat my/plugins-directory "vc-msg/")
                     ,(concat my/plugins-directory "vlf/")
                     ,(concat my/plugins-directory "rtags/")
                     ,(concat my/plugins-directory "auto-complete/")
                     ,(concat my/plugins-directory "yasnippet/")
                     ,(concat my/elisp-directory "emacs-jedi/")
                     ) load-path))
  )
(setq load-prefer-newer t)
(defconst my/user-settings
  (concat my/user-directory "settings/user/" user-login-name))
(load my/user-settings t)
(defconst my/system-name
  (car (reverse (split-string (symbol-name system-type) "\\/" t)))
  "A simplified result from uname.")
(defconst my/os-dir
  (concat my/user-directory "settings/os/" my/system-name "/")
  "Directory in which os-specific settings reside.")
(defconst my/gui-dir
  (concat my/user-directory "settings/gui/")
  "A path to a directory containing window-system-specific settings.")

(eval-when-compile
  (defvar use-package-verbose)          ;silence warning
  (setq use-package-verbose t)
  (require 'use-package))
(require 'bind-key)

(set-register ?~ (cons 'file "~/"))
(set-register ?\C-i (cons 'file user-init-file)) ;edit init file
(set-register ?\C-d (cons 'file "~/Documents"))
(set-register ?\C-k (cons 'file "~/Desktop"))
(set-register ?\C-w (cons 'file "~/Downloads"))
(set-register ?\C-p (cons 'file "~/Dropbox"))
(set-register ?\C-s (cons 'file "~/src"))
(set-register ?\C-h (cons 'file "~/src/harmsway"))
(set-register ?\C-e (cons 'file "~/src/harmsway/.emacs.d"))
(set-register ?\C-o (cons 'file "~/org"))

(use-package custom-backups
  :defines my/backup-exclude-regex
  :init
  (setq my/backup-exclude-regex
        "recentf\\|ido-last\\|emacs-bmk-bmenu-state\\|COMMIT_EDITMSG")
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
;;  truncate long lines
(setq-default truncate-lines t)
(bind-key "M-o c" 'canonically-space-region)
(bind-key "C-x c" 'capitalize-region)
;; default tab width
(setq-default tab-width 4)
;; enable repeatedly popping mark without prefix
(setq set-mark-command-repeat-pop t)
(setq auto-revert-verbose nil)
(unless (version< emacs-version "24.4")
  (defun my/multi-pop-to-mark (orig-fun &rest args)
    "Call ORIG-FUN until the cursor moves. Try the repeated popping
up to 10 times."
    (let ((p (point)))
      (dotimes (i 10)
        (when (= p (point))
          (apply orig-fun args)))))
  (advice-add 'pop-to-mark-command :around
              #'my/multi-pop-to-mark))
;; show current function
(which-function-mode t)
;; winner mode
(winner-mode 1)
;; don't try to create "other files"
(defvar ff-always-try-to-create)        ;silence warning
(setq ff-always-try-to-create nil)
;; Preserve line position on scroll
(setq scroll-preserve-screen-position t)
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
(when (version< emacs-version "24.3")
  (setq-default display-buffer-reuse-frames t))
;; visual settings
(menu-bar-mode -1)
(setq-default fill-column 78)
(defun my/disable-scroll-bars (frame)
  "Disable scroll bars from frame FRAME."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(setq mouse-yank-at-point t)
(mouse-avoidance-mode 'cat-and-mouse)
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(setq ring-bell-function 'ignore)
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
;; this removes the binding for "M-'" to 'abbrev-prefix-mark, without which we
;; can use "M-'" as a prefix binding (one which happens to work in the
;; terminal as well).
(define-key esc-map "'" nil)
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "C-\\"))
;; add shortcut for terminals where C-S-DEL doesn't work
(global-set-key (kbd "M-' DEL") 'kill-whole-line)
(global-set-key (kbd "C-x M-;") 'comment-line)
(global-set-key (kbd "C-x M-p") 'transpose-paragraphs)
(global-set-key [(next)] 'scroll-up-line)
(global-set-key [(prior)] 'scroll-down-line)
(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key "\C-c5" 'toggle-truncate-lines)
(global-set-key "\C-c " 'whitespace-mode)
(global-set-key "\C-c0fb" 'font-lock-fontify-buffer)
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

;; (when (version< "24.3" emacs-version)
;;   (require 's))
;; (require 'f)
;; (require 'deferred)
;; (require 'concurrent)
;; (require 'epc)
;; (require 'epcs)

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



(load-library "compiling")
(load-library "coding")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; proviso ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package proviso
  :demand t
  :bind (("C-c g" . proviso-grep)
         ("C-c t" . proviso-gentags-generate-tags)
         ("C-c pd" . proviso-open-dired-this-project)
         ("C-c pD" . proviso-open-dired-all-projects)
         ("C-c pp" . proviso-display-print-project)
         ("C-c pn" . proviso-display-echo-project-names)
         ("C-c pe" . proviso-display-projects)
         ("C-c pg" . proviso-refresh-current-project)
         )
  :config
  (use-package proviso-frame-title)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (> emacs-major-version 24)
  (global-set-key "\M-*" 'xref-pop-marker-stack)
  (global-set-key "\M-," 'tags-loop-continue)
  )

;; select
(use-package proviso-etags-select
  :init
  (setq tags-revert-without-query t)
  :bind (("M-." . etags-select-find-tag)
         ([?\C-\M-.] . etags-select-find-tag-at-point))
  :demand t
  :config
  ;; stack
  (use-package etags-stack :bind ("C-c C-t" . etags-stack-show))
  ;; table
  (use-package proviso-etags-table
    :init
    ;; we store our tags in a specific directory
    (setq etags-table-search-up-depth nil)
    )
  )

(use-package custom-utils
  :bind (("C-x C-M-e" . sanityinc/eval-last-sexp-or-region)
         ("C-h C-f" . find-function-view)
         ("M-g g" . goto-line-with-feedback)
         )
  :commands
  (insert-now now insert-today today find-file-upwards find-file-dir-upwards
              goto-line-with-feedback goto-line
              shell-command-redirected-output
              sanityinc/eval-last-sexp-or-region
              find-function-view
              ))

(use-package custom-text-utils
  :bind (("M-s i" . my/indent-line-relative)
         ("\e\e\\" . jump-to-matching-paren)
         ("M-]" . highlight-paren-right)
         ("M-[" . highlight-paren-left)
         ("M-s p" . highlight-enclosing-paren)
         ("\e\er" . highlight-current-sexp)
         ("C-c q" . clean-up-buffer)
         ("\e\e(" . enclose-by-braces-paren)
         ("\e\e[" . enclose-by-braces-bracket)
         ("\e\e{" . enclose-by-braces-brace)
         ("\e\e<" . enclose-by-braces-caret)
         ))

(use-package load-environment-vars
             :commands (load-environment-variable-from-file
                        load-environment-variables-from-file))

(use-package custom-environment
  :commands my/load-environment-variables-from-file)

(use-package custom-buffer-utils
  :bind (("C-x C-r" . my/revert-buffer)
         ("C-x K" . kill-other-buffers)
         ("\e\ep" . switch-to-most-recent-buffer)
         ("C-x 4z" . window-toggle-split-direction)
         ("C-x 4s" . swap-buffers)
         ("C-c 0w" . my/toggle-window-dedicated)
         ("C-x 5x" . move-buffer-to-new-frame)
         )
  :commands
  (move-buffer-file move-buffer-to-new-frame)
  )

(use-package custom-word-count
  :if (version< emacs-version "24.0")
  :bind ("M-=" . wordcount)
  )

(use-package custom-coding
  :bind (("C-c C-p" . print-current-function)
         ("C-c ii" . add-header-include-ifdefs)
         ("C-c h" . insert-class-header)
         ("C-c c" . insert-cast)
  ))
(use-package custom-gud
  :bind (("C-c 4" . my/launch-gdb)
         ([f4] . my/launch-gdb)
         )
  :defines (gdb-show-main
            gdb-show-changed-values
            gdb-use-colon-colon-notation
            gdb-create-source-file-list)
  :config
  (add-hook 'gud-mode-hook
            (lambda()
              (set (make-local-variable 'gdb-show-main) t)
              ;; highlight recently-changed variables
              (set (make-local-variable 'gdb-show-changed-values) t)
              ;; watch expressions sharing same variable name
              (set (make-local-variable 'gdb-use-colon-colon-notation) t)
              (set (make-local-variable 'gdb-create-source-file-list) nil)
              (gdb-many-windows 1)
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; align ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun align-values (start end)
  "Vertically align region from START to END.
Alignment will be based on lengths of the first value of each
line."
  (interactive "r")
  (align-regexp start end
                "\\S-+\\(\\s-+\\)"
                1 2 nil))
(global-set-key "\C-caa" 'align)
(global-set-key "\C-car" 'align-regexp)
(global-set-key "\C-cav" 'align-values)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; good-word ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package good-word
  :commands good-word/init-word-processor
  :bind ("M-o w" . hydra-toggle-word-processor/body)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; outrespace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package outrespace
  :commands (outrespace-print-enclosing-ns-name
             outrespace-wrap-namespace-region
             outrespace-ivy-jump-to-ns
             outrespace-change-ns-name
             outrespace-change-enclosing-ns-name
             outrespace-delete-ns-by-name
             outrespace-delete-enclosing-ns
             outrespace-highlight-ns-by-name
             )
  :after cc-mode
  :config
  ;; (setq outrespace-prefix-key "\C-cx")  ;to change prefix
  (outrespace-mode 1)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; epa ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/add-epa-file-encrypt-to ()
  "Add a file local variable for `epa-file-encrypt-to'."
  (interactive)
  (add-file-local-variable-prop-line 'epa-file-encrypt-to
                                     (concat "(" user-mail-address ")")))
(setenv "GPG_AGENT_INFO" nil)
(use-package epa
  :bind (("C-c 09l" . epa-list-keys)
         ("C-c 09L" . epa-list-secret-keys)
         ("C-c 09k" . epa-delete-keys)
         ("C-c 09i" . epa-import-keys)
         ("C-c 09I" . epa-import-keys-region)
         ("C-c 09a" . epa-import-armor-in-region)
         ("C-c 09x" . epa-export-keys)
         ("C-c 09y" . epa-insert-keys)
         ("C-c 09q" . epa-select-keys)
         ("C-c 09d" . epa-decrypt-file)
         ("C-c 09D" . epa-decrypt-region)
         ("C-c 09e" . epa-encrypt-file)
         ("C-c 09E" . epa-encrypt-region)
         ("C-c 09A" . epa-decrypt-armor-in-region)
         ("C-c 09v" . epa-verify-file)
         ("C-c 09V" . epa-verify-region)
         ("C-c 09C" . epa-verify-cleartext-in-region)
         ("C-c 09s" . epa-sign-file)
         ("C-c 09S" . epa-sign-region)
         ("C-c 099" . my/add-epa-file-encrypt-to)
         )
  :defines epa-file-select-keys
  :init
  ;; may no longer be necessary
  (setq epa-file-cache-passphrase-for-symmetric-encryption t)
  :config
  (setq epg-gpg-program "gpg2")
  (setq epa-file-select-keys nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; aes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/aes-default-group "  default")
(use-package aes
  :demand t
  :bind (("C-c 08e" . aes-encrypt-current-buffer)
         ("C-c 08d" . aes-decrypt-current-buffer)
         ("C-c 08t" . aes-toggle-encryption)
         ("C-c 08x" . aes-remove-encryption-hook)
         )
  :config
  (setq aes-always-ask-for-passwords nil)
  (setq aes-enable-plaintext-password-storage t)
  (setq aes-delete-passwords-after-idle 0)
  (aes-enable-auto-decryption)
  (add-hook 'aes-path-passwd-hook (lambda (path) my/aes-default-group))
  ;; if the environment variable is not defined, we will be prompted
  ;; for the password
  (setq aes--plaintext-passwords
        (let ((pwd (or (getenv "EMACS_PWD") "nil")))
          (list (cons my/aes-default-group pwd))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(my/load-environment-variables-from-file my/os-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; full-edit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package full-edit :bind ("C-c C-f" . full-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; c-includer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package c-includer
  :bind ("C-c it" . makey-key-mode-popup-c-includer-brackets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cleanup-funcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cleanup-funcs
  :bind ("C-c ic" . makey-key-mode-popup-c-cleanup-funcs))

;; Show selections
(transient-mark-mode 1)
;; Insertion while text is selected deletes the selected text
(delete-selection-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multi-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
  multi-line
  :bind (("C-`" . multi-line)
         ("M-' `" . multi-line)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs-refactor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emr
  :defer t
  :init
  (with-eval-after-load 'prog-mode
    (require 'emr)
    (bind-key "C-c b" 'emr-show-refactor-menu)
    )
  :config
  (emr-initialize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sudo-edit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sudo-edit :bind ("C-c M-r" . sudo-edit))

;; work around bug in cc-mode in emacs 24.4
;; see debbugs.gnu.org/db/18/18845.html
(eval-and-compile
  (when (< emacs-major-version 24)
    (setq load-path (cons (concat my/elisp-directory "compat/24/0/-/")
                          load-path)))
  (when (< emacs-major-version 25)
    (setq load-path (cons (concat my/elisp-directory "compat/25/0/-/")
                          load-path)))
  )
(eval-when-compile
  (if (and (= emacs-major-version 24) (= emacs-minor-version 4))
      (require 'cl))
  (if (or (and (= emacs-major-version 24) (< emacs-minor-version 4))
          (< emacs-major-version 24))
      (unless (fboundp 'with-eval-after-load)
        (defmacro with-eval-after-load (file &rest body)
          (declare (indent 1) (debug t))
          `(eval-after-load ,file '(progn ,@body)))))
  )
(if (= emacs-major-version 23)
    (progn
      (autoload 'protobuf-mode "protobuf-mode" "Major mode for editing protobuf files." t)
      (autoload 'csharp-mode "csharp-mode" "Major mode for editing csharp files." t)
      )
  (use-package protobuf-mode :mode "\\.proto$")
  (use-package csharp-mode :mode "\\.cs$")
  (use-package sln-mode :mode "\\.sln$")
  )

(use-package pos-tip :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rotate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rotate
  :bind (:map ctl-x-4-map
              ("l" . rotate-layout)
              ("w" . rotate-window)
              ("h" . rotate:even-horizontal)
              ("\C-h" . rotate:main-horizontal)
              ("v" . rotate:even-vertical)
              ("\C-v" . rotate:main-vertical)
              ("t" . rotate:tiled)
              ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; electric-buffer-list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ebuff-menu :bind ("C-x M-b" . electric-buffer-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ibuffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :defines ibuffer-show-empty-filter-groups
  :init
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (add-hook 'ibuffer-mode-hook
            (lambda()
              (ibuffer-auto-mode 1)
              (ibuffer-vc-set-filter-groups-by-vc-root)
              ))
  :config
  (use-package ibuffer-vc)
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
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark modified read-only vc-status-mini " "
                (size-h 9 -1 :right)
                " "
                (name 26 -1))
          (mark modified read-only vc-status-mini " "
                (size-h 9 -1 :right)
                " "
                (filename-and-process 26 -1))
          ))
  )

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
(use-package yascroll :config (global-yascroll-bar-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; iflipb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
  iflipb
  :bind (("<C-tab>" . iflipb-next-buffer)
         ("M-' TAB" . iflipb-next-buffer)
         ("<C-S-iso-lefttab>" . iflipb-previous-buffer)
         ("M-' <backtab>" . iflipb-previous-buffer)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; copyright ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package copyright
  :defines (copyright-query copyright-year-ranges)
  :init
  ;; copyright-update is added to my/before-save-hook below
  (setq copyright-query nil)
  (setq copyright-year-ranges t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hide-lines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hide-lines :bind ("C-c l" . hide-lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; line-comment-banner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package line-comment-banner
  :bind ([?\C-c?\C-/] . line-comment-banner)
  :defines comment-fill
  :init
  (add-hook 'c-mode-common-hook
            (lambda() (make-local-variable 'comment-fill)
              (setq comment-fill "*")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; list-register ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package list-register :bind ("C-x rv" . list-register))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mwim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mwim :bind ("M-m" . mwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; discover-my-major ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package discover-my-major :bind ("C-h C-m" . discover-my-major))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; expand-region ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("M-' '" . er/expand-region)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; embrace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package embrace
  :bind (("C-=" . embrace-commander)
         ("M-' =" . embrace-commander)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; iedit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
 iedit
 :bind (("C-;" . iedit-mode)
        ("M-' ;" . iedit-mode)
        )
 :config
 ;; S-TAB does not work in the terminal
 (bind-key "M-' TAB" 'iedit-prev-occurrence iedit-lib-keymap)
 )
(use-package
 iedit-rect
 :bind ("C-x r RET" . iedit-rectangle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple-cursors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mc/add-cursor ()
  "Add a fake cursor at point."
  (interactive)
  (require 'multiple-cursors)
  (mc/create-fake-cursor-at-point))
(defun mc/activate ()
  "Activate `multiple-cursors-mode'."
  (interactive)
  (require 'multiple-cursors)
  (mc/maybe-multiple-cursors-mode))
(use-package multiple-cursors
  :bind (
         ("C-\\ C-\\ C-\\" . mc/edit-lines)
         ("C-\\ C-a" . mc/edit-beginnings-of-lines)
         ("C-\\ C-e" . mc/edit-ends-of-lines)
         ("C-\\ /" . set-rectangular-region-anchor)
         ;; mark one more occurrence
         ("C->" . mc/mark-next-like-this)
         ("C-\\ ." . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-\\ ," . mc/mark-previous-like-this)

         ("C-+" . mc/mark-next-word-like-this)
         ("C-\\ =" . mc/mark-next-word-like-this)
         ("C-M-+" . mc/mark-next-like-this-word)
         ("C-\\ M-=" . mc/mark-next-like-this-word)
         ("C-}" . mc/mark-next-symbol-like-this)
         ("C-\\ ]" . mc/mark-next-symbol-like-this)
         ("C-M-}" . mc/mark-next-like-this-symbol)
         ("C-\\ M-]" . mc/mark-next-like-this-symbol)

         ("C-\\ -" . mc/mark-previous-word-like-this)
         ("C-M-_" . mc/mark-previous-like-this-word)
         ("C-\\ M--" . mc/mark-previous-like-this-word)
         ("C-{" . mc/mark-previous-symbol-like-this)
         ("C-\\ [" . mc/mark-previous-symbol-like-this)
         ("C-M-{" . mc/mark-previous-like-this-symbol)
         ("C-\\ M-[ [" . mc/mark-previous-like-this-symbol)
         ("C-\\ M-[ M-[" . mc/mark-previous-like-this-symbol)

         ("C-| C-|" . mc/mark-more-like-this-extended)
         ("C-\\ C-c" . mc/mark-more-like-this-extended)

         ;; mark many occurrences
         ("C-\\ ;" . mc/mark-all-like-this)
         ("C-\\ w" . mc/mark-all-words-like-this)
         ("C-\\ s" . mc/mark-all-symbols-like-this)

         ("C-\\ :" . mc/mark-all-in-region)
         ("C-\\ M-;" . mc/mark-all-in-region-regexp)

         ("C-S-h ;" . mc/mark-all-like-this-in-defun)
         ("C-\\ h;" . mc/mark-all-like-this-in-defun)
         ("C-S-h w" . mc/mark-all-words-like-this-in-defun)
         ("C-\\ hw" . mc/mark-all-words-like-this-in-defun)
         ("C-S-h s" . mc/mark-all-symbols-like-this-in-defun)
         ("C-\\ hs" . mc/mark-all-symbols-like-this-in-defun)

         ("C-S-h C-S-h" . mc/mark-all-like-this-dwim)
         ("C-\\ hh" . mc/mark-all-like-this-dwim)
         ("C-c M-;" . mc/mark-all-dwim)

         ("C-S-SPC" . mc/mark-pop)
         ("C-\\ C-SPC" . mc/mark-pop)

         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-\\ C-\\ C-." . mc/add-cursor)
         ("C-\\ C-\\ C-c" . mc/activate)
         )
  :init
  (setq mc/list-file (concat my/user-directory "mc-lists.el"))
  (setq mc/edit-lines-empty-lines 'ignore)
  :config
  (define-key mc/keymap (kbd "RET") 'multiple-cursors-mode)
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
  ;; On terminals this conflicts with undo (had to take this out of the
  ;; autoloads to make the conditional work).
  (when (display-graphic-p)
    (bind-key "C-_" 'mc/mark-previous-word-like-this))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; phi-search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package phi-search
  :bind (("C-S-s" . phi-search)
         ("C-\\ C-s" . phi-search)
         ("C-S-r" . phi-search-backward)
         ("C-\\ C-r" . phi-search-backward)
         )
  :config
  (add-to-list 'phi-search-additional-keybinds
               '((kbd "M-RET") . 'phi-search-complete-at-beginning)
               '((kbd "M-<return>") . 'phi-search-complete-at-beginning)
               )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; iy-go-to-char ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package iy-go-to-char
  :bind (("C-S-f" . iy-go-to-char)
         ("C-\\ f" . iy-go-to-char)
         ("C-S-b" . iy-go-to-char-backward)
         ("C-\\ b" . iy-go-to-char-backward)
         ("C-\\ C-f" . iy-go-to-or-up-to-continue)
         ("C-\\ C-b" . iy-go-to-or-up-to-continue-backward)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; phi-grep ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package phi-grep
  :bind ("M-s M-s g" . phi-grep-in-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; figlet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package figlet
  :bind (("C-c 0ff" . figlet)
         ("C-c 0fc" . figlet-comment)
         ("C-c 0fr" . figlet-figletify-region)
         ("C-c 0fC" . figlet-figletify-region-comment)
         ("C-c 0fp" . figlet-preview-fonts)
         )
  :commands (figlet figlet-comment figlet-figletify-region
                    figlet-figletify-region-comment
                    figlet-preview-fonts)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ascii ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ascii :bind ("\e\ea" . ascii-display))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ascii-table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ascii-table :commands ascii-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 0xc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package 0xc
 :bind (("M-s M-s hc" . 0xc-convert)
        ("M-s M-s h." . 0xc-convert-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elnode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elnode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq vc-follow-symlinks nil)
(bind-key "C-x vR" 'vc-region-history)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; makefile-executor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package makefile-executor
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/git-keymap)
(define-prefix-command 'my/git-keymap)
(global-set-key "\M-sm" 'my/git-keymap)
(defun my/enter-magit-status-fullscreen ()
  "Enter magit's status window, filling the entire frame."
  (interactive)
  (let ((magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
    (magit-status)))
(use-package magit
  :if (not (version< emacs-version "24.4"))
  :init
  ;; The following allows remote repositories to work under tramp on windows
  ;; (plink), and we put git in our exec-path anyways, so the full path is
  ;; unneeded.  This is also the default setting anyway on other platforms.
  (setq magit-git-executable "git")
  (setq magit-popup-show-common-commands nil)
  (setq magit-log-show-refname-after-summary nil)
  (setq magit-no-confirm '())
  (setq magit-process-find-password-functions 'magit-process-password-auth-source)
  (setq magit-auto-revert-tracked-only t)
  (setq magit-prefer-remote-upstream t)
  ;; git commands
  :bind (:map my/git-keymap
              ("g" . magit-status)
              ("SPC" . my/enter-magit-status-fullscreen)
              ("l" . magit-list-repositories)
              ("M-g" . magit-dispatch-popup)
              ("f" . magit-find-file) ;; view arbitrary blobs
              ("4f" . magit-find-file-other-window)
              ("h" . magit-log-buffer-file) ;; show all commits that touch current file
              ("y" . magit-cherry)
              ("e" . ediff-merge-revisions-with-ancestor) ;; to see all differences, even those automatically merged
              ("m" . magit-toggle-margin)
              ("b" . magit-blame)
              ("U" . magit-unstage-all) ;; unstage all changes (like SU but forces HEAD)
              ("s" . magit-stage-file)
              ("u" . magit-unstage-file)
              ("r" . magit-reset-soft) ;; soft reset; hard reset can use C-u x
              ("d" . magit-diff-buffer-file-popup)
              ("c" . magit-clone)
              ("x" . magit-clean)
              ("k" . magit-checkout-stage)
              )
  :config
  (put 'magit-clean 'disabled nil)
  (use-package with-editor)
  (global-magit-file-mode 1)
  (magit-auto-revert-mode 0)
  (setq magit-repository-directories
        `(,(cons (expand-file-name "~/src") 2)))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; add ido shortcut
  (if (version< emacs-version "25.1")
      (add-hook
       'ido-setup-hook
       (lambda() (define-key ido-completion-map
                   (kbd "C-x g") 'ido-enter-magit-status)))
    (define-key ido-common-completion-map
      (kbd "C-x g") 'ido-enter-magit-status))

  (add-hook 'magit-revision-mode-hook 'bug-reference-mode)
  (add-hook 'git-commit-setup-hook 'bug-reference-mode)
  ;; add argument --no-merges to log
  (magit-define-popup-switch 'magit-log-popup
    ?m "Omit merge commits" "--no-merges")
  (setq magit-log-arguments (append
                             (list "--color")
                             magit-log-arguments))
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  ;; to display fullframe, use 'magit-display-buffer-fullframe-status-v1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git-timemachine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-timemachine
  :bind (:map my/git-keymap ("t" . git-timemachine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vc-msg ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vc-msg
  :bind (:map my/git-keymap ("." . vc-msg-show)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shell-pop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package shell-pop
  :bind (("<f1>" . shell-pop)
         ("C-c 1" . shell-pop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; terminal-here ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package terminal-here
  :bind (("C-c <f1>" . terminal-here-launch)
         ("C-c <f2>" . terminal-here-project-launch))
  :init
  (setq terminal-here-project-root-function #'proviso-current-project-root)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; simpleclip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package simpleclip
  :config
  (simpleclip-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shackle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package shackle
  :config
  (setq shackle-default-size 0.4)
  (setq shackle-select-reused-windows nil)
  (setq shackle-rules
        '(
          (occur-mode :popup t :select nil :align bottom)
          (vlf-occur-mode :popup t :select nil :align bottom)
          (grep-mode :popup t :select nil :align bottom)
          ("Help" :regexp t :popup t :select t)
          ("xref" :regexp t :popup t :select t :align bottom)
          (help-mode :popup t :select t)
          (diff-mode :popup t :select t)
          (apropos-mode :popup t :select t)
          (completion-list-mode :select nil)
          ("compilation" :regexp t :popup t :select nil :align bottom)
          (command-history-mode :popup t :select t)
          ("Shell Command Output" :regexp t :popup t :select nil)
          ("COMMIT_EDITMSG" :select t)
          ("VC-history" :regexp t :select t :popup t)
          ("gentags" :regexp t :popup t :select nil :align bottom)
          ("json-path" :regexp t :popup t :select t :align bottom)
          ))
  (shackle-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hl-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hl-line+
  :bind ("M-s L" . hl-line-flash)
  :init
  (setq hl-line-sticky-flag nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; crosshairs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package crosshairs
  :bind ("M-s l" . crosshairs-flash)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; form-feed ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package form-feed
  :bind ("C-c 0-" . form-feed-mode)
  :disabled
  :init
  (setq form-feed-line-width -1)
  (add-hook 'text-mode-hook 'form-feed-mode)
  ;; markdown-mode does not work here: it resets its font-lock-keywords
  ;; (add-hook 'markdown-mode-hook 'form-feed-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; page-break-lines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package page-break-lines
  :config
  (setq page-break-lines-modes
        (append '(markdown-mode text-mode) page-break-lines-modes))
  (global-page-break-lines-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; beacon ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :bind (("M-s C-l" . beacon-blink)
         ("C-c 0b" . beacon-mode))
  :defer t
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'etags-select-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bookmark+ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bookmark+
  :bind (
         ;; C-x p <left>
         ("<f7>" . bmkp-previous-bookmark)
         ;; C-x p <up>
         ("<S-f7>" . bmkp-previous-bookmark-this-file/buffer)
         ("C-c 7" . bmkp-previous-bookmark)
         ("C-c M-7" . bmkp-previous-bookmark-this-file/buffer)
         ;; C-x p <right>
         ("<f8>" . bmkp-next-bookmark)
         ;; C-x p <down>
         ("<S-f8>" . bmkp-next-bookmark-this-file/buffer)
         ("C-c 8" . bmkp-next-bookmark)
         ("C-c M-8" . bmkp-next-bookmark-this-file/buffer)
         ("C-x p%l" . bmkp-set-autonamed-bookmark-at-line)
         ("C-x p%b" . bmkp-set-autonamed-regexp-buffer)
         ("C-x p%r" . bmkp-set-autonamed-regexp-region)
         )
  :demand t
  :config
  (setq bookmark-default-file (concat my/user-directory "bookmarks"))
  (setq bmkp-bmenu-state-file (concat my/user-directory "emacs-bmk-bmenu-state"))
  (setq bookmark-save-flag nil)
  (setq bmkp-crosshairs-flag nil)
  (setq bmkp-last-as-first-bookmark-file nil)
  (add-hook 'bookmark-after-jump-hook #'crosshairs-flash)
  (add-hook 'after-init-hook
            (lambda ()
              (unless (> (length command-line-args) 1)
                (bookmark-bmenu-list)
                (switch-to-buffer "*Bookmark List*"))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; savehist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package savehist
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring kill-ring compile-history
                      ivy-dired-history-variable))
  (setq savehist-file (concat my/user-directory "history"))
  (setq savehist-save-minibuffer-history t)
  (setq history-length 50)
  (setq history-delete-duplicates t)
  (put 'minibuffer-history 'history-length 100)
  (put 'kill-ring 'history-length 25)
  (savehist-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; recentf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package recentf
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-max-menu-items 12)
  (setq recentf-save-file (concat my/user-directory "recentf"))
  (setq recentf-exclude '( "-tags\\'" "ido\.last\\'" "emacs-bmk-bmenu-state"))
  (recentf-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; uniquify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package uniquify
  :config
  ;; append unique parent directory to buffers of same name
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package ido
;;   :defines (ido-temp-list)
;;   :defer t
;;   :config
;;   (setq ido-save-directory-list-file (concat my/user-directory "ido-last"))
;;   (setq ido-max-prospects 25)
;;   ;; (setq ido-enable-flex-matching t)
;;   ;; (setq ido-case-fold t)
;;   (setq ido-auto-merge-work-directories-length -1) ;disables auto-merge
;;   (add-to-list 'ido-work-directory-list-ignore-regexps tramp-file-name-regexp)
;;   ;; ask before reusing an existing buffer
;;   (setq-default ido-default-buffer-method 'maybe-frame)
;;   (setq-default ido-default-file-method 'maybe-frame)
;;   (ido-mode 1)

;;   ;; sort files by descending modified time (except remotely, which is dog-slow)
;;   (defun ido-sort-mtime()
;;     (unless (tramp-tramp-file-p default-directory)
;;       (setq ido-temp-list
;;             (sort ido-temp-list
;;                   (lambda (a b)
;;                     (time-less-p
;;                      (sixth (file-attributes (concat ido-current-directory b)))
;;                      (sixth (file-attributes (concat ido-current-directory a)))))))
;;       ;; (ido-to-end
;;       ;;  (delq nil (mapcar (lambda (x)
;;       ;;                      (and (char-equal (string-to-char x) ?.) x))
;;       ;;                    ido-temp-list)))
;;       ))
;;   (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
;;   (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

;;   (when (< emacs-major-version 24)
;;     ;; use ido to switch modes when smex is not available
;;     (global-set-key (kbd "M-x")
;;                     (lambda() (interactive)
;;                       (call-interactively
;;                        (intern
;;                         (ido-completing-read
;;                          "M-x " (all-completions "" obarray 'commandp)))))))
  ;; for recentf
  ;; (unless (featurep 'uniquify-recentf)
  ;;   (defun recentf-ido-find-file()
  ;;     "Find a recent file using ido."
  ;;     (interactive)
  ;;     (let ((file (ido-completing-read
  ;;                  "Choose recent file:"
  ;;                  recentf-list nil t)))
  ;;       (when file (find-file file))))
  ;;   (global-set-key "\er" 'recentf-ido-find-file))
  ;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ace-window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ace-window
  :bind ("M--" . ace-window)
  :init
  (setq aw-reverse-frame-list t)
  (setq aw-dispatch-alist
        '(
          (?x aw-delete-window " Ace-Delete")
          (?s aw-swap-window " Ace-Swap")
          (?m aw-move-window " Ace-Move")
          (?n aw-flip-window)
          (?o delete-other-windows)
          (?b balance-windows)
          (?i delete-other-windows " Ace-Delete Other Windows")
          (?v aw-split-window-vert " Ace-Split Vert")
          (?h aw-split-window-horz " Ace-Split Horiz")
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; isearch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq isearch-allow-scroll t)
;; allow stopping isearch at opposite end
(defun my/isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
(define-key isearch-mode-map [(meta return)] #'my/isearch-exit-other-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dumb-jump ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dumb-jump
  :bind (("M-o M-j" . dumb-jump-go-prompt)
         ("M-o j" . dumb-jump-go)
         ("M-o o" . dumb-jump-go-other-window)
         ("M-o b" . dumb-jump-back)
         ("M-o q" . dumb-jump-quick-look)
         ("M-o n" . dumb-jump-go-prefer-external)
         ("M-o m" . dumb-jump-go-prefer-external-other-window)
         )
  :config
  (setq dumb-jump-selector 'ivy)
  (push ".proviso" dumb-jump-project-denoters)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; plur ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
  plur
  :if (not (version< emacs-version "24.4"))
  :bind (("M-s M-s q" . plur-isearch-forward)
        ("M-s q" . plur-replace)
        ("M-s M-q" . plur-query-replace)
        :map isearch-mode-map
        ("C-p" . plur-isearch-forward)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smartscan ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartscan
  :bind (("M-n" . smartscan-symbol-go-forward)
         ("M-p" . smartscan-symbol-go-backward))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hydra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hydra
  :defer t
  :init
  (setq lv-use-separator t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; grep ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'grep
  (use-package wgrep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; occur ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-s M-o") 'multi-occur-in-matching-buffers)
(bind-key "C-c b" 'bmkp-occur-create-autonamed-bookmarks occur-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; swiper ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package swiper
  :if (not (version< emacs-version "24.1"))
  :bind (("M-s s" . swiper)
         ("M-s M-a" . swiper-all)
         :map isearch-mode-map
         ("C-o" . swiper-from-isearch))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; avy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :bind (("\e\ec" . avy-goto-char)
         ("\e\ev" . avy-goto-char-2)
         ("\e\et" . avy-goto-char-timer)
         ("\e\ew" . avy-goto-word-1)
         ("\e\el" . avy-goto-line)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ivy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :demand t
  :config
  (setq ivy-wrap t)
  (global-set-key "\e\eii" 'ivy-resume)
  (setq ivy-display-style 'fancy)
  (setq ivy-extra-directories '("../" "./"))
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  ;; (defun ivy-insert-action (x)
  ;;   (with-ivy-window
  ;;     (insert x)))
  ;; (ivy-set-actions
  ;;  t
  ;;  '(("I" ivy-insert-action "insert")))
(use-package ivy-rich
  :init
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; counsel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :bind (("M-s M-f" . counsel-git)
         ("M-s M-r" . counsel-recentf)
         ("M-s M-p" . counsel-git-grep)
         ("M-s M-g" . counsel-grep)
         ("M-s M-z" . counsel-git-stash)
         ("M-s M-i" . counsel-imenu)
         ("M-s M-l" . counsel-git-log)
         ("M-s M-e" . counsel-find-file-extern)
         ("M-s M-d" . counsel-dired-jump)
         ("M-s M-v" . counsel-file-jump)
         ("M-s M-y" . counsel-yank-pop)
         ("M-s M-b" . counsel-load-library)
         ("M-s M-m" . counsel-load-theme)
         ("M-s M-x" . counsel-command-history)
         ("M-s M-k" . counsel-descbinds)
         ("C-c 0g" . counsel-locate)
         ("M-s M-SPC" . counsel-mark-ring)
         ("C-h C-a" . counsel-apropos)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line-and-call)
         )
  :commands (counsel-M-x counsel-find-file)
  :demand t
  :init
  (when (eq system-type 'darwin)
    (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind))
  :config
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        (concat
         "\\(?:^[#]\\)"                 ;start with #
         "\\|\\(?:^\\.[^.]\\)"          ; or a single .
         "\\|\\(?:[#~]$\\)"             ;end with # or ~
         "\\|\\(?:\\.elc$\\)"           ;byte-compiled
         ))                             ;toggle with C-c C-a
  (setf (cdr (assoc 'counsel-M-x ivy-initial-inputs-alist)) "")
  ;; fallback to basic find-file
  (define-key counsel-find-file-map "\C-x\C-f"
    (lambda ()
      (interactive)
      (ivy-set-action
       (lambda (x)
         (let ((completing-read-function 'completing-read-default))
           (call-interactively 'find-file))))
      (ivy-done)))
  (counsel-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smex
  :if (<= 24 emacs-major-version)
  :config
  (smex-initialize)
  ;; the old M-x
  (global-set-key "\e\ex" 'execute-extended-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; imenu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package popup-imenu
  :bind ("C-c C-j" . popup-imenu)
  :config
  (setq popup-imenu-position 'point)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; powerline ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; does not interact with rich-minority mode: try delight.el?
;; (powerline-default-theme)
(use-package powerline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rich-minority ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rich-minority
  ;; this dependency actually comes from smart-mode-line, which uses
  ;; rich-minority.
  :if (version<= "24.3" emacs-version)
  :config
  (rich-minority-mode 1)
  (setq rm-blacklist
        '(" AC" " yas" " Undo-Tree" " Abbrev" " Guide" " Hi" " $" " ,"
          " Ifdef" " Rbow" " ivy" " ElDoc" " (*)" " wg" " ⛓" " GitGutter"
          " Fly" " drag" " mc++fl" " ARev" " Spnxd" " PgLn" " ^L" " be"
          " counsel" " ivy"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smart-mode-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smart-mode-line
  :if (version<= "24.3" emacs-version)
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  )

;; undo
(unless (boundp 'warning-suppress-types)
  (setq warning-suppress-types nil))
(push '(undo discard-info) warning-suppress-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; undo-tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :config
  ;; unset this key for use in other packages
  (define-key undo-tree-map "\C-_" nil)
  ;; reset the undo tree history (useful after reverting buffer)
  (global-set-key "\C-cu" (lambda()(interactive)(setq buffer-undo-tree nil)))
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; goto-chg ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package goto-chg
  :bind (([?\C-.] . goto-last-change)
         ("M-' ." . goto-last-change)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tramp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tramp
  :defer t
  :config
  (when (boundp 'my/user-name)
    (setq tramp-default-user my/user-name))
  ;; my/autosave-dir defined in custom-backups.el
  (setq tramp-auto-save-directory my/autosave-dir)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp tramp-file-name-regexp))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; outline-magic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/add-outline-cycle-binding (map)
  "Add a key binding into keymap MAP for cycling outlines."
  (bind-key "C-c -" 'outline-cycle map))
(use-package
 outline-magic
 :bind ("C-c -" . outline-cycle)
 :commands outline-cycle
 :init
 (setq outline-structedit-modifiers '(control shift)) ;TODO: doesn't work
 (add-hook 'outline-mode-hook
           (lambda()
             (local-set-key (kbd "<f9>") outline-mode-prefix-map)
             (local-set-key "\C-c9" outline-mode-prefix-map)
             (my/add-outline-cycle-binding outline-mode-map)
             ))
 (add-hook 'outline-minor-mode-hook
           (lambda()
             (local-set-key (kbd "<f9>") outline-mode-prefix-map)
             (local-set-key "\C-c9" outline-mode-prefix-map)
             (my/add-outline-cycle-binding outline-minor-mode-map)
             ))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\e\eoa" #'org-agenda)
(global-set-key "\e\eol" #'org-store-link)
(global-set-key "\e\eoc" #'org-capture)
(global-set-key "\e\eos" #'org-sort-entries)
(use-package org
  :defer t
  :defines (
            org-replace-disputed-keys
            org-catch-invisible-edits
            org-use-property-inheritance
            org-use-tag-inheritance
            org-log-done
            org-enforce-todo-dependencies
            org-enforce-todo-checkbox-dependencies
            org-agenda-custom-commands
            org-src-fontify-natively
            org-src-preserve-indentation
            org-src-tab-acts-natively
            org-edit-src-content-indentation
            org-columns-default-format
            org-log-refile
            org-refile-targets
            org-archive-location
            )
  :init
  (setq org-agenda-files '("~/org"))
  (setq org-startup-folded nil)
  (setq org-replace-disputed-keys t)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-use-property-inheritance t)
  (setq org-use-tag-inheritance t)
  (setq org-return-follows-link t)
  ;; (setq org-use-speed-commands t)
  ;; (setq org-startup-indented t)
  ;; todos
  (setq org-log-done 'time)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-agenda-custom-commands
        '(("o" occur-tree "org")))
  ;; babel
  (setq org-src-fontify-natively t)
  ;; following 3 variables are for white-space sensitive languages
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  ;; ditaa
  (setq org-ditaa-jar-path (expand-file-name "~/bin/ditaa.jar"))
  ;; columns
  ;; (to see clocked time add: %10CLOCKSUM %15TIMESTAMP_IA)
  (setq org-columns-default-format "%40ITEM %TODO %PRIORITY %TAGS")
  :config
  (bind-key "C-c C-x t" 'org-table-recalculate-buffer-tables)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key nil)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WORKING(w)" "BLOCKED(b)"
                    "|" "WONTFIX(x)" "DONE(d)")))
  ;; tags
  (setq org-tag-alist '((:startgroup . nil)
                        ("@home" . ?h)
                        ("@work" . ?w)
                        (:endgroup . nil)
                        ("@mobile" . ?m)
                        ("@urgent" . ?u)
                        ))
  ;; capture
  (setq org-default-notes-file "~/Dropbox/notes/todo.org")
  ;; refiling
  (setq org-log-refile 'time)
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  ;; archiving
  (setq org-archive-location "~/org/archive.org::* From %s")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (plantuml . t)
     (python . t)
     (sh . t)
     (sql . t)
     (sqlite . t)
     ))
  (require 'ox-md)                      ;markdown export
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; easily go to top or bottom
;; from fuco1.github.io
;; (defmacro my/beginning-of-buffer (mode &rest forms)
;;   "Define a special form of `beginning-of-buffer' in MODE.
;; Moves point to (point-min); then FORMS are evaluated."
;;   (declare (indent 1))
;;   (let ((fname (intern (concat "my/" (symbol-name mode) "-beginning-of-buffer")))
;;         (mode-map (intern (concat (symbol-name mode) "-mode-map")))
;;         (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
;;     `(progn
;;        (defun ,fname ()
;;          (interactive)
;;          (let ((p (point)))
;;            (goto-char (point-min))
;;            ,@forms
;;            (when (= p (point))
;;              (goto-char (point-min)))))
;;        (add-hook ',mode-hook
;;                  (lambda()
;;                    (define-key ,mode-map
;;                      [remap beginning-of-buffer] ',fname))))))

;; (defmacro my/end-of-buffer (mode &rest forms)
;;   "Define a special form of `end-of-buffer' in MODE.
;; Moves point to (point-max); then FORMS are evaluated."
;;   (declare (indent 1))
;;   (let ((fname (intern (concat "my/" (symbol-name mode) "-end-of-buffer")))
;;         (mode-map (intern (concat (symbol-name mode) "-mode-map")))
;;         (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
;;     `(progn
;;        (defun ,fname ()
;;          (interactive)
;;          (let ((p (point)))
;;            (goto-char (point-max))
;;            ,@forms
;;            (when (= p (point))
;;              (goto-char (point-max)))))
;;        (add-hook ',mode-hook
;;                  (lambda()
;;                    (define-key ,mode-map
;;                      [remap end-of-buffer] ',fname))))))
;; (my/beginning-of-buffer dired
;;                         (while (not (ignore-errors (dired-get-filename)))
;;                           (dired-next-line 1)))
;; (my/end-of-buffer dired (dired-previous-line 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; beginend ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beginend
  :config
  (add-hook 'dired-mode-hook 'beginend-dired-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired
  :defer t
  :init
  ;; (add-hook 'dired-load-hook
  ;;           (lambda()
  ;;             (define-key dired-mode-map (kbd "<prior>") 'dired-up-directory)
  ;;             (define-key dired-mode-map "l" 'dired-launch-command)
  ;;             ))
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  :config
  (use-package dired-x)                 ; C-x C-j now runs 'dired-jump
  (setq diredp-hide-details-initially-flag nil)
  (use-package dired+
    :init
    ;; dired+'s default chord for chmod conflicts with arrow keys in terminal;
    ;; the alternative to this is to unbind the key like so:
    ;; (define-key dired-mode-map [(meta shift ?o)] nil)
    (setq diredp-bind-problematic-terminal-keys nil)
    :config
    ;; make a prefix
    (define-key dired-mode-map [(meta shift ?m)] nil)
    (define-prefix-command 'my/diredp-map)
    (global-set-key [(meta shift ?m)] 'my/diredp-map)
    (define-key my/diredp-map "m" 'diredp-chmod-this-file)
    (define-key my/diredp-map "o" 'diredp-chown-this-file)
    (define-key my/diredp-map "g" 'diredp-chgrp-this-file)
    (define-key my/diredp-map "t" 'dired-do-touch)
    (define-key my/diredp-map "T" 'diredp-touch-this-file)
    (define-key my/diredp-map "\M-t" 'diredp-do-touch-recursive)
    (define-key my/diredp-map "b" 'diredp-do-bookmark-in-bookmark-file)
    (define-key my/diredp-map "B" 'diredp-do-bookmark-in-bookmark-file-recursive)
    (define-key my/diredp-map "\M-b" 'diredp-do-bookmark-dirs-recursive)
    (define-key my/diredp-map "R" 'diredp-toggle-find-file-reuse-dir)
   )
  (define-key dired-mode-map "\C-o" 'dired-display-file) ;remap
  (define-key dired-mode-map "\M-p" nil)                 ;unbind
  (use-package ls-lisp+)
  ;; omit dot-files in dired-omit-mode (C-x M-o)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq ls-lisp-dirs-first t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  ;; next window's dired window used as target for current window operations
  (setq dired-dwim-target t)
  ;; search only in filenames
  (setq dired-isearch-filenames t)
  (use-package dired-filter)
  (define-key dired-mode-map "." dired-filter-mark-map)
  (setq wdired-allow-to-change-permissions t)
  (use-package ivy-dired-history)
  ;; sorting
  (use-package dired-sort)
  ;; du
  (use-package dired-du :init (setq dired-du-size-format t))
  (setq-default dired-listing-switches "-alhvGg")
  (put 'dired-find-alternate-file 'disabled nil)

  (defun my/dired-sort()
    "Toggle sorting in dired buffers."
    (interactive)
    (let ((type
           (completing-read "Sort by: "
                            '( "size" "extension" "ctime" "utime" "time" "name")
                            nil t)))
      ;; on os x, extension (X) not supported;
      ;; also, ctime means time file status was last changed
      (cond ((string= type "size") (dired-sort-size))
            ((string= type "extension") (dired-sort-extension))
            ((string= type "ctime") (dired-sort-ctime))
            ((string= type "utime") (dired-sort-utime))
            ((string= type "time") (dired-sort-time))
            ((string= type "name") (dired-sort-name))
            (t (error "Unknown dired sort %s" type)))))
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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sunrise-commander ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sunrise-commander
  :bind ("C-c 0s" . sunrise)
  :init
  (setq sr-use-commander-keys nil)
  :config
  (use-package sunrise-x-tree)
  (use-package sunrise-x-w32-addons)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; neotree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package neotree
  :bind ("C-c 0n" . neotree-toggle)
  :init
  (setq neo-show-hidden-files t)
  (setq neo-theme 'arrow)               ;or 'ascii
  (setq neo-window-width 30)
  (setq neo-vc-integration '(face char))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; deft ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package deft
  :bind ("C-c M-d" . deft)
  :commands deft-find-file
  :config
  (setq deft-extensions '("org" "md" "txt"))
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-org-mode-title-prefix t)
  (setq deft-file-naming-rules
        '((case-fn . capitalize)
          (noslash . "")
          (nospace . "")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; diff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defun my/diff-buffer-with-file ()
  "Diff the current buffer with its file."
  (interactive)
  (diff-buffer-with-file (current-buffer)))
(global-set-key "\M-sdd" #'my/diff-buffer-with-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ediff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only highlight current chunk
(setq-default ediff-highlight-all-diffs 'nil
              ediff-keep-variants nil
              ediff-forward-word-function 'forward-char
              ediff-auto-refine 'nix
              ediff-split-window-function 'split-window-horizontally
              )
(global-set-key "\M-sde" #'ediff-current-file)
(global-set-key "\M-sdb" #'ediff-buffers)
(global-set-key "\M-sdf" #'ediff-files)
(global-set-key "\M-sdr" #'ediff-revision)
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

;; add a merge both command
(defun ediff-copy-both-to-C ()
  "Add an ediff command to copy both variants."
  (interactive)
  (ediff-copy-diff
   ediff-current-difference nil 'C nil
   (concat
    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun my/add-merge-to-ediff-mode-map ()
  "Add a `merge A and B to C' command to ediff."
  (define-key ediff-mode-map "c" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'my/add-merge-to-ediff-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ediff-trees ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'my/ediff-trees-keymap)
(use-package ediff-trees
  :bind (("C-c e" . my/ediff-trees-keymap)
         :map my/ediff-trees-keymap
         ("e" . ediff-trees)
         ("n" . ediff-trees-examine-next)
         ("p" . ediff-trees-examine-previous)
         ("C-n" . ediff-trees-examine-next-regexp)
         ("C-p" . ediff-trees-examine-previous-regexp)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; diff-hl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package  diff-hl-dired
  :init (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote))
(use-package diff-hl
  :config
  (use-package diff-hl-flydiff :config (diff-hl-flydiff-mode 1))
  (global-diff-hl-mode 1)
  (unless (display-graphic-p)
    (use-package diff-hl-margin :config (diff-hl-margin-mode 1))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shebang ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package shebang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; point-undo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package point-undo
  :bind (([?\C-,] . point-undo)
         ("M-' ," . point-undo)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; move-text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package move-text
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; framemove ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package framemove
  :config
  (use-package windmove)
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t)
  ;;(setq windmove-wrap-around t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; buffer-move ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
 buffer-move
 :bind (([(control shift up)] . buf-move-up)
        ([(control shift down)] . buf-move-down)
        ([(control shift left)] . buf-move-left)
        ([(control shift right)] . buf-move-right)
        ([(control shift end)] . buf-move)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; frame-cmds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this also loads 'frame-fns
(use-package frame-cmds
  :bind (([(meta up)]      . move-frame-up)
         ([(meta down)]    . move-frame-down)
         ([(meta left)]    . move-frame-left)
         ([(meta right)]   . move-frame-right)
         ([(meta shift ?v)]. move-frame-to-screen-top)
         ([(control shift ?v)] . move-frame-to-screen-bottom)
         ([(control shift prior)] . move-frame-to-screen-left)
         ([(control shift next)] . move-frame-to-screen-right)
         ([(control shift home)] . move-frame-to-screen-top-left)
         ([(control meta down)] . enlarge-frame)
         ([(control meta right)] . enlarge-frame-horizontally)
         ([(control meta up)] . shrink-frame)
         ([(control meta left)] . shrink-frame-horizontally)
         ([(control ?x) (control ?z)] . iconify-everything)
         ;; ([(control ?z)] . iconify/show-frame)
         ;; ([mode-line mouse-3] . mouse-iconify/show-frame)
         ;; ([mode-line C-mouse-3] . mouse-remove-window)
         ([(control meta ?z)] . show-hide)
         ;; ([vertical-line C-down-mouse-1] . show-hide)
         ;; ([C-down-mouse-1] . mouse-show-hide-mark-unmark)
         ("C-x t." . save-frame-config)
         ;; :map ctl-x-map
         ;; ("o" . other-window-or-frame)
         ;; :map ctl-x-4-map
         ;; ("1" . delete-other-frames)
         ;; :map ctl-x-5-map
         ;; ("h" . show-*Help*-buffer)
         )
  :commands remove-window
  :config
  (substitute-key-definition 'delete-window      'delete-windows-for global-map)
  (substitute-key-definition 'delete-window      'remove-window global-map)
  ;; disabling in favor of rotate
  ;;   (defun my/tile-frames-vertically()
  ;;     "Tile frames vertically. You can restore prior frame position via going to
  ;; register \\C-l."
  ;;     (interactive)
  ;;     (save-frame-config)
  ;;     (tile-frames-vertically))
  ;;   (defun my/tile-frames-horizontally()
  ;;     "Tile frames horizontally. You can restore prior frame position via going to
  ;; register \\C-l."
  ;;     (interactive)
  ;;     (save-frame-config)
  ;;     (tile-frames-horizontally))
  ;;   (bind-key "\e\ev" 'my/tile-frames-vertically)
  ;;   (bind-key "\e\eh" 'my/tile-frames-horizontally)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; perspective ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package perspective
  :demand t
  :config (persp-mode))

;; (use-package persp-mode
;;   :demand t
;;   :init
;;   (setq persp-keymap-prefix (kbd "C-x x"))
;;   :config (persp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; workgroups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package workgroups
  :demand t
  :bind (:map wg-map
              ("<left>" . wg-switch-left)
              ("<right>" . wg-switch-right)
              )
  :config
  (setq wg-default-buffer "*Bookmark List*")
  (setq wg-use-faces nil)
  ;; doesn't work, isn't needed? (setq wg-restore-position t)
  ;; TODO: set initial string to "( -<{ }>- )"
  (setq wg-query-for-save-on-emacs-exit nil)
  (workgroups-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; workgroups2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'workgroups2)
;; (setq wg-prefix-key "\C-z")
;; ;; (define-key workgroups-mode-map (kbd "<left>") 'wg-switch-left)
;; ;; (define-key workgroups-mode-map (kbd "<right>") 'wg-switch-right)
;; (workgroups-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-theme-directory (concat my/scratch-directory "themes/"))
(use-package custom-themes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; palette ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package palette)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smerge ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smerge-mode
  :init
  (defun try-smerge()
    (save-excursion
      (goto-char (point-min))
      (smerge-mode
       (if (re-search-forward "^<<<<<<<" nil t) 1 0))))
  (add-hook 'find-file-hook 'try-smerge t)
  (add-hook 'after-save-hook (lambda() (if (smerge-mode) (try-smerge))))
  :config
  (define-key smerge-mode-map "\M-n" 'smerge-next)
  (define-key smerge-mode-map "\M-p" 'smerge-prev)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multi-term ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multi-term :commands multi-term)
                                        ;(setq multi-term-program "/bin/tcsh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bash-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bash-completion
  :defer t
  :config
  (bash-completion-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vlf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vlf-setup
  :defines (vlf-tune-enabled vlf-batch-size vlf-batch-size-remote)
  :init
  ;; for files over 50MB, only open 50MB at a time
  (setq large-file-warning-threshold 50000000) ;50MB
  (setq vlf-batch-size 50000000)               ;50MB
  (setq vlf-batch-size-remote 512000)          ;512k
  (setq vlf-tune-enabled nil)           ;don't adjust batch size dynamically
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rtags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rtags-exec (executable-find "rdm"))
(use-package rtags
  :disabled
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tab-always-indent 'complete)      ;or t to avoid completion
(add-to-list 'completion-styles 'initials t)
(setq completion-auto-help nil)
(setq completion-cycle-threshold t)     ;always cycle
;; Ignore case when completing file names
(setq read-file-name-completion-ignore-case nil)
(setq uniquify-recentf-func 'uniquify-recentf-ivy-recentf-open)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-complete
  :init
  (setq ac-quick-help-prefer-pos-tip nil)
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
                                        ;(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  :config
  ;; user dictionary
  (add-to-list 'ac-user-dictionary-files
               (concat my/scratch-directory "user-dict"))
  ;; mode/extension directory (in addition to "plugins/auto-complete/dict")
  (add-to-list 'ac-dictionary-directories
               (concat my/scratch-directory "dict/"))
  (mapc (lambda(mode)
          (setq ac-modes (cons mode ac-modes)))
        '(sql-mode nxml-mode cmake-mode folio-mode protobuf-mode
                   python-mode dos-mode gud-mode sh-mode text-mode
                   makefile-mode makefile-automake-mode makefile-gmake-mode
                   autoconf-mode gdb-script-mode awk-mode csv-mode
                   mock-mode org-mode html-mode text-mode sql-mode
                   sql-interactive-mode conf-mode markdown-mode
                   git-commit-mode mock-mode dart-mode
                   ))
  (use-package auto-complete-config)
  (setq-default ac-sources
                '(
                  ac-source-dictionary
                  ac-source-words-in-same-mode-buffers
                  ac-source-filename
                  ac-source-files-in-current-dir
                  ))
  (setq ac-expand-on-auto-complete nil)
  (setq ac-auto-start nil)
  (setq ac-dwim nil)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case 'smart)
  (setq ac-menu-height 20)
  (global-auto-complete-mode t)

  (defun my/auto-complete-at-point ()
    (when (and (not (minibufferp))
               (fboundp 'auto-complete-mode)
               auto-complete-mode)
      #'auto-complete))

  (defun my/add-ac-completion-at-point ()
    (add-to-list 'completion-at-point-functions 'my/auto-complete-at-point))
  (add-hook 'auto-complete-mode-hook 'my/add-ac-completion-at-point)

  (ac-flyspell-workaround)

  ;; rtags
  (when rtags-exec
    (require 'rtags-ac)
    (setq rtags-completions-enabled t)
    (rtags-enable-standard-keybindings c-mode-base-map)
    (defun my/rtags-complete() (interactive)
           (auto-complete '(ac-source-rtags)))
    (global-set-key (kbd "\C-c r TAB") 'my/rtags-complete)
    )
  ;; clang
  (defvar clang-exec (executable-find "clang"))
  (when clang-exec
    (with-eval-after-load 'cc-mode
      (use-package auto-complete-clang
        :config
        (define-key c-mode-base-map [?\M-/] 'ac-complete-clang)
        )
      ;; (add-to-list 'ac-omni-completion-sources
      ;;              (cons "\\." '(ac-source-clang)))
      ;; (add-to-list 'ac-omni-completion-sources
      ;;              (cons "->" '(ac-source-clang)))
      ))
  ;; c-headers
  (with-eval-after-load 'cc-mode
    (use-package
      auto-complete-c-headers
      :init
      (setq achead:include-patterns (list
                                     "\\.\\(h\\|hpp\\|hh\\|hxx\\|H\\)$"
                                     "/[a-zA-Z-_]+$"
                                     ))
      ;; doesn't work...
      ;; (setq achead:ac-prefix
      ;;       "#?\\(?:include\\|import\\)\\s-*[<\"]\\s-*\\([^\"<>' \t\r\n]+\\)")
      (setq achead:include-directories '("."))
      ))

  (use-package ac-etags
   :init
   (defface ac-etags-candidate-face
     '((t (:inherit ac-candidate-face)))
     "Face for etags candidates (overridden to default)")
   (defface ac-etags-selection-face
     '((t (:inherit ac-selection-face)))
     "Face for etags selections (overridden to default)")
   :config (ac-etags-setup)
   )

  (add-hook 'c-mode-common-hook
            (lambda()
              (when rtags-exec
                (setq ac-sources (cons ac-source-rtags ac-sources)))
              (setq ac-sources (append '(ac-source-etags
                                         ac-source-c-headers
                                         ) ac-sources))
              (setq c-tab-always-indent nil)
              (setq c-insert-tab-function 'indent-for-tab-command)
              ) t)                       ;append to hook list to take effect
                                        ;after ac-config-default
  (add-hook 'protobuf-mode-hook
            (lambda()
              (setq ac-sources (cons ac-source-etags ac-sources))))
  (defun my/expand-imenu() (interactive)
         (auto-complete '(ac-source-imenu)))
  (global-set-key "\C-c0j" 'my/expand-imenu)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; YASnippet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :init
  (add-to-list 'safe-local-variable-values '(require-final-newline . nil))
  (setq yas-snippet-dirs (list
                          (concat my/scratch-directory "snippets/")
                          ;; (concat my/plugins-directory "yasnippet/snippets/")
                          ))
  (setq yas-prompt-functions '(
                               yas-completing-prompt
                               yas-ido-prompt
                               yas-x-prompt
                               yas-dropdown-prompt
                               yas-no-prompt
                               ))
  (add-hook 'after-init-hook (lambda() (yas-global-mode 1)))
  :config
  (bind-keys
   :map yas-minor-mode-map
   ;; disable TAB key from activating a snippet
   ("<tab>" . nil)
   ("TAB" . nil)
   ;; add our own keybindings
   ("C-c se" . yas-expand)
   ("C-c si" . yas-insert-snippet)
   ("C-c sn" . yas-new-snippet)
   ("C-c sv" . yas-visit-snippet-file)
   ("C-c s?" . yas-describe-tables)
   )
  ;; integrate with auto-complete (shows only snippets)
  (defun my/expand-yasnippet() (interactive)
         (auto-complete '(ac-source-yasnippet)))
  (global-set-key [backtab] 'yas-expand)
  (global-set-key [(shift tab)] 'yas-expand)
  (setq yas-fallback-behavior '(apply my/expand-yasnippet))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; quick-peek ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package quick-peek)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :init
  (setq flycheck-emacs-lisp-package-user-dir
        (concat my/user-directory "elpa/"))
  (defun my/setup-flycheck ()
    (flycheck-checkbashisms-setup)
    (flycheck-bashate-setup))
  (add-hook 'flycheck-mode-hook #'my/setup-flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-global-modes
        '(emacs-lisp-mode python-mode dart-mode sh-mode c++-mode json-mode))
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-shellcheck-follow-sources nil)
  (use-package flycheck-package :config (flycheck-package-setup))
  (use-package flycheck-checkbashisms)
  (use-package flycheck-bashate)
  (use-package flycheck-popup-tip)
  (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode)
  ;; (use-package flycheck-pos-tip)
  ;; (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
  ;; (setq flycheck-pos-tip-display-errors-tty-function
  ;;       (lambda (errors)
  ;;         (let ((message (mapconcat #'flycheck-error-format-message-and-id
  ;;                                   errors "\n\n")))
  ;;           (popup-tip message))))
  ;; hack because flycheck unreasonably demands package installation
  (unless (fboundp 'pkg-info-version-info)
    (defun pkg-info-version-info (_) "unknown"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flyspell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/toggle-flyspell-prog-mode ()
  "Toggle flyspell-prog-mode in current buffer."
  (interactive)
  (if flyspell-mode
      (turn-off-flyspell)
    (flyspell-prog-mode)))
(bind-key "C-c C-4" #'my/toggle-flyspell-prog-mode)
(use-package flyspell
  :if (executable-find "hunspell")
  :defer t
  :init
  (setq ispell-program-name (executable-find "hunspell"))
  ;; to enable flyspell-prog-mode automatically:
  ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (mapc (lambda (hook) (add-hook hook #'flyspell-mode))
        '(text-mode-hook markdown-mode-hook org-mode-hook))
  (setq flyspell-issue-message-flag nil)
  :config
  (define-key flyspell-mode-map [?\C-,] nil)
  (define-key flyspell-mode-map [?\C-\;] nil)
  (define-key flyspell-mode-map [?\C-\.] nil)
  (define-key flyspell-mode-map [?\C-\M-i] nil)
  (bind-keys
   :map flyspell-mode-map
   ("C-c \\c" . flyspell-auto-correct-word)
   ("C-c \\a" . flyspell-auto-correct-previous-word)
   ("C-c \\n" . flyspell-goto-next-error)
   ("C-c \\s" . flyspell-correct-word-before-point)
   ("C-c \\w" . ispell-word)
   ("C-c \\b" . flyspell-buffer)
   ("C-c \\r" . flyspell-region)
   )
  (use-package ace-popup-menu :config (ace-popup-menu-mode 1))
  (use-package flyspell-popup
    :demand t
    :bind (:map flyspell-mode-map
                ("C-c \\\\" . flyspell-popup-correct))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; semantic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq semantic-default-submodes
      (append '(global-semantic-stickyfunc-mode
                global-semantic-decoration-mode
                ) semantic-default-submodes))
(add-hook 'semantic-init-hooks
          (lambda()
            (when (cedet-ectag-version-check t)
              (semantic-load-enable-primary-exuberant-ctags-support))
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; headers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; last modification time
(defun my/update-last-modifier ()
  "Update header line indicating identity of last modifier."
  (delete-and-forget-line)
  (insert (format " %s" (let ((name (user-full-name)))
                          (if (and name (not (string= name "")))
                              name
                            (user-login-name))))))
;; file name
(defun my/update-file-name ()
  "Update the line that indicates the file name."
  (beginning-of-line)
  ;; Verify looking at a file name for this mode.
  (when (looking-at (concat (regexp-quote (header-prefix-string)) " *\\([^ ]+\\) +\\-\\-"))
    (goto-char (match-beginning 1))
    (delete-region (match-beginning 1) (match-end 1))
    (insert (file-name-nondirectory (buffer-file-name)))))
(use-package header2
  :commands auto-update-file-header
  :init
  (add-hook 'write-file-functions 'auto-update-file-header)
  :config
  ;; use my own function, because delete-trailing-whitespace prevents a
  ;; space after the colon
  (register-file-header-action "Modified by[ \t]*:" 'my/update-last-modifier)
  (register-file-header-action "^[ \t]*.+ *\\([^ ]+\\) +\\-\\-" 'my/update-file-name)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; request-deferred ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package request-deferred :commands request-deferred)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; web ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web
  :commands (web-http-call web-http-get web-http-post
                           web-json-post web-get))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; restclient ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package restclient
  :mode ("\\.http$" . restclient-mode)
  :commands restclient-mode
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-insert-choose+
  :bind ("C-c st" . auto-insert)
  :demand t
  :defines (auto-insert auto-insert-directory auto-insert-alist)
  :init
  (setq auto-insert 'other)
  (setq auto-insert-directory (concat my/scratch-directory "auto-insert/"))
  ;; list of different templates to choose from
  ;; c++
  (defvar auto-insert-c-header-alist '())
  (defvar auto-insert-c-impl-alist '())
  ;; autoconf
  (defvar auto-insert-autoconf-alist '())
  ;; Makefile.am
  (defvar auto-insert-makefile-am-alist '())
  :config
  (auto-insert-choose+-add-entry 'auto-insert-c-header-alist "template.h")
  (auto-insert-choose+-add-entry 'auto-insert-c-impl-alist "template.cpp")
  (auto-insert-choose+-add-entry 'auto-insert-autoconf-alist
                                 "configure-standard.ac")
  (auto-insert-choose+-add-entry 'auto-insert-autoconf-alist
                                 "configure-library.ac")
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
          ;; projects
          (("\\.proviso$" . "Projects") .
           ["template.proviso" auto-insert-choose-yas-expand])
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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; popup-kill-ring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package popup-kill-ring :bind ("C-M-y" . popup-kill-ring))

;;;;;;;;;;;;;;;;;;;;;;;;;;; popup-global-mark-ring ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package popup-global-mark-ring :bind ("\e\ey" . popup-global-mark-ring))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; zop-to-char ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zop-to-char
  :bind (("M-z" . zop-to-char)
         ("\e\ez" . zop-up-to-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; htmlize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package htmlize
  :commands
  (htmlize-buffer htmlize-region htmlize-file htmlize-many-files
                  htmlize-many-files-dired))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; awk-it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package awk-it
  :bind (("C-c 0aa" . awk-it)
         ("C-c 0ap" . awk-it-with-separator)
         ("C-c 0as" . awk-it-single)
         ("C-c 0ag" . awk-it-single-with-separator)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; list-environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package list-environment :bind ("C-c 0e" . list-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sort-words ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sort-words :commands sort-words)

;;;;;;;;;;;;;;;;;;;;;;;;;;; highlight-indentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package highlight-indentation
  :bind ("C-c 0h" . highlight-indentation-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rotate-text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rotate-text
  :bind (("M-r" . rotate-text)
         ("M-R" . rotate-text-backward)
         ))


(add-hook 'before-save-hook 'my/before-save-hook)
(defun my/before-save-hook() "Presave hook"
       (when (memq major-mode
                   '(
                     awk-mode
                     bat-mode
                     c++-mode
                     csharp-mode
                     dart-mode
                     dos-mode
                     emacs-lisp-mode
                     folio-mode
                     gdb-script-mode
                     gitattributes-mode
                     gitconfig-mode
                     gitignore-mode
                     java-mode
                     json-mode
                     nxml-mode
                     perl-mode
                     protobuf-mode
                     python-mode
                     sed-mode
                     sh-mode
                     ))
         (delete-trailing-whitespace)
         (save-excursion
           (let ((inhibit-message t))
             (copyright-update nil t)
             (copyright-fix-years)))
         (time-stamp)
         ))

(defun my/get-ideal-frame-height (&optional pixels)
  "Return an ideal height for frames, given a toolbar height of PIXELS.
This function's result only has value if it is preceded by any font changes."
  (/ (- (display-pixel-height) (or pixels 90))
     (frame-char-height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; remotehost-connect ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package remotehost-connect
  :demand t
  :bind (("C-c 6" . remotehost-connect)
         ([f6] . remotehost-connect)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; os ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((system-file (concat my/os-dir my/system-name)))
  ;; load os file
  (load system-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((gui window-system)
      gui-file)
  ;; load gui file
  (setq gui-file (concat my/gui-dir (if (null gui) "tty" (symbol-name gui))))
  (load gui-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; site ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/load-site-file (name)
  "Load a site file associated with site NAME.
This may perform related customization."
  (let* ((site-dir
          (file-name-as-directory
           (concat my/user-directory "settings/site/" name)))
         (site-file (concat site-dir name)))
    (when (file-exists-p site-file)
      ;; (setq site-name (file-name-base site-file))
      (load site-file))
    (setq yas-snippet-dirs (cons (concat site-dir "snippets/")
                                 yas-snippet-dirs))
    (when (fboundp 'yas-reload-all) (yas-reload-all))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; host ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/host-plist '())
(defun my/unqualify-host-name (hst)
  "Remove the fully qualified suffix, if any, from a hostname HST."
  (when (string-match "^\\([^.]+\\)\\.?.*$" hst)
    (match-string-no-properties 1 hst)))

(let* ((system (my/unqualify-host-name (system-name)))
       (hosts-dir (concat my/user-directory "settings/host/"))
       (all-hosts-dir (concat hosts-dir "hosts/"))
       (host-dir (file-name-as-directory (concat hosts-dir system)))
       (host-file (concat host-dir system)))
  ;; the list of remote hosts contained in the plist `remotehost-connect-hosts'
  ;; and sourced from a list of hosts in the file `hosts/site'
  ;; serves 3 purposes:
  ;; 1) outside of emacs, is used to release harmsway onto remote hosts
  ;; 2) inside emacs, allows assigning hosts to a site without an
  ;;    explicit host file
  ;; 3) provides a list of remote hosts to connect to, via `remotehost-connect'
  ;;
  ;; First we populate the list of remote hosts
  (when (file-exists-p all-hosts-dir)
    (mapc (lambda (file)
            (let ((site (f-base file)))
              (setq remotehost-connect-hosts
                    (append
                     ;; insert the site into each entry
                     (mapcar (lambda (lst)
                               (plist-put lst :site site))
                             (remotehost-connect-read-file file))
                     remotehost-connect-hosts))))
          (f-files all-hosts-dir)))
  ;; then we load the official host file, if it exists
  (if (file-exists-p host-file)
      (load host-file t)
    ;; otherwise look for current host in hosts file
    (dolist (plist remotehost-connect-hosts)
      (and (string= (plist-get plist :host) system)
           (plist-get plist :site)
           (my/load-site-file (plist-get plist :site)))))
  ;; finally also look for environment variable definitions
  (my/load-environment-variables-from-file host-dir)
  )

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
                ("\\.otq$"      . conf-mode) ;one-tick-query files
                ("\\.bmk$"      . emacs-lisp-mode)
                )
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; awk-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'awk-mode-hook
          (lambda()
            (setq comment-start "#") (setq comment-end "")
            (define-key awk-mode-map "\C-c\C-c" 'comment-region)
            (define-key awk-mode-map "\C-c\C-u" 'uncomment-region)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bat-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bat-mode :mode ("\\.bat$" "\\.cmd$")
  :config
  (use-package dos-indent)
  (add-hook 'bat-mode-hook
            (lambda()
              (setq-default indent-tabs-mode nil)
              (dos-indent)
              (define-key bat-mode-map "\C-c\C-c" 'comment-region)
              (define-key bat-mode-map "\C-c\C-u" 'uncomment-region)
              ;; the following conflicted with C-c C-c
              (define-key bat-mode-map "\C-c\C-r" 'bat-run)
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cask-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cask-mode
  :mode "/Cask\\'"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; conf-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'conf-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            (subword-mode 1)
            ;; (idle-highlight-mode 1)
            ;; conf-colon-mode still bound to "\C-c:"
            (local-unset-key "\C-c\C-c")
            ;; conf-unix-mode now bound to "\C-cu"
            (local-unset-key "\C-c\C-u")
            (define-key conf-mode-map "\C-cu" 'conf-unix-mode)
            (define-key conf-mode-map "\C-c\C-c" 'comment-region)
            (define-key conf-mode-map "\C-c\C-u" 'uncomment-region)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cmake-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
  cmake-mode
  :mode ("CMakeLists\\.txt$" "\\.cmake$")
  :config
  (use-package cmake-font-lock)
  (defun my/cmake-fix-underscore()
    (modify-syntax-entry ?_ "_" cmake-mode-syntax-table))
  (add-hook 'cmake-mode-hook #'my/cmake-fix-underscore)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; crontab-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package crontab-mode
  :mode ("\\.\\(ana\\)?cron\\(tab\\)?$" "\\(ana\\)?cron\\(tab\\)?\\.")
  :commands crontab-get
  :config
  (add-hook 'crontab-mode-hook
            (lambda ()
              (define-key crontab-mode-map "\C-c\C-c" 'comment-region)
              (define-key crontab-mode-map "\C-c\C-u" 'uncomment-region)
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; css-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'css-mode-hook
          (lambda()
            (if (featurep 'rainbow-mode)
                (rainbow-turn-on)
              (my/syntax-color-hex-values))
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; csv-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package csv-mode :mode ("\\.[Cc][Ss][Vv]$" . csv-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dart-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dart-mode :mode "\\.dart$" :interpreter "dart"
  :defines dart-enable-analysis-server
  :init
  (setq dart-enable-analysis-server t)
  :config
  (add-hook 'dart-mode-hook
            (lambda()
              (define-key dart-mode-map "\C-c\C-c" 'comment-region)
              (define-key dart-mode-map "\C-c\C-u" 'uncomment-region)
              )))
;not sure this is needed (add-hook 'dart-mode-hook 'flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elf-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elf-mode
  :config
  (elf-setup-default)                   ;adds entry to magic-mode-alist
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs-lisp-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/compile-lisp-file ()
  "Byte-compile a Lisp code file."
  (require 'bytecomp)
  (when (and
         (eq major-mode 'emacs-lisp-mode)
         (file-exists-p (byte-compile-dest-file (buffer-file-name)))
         (not (string-match
               "^\\.dir-locals.el$"
               (file-name-nondirectory
                (buffer-file-name)))))
    (save-excursion
      (byte-compile-file buffer-file-name))))

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
(add-hook 'after-save-hook #'my/compile-lisp-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; folio-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package folio-mode :mode "\\.folio$"
  :config (use-package folio-electric))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git-modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package gitignore-mode
  :mode ("/\\.gitignore\\'"
         "/info/exclude\\'"
         "/git/ignore\\'"))
(use-package gitconfig-mode
  :mode ("/\\.gitconfig\\'"      "/\\.git/config\\'"
         "/modules/.*/config\\'" "/git/config\\'"
         "/\\.gitmodules\\'"     "/etc/gitconfig\\'"))
(use-package gitattributes-mode
  :mode ("/\\.gitattributes\\'"
         "/info/attributes\\'"
         "/git/attributes\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; html-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'html-mode-hook
          (lambda()
            (if (featurep 'rainbow-mode)
                (rainbow-turn-on)
              (my/syntax-color-hex-values))
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; json-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :mode "\\.json$"
  :if (version< "23" emacs-version)
  :init
  (add-hook 'json-mode-hook
            (lambda()
              (subword-mode 1)
              (add-to-list 'flycheck-disabled-checkers 'json-jsonlint)
              ))
  :config
  (use-package
    json-navigator
    :demand t
    :config
    (define-key json-mode-map "\C-c\C-f" 'json-navigator-navigate-after-point)
    (define-key json-mode-map "\C-c\C-n" 'json-navigator-navigate-region))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; log-viewer-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package log-viewer :mode ("\\.log$" . log-viewer-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :mode (("README\\.md$" . gfm-mode)
         ("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)
         ("LICENSE$" . markdown-mode)
         ("README$" . markdown-mode)
         ("INSTALL$" . markdown-mode)
         ("CONTRIBUTORS$" . markdown-mode)
         ("COPYING$" . markdown-mode)
         )
  :commands (markdown-mode gfm-mode)
  :init
  (setq markdown-command "Markdown.pl")
  (add-hook 'markdown-mode-hook #'good-word/init-word-processor)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nhexl-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nhexl-mode :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pandoc-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
 pandoc-mode
 :defer t
 :commands pandoc-mode
 :init
 (setq pandoc-data-dir (concat my/scratch-directory "pandoc/"))
 (mapc (lambda (hook)
         (add-hook hook 'pandoc-mode))
       '(markdown-mode-hook))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pcap-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package pcap-mode
  :mode ("\\.pcap$" . pcap-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; plantuml ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package plantuml-mode
  :mode ("\\.plantuml$" . plantuml-mode)
  :init
  (setq plantuml-jar-path (expand-file-name "~/bin/plantuml.jar"))
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python
  :if (executable-find "python")
  :defer t
  :config
  (use-package virtualenvwrapper)
  (use-package sphinx-doc)
  (use-package python-switch-quotes)
  ;;(setq venv-location "?")
  ;; add jedi if installed
  (when (eq 0 (call-process "python" nil nil nil "-c" "import jedi"))
                                        ;      (setq jedi:setup-keys t)
    (setq jedi:complete-on-dot t)
    (setq jedi:tooltip-method '(popup))
    (use-package jedi)
    (use-package direx)
    (use-package jedi-direx)
    (add-hook 'jedi-mode-hook 'jedi-direx:setup)
    (add-hook 'python-mode-hook 'jedi:setup)
    (defun my/expand-jedi() (interactive)
           (auto-complete '(ac-source-jedi-direct)))
    )
  (if (executable-find "flake8")
      (progn
        (flycheck-define-checker python-flake8
          "A Python syntax and style checker using Flake8.

Requires Flake8 2.0 or newer. See URL
`https://flake8.readthedocs.io/'."
          :command ("flake8"
                    "--format=default"
                    (config-file "--config" flycheck-flake8rc)
                    (option "--max-complexity" flycheck-flake8-maximum-complexity nil
                            flycheck-option-int)
                    (option "--max-line-length" flycheck-flake8-maximum-line-length nil
                            flycheck-option-int)
                    "-")
          :standard-input t
          :error-filter (lambda (errors)
                          (let ((errors (flycheck-sanitize-errors errors)))
                            (seq-do #'flycheck-flake8-fix-error-level errors)
                            errors))
          :error-patterns
          ((warning line-start
                    "stdin:" line ":" (optional column ":") " "
                    (id (one-or-more (any alpha)) (one-or-more digit)) " "
                    (message (one-or-more not-newline))
                    line-end))
          :modes python-mode))
    (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    (add-to-list 'flycheck-checkers 'python-pyflakes)
    (use-package flycheck-pyflakes)
    )
  (add-hook 'python-mode-hook
            (lambda()
              (subword-mode 1)
              (setq-default indent-tabs-mode nil)
              (setq python-indent-guess-indent-offset nil)
              (setq python-indent-offset 4)
              (setq-local electric-indent-chars
                          (remq ?: electric-indent-chars))
              (setq forward-sexp-function nil)
              (local-unset-key [backtab]) ;save backtab for yasnippet
                                        ; S-TAB ran dedent-line in python,
                                        ; we can just use TAB instead
              (define-key python-mode-map "\C-j" 'newline-and-indent)
              (define-key python-mode-map "\C-c\C-c" 'comment-region)
              (define-key python-mode-map "\C-c\C-u" 'uncomment-region)
              (define-key python-mode-map [?\C-\M-g] 'python-nav-forward-sexp)
              (define-key python-mode-map (kbd "\C-c RET")
                (lambda()(interactive)
                  (compile (concat "python " (buffer-file-name)))))
              (when (featurep 'jedi)
                (define-key python-mode-map [(ctrl tab)] 'my/expand-jedi)
                (define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
              (sphinx-doc-mode 1)
              (define-key python-mode-map "\C-c'" 'python-switch-quotes)
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; qt-pro-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package qt-pro-mode :mode ("\\.pr[oi]$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sed-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sed-mode :mode "\\.sed$" :interpreter "sed")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sh-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sh-script
  :defer t
  :init
  (add-hook 'sh-mode-hook
            (lambda()
              (setq indent-tabs-mode nil)
              (add-to-list 'flycheck-disabled-checkers 'sh-posix-dash)
              ))
  :config
  (setq sh-basic-offset 4)
  (setq sh-indentation 4)
  (define-key sh-mode-map "\r" 'reindent-then-newline-and-indent)
  (define-key sh-mode-map "\C-c\C-c" 'comment-region)
  (define-key sh-mode-map "\C-c\C-u" 'uncomment-region)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; strace-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package strace-mode :mode "\\.strace$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; text-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook #'good-word/init-word-processor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'nxml-mode-hook
          (lambda()
            (setq-default indent-tabs-mode nil)
            ;; (idle-highlight-mode 1)
            (define-key nxml-mode-map "\r" 'reindent-then-newline-and-indent)
            (define-key nxml-mode-map "\C-c\C-c" 'comment-region)
            (define-key nxml-mode-map "\C-c\C-u" 'uncomment-region)
            (use-package auto-complete-nxml)
            ))
(use-package mz-comment-fix
  :if (< emacs-major-version 25)
  :defines comment-strip-start-length
  :config
  (add-to-list 'comment-strip-start-length (cons 'nxml-mode 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yaml-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :mode ("\\.yaml$" "\\.yml$")
  :config
  (defun yaml-next-field() "Jump to next yaml field."
         (interactive)
         (search-forward-regexp ": *"))
  (defun yaml-prev-field() "Jump to previous yaml field."
         (interactive)
         (search-backward-regexp ": *"))
  (add-hook 'yaml-mode-hook
            (lambda()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent)
              (define-key yaml-mode-map "\M-\r" 'insert-ts)
              (define-key yaml-mode-map (kbd "C-<tab>") 'yaml-next-field)
              (define-key yaml-mode-map (kbd "C-S-<tab>") 'yaml-prev-field)
              )))

;; code ends here
