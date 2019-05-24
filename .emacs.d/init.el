;; init.el --- Initialization file
;; Copyright (C) 2015-2019  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Friday, February 27, 2015
;; Modified Time-stamp: <2019-05-24 08:46:14 dharms>
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
(require 'subr-x)

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
                            ,(concat my/user-directory "modes/haskell/")
                            ,(concat my/user-directory "custom/"))
                          load-path))
  (setq load-path (append
                   `(
                     ,(concat my/user-directory "ext/gridlock/")
                     ,(concat my/user-directory "ext/outrespace/")
                     ,(concat my/user-directory "ext/parsenv/")
                     ,(concat my/user-directory "ext/proviso/")
                     ,(concat my/user-directory "ext/xfer/")
                     ,(concat my/plugins-directory "auto-complete/")
                     ,(concat my/plugins-directory "bookmark+/")
                     ,(concat my/plugins-directory "company/")
                     ,(concat my/plugins-directory "ccls/")
                     ,(concat my/plugins-directory "cquery/")
                     ,(concat my/plugins-directory "diff-hl/")
                     ,(concat my/plugins-directory "elnode/")
                     ,(concat my/plugins-directory "emacs-refactor/")
                     ,(concat my/plugins-directory "expand-region/")
                     ,(concat my/plugins-directory "http/")
                     ,(concat my/plugins-directory "hydra/")
                     ,(concat my/plugins-directory "js2/")
                     ,(concat my/plugins-directory "lsp/")
                     ,(concat my/plugins-directory "magit/lisp/")
                     ,(concat my/plugins-directory "multi-line/")
                     ,(concat my/plugins-directory "multiple-cursors/")
                     ,(concat my/plugins-directory "powerline/")
                     ,(concat my/plugins-directory "realgud/")
                     ,(concat my/plugins-directory "smart-jump/")
                     ,(concat my/plugins-directory "smart-mode-line/")
                     ,(concat my/plugins-directory "sunrise/")
                     ,(concat my/plugins-directory "swiper/")
                     ,(concat my/plugins-directory "treemacs/")
                     ,(concat my/plugins-directory "use-package/")
                     ,(concat my/plugins-directory "vc-msg/")
                     ,(concat my/plugins-directory "vlf/")
                     ,(concat my/plugins-directory "yasnippet/")
                     ,(concat my/elisp-directory "emacs-jedi/")
                     ,(concat my/scratch-directory "modules/"
                              (string-trim (shell-command-to-string
                                            "uname")))
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
(setq switch-to-visible-buffer nil)
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
(electric-pair-mode 1)
;; show current function
(which-function-mode 1)
;; winner mode
(winner-mode 1)
;; don't try to create "other files"
(defvar ff-always-try-to-create)        ;silence warning
(setq ff-always-try-to-create nil)
;; Preserve line position on scroll
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)
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
(setq-default fill-column 78)
(mouse-avoidance-mode 'exile)
(setq mouse-yank-at-point t)
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
Cf.  `http://ergoemacs.org/emacs/emacs_CSS_colors.html'."
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
(global-set-key (kbd "ESC M-SPC") 'move-to-window-line-top-bottom)
(global-set-key [(next)] 'scroll-up-line)
(global-set-key [(prior)] 'scroll-down-line)
(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key "\e\e5" 'toggle-truncate-lines)
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

(when (< emacs-major-version 27)
  (push (concat my/elisp-directory "compat/27/0/-/") load-path))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun harmsway-unqualify-host-name (hst)
  "Remove the fully qualified suffix, if any, from a hostname HST."
  (when (string-match "^\\([^.]+\\)\\.?.*$" hst)
    (match-string-no-properties 1 hst)))

(defun harmsway-init-environment ()
  "Load environment variables from various files.
This includes files specific to the current operating system, the
current host, possibly a site file, and personal settings.  It is
not an error if any files do not exist."
  (let ((os (string-trim (shell-command-to-string "uname")))
        (host (harmsway-unqualify-host-name (system-name)))
        (site (getenv "SITE"))
        file)
    (setenv "GPG_AGENT_INFO" nil)
    ;; user
    (parsenv-load-env (expand-file-name (concat "~/." my/user-name ".env")))
    ;; os
    (parsenv-load-env (expand-file-name (concat "~/." os ".env")))
    ;; host
    (parsenv-load-env (expand-file-name (concat "~/." host ".env")))
    ;; site
    (parsenv-load-env (expand-file-name (concat "~/." site ".env")))
    ;; personal
    (parsenv-load-env (expand-file-name "~/.personal.env"))
    (parsenv-adjust-exec-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; parsenv ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package parsenv
  :demand t
  :config
  (harmsway-init-environment))

(load-library "compiling")
(load-library "coding")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; proviso ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package proviso
  :demand t
  :bind (("C-c gg" . proviso-grep)
         ("C-c gag" . proviso-ag)
         ("C-c gr" . proviso-rg)
         ("C-c pt" . proviso-gentags-generate-tags)
         ("C-c ff" . proviso-finder-find-file)
         ("C-c 4ff" . proviso-finder-find-file-other-window)
         ("C-c fF" . proviso-finder-find-file-all)
         ("C-c 4fF" . proviso-finder-find-file-all-other-window)
         ("C-c fd" . proviso-finder-open-dir)
         ("C-c 4fd" . proviso-finder-open-dir-other-window)
         ("C-c fD" . proviso-finder-open-dir-all)
         ("C-c 4fD" . proviso-finder-open-dir-all-other-window)
         ("C-c fr" . proviso-finder-recompute-cache)
         ("C-c dg" . proviso-gud-open-gdb)
         ("C-c dr" . proviso-gud-open-realgud)
         ("C-c pd" . proviso-open-dired-this-project)
         ("C-c pD" . proviso-open-dired-all-projects)
         ("C-c pp" . proviso-display-print-project)
         ("C-c pn" . proviso-display-echo-project-names)
         ("C-c pe" . proviso-display-projects)
         ("C-c p <SPC>" . proviso-dashboard-show)
         ("C-c pg" . proviso-refresh-current-project)
         ("C-c C-f" . proviso-fulledit)
         ("C-c pb" . proviso-bookmarks-switch-to-bookmark)
         ("C-c pcc" . proviso-compile-choose-compile-command)
         ("C-c pf" . proviso-clang-format-buffer-or-region)
         ("C-c pcf" . proviso-clang-format-toggle-active)
         ("C-c p." . proviso-xref-peek-definition)
         :map proviso-deploy-keymode-map
         ("o" . proviso-deploy-open-file)
         ("s" . proviso-deploy-save-file)
         ("S" . proviso-deploy-save-file-as)
         ("g" . proviso-deploy-revert-file)
         ("+" . proviso-deploy-add-deploy)
         ("=" . proviso-deploy-add-deploy-cmd)
         ("-" . proviso-deploy-add-deploy-env)
         ("r" . proviso-deploy-run-deploy)
         ("R" . proviso-deploy-run-all-deploys)
         ("." . proviso-deploy-run-last)
         ("x" . proviso-deploy-delete-deploy)
         ("X" . proviso-deploy-delete-all-deploy)
         ("c" . proviso-deploy-check-file)
         ("d" . proviso-deploy-diff-file)
         ("e" . proviso-deploy-ediff-file)
         ("f" . proviso-deploy-find-file)
         ("F" . proviso-deploy-find-file-other-window)
         ("t" . proviso-deploy-edit-deploy)
         ("<SPC>" . proviso-deploy-show)
         )
  :init
  (define-prefix-command 'proviso-deploy-keymode-map)
  (global-set-key "\C-cpl" 'proviso-deploy-keymode-map)
  :config
  (use-package proviso-frame-title)
  (setq project-find-functions (list #'proviso-find-project))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (> emacs-major-version 24)
  (defun harmsway-xref-find-definition ()
    "Jump straight to a definition."
    (interactive)
    (let ((xref-prompt-for-identifier
           '(not xref-find-definitions xref-find-definitions-other-window
                 xref-find-definitions-other-frame))
          (this-command 'xref-find-definitions))
      (call-interactively 'xref-find-definitions)))

  (setq xref-prompt-for-identifier
        '(xref-find-definitions xref-find-definitions-other-window
                                xref-find-definitions-other-frame
                                xref-find-references
                                xref-find-apropos))
  (global-set-key "\C-x." #'xref-find-definitions)
  (global-set-key "\M-*" #'xref-find-apropos)
  (global-set-key [?\C-\M-.] #'harmsway-xref-find-definition)
  (global-set-key "\C-x\M-." #'harmsway-xref-find-definition)
  ;; (global-set-key "\C-x4\M-." #'harmsway-xref-find-definition)
  ;; (global-set-key "\C-x5\M-." #'harmsway-xref-find-definition)
  )

;; select
(use-package proviso-etags-select
  :if (< emacs-major-version 25)
  :init
  (setq tags-revert-without-query t)
  :bind ("\e\e." . etags-select-find-tag)
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

(use-package install-world
  :bind ("C-c 0qi" . harmsway/install-world))

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
         ;; ("\e\e\\" . jump-to-matching-paren)
         ;; ("M-]" . highlight-paren-right)
         ;; ("M-[" . highlight-paren-left)
         ;; ("M-s p" . highlight-enclosing-paren)
         ;; ("\e\er" . highlight-current-sexp)
         ("C-c q" . clean-up-buffer)
         ;; ("\e\e(" . enclose-by-braces-paren)
         ;; ("\e\e[" . enclose-by-braces-bracket)
         ;; ("\e\e{" . enclose-by-braces-brace)
         ;; ("\e\e<" . enclose-by-braces-caret)
         ))

(use-package custom-buffer-utils
  :bind (("C-x C-r" . harmsway-revert-buffer)
         ("C-x K" . kill-other-buffers)
         ("\e\ep" . switch-to-most-recent-buffer)
         ("C-x 4z" . window-toggle-split-direction)
         ("C-x 4s" . swap-buffers)
         ("C-c 0w" . my/toggle-window-dedicated)
         ("C-x 5x" . move-buffer-to-new-frame)
         )
  :init
  (push `(?r ,(lambda (buf)
                (with-current-buffer buf
                  (harmsway-revert-buffer)))
             "revert this buffer")
        save-some-buffers-action-alist)
  :commands
  (move-buffer-file move-buffer-to-new-frame harmsway-revert-buffer)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; delimit-columns ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package delim-cols+)
(global-set-key "\C-cac" 'delimit-columns-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ialign ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ialign :bind ("C-c ai" . ialign))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs-new-buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs-new-buffer
  :bind (("C-x tb" . emacs-new-buffer-now)
         ("C-x tw" . emacs-new-buffer-as)))

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
  :init
  (setq outrespace-prefix-key "\C-cn")
  (with-eval-after-load 'cc-mode (load-library "outrespace"))
  (add-hook 'c++-mode-hook (lambda() (outrespace-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gridlock ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package gridlock-csv
  :after csv-mode
  :bind (:map csv-mode-map ("C-c C-l" . gridlock-csv-mode)))
(use-package gridlock-fix
  :bind ("C-c M-f" . gridlock-fix-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; epa ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/add-epa-file-encrypt-to ()
  "Add a file local variable for `epa-file-encrypt-to'."
  (interactive)
  (add-file-local-variable-prop-line 'epa-file-encrypt-to
                                     (concat "(" user-mail-address ")")))
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

(use-package fill-function-arguments
  :init
  (add-hook 'prog-mode-hook
            (lambda()
              (local-set-key "\C-c`" #'fill-function-arguments-dwim))))

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
              ("M-h" . rotate:main-horizontal)
              ("v" . rotate:even-vertical)
              ("M-v" . rotate:main-vertical)
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
              ))
  :config
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
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ibuffer-vc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ibuffer-vc
  :after ibuffer
  :config
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ibuffer-project ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ibuffer-project
  :disabled
  :after ibuffer
  :config
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yascroll ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yascroll :config (global-yascroll-bar-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; iflipb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package iflipb
  :disabled
  :bind (("<C-tab>" . iflipb-next-buffer)
         ("M-' TAB" . iflipb-next-buffer)
         ("<C-S-tab>" . iflipb-previous-buffer)
         ("<C-S-iso-lefttab>" . iflipb-previous-buffer)
         ("M-' <backtab>" . iflipb-previous-buffer)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nswbuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nswbuff
  :bind (("<C-tab>" . nswbuff-switch-to-next-buffer)
         ("<C-S-tab>" . nswbuff-switch-to-previous-buffer)
         ("<C-S-iso-lefttab>" . nswbuff-switch-to-previous-buffer)
         )
  :init
  (setq nswbuff-recent-buffers-first t)
  (setq nswbuff-delay-switch nil)
  (setq nswbuff-display-intermediate-buffers nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mission-control ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mission-control :bind ("s-\\" . mcon-switch))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; banner-comment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package banner-comment
  :commands (banner-comment)
  :bind ([?\C-c?\C-\M-/] . banner-comment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; poporg ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package poporg
  :bind ("C-c 0/" . #'poporg-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fence-edit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fence-edit
  :commands (fence-edit-code-at-point fence-edit-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; list-register ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package list-register :bind ("C-x rv" . list-register))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mwim ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mwim
  :bind ("M-m" . mwim-beginning-of-code-or-line-or-comment)
  :config
  (setq mwim-next-position-function 'mwim-next-unique-position)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; which-key ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :demand 1
  :init
  (setq which-key-idle-delay 2.0)
  (setq which-key-idle-secondary-delay 1.0)
  :config
  (which-key-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; home-end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package home-end
  :bind (([home] . home-end-home)
         ([end] . home-end-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; discover-my-major ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package discover-my-major :bind ("C-h C-m" . discover-my-major))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; annotate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package annotate
  :bind (:map annotate-mode-map
              ("C-c C-e" . annotate-export-annotations)
              ("C-c M-e" . annotate-integrate-annotations))
  :config (annotate-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; annot ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package annot
  :disabled
  )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; corral ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package corral
  :bind (("\e\e9" . corral-parentheses-backward)
         ("\e\e0" . corral-parentheses-forward)
         ("M-[" . corral-brackets-backward)
         ("M-]" . corral-brackets-forward)
         ("\e\e[" . corral-braces-backward)
         ("\e\e]" . corral-braces-forward)
         ("\e\e\"" . corral-double-quotes-backward)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; iedit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package iedit
  :bind (("C-;" . iedit-mode)
         ("M-' ;" . iedit-mode)
         )
  :init
  (setq iedit-auto-narrow t)
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

         ("C-\\ C-\\ C-0" . mc/insert-numbers)
         ("C-\\ C-\\ C-p" . mc/insert-letters)
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
  :bind ("C-c 0gg" . phi-grep-in-file))

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
  :config
  (setq figlet-default-font "big")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ascii-table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ascii-table :commands ascii-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ascii ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ascii :bind ("C-c 0aa" . ascii-display))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 0xc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package 0xc
  :bind (("C-c 0ac" . 0xc-convert)
         ("C-c 0a." . 0xc-convert-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elnode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elnode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq vc-follow-symlinks nil)
(setq vc-handled-backends '(Git SVN))
(when (< emacs-major-version 26)
  (bind-key "C-x vh" #'vc-region-history))
(global-set-key "\C-xve" #'vc-ediff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; makefile-executor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package makefile-executor
  :hook (makefile-mode . makefile-executor-mode)
  :config
  (define-key makefile-executor-mode-map "\C-c\C-c" nil)
  (define-key makefile-executor-mode-map "\C-c\C-l" 'makefile-executor-execute-last)
  (define-key makefile-mode-map "\C-c\C-c" nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/git-keymap)
(define-prefix-command 'my/git-keymap)
(global-set-key "\M-sm" 'my/git-keymap)
(defun magit-remove-git-lock-file ()
  "Remove git's index lock file, if it exists."
  (interactive)
  (let ((base (magit-toplevel)))
    (delete-file (concat base "/.git/index.lock"))))
(defun my/enter-magit-status-fullscreen ()
  "Enter magit's status window, filling the entire frame."
  (interactive)
  (require 'magit)
  (let ((magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
    (magit-status-setup-buffer default-directory)))
(use-package magit
  :if (not (version< emacs-version "24.4"))
  :init
  ;; The following allows remote repositories to work under tramp on windows
  ;; (plink), and we put git in our exec-path anyways, so the full path is
  ;; unneeded.  This is also the default setting anyway on other platforms.
  (setq magit-git-executable "git")
  (setq magit-revision-filter-files-on-follow t)
  (setq magit-log-show-refname-after-summary nil)
  (setq magit-no-confirm '())
  (setq magit-process-find-password-functions 'magit-process-password-auth-source)
  (setq magit-auto-revert-tracked-only t)
  (setq magit-prefer-remote-upstream t)
  (setq magit-section-visibility-indicator '("…" . t))
  (setq magit-clone-always-transient t)
  ;; git commands
  :bind (:map my/git-keymap
              ("g" . magit-status)
              ("SPC" . my/enter-magit-status-fullscreen)
              ("l" . magit-list-repositories)
              ("M-g" . magit-dispatch)
              ("f" . magit-find-file) ;; view arbitrary blobs
              ("4f" . magit-find-file-other-window)
              ("h" . magit-log-buffer-file) ;; show all commits that touch current file
              ("H" . magit-dired-log)
              ("y" . magit-cherry)
              ("e" . ediff-merge-revisions-with-ancestor) ;; to see all differences, even those automatically merged
              ("m" . magit-toggle-margin)
              ("b" . magit-blame-addition)
              ("B" . magit-blame-reverse)
              ("U" . magit-unstage-all) ;; unstage all changes (like SU but forces HEAD)
              ("s" . magit-stage-file)
              ("u" . magit-unstage-file)
              ("r" . magit-reset-soft) ;; soft reset; hard reset can use C-u x
              ("d" . magit-diff-buffer-file-popup)
              ("c" . magit-clone)
              ("x" . magit-clean)
              ("k" . magit-checkout-stage)
              ("o" . magit-file-checkout)
              ("D" . magit-file-delete)
              )
  :config
  (put 'magit-clean 'disabled nil)
  (use-package with-editor)
  (global-magit-file-mode 1)
  (magit-auto-revert-mode 0)
  (setq magit-repository-directories
        `(,(cons (expand-file-name "~/src") 2)))
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'magit-section-movement-hook 'magit-status-maybe-update-blob-buffer)
  (push (cons 'stashes 'hide) magit-section-initial-visibility-alist)
  (push (cons [* unpushed status] 'show) magit-section-initial-visibility-alist)
  (push (cons [* unpulled status] 'show) magit-section-initial-visibility-alist)
  ;; add ido shortcut
  (if (version< emacs-version "25.1")
      (add-hook
       'ido-setup-hook
       (lambda() (define-key ido-completion-map
                   (kbd "C-x g") 'ido-enter-magit-status)))
    (add-hook
     'ido-setup-hook
     (lambda() (define-key ido-common-completion-map
                 (kbd "C-x g") 'ido-enter-magit-status))))

  (add-hook 'magit-revision-mode-hook 'bug-reference-mode)
  (add-hook 'git-commit-setup-hook 'bug-reference-mode)

  (defun harmsway-repolist-column-dirty (_id)
    "Insert a letter if there are uncommitted changes.
This is a shameless copy of the version in `magit', but reverses
the priority such that staged and unstaged changes appear before
untracked files, cf. `https://github.com/magit/magit/issues/3354'.

Show S if there is at least one staged file.
Show U if there is at least one unstaged file.
Show N if there is at least one untracked file.
Only one letter is shown, the first that applies."
    (cond ((magit-staged-files)    "S")
          ((magit-unstaged-files)  "U")
          ((magit-untracked-files) "N")))

  ;; add a dirty indicator to the second-to-last column
  (setq magit-repolist-columns
        (reverse (append (cons (car (reverse magit-repolist-columns))
                               (list '("D" 1 harmsway-repolist-column-dirty nil)))
                         (cdr (reverse magit-repolist-columns)))))
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  ;; to display fullframe, use 'magit-display-buffer-fullframe-status-v1
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; transient ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package transient
  :after magit
  :config
  (transient-bind-q-to-quit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; forge ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package forge
  :disabled
  :after magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; magit-org-todos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit-org-todos
  :after magit
  :config (magit-org-todos-autoinsert))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git-timemachine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-timemachine
  :bind (:map my/git-keymap ("t" . git-timemachine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vc-msg ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vc-msg
  :bind (:map my/git-keymap ("." . vc-msg-show)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default comint-input-ignoredups t)
(setq comint-terminfo-terminal "ansi")
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 1024)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shx ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package shx :config (shx-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xterm-color ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package xterm-color
  :demand t
  :init
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              (font-lock-mode -1)       ;disable font lock, prevent re-enabling
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (add-hook 'comint-preoutput-filter-functions
                        'xterm-color-filter nil t)))
  (add-hook 'compilation-start-hook
            (lambda (proc)
              (when (eq (process-filter proc) 'compilation-filter)
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (funcall 'compilation-filter proc
                            (xterm-color-filter string))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shell-pop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package shell-pop
  :bind (("<f1>" . shell-pop)
         ("\e\e1" . shell-pop))
  :init
  (setq shell-pop-autocd-to-working-dir nil)
  (setq shell-pop-universal-key "<f1>"))

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
          ;; this works around a bug with shell-pop on emacs 25.1,
          ;; cf. https://github.com/kyagi/shell-pop-el/issues/51
          ("\\*shell\\*" :regexp t :same t)
          (occur-mode :popup t :select nil :align bottom)
          (vlf-occur-mode :popup t :select nil :align bottom)
          (grep-mode :popup t :select nil :align bottom)
          ("\\*.*Help.*\\*" :regexp t :popup t :select t)
          ("\\*xref\\*" :regexp t :popup t :select t :align bottom)
          (help-mode :popup t :select t)
          (Man-mode :popup t :select t)
          (diff-mode :popup t :select t)
          (apropos-mode :popup t :select t)
          (completion-list-mode :select t)
          ("compilation" :regexp t :popup t :select nil :align bottom)
          (command-history-mode :popup t :select t)
          ("Shell Command Output" :regexp t :popup t :select nil)
          ("COMMIT_EDITMSG" :select t)
          ("VC-history" :regexp t :select t :popup t)
          (" \\*gentags-" :regexp t :popup t :select nil :align bottom)
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
        (append '(markdown-mode
                  text-mode
                  proviso-dashboard-mode
                  proviso-deploy-mode)
                page-break-lines-modes))
  (global-page-break-lines-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; beacon ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :bind (("M-s C-l" . beacon-blink)
         ("C-c 0 M-b" . beacon-mode))
  :defer t
  :config
  (add-to-list 'beacon-dont-blink-major-modes 'etags-select-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bookmark+ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bookmark+
  :bind (
         ;; C-x p <left>
         ("<f7>" . bmkp-previous-bookmark)
         ("\e\e7" . bmkp-previous-bookmark)
         ;; C-x p <up>
         ("<S-f7>" . bmkp-previous-bookmark-this-file/buffer)
         ("\e\e&" . bmkp-previous-bookmark)
         ("C-c M-7" . bmkp-previous-bookmark-this-file/buffer)
         ;; C-x p <right>
         ("<f8>" . bmkp-next-bookmark)
         ("\e\e8" . bmkp-next-bookmark)
         ;; C-x p <down>
         ("<S-f8>" . bmkp-next-bookmark-this-file/buffer)
         ("\e\e*" . bmkp-next-bookmark)
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
  :config
  (add-to-list 'aw-dispatch-alist
               '(?B balance-windows "Balance Windows")))

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
  (setq dumb-jump-max-find-time 5)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smart-jump ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smart-jump
  :if (> emacs-major-version 24)        ;requires 'xref
  :bind (("C-c .." . #'smart-jump-jump-key)
         ("C-c .," . #'smart-jump-pop-key)
         ("C-c .?" . #'smart-jump-refs-key)
         ("C-c .p" . #'smart-jump-peek-key))
  :init
  (setq smart-jump-bind-keys t)
  (setq smart-jump-bind-keys-for-evil nil)
  (setq smart-jump-jump-key "C-c ..")
  (setq smart-jump-pop-key "C-c .,")
  (setq smart-jump-refs-key "C-c .?")
  (setq smart-jump-peek-key "C-c .p")
  :config
  (smart-jump-setup-default-registers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; source-peek ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package source-peek
  :bind ("\e\e." . source-peek))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; plur ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package
  plur
  :if (not (version< emacs-version "24.4"))
  :bind (("C-c 0gs" . plur-isearch-forward)
         ("C-c 0gr" . plur-replace)
         ("C-c 0g%" . plur-query-replace)
         :map isearch-mode-map
         ("C-p" . plur-isearch-forward)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; string-inflection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package string-inflection
  :bind ("C--" . string-inflection-all-cycle)
  :init
  (setq string-inflection-skip-backward-when-done t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hydra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package hydra
  :defer t
  :init
  (setq lv-use-separator t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; grep ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package wgrep
  :after grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ag ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ag
  :if (executable-find "ag")
  :defer t
  :init
  :bind (("C-c gaa" . ag)
         ("C-c gap" . ag-project)
         )
  :config
  (require 'wgrep-ag)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; grep-context ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package grep-context
  :defer t
  :init
  (with-eval-after-load 'compile
    (require 'grep-context)
    (define-key compilation-mode-map (kbd "+") #'grep-context-more-around-point)
    (define-key compilation-mode-map (kbd "-") #'grep-context-less-around-point))
  (with-eval-after-load 'grep
    (require 'grep-context)
    (define-key grep-mode-map (kbd "+") #'grep-context-more-around-point)
    (define-key grep-mode-map (kbd "-") #'grep-context-less-around-point))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; el-grep ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elgrep
  :bind ("C-c 0ge" . elgrep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; deadgrep ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package deadgrep
  :bind ("C-c 0gd" . deadgrep)
  :init
  (setq deadgrep-project-root-function #'proviso-current-project-root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; occur ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-s M-o") 'multi-occur-in-matching-buffers)
(bind-key "C-c b" 'bmkp-occur-create-autonamed-bookmarks occur-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; swiper ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package swiper
  :if (not (version< emacs-version "24.1"))
  :bind (("M-s M-s" . swiper)
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
         )
  :config
  (global-set-key "\e\eia" 'avy-resume))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ivy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :demand t
  :bind (("M-s M-." . ivy-push-view)
         ("M-s M-," . ivy-pop-view)
         ("M-s M-/" . ivy-switch-view))
  :init
  (setq resize-mini-windows t)
  (setq ivy-display-style 'fancy)
  (setq ivy-extra-directories '("../" "./"))
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t)
  (setq ivy-use-selectable-prompt t)
  :config
  (global-set-key "\e\eii" 'ivy-resume)
  (ivy-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ivy-rich ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy-rich
  :if (> emacs-major-version 24)
  :after ivy
  :config
  (ivy-rich-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ivy-prescient ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package prescient
  :disabled
  :config
  (setq prescient-save-file (concat my/user-directory "prescient"))
  (prescient-persist-mode 1))
(use-package ivy-prescient
  :disabled
  :after (ivy prescient)
  :config
  (ivy-prescient-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ivy-posframe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy-posframe
  :disabled
  :if (<= 26 emacs-major-version)
  :after ivy
  :config
  (setq ivy-display-function #'ivy-posframe-display)
  (ivy-posframe-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; counsel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :bind (("M-s M-f" . counsel-git)
         ("M-s M-r" . counsel-recentf)
         ("M-s M-p" . counsel-git-grep)
         ("M-s M-g" . counsel-grep)
         ("M-s M-c" . counsel-ag)
         ("M-s M-z" . counsel-git-stash)
         ("M-s M-i" . counsel-imenu)
         ("M-s M-l" . counsel-git-log)
         ("M-s M-e" . counsel-find-file-extern)
         ("C-c 0ei" . counsel-read-env)
         ("M-s M-d" . counsel-dired-jump)
         ("M-s M-v" . counsel-file-jump)
         ("M-s M-y" . counsel-yank-pop)
         ("M-s M-b" . counsel-load-library)
         ("M-s M-m" . counsel-load-theme)
         ("M-s M-x" . counsel-command-history)
         ("M-s M-k" . counsel-descbinds)
         ("C-c 0l" . counsel-locate)
         ("M-s M-SPC" . counsel-mark-ring)
         ("C-h C-a" . counsel-apropos)
         ("M-s M-0" . counsel-wmctrl)
         ("M-s M-1" . counsel-shell-history)
         ("M-s M-n" . counsel-minibuffer-history)
         ("M-s a" . counsel-linux-app)
         ("C-c 0q`" . counsel-tmm)
         ("C-c 0cf" . counsel-faces)
         ("C-c 0ce" . counsel-colors-emacs)
         ("C-c 0cw" . counsel-colors-web)
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
  (let ((elt (assoc 'counsel-M-x ivy-initial-inputs-alist)))
    (when elt (setf (cdr elt) "")))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; frog-jump-buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package frog-jump-buffer
  :bind ("M-i" . frog-jump-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eldoc-box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun harmsway-toggle-eldoc-box-mode ()
  "Toggle `eldoc-box-hover-mode' in current buffer."
  (interactive)
  (if eldoc-box-hover-mode
      (eldoc-box-hover-mode -1)
    (eldoc-box-hover-mode 1)))
(bind-key "C-c 0b" #'harmsway-toggle-eldoc-box-mode)
(use-package eldoc-box)

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
          " counsel" " ivy" " WK" " company"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smart-mode-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smart-mode-line
  :if (version<= "24.3" emacs-version)
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; moody ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package moody
  :disabled
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-sml/mode-line-buffer-identification)
  )

;; undo
(unless (boundp 'warning-suppress-types)
  (setq warning-suppress-types nil))
(push '(undo discard-info) warning-suppress-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; undo-tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :init
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-enable-undo-in-region nil)
  :config
  ;; unset this key for use in other packages
  (define-key undo-tree-map "\C-_" nil)
  ;; reset the undo tree history (useful after reverting buffer)
  (global-set-key "\C-cu" (lambda()(interactive)(setq buffer-undo-tree nil)))
  (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; goto-chg ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package goto-chg
  :bind (([?\C-.] . goto-last-change)
         ("M-' ." . goto-last-change)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tramp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tramp
  :defer t
  :init
  (setq tramp-verbose 1)
  (when (boundp 'my/user-name)
    (setq tramp-default-user my/user-name))
  ;; my/autosave-dir defined in custom-backups.el
  (setq tramp-auto-save-directory my/autosave-dir)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq remote-file-name-inhibit-cache 10)
  (setq tramp-completion-reread-directory-timeout 10)
  ;; (setq vc-ignore-dir-regexp
  ;;       (format "\\(%s\\)\\|\\(%s\\)"
  ;;               vc-ignore-dir-regexp tramp-file-name-regexp))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ssh-deploy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun harmsway-ssh-deploy-save-hook ()
;;   "Hook for `ssh-deploy' mode that uploads a file when saved."
;;   (when (bound-and-true-p ssh-deploy-on-explicit-save)
;;     (ssh-deploy-upload-handler)))
;; (defun harmsway-ssh-deploy-find-file-hook ()
;;   "Hook for `ssh-deploy' mode to detect remote modifications on file open."
;;   (when (bound-and-true-p ssh-deploy-automatically-detect-remote-changes)
;;     (ssh-deploy-remote-changes-handler)))
;; (defvar my/ssh-deploy-keymap)
;; (define-prefix-command 'my/ssh-deploy-keymap)
;; (global-set-key "\C-c0d" 'my/ssh-deploy-keymap)
;; (use-package ssh-deploy
;;   :bind (:map my/ssh-deploy-keymap
;;               ("u" . ssh-deploy-upload-handler)
;;               ("f" . ssh-deploy-upload-handler-forced)
;;               ("d" . ssh-deploy-download-handler)
;;               ("D" . ssh-deploy-delete-handler)
;;               ("R" . ssh-deploy-rename-handler)
;;               ("=" . ssh-deploy-diff-handler)
;;               ("x" . ssh-deploy-remote-changes-handler)
;;               ("o" . ssh-deploy-open-remote-file-handler)
;;               ("b" . ssh-deploy-browse-remote-base-handler)
;;               ("B" . ssh-deploy-browse-remote-handler)
;;               ("t" . ssh-deploy-remote-terminal-shell-base-handler)
;;               ("T" . ssh-deploy-remote-terminal-shell-handler)
;;               )
;;   :config
;;   (add-hook 'after-save-hook #'harmsway-ssh-deploy-save-hook)
;;   (add-hook 'find-file-hook #'harmsway-ssh-deploy-find-file-hook)
;;   (require 'async)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; outshine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package outshine
  :demand t
  :bind ("C-c -" . outshine-cycle)
  :init
  (defvar outline-minor-mode-prefix "\M-@")
  (dolist (hook '(
                  ;; emacs-lisp-mode-hook
                  restclient-mode-hook
                  ))
    (add-hook hook 'outshine-mode)))

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
  (setq org-M-RET-may-split-line '((default . nil)))
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
     (sql . t)
     (sqlite . t)
     ))
  (if (< emacs-major-version 26)
      (progn
        (add-to-list 'org-babel-load-languages '(sh . t))
        (require 'ob-sh))
    (add-to-list 'org-babel-load-languages '(shell . t))
    (require 'ob-shell))
  (require 'ox-md)                      ;markdown export
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package pack
  :init
  (setq pack-silence t)
  :config
  (with-eval-after-load 'dired
    (define-key dired-mode-map "P" #'pack-dired-dwim)))

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
  (use-package dired+
    :init
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag t)
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
  (setq wdired-create-parent-directories t)
  (setq wdired-allow-to-change-permissions t)
  ;; (use-package ivy-dired-history)
  (use-package dired-sidebar
    :init
    (setq dired-sidebar-use-evil-integration nil)
    )
  ;; sorting
  ;; dired-quick-sort breaks ftp
  ;; (use-package dired-quick-sort
  ;;   :config
  ;;   ;; test here since we load the os file after dired is already loaded
  ;;   (when (string= my/system-name "windows-nt")
  ;;     (setq ls-lisp-use-insert-directory-program t))
  ;;   (dired-quick-sort-setup))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; disk-usage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package disk-usage
  :bind (("C-c 0du" . disk-usage)
         ("C-c 0d." . disk-usage-here)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; treemacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'harmsway-treemacs-keymap)
(use-package treemacs
  :bind (("C-c 0t" . harmsway-treemacs-keymap)
         :map harmsway-treemacs-keymap
         ("t" . treemacs)
         ("f" . treemacs-find-file)
         ("SPC" . treemacs-select-window)
         ("1" . treemacs-delete-other-windows)
         ("B" . treemacs-bookmark)
         ("M-t" . treemacs-find-tag))
  :init
  (setq treemacs-no-png-images t)
  (setq treemacs-collapse-dirs (if (executable-find "python3") 3 0))
  (setq treemacs-persist-file (concat my/user-directory "treemacs"))
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode)
  (treemacs-git-mode 'extended)
  )
(use-package treemacs-magit
  :after treemacs)

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
  "Advise `ediff-buffers'."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; diffview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diffview
  :after diff-mode
  :bind (:map diff-mode-map
              ("C-c C-v" . diffview-current)
              ("C-c C-g" . diffview-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ediff-trees ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'harmsway-difftrees-keymap)
(use-package ediff-trees
  :bind (("C-c e" . harmsway-difftrees-keymap)
         :map harmsway-difftrees-keymap
         ("e" . ediff-trees)
         ("n" . ediff-trees-examine-next)
         ("p" . ediff-trees-examine-previous)
         ("C-n" . ediff-trees-examine-next-regexp)
         ("C-p" . ediff-trees-examine-previous-regexp)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ztree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ztree
  :bind (:map harmsway-difftrees-keymap
              ("z" . ztree-diff)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; diff-hl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun harmsway-diff-hl-revert-highlight-hunk (end)
  "Highlight only the diff to be reverted, from point until END."
  (redisplay)
  (font-lock-unfontify-buffer)
  (font-lock-fontify-region (point) end))
(defun harmsway-diff-hl-revert-hide-other-hunks (_end)
  "Show only the current hunk to be reverted."
  (diff-restrict-view))

(use-package  diff-hl-dired
  :init (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote))
(use-package diff-hl
  :config
  (setq diff-hl-highlight-revert-hunk-function
        #'harmsway-diff-hl-revert-highlight-hunk)
  (use-package diff-hl-flydiff :config (diff-hl-flydiff-mode 1))
  (use-package diff-hl-amend)
  (global-diff-hl-mode 1)
  (unless (display-graphic-p)
    (use-package diff-hl-margin :config (diff-hl-margin-mode 1))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; in-memory-diff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package in-memory-diff
  :bind (("M-s dii" . in-memory-diff)
         ("M-s dif" . in-memory-diff-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; difflib ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package difflib :defer t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; zoom-window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zoom-window
             :bind ("C-0" . zoom-window-zoom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gif-screencast ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package gif-screencast
  :commands (gif-screencast)
  :config
  (define-key gif-screencast-mode-map (kbd "<f3>") #'gif-screencast-toggle-pause)
  (define-key gif-screencast-mode-map (kbd "<f4>") #'gif-screencast-stop)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; perspective ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package perspective
  :disabled
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
(defun harmsway-try-smerge()
  "Evaluate whether to turn on `smerge-mode'."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward smerge-begin-re nil t)
      (smerge-mode 1))))

;; Modified from `https://github.com/alphapapa/unpackaged.el'
(with-eval-after-load 'hydra
  (defhydra smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper :color blue)
    ("=" smerge-diff-upper-lower :color blue)
    (">" smerge-diff-base-lower :color blue)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("Q" nil "cancel" :color blue)))

(defun harmsway-smerge-hydra ()
  "Run `smerge-hydra'."
  (interactive)
  (require 'hydra)
  (smerge-hydra/body))

(use-package smerge-mode
  :init
  (add-hook 'find-file-hook #'harmsway-try-smerge t)
  ;; (add-hook 'smerge-mode-hook #'harmsway-smerge-hydra)
  :config
  (define-key smerge-mode-map "h" #'harmsway-smerge-hydra))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multi-term ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multi-term
  :bind (("C-2" . multi-term)
         ("M-' 2" . multi-term)
         ("C-1" . multi-term-prev)
         ("M-' 1" . multi-term-prev)
         ("C-3" . multi-term-next)
         ("M-' 3" . multi-term-next)
         ("C-4" . multi-term-dedicated-toggle)
         ("M-' 4" . multi-term-dedicated-toggle)
         ("C-5" . multi-term-dedicated-select)
         ("M-' 5" . multi-term-dedicated-select)
         )
  :init
  (setq multi-term-dedicated-select-after-open-p t)
  ;:init (setq multi-term-program "/bin/tcsh")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bash-completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bash-completion
  :after shell-mode
  :config
  (bash-completion-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vlf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vlf-setup
  :defines (vlf-tune-enabled vlf-batch-size vlf-batch-size-remote)
  :init
  ;; for files over 50MB, only open 50MB at a time
  (setq large-file-warning-threshold 26214400) ;25MB
  (setq vlf-batch-size 15728640)               ;15MB
  (setq vlf-batch-size-remote 524288)          ;512k
  (setq vlf-tune-enabled nil)           ;don't adjust batch size dynamically
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rtags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar rtags-exec (executable-find "rdm"))
(use-package rtags
  :disabled
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flyspell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun harmsway-toggle-flyspell-prog-mode ()
  "Toggle `flyspell-prog-mode' in current buffer."
  (interactive)
  (if flyspell-mode
      (turn-off-flyspell)
    (flyspell-prog-mode)))
(bind-key "C-c C-4" #'harmsway-toggle-flyspell-prog-mode)

(defun harmsway-try-flyspell (arg)
  "Attempt to correct spelling of ARG, if not in a comment."
  (if (nth 4 (syntax-ppss))
      (call-interactively 'flyspell-correct-word-before-point)
    nil))

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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flyspell-correct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map ("C-c \\\\" . flyspell-correct-wrapper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; company-ispell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun harmsway-company-ispell-at-point ()
  "Begin `company-ispell' at point."
  (interactive)
  (company-begin-backend 'company-ispell))
(use-package company-ispell
  :after ispell
  :bind ("C-c \\ TAB" . #'harmsway-company-ispell-at-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ace-popup-menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ace-popup-menu :config (ace-popup-menu-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hippie-expand ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\M-/" 'hippie-expand)
(setq hippie-expand-try-functions-list
      (append '(harmsway-try-flyspell
                yas-hippie-try-expand
                )
              hippie-expand-try-functions-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tab-always-indent 'complete)      ;or t to avoid completion
(add-to-list 'completion-styles 'initials t)
(setq completion-auto-help nil)
(setq completion-cycle-threshold t)     ;always cycle
;; Ignore case when completing file names
(setq read-file-name-completion-ignore-case nil)
(setq uniquify-recentf-func 'uniquify-recentf-ivy-recentf-open)

(defun harmsway-dabbrev-complete-at-point ()
  "Complete dabbrev at point."
  (dabbrev--reset-global-variables)
  (let* ((abbrev (dabbrev--abbrev-at-point))
         (cands (dabbrev--find-all-expansions abbrev t))
         (bnd (bounds-of-thing-at-point 'symbol)))
    (list (car bnd) (cdr bnd) cands)))
(defun harmsway-add-ivy-completion-at-point ()
  "Add completion at point for ivy."
  (require 'dabbrev)
  (add-hook 'completion-at-point-functions 'harmsway-dabbrev-complete-at-point))
;(add-hook 'after-init-hook 'harmsway-add-ivy-completion-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; company ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion-at-point
(defun harmsway-company-at-point ()
  "Try company completion at point."
  (and (not (minibufferp))
       (featurep 'company)
       company-mode
       #'company-complete-common))

(defun harmsway-company-capf-workaround (completion-functions)
  "Ensure `harmsway-company-at-point' is not a member of COMPLETION-FUNCTIONS.
This is a workaround for `company-capf' so that harmsway's
completion at point mechanism does not interfere with `completion-at-point-functions'."
  (if (listp completion-functions)
      (remq 'harmsway-company-at-point completion-functions)
    completion-functions))

(advice-add 'company--capf-workaround :filter-return 'harmsway-company-capf-workaround)

;; Smart completion
(defvar-local company-smart-backend #'company-etags
  "The smartest backend for company-completion.")
(defun harmsway-smart-completion-at-point ()
  "Perform smart completion at point."
  (interactive)
  (company-begin-backend company-smart-backend))

(global-set-key "\M-/" #'hippie-expand)
(global-set-key "\e\e/" #'harmsway-smart-completion-at-point)
(global-set-key (kbd "s-/") #'harmsway-smart-completion-at-point)
;; (global-set-key [?\C-\M-/] #'harmsway-smart-completion-at-point)

(use-package company
  :init
  (setq company-idle-delay nil)
  (setq company-tooltip-idle-delay 1)
  (setq company-require-match nil)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-minimum-length 1)
  (setq company-dabbrev-downcase 'case-replace)
  ;; (setq company-begin-commands '(self-insert-command))
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-backends
        '((
           company-files
           company-keywords
           company-capf
           company-dabbrev-code
           company-etags
           company-dabbrev
           )))
  :config
  (global-company-mode 1)
  (add-hook 'completion-at-point-functions 'harmsway-company-at-point)
  ;; Use Ctrl-[N,P] rather than Meta to cycle
  (define-key company-active-map "\C-n" #'company-select-next)
  (define-key company-active-map "\C-p" #'company-select-previous)
  (define-key company-active-map "\M-n" nil)
  (define-key company-active-map "\M-p" nil)
  ;; cycle back and forth with TAB and S-TAB
  (define-key company-active-map [tab] #'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") #'company-complete-common-or-cycle)
  (define-key company-active-map [backtab] #'company-select-previous)
  (define-key company-active-map (kbd "S-TAB") #'company-select-previous)
  (define-key company-active-map "\C-e" #'company-other-backend)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; company-quickhelp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company-quickhelp
  :if (display-graphic-p)
  :after company
  :config
  (setq company-quickhelp-delay 0.5)
  (setq company-quickhelp-use-propertized-text t)
  (company-quickhelp-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; company-statistics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company-statistics
  :after company
  :config
  (company-statistics-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-complete
  :disabled
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
                   python-mode bat-mode gud-mode sh-mode text-mode
                   makefile-mode makefile-automake-mode makefile-gmake-mode
                   autoconf-mode gdb-script-mode awk-mode csv-mode
                   mock-mode org-mode html-mode text-mode sql-mode
                   sql-interactive-mode conf-mode markdown-mode
                   git-commit-mode mock-mode dart-mode plantuml-mode
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

  (defun harmsway-auto-complete-at-point ()
    (when (and (not (minibufferp))
               (fboundp 'auto-complete-mode)
               auto-complete-mode)
      #'auto-complete))

  (defun harmsway-add-ac-completion-at-point ()
    (add-to-list 'completion-at-point-functions 'harmsway-auto-complete-at-point))
  (add-hook 'auto-complete-mode-hook 'harmsway-add-ac-completion-at-point)

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
(defun harmsway-yas-at-point ()
  "Try to complete yasnippet via company.
See `https://github.com/company-mode/company-mode/issues/205'."
  (interactive)
  (require 'company-yasnippet)
  (let ((prefix (company-yasnippet 'prefix))
        candidates)
    (when prefix
      (setq candidates (company-yasnippet 'candidates prefix))
      (if (and (= (length candidates) 1)
               (string= prefix (car candidates)))
          (yas-expand)
        (company-begin-backend 'company-yasnippet)))))

(global-set-key [backtab] #'harmsway-yas-at-point)
(global-set-key [(shift tab)] #'harmsway-yas-at-point)

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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emmet-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emmet-mode
  :commands (emmet-mode)
  :init
  (setq emmet-move-cursor-between-quotes t)
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; quick-peek ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package quick-peek)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flymake ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO

;;;;;;;;;;;;;;;;;;;;;;;; flymake-diagnostic-at-point ;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-hook #'flymake-diagnostic-at-point-mode))

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
  (setq flycheck-indication-mode nil)
  (setq flycheck-global-modes
        '(emacs-lisp-mode python-mode dart-mode sh-mode c++-mode json-mode
                          js2-mode go-mode))
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-shellcheck-follow-sources nil)
  (use-package flycheck-package :config (flycheck-package-setup))
  (use-package flycheck-checkbashisms)
  (use-package flycheck-bashate)
  ;; hack because flycheck unreasonably demands package installation
  (unless (fboundp 'pkg-info-version-info)
    (defun pkg-info-version-info (_) "unknown"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck-popup-tip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck-popup-tip
  :after flycheck
  :disabled
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck-pos-tip ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck-pos-tip
  :after flycheck
  :disabled
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
  (setq flycheck-pos-tip-display-errors-tty-function
        (lambda (errors)
          (let ((message (mapconcat #'flycheck-error-format-message-and-id
                                    errors "\n\n")))
            (popup-tip message)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck-inline ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck-inline
  :after flycheck
  :disabled
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck-posframe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck-posframe
  :after flycheck
  :if (display-graphic-p)
  :init
  (defface flycheck-posframe-background-face
    '((t :background "yellow"))
    "Background face used for flycheck posframe popups."
    :group 'flycheck-posframe)
  (defface flycheck-posframe-face
    '((t (:foreground "black" :background "yellow")))
    "Face used for flycheck posframe popups."
    :group 'flycheck-posframe)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; restclient ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package restclient
  :mode ("\\.http$" . restclient-mode)
  :commands restclient-mode
  :config
  (use-package company-restclient)
  (add-hook 'restclient-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (setq company-backends
                    (list
                     (cons 'company-restclient
                           (copy-tree
                            (car company-backends)))))
              (setq-local company-smart-backend 'company-restclient)))
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
          ((bat-mode . "Dos") .
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; easy-kill ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package easy-kill
  :disabled
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; popup-kill-ring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq kill-ring-max 100)
(use-package popup-kill-ring :bind ("C-M-y" . popup-kill-ring))

;;;;;;;;;;;;;;;;;;;;;;;;;;; popup-global-mark-ring ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package popup-global-mark-ring :bind ("\e\ey" . popup-global-mark-ring))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; zop-to-char ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zop-to-char
  :bind (("M-z" . zop-to-char)
         ("\e\ez" . zop-up-to-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; detour ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package detour
  :bind (("C-8" . detour-mark)
         ("C-9" . detour-back)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; htmlize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package htmlize
  :commands
  (htmlize-buffer htmlize-region htmlize-file htmlize-many-files
                  htmlize-many-files-dired))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; awk-it ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package awk-it
  :bind (("C-c 0aw" . awk-it)
         ("C-c 0ap" . awk-it-with-separator)
         ("C-c 0as" . awk-it-single)
         ("C-c 0ag" . awk-it-single-with-separator)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; list-environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package list-environment :bind ("C-c 0ee" . list-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sort-words ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sort-words :commands sort-words)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; unfill ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package unfill
  :bind ("M-Q" . unfill-paragraph))

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
(defun my/before-save-hook() "Presave hook."
       (when (memq major-mode
                   '(
                     awk-mode
                     bat-mode
                     c++-mode
                     cmake-mode
                     csharp-mode
                     dart-mode
                     bat-mode
                     emacs-lisp-mode
                     folio-mode
                     gdb-script-mode
                     gitattributes-mode
                     gitconfig-mode
                     gitignore-mode
                     java-mode
                     json-mode
                     lua-mode
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
  :bind (([f6] . remotehost-connect)
         ("\e\e6" . remotehost-connect)
         )
  :commands remotehost-connect-read-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; os ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((system-file (concat my/os-dir my/system-name)))
  ;; load os file
  (load system-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package harmsway-gui
  :config
  (harmsway-gui-load))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; choose-font ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package choose-font
  :demand t
  :bind ("C-c M-o" . choose-font))

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
(let* ((system (harmsway-unqualify-host-name (system-name)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; applescript-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package applescript-mode
  :mode "\\.applescript$"
  :config
  ;; (add-hook 'applescript-mode-hook
  ;;           (lambda ()
  ;;             ))
  ;; remap 'as-execute-buffer
  (define-key as-mode-map "\C-c\C-c" nil)
  (define-key as-mode-map "\C-c\C-ce" #'as-execute-buffer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; awk-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'awk-mode-hook
          (lambda()
            (setq comment-start "#") (setq comment-end "")
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bat-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun harmsway-company-cmd-fix-shell (orig-fun &rest args)
  "Fix shell used to create candidates by `company-cmd', aka ORIG-FUN with ARGS."
  (let ((shell-file-name (executable-find "cmdproxy.exe")))
    (apply orig-fun args)))

(use-package bat-mode :mode ("\\.bat$" "\\.cmd$")
  :config
  (use-package dos-indent)
  (advice-add #'company-cmd-build-alist :around
              #'harmsway-company-cmd-fix-shell)
  (add-hook 'bat-mode-hook
            (lambda()
              (setq-default indent-tabs-mode nil)
              (dos-indent)
              (setq-local company-smart-backend 'company-cmd)
              ))
  ;; remap 'bat-run
  (define-key bat-mode-map "\C-c\C-c" nil)
  (define-key bat-mode-map "\C-c\C-cr" 'bat-run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bazel-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bazel-mode
  :mode ("\\.bazel$" "\\.bzl$" "WORKSPACE$")
  :config
  (add-hook 'bazel-mode-hook
            (lambda()
              (add-hook 'before-save-hook #'bazel-format nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cask-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cask-mode
  :mode "/Cask\\'"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; conf-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'conf-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            (subword-mode 1)
            (setq comment-start "#") (setq comment-end "")
            ;; (idle-highlight-mode 1)
            ;; conf-colon-mode still bound to "\C-c:"
            (local-unset-key "\C-c\C-c")
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cmake-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cmake-mode
  :mode ("CMakeLists\\.txt$" "\\.cmake$")
  :config
  (use-package cmake-font-lock)
  (defun my/cmake-fix-underscore()
    (modify-syntax-entry ?_ "_" cmake-mode-syntax-table))
  (add-hook 'cmake-mode-hook #'my/cmake-fix-underscore)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
  (add-hook 'cmake-mode-hook (lambda ()
                               (make-local-variable 'company-backends)
                               (setq company-backends
                                     (list
                                      (cons 'company-cmake
                                            (copy-tree
                                             (car company-backends)))))
                               (setq-local company-smart-backend
                                           'company-cmake)))
  )
(use-package eldoc-cmake
  :after cmake-mode
  :config
  (add-hook 'cmake-mode-hook #'eldoc-cmake-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; crontab-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package crontab-mode
  :mode ("\\.\\(ana\\)?cron\\(tab\\)?$" "\\(ana\\)?cron\\(tab\\)?\\.")
  :commands crontab-get
  :config
  (add-hook 'crontab-mode-hook
            (lambda ()
              (setq comment-start "#") (setq comment-end "")
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; csproj-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package csproj-mode :mode "\\.[^.]*proj$")

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
              ;; (flycheck-mode 1)
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dotenv-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dotenv-mode :mode ("\\.env$" "\\.env\\.example$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elf-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elf-mode
  :config
  (elf-setup-default)                   ;adds entry to magic-mode-alist
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs-lisp-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun harmsway-compile-lisp-file ()
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
(add-hook 'after-save-hook #'harmsway-compile-lisp-file)

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            (add-hook 'completion-at-point-functions 'harmsway-company-at-point nil t)
            (define-key emacs-lisp-mode-map "\r" 'reindent-then-newline-and-indent)
            (define-key emacs-lisp-mode-map (kbd "\C-c RET")
              (lambda()(interactive)
                (byte-compile-file (buffer-file-name))))
            (if (featurep 'rainbow-mode)
                (rainbow-turn-on)
              (my/syntax-color-hex-values))
            ))
(when (< emacs-major-version 26)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))
(use-package lisp-extra-font-lock
  :after elisp-mode
  :config
  (lisp-extra-font-lock-global-mode 1))
(use-package highlight-defined
  :after elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package go-mode :mode "\\.go$"
  :init
  (add-hook 'go-mode-hook #'go-eldoc-setup))
(use-package go-guru :after go-mode)
(use-package go-eldoc :after go-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; groovy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package groovy-mode :interpreter "groovy"
  :mode ("\\.groovy$"
         "\\.gradle$"
         "Jenkinsfile"
         )
  :config
  (use-package inf-groovy)
  (add-hook 'groovy-mode-hook 'inf-groovy-keys)
  ;; for 'run-groovy, need to set $GROOVY_HOME, or groovysh needs to exist or
  ;; be in $PATH
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; haskell-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package haskell-mode
  :interpreter "runghc"
  :interpreter "runhaskell"
  :mode ("\\.[gh]s$" "\\.hsig$" "\\.l[gh]s$" "\\.hsc$")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; html-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'html-mode-hook
          (lambda()
            (if (featurep 'rainbow-mode)
                (rainbow-turn-on)
              (my/syntax-color-hex-values))
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; jinja-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package jinja2-mode :mode "\\.jinja$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; jq-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package jq-mode
  :mode "\\.jq$"
  :init
  (with-eval-after-load 'json-mode
    (require 'jq-mode)
    (define-key json-mode-map "\C-c\C-q" #'jq-interactively)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; js-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package nodejs-repl
  :init
  (add-hook 'js-mode-hook
            (lambda ()
              (define-key js-mode-map "\C-x\C-e" 'nodejs-repl-send-last-expression)
              (define-key js-mode-map "\C-c\C-j" 'nodejs-repl-send-line)
              (define-key js-mode-map "\C-c\C-r" 'nodejs-repl-send-region)
              (define-key js-mode-map "\C-c\C-l" 'nodejs-repl-load-file)
              (define-key js-mode-map "\C-c\C-z" 'nodejs-repl-switch-to-repl)
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; js2-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package js2-mode
  :mode (("\\.jsx?$" . js2-mode)
         ("\\.js\\.erb$" . js2-mode))
  :config
  (setq js2-basic-offset 2)
  (setq js2-include-node-externs t)
  (define-key js2-mode-map (kbd "M-.") nil)
  (define-key js2-mode-map "\C-c." 'js2-jump-to-definition))
(use-package js2-imenu-extras
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))
(use-package js2-refactor
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config
  (define-key js2-mode-map "\C-k" #'js2r-kill)
  (js2r-add-keybindings-with-prefix "\e\eb"))
(use-package xref-js2
  :if (< 24 emacs-major-version)
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions
                                       #'xref-js2-xref-backend nil t)))
  :config
  (define-key js2-mode-map (kbd "M-.") 'xref-find-definitions))

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
  (use-package json-navigator
    :if (< 24 emacs-major-version)
    :demand t
    :config
    (define-key json-mode-map "\C-c\C-f" #'json-navigator-navigate-after-point)
    (define-key json-mode-map "\C-c\C-n" #'json-navigator-navigate-region))
  (use-package json-pointer)
  (use-package jq-format)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; logview-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package logview
  :mode ("\\.log$" . logview-mode)
  :bind ("C-c xt" . logview-enter-tail-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lua-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init
  (setq lua-indent-string-contents t)
  (add-hook 'lua-mode-hook
            (lambda()
              (setq-local company-smart-backend 'company-lua)
              )))

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
  :mode ("\\.py[iw]?$" . python-mode)
  :interpreter ("python" . python-mode)
  :init
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
              ;; fine-tune company
              (make-local-variable 'company-backends)
              (setq company-backends
                    (list
                     (cons 'company-jedi
                           (copy-tree
                            (car company-backends)))))
              (setq-local company-smart-backend 'company-jedi)))
  :config
  (use-package pipenv
    :config
    (add-hook 'python-mode-hook 'pipenv-mode))
  (use-package pyvenv)
  ;; add jedi if installed
  (when (eq 0 (call-process "python" nil nil nil "-c" "import jedi"))
    (require 'jedi-core))
  (define-key python-mode-map "\C-j" 'newline-and-indent)
  ;; remap 'python-shell-send-buffer
  (define-key python-mode-map "\C-c\C-c" nil)
  (define-key python-mode-map "\C-c\C-cc" #'python-shell-send-buffer)
  (define-key python-mode-map [?\C-\M-g] 'python-nav-forward-sexp)
  (define-key python-mode-map (kbd "\C-c RET")
    (lambda()(interactive)
      (compile (concat "python " (buffer-file-name)))))
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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rust-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode :mode "\\.rs$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sphinx-doc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sphinx-doc
  :after python
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (sphinx-doc-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; python-switch-quotes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package python-switch-quotes
  :after python
  :config
  (define-key python-mode-map "\C-c'" #'python-switch-quotes))

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
              ;; set completion
              (make-local-variable 'company-backends)
              (setq company-backends
                    (list
                     (cons 'company-shell
                           (copy-tree
                            (car company-backends)))))
              (setq-local company-smart-backend 'company-shell)
              ;; (add-hook 'completion-at-point-functions 'harmsway-company-at-point nil t)
              ))
  :config
  (setq sh-basic-offset 4)
  (define-key sh-mode-map "\r" 'reindent-then-newline-and-indent)
  ;; remap 'sh-case
  (define-key sh-mode-map "\C-c\C-c" nil)
  (define-key sh-mode-map "\C-c\C-cc" 'sh-case)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shell-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'shell-mode-hook
          (lambda ()
            ;; set completion
            (make-local-variable 'company-backends)
            (setq company-backends
                  (list
                   (cons 'company-shell
                         (copy-tree
                          (car company-backends)))))
            (setq-local company-smart-backend 'company-shell)
            ;; (add-hook 'completion-at-point-functions 'harmsway-company-at-point nil t)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sln-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sln-mode
  :mode "\\.sln$"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sql-indent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sql-indent
  :init
  (add-hook 'sql-mode-hook #'sqlind-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; strace-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package strace-mode :mode "\\.strace$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; text-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook #'good-word/init-word-processor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; web ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web
  :commands (web-http-call web-http-get web-http-post
                           web-json-post web-get)
  :config
  (add-to-list 'company-dabbrev-code-modes 'web-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (setq company-backends
                    (list
                     (cons 'company-web-html
                           (copy-tree
                            (car company-backends))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'nxml-mode-hook
          (lambda()
            (setq-default indent-tabs-mode nil)
            ;; (idle-highlight-mode 1)
            (define-key nxml-mode-map "\r" 'reindent-then-newline-and-indent)
            (make-local-variable 'electric-pair-pairs)
            (push (cons ?< ?>) electric-pair-pairs)
            (make-local-variable 'company-backends)
            (setq company-backends
                  (list
                   (cons 'company-nxml
                         (copy-tree
                          (car company-backends)))))
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
