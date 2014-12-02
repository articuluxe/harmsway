;; -*- Mode: Emacs-Lisp -*-
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Dan Harms init.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; load-path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my/user-directory (expand-file-name user-emacs-directory))
(defconst my/plugins-directory (concat my/user-directory "plugins/"))
(add-to-list 'load-path my/user-directory)
(add-to-list 'load-path my/plugins-directory)
(add-to-list 'load-path (concat my/user-directory "modes/"))
(add-to-list 'load-path (concat my/user-directory "custom/"))
(add-to-list 'load-path (concat my/plugins-directory "auto-complete/"))
(add-to-list 'load-path (concat my/plugins-directory "yasnippet/"))
(defconst my/user-settings
  (concat my/user-directory "settings/users/" user-login-name))
(load my/user-settings)

(set-register ?i (cons 'file user-init-file)) ;edit init file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-save ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my/autosave-dir (concat my/user-directory "autosaves/"))
(unless (file-directory-p my/autosave-dir)
  (make-directory my/autosave-dir t))
(setq auto-save-file-name-transforms
      `((".*" ,(concat my/autosave-dir "\\1") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my/backup-dir (concat my/user-directory "backups/"
  (format-time-string "%Y-%m-%d")))
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

; Suppress GNU startup message
(setq inhibit-startup-message t)
(setq inhibit-default-init t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq use-dialog-box nil)
(setq large-file-warning-threshold nil)
(setq kill-do-not-save-duplicates t)
; truncate long lines
(setq-default truncate-lines t)
; search is case-sensitive by default
(setq-default case-fold-search nil)
;; interactive regexp-search space character stands only for 1 char
(setq-default search-whitespace-regexp nil)
; default tab width
(setq-default tab-width 4)
; Show selections
(transient-mark-mode 1)
;; show current function
(which-function-mode t)
; Insertion while text is selected deletes the selected text
(delete-selection-mode 1)
;; winner mode
(winner-mode 1)
; append unique parent directory to buffers of same name
(toggle-uniquify-buffer-names)
; Preserve line position on scroll
(setq scroll-preserve-screen-position t)
; Ignore case when completing file names
(setq read-file-name-completion-ignore-case nil)
(show-paren-mode t)
; don't add new-lines to end of buffer on scroll
(setq next-line-add-newlines nil)
; reuse frames
(setq-default display-buffer-reuse-frames t)
;; allow converting regions to upper/lower case
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; disable nuisances
(put 'overwrite-mode 'disabled t)
(fset 'yes-or-no-p 'y-or-n-p)
;; reuse frames
(setq-default display-buffer-reuse-frames t)

;; visual settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
; default colors
(set-foreground-color "white")
(set-background-color "black")
(set-cursor-color "yellow")
(set-mouse-color "white")
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(mouse-avoidance-mode 'cat-and-mouse)
(blink-cursor-mode 1)
(when (display-graphic-p)
  (global-unset-key "\C-z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; key-bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "<f1>"))
(global-set-key "\M-r" 'revert-buffer)
(global-set-key "\M-]" 'jump-to-match-paren)
(global-set-key "\C-ca" 'align)
(global-set-key "\C-cr" 'align-repeat-regexp)
(global-set-key "\C-cw" 'wordcount)
(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key "\C-c " 'global-whitespace-mode)
(global-set-key "\C-cf" 'font-lock-fontify-buffer)
(global-set-key "\e\es" 'speedbar)
(global-set-key "\e\eo" 'speedbar-get-focus)
(global-set-key "\C-cv" 'ff-find-other-file)

;; (global-set-key [f7] 'select-previous-window)
;; (global-set-key [f8] 'select-next-window)
;(global-set-key [f9] 'previous-error)
;(global-set-key [f10] 'next-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tags-revert-without-query t)
(global-set-key "\C-ct" 'find-my-tags-file)
(global-set-key "\M-." 'etags-select-find-tag)
(global-set-key [?\C-\M-.] 'etags-select-find-tag-at-point)

;;;;;;; FUNCTIONS ;;;;;;;
;; (defun select-next-window () "Switch to next window"
;;   (interactive)
;;   (select-window (next-window)))
;; (defun select-previous-window () "Switch to previous window"
;;   (interactive)
;;   (select-window (previous-window)))
;; ; Jump to matching parentheses
;; (defun jump-to-match-paren () "goto matching paren" (interactive)
;;    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;      ((looking-at "\\s\)") (forward-char 1) (forward-list -1))))
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


(require 'log-viewer)
(require 'cmake-mode)
(require 'protobuf-mode)
(require 'dos)
(require 'etags-select)
(require 'folio-mode)
(require 'folio-electric)
(require 'pos-tip)

;;;;;;;;;;;;;;;;;;;;;;;;;; line-comment-banner ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'line-comment-banner)
(global-set-key [?\C-\;] 'line-comment-banner)
(add-hook 'c-mode-common-hook
          (lambda() (make-local-variable 'comment-fill)
            (setq comment-fill "*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ido-mode 1)
(setq ido-max-prospects 25)
;; (setq ido-enable-flex-matching t)
;; (setq ido-case-fold t)
;; (ido-auto-merge-work-directories-length -1) ;would disable auto-merge
(add-to-list 'ido-work-directory-list-ignore-regexps tramp-file-name-regexp)
;; ask before reusing an existing buffer
(setq-default ido-default-buffer-method 'maybe-frame)
(setq-default ido-default-file-method 'maybe-frame)
;; sort files by descending modified time
(defun ido-sort-mtime()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (time-less-p
                 (sixth (file-attributes (concat ido-current-directory b)))
                 (sixth (file-attributes (concat ido-current-directory a)))))))
  (ido-to-end
   (delq nil (mapcar (lambda (x)
                       (and (char-equal (string-to-char x) ?.) x))
                     ido-temp-list))))
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
;; also use ido to switch modes
(global-set-key "\e\ex"
                (lambda() (interactive)
                  (call-interactively
                   (intern
                    (ido-completing-read
                     "M-x " (all-completions "" obarray 'commandp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; undo-tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key "\C-cu" (lambda()(interactive)(setq buffer-undo-tree nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tramp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")
(setq tramp-default-user my/user-name)
;;"dharms")
(defvar my/tramp-file-list nil)
(defun my/open-tramp-file() (interactive)
  (find-file (ido-completing-read "Remote file: " my/tramp-file-list)))
(add-to-list 'my/tramp-file-list '(
                                   "/ssh:dharms@chl-ls-rex01:~/"
                                   ))
(global-set-key "\C-c\C-t" 'my/open-tramp-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-x)                      ; C-x C-j now runs 'dired-jump
(require 'dired-details)
(setq-default dired-listing-switches "-alhvGg")
(setq dired-details-initially-hide nil)
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
     )
   nil (dired-get-marked-files t current-prefix-arg)))

(add-hook 'dired-load-hook
          '(lambda()
             (define-key dired-mode-map (kbd "<prior>") 'dired-up-directory)
             (define-key dired-mode-map "l" 'dired-launch-command)
             ))

;; easily go to top or bottom
(defun dired-back-to-top() (interactive)
  (goto-char (point-min))
  (dired-next-line 4))
(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
(defun dired-jump-to-bottom() (interactive)
  (goto-char (point-max))
  (dired-next-line -1))
(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(defun my-dired-do-command (command)
  "Run command on marked files. Any files not alreay open will be opened.
 After this command has been run, any buffers it's modified will remain
 open and unsaved."
  (interactive "Run on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; diff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                               ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shebang ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'shebang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; framemove ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'windmove)
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)
;;(setq windmove-wrap-around t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smerge ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun try-smerge()
  (save-excursion
    (goto-char (point-min))
    (smerge-mode
     (if (re-search-forward "^<<<<<<<" nil t) 1 0))))
(add-hook 'find-file-hook 'try-smerge t)
(add-hook 'after-save-hook (lambda() (if (smerge-mode) (try-smerge))))
(setq smerge-command-prefix "\e")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multi-term ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'multi-term)
(setq multi-term-program "/bin/tcsh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories
             (concat user-emacs-directory "plugins/auto-complete/dict"))
(mapc (lambda(mode)
        (add-to-list 'ac-modes mode))
      '(sql-mode nxml-mode cmake-mode folio-mode protobuf-mode
                 python-mode dos-mode gud-mode))
(require 'auto-complete-config)
(ac-config-default)
(setq-default ac-sources (append '(ac-source-filename) ac-sources))
(setq ac-use-menu-map t)
(setq ac-auto-start t)
;; TODO: unix vs windows
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key ac-mode-map (kbd "<lwindow> TAB") 'auto-complete)
(define-key ac-mode-map (kbd "\C-c TAB") (lambda()(interactive)
                                           (setq ac-auto-start
                                                 (null ac-auto-start))))
(setq ac-menu-height 20)
(require 'auto-complete-etags)
(require 'auto-complete-nxml)
(add-hook 'c-mode-common-hook
          '(lambda()
             (set (make-local-variable 'ac-auto-start) nil)
             ;; we'll define a special key event for yasnippet
             (setq ac-sources (remove 'ac-source-yasnippet ac-sources))
             (setq ac-sources (remove 'ac-source-gtags ac-sources))
             (setq ac-sources (add-to-list 'ac-sources 'ac-source-etags))
             ) t)                       ;append to hook list to take effect
                                        ;after ac-config-default
(add-hook 'protobuf-mode-hook
          '(lambda()
             (setq ac-sources (add-to-list 'ac-sources 'ac-source-etags))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; YASnippet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs (concat my/user-directory "snippets/"))
(setq yas-prompt-functions '(
                             yas-x-prompt
                             yas-ido-prompt
                             ;; yas-completing-prompt
                             ;; yas-no-prompt
                             ))
;; disable TAB keys
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-keymap [(tab)] nil)
;; (define-key yas-keymap (kbd "TAB") nil)
;; (define-key yas-keymap [(shift tab)] nil)
;; (define-key yas-keymap [backtab] nil)
;; add our own keybindings
(define-key yas-minor-mode-map (kbd "\C-cse") 'yas-expand)
(define-key yas-minor-mode-map (kbd "\C-csn") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "\C-csv") 'yas-visit-snippet-file)
(define-key yas-minor-mode-map "\C-csi" 'yas-insert-snippet)
;; integrate with auto-complete
(defun my/expand-yasnippet() (interactive)
  (auto-complete '(ac-source-yasnippet)))
(global-set-key [backtab] 'my/expand-yasnippet)
(global-set-key [(shift tab)] 'my/expand-yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; popup-kill-ring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'popup-kill-ring)
(global-set-key "\C-\M-y" 'popup-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; os ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      (load-environment-variable-from-file "PATH" path-file))
  (if (file-exists-p include-file)
      (load-environment-variable-from-file "INCLUDE" include-file))
  (if (file-exists-p lib-file)
      (load-environment-variable-from-file "LIB" lib-file))
  (if (file-exists-p libpath-file)
      (load-environment-variable-from-file "LIBPATH" libpath-file))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((gui-dir (concat my/user-directory "settings/gui/"))
      (gui (symbol-name window-system))
      gui-file)
  ;; load gui file
  (setq gui-file (concat gui-dir (if (null gui) "tty" gui)))
  (load gui-file)
  )



(add-hook 'before-save-hook 'do-before-save-hook)
(defun do-before-save-hook() "Presave hook"
  (when (memq major-mode '(c++-mode emacs-lisp-mode perl-mode
                                    java-mode python-mode dos-mode
                                    nxml-mode protobuf-mode folio-mode
                                    sh-mode))
    (delete-trailing-whitespace)
))

;;;;;;; MODES ;;;;;;;
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
             ("CMakeLists\\.txt$"   . cmake-mode)
             ("\\.cmake$"    . cmake-mode)
             ("\\.proto$"    . protobuf-mode)
             ("\\.folio$"    . folio-mode)
             ("\\.bat$"      . dos-mode)
             ("\\.log$"      . log-viewer-mode)
            )
     auto-mode-alist
   )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sh-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'sh-mode-hook
          '(lambda()
             (setq-default indent-tabs-mode nil)
             (define-key sh-mode-map "\r" 'reindent-then-newline-and-indent)
             (setq sh-basic-offset 3)
             (setq sh-indentation 3)
             (define-key sh-mode-map "\C-c\C-c" 'comment-region)
             (define-key sh-mode-map "\C-c\C-u" 'uncomment-region)
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'nxml-mode-hook
          '(lambda()
             (setq-default indent-tabs-mode nil)
             (define-key nxml-mode-map "\r" 'reindent-then-newline-and-indent)
             (define-key nxml-mode-map "\C-c\C-c" 'comment-region)
             (define-key nxml-mode-map "\C-c\C-u" 'uncomment-region)
             ))
(require 'mz-comment-fix)
;; the following is a hack to fix nested XML commenting in Emacs 24.
;; Note that 'comment-strip-start-length also exists for other modes if needed.
(add-to-list 'comment-strip-start-length (cons 'nxml-mode 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook
          '(lambda()
             (setq-default indent-tabs-mode nil)
             (define-key python-mode-map "\r" 'reindent-then-newline-and-indent)
             (define-key python-mode-map "\C-c\C-c" 'comment-region)
             (define-key python-mode-map "\C-c\C-u" 'uncomment-region)
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs-lisp-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (setq indent-tabs-mode nil)
             (define-key emacs-lisp-mode-map "\r" 'reindent-then-newline-and-indent)
             (define-key emacs-lisp-mode-map "\C-c\C-c" 'comment-region)
             (define-key emacs-lisp-mode-map "\C-c\C-u" 'uncomment-region)
             (define-key emacs-lisp-mode-map (kbd "\C-c RET")
               '(lambda()(interactive)
                  (byte-compile-file (buffer-file-name))))
             ))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'after-save-hook (lambda()
                             (if (eq major-mode 'emacs-lisp-mode)
                                 (save-excursion
                                   (byte-compile-file buffer-file-name)))))


(load-library "compiling")
(load-library "coding")
