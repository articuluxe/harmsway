;; -*- Mode: Emacs-Lisp -*-
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dan Harms init.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; load-path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my/user-directory (expand-file-name
                             (if (boundp 'user-emacs-directory)
                                 user-emacs-directory
                               "~/.emacs.d/")))
(defconst my/plugins-directory (concat my/user-directory "plugins/"))
(add-to-list 'load-path my/plugins-directory)
(add-to-list 'load-path (concat my/user-directory "modes/"))
(add-to-list 'load-path (concat my/user-directory "custom/"))
(defconst my/user-settings
  (concat my/user-directory "settings/user/" user-login-name))
(load my/user-settings)

(set-register ?\C-i (cons 'file user-init-file)) ;edit init file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-save ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my/autosave-dir (concat my/user-directory "autosaves/"))
(unless (file-directory-p my/autosave-dir)
  (make-directory my/autosave-dir t))
(setq auto-save-file-name-transforms
      `((".*" ,(concat my/autosave-dir "\\1") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my/backup-dir
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

; Suppress GNU startup message
(setq inhibit-startup-message t)
(setq inhibit-default-init t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq use-dialog-box nil)
(setq large-file-warning-threshold nil)
(setq kill-do-not-save-duplicates t)
(file-name-shadow-mode 1)
(setq enable-recursive-minibuffers t)
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
(size-indication-mode 1)
; don't add new-lines to end of buffer on scroll
(setq next-line-add-newlines nil)
;; allow converting regions to upper/lower case
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
;; disable nuisances
(put 'overwrite-mode 'disabled t)
(fset 'yes-or-no-p 'y-or-n-p)
;; reuse frames
(setq-default display-buffer-reuse-frames t)

;; visual settings
(menu-bar-mode -1)
(setq-default fill-column 78)
; default colors
(set-foreground-color "white")
(set-background-color "black")
(set-cursor-color "yellow")
(set-mouse-color "white")
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(mouse-avoidance-mode 'cat-and-mouse)
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(setq ring-bell-function (lambda() ()))
(when (display-graphic-p)
  (global-unset-key "\C-z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; key-bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "<f1>"))
(global-set-key [(next)] 'scroll-up-line)
(global-set-key [(prior)] 'scroll-down-line)
(global-set-key "\M-r" 'revert-buffer)
(global-set-key "\M-]" 'jump-to-match-paren)
(global-set-key "\C-ca" 'align)
(global-set-key "\C-cr" 'align-repeat-regexp)
(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key "\C-c " 'global-whitespace-mode)
(global-set-key "\C-cf" 'font-lock-fontify-buffer)
(global-set-key "\e\es" 'speedbar)
(global-set-key "\e\eo" 'speedbar-get-focus)
(global-set-key "\C-cv" 'ff-find-other-file)
(global-set-key (kbd "M-#") 'sort-lines)
(global-set-key (kbd "C-#") 'sort-paragraphs)
(global-set-key "\C-xw" 'write-region)

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
(load-library "compiling")
(load-library "coding")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'log-viewer)
(require 'cmake-mode)
;; work around bug in cc-mode in emacs 24.4
;; see debbugs.gnu.org/db/18/18845.html
(eval-when-compile
  (if (and (= emacs-major-version 24) (= emacs-minor-version 4))
      (require 'cl)))
(require 'protobuf-mode)
(require 'dos)
(require 'etags-select)
(require 'folio-mode)
(require 'folio-electric)
(require 'pos-tip)
(require 'qt-pro)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; copyright ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'copyright)
;; copyright-update is added to my/before-save-hook below
(setq copyright-query nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'autoinsert)

;; (setq auto-insert-directory (concat my/user-directory "templates/"))
;; (define-auto-insert "\.el" "my-emacs-lisp-template.el")
;; (setq auto-insert-query nil)
;; (setq auto-insert 'other)
;; (setq auto-insert-directory (concat my/user-directory "autoinsert/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hide-lines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'hide-lines)
(global-set-key "\C-c\\" 'hide-lines)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; line-comment-banner ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'line-comment-banner)
(global-set-key [?\C-\;] 'line-comment-banner)
(add-hook 'c-mode-common-hook
          (lambda() (make-local-variable 'comment-fill)
            (setq comment-fill "*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; list-register ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'list-register)
(global-set-key (kbd "\C-xrv") 'list-register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; popwin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'popwin)
(popwin-mode 1)
(global-set-key [f1] popwin:keymap)
(setq popwin:special-display-config
      '(
        help-mode
;        (compilation-mode :noselect t)
        (completion-list-mode :noselect t)            ;noselect?
        ("*Ido Completions*" :noselect t)
        (grep-mode :noselect t)
        (occur-mode :noselect t)
        "*Shell Command Output*"
        ("^\\*terminal<[[:digit:]]>\\*" :regexp t)
        ))
;; (push '(" *undo-tree*" :width 0.5 :position right)
;;       popwin:special-display-config)
;; (push '("*undo-tree Diff*" :height 0.5 :position bottom)
;;       popwin:special-display-config)
;; use popwin for term buffers
(require 'popwin-term)
(push '(term-mode :position :top :height 0.25 :stick t)
      popwin:special-display-config)
(define-key popwin:keymap "t" 'popwin-term:term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rich-minority ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rich-minority)
(rich-minority-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smart-mode-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat my/plugins-directory "smart-mode-line/"))
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; undo-tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)
(global-undo-tree-mode)
;; reset the undo tree history (useful after reverting buffer)
(global-set-key "\C-cu" (lambda()(interactive)(setq buffer-undo-tree nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tramp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")
(setq tramp-default-user my/user-name)
(defvar my/tramp-file-list '())
(defun my/open-tramp-file() (interactive)
  (find-file (ido-completing-read "Remote file: " my/tramp-file-list)))
(global-set-key "\C-c\C-t" 'my/open-tramp-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-x)                      ; C-x C-j now runs 'dired-jump
(require 'dired-details+)
(require 'dired-sort)
(setq-default dired-listing-switches "-alhvGg")
(setq dired-details-initially-hide nil)
(put 'dired-find-alternate-file 'disabled nil)

(defun my/dired-sort()
  "Toggle sorting in dired buffers."
  (interactive)
  (let ((type (ido-completing-read
               "Sort by: "
               '( "extension" "ctime" "utime" "time" "name")
               nil t)))
    ;; on os x, extension (X) not supported;
    ;; also, ctime means time file status was last changed
    (cond ((string= type "extension") (dired-sort-extension))
          ((string= type "ctime") (dired-sort-ctime))
          ((string= type "utime") (dired-sort-utime))
          ((string= type "time") (dired-sort-time))
          ((string= type "name") (dired-sort-name))
          (t (error "unknown dired sort %s" type)))))
;; TODO: choose a key for this

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
  "Run command on marked files. Any files not already open will be opened.
 After this command has been run, any buffers it's modified will remain
 open and unsaved."
  (interactive "Run on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; diff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; shebang ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'shebang)

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
  "Tile frames vertically. You can restore prior frame position via going to register \\C-l."
  (interactive)
  (save-frame-config)
  (tile-frames-vertically))
(global-set-key "\e\ev" 'my/tile-frames-vertically)
(defun my/tile-frames-horizontally()
  "Tile frames horizontally. You can restore prior frame position via going to register \\C-l."
  (interactive)
  (save-frame-config)
  (tile-frames-horizontally))
(global-set-key "\e\eh" 'my/tile-frames-horizontally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; color-theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat my/plugins-directory "color-theme/"))
(require 'color-theme)
(color-theme-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; smerge ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun try-smerge()
  (save-excursion
    (goto-char (point-min))
    (smerge-mode
     (if (re-search-forward "^<<<<<<<" nil t) 1 0))))
(add-hook 'find-file-hook 'try-smerge t)
(add-hook 'after-save-hook (lambda() (if (smerge-mode) (try-smerge))))
(setq smerge-command-prefix "\e")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multi-term ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'multi-term)
(setq multi-term-program "/bin/tcsh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; profiles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'profiles+)
(profile-define "default" "dharms" "danielrharms@gmail.com"
                ;; relative path to makefiles
                'build-sub-dir "build/"
                ;; relative path to source file root
                'src-sub-dir "src/"
                ;; relative path to debug executables (under project-root and
                ;; build-sub-dir)
                'debug-sub-dir "tests/"
                ;; specific compiler invocation command
                'compile-sub-command "make"
                )
(profile-set-default "default")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat my/plugins-directory "auto-complete/"))
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories
             (concat my/plugins-directory "auto-complete/dict/"))
(mapc (lambda(mode)
        (add-to-list 'ac-modes mode))
      '(sql-mode nxml-mode cmake-mode folio-mode protobuf-mode
                 python-mode dos-mode gud-mode))
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
(require 'auto-complete-etags)
(require 'auto-complete-nxml)
(require 'auto-complete-c-headers)
(setq achead:include-patterns (list
                               "\\.\\(h\\|hpp\\|hh\\|hxx\\|H\\)$"
                               "/[a-zA-Z-_]+$"
                               ))
(setq achead:include-directories '("."))

(add-hook 'c-mode-common-hook
          '(lambda()
             (set (make-local-variable 'ac-auto-start) nil)
             ;; we'll define a special key event for yasnippet
             (setq ac-sources (remove 'ac-source-yasnippet ac-sources))
             (setq ac-sources (remove 'ac-source-gtags ac-sources))
             (add-to-list 'ac-sources 'ac-source-etags)
             (add-to-list 'ac-sources 'ac-source-c-headers)
             ) t)                       ;append to hook list to take effect
                                        ;after ac-config-default
(add-hook 'protobuf-mode-hook
          '(lambda()
             (setq ac-sources (add-to-list 'ac-sources 'ac-source-etags))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; YASnippet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat my/plugins-directory "yasnippet/"))
(require 'yasnippet)
(add-to-list 'safe-local-variable-values '(require-final-newline . nil))
(yas-global-mode 1)
(setq yas-snippet-dirs (concat my/user-directory "snippets/"))
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
;; integrate with auto-complete
(defun my/expand-yasnippet() (interactive)
  (auto-complete '(ac-source-yasnippet)))
(global-set-key [backtab] 'my/expand-yasnippet)
(global-set-key [(shift tab)] 'my/expand-yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; popup-kill-ring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'popup-kill-ring)
(global-set-key "\C-\M-y" 'popup-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; speedbar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'speedbar-mode-hook
          '(lambda()
             (when (display-graphic-p)
               (setq-default gdb-speedbar-auto-raise t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; htmlize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'htmlize)

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
      (load-environment-variable-from-file "PATH" path-file))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; host ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((host-dir
        (file-name-as-directory
         (concat my/user-directory "settings/host/" system-name)))
	   (host-file (concat host-dir system-name)))
  ;; load host file (if present)
  (load host-file t)
  ;; also load any profiles
  (mapc (lambda (file)
          (load-file file))
        (directory-files host-dir t "\\.profile$")))


(add-hook 'before-save-hook 'my/before-save-hook)
(defun my/before-save-hook() "Presave hook"
  (when (memq major-mode '(c++-mode emacs-lisp-mode perl-mode
                                    java-mode python-mode dos-mode
                                    nxml-mode protobuf-mode folio-mode
                                    sh-mode))
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
             ("CMakeLists\\.txt$"   . cmake-mode)
             ("\\.cmake$"    . cmake-mode)
             ("\\.proto$"    . protobuf-mode)
             ("\\.folio$"    . folio-mode)
             ("\\.bat$"      . dos-mode)
             ("\\.log$"      . log-viewer-mode)
             ("\\.pro$"      . qt-pro-mode)
            )
     auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sh-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'sh-mode-hook
          '(lambda()
             (setq-default indent-tabs-mode nil)
             (define-key sh-mode-map "\r" 'reindent-then-newline-and-indent)
             (setq sh-basic-offset 3)
             (setq sh-indentation 3)
             (define-key sh-mode-map "\C-c\C-c" 'comment-region)
             (define-key sh-mode-map "\C-c\C-u" 'uncomment-region)
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dos-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'dos-mode-hook
          '(lambda()
             (setq-default indent-tabs-mode nil)
             (define-key dos-mode-map "\r" 'reindent-then-newline-and-indent)
             (setq dos-basic-offset 3)
             (setq dos-indentation 3)
             (define-key dos-mode-map "\C-c\C-c" 'comment-region)
             (define-key dos-mode-map "\C-c\C-u" 'uncomment-region)
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xml-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook
          '(lambda()
             (setq-default indent-tabs-mode nil)
             (define-key python-mode-map "\r" 'reindent-then-newline-and-indent)
             (define-key python-mode-map "\C-c\C-c" 'comment-region)
             (define-key python-mode-map "\C-c\C-u" 'uncomment-region)
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs-lisp-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
          '(lambda()
             (setq indent-tabs-mode nil)
             ;; conf-colon-mode still bound to "\C-c:"
             (local-unset-key "\C-c\C-c")
             ;; conf-unix-mode now bound to "\C-cu"
             (local-unset-key "\C-c\C-u")
             (define-key conf-mode-map "\C-cu" 'conf-unix-mode)
             (define-key conf-mode-map "\C-c\C-c" 'comment-region)
             (define-key conf-mode-map "\C-c\C-u" 'uncomment-region)
             ))
