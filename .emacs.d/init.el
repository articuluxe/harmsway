;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Dan Harms ~/.emacs ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq how-many-windows nil)
;(setq how-many-windows 'one-window-p)
; Suppress GNU startup message
(setq inhibit-startup-message t)
(setq line-number-mode t)
(setq column-number-mode t)
(menu-bar-mode -1)
; truncate long lines
(setq-default truncate-lines t)
; search is case-sensitive by default
(setq-default case-fold-search nil)
; default tab width
(setq-default tab-width 4)
; Show selections
(transient-mark-mode 1)
; Highlight shared selections in other (non-active) buffer windows 
(setq highlight-nonselected-windows t) 
; Insertion while text is selected deletes the selected text 
(delete-selection-mode 1) 
; blink the cursor 
(blink-cursor-mode 1) 
; append unique parent directory to buffers of same name
(toggle-uniquify-buffer-names)
; Preserve line position on scroll
(setq scroll-preserve-screen-position t) 
; Ignore case when auto-completing file names 
(setq read-file-name-completion-ignore-case nil) (show-paren-mode t) 
; don't add new-lines to end of buffer on scroll
(setq next-line-add-newlines nil)
; automatically scroll compilation window
(setq compilation-scroll-output t)
; reuse frames
(setq-default display-buffer-reuse-frames t)
; allow converting regions to upper/lower case 
(put 'upcase-region 'disabled nil) 
(put 'downcase-region 'disabled nil) 
(fset 'yes-or-no-p 'y-or-n-p)
(setq font-lock-maximum-decoration t)
; default colors
(set-foreground-color "white")
(set-background-color "black")
(set-cursor-color "yellow")
(set-mouse-color "white")
(global-font-lock-mode t)

;;;;;;; KEY BINDINGS ;;;;;;;
(global-set-key "\M-r" 'revert-buffer)
(global-set-key "\M-]" 'jump-to-match-paren) 
(global-set-key "\C-ca" 'align) 
(global-set-key "\C-cr" 'align-regexp) 
(global-set-key "\C-cw" 'wordcount) 
(global-set-key [f5] 'toggle-truncate-lines) 
(global-set-key "\C-c " 'global-whitespace-mode)
(global-set-key "\C-cq" 'indent-buffer) 
(global-set-key "\C-cf" 'font-lock-fontify-buffer) 
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
;(global-set-key "\C-c%" 'replace-regexp)
(global-set-key "\C-cp" 'print-current-function)
(global-set-key "\C-cv" 'ff-find-other-file)
(global-set-key [f7] 'select-previous-window)
(global-set-key [f8] 'select-next-window)
;(global-set-key [f9] 'previous-error)
;(global-set-key [f10] 'next-error)
(global-set-key "\C-ci" 'add-header-include-ifdefs)
(global-set-key "\C-cn" 'wrap-namespace-region)
(global-set-key "\C-ch" 'insert-class-header)
(global-set-key "\C-cc" 'insert-cast)

(global-set-key "\e\ep" ‘jump-to-matching-paren)
(global-set-key "\M-]" ‘highlight-paren-right)
(global-set-key "\M-[" ‘highlight-paren-left)
(global-set-key "\M-p" ‘highlight-enclosing-paren)
(global-set-key "\e\ei" ‘call-clean-up-func-declaration-and-indent)
(global-set-key "\e\ed" ‘call-clean-up-func-declaration)
(global-set-key "\e\e(" (lambda()(interactive)(enclose-by-braces ?( ?) )))
(global-set-key "\e\e[" (lambda()(interactive)(enclose-by-braces ?[ ?] )))
(global-set-key "\e\e{" (lambda()(interactive)(enclose-by-braces ?{ ?} )))
(global-set-key "\e\e<" (lambda()(interactive)(enclose-by-braces ?< ?> )))
(global-set-key "\e\er" ‘highlight-current-sexp)


;; TAGS ;;
(global-set-key "\C-ctf" 'find-my-tags-file)
(global-set-key "\C-ct." 'etags-select-find-tag)
(global-set-key "\C-ct?" 'etags-select-find-tag-at-point)

;;;;;;; FUNCTIONS ;;;;;;;
(defun select-next-window () "Switch to next window"
  (interactive)
  (select-window (next-window)))
(defun select-previous-window () "Switch to previous window"
  (interactive)
  (select-window (previous-window)))
; Jump to matching parentheses
(defun jump-to-match-paren () "goto matching paren" (interactive)
   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
     ((looking-at "\\s\)") (forward-char 1) (forward-list -1)))) 
; man-page lookups 
(defun openman () "lookup man page" (interactive)
   (manual-entry (current-word)))
; indent entire file
(defun indent-buffer () "indent entire buffer" (interactive)
   (indent-region (point-min) (point-max) nil)) 
; convert tabs to spaces 
(defun untabify-buffer () "untabify current buffer" (interactive)
   (save-excursion
     (untabify (point-min) (point-max)))) 
; word count 
(defun wordcount () "print buffer word count in minibuffer" (interactive)
   (save-excursion
     (let ((count 0))
       (goto-char (point-min))
       (while (< (point) (point-max))
         (forward-word 1)
         (setq count (1+ count)))
       (message "buffer contains %d words" count))))
; shell command on region
(defun shell-command-on-buffer (cmd) (interactive "sShell command on buffer: ")
  (shell-command-on-region (point-min) (point-max) cmd t t
))
(defun print-current-function() "Print current function under point."
(interactive)
(message (which-function))
)

; Jump to matching parentheses
(defun jump-to-matching-paren() "Go to matching paren" (interactive)
  (if (looking-at "\\s\(")
	  (forward-list 1)
	(backward-char)
	(if (looking-at "\\s\)")
		(prong
		 (forward-char 1)
		 (forward-list -1))
	  (forward-char 1))))
(defun highlight-paren-right()
  "search forward for a parenthesized sexp and set region if found"
   (interactive)
   (let ((pos (search-forward-regexp "\\s\(" nil t)))
	 (when pos
	   (set-mark-command nil)
	   (backward-char)
	   (forward-list 1)
	   (backward-char)
	   (setq deactivate-mark nil))))
(defun highlight-paren-left()
  "search backward for a parenthesized sexp and set region if found"
   (interactive)
   (let ((pos (search-backward-regexp "\\s\)" nil t)))
	 (when pos
	   (set-mark-command nil)
	   (forward-char)
	   (forward-list -1)
	   (forward-char)
	   (setq deactivate-mark nil))))
(defun highlight-enclosing-paren(&optional arg)
  "assume point is bounded by paren and set region to that exp"
   (interactive "P")
   (if arg
	   (let ((pos (search-forward-regexp "\\s\)" nil t)))
		 (when pos
		   (backward-char)
		   (set-mark-command nil)
		   (forward-char)
		   (forward-list -1)
		   (forward-char)
		   (setq deactivate-mark nil))
		 )
	 (let ((pos (search-backward-regexp "\\s\(" nil t)))
	   (when pos
		 (forward-char)
		 (set-mark-command nil)
		 (backward-char)
		 (forward-list 1)
		 (backward-char)
		 (setq deactivate-mark nil))
	   )))

(defun enclose-by-braces (left right) "insert braces around a region or point"
  (interactive "r")
  (if (use-region-p) ; act on region
	  (let ((start (region-beginning))
			(end (region-end)))
		(save-excursion
		  (goto-char end)
		  (insert right)
		  (goto-char start)
		  (insert left)))
	(progn ; act around point
	  (insert left right)
	  (backward-char 1))))

(defun highlight-current-sexp(&optional arg) "Highlight the current sexp around point"
  (interactive "P")
  (let ((n (if arg arg 1)))
	(unless (looking-at "\\_<")
	  (backward-sexp n))
	(set-mark-command nil)
	(forward-sexp n)
	(setq deactivate-mark nil)))


;;;;;;; FILE BACKUPS ;;;;;;;
(setq backup-directory-alist
  (list (cons "." "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
	  '((".*" "~/.emacs.d/autosaves/\\1" t)))

;;;;;;; COMPILATION ;;;;;;;
(defun my-compilation-mode-hook()
  (setq truncate-lines nil) ; is buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun drh-compile() (interactive)
	   (setq one-window-in-frame (one-window-p t))
	   (call-interactively 'compile)
	   )
(defun drh-recompile() (interactive)
	   (setq one-window-in-frame (one-window-p t))
	   (call-interactively 'recompile)
	   )

(add-hook 'compilation-start-hook '(lambda (process)
; the compile is about to start
))

(setq load-path (cons (expand-file-name "~/.emacs.d/modes") load-path))
(require 'cmake-mode)
;(require 'protobuf-mode)
(require 'etags-select)
(require 'folio-mode)
(require 'folio-electric)

;;;;;;; tty ;;;;;;;
(if (eq window-system nil)
   (progn
     ; FONT LOCK CUSTOMIZATION
     ; comment
     (set-face-foreground 'font-lock-comment-face "yellow")
     (set-variable font-lock-comment-face 'font-lock-comment-face)
     ; comment-delimiter
     (set-face-foreground 'font-lock-comment-delimiter-face "yellow")
     (set-variable font-lock-comment-delimiter-face 'font-lock-comment-delimiter-face)
     ; builtin
     (set-face-foreground 'font-lock-builtin-face "blue")
     (set-variable font-lock-builtin-face 'font-lock-builtin-face)
     ; constant
     (set-face-foreground 'font-lock-constant-face "cyan")
     (set-variable font-lock-constant-face 'font-lock-constant-face)
     ; function-name
     (set-face-foreground 'font-lock-function-name-face "red")
     (set-variable font-lock-function-name-face 'font-lock-function-name-face)
     ; keyword
     (set-face-foreground 'font-lock-keyword-face "magenta")
     (set-variable font-lock-keyword-face 'font-lock-keyword-face)
     ; string
     (set-face-foreground 'font-lock-string-face "green")
     (set-variable font-lock-string-face 'font-lock-string-face)
     ; type
     (set-face-foreground 'font-lock-type-face "magenta")
     (set-variable font-lock-type-face 'font-lock-type-face)
     ; variable-name
     (set-face-foreground 'font-lock-variable-name-face "white")
     (set-variable font-lock-variable-name-face 'font-lock-variable-name-face)
     ; warning
     (set-face-foreground 'font-lock-warning-face "red")
     (set-variable font-lock-warning-face 'font-lock-warning-face)
     ;
     (setq default-frame-alist
      '(
         (cursor-color . "red")
         (cursor-type . (bar . 2))
       )
     )
   )
)

;;;;;;; mac ;;;;;;;
(if (eq window-system 'ns)
   (progn
	 (tool-bar-mode -1)
     (setq frame-title-format "%b")
     (global-unset-key [?\C-x ?\C-z])
     (setq default-fill-column 74) ; this defaults to 70
     ; FONT LOCK CUSTOMIZATION
     ; comments
     (set-face-foreground 'font-lock-comment-face "yellow")
     (set-variable font-lock-comment-face 'font-lock-comment-face)
     ; builtin
     (set-face-foreground 'font-lock-builtin-face "SlateBlue")
     (set-variable font-lock-builtin-face 'font-lock-builtin-face)
     ; constant
     (set-face-foreground 'font-lock-constant-face "chocolate")
     (set-variable font-lock-constant-face 'font-lock-constant-face)
     ; function-name
     (set-face-foreground 'font-lock-function-name-face "maroon")
     (set-variable font-lock-function-name-face 'font-lock-function-name-face)
     ; keyword
     (set-face-foreground 'font-lock-keyword-face "SteelBlue")
     (set-variable font-lock-keyword-face 'font-lock-keyword-face)
     ; string
     (set-face-foreground 'font-lock-string-face "LimeGreen")
     (set-variable font-lock-string-face 'font-lock-string-face)
     ; type
     (set-face-foreground 'font-lock-type-face "peru")
     (set-variable font-lock-type-face 'font-lock-type-face)
     ; variable-name
     (set-face-foreground 'font-lock-variable-name-face "salmon")
     (set-variable font-lock-variable-name-face 'font-lock-variable-name-face)
     ; warning
     (set-face-foreground 'font-lock-warning-face "gold")
     (set-variable font-lock-warning-face 'font-lock-warning-face)
     ;
     (setq default-frame-alist
      '(
         (cursor-color . "yellow")
         (cursor-type . (bar . 2))
         (background-color . "black")
         (foreground-color . "white")
       )
     )
     (setq initial-frame-alist
      '(
         (top . 25) (left . 50) (height . 70) (width . 80)
       )
     )
   )
)

;;;;;;; windows ;;;;;;;
(if (eq window-system 'w32)
   (progn
	 (tool-bar-mode -1)
     (setq frame-title-format "%b")
     (global-unset-key [?\C-x ?\C-z])
     (setq default-fill-column 74) ; this defaults to 70
     ; FONT LOCK CUSTOMIZATION
     ; comments
     (set-face-foreground 'font-lock-comment-face "yellow")
     (set-variable font-lock-comment-face 'font-lock-comment-face)
     ; builtin
     (set-face-foreground 'font-lock-builtin-face "SlateBlue")
     (set-variable font-lock-builtin-face 'font-lock-builtin-face)
     ; constant
     (set-face-foreground 'font-lock-constant-face "chocolate")
     (set-variable font-lock-constant-face 'font-lock-constant-face)
     ; function-name
     (set-face-foreground 'font-lock-function-name-face "maroon")
     (set-variable font-lock-function-name-face 'font-lock-function-name-face)
     ; keyword
     (set-face-foreground 'font-lock-keyword-face "SteelBlue")
     (set-variable font-lock-keyword-face 'font-lock-keyword-face)
     ; string
     (set-face-foreground 'font-lock-string-face "LimeGreen")
     (set-variable font-lock-string-face 'font-lock-string-face)
     ; type
     (set-face-foreground 'font-lock-type-face "peru")
     (set-variable font-lock-type-face 'font-lock-type-face)
     ; variable-name
     (set-face-foreground 'font-lock-variable-name-face "salmon")
     (set-variable font-lock-variable-name-face 'font-lock-variable-name-face)
     ; warning
     (set-face-foreground 'font-lock-warning-face "gold")
     (set-variable font-lock-warning-face 'font-lock-warning-face)
     ;
     (setq default-frame-alist
      '(
         (cursor-type . (bar . 2))
         (background-color . "black")
         (foreground-color . "white")
       )
     )
     (setq initial-frame-alist
      '(
         (top . 5) (left . 5) (height . 55) (width . 80)
       )
     )
   )
)

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

(add-hook 'before-save-hook 'do-before-save-hook)
(defun do-before-save-hook() "Presave hook"
  (when (eq major-mode 'c++-mode)
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
			 ("\\.folio$"    . folio-mode)
            )
     auto-mode-alist
   )
)

(defun find-file-upwards (file-to-find)
  "Recursively search upward for file; returns path to file or nil if not found."
  (let*
	  ((find-file-r (lambda (path)
					  (let* ((parent (file-name-directory path))
							 (possible-file (concat parent file-to-find)))
						(cond
						 ((file-exists-p possible-file) possible-file) ; found
						 ; parent of ~ is nil, parent of / is itself
						 ; This terminating condition accounts for both
						 ((or (null parent) (equal parent (directory-file-name parent))) nil)
						 (t (funcall find-file-r (directory-file-name parent))))))))
	(funcall find-file-r default-directory)))

(defun find-my-tags-file() "Find tags file"
  (interactive)
  (let ((my-tags-file (find-file-upwards "TAGS")))
	(if my-tags-file
		(progn
		  (message "Loading tags file: %s" my-tags-file)
		  (run-with-timer 2 nil 'visit-tags-table my-tags-file))
	  (message "Did not find tags file")
	  )))

(defun add-header-include-ifdefs (string)
  "Add header include guards"
  (interactive "sInclude guard: ")
  (let ((guard (concat "__" (upcase string) "_H__")))
	(beginning-of-buffer)
	(insert "#ifndef ")
	(insert guard)
	(insert "\n#define ")
	(insert guard)
	(insert "\n\n")
	(end-of-buffer)
	(insert "#endif    /* #ifndef ")
	(insert guard)
	(insert " */\n")
))

(defun wrap-namespace-region (start end)
  "Insert enclosing namespace brackets around a region."
  (interactive "r")
  (let ((str))
	(setq str (read-string "Enter namespace name: "))
	(save-excursion
	  (goto-char end) (insert "\n}    // end namespace " str "\n")
	  (goto-char start) (insert "namespace " str " {\n\n")
	  ))
)

(defun insert-cast (start end)
  "Insert code for a cast around a region."
  (interactive "r")
  (let (
		(initial "static")
		(type)
		)
	(setq type (read-string "Enter the data type to cast to: "))
	(setq str (read-string "Enter the type of <>_cast (static): "))
	(if (= 0 (length str))
		(setq str initial))
	(save-excursion
		  (goto-char end)(insert ")")
		  (goto-char start)(insert str "_cast<" type ">(")
		  ))
  )

(defun insert-class-header ()
  "Insert a formatted class header given the current selection or position."
  (interactive)
  (let ((str
		 (if (region-active-p)
			 (buffer-substring-no-properties (region-beginning)(region-end))
		   (thing-at-point 'symbol))
		 ))
	(if (= 0 (length str))
		(setq str (read-string "Enter the title symbol: ")))
; (message "symbol %s is %d chars long" str (length str))(read-char)
;	(move-beginning-of-line nil)
	(c-beginning-of-defun)
	(insert "//----------------------------------------------------------------------------\n")
	(insert "//---- " str " ")
	(let ((len (- 70 (length str)))(i 0))
	  (while (< i len)
		(insert "-")
		(setq i (1+ i))))
	(insert "\n//----------------------------------------------------------------------------\n")
	)
)

(defun remove-leading-whitespace (start end) "Remove a region’s leading whitespace"
  (interactive "r")
  (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char (point-min))
	  (while (looking-at "\\s-")
		(replace-match "" nil nil)))))
(defun remove-trailing-whitespace (start end) "Remove a region’s trailing whitespace"
  (interactive "r")
  (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char (point-max))
	  (backward-char)
	  (while (looking-at "\\s-")
		(replace-match "" nil nil)))))
(defun remove-embedded-newlines (start end) "Remove a region’s embedded newlines"
  (interactive "r")
  (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char (point-min))
	  (while (re-search-forward "\n" nil t)
		(replace-match "" nil nil)))))
(defun cleanup-func-parameter (start end) "clean up spacing of a single parameter decl"
  (interactive "r")
  (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char (point-min))
	  ;; remove multiple consecutive whitespace
	  (while re-search-forward "\\s-\\{2,\\}"nil t)
	  (replace-match " " nil nil))
	(goto-char (point-min))
	;; remove whitespace before punctuation or parentheses
	(while (re-search-forward "\\s-+\\(\\s.\\|\\s\(\\|\\s\)\\)" nil t)
	  (replace-match "\\1" nil nil))
	(goto-char (point-min))
	;; remove whitespace after punctuation or parentheses
	(while (re-search-forward "\\(\\s.\\|\\s\(\\|\\s\)\\)\\s-+" nil t)
	  (replace-match "\\1" nil nil))
	(goto-char (point-min))
	;; add a space if not present before parameter name
	(while (re-search-forward "\\(.*?\\)\\s-?\\(\\(?:\\sw\\|_\\)+\\)\\s-*$" nil t)
	  (replace-match "\\1 \\2" nil nil))
	)))
(defun call-clean-up-func-declaration-and-indent (start end)
  "Clean up spacing and indentation of a function declaration"
   (interactive "r")
   (clean-up-func-declaration start end t))
(defun call-clean-up-func-declaration (start end)
  "Clean up spacing of a function declaration"
   (interactive "r")
   (clean-up-func-declaration start end nil))
(defun clean-up-func-declaration (start end indent)
  "Does the actual work of cleaning up spacing and (optionally) indentation of a func decl"
   (let ((saved nil))
	 (save-excursion
	   (save-restriction
		 (narrow-to-region start end) ; separate and process each parameter
		 (goto-char (point-min))
		 (let ((start (point-min)))
		   (catch ‘break
			 (while (<= (point) (point-max))
			   (cond
				((or (looking-at ",") (eq (point) (point-max)))
				 ;; if we’re later indenting, dispose of old newlines
				 (if indent
					 (remove-embedded-newlines start (point)))
				 (cleanup-func-parameter start (point))
				 (remove-leading-whitespace start (point))
				 (remove-trailing-whitespace start (point))
				 ;; save this point to insert newline later
				 (setq saved (cons (point) saved))
				 ;; stop at region end
				 (if (eq (point) (point-max))
					 (throw ‘break nil))
				 (forward-char) ; at end of buffer this would quit
				 (insert " ")
				 (setq start (point)))
				((looking-at "\\s\(")
				 (forward-list 1))
				(t (forward-char))
				))))
		 ))
	 (if indent
		 (mapc (lambda(pos) ; points later in buffer are processed first
				 (prong
				  (goto-char pos)
				  (newline-and-indent)))
			   saved)
	   ;; if not inserting newlines (maintaining those present), we may need to re-indent
	   (indent-region start end))
	 ))
 

(add-hook 'nxml-mode-hook 
		  '(lambda()
			 (define-key nxml-mode-map "\C-c\C-c" 'comment-region)
			 (define-key nxml-mode-map "\C-c\C-u" 'uncomment-region)
			 ))

(add-hook 'sh-mode-hook
		  '(lambda()
			 (setq-default indent-tabs-mode nil)
			 (setq sh-basic-offset 3)
			 (setq sh-indentation 3)
			 (define-key sh-mode-map "\C-c\C-c" 'comment-region)
			 (define-key sh-mode-map "\C-c\C-u" 'uncomment-region)
			 ))
 
;;;;;;; C-C++-MODE ;;;;;;;
(add-hook
  'c-mode-common-hook
  '(lambda ()
     (setq-default indent-tabs-mode nil)
     (setq c-auto-newline t)
     (c-toggle-hungry-state t)
     (setq comment-column 40)
     (setq compile-command "upmake - -")
     (setq grep-command
		   "find -L `findRoot` -name TAGS -o -name '#*' -prune -o -print0 | xargs -0 grep -Isn ")
     (define-key c++-mode-map "\C-c\C-m" 'drh-compile)
     (define-key c++-mode-map "\C-cm" 'drh-recompile)
     (define-key c++-mode-map "\C-cg" 'grep)
	 (setq comment-start "/*")
	 (setq comment-end "*/")
     (c-add-style "drh"
       (quote
         ((c-basic-offset . 4)
		  (c-cleanup-list . (
							 empty-defun-braces
							 defun-close-semi
							 one-liner-defun
							 scope-operator
							 list-close-comma
							 ))
          (c-offsets-alist . (
                              (innamespace          . 0)
                              (substatement-open    . 0)
                              (statement-case-intro . +)
                              (statement-case-open  . +)
                              (statement-cont       . c-lineup-math)
                              (access-label         . -2)
                              (comment-intro        . c-lineup-comment)
							  (inline-open          . 0)
							  (member-init-intro    . +)
							  (arglist-cont-nonempty . +)
;                              (comment-intro        . 0)
;                              (arglist-intro . c-lineup-arglist-intro-after-paren)
;                              (arglist-close . c-lineup-arglist)
          )
         )
        )
       )
     t)
   )
)

(add-hook 'c++-mode-hook
      '(lambda()
        (font-lock-add-keywords
         nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; add the new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
           ))
        ) t)


(defun bury-compile-buffer-if-successful (buffer string)
"Bury a compilation buffer if it succeeded without warnings."
	  (if (and
	  (string-match "compilation" (buffer-name buffer))
	  (string-match "finished" string)
	  (not
	  (with-current-buffer buffer
	  (goto-char 1)
	  (search-forward "warning" nil t)
	  )))
	  (run-with-timer 2 nil
	  (lambda (buf)
	  (bury-buffer buf)
	  (if one-window-in-frame
	  (delete-window (get-buffer-window buf t))
	  (switch-to-prev-buffer (get-buffer-window buf t) 'kill)
	  )) buffer)
	   ))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(defun insert-tt-header () (interactive)
 (insert "/***************************************************************************
 *    
 *                    Unpublished Work Copyright (c) 2010
 *                  Trading Technologies International, Inc.
 *                       All Rights Reserved Worldwide
 *
 *          * * *   S T R I C T L Y   P R O P R I E T A R Y   * * *
 *
 * WARNING:  This program (or document) is unpublished, proprietary property
 * of Trading Technologies International, Inc. and is to be maintained in
 * strict confidence. Unauthorized reproduction, distribution or disclosure 
 * of this program (or document), or any program (or document) derived from
 * it is prohibited by State and Federal law, and by local law outside of 
 * the U.S.
 *
 ***************************************************************************
 * $RCSfile: $
 * $Date: $
 * $Author: $
 * $Revision: $
 ***************************************************************************/
"))

(defun insert-tt-footer () (interactive)
 (insert "
/***************************************************************************
 * $Log: ")
(insert (file-name-nondirectory (buffer-file-name)))
(insert "  $
 ***************************************************************************/
"))




