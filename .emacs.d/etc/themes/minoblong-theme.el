(deftheme minoblong "MINIMAL --MISS-SHAPPEN AND CYBERPUNK")

(let ((accent  "#e35420")    
      (diff-bg "#13154c")
      (diff-fg "#0000CD")
      (gray-11 "#111111") 
      (gray-1c "#1c1c1c")    
      (gray-22 "#222222")
      (gray-44 "#444444")
      (gray-66 "#666666")    
      (gray-77 "#777777")    
      (gray-7c "#7c7c7c") 
      (gray-99 "#999999")  
      (gray-aa "#aaaaaa")   
      (purple  "#f752c0"))  

  (custom-theme-set-faces
   'minoblong

   ;; Base faces
   `(default ((t (:foreground ,gray-7c :background ,gray-11))))
   `(cursor ((t (:background ,accent))))
   `(region ((t (:foreground ,accent :background ,gray-22))))
   `(highlight ((t (:background ,gray-22))))
   `(hl-line ((t (:background ,gray-1c))))
   `(fringe ((t (:background ,gray-11 :foreground ,gray-66))))
   `(vertical-border ((t (:foreground ,gray-44))))

   ;; Font lock 
   `(font-lock-comment-face ((t (:foreground ,gray-44))))
   `(font-lock-doc-face ((t (:foreground ,accent))))
   `(font-lock-string-face ((t (:foreground ,gray-aa :background ,gray-22))))
   `(font-lock-keyword-face ((t (:foreground ,purple))))
   `(font-lock-builtin-face ((t (:foreground ,gray-7c))))
   `(font-lock-function-name-face ((t (:foreground ,gray-aa))))
   `(font-lock-variable-name-face ((t (:foreground ,gray-aa))))
   `(font-lock-type-face ((t (:foreground ,gray-aa))))
   `(font-lock-constant-face ((t (:foreground ,gray-aa))))
   `(font-lock-warning-face ((t (:foreground ,gray-44))))
   `(font-lock-negation-char-face ((t (:foreground ,purple))))
   `(font-lock-operator-face ((t (:foreground ,purple))))
   `(font-lock-punctuation-face ((t (:foreground ,purple))))
   `(font-lock-preprocessor-face ((t (:foreground ,purple))))
   `(button ((t (:foreground ,gray-aa :background ,gray-22))))

   ;; Mode line
   `(mode-line ((t (:foreground ,gray-7c :background ,gray-22 ))))
   `(mode-line-inactive ((t (:foreground ,gray-66 :background ,gray-1c :box (:line-width 1 :color ,gray-1c)))))
   `(mode-line-buffer-id ((t (:weight bold))))

   ;; Minibuffer
   `(minibuffer-prompt ((t (:foreground ,accent :weight bold))))

   ;; Search
   `(isearch ((t (:foreground ,gray-7c :background ,gray-22 :weight bold))))
   `(match ((t (:background ,gray-22 :foreground ,gray-7c))))
   `(completions-common-part ((t (:background ,gray-22 :foreground ,gray-7c))))
   `(lazy-highlight ((t (:foreground ,gray-7c :background ,gray-1c :underline t))))

   ;; Line numbers
   `(line-number ((t (:foreground ,gray-66 :background ,gray-11))))
   `(line-number-current-line ((t (:foreground ,accent :background ,gray-11 :weight bold))))

   ;; Parentheses matching
   `(show-paren-match ((t (:foreground ,gray-99 :background ,gray-11 :weight bold))))
   `(show-paren-mismatch ((t (:foreground ,accent :background ,gray-11 :weight bold))))

   ;; Error/warning faces
   `(error ((t (:foreground ,accent :weight bold))))
   `(warning ((t (:foreground ,accent :weight bold))))
   `(success ((t (:foreground ,gray-77 :weight bold))))

   ;; Diagnostic severity levels
   `(lsp-lsp-flycheck-error-unnecessary-face ((t (:foreground ,gray-22 :underline nil))))
   `(lsp-lsp-flycheck-warning-unnecessary-face ((t (:foreground ,gray-22 :underline nil))))

   ;; LSP UI faces
   `(lsp-ui-doc-background ((t (:background ,gray-22))))
   `(lsp-ui-doc-header ((t (:foreground ,gray-22))))
   `(lsp-ui-peek-filename ((t (:foreground ,accent))))
   `(lsp-ui-peek-header ((t (:background ,gray-22))))
   `(lsp-ui-sideline-code-action ((t (:foreground ,accent))))

   `(lsp-face-highlight-read ((t (:background ,gray-22))))
   `(lsp-face-highlight-write ((t (:background ,gray-22))))
   `(lsp-face-highlight-textual ((t (:background ,gray-22))))

   `(popup-tip-face ((t (:background ,accent :foreground ,gray-11))))

   ;; Dired
   `(dired-directory ((t (:foreground ,gray-aa :weight bold))))
   `(dired-symlink ((t (:foreground ,gray-aa))))
   `(dired-executable ((t (:foreground ,gray-77))))

   `(minibuffer-prompt ((t (:foreground ,accent :weight bold))))
   `(help-key-binding ((t (:foreground ,accent :background ,gray-11))))
   `(read-multiple-choice-face ((t (:foreground ,accent :weight bold))))

   ;; Company
   `(company-tooltip ((t (:foreground ,gray-7c :background ,gray-1c))))
   `(company-tooltip-gray-22 ((t (:foreground ,gray-7c :background ,gray-22))))
   `(company-tooltip-common ((t (:foreground ,accent :weight bold))))

   ;; Org mode
   `(org-level-1 ((t (:foreground ,gray-aa :weight bold :height 1.3))))
   `(org-level-2 ((t (:foreground ,purple :weight bold :height 1.2))))
   `(org-level-3 ((t (:foreground ,gray-77 :weight bold :height 1.1))))
   `(org-level-4 ((t (:foreground ,accent :weight bold))))
   `(org-code ((t (:foreground ,gray-aa :background ,gray-1c))))
   `(org-block ((t (:background ,gray-1c))))
   `(org-block-begin-line ((t (:foreground ,accent :background ,gray-1c))))
   `(org-block-end-line ((t (:inherit org-block-begin-line))))

   ;; Markdown
   `(markdown-header-face-1 ((t (:inherit org-level-1))))
   `(markdown-header-face-2 ((t (:inherit org-level-2))))
   `(markdown-header-face-3 ((t (:inherit org-level-3))))
   `(markdown-code-face ((t (:inherit org-code))))

   ;; Magit
   `(magit-branch-local ((t (:foreground ,gray-aa))))
   `(magit-branch-remote ((t (:foreground ,gray-77))))
   `(magit-diff-added ((t (:foreground ,gray-77 :background ,(concat gray-77 "20")))))
   `(magit-diff-removed ((t (:foreground ,accent :background ,(concat accent "20")))))
   `(magit-diff-context ((t (:foreground ,gray-66))))
   `(diff-hl-margin-change ((t (:foreground ,diff-fg :background ,diff-bg))))

   ;; Which-key
   `(which-key-key-face ((t (:foreground ,accent :weight bold))))
   `(which-key-description-face ((t (:foreground ,gray-7c))))
   `(which-key-group-description-face ((t (:foreground ,purple))))

   ;; Helm/Ivy
   `(helm-gray-22 ((t (:foreground ,accent :background ,gray-22))))
   `(ivy-current-match ((t (:foreground ,accent :background ,gray-22 :weight bold))))
   `(ivy-minibuffer-match-highlight ((t (:background ,gray-22 ))))
   `(ivy-minibuffer-match-1 ((t (:background ,gray-22 ))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,gray-22 ))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,accent :background ,gray-22 ))))

   ;; vterm
   `(vterm-color-black ((t ( :foreground ,gray-11 :background ,gray-11))))
   `(vterm-color-red ((t ( :foreground ,accent :background ,accent))))
   `(vterm-color-gray-77 ((t ( :foreground ,gray-77 :background ,gray-77))))
   `(vterm-color-yellow ((t ( :foreground ,gray-99 :background ,gray-99))))
   `(vterm-color-blue ((t ( :foreground ,gray-aa :background ,gray-aa))))
   `(vterm-color-magenta ((t ( :foreground ,purple :background ,purple))))
   `(vterm-color-gray-aa ((t ( :foreground ,gray-aa :background ,gray-aa))))
   `(vterm-color-white ((t( :foreground ,gray-7c :background ,gray-7c))))
   ))

(provide-theme 'minoblong)


