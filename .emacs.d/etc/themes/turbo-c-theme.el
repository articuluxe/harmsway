;;; turbo-c-theme.el --- Emacs theme in the style of Turbo C / Borland IDE -*- lexical-binding: t; -*-

;;; Code:

(deftheme turbo-c
  "Borland Turbo C IDE.")

(let* (;; Classic DOS CGA/EGA palette
       (tc-blue        "#0000A8") ; Classic IBM background blue
       (tc-dark-blue   "#000080") ; Darker blue for highlights
       (tc-bright-blue "#5555FF") ; Light blue
       (tc-yellow      "#FFFF55") ; Bright yellow (default text)
       (tc-dark-yellow "#AAAA00") ; Dark brown/yellow
       (tc-white       "#FFFFFF") ; Bright white (keywords)
       (tc-light-gray  "#AAAAAA") ; Light gray (strings/comments)
       (tc-dark-gray   "#555555") ; Dark gray
       (tc-cyan        "#55FFFF") ; Bright cyan (preprocessor)
       (tc-dark-cyan   "#00AAAA") ; Dark cyan
       (tc-green       "#55FF55") ; Bright green
       (tc-dark-green  "#00AA00") ; Dark green
       (tc-red         "#FF5555") ; Bright red (function keys)
       (tc-dark-red    "#AA0000") ; Dark red (errors)
       (tc-magenta     "#FF55FF") ; Bright magenta
       (tc-orange      "#FFAA00") ; Orange (cursor)
       (tc-black       "#000000")
       (tc-menu-bg     "#AAAAAA") ; Gray background of the top menu
       (tc-menu-fg     "#000000") ; Black menu text
       (tc-modeline-bg "#AAAAAA") ; Gray bottom bar
       (tc-modeline-fg "#000000") ; Bottom bar text
       (tc-selection   "#00AAAA") ; Dark cyan selection
       (tc-region-bg   "#AAAAAA")) ; Selected region background

  (custom-theme-set-faces
   'turbo-c

   ;; ===== Basic faces =====
   `(default ((t (:background ,tc-blue :foreground ,tc-yellow))))
   `(cursor  ((t (:background ,tc-orange :foreground ,tc-blue))))
   `(region  ((t (:background ,tc-region-bg :foreground ,tc-black))))
   `(highlight ((t (:background ,tc-dark-cyan :foreground ,tc-white))))
   `(secondary-selection ((t (:background ,tc-dark-cyan :foreground ,tc-white))))
   `(fringe ((t (:background ,tc-blue :foreground ,tc-light-gray))))
   `(vertical-border ((t (:foreground ,tc-light-gray :background ,tc-blue))))
   `(window-divider ((t (:foreground ,tc-light-gray))))
   `(window-divider-first-pixel ((t (:foreground ,tc-light-gray))))
   `(window-divider-last-pixel ((t (:foreground ,tc-light-gray))))
   `(minibuffer-prompt ((t (:foreground ,tc-white :weight bold))))
   `(error ((t (:foreground ,tc-red :weight bold))))
   `(warning ((t (:foreground ,tc-yellow :weight bold))))
   `(success ((t (:foreground ,tc-green :weight bold))))
   `(shadow ((t (:foreground ,tc-light-gray))))
   `(link ((t (:foreground ,tc-cyan :underline t))))
   `(link-visited ((t (:foreground ,tc-magenta :underline t))))
   `(escape-glyph ((t (:foreground ,tc-cyan))))
   `(trailing-whitespace ((t (:background ,tc-dark-red))))

   ;; ===== Menu bar (Turbo C style: gray with black text) =====
   `(menu ((t (:background ,tc-menu-bg :foreground ,tc-menu-fg))))
   `(tool-bar ((t (:background ,tc-menu-bg :foreground ,tc-menu-fg))))
   `(tab-bar ((t (:background ,tc-menu-bg :foreground ,tc-menu-fg))))
   `(tab-bar-tab ((t (:background ,tc-blue :foreground ,tc-yellow :weight bold))))
   `(tab-bar-tab-inactive ((t (:background ,tc-menu-bg :foreground ,tc-menu-fg))))
   `(header-line ((t (:background ,tc-menu-bg :foreground ,tc-menu-fg :weight bold))))

   ;; ===== Mode line (status bar, Turbo C F1/F2/F3 style) =====
   `(mode-line ((t (:background ,tc-modeline-bg :foreground ,tc-modeline-fg
                    :box (:line-width 1 :color ,tc-black)))))
   `(mode-line-inactive ((t (:background ,tc-dark-gray :foreground ,tc-light-gray
                             :box (:line-width 1 :color ,tc-black)))))
   `(mode-line-highlight ((t (:foreground ,tc-dark-red :weight bold))))
   `(mode-line-buffer-id ((t (:foreground ,tc-dark-red :weight bold))))
   `(mode-line-emphasis ((t (:foreground ,tc-dark-red :weight bold))))

   ;; ===== Line highlight =====
   `(hl-line ((t (:background ,tc-dark-blue))))
   `(line-number ((t (:background ,tc-blue :foreground ,tc-dark-cyan))))
   `(line-number-current-line ((t (:background ,tc-blue :foreground ,tc-white :weight bold))))

   ;; ===== font-lock (syntax highlighting) - authentic Turbo C palette =====
   `(font-lock-builtin-face        ((t (:foreground ,tc-white :weight bold))))
   `(font-lock-comment-face        ((t (:foreground ,tc-red :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,tc-light-gray :slant italic))))
   `(font-lock-constant-face       ((t (:foreground ,tc-white :weight bold))))
   `(font-lock-doc-face            ((t (:foreground ,tc-light-gray :slant italic))))
   `(font-lock-function-name-face  ((t (:foreground ,tc-green :weight bold))))
   `(font-lock-keyword-face        ((t (:foreground ,tc-white :weight bold))))
   `(font-lock-negation-char-face  ((t (:foreground ,tc-white))))
   `(font-lock-preprocessor-face   ((t (:foreground ,tc-cyan :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,tc-cyan))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,tc-magenta))))
   `(font-lock-string-face         ((t (:foreground ,tc-red))))
   `(font-lock-type-face           ((t (:foreground ,tc-white :weight bold))))
   `(font-lock-variable-name-face  ((t (:foreground ,tc-green))))
   `(font-lock-warning-face        ((t (:foreground ,tc-red :weight bold))))

   ;; ===== Search / Isearch =====
   `(isearch ((t (:background ,tc-dark-cyan :foreground ,tc-white :weight bold))))
   `(isearch-fail ((t (:background ,tc-dark-red :foreground ,tc-white :weight bold))))
   `(lazy-highlight ((t (:background ,tc-dark-blue :foreground ,tc-yellow))))
   `(match ((t (:background ,tc-dark-cyan :foreground ,tc-white))))

   ;; ===== Matching parentheses =====
   `(show-paren-match ((t (:background ,tc-dark-cyan :foreground ,tc-white :weight bold))))
   `(show-paren-mismatch ((t (:background ,tc-dark-red :foreground ,tc-white :weight bold))))

   ;; ===== Company / Autocomplete (DOS-style popups) =====
   `(company-tooltip ((t (:background ,tc-light-gray :foreground ,tc-black))))
   `(company-tooltip-selection ((t (:background ,tc-dark-cyan :foreground ,tc-white))))
   `(company-tooltip-common ((t (:foreground ,tc-dark-red :weight bold))))
   `(company-tooltip-common-selection ((t (:foreground ,tc-yellow :weight bold))))
   `(company-tooltip-annotation ((t (:foreground ,tc-dark-blue))))
   `(company-scrollbar-bg ((t (:background ,tc-dark-gray))))
   `(company-scrollbar-fg ((t (:background ,tc-dark-cyan))))
   `(company-preview ((t (:background ,tc-blue :foreground ,tc-light-gray))))
   `(company-preview-common ((t (:foreground ,tc-white :weight bold))))

   ;; ===== Org-mode =====
   `(org-level-1 ((t (:foreground ,tc-white :weight bold :height 1.1))))
   `(org-level-2 ((t (:foreground ,tc-yellow :weight bold))))
   `(org-level-3 ((t (:foreground ,tc-cyan :weight bold))))
   `(org-level-4 ((t (:foreground ,tc-green))))
   `(org-level-5 ((t (:foreground ,tc-magenta))))
   `(org-level-6 ((t (:foreground ,tc-red))))
   `(org-link ((t (:foreground ,tc-cyan :underline t))))
   `(org-todo ((t (:foreground ,tc-red :weight bold))))
   `(org-done ((t (:foreground ,tc-green :weight bold))))
   `(org-date ((t (:foreground ,tc-cyan :underline t))))
   `(org-code ((t (:foreground ,tc-light-gray :background ,tc-dark-blue))))
   `(org-block ((t (:background ,tc-dark-blue :foreground ,tc-yellow))))
   `(org-block-begin-line ((t (:foreground ,tc-light-gray :background ,tc-dark-blue))))
   `(org-block-end-line ((t (:foreground ,tc-light-gray :background ,tc-dark-blue))))
   `(org-table ((t (:foreground ,tc-cyan))))
   `(org-special-keyword ((t (:foreground ,tc-magenta))))

   ;; ===== Dired =====
   `(dired-directory ((t (:foreground ,tc-white :weight bold))))
   `(dired-symlink ((t (:foreground ,tc-cyan))))
   `(dired-marked ((t (:foreground ,tc-red :weight bold))))
   `(dired-flagged ((t (:foreground ,tc-red :weight bold))))
   `(dired-header ((t (:foreground ,tc-white :weight bold))))

   ;; ===== Diff =====
   `(diff-added ((t (:foreground ,tc-green :background ,tc-dark-green))))
   `(diff-removed ((t (:foreground ,tc-red :background ,tc-dark-red))))
   `(diff-changed ((t (:foreground ,tc-yellow))))
   `(diff-header ((t (:foreground ,tc-white :weight bold))))
   `(diff-file-header ((t (:foreground ,tc-cyan :weight bold))))
   `(diff-hunk-header ((t (:foreground ,tc-magenta))))

   ;; ===== Magit =====
   `(magit-section-heading ((t (:foreground ,tc-white :weight bold))))
   `(magit-branch-local ((t (:foreground ,tc-cyan :weight bold))))
   `(magit-branch-remote ((t (:foreground ,tc-green :weight bold))))
   `(magit-diff-added ((t (:foreground ,tc-green))))
   `(magit-diff-removed ((t (:foreground ,tc-red))))
   `(magit-diff-context ((t (:foreground ,tc-light-gray))))
   `(magit-hash ((t (:foreground ,tc-magenta))))

   ;; ===== Flycheck / Flymake =====
   `(flycheck-error ((t (:underline (:color ,tc-red :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,tc-yellow :style wave)))))
   `(flycheck-info ((t (:underline (:color ,tc-cyan :style wave)))))
   `(flymake-error ((t (:underline (:color ,tc-red :style wave)))))
   `(flymake-warning ((t (:underline (:color ,tc-yellow :style wave)))))

   ;; ===== Whitespace =====
   `(whitespace-space ((t (:foreground ,tc-dark-cyan))))
   `(whitespace-tab ((t (:foreground ,tc-dark-cyan))))
   `(whitespace-newline ((t (:foreground ,tc-dark-cyan))))
   `(whitespace-trailing ((t (:background ,tc-dark-red))))

   ;; ===== Others =====
   `(button ((t (:foreground ,tc-cyan :underline t :weight bold))))
   `(custom-button ((t (:background ,tc-light-gray :foreground ,tc-black
                        :box (:line-width 1 :color ,tc-black)))))
   `(widget-field ((t (:background ,tc-dark-blue :foreground ,tc-yellow))))
   `(help-key-binding ((t (:foreground ,tc-red :weight bold))))
   `(completions-common-part ((t (:foreground ,tc-white :weight bold))))
   `(completions-first-difference ((t (:foreground ,tc-yellow))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'turbo-c)

;;; turbo-c-theme.el ends here
