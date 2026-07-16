;;; nerv-theme.el --- NERV Central Dogma: industrial monochrome + amber  -*- lexical-binding: t; -*-

;;; Code:
(deftheme nerv
  "NERV Central Dogma.")

(let ((class '((class color) (min-colors 89)))
      ;; ── Paleta Central Dogma ───────────────────────────────────────────
      (nerv-bg        "#0A0B0C")
      (nerv-bg-alt    "#15171A")
      (nerv-bg-elev   "#1E2125")
      (nerv-line      "#2E3338")
      (nerv-gray      "#4A4F54")
      (nerv-gray-dim  "#3A3F44")
      (nerv-fg        "#C9C7C0")
      (nerv-fg-bright "#E8E6E1")
      (nerv-amber     "#E8551F")
      (nerv-amber2    "#D98E2B")
      (nerv-red       "#C0392B")
      (nerv-green     "#6B8E4E")
      (nerv-blue      "#4A6E8A")
      (nerv-teal      "#4E8A85")
      (nerv-violet    "#7A5C7E"))

  (custom-theme-set-faces
   'nerv

   ;; ── Base ──────────────────────────────────────────────────────────────
   `(default            ((,class (:background ,nerv-bg :foreground ,nerv-fg))))
   `(cursor             ((,class (:background ,nerv-amber))))
   `(region             ((,class (:background ,nerv-line :extend t))))
   `(highlight          ((,class (:background ,nerv-bg-elev :foreground ,nerv-fg-bright))))
   `(hl-line            ((,class (:background ,nerv-bg-alt :extend t))))
   `(fringe             ((,class (:background ,nerv-bg :foreground ,nerv-gray))))
   `(vertical-border    ((,class (:foreground ,nerv-line))))
   `(window-divider     ((,class (:foreground ,nerv-line))))
   `(window-divider-first-pixel ((,class (:foreground ,nerv-line))))
   `(window-divider-last-pixel  ((,class (:foreground ,nerv-line))))
   `(minibuffer-prompt  ((,class (:foreground ,nerv-amber :weight bold))))
   `(shadow             ((,class (:foreground ,nerv-gray))))
   `(escape-glyph       ((,class (:foreground ,nerv-amber2))))
   `(homoglyph          ((,class (:foreground ,nerv-amber2))))
   `(secondary-selection ((,class (:background ,nerv-bg-elev :extend t))))
   `(match              ((,class (:background ,nerv-amber :foreground ,nerv-bg))))
   `(trailing-whitespace ((,class (:background ,nerv-red))))
   `(fill-column-indicator ((,class (:foreground ,nerv-gray-dim))))
   `(link               ((,class (:foreground ,nerv-blue :underline t))))
   `(link-visited       ((,class (:foreground ,nerv-violet :underline t))))
   `(button             ((,class (:foreground ,nerv-amber :underline t))))
   `(error              ((,class (:foreground ,nerv-red :weight bold))))
   `(warning            ((,class (:foreground ,nerv-amber2 :weight bold))))
   `(success            ((,class (:foreground ,nerv-green :weight bold))))

   ;; ── Line numbers ─────────────────────────────────────────────────────
   `(line-number               ((,class (:background ,nerv-bg :foreground ,nerv-gray))))
   `(line-number-current-line  ((,class (:background ,nerv-bg-alt :foreground ,nerv-amber :weight bold))))
   `(line-number-major-tick    ((,class (:foreground ,nerv-amber2))))
   `(line-number-minor-tick    ((,class (:foreground ,nerv-gray-dim))))

   ;; ── Mode line / header (sharp: box fino, sem 3D) ─────────────────────
   `(mode-line          ((,class (:background ,nerv-bg-elev :foreground ,nerv-fg-bright
                                  :box (:line-width 1 :color ,nerv-line)))))
   `(mode-line-inactive ((,class (:background ,nerv-bg-alt :foreground ,nerv-gray
                                  :box (:line-width 1 :color ,nerv-line)))))
   `(mode-line-highlight ((,class (:foreground ,nerv-amber :box nil))))
   `(mode-line-buffer-id ((,class (:foreground ,nerv-amber :weight bold))))
   `(header-line        ((,class (:background ,nerv-bg-alt :foreground ,nerv-fg
                                  :box (:line-width 1 :color ,nerv-line)))))

   ;; ── Tab bar / tab line ───────────────────────────────────────────────
   `(tab-bar            ((,class (:background ,nerv-bg-alt :foreground ,nerv-gray))))
   `(tab-bar-tab        ((,class (:background ,nerv-bg-elev :foreground ,nerv-fg-bright
                                  :box (:line-width 1 :color ,nerv-amber)))))
   `(tab-bar-tab-inactive ((,class (:background ,nerv-bg-alt :foreground ,nerv-gray
                                  :box (:line-width 1 :color ,nerv-line)))))
   `(tab-line           ((,class (:background ,nerv-bg-alt :foreground ,nerv-gray))))

   ;; ── Search ───────────────────────────────────────────────────────────
   `(isearch            ((,class (:background ,nerv-amber :foreground ,nerv-bg :weight bold))))
   `(isearch-fail       ((,class (:background ,nerv-red :foreground ,nerv-fg-bright))))
   `(lazy-highlight     ((,class (:background ,nerv-line :foreground ,nerv-amber2 :extend t))))

   ;; ── Parens ───────────────────────────────────────────────────────────
   `(show-paren-match            ((,class (:background ,nerv-amber :foreground ,nerv-bg :weight bold))))
   `(show-paren-match-expression ((,class (:background ,nerv-bg-elev))))
   `(show-paren-mismatch         ((,class (:background ,nerv-red :foreground ,nerv-fg-bright :weight bold))))

   ;; ── Completions ──────────────────────────────────────────────────────
   `(completions-common-part      ((,class (:foreground ,nerv-amber :weight bold))))
   `(completions-first-difference ((,class (:foreground ,nerv-fg-bright))))
   `(completions-annotations      ((,class (:foreground ,nerv-gray :slant italic))))

   ;; ── font-lock (sintaxe) ──────────────────────────────────────────────
   ;; Filosofia: base neutra, keyword em âmbar, comentário recuado em cinza.
   `(font-lock-keyword-face       ((,class (:foreground ,nerv-amber :weight bold))))
   `(font-lock-builtin-face       ((,class (:foreground ,nerv-amber2))))
   `(font-lock-preprocessor-face  ((,class (:foreground ,nerv-violet))))
   `(font-lock-comment-face       ((,class (:foreground ,nerv-gray :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,nerv-gray-dim :slant italic))))
   `(font-lock-doc-face           ((,class (:foreground ,nerv-gray :slant italic))))
   `(font-lock-doc-markup-face    ((,class (:foreground ,nerv-teal))))
   `(font-lock-string-face        ((,class (:foreground ,nerv-green))))
   `(font-lock-function-name-face ((,class (:foreground ,nerv-fg-bright :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,nerv-fg))))
   `(font-lock-type-face          ((,class (:foreground ,nerv-amber2))))
   `(font-lock-constant-face      ((,class (:foreground ,nerv-teal))))
   `(font-lock-warning-face       ((,class (:foreground ,nerv-red :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,nerv-red))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,nerv-amber2))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,nerv-amber))))
   ;; Faces novas (Emacs 28/29) — inofensivas em versões anteriores
   `(font-lock-number-face          ((,class (:foreground ,nerv-teal))))
   `(font-lock-operator-face        ((,class (:foreground ,nerv-amber))))
   `(font-lock-property-name-face   ((,class (:foreground ,nerv-blue))))
   `(font-lock-property-use-face    ((,class (:foreground ,nerv-blue))))
   `(font-lock-function-call-face   ((,class (:foreground ,nerv-fg-bright))))
   `(font-lock-variable-use-face    ((,class (:foreground ,nerv-fg))))
   `(font-lock-bracket-face         ((,class (:foreground ,nerv-gray))))
   `(font-lock-delimiter-face       ((,class (:foreground ,nerv-gray))))
   `(font-lock-punctuation-face     ((,class (:foreground ,nerv-gray))))
   `(font-lock-escape-face          ((,class (:foreground ,nerv-amber2))))
   `(font-lock-misc-punctuation-face ((,class (:foreground ,nerv-gray))))

   ;; ── Org-mode ─────────────────────────────────────────────────────────
   `(org-level-1        ((,class (:foreground ,nerv-amber :weight bold :height 1.15))))
   `(org-level-2        ((,class (:foreground ,nerv-fg-bright :weight bold :height 1.08))))
   `(org-level-3        ((,class (:foreground ,nerv-amber2 :weight bold))))
   `(org-level-4        ((,class (:foreground ,nerv-teal :weight bold))))
   `(org-level-5        ((,class (:foreground ,nerv-blue))))
   `(org-level-6        ((,class (:foreground ,nerv-green))))
   `(org-document-title ((,class (:foreground ,nerv-amber :weight bold :height 1.3))))
   `(org-document-info  ((,class (:foreground ,nerv-gray))))
   `(org-block          ((,class (:background ,nerv-bg-alt :extend t))))
   `(org-block-begin-line ((,class (:background ,nerv-bg-alt :foreground ,nerv-gray :extend t))))
   `(org-block-end-line   ((,class (:background ,nerv-bg-alt :foreground ,nerv-gray :extend t))))
   `(org-code           ((,class (:foreground ,nerv-amber2 :background ,nerv-bg-alt))))
   `(org-verbatim       ((,class (:foreground ,nerv-green))))
   `(org-table          ((,class (:foreground ,nerv-fg :background ,nerv-bg-alt))))
   `(org-link           ((,class (:foreground ,nerv-blue :underline t))))
   `(org-todo           ((,class (:foreground ,nerv-red :weight bold))))
   `(org-done           ((,class (:foreground ,nerv-green :weight bold))))
   `(org-headline-done  ((,class (:foreground ,nerv-gray))))
   `(org-date           ((,class (:foreground ,nerv-teal :underline t))))
   `(org-special-keyword ((,class (:foreground ,nerv-gray))))
   `(org-checkbox       ((,class (:foreground ,nerv-amber))))

   ;; ── Dired ────────────────────────────────────────────────────────────
   `(dired-directory    ((,class (:foreground ,nerv-amber :weight bold))))
   `(dired-symlink      ((,class (:foreground ,nerv-teal))))
   `(dired-broken-symlink ((,class (:foreground ,nerv-red :weight bold))))
   `(dired-flagged      ((,class (:foreground ,nerv-red :weight bold))))
   `(dired-marked       ((,class (:foreground ,nerv-amber :weight bold))))
   `(dired-header       ((,class (:foreground ,nerv-fg-bright :weight bold))))
   `(dired-ignored      ((,class (:foreground ,nerv-gray))))

   ;; ── Diff / VC ────────────────────────────────────────────────────────
   `(diff-added         ((,class (:foreground ,nerv-green :background ,nerv-bg-alt :extend t))))
   `(diff-removed       ((,class (:foreground ,nerv-red :background ,nerv-bg-alt :extend t))))
   `(diff-changed       ((,class (:foreground ,nerv-amber2 :background ,nerv-bg-alt :extend t))))
   `(diff-header        ((,class (:background ,nerv-bg-elev :foreground ,nerv-fg-bright))))
   `(diff-file-header   ((,class (:foreground ,nerv-amber :weight bold))))
   `(diff-hunk-header   ((,class (:background ,nerv-bg-elev :foreground ,nerv-amber2))))

   ;; ── Compilação / mensagens ───────────────────────────────────────────
   `(compilation-error    ((,class (:foreground ,nerv-red :weight bold))))
   `(compilation-warning  ((,class (:foreground ,nerv-amber2 :weight bold))))
   `(compilation-info     ((,class (:foreground ,nerv-green))))
   `(compilation-line-number ((,class (:foreground ,nerv-amber))))
   `(compilation-mode-line-fail ((,class (:foreground ,nerv-red :weight bold))))
   `(compilation-mode-line-run  ((,class (:foreground ,nerv-amber2))))
   `(compilation-mode-line-exit ((,class (:foreground ,nerv-green))))

   ;; ── Widgets / custom ─────────────────────────────────────────────────
   `(widget-field       ((,class (:background ,nerv-bg-elev :foreground ,nerv-fg
                                  :box (:line-width 1 :color ,nerv-line)))))
   `(custom-state       ((,class (:foreground ,nerv-green))))
   `(custom-group-tag   ((,class (:foreground ,nerv-amber :weight bold))))
   `(custom-variable-tag ((,class (:foreground ,nerv-fg-bright :weight bold)))))

  
  (custom-theme-set-variables
   'nerv
   `(ansi-color-names-vector
     [,nerv-bg ,nerv-red ,nerv-green ,nerv-amber2
      ,nerv-blue ,nerv-violet ,nerv-teal ,nerv-fg])))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nerv)
(provide 'nerv-theme)

;;; nerv-theme.el ends here
