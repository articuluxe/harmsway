;;; beach-theme.el --- A light, sunny and calm Emacs theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2021–2024 Dan Dee
;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; URL: https://github.com/monkeyjunglejuice/beach-emacs-theme
;; Version: 1.4.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces, theme
;; SPDX-License-Identifier: MIT
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Gulp down your happy shake, listen to the trippy tunes and slowly do
;; "M-x load-theme" ... you deserve it. »Beach« is a sunny theme inspired
;; by monochromatic themes and e-ink displays. It appears calm, wears colors
;; sparingly, and only where really necessary.
;;
;; My other themes:
;; - "The Matrix" <https://github.com/monkeyjunglejuice/matrix-emacs-theme>
;;
;;; Code:

(deftheme beach "A light, sunny and calm Emacs theme.")

;; Colors
(let* ((color-bg           "#fef7dc")
       (color-bg-alt       "#f4edd2")
       (color-fg           "#46432f")
       (color-fg-alt       "#181611")
       (color-fg-dim       "#9b957d")
       (color-fg-dimmer    "#d9d5b5")
       (color-light        "#d3f7e1")
       (color-middle       "#4ba7a5")
       (color-dark         "#0e7881")
       (color-dark-soft    "#78beb8")
       (color-bright       "#e68f4c")
       (color-bright-soft  "#f4ad67")
       (color-bright-light "#ffefb6")
       (color-strong       "#ac390c")
       (color-strong-soft  "#f0885e")
       (color-strong-light "#ffdac2")
       (color-shade-1      "#d9d5b5")   ; same as color-fg-dimmer
       (color-shade-2      "#c4c0a1")
       (color-shade-3      "#b0ad90")
       (color-shade-4      "#9b957d")   ; same as color-fg-dim
       (color-shade-5      "#838067")
       (color-shade-6      "#6e6b54")
       (color-shade-7      "#595641")
       (color-shade-8      "#46432f")   ; same as color-fg
       (color-shade-9      "#181611"))  ; same as color-fg-alt

  (custom-theme-set-faces
   'beach
   `(default ((t (:background ,color-bg :foreground ,color-fg))))
   `(cursor ((t (:background ,color-fg-alt))))
   `(region ((t (:foreground ,color-dark :background ,color-light))))
   `(success ((t (:foreground ,color-dark-soft :weight bold))))
   `(warning ((t (:foreground ,color-bright :weight bold))))
   `(error ((t (:foreground ,color-strong :weight bold))))
   `(secondary-selection ((t (:background ,color-bg-alt))))
   `(mode-line ((t (:background ,color-fg :foreground ,color-bg :box ,color-fg))))
   `(mode-line-buffer-id ((t (:foreground ,color-light :distant-foreground ,color-fg :weight bold))))
   `(mode-line-inactive ((t (:background ,color-fg-dim :foreground ,color-bg :box ,color-fg-dim))))
   `(fringe ((t (:background ,color-bg-alt))))
   `(vertical-border ((t (:foreground ,color-fg :background unspecified))))
   `(minibuffer-prompt ((t (:foreground ,color-fg-alt :weight bold))))
   `(isearch ((t (:foreground ,color-fg-alt :background ,color-light :underline t))))
   `(isearch-fail ((t (:inherit error))))
   `(lazy-highlight ((t (:background ,color-bright-light :underline (:foreground ,color-fg)))))
   `(link ((t (:foreground ,color-fg-alt :underline t))))
   `(link-visited ((t (:foreground ,color-fg-dim :underline t))))
   `(button ((t (:weight bold :inherit link))))
   `(help-face-button ((t (:inherit button))))
   `(help-key-binding ((t (:foreground ,color-fg :weight bold :inherit fixed-pitch-serif))))
   `(transient-key-stay ((t (:foreground ,color-strong))))
   `(header-line ((t (:foreground ,color-fg-dim :background ,color-bg-alt))))
   `(shadow ((t (:foreground ,color-fg-dim))))
   `(widget-inactive ((t (:inherit shadow :box (:color ,color-fg-dim)))))
   `(show-paren-match ((t (:foreground ,color-fg-alt :weight bold :underline (:color ,color-fg-alt)))))
   `(show-paren-match-expression ((t (:background ,color-bright-light))))
   `(show-paren-mismatch ((t (:foreground ,color-strong :background ,color-strong-light :weight bold))))
   `(highlight ((t (:background ,color-light :underline (:color ,color-fg-alt)))))
   `(match ((t (:weight bold))))
   `(hl-line ((t (:underline (:color ,color-fg-alt) :extend t))))
   `(separator-line ((t (:height 0.1 :background ,color-fg-dimmer))))
   `(widget-field ((t (:foreground ,color-fg-alt :background ,color-bg-alt :box (:color ,color-bg)))))
   `(trailing-whitespace ((t (:background ,color-strong-light))))
   `(escape-glyph ((t (:weight bold :inherit shadow))))

   `(font-lock-builtin-face ((t (:foreground ,color-bright :slant italic))))
   `(font-lock-comment-face ((t (:foreground ,color-fg-dim))))
   `(font-lock-constant-face ((t (:foreground ,color-shade-5 :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,color-fg-dim :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,color-fg :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,color-bright))))
   `(font-lock-number-face ((t (:foreground ,color-dark))))
   `(font-lock-operator-face ((t (:foreground ,color-fg-alt :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,color-middle))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,color-middle))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,color-middle))))
   `(font-lock-string-face ((t (:foreground ,color-dark :inherit fixed-pitch-serif))))
   `(font-lock-type-face ((t (:weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,color-fg))))
   `(font-lock-warning-face ((t (:foreground ,color-strong :slant italic))))

   ;; ansi-colors
   `(ansi-color-black ((t (:foreground ,color-fg :background ,color-fg))))
   `(ansi-color-red ((t (:foreground ,color-strong :background ,color-strong))))
   `(ansi-color-green ((t (:foreground ,color-dark :background ,color-dark))))
   `(ansi-color-yellow ((t (:foreground ,color-bright :background ,color-bright))))
   `(ansi-color-blue ((t (:foreground ,color-middle :background ,color-middle))))
   `(ansi-color-magenta ((t (:foreground ,color-strong :background ,color-strong))))
   `(ansi-color-cyan ((t (:foreground ,color-dark :background ,color-dark))))
   `(ansi-color-white ((t (:foreground ,color-fg-dim :background ,color-fg-dim))))

   ;; shell-mode
   `(sh-heredoc ((t (:foreground unspecified :inherit font-lock-string-face))))
   `(sh-quoted-exec ((t (:inherit font-lock-function-name-face))))

   ;; dired
   `(dired-header ((t (:foreground ,color-bright :slant italic))))
   `(dired-directory ((t (:weight bold))))
   `(dired-broken-symlink ((t (:slant italic :inherit error))))
   `(dired-symlink ((t (:slant italic))))
   `(dired-mark ((t (:foreground ,color-dark :background ,color-light))))
   `(dired-marked ((t (:foreground ,color-dark :background ,color-light))))
   `(dired-flagged ((t (:foreground ,color-strong :background ,color-strong-light))))
   `(dired-perm-write ((t (:foreground ,color-strong))))
   `(dired-special ((t (:foreground ,color-middle))))

   ;; dired-subtree
   `(dired-subtree-depth-1-face ((t (:background unspecified))))
   `(dired-subtree-depth-2-face ((t (:background unspecified))))
   `(dired-subtree-depth-3-face ((t (:background unspecified))))
   `(dired-subtree-depth-4-face ((t (:background unspecified))))
   `(dired-subtree-depth-5-face ((t (:background unspecified))))
   `(dired-subtree-depth-6-face ((t (:background unspecified))))

   ;; eldoc
   `(eldoc-highlight-function-argument ((t (:inherit lazy-highlight))))

   ;; elixir-mode
   `(elixir-number-face ((t (:inherit font-lock-number-face))))

   ;; proced
   `(proced-mark ((t (:inherit dired-mark))))
   `(proced-marked ((t (:inherit dired-marked))))

   ;; eshell
   `(eshell-prompt ((t (:inherit minibuffer-prompt))))
   `(eshell-ls-backup ((t (:inherit dired-ignored))))
   `(eshell-ls-directory ((t (:inherit dired-directory))))
   `(eshell-ls-archive ((t (:slant italic :inherit dired-directory))))
   `(eshell-ls-symlink ((t (:inherit dired-symlink))))
   `(eshell-ls-executable ((t (:foreground ,color-bright))))
   `(eshell-ls-missing ((t (:inherit error))))
   `(eshell-ls-product ((t (:foreground ,color-dark))))
   `(eshell-ls-readonly ((t (:inherit shadow))))
   `(eshell-ls-special ((t (:inherit dired-special))))

   ;; comint
   `(comint-highlight-prompt ((t (:inherit minibuffer-prompt))))
   `(comint-highlight-input ((t (:foreground ,color-fg))))

   ;; completions
   `(completions-annotations ((t (:inherit font-lock-comment-face))))
   `(completions-common-part ((t (:weight bold))))
   `(icomplete-first-match ((t (:weight bold :underline t))))

   ;; diff
   `(diff-added ((t (:foreground ,color-dark :background ,color-light))))
   `(diff-removed ((t (:foreground ,color-strong :background ,color-strong-light))))
   `(diff-context ((t (:inherit shadow))))
   `(diff-file-header ((t (:bold t :background ,color-bright-light :weight bold))))
   `(diff-header ((t (:background ,color-bright-light :foreground ,color-fg))))

   ;; package
   `(package-name ((t (:weight bold))))
   `(package-description ((t (:inherit fixed-pitch-serif))))
   `(package-status-available ((t (:slant italic))))
   `(package-status-installed ((t (:foreground ,color-middle :slant italic))))
   `(package-status-dependency ((t (:foreground ,color-middle :slant italic))))
   `(package-status-built-in ((t (:foreground ,color-fg-dim :slant italic))))
   `(package-status-incompat ((t (:slant italic :inherit font-lock-warning-face))))

   ;; customization
   `(custom-button ((t (:foreground ,color-fg :background ,color-bg :box (:color ,color-fg)))))
   `(custom-button-mouse ((t (:inherit custom-button :background ,color-light))))
   `(custom-button-pressed ((t (:inherit custom-button :background ,color-light ))))
   `(custom-button-pressed-unraised ((t (:inherit custom-button-pressed))))
   `(custom-button-unraised ((t (:inherit custom-button))))
   `(custom-comment ((t (:inherit font-lock-doc-face))))
   `(custom-comment-tag ((t (:foreground ,color-fg))))
   `(custom-documentation ((t (:inherit fixed-pitch-serif))))
   `(custom-group-tag ((t (:inherit bold))))
   `(custom-state ((t (:foreground ,color-bright :slant italic))))
   `(custom-variable-tag ((t (:weight bold))))
   `(custom-variable-obsolete ((t (:foreground ,color-fg-dim :inherit custom-variable-tag))))
   `(custom-visibility ((t (:inherit custom-documentation :underline t))))

   ;; info
   `(info-menu-star ((t (:foreground ,color-bright))))

   ;; message
   `(message-header-name ((t (:foreground ,color-fg-dim :weight bold :slant italic))))
   `(message-header-other ((t (:foreground ,color-dark))))
   `(message-header-cc ((t (:inherit message-header-other))))
   `(message-header-newsgroups ((t (:inherit message-header-other))))
   `(message-header-xheader ((t (:inherit message-header-other))))
   `(message-header-subject ((t (:weight bold))))
   `(message-header-to ((t (:foreground ,color-bright))))
   `(message-cited-text ((t (:foreground ,color-fg-dim :inherit italic))))
   `(message-mml ((t (:foreground ,color-bright))))
   `(message-separator ((t (:inherit font-lock-comment-face))))

   ;; erc
   `(erc-notice-face ((t (:foreground ,color-dark :weight unspecified))))
   `(erc-header-line ((t (:inherit header-line))))
   `(erc-timestamp-face ((t (:foreground ,color-bright :weight unspecified))))
   `(erc-current-nick-face ((t (:background ,color-dark :foreground ,color-bg :weight unspecified))))
   `(erc-input-face ((t (:foreground ,color-dark))))
   `(erc-prompt-face ((t (:inherit minibuffer-prompt))))
   `(erc-my-nick-face ((t (:foreground ,color-fg))))
   `(erc-pal-face ((t (:foreground ,color-dark :inherit italic))))

   ;; table
   `(table-cell ((t (:foreground ,color-fg :background ,color-bg-alt))))

   ;; tabs
   `(tab-bar ((t (:foreground ,color-fg-dim :background ,color-bg-alt :box (:color ,color-bg-alt)))))
   `(tab-bar-tab ((t (:foreground ,color-fg))))
   `(tab-bar-tab-ungrouped ((t (:inherit tab-bar-tab))))
   `(tab-bar-tab-group-current ((t (:foreground ,color-fg))))
   `(tab-bar-tab-group-inactive ((t (:foreground ,color-fg-dim))))
   `(tab-bar-tab-inactive ((t (:foreground ,color-fg-dim))))
   `(tab-line ((t :foreground ,color-fg-dim :background ,color-bg-alt :box (:color ,color-bg-alt))))
   `(tab-line-close-highlight ((t (:foreground ,color-fg-alt))))
   `(tab-line-highlight ((t (:foreground ,color-fg-alt))))
   `(tab-line-tab ((t (:foreground ,color-fg))))
   `(tab-line-tab-current ((t (:foreground ,color-fg))))
   `(tab-line-tab-inactive ((t (:foreground ,color-fg-dim))))
   `(tab-line-tab-inactive-alternate ((t (:inherit tab-line-tab-inactive))))

   ;; activities
   `(activities-tabs ((t (:weight bold))))

   ;; tex
   `(font-latex-sedate-face ((t (:foreground ,color-dark))))
   `(font-latex-math-face ((t (:foreground ,color-fg))))
   `(font-latex-script-char-face ((t (:inherit font-latex-math-face))))

   ;; outline
   `(outline-1 ((t (:foreground ,color-strong :weight bold :height 1.2))))
   `(outline-2 ((t (:foreground ,color-bright :weight bold))))
   `(outline-3 ((t (:foreground ,color-dark :weight bold))))
   `(outline-4 ((t (:foreground ,color-fg-alt :weight bold))))
   `(outline-5 ((t (:foreground ,color-fg-alt :weight bold))))
   `(outline-6 ((t (:foreground ,color-fg-alt :weight bold))))
   `(outline-7 ((t (:foreground ,color-fg-alt :weight bold))))
   `(outline-8 ((t (:foreground ,color-fg-alt :weight bold))))

   ;; org-mode
   `(org-archived ((t (:foreground ,color-fg-dim))))
   `(org-block ((t (:foreground ,color-fg :background ,color-bg-alt :inherit fixed-pitch :extend t))))
   `(org-block-begin-line ((t (:foreground ,color-fg-dim))))
   `(org-block-end-line ((t (:foreground ,color-fg-dim))))
   `(org-checkbox ((t (:foreground ,color-fg-alt :background ,color-bg-alt :weight bold :inherit fixed-pitch))))
   `(org-code ((t (:background ,color-bg-alt :inherit fixed-pitch))))
   `(org-date ((t (:foreground ,color-bright))))
   `(org-date-selected ((t (:foreground ,color-bg :background ,color-bright :weight bold))))
   `(org-document-info ((t (:foreground ,color-middle))))
   `(org-document-info-keyword ((t (:foreground ,color-fg-dim))))
   `(org-document-title ((t (:foreground ,color-fg-alt :weight bold))))
   `(org-done ((t (:foreground ,color-middle :background ,color-light :weight normal))))
   `(org-drawer ((t (:inherit font-lock-comment-face))))
   `(org-headline-done ((t (:foreground ,color-fg-dim))))
   `(org-hide ((t (:foreground ,color-bg))))
   `(org-latex-and-related ((t (:foreground ,color-dark :italic t))))
   `(org-link ((t (:inherit link))))
   `(org-meta-line ((t (:foreground ,color-fg-dim))))
   `(org-mode-line-clock ((t (:background unspecified))))
   `(org-table ((t (:foreground ,color-fg :inherit fixed-pitch-serif))))
   `(org-tag ((t (:foreground ,color-fg-dim :slant italic :weight normal))))
   `(org-todo ((t (:foreground ,color-strong :background ,color-strong-light :weight normal))))
   `(org-verbatim ((t (:inherit font-lock-string-face))))

   ;; org-tree-slide
   `(org-tree-slide-header-overlay-face ((t (:inherit font-lock-comment-face :foreground unspecified :background unspecified))))

   ;; shortdoc
   `(shortdoc-heading ((t (:inherit outline-1))))

   ;; compilation
   `(compilation-column-number ((t (:foreground ,color-fg :underline t))))
   `(compilation-error ((t (:foreground ,color-strong))))
   `(compilation-info ((t (:foreground ,color-dark))))
   `(compilation-line-number ((t (:foreground ,color-fg :weight bold :underline t))))
   `(compilation-mode-line-fail ((t (:foreground ,color-strong :background ,color-strong-light))))
   `(compilation-mode-line-exit ((t (:foreground ,color-dark :background ,color-light))))
   `(compilation-warning ((t (:foreground ,color-bright))))

   ;; whitespace
   `(whitespace-trailing ((t (:background ,color-strong-light))))
   `(whitespace-line ((t (:inherit whitespace-trailing))))
   `(whitespace-space (( t(:foreground ,color-dark-soft))))
   `(whitespace-newline ((t (:inherit whitespace-space))))
   `(whitespace-empty ((t (:inherit whitespace-line))))

   ;; smartparens
   `(sp-pair-overlay-face ((t (:background ,color-bright-light))))
   `(sp-show-pair-match-content ((t (:inherit show-paren-match-expression))))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,color-fg-dim))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,color-bright-soft))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,color-dark-soft))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,color-strong-soft))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,color-fg-dim))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,color-bright-soft))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,color-dark-soft))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,color-strong-soft))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,color-fg-dim))))
   `(rainbow-delimiters-unmatched-face ((t (:inherit show-paren-mismatch))))

   ;; paren-face
   `(parenthesis ((t (:foreground ,color-fg-dim :weight light))))

   ;; git-commit
   `(git-commit-summary ((t (:foreground ,color-fg))))
   `(git-commit-comment-heading ((t (:slant italic :inherit font-lock-comment-face))))
   `(git-commit-comment-branch-local ((t (:slant italic :weight bold))))
   `(git-commit-comment-branch-remote ((t (:slant italic :weight bold))))
   `(git-commit-comment-file ((t (:foreground ,color-bright :background ,color-bright-light))))
   `(git-commit-comment-action ((t (:weight bold :inherit font-lock-comment-face))))

   ;; magit
   `(magit-branch-local ((t (:weight bold))))
   `(magit-branch-remote ((t (:weight bold :slant italic))))
   `(magit-tag ((t (:foreground ,color-dark :background unspecified :inherit italic))))
   `(magit-hash ((t (:foreground ,color-bright))))
   `(magit-section-title ((t (:foreground ,color-fg :background unspecified))))
   `(magit-section-heading ((t (:background unspecified :foreground ,color-fg))))
   `(magit-section-heading-selection ((t (:inherit region))))
   `(magit-section-highlight ((t (:background ,color-bg-alt))))
   `(magit-item-highlight ((t (:foreground ,color-fg :background ,color-bg-alt))))
   `(magit-log-author ((t (:foreground ,color-dark))))
   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diffstat-added ((t (:foreground ,color-dark))))
   `(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diffstat-removed ((t (:foreground ,color-strong))))
   `(magit-diff-removed-highlight ((t (:inherit magit-diff-removed))))
   `(magit-diff-context ((t (:inherit diff-context))))
   `(magit-diff-context-highlight ((t (:foreground ,color-fg-dim :inherit magit-section-highlight))))
   `(magit-popup-argument ((t (:inherit font-lock-function-name-face))))
   `(magit-popup-disabled-argument ((t (:inherit font-lock-comment-face))))
   `(magit-process-ok ((t (:inherit success))))
   `(magit-diff-hunk-heading ((t (:background ,color-bg :inherit header-line :underline (:color ,color-fg)))))
   `(magit-diff-hunk-heading-highlight ((t (:inherit magit-section-highlight))))
   `(magit-filename ((t (:inherit git-commit-comment-file))))

   ;; git-gutter-fringe
   `(git-gutter-fr:modified ((t (:foreground ,color-bright-soft))))
   `(git-gutter-fr:added ((t (:foreground ,color-dark-soft))))
   `(git-gutter-fr:deleted ((t (:foreground ,color-strong-soft))))

   ;; company
   `(company-echo ((t (:inherit company-preview))))
   `(company-echo-common ((t (:inherit company-tooltip-common))))
   `(company-preview ((t (:foreground ,color-fg))))
   `(company-preview-common ((t (:foreground ,color-fg :background unspecified))))
   `(company-tooltip-search ((t (:inherit lazy-highlight))))
   `(company-tooltip-search-selection ((t (:foreground ,color-fg-alt :inherit company-tooltip-search))))
   `(company-tooltip ((t (:foreground ,color-fg :background ,color-bright-light))))
   `(company-tooltip-annotation ((t (:slant normal :inherit completions-annotations))))
   `(company-tooltip-annotation-selection ((t (:foreground ,color-fg-alt :slant normal :inherit completions-annotations))))
   `(company-tooltip-common ((t (:slant normal :inherit completions-common-part))))
   `(company-tooltip-common-selection ((t (:weight bold))))
   `(company-tooltip-selection ((t (:inherit highlight))))
   `(company-tooltip-scrollbar-thumb ((t (:background ,color-bright))))
   `(company-tooltip-scrollbar-track ((t (:background ,color-bright-light))))
   `(company-scrollbar-fg ((t (:inherit company-tooltip-scrollbar-thumb))))  ; obsolete
   `(company-scrollbar-bg ((t (:inherit company-tooltip-scrollbar-track))))  ; obsolete

   ;; corfu
   `(corfu-annotations ((t (:foreground ,color-fg))))
   `(corfu-bar ((t (:background ,color-bright))))
   `(corfu-border ((t (:background ,color-fg-dimmer))))
   `(corfu-current ((t (:inherit highlight))))
   `(corfu-default ((t (:background ,color-bright-light))))
   `(corfu-deprecated ((t (:strike-through t :inherit shadow))))
   `(corfu-indexed ((t (:foreground ,color-bright :background ,color-bright-light :height 0.8))))
   `(corfu-popupinfo ((t (:background ,color-bright-light))))

   ;; flymake
   `(flymake-error ((t (:underline (:color ,color-strong :style wave)))))
   `(flymake-warning ((t (:underline (:color ,color-bright :style wave)))))
   `(flymake-note ((t (:underline (:color ,color-middle :style wave)))))

   ;; flycheck
   `(flycheck-error ((t (:inherit flymake-error))))
   `(flycheck-warning ((t (:inherit flymake-warning))))
   `(flycheck-info ((t (:inherit flymake-note))))
   `(flycheck-fringe-error ((t (:inherit error))))
   `(flycheck-fringe-warning ((t (:inherit warning))))
   `(flycheck-fringe-info ((t (:inherit info))))
   `(flycheck-error-list-info ((t (:inherit flycheck-info))))
   `(flycheck-error-list-filename ((t (:foreground ,color-fg))))

   ;; lsp
   `(lsp-headerline-breadcrumb-path-face ((t (:foreground ,color-fg))))
   `(lsp-headerline-breadcrumb-path-error-face ((t (:inherit error))))
   `(lsp-headerline-breadcrumb-separator-face ((t (:foreground ,color-fg))))

   ;; eglot
   `(eglot-diagnostic-tag-deprecated-face ((t (:inherit shadow :strike-through t))))
   `(eglot-diagnostic-tag-unnecessary-face ((t (:inherit flymake-warning))))
   `(eglot-highlight-symbol-face ((t (:inherit lazy-highlight))))
   `(eglot-mode-line ((t (:foreground ,color-bg :weight bold))))

   ;; csv
   `(csv-separator-face ((t (:foreground ,color-strong))))

   ;; css
   `(css-selector ((t (:weight bold))))
   `(css-property ((t (:inherit font-lock-builtin-face))))

   ;; web-mode
   `(web-mode-html-tag-bracket-face ((t (:inherit shadow))))
   `(web-mode-html-tag-face ((t (:weight bold :inherit shadow))))
   `(web-mode-html-attr-name-face ((t (:inherit shadow :slant italic))))
   `(web-mode-css-selector-face ((t (:inherit css-selector))))
   `(web-mode-css-property-name-face ((t (:inherit css-property))))
   `(web-mode-css-color-face ((t (:foreground ,color-fg))))
   `(web-mode-current-element-highlight-face ((t (:inherit lazy-highlight))))
   `(web-mode-doctype-face ((t (:inherit shadow))))

   ;; slime
   `(slime-repl-prompt ((t (:inherit minibuffer-prompt))))
   `(slime-repl-input-face ((t (:foreground ,color-fg))))
   `(slime-repl-inputed-output-face ((t (:foreground ,color-dark))))
   `(slime-repl-output-mouseover-face ((t (:foreground ,color-bright :box unspecified))))
   `(slime-highlight-face ((t (:inherit highlight))))
   `(slime-highlight-edits-face ((t (:underline (:color ,color-fg-dimmer)))))
   `(slime-error-face ((t (:inherit error))))
   `(slime-warning-face ((t (:inherit flymake-warning))))
   `(slime-style-warning-face ((t (:inherit flymake-warning))))
   `(slime-reader-conditional-face ((t (:slant italic :inherit fixed-pitch-serif))))
   `(sldb-section-face ((t (:foreground ,color-fg-dim))))
   `(sldb-restartable-frame-line-face ((t (:weight bold))))

   ;; sly
   `(sly-db-topline-face ((t (:weight bold))))
   `(sly-action-face ((t (:foreground ,color-bright :weight bold))))
   `(sly-mode-line ((t (:foreground ,color-bg))))
   `(sly-mrepl-prompt-face ((t (:inherit minibuffer-prompt))))
   `(sly-mrepl-output-face ((t (:foreground ,color-fg))))
   `(sly-mrepl-note-face ((t (:inherit font-lock-warning-face))))
   `(sly-style-warning-face ((t (:inherit flymake-warning))))
   `(sly-db-condition-face ((t (:foreground ,color-strong :background ,color-strong-light :extend t))))
   `(sly-db-restart-face ((t (:inherit package-description))))
   `(sly-db-restart-number-face ((t (:foreground ,color-fg-alt :weight bold))))
   `(sly-db-section-face ((t (:inherit shadow))))
   `(sly-db-frame-label-face ((t (:inherit shadow))))
   `(sly-db-frame-line-face ((t (:foreground ,color-fg))))
   `(sly-db-restartable-frame-line-face ((t (:weight bold))))
   `(sly-db-local-name-face ((t (:slant italic))))

   ;; geiser
   `(geiser-font-lock-repl-output ((t (:foreground ,color-dark))))
   `(geiser-font-lock-autodoc-identifier ((t (:inherit font-lock-keyword-face))))
   `(geiser-font-lock-autodoc-current-arg ((t (:inherit eldoc-highlight-function-argument))))

   ;; cider
   `(cider-result-overlay-face ((t (:background ,color-bg-alt))))
   `(cider-fringe-good-face ((t (:foreground ,color-dark))))
   `(cider-warning-highlight-face ((t (:foreground ,color-bright :background ,color-bright-light :slant italic))))
   `(cider-test-error-face ((t (:inherit font-lock-warning-face))))
   `(cider-test-failure-face ((t (:inherit font-lock-warning-face))))
   `(cider-test-success-face ((t (:foreground ,color-middle :weight bold))))
   `(cider-repl-prompt-face ((t (:inherit minibuffer-prompt))))
   `(cider-repl-stdout-face ((t (:foreground ,color-fg))))
   `(cider-repl-stderr-face ((t (:inherit font-lock-warning-face))))
   `(cider-stacktrace-error-class-face ((t (:inherit font-lock-warning-face))))
   `(cider-error-highlight-face ((t (:inherit error))))

   ;; clojure-mode
   `(clojure-keyword-face ((t (:inherit font-lock-builtin-face))))

   ;; tuareg
   `(tuareg-font-double-semicolon-face ((t (:foreground ,color-middle))))
   `(tuareg-font-double-colon-face ((t (:foreground ,color-middle))))
   `(tuareg-font-lock-constructor-face ((t (:foreground ,color-fg-alt))))
   `(tuareg-font-lock-error-face ((t (:inherit error))))
   `(tuareg-font-lock-governing-face ((t (:foreground ,color-bright :weight bold))))
   `(tuareg-font-lock-interactive-output-face ((t (:foreground ,color-dark))))
   `(tuareg-font-lock-interactive-error-face ((t (:inherit font-lock-warning-face))))
   `(tuareg-font-lock-interactive-directive-face ((t (:foreground ,color-middle))))
   `(tuareg-font-lock-label-face ((t (:inherit shadow))))
   `(tuareg-font-lock-line-number-face ((t (:inherit linum))))
   `(tuareg-font-lock-module-face ((t (:inherit shadow))))
   `(tuareg-font-lock-operator-face ((t (:inherit font-lock-operator-face))))

   ;; caml
   `(ocaml-help-face ((t (:inherit highlight))))

   ;; merlin
   `(merlin-compilation-error-face ((t (:inherit error :underline (:color ,color-strong :style wave)))))
   `(merlin-type-face ((t (:background ,color-light))))

   ;; merlin-eldoc
   `(merlin-eldoc-occurrences-face ((t (:inherit lazy-highlight))))

   ;; utop
   `(utop-frozen ((t (:foreground ,color-fg))))
   `(utop-prompt ((t (:inherit minibuffer-prompt))))
   `(utop-error  ((t (:inherit error))))
   `(utop-stderr ((t (:inherit font-lock-warning-face))))
   `(utop-stdout ((t (:inherit tuareg-font-lock-interactive-output-face))))

   ;; haskell-mode
   `(haskell-operator-face ((t (:inherit font-lock-operator-face))))
   `(haskell-warning-face ((t (:inherit flymake-warning))))
   `(haskell-interactive-face-compile-warning ((t (:inherit compilation-warning))))

   ;; selectrum
   `(selectrum-mouse-highlight ((t (:background unspecified :underline t :extend t))))
   `(selectrum-prescient-primary-highlight ((t (:inherit completions-common-part))))

   ;; marginalia
   `(marginalia-archive ((t (:inherit unspecified))))
   `(marginalia-key ((t (:inherit unspecified))))
   `(marginalia-number ((t (:inherit unspecified))))
   `(marginalia-file-priv-dir ((t (:weight bold))))
   `(marginalia-file-priv-read ((t (:foreground ,color-fg))))
   `(marginalia-file-priv-write ((t (:foreground ,color-strong))))
   `(marginalia-file-priv-exec ((t (:foreground ,color-dark))))

   ;; consult
   `(consult-preview-line ((t (:inherit highlight))))
   `(consult-preview-cursor ((t (:background ,color-bg :underline unspecified))))
   `(consult-separator-face ((t (:foreground ,color-fg-dimmer))))

   ;; helm
   `(helm-candidate-number ((t (:foreground ,color-fg-dim :background unspecified))))
   `(helm-command-active-mode ((t (:foreground ,color-strong))))
   `(helm-source-header ((t (:inherit font-lock-comment-face :background unspecified :foreground unspecified))))
   `(helm-selection ((t (:inherit highlight))))
   `(helm-selection-line ((t ())))
   `(helm-separator ((t (:foreground ,color-fg-dimmer))))
   `(helm-prefarg ((t (:foreground ,color-dark))))
   `(helm-ff-file ((t (:foreground ,color-fg))))
   `(helm-ff-file-extension ((t (:foreground ,color-bright))))
   `(helm-ff-directory ((t (:inherit dired-directory :foreground unspecified))))
   `(helm-ff-executable ((t (:inherit eshell-ls-executable :foreground unspecified))))
   `(helm-ff-file-extension ((t (:foreground unspecified :background unspecified))))
   `(helm-ff-invalid-symlink ((t (:slant italic :inherit error))))
   `(helm-ff-suid ((t (:foreground ,color-bg :background ,color-strong))))
   `(helm-ff-symlink ((t (:inherit dired-symlink))))
   `(helm-ff-truename ((t (:inherit font-lock-doc-face))))
   `(helm-ff-prefix ((t (:background unspecified))))
   `(helm-ff-dotted-directory ((t (:background unspecified :foreground ,color-middle))))
   `(helm-grep-cmd-line ((t ())))
   `(helm-grep-file ((t (:background ,color-bright-light))))
   `(helm-grep-finish ((t (:inherit helm-grep-file))))
   `(helm-grep-lineno ((t (:inherit line-number))))
   `(helm-grep-match ((t (:inherit helm-match))))
   `(helm-M-x-key ((t (:inherit help-key-binding))))
   `(helm-M-x-short-doc ((t (:inherit font-lock-doc-face))))
   `(helm-buffer-file ((t (:foreground ,color-fg))))
   `(helm-buffer-archive ((t (:inherit eshell-ls-archive))))
   `(helm-buffer-directory ((t (:inherit dired-directory))))
   `(helm-buffer-not-saved ((t (:foreground ,color-strong :underline (:color ,color-strong :style wave)))))
   `(helm-buffer-saved-out ((t (:inherit helm-buffer-not-saved))))
   `(helm-buffer-modified ((t (:foreground ,color-bright :underline (:color ,color-bright :style wave)))))
   `(helm-buffer-process ((t (:foreground ,color-dark))))
   `(helm-buffer-size ((t (:foreground ,color-dark))))
   `(helm-match ((t (:inherit match))))
   `(helm-match-item ((t (:inherit helm-match))))
   `(helm-bookmark-addressbook ((t (:foreground ,color-fg))))
   `(helm-bookmark-directory ((t (:inherit dired-directory))))
   `(helm-bookmark-file ((t (:foreground ,color-fg))))
   `(helm-bookmark-file-not-found ((t (:foreground ,color-fg :slant italic))))
   `(helm-bookmark-gnus ((t (:foreground ,color-fg))))
   `(helm-bookmark-info ((t (:foreground ,color-fg))))
   `(helm-bookmark-man ((t (:foreground ,color-fg))))
   `(helm-bookmark-w3m ((t (:foreground ,color-fg))))

   ;; adoc-mode
   `(markup-code-face ((t (:inherit markup-verbatim-face))))
   `(markup-complex-replacement-face ((t (:background ,color-bright-light :foreground ,color-fg))))
   `(markup-gen-face ((t (:foreground unspecified))))
   `(markup-list-face ((t (:weight bold))))
   `(markup-meta-face ((t (:height 1.0 :foreground ,color-fg-dim))))
   `(markup-meta-hide-face ((t (:height 1.0 :foreground ,color-bright))))
   `(markup-passthrough-face ((t (:inherit markup-dark))))
   `(markup-reference-face ((t (:underline unspecified :foreground ,color-dark))))
   `(markup-replacement-face ((t (:family unspecified :foreground ,color-dark))))
   `(markup-secondary-text-face ((t (:height 1.0 :foreground ,color-dark))))
   `(markup-table-cell-face ((t (:inherit table-cell))))
   `(markup-title-0-face ((t (:height 1.2 :inherit markup-gen-face))))
   `(markup-title-1-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-2-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-3-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-4-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-5-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-typewriter-face ((t (:inherit unspecified))))
   `(markup-verbatim-face ((t (:foreground ,color-dark))))

   ;; highlight-indent-guides
   `(highlight-indent-guides-odd-face ((t (:background ,color-bright))))
   `(highlight-indent-guides-even-face ((t (:background unspecified))))

   ;; notmuch
   `(notmuch-search-unread-face ((t (:foreground ,color-bright))))
   `(notmuch-tag-face ((t (:foreground ,color-dark))))
   `(notmuch-tree-match-author-face ((t (:foreground ,color-middle))))
   `(notmuch-tree-no-match-face ((t (:foreground ,color-dark))))
   `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tree-match-author-face))))
   `(notmuch-tag-unread-face ((t (:foreground ,color-fg :background ,color-middle))))
   `(notmuch-message-summary-face ((t (:foreground ,color-dark))))

   ;; switch-window
   `(switch-window-background ((t (:foreground ,color-fg-dim))))
   `(switch-window-label ((t (:foreground ,color-strong :height 1.5))))

   ;; telega
   `(telega-msg-heading ((t (:foreground ,color-dark :background unspecified :inherit unspecified))))
   `(telega-msg-inline-reply ((t (:foreground ,color-bright :inherit unspecified))))
   `(telega-entity-type-texturl ((t (:inherit unspecified :foreground ,color-dark))))

   ;; beancount
   `(beancount-date ((t (:inherit italic :foreground unspecified))))
   `(beancount-account ((t (:foreground ,color-fg))))

   ;; w3m
   `(w3m-anchor ((t (:inherit link))))
   `(w3m-arrived-anchor ((t (:inherit link-visited))))
   `(w3m-current-anchor ((t (:inherit highlight))))
   `(w3m-error ((t (:inherit error))))
   `(w3m-header-line-content ((t (:inherit header-line))))
   `(w3m-header-line-background ((t (:inherit header-line))))
   `(w3m-header-line-title ((t (:inherit header-line))))
   `(w3m-form ((t (:inherit widget-field))))
   `(w3m-form-button ((t (:inherit custom-button))))
   `(w3m-form-button-mouse ((t (:inherit custom-button-mouse))))
   `(w3m-form-button-pressed ((t (:inherit custom-button-pressed))))

   ;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,color-middle))))
   `(elfeed-search-title-face ((t (:foreground ,color-fg-dim))))
   `(elfeed-search-unread-title-face ((t (:foreground ,color-fg))))
   `(elfeed-search-feed-face ((t (:foreground ,color-middle))))
   `(elfeed-search-tag-face ((t (:foreground ,color-fg))))
   `(elfeed-search-last-update-face ((t (:inherit font-lock-comment-face))))
   `(elfeed-search-unread-count-face ((t (:weight bold))))
   `(elfeed-search-filter-face ((t (:foreground ,color-bright))))
   `(elfeed-log-debug-level-face ((t (:foreground ,color-middle))))
   `(elfeed-log-error-level-face ((t (:inherit error))))
   `(elfeed-log-info-level-face ((t (:foreground ,color-dark))))
   `(elfeed-log-warn-level-face ((t (:inherit warning))))

   ;; rg / ripgrep
   `(rg-file-tag-face ((t (:foreground ,color-dark))))
   `(rg-filename-face ((t (:foreground ,color-dark :weight bold))))
   `(rg-line-number-face ((t (:inherit line-number))))
   `(rg-match-face ((t (:underline t :inherit match))))

   ;; wgrep
   `(wgrep-face ((t (:foreground ,color-bright :background ,color-bright-light))))
   `(wgrep-file-face ((t (:background ,color-bright-light))))
   `(wgrep-done-face ((t (:inherit wgrep-file-face))))

   ;; orderless
   `(orderless-match-face-0 ((t (:inherit match))))
   `(orderless-match-face-1 ((t (:inherit match :foreground ,color-bright))))
   `(orderless-match-face-2 ((t (:inherit match :foreground ,color-dark))))
   `(orderless-match-face-3 ((t (:inherit match :foreground ,color-strong))))

   ;; yasnippet
   `(yas-field-highlight-face ((t (:background ,color-bright-light))))

   ;; which-key
   `(which-key-key-face ((t (:foreground ,color-fg :weight bold))))
   `(which-key-group-description-face ((t (:inherit font-lock-comment-face :weight bold))))
   `(which-key-command-description-face ((t (:inherit font-lock-comment-face))))

   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'beach)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; beach-theme.el ends here
