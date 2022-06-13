;;; nude-beach-theme.el --- A light, sunny and calm Emacs theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2021–2022 Dan Dee

;; Author: Dan Dee <monkeyjunglejuice@pm.me>
;; URL: https://github.com/monkeyjunglejuice/nude-beach-theme-emacs
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces, theme
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This is not your "angry fruit salad". The light Nude Beach theme draws
;; inspiration from monochrome eink-like themes. So it appears sunny and calm,
;; wearing colors sparingly, and only where really really neccessary.
;;
;; Other themes:
;; - "The Matrix" https://github.com/monkeyjunglejuice/matrix-emacs-theme

;;; Code:

(deftheme nude-beach "A light, sunny and calm Emacs theme.")

;; Colors
(let* ((color-bg           "#fff8dc")
       (color-bg-alt       "#f4efd2")
       (color-fg           "#43402d")
       (color-fg-alt       "#000000")
       (color-fg-dim       "#8e8a72")
       (color-fg-dimmer    "#d7d3b7")
       (color-light        "#cff7d8")
       (color-middle       "#56ae9a")
       (color-dark         "#0e7b6e")
       (color-bright       "#d6761f")
       (color-strong       "#9e240a")
       (color-strong-light "#fcc9be")
       (color-bright-light "#ffefc2"))

  (custom-theme-set-faces
   'nude-beach
   `(default ((t (:background ,color-bg :foreground ,color-fg))))
   `(cursor ((t (:background ,color-fg-alt))))
   `(region ((t (:foreground ,color-dark :background ,color-light))))
   `(success ((t (:foreground ,color-dark :background ,color-light :extend t))))
   `(warning ((t (:foreground ,color-bright :background ,color-bright-light :extend t))))
   `(error ((t (:foreground ,color-strong :background ,color-strong-light :extend t))))
   `(secondary-selection ((t (:background ,color-bg-alt))))
   `(mode-line ((t (:background ,color-fg :foreground ,color-bg :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,color-bg :weight bold))))
   `(mode-line-inactive ((t (:background ,color-fg-dim :foreground ,color-bg))))
   `(fringe ((t (:background ,color-bg))))
   `(vertical-border ((t (:foreground ,color-fg :background nil))))
   `(minibuffer-prompt ((t (:foreground ,color-fg-alt :weight bold))))
   `(isearch ((t (:foreground ,color-fg-alt :background ,color-light :underline t))))
   `(isearch-fail ((t (:inherit error))))
   `(lazy-highlight ((t (:background ,color-bright-light :underline (:foreground ,color-fg-alt)))))
   `(link ((t (:foreground ,color-dark :underline t))))
   `(link-visited ((t (:foreground ,color-middle :underline t))))
   `(button ((t (:inherit link))))
   `(help-face-button ((t (:inherit button))))
   `(help-key-binding ((t (:foreground ,color-fg-alt :background ,color-bg-alt :box (:color ,color-fg-dim) :inherit fixed-pitch-serif))))
   `(header-line ((t (:foreground ,color-fg-dim :background ,color-bg-alt))))
   `(shadow ((t (:foreground ,color-fg-dim))))
   `(show-paren-match ((t (:foreground ,color-fg-alt :background ,color-light :weight bold))))
   `(show-paren-mismatch ((t (:inherit error))))
   `(highlight ((t (:foreground ,color-fg-alt :background ,color-light :weight bold :underline (:color ,color-fg-alt)))))
   `(match ((t (:inherit highlight))))
   `(hl-line ((t (:underline (:color ,color-fg-dim) :extend t))))
   `(separator-line ((t (:height 0.1 :background ,color-fg-dimmer))))
   `(widget-field ((t (:foreground ,color-fg-alt :background ,color-bg-alt))))
   `(trailing-whitespace ((t (:background ,color-strong-light))))
   `(escape-glyph ((t (:inverse-video t))))

   `(font-lock-face ((t (:foreground ,color-middle))))
   `(font-lock-builtin-face ((t (:foreground ,color-strong))))
   `(font-lock-comment-face ((t (:inherit shadow))))
   `(font-lock-constant-face ((t (:foreground ,color-fg))))
   `(font-lock-doc-face ((t (:foreground ,color-middle :slant italic :inherit fixed-pitch-serif))))
   `(font-lock-function-name-face ((t (:foreground ,color-fg :weight bold :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,color-bright))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,color-bright))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,color-bright))))
   `(font-lock-string-face ((t (:foreground ,color-dark :inherit fixed-pitch-serif))))
   `(font-lock-type-face ((t (:foreground ,color-fg :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,color-fg :slant italic))))
   `(font-lock-warning-face ((t (:foreground ,color-strong :slant italic))))

   ;; shell-mode
   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face))))
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

   ;; proced
   `(proced-mark ((t (:inherit dired-mark))))
   `(proced-marked ((t (:inherit dired-marked))))

   ;; eshell
   `(eshell-prompt ((t (:inherit minibuffer-prompt))))
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
   `(completions-common-part ((t (:weight bold))))
   `(icomplete-first-match ((t (:foreground ,color-fg-alt :weight bold :underline t))))

   ;; diff
   `(diff-added ((t (:foreground ,color-dark :background ,color-light))))
   `(diff-removed ((t (:foreground ,color-strong :background ,color-strong-light))))
   `(diff-context ((t (:inherit shadow))))
   `(diff-file-header ((t (:bold t :background ,color-bright-light :weight bold))))
   `(diff-header ((t (:background ,color-bright-light :foreground ,color-fg))))

   ;; package manager
   `(package-name ((t (:inherit link))))
   `(package-description ((t (:slant italic :inherit fixed-pitch-serif))))
   `(package-status-installed ((t (:foreground ,color-middle))))
   `(package-status-dependency ((t (:foreground ,color-middle :slant italic))))
   `(package-status-built-in ((t (:foreground ,color-fg-dim :slant italic))))
   `(package-status-incompat ((t (:slant italic :inherit font-lock-warning-face))))

   ;; customization
   `(custom-group-tag ((t (:inherit bold))))
   `(custom-variable-tag ((t (:weight bold))))
   `(custom-variable-obsolete ((t (:foreground ,color-fg-dim :inherit custom-variable-tag))))
   `(custom-documentation ((t (:inherit fixed-pitch-serif))))
   `(custom-visibility ((t (:inherit custom-documentation :underline t))))
   `(custom-state ((t (:foreground ,color-bright :slant italic))))
   `(custom-button ((t (:foreground ,color-bg :background ,color-fg))))
   `(custom-button-mouse ((t (:foreground ,color-fg :background ,color-bright-light))))
   `(custom-button-pressed ((t (:foreground ,color-bright :background ,color-bright-light))))
   `(custom-button-pressed-unraised ((t (:inherit custom-button-pressed))))
   `(custom-button-unraised ((t (:inherit custom-button))))

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

   ;; tex
   `(font-latex-sedate-face ((t (:foreground ,color-dark))))
   `(font-latex-math-face ((t (:foreground ,color-fg))))
   `(font-latex-script-char-face ((t (:inherit font-latex-math-face))))

   ;; outline
   `(outline-1 ((t (:foreground ,color-fg-alt :weight bold :height 1.2))))
   `(outline-2 ((t (:foreground ,color-fg-alt :weight bold))))
   `(outline-3 ((t (:foreground ,color-fg-alt :weight bold))))
   `(outline-4 ((t (:foreground ,color-fg-alt :weight bold))))
   `(outline-5 ((t (:foreground ,color-fg-alt :weight bold))))
   `(outline-6 ((t (:foreground ,color-fg-alt :weight bold))))
   `(outline-7 ((t (:foreground ,color-fg-alt :weight bold))))
   `(outline-8 ((t (:foreground ,color-fg-alt :weight bold))))

   ;; org-mode
   `(org-hide ((t (:foreground ,color-bg))))
   `(org-table ((t (:foreground ,color-fg :inherit fixed-pitch-serif))))
   `(org-code ((t (:background ,color-bg-alt :inherit fixed-pitch))))
   `(org-date ((t (:foreground ,color-bright))))
   `(org-todo ((t (:foreground ,color-bright :box t :weight normal))))
   `(org-done ((t (:foreground ,color-middle :box t :weight normal))))
   `(org-headline-done ((t (:foreground ,color-fg-dim))))
   `(org-latex-and-related ((t (:foreground ,color-dark :italic t))))
   `(org-checkbox ((t (:foreground ,color-fg-alt :weight normal :inherit fixed-pitch))))
   `(org-verbatim ((t (:inherit font-lock-string-face))))
   `(org-mode-line-clock ((t (:background nil))))
   `(org-document-title ((t (:foreground ,color-fg-alt :weight bold))))
   `(org-drawer ((t (:inherit font-lock-comment-face))))
   `(org-block ((t (:foreground ,color-fg :background ,color-bg-alt :inherit fixed-pitch :extend t))))
   `(org-block-begin-line ((t (:inherit font-lock-comment-face))))
   `(org-block-end-line ((t (:inherit font-lock-comment-face))))
   `(org-meta-line ((t (:inherit font-lock-comment-face))))
   `(org-document-info-keyword ((t (:inherit font-lock-comment-face))))
   `(org-document-info ((t (:foreground ,color-dark))))
   `(org-archived ((t (:foreground ,color-fg-dim))))

   ;; org-tree-slide
   `(org-tree-slide-header-overlay-face ((t (:inherit font-lock-comment-face :foreground nil :background nil))))

   ;; shortdoc
   `(shortdoc-heading ((t (:inherit outline-1))))

   ;; compilation
   `(compilation-error ((t (:inherit error))))
   `(compilation-warning ((t (:inherit warning))))
   `(compilation-info ((t (:foreground ,color-dark))))

   ;; whitespace
   `(whitespace-trailing ((t (:background ,color-strong-light))))
   `(whitespace-line ((t (:inherit whitespace-trailing))))
   `(whitespace-space (( t(:foreground ,color-middle))))
   `(whitespace-newline ((t (:inherit whitespace-space))))
   `(whitespace-empty ((t (:inherit whitespace-line))))

   ;; smart parens
   `(sp-pair-overlay-face ((t (:background ,color-bright-light))))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,color-fg :weight light))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,color-fg-dim :weight light))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,color-fg-dim :weight light))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,color-fg-dim :weight light))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,color-fg-dim :weight light))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,color-fg-dim :weight light))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,color-fg-dim :weight light))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,color-fg-dim :weight light))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,color-fg-dim :weight light))))
   `(rainbow-delimiters-unmatched-face ((t (:inherit error))))

   ;; paren face
   `(parenthesis ((t (:inherit shadow :weight light))))

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
   `(magit-tag ((t (:foreground ,color-dark :background nil :inherit italic))))
   `(magit-hash ((t (:foreground ,color-bright))))
   `(magit-section-title ((t (:foreground ,color-fg :background nil))))
   `(magit-section-heading ((t (:background nil :foreground ,color-fg))))
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
   `(magit-diff-hunk-heading ((t (:background ,color-bg :inherit header-line :underline t))))
   `(magit-diff-hunk-heading-highlight ((t (:inherit magit-section-highlight))))
   `(magit-filename ((t (:inherit git-commit-comment-file))))

   ;; git-gutter-fringe
   `(git-gutter-fr:modified ((t (:foreground ,color-dark))))
   `(git-gutter-fr:added ((t (:foreground ,color-dark))))
   `(git-gutter-fr:deleted ((t (:foreground ,color-dark))))

   ;; company
   `(company-echo ((t (:inherit company-preview))))
   `(company-echo-common ((t (:inherit company-tooltip-common))))
   `(company-preview ((t (:foreground ,color-fg))))
   `(company-preview-common ((t (:foreground ,color-fg :background nil))))
   `(company-tooltip-search ((t (:inherit lazy-highlight))))
   `(company-tooltip-search-selection ((t (:foreground ,color-fg-alt :inherit company-tooltip-search))))
   `(company-tooltip ((t (:foreground ,color-fg :background ,color-bright-light))))
   `(company-tooltip-annotation ((t (:foreground ,color-fg))))
   `(company-tooltip-annotation-selection ((t (:foreground ,color-fg-alt :weight normal))))
   `(company-tooltip-common ((t (:foreground ,color-bright))))
   `(company-tooltip-common-selection ((t (:foreground ,color-bright))))
   `(company-tooltip-selection ((t (:foreground ,color-fg-alt :background ,color-bright-light :weight bold :underline (:color ,color-bright)))))
   `(company-scrollbar-bg ((t (:background ,color-bright-light))))
   `(company-scrollbar-fg ((t (:background ,color-bright))))

   ;; flymake
   `(flymake-error ((t (:underline (:color ,color-strong :style wave)))))
   `(flymake-warning ((t (:underline (:color ,color-bright :style wave)))))
   `(flymake-note ((t (:underline (:color ,color-middle :style wave)))))

   ;; flycheck
   `(flycheck-error ((t (:underline (:color ,color-strong :style wave)))))
   `(flycheck-fringe-error ((t (:foreground ,color-strong :background ,color-strong-light))))
   `(flycheck-warning ((t (:underline (:color ,color-bright :style wave)))))
   `(flycheck-fringe-warning ((t (:foreground ,color-bright :background ,color-bright-light))))
   `(flycheck-info ((t (:underline (:color ,color-middle :style wave)))))
   `(flycheck-fringe-info ((t (:foreground ,color-middle :background ,color-light))))

   ;; lsp
   `(lsp-headerline-breadcrumb-path-face ((t (:foreground ,color-fg))))
   `(lsp-headerline-breadcrumb-path-error-face ((t (:inherit error))))
   `(lsp-headerline-breadcrumb-separator-face ((t (:foreground ,color-fg))))

   ;; eglot
   `(eglot-highlight-symbol-face ((t (:inherit lazy-highlight))))

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
   `(web-mode-doctype-face ((t (:inherit shadow))))
   `(web-mode-css-color-face ((t (:foreground ,color-fg))))

   ;; slime
   `(slime-repl-inputed-output-face ((t (:foreground ,color-dark))))
   `(slime-repl-output-mouseover-face ((t (:foreground ,color-bright :box nil))))
   `(slime-repl-input-face ((t (:foreground ,color-fg))))
   `(slime-repl-prompt ((t (:inherit minibuffer-prompt))))
   `(slime-highlight-edits-face ((t (:underline (:color ,color-fg-dimmer)))))
   `(slime-highlight-face ((t (:inherit highlight))))
   `(slime-error-face ((t (:inherit error))))
   `(slime-warning-face ((t (:inherit warning))))
   `(slime-style-warning-face ((t (:inherit warning))))
   `(sldb-section-face ((t (:foreground ,color-fg-dim :weight bold))))
   `(sldb-restartable-frame-line-face ((t (:inherit link))))

   ;; geiser
   `(geiser-font-lock-repl-output ((t (:foreground ,color-dark))))

   ;; cider
   `(cider-result-overlay-face ((t (:background ,color-bright-light))))
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
   `(tuareg-font-lock-constructor-face ((t (:foreground ,color-fg))))
   `(tuareg-font-lock-interactive-output-face ((t (:foreground ,color-dark))))
   `(tuareg-font-lock-interactive-error-face ((t (:inherit font-lock-warning-face))))
   `(tuareg-font-lock-interactive-directive-face ((t (:foreground ,color-middle))))
   `(tuareg-font-lock-operator-face ((t (:foreground ,color-fg-alt))))
   `(tuareg-font-lock-module-face ((t (:inherit shadow))))
   `(tuareg-font-lock-governing-face ((t (:foreground ,color-bright :weight bold))))
   `(tuareg-font-lock-label-face ((t (:inherit shadow))))
   `(tuareg-font-lock-line-number-face ((t (:inherit linum))))
   `(tuareg-font-double-semicolon-face ((t (:inherit tuareg-font-lock-interactive-directive-face))))
   `(tuareg-font-double-colon-face ((t (:inherit tuareg-font-double-semicolon-face))))
   `(tuareg-font-lock-error-face ((t (:inherit error))))

   ;; merlin
   `(merlin-compilation-error-face ((t (:inherit error :underline (:color ,color-strong :style wave)))))
   `(merlin-type-face ((t (:background ,color-light))))

   ;; merlin-eldoc
   `(merlin-eldoc-occurrences-face ((t (:inherit highlight))))

   ;; utop
   `(utop-frozen ((t (:foreground ,color-fg))))
   `(utop-prompt ((t (:inherit minibuffer-prompt))))
   `(utop-error  ((t (:inherit error))))

   ;; selectrum
   `(selectrum-mouse-highlight ((t (:background nil :underline t :extend t))))
   `(selectrum-prescient-primary-highlight ((t (:inherit completions-common-part))))

   ;; marginalia
   `(marginalia-archive ((t (:inherit nil))))
   `(marginalia-key ((t (:inherit nil))))
   `(marginalia-number ((t (:inherit nil))))
   `(marginalia-file-priv-dir ((t (:weight bold))))
   `(marginalia-file-priv-read ((t (:foreground ,color-fg))))
   `(marginalia-file-priv-write ((t (:foreground ,color-strong))))
   `(marginalia-file-priv-exec ((t (:foreground ,color-dark))))

   ;; consult
   `(consult-preview-line ((t (:inherit highlight))))
   `(consult-preview-cursor ((t (:background ,color-bg :underline nil))))

   ;; helm
   `(helm-candidate-number ((t (:foreground ,color-fg-dim :background nil))))
   `(helm-source-header ((t (:inherit font-lock-comment-face :background unspecified :foreground unspecified))))
   `(helm-selection ((t (:inherit highlight))))
   `(helm-prefarg ((t (:foreground ,color-dark))))
   `(helm-ff-file ((t (:foreground ,color-fg))))
   `(helm-ff-directory ((t (:inherit dired-directory :foreground unspecified))))
   `(helm-ff-executable ((t (:inherit eshell-ls-executable :foreground unspecified))))
   `(helm-ff-file-extension ((t (:foreground nil :background nil))))
   `(helm-ff-invalid-symlink ((t (:slant italic :inherit error))))
   `(helm-ff-symlink ((t (:inherit dired-symlink))))
   `(helm-ff-prefix ((t (:background nil))))
   `(helm-ff-dotted-directory ((t (:background nil :foreground ,color-middle))))
   `(helm-M-x-key ((t (:foreground ,color-bright))))
   `(helm-M-x-short-doc ((t (:inherit font-lock-doc-face))))
   `(helm-buffer-file ((t (:foreground ,color-fg))))
   `(helm-buffer-archive ((t (:inherit eshell-ls-archive))))
   `(helm-buffer-directory ((t (:inherit dired-directory))))
   `(helm-buffer-not-saved ((t (:foreground ,color-strong :underline (:color ,color-strong :style wave)))))
   `(helm-buffer-saved-out ((t (:inherit helm-buffer-not-saved))))
   `(helm-buffer-modified ((t (:foreground ,color-bright :underline (:color ,color-bright :style wave)))))
   `(helm-buffer-process ((t (:foreground ,color-dark))))
   `(helm-buffer-size ((t (:foreground ,color-dark))))
   `(helm-match ((t (:inherit completions-common-part))))
   `(helm-bookmark-addressbook ((t (:inherit default))))
   `(helm-bookmark-directory ((t (:inherit dired-directory))))
   `(helm-bookmark-file ((t (:inherit default))))
   `(helm-bookmark-file-not-found ((t (:inherit default :slant italic))))
   `(helm-bookmark-gnus ((t (:inherit default))))
   `(helm-bookmark-info ((t (:inherit default))))
   `(helm-bookmark-man ((t (:inherit default))))
   `(helm-bookmark-w3m ((t (:inherit default))))

   ;; adoc-mode
   `(markup-meta-hide-face ((t (:height 1.0 :foreground ,color-bright))))
   `(markup-meta-face ((t (:height 1.0 :foreground ,color-dark :family nil))))
   `(markup-reference-face ((t (:underline nil :foreground ,color-dark))))
   `(markup-gen-face ((t (:foreground nil))))
   `(markup-passthrough-face ((t (:inherit markup-dark))))
   `(markup-replacement-face ((t (:family nil :foreground ,color-dark))))
   `(markup-list-face ((t (:weight bold))))
   `(markup-secondary-text-face ((t (:height 1.0 :foreground ,color-dark))))
   `(markup-verbatim-face ((t (:foreground ,color-dark))))
   `(markup-code-face ((t (:inherit markup-verbatim-face))))
   `(markup-typewriter-face ((t (:inherit nil))))
   `(markup-complex-replacement-face ((t (:background ,color-bright-light :foreground ,color-fg))))
   `(markup-title-0-face ((t (:height 1.2 :inherit markup-gen-face))))
   `(markup-title-1-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-2-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-3-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-4-face ((t (:height 1.0 :inherit markup-gen-face))))
   `(markup-title-5-face ((t (:height 1.0 :inherit markup-gen-face))))

   ;; highlight-indent-guides
   `(highlight-indent-guides-odd-face ((t (:background ,color-bright))))
   `(highlight-indent-guides-even-face ((t (:background nil))))

   ;; notmuch
   `(notmuch-search-unread-face ((t (:foreground ,color-bright))))
   `(notmuch-tag-face ((t (:foreground ,color-dark))))
   `(notmuch-tree-match-author-face ((t (:foreground ,color-middle))))
   `(notmuch-tree-no-match-face ((t (:foreground ,color-dark))))
   `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tree-match-author-face))))
   `(notmuch-tag-unread-face ((t (:foreground ,color-fg :background ,color-middle))))
   `(notmuch-message-summary-face ((t (:foreground ,color-dark))))

   ;; telega
   `(telega-msg-heading ((t (:foreground ,color-dark :background nil :inherit nil))))
   `(telega-msg-inline-reply ((t (:foreground ,color-bright :inherit nil))))
   `(telega-entity-type-texturl ((t (:inherit nil :foreground ,color-dark))))

   ;; beancount
   `(beancount-date ((t (:inherit italic :foreground nil))))
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
   `(elfeed-log-warn-level-face ((t (:inherit warning))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nude-beach)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; nude-beach-theme.el ends here
