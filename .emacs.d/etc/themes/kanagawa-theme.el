;;; kanagawa-theme.el --- Retro elegant theme -*- lexical-binding: t -*-

;; Copyright (C) 2023 Meritamen <meritamen@sdf.org>

;; Author: Meritamen <meritamen@sdf.org>
;; URL: https://github.com/Meritamen/kanagawa-theme
;; Version: 0.0.1
;; Package-Requires: ((autothemer "0.2.18") (emacs "28.1"))
;; Created: 16 September 2023
;; Keywords: themes faces

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Kanagawa is a theme inspired by the colors of the famous painting by
;; Katsushika Hokusa. 
;; Original theme created by rebelot see: https://github.com/rebelot/kanagawa.nvim

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

;;; Customizations:

(defgroup kanagawa-theme nil
  "Kanagawa-theme options."
  :group 'faces)

(defcustom kanagawa-theme-comment-italic t
  "Enable italics for comments and also disable background."
  :type 'boolean
  :group 'kanagawa-theme)

(defcustom kanagawa-theme-keyword-italic t
  "Enable italics for keywords."
  :type 'boolean
  :group 'kanagawa-theme)

(autothemer-deftheme
  kanagawa "A theme inspired by the colors of the famous painting by Katsushika Hokusa"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (fuji-white		   "#DCD7BA" "#ffffff")
  (old-white		   "#C8C093" "#ffffff")

  (sumi-ink-0		   "#16161D" "#000000")
  (sumi-ink-1b	   "#1f1f28" "#000000")
  (sumi-ink-1		   "#1F1F28" "#080808")
  (sumi-ink-2		   "#2A2A37" "#121212")
  (sumi-ink-3		   "#363646" "#303030")
  (sumi-ink-4		   "#54546D" "#303030")

  (wave-blue-1 	   "#223249" "#4e4e4e")
  (wave-blue-2  	 "#2D4F67" "#585858")
  (wave-aqua-1		 "#6A9589" "#6a9589")
  (wave-aqua-2		 "#7AA89F" "#717C7C")

  (winter-green	   "#2B3328" "#585858")
  (winter-yellow   "#49443C" "#585858")
  (winter-red		   "#43242B" "#585858")
  (winter-blue	   "#252535" "#585858")

  (autumn-green	   "#76946A" "#585858")
  (autumn-red		   "#C34043" "#585858")
  (autumn-yellow   "#DCA561" "#585858")

  (samurai-red		 "#E82424" "#585858")
  (ronin-yellow		 "#FF9E3B" "#585858")

  (dragon-blue		 "#658594" "#658594")
  (fuji-gray       "#727169" "#717C7C")
  (spring-violet-1 "#938AA9" "#717C7C")
  (oni-violet		   "#957FB8" "#717C7C")
  (crystal-blue		 "#7E9CD8" "#717C7C")
  (spring-violet-2 "#9CABCA" "#717C7C")
  (spring-blue		 "#7FB4CA" "#717C7C")
  (light-blue		   "#A3D4D5" "#717C7C")
  (spring-green		 "#98BB6C" "#717C7C")
  (boat-yellow-1	 "#938056" "#717C7C")
  (boat-yellow-2	 "#C0A36E" "#717C7C")
  (carp-yellow		 "#E6C384" "#717C7C")
  (sakura-pink		 "#D27E99" "#717C7C")
  (wave-red        "#E46876" "#717C7C")
  (peach-red       "#FF5D62" "#717C7C")
  (surimi-orange	 "#FFA066" "#717C7C")
  (katana-gray		 "#717C7C" "#717C7C")
  (comet           "#54536D" "#4e4e4e"))

  ;; Customize faces
  (
  (default                                       (:background sumi-ink-1b :foreground fuji-white))
  (border                                        (:background sumi-ink-1b :foreground sumi-ink-0))
  (button                                        (:foreground wave-aqua-2))
  (child-frame                                   (:background sumi-ink-0 :foreground sumi-ink-0))
  (child-frame-border                            (:background sumi-ink-0 :foreground sumi-ink-0))
  (cursor                                        (:background light-blue :foreground sumi-ink-0 :bold t))
  (error                                         (:foreground samurai-red))
  (fringe                                        (:foreground sumi-ink-3))
  (glyph-face                                    (:background sumi-ink-4))
  (glyphless-char                                (:foreground sumi-ink-4))
  (header-line                                   (:background sumi-ink-0))
  (highlight                                     (:background comet :foreground spring-violet-1))
  (hl-line                                       (:background sumi-ink-2))
  (homoglyph                                     (:foreground light-blue))
  (internal-border                               (:background sumi-ink-1b))
  (line-number                                   (:foreground sumi-ink-4))
  (line-number-current-line                      (:foreground spring-violet-2 :background sumi-ink-2 :bold t))
  (lv-separator                                  (:foreground wave-blue-2 :background sumi-ink-2))
  (match                                         (:background carp-yellow :foreground sumi-ink-0))
  (menu                                          (:background sumi-ink-0 :foreground fuji-white))
  (mode-line                                     (:background sumi-ink-0))
  (mode-line-inactive                            (:background nil :foreground sumi-ink-4 :bold nil))
  (mode-line-active                              (:background sumi-ink-0 :foreground old-white :bold nil))
  (mode-line-highlight                           (:foreground boat-yellow-2))
  (mode-line-buffer-id                           (:foreground wave-aqua-2 :bold t))
  (numbers                                       (:background sakura-pink))
  (region                                        (:background wave-blue-2))
  (separator-line                                (:background sumi-ink-0))
  (shadow                                        (:background sumi-ink-0))
  (success                                       (:foreground wave-aqua-2))
  (vertical-border                               (:foreground sumi-ink-4))
  (warning                                       (:foreground ronin-yellow))
  (window-border                                 (:background sumi-ink-1b))
  (window-divider                                (:foreground sumi-ink-2))
  (hi-yellow                                     (:background carp-yellow :foreground sumi-ink-1b))

  ;; Font lock
  (font-lock-type-face                           (:foreground wave-aqua-2))
  (font-lock-regexp-grouping-backslash           (:foreground boat-yellow-2))
  (font-lock-keyword-face                        (:foreground oni-violet :weight 'semi-bold :italic kanagawa-theme-keyword-italic))
  (font-lock-warning-face                        (:foreground ronin-yellow))
  (font-lock-string-face                         (:foreground spring-green :italic t))
  (font-lock-builtin-face                        (:foreground spring-blue))
  (font-lock-reference-face                      (:foreground peach-red))
  (font-lock-constant-face                       (:foreground carp-yellow))
  (font-lock-function-name-face                  (:foreground crystal-blue))
  (font-lock-variable-name-face                  (:foreground wave-red))
  (font-lock-negation-char-face                  (:foreground peach-red))
  (font-lock-comment-face                        (:foreground fuji-gray :italic kanagawa-theme-comment-italic))
  (font-lock-comment-delimiter-face              (:foreground fuji-gray :italic t))
  (font-lock-doc-face                            (:foreground comet))
  (font-lock-doc-markup-face                     (:foreground comet))
  (font-lock-preprocessor-face	                 (:foreground boat-yellow-2))
  (elisp-shorthand-font-lock-face                (:foreground fuji-white))

  (info-xref                                     (:foreground carp-yellow))
  (minibuffer-prompt-end                         (:foreground autumn-red :background winter-red))
  (minibuffer-prompt                             (:foreground carp-yellow :background winter-yellow))
  (epa-mark                                      (:foreground wave-red))
  (dired-mark                                    (:foreground wave-red))
  (trailing-whitespace                           (:background comet))
  (mode-line                                     (:background sumi-ink-0 :foreground fuji-white :bold t))

  ;; Battery colors
  (doom-modeline-battery-critical                (:foreground peach-red))
  (doom-modeline-battery-warning                 (:foreground spring-green))
  (doom-modeline-battery-charging                (:foreground fuji-gray))
  (doom-modeline-battery-error                   (:foreground peach-red))
  (doom-modeline-battery-normal                  (:foreground spring-violet-1))
  (doom-modeline-battery-full                    (:foreground wave-aqua-2))
  
  ;; Doom visual state
  (doom-modeline-evil-motion-state               (:foreground light-blue))
  (doom-modeline-evil-emacs-state                (:foreground crystal-blue))
  (doom-modeline-evil-insert-state               (:foreground peach-red))
  (doom-modeline-evil-normal-state               (:foreground light-blue))
  (doom-modeline-evil-visual-state               (:foreground spring-green))
  (doom-modeline-evil-replace-state              (:foreground ronin-yellow))
  (doom-modeline-evil-operator-state             (:foreground crystal-blue))

  (doom-modeline-project-dir                     (:bold t :foreground wave-aqua-2))
  (doom-modeline-buffer-path                     (:inherit 'bold :foreground wave-aqua-2))
  (doom-modeline-buffer-file                     (:inherit 'bold :foreground oni-violet))
  (doom-modeline-buffer-modified                 (:inherit 'bold :foreground carp-yellow))
  (doom-modeline-error                           (:background peach-red))
  (doom-modeline-buffer-major-mode               (:foreground wave-aqua-2 :bold t))
  (doom-modeline-info                            (:bold t :foreground light-blue))
  (doom-modeline-project-dir                     (:bold t :foreground surimi-orange))
  (doom-modeline-bar                             (:bold t :background spring-violet-1))
  (doom-modeline-panel                           (:inherit 'bold :background boat-yellow-2 :foreground sumi-ink-2))
  (doom-themes-visual-bell                       (:background autumn-red))

  ;; elfeed
  (elfeed-search-feed-face                       (:foreground spring-violet-1))
  (elfeed-search-tag-face                        (:foreground wave-aqua-2))

  ;; message colors
  (message-header-name                           (:foreground sumi-ink-4))
  (message-header-other                          (:foreground surimi-orange))
  (message-header-subject                        (:foreground carp-yellow))
  (message-header-to                             (:foreground old-white))
  (message-header-cc                             (:foreground wave-aqua-2))
  (message-header-xheader                        (:foreground old-white))
  (custom-link                                   (:foreground crystal-blue))
  (link                                          (:foreground crystal-blue))

  ;; org-mode
  (org-done                                      (:foreground dragon-blue))
  (org-code                                      (:background sumi-ink-0))
  (org-meta-line                                 (:background winter-green :foreground spring-green))
  (org-block                                     (:background sumi-ink-0 :foreground sumi-ink-4))
  (org-block-begin-line                          (:background winter-blue :foreground spring-blue))
  (org-block-end-line	                           (:background winter-red :foreground peach-red))
  (org-headline-done                             (:foreground dragon-blue :strike-through t))
  (org-todo                                      (:foreground surimi-orange :bold t))
  (org-headline-todo                             (:foreground sumi-ink-2))
  (org-upcoming-deadline                         (:foreground peach-red))
  (org-footnote                                  (:foreground wave-aqua-2))
  (org-indent                                    (:background sumi-ink-1b :foreground sumi-ink-1b))
  (org-hide                                      (:background sumi-ink-1b :foreground sumi-ink-1b))
  (org-date                                      (:foreground wave-blue-2))
  (org-ellipsis                                  (:foreground wave-blue-2 :bold t))
  (org-level-1                                   (:foreground peach-red :height 1.3 :bold t))
  (org-level-2                                   (:foreground spring-violet-2 :height 1.15 :bold t))
  (org-level-3                                   (:foreground boat-yellow-2 :height 1.05))
  (org-level-4                                   (:foreground fuji-white))
  (org-level-5                                   (:foreground fuji-white))
  (org-level-6                                   (:foreground carp-yellow))
  (org-level-7                                   (:foreground surimi-orange))
  (org-level-8                                   (:foreground spring-green))

  ;; which-key
  (which-key-key-face                            (:inherit 'font-lock-variable-name-face))
  (which-func                                    (:inherit 'font-lock-function-name-face :bold t))
  (which-key-group-description-face              (:foreground wave-red))
  (which-key-command-description-face            (:foreground crystal-blue))
  (which-key-local-map-description-face          (:foreground carp-yellow))
  (which-key-posframe                            (:background wave-blue-1))
  (which-key-posframe-border	                   (:background wave-blue-1))

  ;; swiper
  (swiper-line-face                              (:foreground carp-yellow))
  (swiper-background-match-face-1                (:background surimi-orange :foreground sumi-ink-0))
  (swiper-background-match-face-2                (:background crystal-blue :foreground sumi-ink-0))
  (swiper-background-match-face-3                (:background boat-yellow-2 :foreground sumi-ink-0))
  (swiper-background-match-face-4                (:background peach-red :foreground sumi-ink-0))
  (swiper-match-face-1                           (:inherit 'swiper-background-match-face-1))
  (swiper-match-face-2                           (:inherit 'swiper-background-match-face-2))
  (swiper-match-face-3                           (:inherit 'swiper-background-match-face-3))
  (swiper-match-face-4                           (:inherit 'swiper-background-match-face-4))

  (counsel-outline-default                       (:foreground carp-yellow))
  (info-header-xref                              (:foreground carp-yellow))
  (xref-file-header                              (:foreground carp-yellow))
  (xref-match                                    (:foreground carp-yellow))

  ;; rainbow delimiters
  (rainbow-delimiters-mismatched-face            (:foreground peach-red))
  (rainbow-delimiters-unmatched-face             (:foreground wave-aqua-2))
  (rainbow-delimiters-base-error-face            (:foreground peach-red))
  (rainbow-delimiters-base-face                  (:foreground sumi-ink-4))

  (rainbow-delimiters-depth-1-face               (:foreground spring-violet-2))
  (rainbow-delimiters-depth-2-face               (:foreground dragon-blue))
  (rainbow-delimiters-depth-3-face               (:foreground spring-violet-1))
  (rainbow-delimiters-depth-4-face               (:foreground spring-green))
  (rainbow-delimiters-depth-5-face               (:foreground wave-aqua-2))
  (rainbow-delimiters-depth-6-face               (:foreground carp-yellow))
  (rainbow-delimiters-depth-7-face               (:foreground wave-red))
  (rainbow-delimiters-depth-8-face               (:foreground light-blue))
  (rainbow-delimiters-depth-9-face               (:foreground spring-violet-2))

  ;; show-paren
  (show-paren-match                              (:background wave-aqua-1 :foreground sumi-ink-0 :bold t))
  (show-paren-match-expression	                 (:background wave-aqua-1 :foreground sumi-ink-0 :bold t))
  (show-paren-mismatch                           (:background peach-red :foreground old-white))
  (tooltip                                       (:foreground sumi-ink-0 :background carp-yellow :bold t))
  
  ;; company-box
  (company-tooltip                               (:background sumi-ink-2))
  (company-tooltip-common                        (:foreground autumn-yellow))
  (company-tooltip-quick-access                  (:foreground spring-violet-2))
  (company-tooltip-scrollbar-thumb               (:background autumn-red))
  (company-tooltip-scrollbar-track               (:background sumi-ink-2))
  (company-tooltip-search                        (:background carp-yellow :foreground sumi-ink-0 :distant-foreground fuji-white))
  (company-tooltip-selection                     (:background peach-red :foreground winter-red :bold t))
  (company-tooltip-mouse                         (:background sumi-ink-2 :foreground sumi-ink-0 :distant-foreground fuji-white))
  (company-tooltip-annotation                    (:foreground peach-red :distant-foreground sumi-ink-1))
  (company-scrollbar-bg                          (:inherit 'tooltip))
  (company-scrollbar-fg                          (:background peach-red))
  (company-preview                               (:foreground carp-yellow))
  (company-preview-common                        (:foreground peach-red :bold t))
  (company-preview-search                        (:inherit 'company-tooltip-search))
  (company-template-field                        (:inherit 'match))

  ;; flycheck
  (flycheck-posframe-background-face             (:background sumi-ink-0))
  (flycheck-posframe-face                        (:background sumi-ink-0))
  (flycheck-posframe-info-face                   (:background sumi-ink-0 :foreground autumn-green))
  (flycheck-posframe-warning-face                (:background sumi-ink-0 :foreground light-blue))
  (flycheck-posframe-error-face                  (:background sumi-ink-0 :foreground samurai-red))
  (flycheck-fringe-warning                       (:foreground light-blue))
  (flycheck-fringe-error                         (:foreground samurai-red))
  (flycheck-fringe-info                          (:foreground autumn-green))
  (flycheck-error-list-warning                   (:foreground ronin-yellow :bold t))
  (flycheck-error-list-error                     (:foreground samurai-red :bold t))
  (flycheck-error-list-info                      (:foreground wave-aqua-1 :bold t))
  (flycheck-inline-error                         (:foreground samurai-red :background winter-red :italic t :bold t :height 138))
  (flycheck-inline-info                          (:foreground light-blue :background winter-blue :italic t  :bold t :height 138))
  (flycheck-inline-warning                       (:foreground winter-yellow :background carp-yellow :italic t :bold t :height 138))

  ;; indent dots
  (highlight-indent-guides-character-face        (:foreground sumi-ink-3))
  (highlight-indent-guides-stack-character-face  (:foreground sumi-ink-3))
  (highlight-indent-guides-stack-odd-face        (:foreground sumi-ink-3))
  (highlight-indent-guides-stack-even-face       (:foreground comet))
  (highlight-indent-guides-stack-character-face  (:foreground sumi-ink-3))
  (highlight-indent-guides-even-face             (:foreground sumi-ink-2))
  (highlight-indent-guides-odd-face              (:foreground comet))

  (highlight-operators-face                      (:foreground boat-yellow-2))
  (highlight-quoted-symbol                       (:foreground spring-green))
  (highlight-numbers-face                        (:foreground sakura-pink))
  (highlight-symbol-face                         (:background wave-blue-1 :foreground light-blue))
  
  ;; ivy
  (ivy-current-match                             (:background crystal-blue :foreground sumi-ink-0 :bold t))
  (ivy-action                                    (:background nil :foreground fuji-white))
  (ivy-grep-line-number                          (:background nil :foreground spring-green))
  (ivy-minibuffer-match-face-1                   (:background nil :foreground wave-red))
  (ivy-minibuffer-match-face-2                   (:background nil :foreground spring-green))
  (ivy-minibuffer-match-highlight                (:foreground light-blue))
  (ivy-grep-info                                 (:foreground light-blue))
  (ivy-grep-line-number                          (:foreground spring-violet-2))
  (ivy-confirm-face                              (:foreground wave-aqua-2))

  ;; posframe's
  (ivy-posframe                                  (:background sumi-ink-2))
  (ivy-posframe-border                           (:background sumi-ink-3))
  
  ;;treemacs
  (treemacs-directory-collapsed-face             (:foreground fuji-white))
  (treemacs-directory-face                       (:foreground fuji-white))
  (treemacs-file-face                            (:foreground fuji-white))

  (treemacs-git-added-face                       (:foreground surimi-orange))
  (treemacs-git-renamed-face                     (:foreground fuji-white))
  (treemacs-git-ignored-face                     (:foreground sumi-ink-4))
  (treemacs-git-unmodified-face                  (:foreground fuji-white))
  (treemacs-git-renamed-face                     (:foreground fuji-white))
  (treemacs-git-modified-face                    (:foreground spring-green))
   
  ;; lsp and lsp-ui
  (lsp-headerline-breadcrumb-path-error-face     (:underline (:color spring-green :style 'wave) :foreground sumi-ink-4 :background sumi-ink-0))
  (lsp-headerline-breadcrumb-path-face           (:background sumi-ink-0))
  (lsp-headerline-breadcrumb-path-hint-face      (:background sumi-ink-0))
  (lsp-headerline-breadcrumb-path-info-face      (:background sumi-ink-0))
  (lsp-headerline-breadcrumb-separator-face      (:background sumi-ink-0))
  (lsp-headerline-breadcrumb-symbols-face        (:background sumi-ink-0))
  (lsp-headerline-breadcrumb-project-prefix-face (:background sumi-ink-0))
  (lsp-headerline-breadcrumb-symbols-error-face  (:foreground peach-red))

  (lsp-ui-doc-background                         (:background sumi-ink-0 :foreground peach-red))
  (lsp-ui-doc-header                             (:background sumi-ink-0 :foreground peach-red))
  (lsp-ui-doc-border                             (:background nil :foreground nil))
  (lsp-ui-peek-filename                          (:foreground light-blue))
  (lsp-ui-sideline-code-action                   (:foreground carp-yellow))
  (lsp-ui-sideline-current-symbol                (:foreground spring-blue))
  (lsp-ui-sideline-symbol                        (:foreground dragon-blue))

  ;; dashboard
  (dashboard-heading                             (:foreground spring-violet-2 :bold t))
  (dashboard-items-face                          (:bold nil :foreground fuji-white))
  (dashboard-banner-logo-title                   (:bold t :height 200))
  (dashboard-no-items-face                       (:foreground sumi-ink-4))

  ;; all-the-icons
  (all-the-icons-dgreen                          (:foreground wave-aqua-2))
  (all-the-icons-green                           (:foreground wave-aqua-2))
  (all-the-icons-dpurple                         (:foreground spring-violet-2))
  (all-the-icons-purple                          (:foreground spring-violet-2))

  ;; evil
  (evil-ex-lazy-highlight                        (:foreground winter-green :background autumn-green :bold t))
  (evil-ex-substitute-matches                    (:foreground winter-red :background autumn-red :bold t))
  (evil-ex-substitute-replacement                (:foreground surimi-orange :strike-through nil :inherit 'evil-ex-substitute-matches))
  (evil-search-highlight-persist-highlight-face  (:background carp-yellow))

  ;; term
  (term                                          (:background sumi-ink-0 :foreground fuji-white))
  (term-color-blue                               (:background crystal-blue :foreground crystal-blue))
  (term-color-bright-blue                        (:inherit 'term-color-blue))
  (term-color-green                              (:background wave-aqua-2 :foreground wave-aqua-2))
  (term-color-bright-green                       (:inherit 'term-color-green))
  (term-color-black                              (:background sumi-ink-0 :foreground fuji-white))
  (term-color-bright-black                       (:background sumi-ink-1b :foreground sumi-ink-1b))
  (term-color-white                              (:background fuji-white :foreground fuji-white))
  (term-color-bright-white                       (:background old-white :foreground old-white))
  (term-color-red                                (:background peach-red :foreground peach-red))
  (term-color-bright-red                         (:background spring-green :foreground spring-green))
  (term-color-yellow                             (:background carp-yellow :foreground carp-yellow))
  (term-color-bright-yellow                      (:background carp-yellow :foreground carp-yellow))
  (term-color-cyan                               (:background spring-blue :foreground spring-blue))
  (term-color-bright-cyan                        (:background spring-blue :foreground spring-blue))
  (term-color-magenta                            (:background spring-violet-2 :foreground spring-violet-2))
  (term-color-bright-magenta                     (:background spring-violet-2 :foreground spring-violet-2))

  ;; popup
  (popup-face                                    (:inherit 'tooltip))
  (popup-selection-face                          (:inherit 'tooltip))
  (popup-tip-face                                (:inherit 'tooltip))

  ;; anzu
  (anzu-match-1                                  (:foreground wave-aqua-2 :background sumi-ink-2))
  (anzu-match-2                                  (:foreground carp-yellow :background sumi-ink-2))
  (anzu-match-3                                  (:foreground light-blue :background sumi-ink-2))

  (anzu-mode-line                                (:foreground sumi-ink-0 :background spring-violet-2))
  (anzu-mode-no-match	                         (:foreground fuji-white :background peach-red))
  (anzu-replace-to                               (:foreground spring-blue :background winter-blue))
  (anzu-replace-highlight                        (:foreground peach-red :background winter-red :strike-through t))

  ;; ace
  (ace-jump-face-background                      (:foreground wave-blue-2))
  (ace-jump-face-foreground                      (:foreground peach-red :background sumi-ink-0 :bold t))
  
  ;; vertico
  (vertico-multiline                             (:background samurai-red))
  (vertico-group-title                           (:background winter-blue :foreground light-blue :bold t))
  (vertico-group-separator                       (:background winter-blue :foreground light-blue :strike-through t))
  (vertico-current                               (:foreground carp-yellow :bold t :italic nil :background wave-blue-1))

  (vertico-posframe-border                       (:background sumi-ink-3))
  (vertico-posframe                              (:background sumi-ink-2))
  (orderless-match-face-0                        (:foreground crystal-blue :bold t))
  
  (comint-highlight-prompt                       (:background spring-violet-2 :foreground sumi-ink-1))
  (completions-annotations                       (:background nil :foreground dragon-blue :italic t))
  (marginalia-file-priv-no                       (:background 'unspecified))
  
  ;; hydra
  (hydra-face-amaranth                           (:foreground autumn-red))
  (hydra-face-blue                               (:foreground spring-blue))
  (hydra-face-pink                               (:foreground sakura-pink))
  (hydra-face-red                                (:foreground peach-red))
  (hydra-face-teal                               (:foreground light-blue))

  ;; centaur-tabs
  (centaur-tabs-active-bar-face                  (:background spring-blue :foreground fuji-white))
  (centaur-tabs-selected                         (:background sumi-ink-1b :foreground fuji-white :bold t))
  (centaur-tabs-selected-modified                (:background sumi-ink-1b :foreground fuji-white))
  (centaur-tabs-modified-marker-selected         (:background sumi-ink-1b :foreground autumn-yellow))
  (centaur-tabs-close-selected                   (:inherit 'centaur-tabs-selected))
  (tab-line                                      (:background sumi-ink-0))

  (centaur-tabs-unselected                       (:background sumi-ink-0 :foreground sumi-ink-4))
  (centaur-tabs-default                          (:background sumi-ink-0 :foreground sumi-ink-4))
  (centaur-tabs-unselected-modified              (:background sumi-ink-0 :foreground peach-red))
  (centaur-tabs-modified-marker-unselected       (:background sumi-ink-0 :foreground sumi-ink-4))
  (centaur-tabs-close-unselected                 (:background sumi-ink-0 :foreground sumi-ink-4))

  (centaur-tabs-close-mouse-face                 (:background nil :foreground peach-red))
  (centaur-tabs-default                          (:background ronin-yellow ))
  (centaur-tabs-name-mouse-face                  (:foreground spring-blue :bold t))

  (git-gutter:added                              (:foreground autumn-green))
  (git-gutter:deleted                            (:foreground wave-red))
  (git-gutter:modified                           (:foreground spring-blue))

  (diff-hl-margin-change                         (:foreground spring-blue :background winter-blue))
  (diff-hl-margin-delete                         (:foreground peach-red :background winter-red))
  (diff-hl-margin-insert                         (:foreground comet :background winter-blue))

  (bm-fringe-face                                (:background peach-red :foreground sumi-ink-3))
  (bm-fringe-persistent-face                     (:background peach-red :foreground sumi-ink-3))

  (ansi-color-green                              (:foreground spring-green))
  (ansi-color-black                              (:background sumi-ink-0))
  (ansi-color-cyan                               (:foreground wave-aqua-2))
  (ansi-color-magenta                            (:foreground sakura-pink))
  (ansi-color-blue                               (:foreground crystal-blue))
  (ansi-color-red                                (:foreground peach-red))
  (ansi-color-white                              (:foreground fuji-white))
  (ansi-color-yellow                             (:foreground autumn-yellow))
  (ansi-color-bright-white                       (:foreground old-white))
  (ansi-color-bright-white                       (:foreground old-white))

  (tree-sitter-hl-face:attribute                 (:foreground surimi-orange))
  (tree-sitter-hl-face:escape                    (:foreground wave-red))
  (tree-sitter-hl-face:constructor               (:foreground wave-red :weight 'semi-bold))
  
  (tree-sitter-hl-face:constant                  (:foreground surimi-orange))
  (tree-sitter-hl-face:constant.builtin          (:foreground carp-yellow :weight 'semi-bold))

  (tree-sitter-hl-face:embedded                  (:foreground boat-yellow-2))
  
  (tree-sitter-hl-face:function                  (:foreground crystal-blue))
  (tree-sitter-hl-face:function.builtin          (:foreground peach-red :italic t :background winter-red))
  (tree-sitter-hl-face:function.call             (:foreground spring-violet-2))
  (tree-sitter-hl-face:function.macro            (:foreground samurai-red))
  (tree-sitter-hl-face:function.special          (:foreground sakura-pink))
  (tree-sitter-hl-face:function.label            (:foreground surimi-orange))
 
  (tree-sitter-hl-face:method                    (:foreground light-blue))
  (tree-sitter-hl-face:method.call               (:foreground light-blue))

  (tree-sitter-hl-face:property                  (:foreground carp-yellow))
  (tree-sitter-hl-face:property.definition       (:foreground old-white :italic t))
  
  (tree-sitter-hl-face:tag                       (:foreground peach-red))

  (tree-sitter-hl-face:type                      (:foreground wave-aqua-2 :weight 'semi-bold))
  (tree-sitter-hl-face:type.argument             (:foreground surimi-orange))
  (tree-sitter-hl-face:type.builtin              (:foreground autumn-red))
  (tree-sitter-hl-face:type.parameter            (:foreground surimi-orange))
  (tree-sitter-hl-face:type.super                (:foreground samurai-red :bold t))

  (tree-sitter-hl-face:variable                  (:foreground spring-blue :italic t))
  (tree-sitter-hl-face:variable.builtin          (:foreground wave-red))
  (tree-sitter-hl-face:variable.parameter        (:foreground spring-violet-2 :italic t))
  (tree-sitter-hl-face:variable.special          (:foreground surimi-orange))
  (tree-sitter-hl-face:variable.synthesized      (:foreground light-blue))

  (tree-sitter-hl-face:number                    (:foreground sakura-pink))
  (tree-sitter-hl-face:operator                  (:foreground sakura-pink :bold t))
  
  (tree-sitter-hl-face:punctuation               (:foreground light-blue))
  (tree-sitter-hl-face:punctuation.bracket       (:foreground spring-violet-2 :bold t))
  (tree-sitter-hl-face:punctuation.delimiter     (:foreground spring-violet-2 :bold t))
  (tree-sitter-hl-face:punctuation.special       (:foreground peach-red))

  (tree-sitter-hl-face:case-pattern              (:foreground wave-red))
  (tree-sitter-hl-face:variable.synthesized      (:foreground wave-red))
  (tree-sitter-hl-face:keyword.compiler          (:foreground peach-red :bold t :italic t))

  (focus-unfocused                               (:foreground sumi-ink-4))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kanagawa)
;;; kanagawa-theme.el ends here
