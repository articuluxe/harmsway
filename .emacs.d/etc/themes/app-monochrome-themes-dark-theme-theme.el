;;; app-monochrome-themes-dark-theme-theme.el --- Monochrome theme for OLED

;; Author: Aleksandr Petrosyan <appetrosyan3@gmail.com>
;; URL: https://github.com/Greybeard-Entertainment/app-monochrome
;; Version: 0.0.1

;;; Blurb:

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

;; A monochromatic theme that emphasises shapes over colours.

;; The human eye has a ton more rods than cones, and in low-light
;; conditions one finds shapes more easily than colours.  It is the
;; basis of this theme.

;; Rather than have the effective equivalent of a circus on your
;; screen, it is instead the intention of this package to provide you
;; with a well-shaped buffer that has an occasional splash of colours
;; to highlight the important aspects that cannot be highlighted
;; otherwise.

;; This package depends on the following freely available, fonts:
;; - IBM Plex <https://github.com/IBM/plex>
;; - Ubuntu Mono <https://github.com/canonical/UbuntuMono-fonts>
;; - Victor Mono <https://github.com/rubjo/victor-mono>
;; - Linux Libertine <https://libertine-fonts.org>
;; - Fira Code <https://github.com/tonsky/FiraCode>


;; The best way to install these fonts is via the package manager, but
;; downloading those fonts and installing them on a per-user or
;; system-wide basis is also an option.

;; The fonts were specifically chosen in order to facilitate usage
;; with other packages.  I highly recommend using this in tandem with
;; `rainbow-delimiters' and `rainbow-mode' or
;; `color-identifiers-mode'.  The main idea is that the font shapes
;; and shades of luminosity convey that syntactic roles, if they
;; weren't obvious before.  At the same time, the colour are used to
;; emphasise the delimiters, and identifiers.  The faces used for
;; those packages are not yet included in this package.


;;; Code:

(deftheme app-monochrome-themes-dark-theme
  "A mononcrhomatic white text on grey background theme with emphasis on shapes rather than colours.")

(custom-theme-set-faces
 'app-monochrome-themes-dark-theme
 ;; Basic faces
 '(default ((t (:family "UbuntuMono Nerd Font" :foundry "DAMA" :slant normal :weight regular :height 98 :width normal))))

 '(dired-directory ((t (:weight bold))))
 '(dirvish-media-info-property-key ((t nil)))
 '(dired-flagged ((t (:foreground "Red" :box (:line-width (2 . 2) :color "Red" :style released-button) :weight bold))))
 '(dired-set-id ((t (:underline t))))

 '(variable-pitch ((t (:family "IBM Plex Serif"))))
 '(highlight ((t (:background "#bcc" :foreground "black"))))
 '(italic ((t (:slant italic :weight normal :family "IBM Plex Sans"))))
 '(error ((t (:box (:line-width (2 . 2) :color "Red" :style released-button) :weight bold))))
 '(warning ((t (:foreground "gold"))))
 '(success ((t (:foreground "white" :weight bold))))
 '(bookmark-face ((t (:background "Black" :foreground "Gold"))))
 '(isearch ((t (:inherit link))))
 '(custom-link ((t (:inherit link :box (:line-width (2 . 2) :color "grey75" :style released-button)))))
 '(link ((t (:underline t :foreground "#5cacac"))))
 '(line-number ((t (:inherit shadow :family "VictorMono Nerd Font"))))

 '(font-lock-comment-face ((t (:foreground "#aaa"))))
 '(font-lock-type-face ((t (:weight bold :family "VictorMono Nerd Font"))))
 '(font-lock-builtin-face ((t (:family "Linux Libertine Mono" :background "grey16"))))
 '(font-lock-function-name-face ((t (:slant italic :family "IBM Plex Mono"))))
 '(font-lock-keyword-face ((t (:weight bold :family "Ubuntu Mono"))))
 '(font-lock-constant-face ((t (:foreground "white" :weight bold :inherit font-lock-type-face))))
 '(font-lock-string-face ((t (:foreground "grey62" :family "IBM Plex Mono"))))
 '(font-lock-negation-char-face ((t (:weight bold))))
 '(font-lock-doc-face ((t (:slant italic :inherit font-lock-string-face))))
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-variable-name-face ((t (:foreground "white" :weight thin))))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))

 '(font-latex-bold-face ((t (:inherit bold))))
 '(font-latex-italic-face ((t (:inherit italic))))
 '(font-latex-math-face ((t (:family "IBM Plex Mono"))))
 '(font-latex-sectioning-5-face ((t (:inherit org-level-5 :weight bold))))
 '(font-latex-string-face ((t (:inherit font-lock-string-face))))
 '(font-latex-underline-face ((t (:inherit underline))))
 '(font-latex-verbatim-face ((t (:inherit fixed-pitch))))
 '(font-latex-warning-face ((t (:inherit warning))))

 '(mode-line ((t (:inherit line-number :box t))))
 '(mode-line-highlight ((t (:family "Linux Libertine Mono" :inherit mode-line :weight bold))))
 '(mode-line-buffer-id ((t (:inherit mode-line-highlight))))

 '(lsp-face-semhl-function ((t (:inherit (font-lock-function-name-face)))))
 '(lsp-face-highlight-textual ((t (:inherit (highlight)))))
 '(lsp-face-highlight-write ((t (:weight bold :inherit (highlight)))))
 '(lsp-face-semhl-type ((t (:inherit (font-lock-type-face)))))
 '(lsp-face-semhl-implementation ((t (:weight bold :inherit (font-lock-function-name-face)))))

 '(counsel-outline-1 ((t (:inherit (org-level-1)))))
 '(counsel-application-name ((t (:inherit (font-lock-builtin-face)))))
 '(counsel-active-mode ((t (:inherit (font-lock-builtin-face)))))
 '(swiper-background-match-face-1 ((t (:inherit (swiper-match-face-1)))))
 '(swiper-match-face-1 ((t (:inherit (lazy-highlight)))))
 '(lazy-highlight ((t (:distant-foreground "white" :box (:line-width (2 . 2) :color "grey75" :style released-button)))))
 '(swiper-background-match-face-3 ((t (:inherit (swiper-match-face-3)))))
 '(swiper-line-face ((t (:inherit highlight :box (:line-width (2 . 2) :color "grey75" :style released-button)))))

 '(lsp-ui-doc-background ((t (:background "grey10"))))
 '(lsp-ui-doc-highlight-hover ((t (:inherit highlight))))
 '(lsp-ui-doc-background ((t (:background "grey90"))))
 '(lsp-ui-sideline-global ((t nil)))
 '(lsp-ui-sideline-symbol ((t (:height 0.99 :box (:line-width (1 . -1) :color "grey" :style nil) :foreground "grey"))))
 '(lsp-ui-peek-list ((((background light)) (:background "light gray")) (t (:background "#181818"))))
 '(lsp-ui-peek-line-number ((t (:foreground "grey25"))))

 '(flymake-error ((t (:underline (:color "Red1" :style dashes :position nil)))))

 '(flycheck-fringe-warning ((t (:inherit (warning)))))
 '(flycheck-info ((t (:inherit success :underline t))))

 '(ivy-match-required-face ((t (:foreground "red" :inherit (minibuffer-prompt)))))
 '(ivy-cursor ((((class color) (background light)) (:foreground "white" :background "black")) (((class color) (background dark)) (:foreground "black" :background "white"))))
 '(ivy-virtual ((t (:inherit (font-lock-builtin-face)))))
 '(ivy-action ((t (:inherit (font-lock-builtin-face)))))

 '(doom-modeline-lsp-success ((t (:inherit nil :weight bold))))

 '(org-list-dt ((t (:bold t))))
 '(compilation-line-number ((t (:inherit (font-lock-keyword-face)))))
 '(doom-modeline-buffer-file ((t (:inherit (mode-line-buffer-id bold)))))
 '(counsel-variable-documentation ((t (:inherit (font-lock-comment-face)))))
 '(ivy-prompt-match ((t (:inherit (ivy-current-match)))))

 '(minibuffer-prompt ((t (:inherit default :weight bold :slant italic :box t))))
 '(orderless-match-face-0 ((t (:inherit error :weight bold))))
 '(orderless-match-face-1 ((t (:inherit error :underline t))))
 '(orderless-match-face-2 ((t (:inherit link :weight bold))))
 '(orderless-match-face-3 ((t (:inherit link :underline t))))
 '(completions-common-part ((t (:inherit error))))

 '(ivy-grep-line-number ((t (:inherit (compilation-line-number)))))
 '(sp-show-pair-match-content-face ((t nil)))
 '(show-paren-match ((t (:inherit default :background "#8cc"))))
 '(show-paren-mismatch ((t (:inherit show-paren-match :inverse-video t))))
 '(trailing-whitespace ((t (:underline "blue"))))

 '(magit-filename ((t (:weight normal))))
 '(magit-keyword-squash ((t (:inherit (font-lock-warning-face)))))
 '(magit-blame-name ((t nil)))
 '(magit-diff-base-highlight ((((class color) (background light)) (:foreground "#aaaa11" :background "#eeeebb" :extend t)) (((class color) (background dark)) (:foreground "#eeeebb" :background "#666622" :extend t))))
 '(magit-keyword ((t (:inherit (font-lock-string-face)))))
 '(magit-tag ((t (:inherit font-lock-constant-face))))
 '(magit-branch-upstream ((t (:slant italic))))
 '(magit-branch-remote ((t (:foreground "white" :weight bold :height 1.25 :family "IBM Plex Serif"))))
 '(magit-branch-local ((t (:weight bold :height 1.25 :family "IBM Plex Serif"))))
 '(magit-branch-current ((t (:box (:line-width (1 . 1) :color nil :style nil) :inherit (magit-branch-local)))))
 '(magit-section-heading-selection ((t (:extend t :weight bold :height 1.25))))
 '(magit-section-secondary-heading ((t (:weight bold :extend t))))
 '(magit-section-heading ((t (:extend t :weight bold :height 2.0 :inherit font-lock-constant-face))))
 '(magit-section-highlight ((((class color) (background light)) (:background "grey95" :extend t)) (((class color) (background dark)) (:background "grey20" :extend t))))
 '(magit-section-child-count ((t nil)))
 '(magit-diff-file-heading ((t (:extend t :weight bold :family "IBM Plex Serif"))))

 '(git-commit-keyword ((t (:inherit font-lock-keyword-face))))
 '(git-commit-summary ((t (:inherit default))))

 '(org-default ((t (:inherit (default)))))

 '(tree-sitter-hl-face:constructor ((t (:inherit tree-sitter-hl-face:constant))))
 '(tree-sitter-hl-face:property.definition ((t (:inherit tree-sitter-hl-face:variable.parameter))))
 '(tree-sitter-hl-face:number ((t (:inherit (tree-sitter-hl-face:constant)))))
 '(tree-sitter-hl-face:method ((t (:inherit (tree-sitter-hl-face:function)))))
 '(tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face))))
 '(tree-sitter-hl-face:operator ((t (:inherit (tree-sitter-hl-face:keyword)))))
 '(tree-sitter-hl-face:type ((t (:inherit font-lock-type-face))))
 '(tree-sitter-hl-face:label ((t (:inherit (font-lock-preprocessor-face)))))
 '(tree-sitter-hl-face:function ((t (:inherit (font-lock-function-name-face)))))
 '(tree-sitter-hl-face:type.builtin ((t (:inherit (font-lock-builtin-face)))))
 '(tree-sitter-hl-face:method.call ((t (:inherit (tree-sitter-hl-face:function.call)))))
 '(tree-sitter-hl-face:variable.parameter ((t (:inherit (tree-sitter-hl-face:variable)))))
 '(tree-sitter-hl-face:function.special ((t (:inherit (font-lock-preprocessor-face)))))
 '(tree-sitter-hl-face:doc ((t (:inherit (font-lock-doc-face)))))
 '(tree-sitter-hl-face:embedded ((t (:inherit (default)))))
 '(tree-sitter-hl-face:variable ((t (:inherit (font-lock-variable-name-face)))))
 '(tree-sitter-hl-face:variable.special ((t (:inherit (font-lock-warning-face)))))
 '(tree-sitter-hl-face:variable.builtin ((t (:inherit (font-lock-builtin-face)))))
 '(tree-sitter-hl-face:constant ((t (:inherit font-lock-constant-face))))
 '(tree-sitter-hl-face:escape ((t (:inherit (font-lock-keyword-face)))))
 '(tree-sitter-hl-face:punctuation.delimiter ((t (:inherit (tree-sitter-hl-face:punctuation)))))
 '(tree-sitter-hl-face:string.special ((t (:weight bold :inherit (tree-sitter-hl-face:string)))))
 '(tree-sitter-hl-face:punctuation ((t (:inherit (default)))))
 '(tree-sitter-hl-face:constant.builtin ((t (:inherit (font-lock-builtin-face)))))
 '(tree-sitter-hl-face:type.parameter ((t (:inherit (font-lock-variable-name-face font-lock-type-face) :slant italic :weight bold))))
 '(tree-sitter-hl-face:type.super ((t (:inherit (tree-sitter-hl-face:type)))))
 '(tree-sitter-hl-face:type.argument ((t (:inherit (tree-sitter-hl-face:type)))))
 '(tree-sitter-hl-face:property ((t (:inherit nil :slant italic))))
 '(tree-sitter-hl-face:attribute ((t (:inherit default))))


 '(markdown-header-face ((t (:weight bold :inherit magit-section-heading :height 1.5))))
 '(markdown-header-face ((t (:weight bold :height 1.2 :family "Ubuntu"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 0.8))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face-1 :height 0.8))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face-2 :height 0.8))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face-3 :height 0.8))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face-4 :height 0.8))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face-4 :height 0.8))))
 '(markdown-inline-code-face ((t (:inherit org-code))))

 '(org-level-1 ((t (:inherit markdown-header-face))))
 '(org-level-2 ((t (:inherit markdown-header-face-1))))
 '(org-level-3 ((t (:inherit markdown-header-face-2))))
 '(org-level-4 ((t (:inherit markdown-header-face-3))))
 '(org-level-5 ((t (:inherit markdown-header-face-4))))
 '(org-level-6 ((t (:inherit markdown-header-face-4 :height 0.85))))
 '(org-level-7 ((t (:inherit markdown-header-face-4 :height 0.75))))
 '(org-level-8 ((t (:inherit markdown-header-face-4 :height 0.65))))

 '(org-code ((t (:family "VictorMono Nerd Font" :foreground "grey75" :background "grey10"))))
 '(org-verbatim ((t (:inherit org-code :weight light))))
 '(org-done ((t (:inherit org-headline :foreground "white" :background "#048" :box t))))
 '(org-todo ((t (:foreground "white" :background "#800" :box t))))
 '(org-headline-todo ((t (:inherit org-headline))))
 '(org-headline-done ((t (:inherit org-headline))))
 '(org-table ((t (:foreground "white" :background "black"  :family "VictorMono Nerd Font"))))

 '(org-modern-done ((t (:inherit org-done :height 2.0))))
 '(org-modern-todo ((t (:inherit org-todo :height 1.0))))

 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-2-face ((t (:weight bold :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "#fda"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "#7fd"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "#f0a"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "#acf"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "#faa"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "#0af"))))
 '(rainbow-delimiters-depth-9-face ((t (:weight bold :inherit rainbow-delimiters-base-face :foreground "#afa"))))

 '(jinx-misspelled ((t (:underline t :inherit warning))))
 '(jinx-highlight ((t (:inherit warning :box t))))

 '(orderless-match-face-0 ((t (:background "#8cc"))))
 '(orderless-match-face-1 ((t (:slant italic :inherit orderless-match-face-0))))

 '(flyspell-incorrect ((t (:underline t :inherit warning))))

 '(lsp-rust-analyzer-inlay-param-face ((t (:inherit (lsp-rust-analyzer-inlay-face)))))
 '(lsp-rust-analyzer-inlay-type-face ((t (:inherit (lsp-rust-analyzer-inlay-face)))))
 '(lsp-rust-analyzer-inlay-face ((t (:inherit font-lock-comment-face :foreground "black" :background "white"))))

 '(rust-unsafe ((t (:weight bold))))

 '(fancy-compilation-default-face ((t (:inherit font-lock-string-face)))))

;;;###autoload
(and load-file-name
	 (boundp 'custom-theme-load-path)
	 (add-to-list 'custom-theme-load-path
				  (file-name-as-directory
				   (file-name-directory load-file-name))))


(provide-theme 'app-monochrome-themes-dark-theme)
(provide 'app-monochrome-themes-dark-theme)
;;; app-monochrome-themes-dark-theme-theme.el ends here


;; Local Variables:
;; jinx-local-words: "Fira Plex el gmail"
;; package-lint-main-file: "app-monochrome-themes.el"
;; End:
