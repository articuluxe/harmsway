;;; guava-themes-prunus-theme.el --- A theme inspired by cherry colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Dec 29, 2025
;; Version: 0.19.0
;; Keywords: themes, faces, color
;; URL: http://github.com/bormoge/guava-themes
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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
;;
;; A theme inspired by cherry colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-prunus "A theme inspired by cherry colors.")

(let* (
      (prunus-class '((class color) (min-colors 257)))
      (prunus-black                     "#000000")
      (prunus-white                     "#FFFFFF")

      (prunus-light-brown               "#826455")
      (prunus-alt-light-brown           "#9b7d6e")
      (prunus-brown                     "#50463c")
      (prunus-alt-brown                 "#645a50")
      (prunus-coffee                    "#bea091")

      (prunus-light-green               "#50a05f")
      (prunus-green-forest              "#007341")
      (prunus-green-subdued             "#2d5519")
      (prunus-oxidized-green            "#4e584e")

      (prunus-light-red                 "#be3c32")
      (prunus-red                       "#a01e14")
      (prunus-deep-red                  "#781e14")
      (prunus-light-pink                "#cd7888")
      (prunus-pink                      "#cd3c88")

      (prunus-blue                      "#4650e1")
      (prunus-deep-blue                 "#4650af")
      (prunus-cyan                      "#00aaaa")

      (prunus-purple                    "#7350a0")
      (prunus-purple-red                "#8B2252")

      (prunus-fg-1                      "#ebafc8")
      (prunus-fg-2                      "#af738c")
      (prunus-bg                        "#190f05")
      (prunus-highlight                 "#2d2319")
      (prunus-shadow                    "#b3b3b3")
      (prunus-tooltip                   "#ffffe0")
      (prunus-region-1                  "#3c3228")
      (prunus-region-2                  "#645a50")

      (prunus-error                     "#FF0000")
      (prunus-warning                   "#f6d911")
      (prunus-success                   "#23D734")
      (prunus-change                    "#4650e1")

      (prunus-mode-line                 "#50463c")
      (prunus-mode-line-inactive        "#645a50")
      (prunus-mode-line-modified        "#d74692")
      (prunus-bell                      "#465a5a")

      (prunus-tab-1                     prunus-mode-line)
      (prunus-tab-2                     prunus-mode-line-inactive)
      (prunus-tab-3                     prunus-alt-light-brown)

      (prunus-fl-comment                prunus-green-forest)
      (prunus-fl-string                 prunus-purple-red)
      (prunus-fl-keyword                prunus-light-brown)
      (prunus-fl-builtin                prunus-pink)
      (prunus-fl-type                   prunus-coffee)
      (prunus-fl-function-name          prunus-red)
      (prunus-fl-variable-name          prunus-green-subdued)
      (prunus-fl-constant               prunus-purple)
      (prunus-fl-warning                prunus-warning)
      (prunus-fl-punctuation            prunus-fg-2)
      (prunus-fl-negation-char          prunus-deep-red)

      (prunus-diff-added                "#5aa05a")
      (prunus-diff-removed              "#a05a5a")
      (prunus-diff-refine-added         "#007800")
      (prunus-diff-refine-removed       "#780000")
      (prunus-diff-header               "#5a5a5a")
      (prunus-diff-file-header          "#3c3c3c")
      (prunus-diff-context              "#828282")
      (prunus-smerge-base               "#5A5AA0")

      (prunus-square-a-0                "#af50c8")
      (prunus-square-a-1                "#28a03c")
      (prunus-square-a-2                "#ff6400")
      (prunus-square-a-3                "#3c82ff")

      (prunus-complementary-a-0         "#46C8A5")
      (prunus-complementary-a-1         "#C84669"))

  (custom-theme-set-faces
   'guava-themes-prunus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,prunus-class (:foreground ,prunus-fg-1 :background ,prunus-bg))))

   ;; error, warning, success
   `(error ((,prunus-class (:foreground ,prunus-error :weight bold))))
   `(warning ((,prunus-class (:foreground ,prunus-warning :weight bold))))
   `(success ((,prunus-class (:foreground ,prunus-success :weight bold))))

   ;; highlight
   `(highlight ((,prunus-class (:background ,prunus-highlight))))

   ;; shadow
   `(shadow ((,prunus-class (:foreground ,prunus-shadow))))

   ;; tooltip
   `(tooltip ((,prunus-class (:weight bold :foreground ,prunus-black :background ,prunus-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,prunus-class (:background ,prunus-region-1 :extend t))))
   `(secondary-selection ((,prunus-class (:background ,prunus-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,prunus-class (:foreground ,prunus-fl-comment :weight medium))))
   `(font-lock-string-face ((,prunus-class (:foreground ,prunus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,prunus-class (:foreground ,prunus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,prunus-class (:foreground ,prunus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,prunus-class (:foreground ,prunus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,prunus-class (:foreground ,prunus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,prunus-class (:foreground ,prunus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,prunus-class (:foreground ,prunus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,prunus-class (:foreground ,prunus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,prunus-class (:foreground ,prunus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,prunus-class (:foreground ,prunus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,prunus-class (:foreground ,prunus-white :background ,prunus-mode-line))))

   ;; fringe
   `(fringe ((,prunus-class (:foreground ,prunus-cyan :background ,prunus-bg))))
   `(diff-hl-change ((,prunus-class (:foreground ,prunus-change :background ,prunus-change))))
   `(diff-hl-insert ((,prunus-class (:foreground ,prunus-success :background ,prunus-success))))
   `(diff-hl-delete ((,prunus-class (:foreground ,prunus-error :background ,prunus-error))))

   ;; line-number
   `(line-number ((,prunus-class (:foreground ,prunus-fg-1 :inherit default))))
   `(line-number-current-line ((,prunus-class (:foreground ,prunus-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,prunus-class (:background ,prunus-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,prunus-class (:background ,prunus-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,prunus-class (:foreground ,prunus-white :background ,prunus-mode-line
                                            :box ,(if guava-themes-boxed-mode-line
                                                      '(:line-width -1 :style released-button)
                                                    'unspecified)))))
   `(mode-line-inactive ((,prunus-class (:background ,prunus-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,prunus-class (:weight bold))))
   `(mode-line-highlight ((,prunus-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,prunus-class (:foreground ,prunus-white :background ,prunus-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,prunus-class (:weight bold :foreground ,prunus-mode-line))))

   ;; borders
   `(vertical-border ((,prunus-class (:foreground ,prunus-mode-line))))

   ;; header-line
   `(header-line ((,prunus-class (:inherit mode-line))))
   `(which-func ((,prunus-class (:weight bold :foreground ,prunus-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-2 :inherit tab-bar))))

   ;; tab-line
   `(tab-line ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,prunus-class (:foreground ,prunus-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,prunus-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,prunus-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,prunus-class (:foreground ,prunus-white :background ,prunus-bell))))
   `(show-paren-mismatch ((,prunus-class (:foreground ,prunus-white :background ,prunus-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,prunus-class (:background ,prunus-error))))

   ;; links
   `(link ((,prunus-class (:foreground ,prunus-deep-blue :underline t :weight bold))))
   `(link-visited ((,prunus-class (:foreground ,prunus-cyan :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,prunus-class (:foreground ,prunus-coffee :weight medium))))
   `(outline-2 ((,prunus-class (:foreground ,prunus-green-forest :weight medium))))
   `(outline-4 ((,prunus-class (:foreground ,prunus-deep-blue :weight medium))))
   `(outline-3 ((,prunus-class (:foreground ,prunus-red :weight medium))))
   `(outline-5 ((,prunus-class (:inherit outline-1))))
   `(outline-6 ((,prunus-class (:inherit outline-2))))
   `(outline-7 ((,prunus-class (:inherit outline-3))))
   `(outline-8 ((,prunus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,prunus-class (:foreground ,prunus-cyan))))
   `(escape-glyph ((,prunus-class (:inherit homoglyph))))
   `(nobreak-space ((,prunus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,prunus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,prunus-class (:background ,prunus-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,prunus-class (:foreground ,prunus-cyan :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-header :extend t))))
   `(diff-file-header ((,prunus-class (:weight bold :foreground ,prunus-white :background ,prunus-diff-file-header :extend t))))
   `(diff-context ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,prunus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,prunus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,prunus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,prunus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,prunus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,prunus-class (:foreground ,prunus-white :background ,prunus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,prunus-class (:foreground ,prunus-warning :weight bold))))
   `(completions-first-difference ((,prunus-class (:foreground ,prunus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,prunus-class (:foreground ,prunus-error :weight bold))))
   `(org-done ((,prunus-class (:foreground ,prunus-success :weight bold))))
   `(org-hide ((,prunus-class (:foreground ,prunus-bg))))
   `(org-table ((,prunus-class (:foreground ,prunus-light-pink))))
   `(org-table-header ((,prunus-class (:inverse-video t :inherit org-table))))
   `(org-date ((,prunus-class (:foreground ,prunus-deep-red))))
   `(org-date-selected ((,prunus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,prunus-class (:foreground ,prunus-square-a-2))))
   `(org-headline-done ((,prunus-class (:foreground ,prunus-square-a-3))))
   `(org-document-title ((,prunus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,prunus-class (:inherit shadow))))
   `(org-meta-line ((,prunus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,prunus-class (:foreground ,prunus-tab-3))))
   `(window-divider-first-pixel ((,prunus-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,prunus-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,prunus-class (:foreground ,prunus-white :background ,prunus-square-a-2))))
   `(isearch-fail ((,prunus-class (:foreground ,prunus-white :background ,prunus-error))))
   `(lazy-highlight ((,prunus-class (:foreground ,prunus-white :background ,prunus-square-a-3))))
   `(isearch-group-1 ((,prunus-class (:foreground ,prunus-white :background ,prunus-square-a-0))))
   `(isearch-group-2 ((,prunus-class (:foreground ,prunus-white :background ,prunus-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,prunus-class (:inherit isearch))))
   `(match ((,prunus-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,prunus-class (:foreground ,prunus-white :background ,prunus-mode-line :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,prunus-class (:foreground ,prunus-white :background ,prunus-mode-line :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,prunus-class (:foreground ,prunus-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,prunus-class (:foreground ,prunus-bell :inherit custom-button-unraised))))
   `(custom-comment ((,prunus-class (:background ,prunus-tab-3))))
   `(custom-variable-tag ((,prunus-class (:weight bold :foreground ,prunus-tab-2))))
   `(custom-group-tag ((,prunus-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,prunus-class (:foreground ,prunus-fl-keyword))))

   ;; widget
   `(widget-field ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-3))))

   ;; package
   `(package-status-dependency ((,prunus-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,prunus-class (:weight bold :foreground ,prunus-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,prunus-class (:weight bold :foreground ,prunus-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,prunus-class (:foreground ,prunus-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,prunus-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,prunus-class (:underline (:style wave :color ,prunus-success)))))
   `(flymake-warning ((,prunus-class (:underline (:style wave :color ,prunus-warning)))))
   `(flymake-error ((,prunus-class (:underline (:style wave :color ,prunus-error)))))

   ;; eww
   `(eww-valid-certificate ((,prunus-class (:weight bold :foreground ,prunus-success))))
   `(eww-invalid-certificate ((,prunus-class (:weight bold :foreground ,prunus-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,prunus-class (:foreground ,prunus-fg-1 :background ,prunus-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,prunus-class (:foreground ,prunus-fg-1 :background ,prunus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,prunus-class (:foreground ,prunus-fg-1 :background ,prunus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,prunus-class (:foreground ,prunus-fg-1 :background ,prunus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,prunus-class (:foreground ,prunus-fg-1 :background ,prunus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,prunus-class (:foreground ,prunus-fg-1 :background ,prunus-tab-3 :box (:line-width 1 :color ,prunus-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,prunus-class (:foreground ,prunus-light-pink))))
   `(elfeed-search-date-face ((,prunus-class (:foreground ,prunus-light-red))))
   `(elfeed-search-feed-face ((,prunus-class (:foreground ,prunus-pink))))
   `(elfeed-search-title-face ((,prunus-class (:foreground ,prunus-alt-brown))))
   `(elfeed-search-unread-title-face ((,prunus-class (:weight bold :foreground ,prunus-alt-light-brown))))
   `(elfeed-search-filter-face ((,prunus-class (:weight bold :foreground ,prunus-light-pink))))
   `(elfeed-search-last-update-face ((,prunus-class (:weight bold :foreground ,prunus-light-pink))))
   `(elfeed-search-unread-count-face ((,prunus-class (:weight bold :foreground ,prunus-light-pink))))

   `(elfeed-show-header-face ((,prunus-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,prunus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,prunus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,prunus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,prunus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,prunus-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,prunus-class (:foreground ,prunus-green-forest :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,prunus-class (:foreground ,prunus-green-forest :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,prunus-class (:foreground ,prunus-shadow :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,prunus-class (:foreground ,prunus-shadow :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,prunus-class (:foreground ,prunus-shadow :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,prunus-class (:foreground ,prunus-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,prunus-class (:foreground ,prunus-fg-1 :background ,prunus-bg))))
   `(corfu-current ((,prunus-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,prunus-class (:background ,prunus-shadow))))
   `(corfu-border ((,prunus-class (:background ,prunus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,prunus-class (:foreground ,prunus-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,prunus-class (:foreground ,prunus-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,prunus-class (:foreground ,prunus-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,prunus-class (:foreground ,prunus-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,prunus-class (:foreground ,prunus-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,prunus-class (:foreground ,prunus-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,prunus-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,prunus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,prunus-class (:inherit success))))
   `(envrc-mode-line-none-face ((,prunus-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,prunus-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,prunus-class (:weight bold :background ,prunus-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,prunus-class (:foreground ,prunus-white :background ,prunus-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,prunus-class (:foreground ,prunus-white :background ,prunus-fl-builtin :inherit region))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,prunus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-prunus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-prunus-theme.el ends here
