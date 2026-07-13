;;; guava-themes-dracaena-theme.el --- A theme inspired by the dragon tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Jan 06, 2026
;; Version: 0.18.0
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
;; A theme inspired by the dragon tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-dracaena "A theme inspired by the dragon tree colors.")

(let* (
      (dracaena-class '((class color) (min-colors 257)))
      (dracaena-black                     "#000000")
      (dracaena-white                     "#FFFFFF")

      (dracaena-gray                      "#4f4f4f")
      (dracaena-dark-gray                 "#353838")

      (dracaena-light-green               "#78be78")
      (dracaena-green                     "#288c23")

      (dracaena-snakeplant-yellow         "#dcd78c")

      (dracaena-light-orange              "#ffb38f")
      (dracaena-orange                    "#e6825f")
      (dracaena-orange-red                "#f06e6e")
      (dracaena-red                       "#c83c50")
      (dracaena-alt-deep-red              "#751e1e")
      (dracaena-deep-red                  "#792725")
      (dracaena-pink                      "#cd6eaf")

      (dracaena-light-brown               "#a89d92")

      (dracaena-light-blue                "#5a8ce6")
      (dracaena-blue                      "#2d50d5")
      (dracaena-steel-blue                "#4f94cd")
      (dracaena-cyan                      "#00ffff")
      (dracaena-dark-cyan                 "#005f55")

      (dracaena-fg                        "#becee6")
      (dracaena-bg                        "#3e4441")
      (dracaena-highlight                 "#2a302d")
      (dracaena-shadow                    "#b3b3b3")

      (dracaena-error                     "#FF0000")
      (dracaena-warning                   "#f6be14")
      (dracaena-success                   "#29d925")

      (dracaena-mode-line                 "#792725")
      (dracaena-mode-line-inactive        "#353838")
      (dracaena-bell                      "#257950")

      (dracaena-tab-1                     dracaena-mode-line)
      (dracaena-tab-2                     dracaena-mode-line-inactive)
      (dracaena-tab-3                     dracaena-gray)

      (dracaena-fl-comment                dracaena-green)
      (dracaena-fl-string                 dracaena-snakeplant-yellow)
      (dracaena-fl-keyword                dracaena-light-green)
      (dracaena-fl-builtin                dracaena-red)
      (dracaena-fl-type                   dracaena-pink)
      (dracaena-fl-function-name          dracaena-light-orange)
      (dracaena-fl-variable-name          dracaena-orange-red)
      (dracaena-fl-constant               dracaena-light-blue)
      (dracaena-fl-warning                dracaena-warning)
      (dracaena-fl-punctuation            dracaena-light-brown)
      (dracaena-fl-negation-char          dracaena-orange)

      (dracaena-diff-added                "#5aa05a")
      (dracaena-diff-removed              "#a05a5a")
      (dracaena-diff-refine-added         "#007800")
      (dracaena-diff-refine-removed       "#780000")
      (dracaena-diff-header               "#5a5a5a")
      (dracaena-diff-file-header          "#3c3c3c")
      (dracaena-diff-context              "#828282")
      (dracaena-smerge-base               "#5A5AA0")

      (dracaena-orderless-0               "#af50b9")
      (dracaena-orderless-1               "#28a03c")
      (dracaena-orderless-2               "#ff6400")
      (dracaena-orderless-3               "#3c82ff")

      (dracaena-prescient-0               "#46E6A6")
      (dracaena-prescient-1               "#E64686")

      (dracaena-vc-change                 dracaena-blue)
      (dracaena-vc-insert                 dracaena-green)
      (dracaena-vc-delete                 dracaena-error))

  (custom-theme-set-faces
   'guava-themes-dracaena

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,dracaena-class (:foreground ,dracaena-fg :background ,dracaena-bg))))

   ;; error, warning, success
   `(error ((,dracaena-class (:foreground ,dracaena-error :weight bold))))
   `(warning ((,dracaena-class (:foreground ,dracaena-warning :weight bold))))
   `(success ((,dracaena-class (:foreground ,dracaena-success :weight bold))))

   ;; highlight
   `(highlight ((,dracaena-class (:background ,dracaena-highlight))))

   ;; shadow
   `(shadow ((,dracaena-class (:foreground ,dracaena-shadow))))

   ;; region
   `(region ((,dracaena-class (:background ,dracaena-alt-deep-red :extend t))))
   `(secondary-selection ((,dracaena-class (:background ,dracaena-dark-cyan :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,dracaena-class (:foreground ,dracaena-fl-comment :weight medium))))
   `(font-lock-string-face ((,dracaena-class (:foreground ,dracaena-fl-string :weight medium))))
   `(font-lock-keyword-face ((,dracaena-class (:foreground ,dracaena-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,dracaena-class (:foreground ,dracaena-fl-builtin :weight medium))))
   `(font-lock-type-face ((,dracaena-class (:foreground ,dracaena-fl-type :weight medium))))
   `(font-lock-function-name-face ((,dracaena-class (:foreground ,dracaena-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,dracaena-class (:foreground ,dracaena-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,dracaena-class (:foreground ,dracaena-fl-constant :weight medium))))
   `(font-lock-warning-face ((,dracaena-class (:foreground ,dracaena-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,dracaena-class (:foreground ,dracaena-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,dracaena-class (:foreground ,dracaena-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-mode-line))))

   ;; fringe
   `(fringe ((,dracaena-class (:foreground ,dracaena-light-orange :background ,dracaena-bg))))
   `(diff-hl-change ((,dracaena-class (:foreground ,dracaena-vc-change :background ,dracaena-vc-change))))
   `(diff-hl-insert ((,dracaena-class (:foreground ,dracaena-vc-insert :background ,dracaena-vc-insert))))
   `(diff-hl-delete ((,dracaena-class (:foreground ,dracaena-vc-delete :background ,dracaena-vc-delete))))

   ;; line-number
   `(line-number ((,dracaena-class (:foreground ,dracaena-fg :inherit default))))
   `(line-number-current-line ((,dracaena-class (:foreground ,dracaena-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,dracaena-class (:background ,dracaena-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,dracaena-class (:background ,dracaena-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-mode-line))))
   `(mode-line-inactive ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,dracaena-class (:foreground ,dracaena-orange))))

   ;; borders
   `(vertical-border ((,dracaena-class (:foreground ,dracaena-mode-line))))

   ;; header-line
   `(header-line ((,dracaena-class (:inherit mode-line))))
   `(which-func ((,dracaena-class (:foreground ,dracaena-white))))

   ;; tab-bar
   `(tab-bar ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,dracaena-class (:foreground ,dracaena-orange :weight bold :height 0.9))))
   `(tab-line-tab-special ((,dracaena-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-bell))))
   `(show-paren-mismatch ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,dracaena-class (:background ,dracaena-error))))

   ;; links
   `(link ((,dracaena-class (:foreground ,dracaena-red :underline t :weight bold))))
   `(link-visited ((,dracaena-class (:foreground ,dracaena-orange :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,dracaena-class (:foreground ,dracaena-light-orange :weight medium))))
   `(outline-2 ((,dracaena-class (:foreground ,dracaena-light-green :weight medium))))
   `(outline-3 ((,dracaena-class (:foreground ,dracaena-pink :weight medium))))
   `(outline-4 ((,dracaena-class (:foreground ,dracaena-light-blue :weight medium))))
   `(outline-5 ((,dracaena-class (:inherit outline-1))))
   `(outline-6 ((,dracaena-class (:inherit outline-2))))
   `(outline-7 ((,dracaena-class (:inherit outline-3))))
   `(outline-8 ((,dracaena-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,dracaena-class (:foreground ,dracaena-cyan))))
   `(escape-glyph ((,dracaena-class (:inherit homoglyph))))
   `(nobreak-space ((,dracaena-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,dracaena-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,dracaena-class (:background ,dracaena-steel-blue))))

   ;; help-key-binding
   `(help-key-binding ((,dracaena-class (:foreground ,dracaena-snakeplant-yellow :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-diff-header :extend t))))
   `(diff-file-header ((,dracaena-class (:weight bold :foreground ,dracaena-white :background ,dracaena-diff-file-header :extend t))))
   `(diff-context ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,dracaena-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,dracaena-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,dracaena-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,dracaena-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,dracaena-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,dracaena-class (:foreground ,dracaena-warning :weight bold))))
   `(completions-first-difference ((,dracaena-class (:foreground ,dracaena-error :weight bold))))

   ;; org-faces
   `(org-todo ((,dracaena-class (:foreground ,dracaena-vc-delete :weight bold))))
   `(org-done ((,dracaena-class (:foreground ,dracaena-vc-insert :weight bold))))
   `(org-hide ((,dracaena-class (:foreground ,dracaena-bg))))
   `(org-table ((,dracaena-class (:foreground ,dracaena-light-orange))))
   `(org-date ((,dracaena-class (:foreground ,dracaena-orange))))
   `(org-date-selected ((,dracaena-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,dracaena-class (:foreground ,dracaena-orderless-2))))
   `(org-headline-done ((,dracaena-class (:foreground ,dracaena-orderless-1))))
   `(org-document-title ((,dracaena-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,dracaena-class (:inherit shadow))))
   `(org-meta-line ((,dracaena-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,dracaena-class (:foreground ,dracaena-tab-3))))
   `(window-divider-first-pixel ((,dracaena-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,dracaena-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-orderless-2))))
   `(isearch-fail ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-error))))
   `(lazy-highlight ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-orderless-3))))
   `(isearch-group-1 ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-orderless-0))))
   `(isearch-group-2 ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,dracaena-class (:inherit isearch))))
   `(match ((,dracaena-class (:inherit lazy-highlight))))

   ;; custom-button
   `(custom-button ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-mode-line :box (:line-width 2 :style released-button)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,dracaena-class (:foreground ,dracaena-steel-blue))))
   `(elfeed-search-date-face ((,dracaena-class (:foreground ,dracaena-light-blue))))
   `(elfeed-search-feed-face ((,dracaena-class (:foreground ,dracaena-pink))))
   `(elfeed-search-title-face ((,dracaena-class (:foreground ,dracaena-snakeplant-yellow))))
   `(elfeed-search-unread-title-face ((,dracaena-class (:weight bold :foreground ,dracaena-green))))
   `(elfeed-search-filter-face ((,dracaena-class (:weight bold :foreground ,dracaena-light-green))))
   `(elfeed-search-last-update-face ((,dracaena-class (:weight bold :foreground ,dracaena-light-green))))
   `(elfeed-search-unread-count-face ((,dracaena-class (:weight bold :foreground ,dracaena-light-green))))

   `(elfeed-show-header-face ((,dracaena-class (:foreground ,dracaena-pink))))
   `(elfeed-show-author-face ((,dracaena-class (:weight bold :foreground ,dracaena-green))))
   `(elfeed-show-title-face ((,dracaena-class (:weight bold :foreground ,dracaena-green))))
   `(elfeed-show-date-face ((,dracaena-class (:foreground ,dracaena-steel-blue))))
   `(elfeed-show-feed-face ((,dracaena-class (:foreground ,dracaena-steel-blue))))
   `(elfeed-show-tags-face ((,dracaena-class (:foreground ,dracaena-snakeplant-yellow))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,dracaena-class (:foreground ,dracaena-snakeplant-yellow :inherit italic))))
   `(doom-modeline-project-parent-dir ((,dracaena-class (:foreground ,dracaena-snakeplant-yellow))))
   `(doom-modeline-buffer-minor-mode ((,dracaena-class (:foreground ,dracaena-green))))

   ;; corfu
   `(corfu-default ((,dracaena-class (:foreground ,dracaena-fg :background ,dracaena-bg))))
   `(corfu-current ((,dracaena-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,dracaena-class (:background ,dracaena-shadow))))
   `(corfu-border ((,dracaena-class (:background ,dracaena-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,dracaena-class (:foreground ,dracaena-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,dracaena-class (:foreground ,dracaena-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,dracaena-class (:foreground ,dracaena-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,dracaena-class (:foreground ,dracaena-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,dracaena-class (:foreground ,dracaena-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,dracaena-class (:foreground ,dracaena-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,dracaena-class (:inherit error))))
   `(envrc-mode-line-none-face ((,dracaena-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,dracaena-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,dracaena-class (:weight bold :background ,dracaena-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,dracaena-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-dracaena)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-dracaena-theme.el ends here
