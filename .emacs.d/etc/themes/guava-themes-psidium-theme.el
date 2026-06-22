;;; guava-themes-psidium-theme.el --- A theme inspired by guava colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Dec 07, 2025
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
;; A theme inspired by guava colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-psidium "A theme inspired by guava colors.")

(let* (
      (psidium-class '((class color) (min-colors 257)))
      (psidium-black                     "#000000")
      (psidium-white                     "#FFFFFF")

      (psidium-green                     "#599b48")
      (psidium-light-green               "#28bb6b")
      (psidium-guava-green               "#aecd34")
      (psidium-deep-green                "#097d2c")

      (psidium-light-orange              "#ffbe9b")
      (psidium-red-orange                "#cd605f")
      (psidium-red                       "#c1153b")
      (psidium-pink                      "#F8767C")

      (psidium-brown                     "#785f46")

      (psidium-light-blue                "#41C3CA")
      (psidium-blue                      "#2a4ad9")
      (psidium-deep-blue                 "#483d8b")

      (psidium-light-purple              "#8cb4d1")
      (psidium-purple                    "#812db2")

      (psidium-fg                        "#000000")
      (psidium-bg                        "#F1EECE")
      (psidium-highlight                 "#dddaba")
      (psidium-shadow                    "#7f7f7f")

      (psidium-error                     "#FF0000")
      (psidium-warning                   "#d6c800")
      (psidium-success                   "#228B22")

      (psidium-mode-line                 "#F8767C")
      (psidium-mode-line-inactive        "#aecd34")
      (psidium-bell                      "#76BEF8")

      (psidium-tab-1                     psidium-mode-line)
      (psidium-tab-2                     psidium-mode-line-inactive)
      (psidium-tab-3                     psidium-light-green)

      (psidium-fl-comment                psidium-green)
      (psidium-fl-string                 psidium-brown)
      (psidium-fl-keyword                psidium-red)
      (psidium-fl-builtin                psidium-deep-blue)
      (psidium-fl-type                   psidium-deep-green)
      (psidium-fl-function-name          psidium-light-green)
      (psidium-fl-variable-name          psidium-purple)
      (psidium-fl-constant               psidium-blue)
      (psidium-fl-warning                psidium-warning)
      (psidium-fl-punctuation            psidium-red-orange)
      (psidium-fl-negation-char          psidium-pink)

      (psidium-diff-added                "#c8f0c8")
      (psidium-diff-removed              "#f0c8c8")
      (psidium-diff-refine-added         "#78f078")
      (psidium-diff-refine-removed       "#f07878")
      (psidium-diff-header               "#b4b4b4")
      (psidium-diff-file-header          "#8c8c8c")
      (psidium-diff-context              "#dcdcdc")
      (psidium-smerge-base               "#C8C8F0")

      (psidium-orderless-0               "#af50c8")
      (psidium-orderless-1               "#28a03c")
      (psidium-orderless-2               "#ff6400")
      (psidium-orderless-3               "#3c82ff")

      (psidium-prescient-0               "#4680DE")
      (psidium-prescient-1               "#DE4680")

      (psidium-vc-change                 psidium-blue)
      (psidium-vc-insert                 psidium-success)
      (psidium-vc-delete                 psidium-error))

  (custom-theme-set-faces
   'guava-themes-psidium

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,psidium-class (:foreground ,psidium-fg :background ,psidium-bg))))

   ;; error, warning, success
   `(error ((,psidium-class (:foreground ,psidium-error :weight bold))))
   `(warning ((,psidium-class (:foreground ,psidium-warning :weight bold))))
   `(success ((,psidium-class (:foreground ,psidium-success :weight bold))))

   ;; highlight
   `(highlight ((,psidium-class (:background ,psidium-highlight))))

   ;; shadow
   `(shadow ((,psidium-class (:foreground ,psidium-shadow))))

   ;; region
   `(region ((,psidium-class (:background ,psidium-light-orange :extend t))))
   `(secondary-selection ((,psidium-class (:background ,psidium-light-purple :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,psidium-class (:foreground ,psidium-fl-comment :weight medium))))
   `(font-lock-string-face ((,psidium-class (:foreground ,psidium-fl-string :weight medium))))
   `(font-lock-keyword-face ((,psidium-class (:foreground ,psidium-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,psidium-class (:foreground ,psidium-fl-builtin :weight medium))))
   `(font-lock-type-face ((,psidium-class (:foreground ,psidium-fl-type :weight medium))))
   `(font-lock-function-name-face ((,psidium-class (:foreground ,psidium-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,psidium-class (:foreground ,psidium-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,psidium-class (:foreground ,psidium-fl-constant :weight medium))))
   `(font-lock-warning-face ((,psidium-class (:foreground ,psidium-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,psidium-class (:foreground ,psidium-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,psidium-class (:foreground ,psidium-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,psidium-class (:foreground ,psidium-black :background ,psidium-mode-line))))

   ;; fringe
   `(fringe ((,psidium-class (:foreground ,psidium-blue :background ,psidium-bg))))
   `(diff-hl-change ((,psidium-class (:foreground ,psidium-vc-change :background ,psidium-vc-change))))
   `(diff-hl-insert ((,psidium-class (:foreground ,psidium-vc-insert :background ,psidium-vc-insert))))
   `(diff-hl-delete ((,psidium-class (:foreground ,psidium-vc-delete :background ,psidium-vc-delete))))

   ;; line-number
   `(line-number ((,psidium-class (:foreground ,psidium-brown :inherit default))))
   `(line-number-current-line ((,psidium-class (:foreground ,psidium-fg :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,psidium-class (:background ,psidium-light-orange :inherit line-number))))
   `(line-number-major-tick ((,psidium-class (:background ,psidium-light-purple :inherit line-number))))

   ;; mode-line
   `(mode-line ((,psidium-class (:foreground ,psidium-white :background ,psidium-mode-line))))
   `(mode-line-inactive ((,psidium-class (:foreground ,psidium-white :background ,psidium-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,psidium-class (:foreground ,psidium-white :background ,psidium-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,psidium-class (:foreground ,psidium-black))))

   ;; borders
   `(vertical-border ((,psidium-class (:foreground ,psidium-mode-line))))

   ;; header-line
   `(header-line ((,psidium-class (:inherit mode-line))))
   `(which-func ((,psidium-class (:foreground ,psidium-white))))

   ;; tab-bar
   `(tab-bar ((,psidium-class (:foreground ,psidium-white :background ,psidium-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,psidium-class (:foreground ,psidium-white :background ,psidium-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,psidium-class (:foreground ,psidium-white :background ,psidium-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,psidium-class (:foreground ,psidium-white :background ,psidium-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,psidium-class (:foreground ,psidium-white :background ,psidium-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,psidium-class (:foreground ,psidium-white :background ,psidium-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,psidium-class (:foreground ,psidium-white :background ,psidium-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,psidium-class (:foreground ,psidium-white :background ,psidium-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,psidium-class (:foreground ,psidium-deep-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,psidium-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,psidium-class (:foreground ,psidium-black :background ,psidium-bell))))
   `(show-paren-mismatch ((,psidium-class (:foreground ,psidium-black :background ,psidium-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,psidium-class (:background ,psidium-error))))

   ;; links
   `(link ((,psidium-class (:foreground ,psidium-light-blue :underline t :weight bold))))
   `(link-visited ((,psidium-class (:foreground ,psidium-purple :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,psidium-class (:foreground ,psidium-red :weight medium))))
   `(outline-2 ((,psidium-class (:foreground ,psidium-blue :weight medium))))
   `(outline-3 ((,psidium-class (:foreground ,psidium-deep-green :weight medium))))
   `(outline-4 ((,psidium-class (:foreground ,psidium-brown :weight medium))))
   `(outline-5 ((,psidium-class (:inherit outline-1))))
   `(outline-6 ((,psidium-class (:inherit outline-2))))
   `(outline-7 ((,psidium-class (:inherit outline-3))))
   `(outline-8 ((,psidium-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,psidium-class (:foreground ,psidium-blue))))
   `(escape-glyph ((,psidium-class (:inherit homoglyph))))
   `(nobreak-space ((,psidium-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,psidium-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,psidium-class (:background ,psidium-light-blue))))

   ;; help-key-binding
   `(help-key-binding ((,psidium-class (:foreground ,psidium-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,psidium-class (:foreground ,psidium-black :background ,psidium-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,psidium-class (:foreground ,psidium-black :background ,psidium-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,psidium-class (:foreground ,psidium-black :background ,psidium-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,psidium-class (:foreground ,psidium-black :background ,psidium-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,psidium-class (:foreground ,psidium-black :background ,psidium-diff-header :extend t))))
   `(diff-file-header ((,psidium-class (:weight bold :foreground ,psidium-black :background ,psidium-diff-file-header :extend t))))
   `(diff-context ((,psidium-class (:foreground ,psidium-black :background ,psidium-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,psidium-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,psidium-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,psidium-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,psidium-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,psidium-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,psidium-class (:foreground ,psidium-black :background ,psidium-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,psidium-class (:foreground ,psidium-vc-change :weight bold))))
   `(completions-first-difference ((,psidium-class (:foreground ,psidium-error :weight bold))))

   ;; org-faces
   `(org-todo ((,psidium-class (:foreground ,psidium-vc-delete :weight bold))))
   `(org-done ((,psidium-class (:foreground ,psidium-vc-insert :weight bold))))
   `(org-hide ((,psidium-class (:foreground ,psidium-bg))))
   `(org-table ((,psidium-class (:foreground ,psidium-deep-green))))
   `(org-date ((,psidium-class (:foreground ,psidium-pink))))
   `(org-date-selected ((,psidium-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,psidium-class (:foreground ,psidium-orderless-2))))
   `(org-headline-done ((,psidium-class (:foreground ,psidium-orderless-3))))
   `(org-document-title ((,psidium-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,psidium-class (:inherit shadow))))
   `(org-meta-line ((,psidium-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,psidium-class (:foreground ,psidium-tab-3))))
   `(window-divider-first-pixel ((,psidium-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,psidium-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,psidium-class (:foreground ,psidium-white :background ,psidium-orderless-2))))
   `(isearch-fail ((,psidium-class (:foreground ,psidium-white :background ,psidium-error))))
   `(lazy-highlight ((,psidium-class (:foreground ,psidium-white :background ,psidium-orderless-3))))
   `(isearch-group-1 ((,psidium-class (:foreground ,psidium-white :background ,psidium-orderless-0))))
   `(isearch-group-2 ((,psidium-class (:foreground ,psidium-white :background ,psidium-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,psidium-class (:inherit isearch))))
   `(match ((,psidium-class (:inherit lazy-highlight))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,psidium-class (:foreground ,psidium-light-blue))))
   `(elfeed-search-date-face ((,psidium-class (:foreground ,psidium-red))))
   `(elfeed-search-feed-face ((,psidium-class (:foreground ,psidium-red-orange))))
   `(elfeed-search-title-face ((,psidium-class (:foreground ,psidium-brown))))
   `(elfeed-search-unread-title-face ((,psidium-class (:weight bold :foreground ,psidium-green))))
   `(elfeed-search-filter-face ((,psidium-class (:weight bold :foreground ,psidium-deep-blue))))
   `(elfeed-search-last-update-face ((,psidium-class (:weight bold :foreground ,psidium-deep-blue))))
   `(elfeed-search-unread-count-face ((,psidium-class (:weight bold :foreground ,psidium-deep-blue))))

   `(elfeed-show-header-face ((,psidium-class (:foreground ,psidium-deep-blue))))
   `(elfeed-show-author-face ((,psidium-class (:weight bold :foreground ,psidium-red))))
   `(elfeed-show-title-face ((,psidium-class (:weight bold :foreground ,psidium-red))))
   `(elfeed-show-date-face ((,psidium-class (:foreground ,psidium-deep-green))))
   `(elfeed-show-feed-face ((,psidium-class (:foreground ,psidium-deep-green))))
   `(elfeed-show-tags-face ((,psidium-class (:foreground ,psidium-purple))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,psidium-class (:foreground ,psidium-deep-blue :inherit italic))))
   `(doom-modeline-project-parent-dir ((,psidium-class (:foreground ,psidium-deep-blue))))
   `(doom-modeline-buffer-minor-mode ((,psidium-class (:foreground ,psidium-shadow))))

   ;; corfu
   `(corfu-default ((,psidium-class (:foreground ,psidium-fg :background ,psidium-bg))))
   `(corfu-current ((,psidium-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,psidium-class (:background ,psidium-shadow))))
   `(corfu-border ((,psidium-class (:background ,psidium-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,psidium-class (:foreground ,psidium-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,psidium-class (:foreground ,psidium-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,psidium-class (:foreground ,psidium-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,psidium-class (:foreground ,psidium-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,psidium-class (:foreground ,psidium-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,psidium-class (:foreground ,psidium-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,psidium-class (:inherit error))))
   `(envrc-mode-line-none-face ((,psidium-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,psidium-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,psidium-class (:weight bold :background ,psidium-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,psidium-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-psidium)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-psidium-theme.el ends here
