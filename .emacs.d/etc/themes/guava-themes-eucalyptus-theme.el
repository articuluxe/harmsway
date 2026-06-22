;;; guava-themes-eucalyptus-theme.el --- A theme inspired by the eucalyptus tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Jun 12, 2026
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
;; A theme inspired by the eucalyptus tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-eucalyptus "A theme inspired by the eucalyptus tree colors.")

(let* (
      (eucalyptus-class '((class color) (min-colors 257)))
      (eucalyptus-black                     "#000000")
      (eucalyptus-white                     "#FFFFFF")

      (eucalyptus-light-gray                "#9196a5")
      (eucalyptus-gray                      "#646464")
      (eucalyptus-gray-green                "#5a6450")

      (eucalyptus-green                     "#50693c")
      (eucalyptus-light-green               "#448223")

      (eucalyptus-deep-red                  "#82312e")

      (eucalyptus-yellow                    "#e1e696")

      (eucalyptus-light-blue                "#2d8296")
      (eucalyptus-alt-light-blue            "#96B9E6")
      (eucalyptus-blue                      "#2327dc")
      (eucalyptus-deep-blue                 "#46466e")
      (eucalyptus-cyan                      "#00ffff")
      (eucalyptus-alt-cyan                  "#008cd2")

      (eucalyptus-purple-red                "#874164")

      (eucalyptus-brown-wood                "#675951")

      (eucalyptus-fg                        "#373c4b")
      (eucalyptus-bg                        "#a5aa96")
      (eucalyptus-highlight                 "#919682")
      (eucalyptus-shadow                    "#7f7f7f")

      (eucalyptus-error                     "#e10000")
      (eucalyptus-warning                   "#f6c911")
      (eucalyptus-success                   "#1ea01e")

      (eucalyptus-mode-line                 "#373c4b")
      (eucalyptus-mode-line-inactive        "#737887")
      (eucalyptus-bell                      "#464B37")

      (eucalyptus-tab-1                     eucalyptus-mode-line)
      (eucalyptus-tab-2                     eucalyptus-mode-line-inactive)
      (eucalyptus-tab-3                     eucalyptus-light-gray)

      (eucalyptus-fl-comment                eucalyptus-green)
      (eucalyptus-fl-string                 eucalyptus-purple-red)
      (eucalyptus-fl-keyword                eucalyptus-deep-red)
      (eucalyptus-fl-builtin                eucalyptus-deep-blue)
      (eucalyptus-fl-type                   eucalyptus-light-green)
      (eucalyptus-fl-function-name          eucalyptus-brown-wood)
      (eucalyptus-fl-variable-name          eucalyptus-gray-green)
      (eucalyptus-fl-constant               eucalyptus-light-blue)
      (eucalyptus-fl-warning                eucalyptus-warning)
      (eucalyptus-fl-punctuation            eucalyptus-gray)
      (eucalyptus-fl-negation-char          eucalyptus-alt-cyan)

      (eucalyptus-diff-added                "#c8f0c8")
      (eucalyptus-diff-removed              "#f0c8c8")
      (eucalyptus-diff-refine-added         "#78f078")
      (eucalyptus-diff-refine-removed       "#f07878")
      (eucalyptus-diff-header               "#b4b4b4")
      (eucalyptus-diff-file-header          "#8c8c8c")
      (eucalyptus-diff-context              "#dcdcdc")
      (eucalyptus-smerge-base               "#C8C8F0")

      (eucalyptus-orderless-0               "#af37b9")
      (eucalyptus-orderless-1               "#147828")
      (eucalyptus-orderless-2               "#b45a00")
      (eucalyptus-orderless-3               "#3c82e6")

      (eucalyptus-prescient-0               "#4680DE")
      (eucalyptus-prescient-1               "#DE4680")

      (eucalyptus-vc-change                 eucalyptus-blue)
      (eucalyptus-vc-insert                 eucalyptus-success)
      (eucalyptus-vc-delete                 eucalyptus-error))

  (custom-theme-set-faces
   'guava-themes-eucalyptus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,eucalyptus-class (:foreground ,eucalyptus-fg :background ,eucalyptus-bg))))

   ;; error, warning, success
   `(error ((,eucalyptus-class (:foreground ,eucalyptus-error :weight bold))))
   `(warning ((,eucalyptus-class (:foreground ,eucalyptus-warning :weight bold))))
   `(success ((,eucalyptus-class (:foreground ,eucalyptus-success :weight bold))))

   ;; highlight
   `(highlight ((,eucalyptus-class (:background ,eucalyptus-highlight))))

   ;; shadow
   `(shadow ((,eucalyptus-class (:foreground ,eucalyptus-shadow))))

   ;; region
   `(region ((,eucalyptus-class (:background ,eucalyptus-yellow :extend t))))
   `(secondary-selection ((,eucalyptus-class (:background ,eucalyptus-alt-light-blue :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-comment :weight medium))))
   `(font-lock-string-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-mode-line))))

   ;; fringe
   `(fringe ((,eucalyptus-class (:foreground ,eucalyptus-blue :background ,eucalyptus-bg))))
   `(diff-hl-change ((,eucalyptus-class (:foreground ,eucalyptus-vc-change :background ,eucalyptus-vc-change))))
   `(diff-hl-insert ((,eucalyptus-class (:foreground ,eucalyptus-vc-insert :background ,eucalyptus-vc-insert))))
   `(diff-hl-delete ((,eucalyptus-class (:foreground ,eucalyptus-vc-delete :background ,eucalyptus-vc-delete))))

   ;; line-number
   `(line-number ((,eucalyptus-class (:foreground ,eucalyptus-fg :inherit default))))
   `(line-number-current-line ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,eucalyptus-class (:background ,eucalyptus-green :inherit line-number))))
   `(line-number-major-tick ((,eucalyptus-class (:background ,eucalyptus-gray-green :inherit line-number))))

   ;; mode-line
   `(mode-line ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-mode-line))))
   `(mode-line-inactive ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue))))

   ;; borders
   `(vertical-border ((,eucalyptus-class (:foreground ,eucalyptus-mode-line))))

   ;; header-line
   `(header-line ((,eucalyptus-class (:inherit mode-line))))
   `(which-func ((,eucalyptus-class (:foreground ,eucalyptus-white))))

   ;; tab-bar
   `(tab-bar ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,eucalyptus-class (:foreground ,eucalyptus-deep-red :weight bold :height 0.9))))
   `(tab-line-tab-special ((,eucalyptus-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-bell))))
   `(show-paren-mismatch ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,eucalyptus-class (:background ,eucalyptus-error))))

   ;; links
   `(link ((,eucalyptus-class (:foreground ,eucalyptus-light-blue :underline t :weight bold))))
   `(link-visited ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue :weight medium))))
   `(outline-2 ((,eucalyptus-class (:foreground ,eucalyptus-brown-wood :weight medium))))
   `(outline-3 ((,eucalyptus-class (:foreground ,eucalyptus-deep-red :weight medium))))
   `(outline-4 ((,eucalyptus-class (:foreground ,eucalyptus-gray-green :weight medium))))
   `(outline-5 ((,eucalyptus-class (:inherit outline-1))))
   `(outline-6 ((,eucalyptus-class (:inherit outline-2))))
   `(outline-7 ((,eucalyptus-class (:inherit outline-3))))
   `(outline-8 ((,eucalyptus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,eucalyptus-class (:foreground ,eucalyptus-cyan))))
   `(escape-glyph ((,eucalyptus-class (:inherit homoglyph))))
   `(nobreak-space ((,eucalyptus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,eucalyptus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,eucalyptus-class (:background ,eucalyptus-alt-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-header :extend t))))
   `(diff-file-header ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-black :background ,eucalyptus-diff-file-header :extend t))))
   `(diff-context ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,eucalyptus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,eucalyptus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,eucalyptus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,eucalyptus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,eucalyptus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,eucalyptus-class (:foreground ,eucalyptus-vc-change :weight bold))))
   `(completions-first-difference ((,eucalyptus-class (:foreground ,eucalyptus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,eucalyptus-class (:foreground ,eucalyptus-vc-delete :weight bold))))
   `(org-done ((,eucalyptus-class (:foreground ,eucalyptus-vc-insert :weight bold))))
   `(org-hide ((,eucalyptus-class (:foreground ,eucalyptus-bg))))
   `(org-table ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue))))
   `(org-date ((,eucalyptus-class (:foreground ,eucalyptus-purple-red))))
   `(org-date-selected ((,eucalyptus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,eucalyptus-class (:foreground ,eucalyptus-orderless-2))))
   `(org-headline-done ((,eucalyptus-class (:foreground ,eucalyptus-orderless-1))))
   `(org-document-title ((,eucalyptus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,eucalyptus-class (:inherit shadow))))
   `(org-meta-line ((,eucalyptus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,eucalyptus-class (:foreground ,eucalyptus-tab-3))))
   `(window-divider-first-pixel ((,eucalyptus-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,eucalyptus-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-orderless-2))))
   `(isearch-fail ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-error))))
   `(lazy-highlight ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-orderless-3))))
   `(isearch-group-1 ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-orderless-0))))
   `(isearch-group-2 ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,eucalyptus-class (:inherit isearch))))
   `(match ((,eucalyptus-class (:inherit lazy-highlight))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,eucalyptus-class (:foreground ,eucalyptus-brown-wood))))
   `(elfeed-search-date-face ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue))))
   `(elfeed-search-feed-face ((,eucalyptus-class (:foreground ,eucalyptus-purple-red))))
   `(elfeed-search-title-face ((,eucalyptus-class (:foreground ,eucalyptus-gray-green))))
   `(elfeed-search-unread-title-face ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-green))))
   `(elfeed-search-filter-face ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-light-blue))))
   `(elfeed-search-last-update-face ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-light-blue))))
   `(elfeed-search-unread-count-face ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-light-blue))))

   `(elfeed-show-header-face ((,eucalyptus-class (:foreground ,eucalyptus-green))))
   `(elfeed-show-author-face ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-deep-blue))))
   `(elfeed-show-title-face ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-deep-blue))))
   `(elfeed-show-date-face ((,eucalyptus-class (:foreground ,eucalyptus-brown-wood))))
   `(elfeed-show-feed-face ((,eucalyptus-class (:foreground ,eucalyptus-brown-wood))))
   `(elfeed-show-tags-face ((,eucalyptus-class (:foreground ,eucalyptus-purple-red))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,eucalyptus-class (:foreground ,eucalyptus-light-blue :inherit italic))))
   `(doom-modeline-project-parent-dir ((,eucalyptus-class (:foreground ,eucalyptus-light-blue))))
   `(doom-modeline-buffer-minor-mode ((,eucalyptus-class (:foreground ,eucalyptus-tab-3))))

   ;; corfu
   `(corfu-default ((,eucalyptus-class (:foreground ,eucalyptus-fg :background ,eucalyptus-bg))))
   `(corfu-current ((,eucalyptus-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,eucalyptus-class (:background ,eucalyptus-shadow))))
   `(corfu-border ((,eucalyptus-class (:background ,eucalyptus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,eucalyptus-class (:foreground ,eucalyptus-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,eucalyptus-class (:foreground ,eucalyptus-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,eucalyptus-class (:foreground ,eucalyptus-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,eucalyptus-class (:foreground ,eucalyptus-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,eucalyptus-class (:foreground ,eucalyptus-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,eucalyptus-class (:foreground ,eucalyptus-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,eucalyptus-class (:inherit error))))
   `(envrc-mode-line-none-face ((,eucalyptus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,eucalyptus-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,eucalyptus-class (:weight bold :background ,eucalyptus-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,eucalyptus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-eucalyptus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-eucalyptus-theme.el ends here
