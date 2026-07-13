;;; guava-themes-acer-theme.el --- A theme inspired by maple colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Jan 12, 2026
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
;; A theme inspired by maple colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-acer "A theme inspired by maple colors.")

(let* (
      (acer-class '((class color) (min-colors 257)))
      (acer-black                     "#000000")
      (acer-white                     "#FFFFFF")

      (acer-green                     "#237c34")
      (acer-green-cyan                "#277a6a")
      (acer-deep-green                "#006041")

      (acer-orange                    "#e76144")
      (acer-deep-orange               "#d44400")
      (acer-orange-pink               "#cd475f")
      (acer-autumn                    "#ff8c4e")

      (acer-cream                     "#dca56e")
      (acer-brown                     "#c88550")
      (acer-deep-brown                "#8c6950")

      (acer-light-blue                "#3a9187")
      (acer-blue                      "#2134d5")
      (acer-deep-blue                 "#1B3B4D")

      (acer-purple                    "#9e4d76")
      (acer-deep-purple               "#60366e")
      (acer-purple-red                "#9b234b")
      (acer-purple-pink               "#5f2258")

      (acer-fg                        "#000000")
      (acer-bg                        "#f7bb78")
      (acer-highlight                 "#e3a764")
      (acer-shadow                    "#717171")

      (acer-error                     "#d70000")
      (acer-warning                   "#f0dc00")
      (acer-success                   "#28823c")

      (acer-mode-line                 "#e76144")
      (acer-mode-line-inactive        "#ff8c4e")
      (acer-bell                      "#4478E7")

      (acer-tab-1                     acer-mode-line)
      (acer-tab-2                     acer-mode-line-inactive)
      (acer-tab-3                     acer-cream)

      (acer-fl-comment                acer-deep-green)
      (acer-fl-string                 acer-deep-orange)
      (acer-fl-keyword                acer-deep-purple)
      (acer-fl-builtin                acer-purple)
      (acer-fl-type                   acer-green)
      (acer-fl-function-name          acer-purple-red)
      (acer-fl-variable-name          acer-orange-pink)
      (acer-fl-constant               acer-green-cyan)
      (acer-fl-warning                acer-warning)
      (acer-fl-punctuation            acer-purple-pink)
      (acer-fl-negation-char          acer-light-blue)

      (acer-diff-added                "#c8f0c8")
      (acer-diff-removed              "#f0c8c8")
      (acer-diff-refine-added         "#78f078")
      (acer-diff-refine-removed       "#f07878")
      (acer-diff-header               "#b4b4b4")
      (acer-diff-file-header          "#8c8c8c")
      (acer-diff-context              "#dcdcdc")
      (acer-smerge-base               "#C8C8F0")

      (acer-orderless-0               "#af2ab9")
      (acer-orderless-1               "#28823c")
      (acer-orderless-2               "#d44400")
      (acer-orderless-3               "#6496d7")

      (acer-prescient-0               "#4680DE")
      (acer-prescient-1               "#DE4680")

      (acer-vc-change                 acer-blue)
      (acer-vc-insert                 acer-success)
      (acer-vc-delete                 acer-error))

  (custom-theme-set-faces
   'guava-themes-acer

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,acer-class (:foreground ,acer-fg :background ,acer-bg))))

   ;; error, warning, success
   `(error ((,acer-class (:foreground ,acer-error :weight bold))))
   `(warning ((,acer-class (:foreground ,acer-warning :weight bold))))
   `(success ((,acer-class (:foreground ,acer-success :weight bold))))

   ;; highlight
   `(highlight ((,acer-class (:background ,acer-highlight))))

   ;; shadow
   `(shadow ((,acer-class (:foreground ,acer-shadow))))

   ;; region
   `(region ((,acer-class (:background ,acer-brown :extend t))))
   `(secondary-selection ((,acer-class (:background ,acer-orange :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,acer-class (:foreground ,acer-fl-comment :weight medium))))
   `(font-lock-string-face ((,acer-class (:foreground ,acer-fl-string :weight medium))))
   `(font-lock-keyword-face ((,acer-class (:foreground ,acer-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,acer-class (:foreground ,acer-fl-builtin :weight medium))))
   `(font-lock-type-face ((,acer-class (:foreground ,acer-fl-type :weight medium))))
   `(font-lock-function-name-face ((,acer-class (:foreground ,acer-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,acer-class (:foreground ,acer-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,acer-class (:foreground ,acer-fl-constant :weight medium))))
   `(font-lock-warning-face ((,acer-class (:foreground ,acer-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,acer-class (:foreground ,acer-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,acer-class (:foreground ,acer-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,acer-class (:foreground ,acer-black :background ,acer-mode-line))))

   ;; fringe
   `(fringe ((,acer-class (:foreground ,acer-deep-blue :background ,acer-bg))))
   `(diff-hl-change ((,acer-class (:foreground ,acer-vc-change :background ,acer-vc-change))))
   `(diff-hl-insert ((,acer-class (:foreground ,acer-vc-insert :background ,acer-vc-insert))))
   `(diff-hl-delete ((,acer-class (:foreground ,acer-vc-delete :background ,acer-vc-delete))))

   ;; line-number
   `(line-number ((,acer-class (:foreground ,acer-fg :inherit default))))
   `(line-number-current-line ((,acer-class (:foreground ,acer-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,acer-class (:background ,acer-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,acer-class (:background ,acer-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,acer-class (:foreground ,acer-white :background ,acer-mode-line))))
   `(mode-line-inactive ((,acer-class (:foreground ,acer-white :background ,acer-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,acer-class (:foreground ,acer-white :background ,acer-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,acer-class (:foreground ,acer-black))))

   ;; borders
   `(vertical-border ((,acer-class (:foreground ,acer-mode-line))))

   ;; header-line
   `(header-line ((,acer-class (:inherit mode-line))))
   `(which-func ((,acer-class (:foreground ,acer-white))))

   ;; tab-bar
   `(tab-bar ((,acer-class (:foreground ,acer-white :background ,acer-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,acer-class (:foreground ,acer-white :background ,acer-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,acer-class (:foreground ,acer-white :background ,acer-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,acer-class (:foreground ,acer-white :background ,acer-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,acer-class (:foreground ,acer-white :background ,acer-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,acer-class (:foreground ,acer-white :background ,acer-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,acer-class (:foreground ,acer-white :background ,acer-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,acer-class (:foreground ,acer-white :background ,acer-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,acer-class (:foreground ,acer-green-cyan :weight bold :height 0.9))))
   `(tab-line-tab-special ((,acer-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,acer-class (:foreground ,acer-black :background ,acer-bell))))
   `(show-paren-mismatch ((,acer-class (:foreground ,acer-black :background ,acer-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,acer-class (:background ,acer-error))))

   ;; links
   `(link ((,acer-class (:foreground ,acer-green-cyan :underline t :weight bold))))
   `(link-visited ((,acer-class (:foreground ,acer-blue :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,acer-class (:foreground ,acer-purple-red :weight medium))))
   `(outline-2 ((,acer-class (:foreground ,acer-deep-green :weight medium))))
   `(outline-3 ((,acer-class (:foreground ,acer-purple :weight medium))))
   `(outline-4 ((,acer-class (:foreground ,acer-deep-blue :weight medium))))
   `(outline-5 ((,acer-class (:inherit outline-1))))
   `(outline-6 ((,acer-class (:inherit outline-2))))
   `(outline-7 ((,acer-class (:inherit outline-3))))
   `(outline-8 ((,acer-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,acer-class (:foreground ,acer-blue))))
   `(escape-glyph ((,acer-class (:inherit homoglyph))))
   `(nobreak-space ((,acer-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,acer-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,acer-class (:background ,acer-light-blue))))

   ;; help-key-binding
   `(help-key-binding ((,acer-class (:foreground ,acer-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,acer-class (:foreground ,acer-black :background ,acer-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,acer-class (:foreground ,acer-black :background ,acer-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,acer-class (:foreground ,acer-black :background ,acer-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,acer-class (:foreground ,acer-black :background ,acer-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,acer-class (:foreground ,acer-black :background ,acer-diff-header :extend t))))
   `(diff-file-header ((,acer-class (:weight bold :foreground ,acer-black :background ,acer-diff-file-header :extend t))))
   `(diff-context ((,acer-class (:foreground ,acer-black :background ,acer-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,acer-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,acer-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,acer-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,acer-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,acer-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,acer-class (:foreground ,acer-black :background ,acer-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,acer-class (:foreground ,acer-vc-change :weight bold))))
   `(completions-first-difference ((,acer-class (:foreground ,acer-error :weight bold))))

   ;; org-faces
   `(org-todo ((,acer-class (:foreground ,acer-vc-delete :weight bold))))
   `(org-done ((,acer-class (:foreground ,acer-vc-insert :weight bold))))
   `(org-hide ((,acer-class (:foreground ,acer-bg))))
   `(org-table ((,acer-class (:foreground ,acer-deep-green))))
   `(org-date ((,acer-class (:foreground ,acer-light-blue))))
   `(org-date-selected ((,acer-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,acer-class (:foreground ,acer-orderless-0))))
   `(org-headline-done ((,acer-class (:foreground ,acer-orderless-3))))
   `(org-document-title ((,acer-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,acer-class (:inherit shadow))))
   `(org-meta-line ((,acer-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,acer-class (:foreground ,acer-tab-3))))
   `(window-divider-first-pixel ((,acer-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,acer-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,acer-class (:foreground ,acer-white :background ,acer-orderless-2))))
   `(isearch-fail ((,acer-class (:foreground ,acer-white :background ,acer-error))))
   `(lazy-highlight ((,acer-class (:foreground ,acer-white :background ,acer-orderless-3))))
   `(isearch-group-1 ((,acer-class (:foreground ,acer-white :background ,acer-orderless-0))))
   `(isearch-group-2 ((,acer-class (:foreground ,acer-white :background ,acer-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,acer-class (:inherit isearch))))
   `(match ((,acer-class (:inherit lazy-highlight))))

   ;; custom-button
   `(custom-button ((,acer-class (:foreground ,acer-white :background ,acer-mode-line :box (:line-width 2 :style released-button)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,acer-class (:foreground ,acer-light-blue))))
   `(elfeed-search-date-face ((,acer-class (:foreground ,acer-deep-orange))))
   `(elfeed-search-feed-face ((,acer-class (:foreground ,acer-deep-purple))))
   `(elfeed-search-title-face ((,acer-class (:foreground ,acer-deep-brown))))
   `(elfeed-search-unread-title-face ((,acer-class (:weight bold :foreground ,acer-deep-green))))
   `(elfeed-search-filter-face ((,acer-class (:weight bold :foreground ,acer-purple-pink))))
   `(elfeed-search-last-update-face ((,acer-class (:weight bold :foreground ,acer-purple-pink))))
   `(elfeed-search-unread-count-face ((,acer-class (:weight bold :foreground ,acer-purple-pink))))

   `(elfeed-show-header-face ((,acer-class (:foreground ,acer-deep-green))))
   `(elfeed-show-author-face ((,acer-class (:weight bold :foreground ,acer-purple))))
   `(elfeed-show-title-face ((,acer-class (:weight bold :foreground ,acer-purple))))
   `(elfeed-show-date-face ((,acer-class (:foreground ,acer-light-blue))))
   `(elfeed-show-feed-face ((,acer-class (:foreground ,acer-light-blue))))
   `(elfeed-show-tags-face ((,acer-class (:foreground ,acer-orange-pink))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,acer-class (:foreground ,acer-deep-purple :inherit italic))))
   `(doom-modeline-project-parent-dir ((,acer-class (:foreground ,acer-deep-purple))))
   `(doom-modeline-buffer-minor-mode ((,acer-class (:foreground ,acer-purple))))

   ;; corfu
   `(corfu-default ((,acer-class (:foreground ,acer-fg :background ,acer-bg))))
   `(corfu-current ((,acer-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,acer-class (:background ,acer-shadow))))
   `(corfu-border ((,acer-class (:background ,acer-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,acer-class (:foreground ,acer-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,acer-class (:foreground ,acer-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,acer-class (:foreground ,acer-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,acer-class (:foreground ,acer-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,acer-class (:foreground ,acer-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,acer-class (:foreground ,acer-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,acer-class (:inherit error))))
   `(envrc-mode-line-none-face ((,acer-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,acer-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,acer-class (:weight bold :background ,acer-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,acer-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-acer)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-acer-theme.el ends here
