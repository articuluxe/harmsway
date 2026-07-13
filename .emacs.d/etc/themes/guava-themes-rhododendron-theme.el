;;; guava-themes-rhododendron-theme.el --- A theme inspired by the azalea tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Jan 19, 2026
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
;; A theme inspired by the azalea tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-rhododendron "A theme inspired by the azalea tree colors.")

(let* (
      (rhododendron-class '((class color) (min-colors 257)))
      (rhododendron-black                     "#000000")
      (rhododendron-white                     "#FFFFFF")

      (rhododendron-light-green               "#64d264")
      (rhododendron-forest-green              "#228b22")
      (rhododendron-deep-green                "#3e7411")

      (rhododendron-red                       "#c00353")
      (rhododendron-red-orange                "#cd605f")
      (rhododendron-light-orange              "#ff967c")
      (rhododendron-bright-orange             "#ff5b4c")
      (rhododendron-cream                     "#fcbdb2")

      (rhododendron-blue                      "#3c3cee")
      (rhododendron-deep-blue                 "#3e3d8b")

      (rhododendron-bright-pink               "#fd3aae")
      (rhododendron-alt-bright-pink           "#e78deb")
      (rhododendron-deep-pink                 "#c00e88")

      (rhododendron-light-purple              "#e0bde7")
      (rhododendron-purple                    "#a08ce8")
      (rhododendron-purple-pink               "#ad20f0")
      (rhododendron-purple-blue               "#5346cc")
      (rhododendron-purple-red                "#a8206f")
      (rhododendron-dark-purple-red           "#7d2061")

      (rhododendron-fg                        "#000000")
      (rhododendron-bg                        "#e8c7e3")
      (rhododendron-highlight                 "#d4b3cf")
      (rhododendron-shadow                    "#898989")

      (rhododendron-error                     "#FF0000")
      (rhododendron-warning                   "#F68511")
      (rhododendron-success                   "#29b425")

      (rhododendron-mode-line                 "#c00353")
      (rhododendron-mode-line-inactive        "#fd3aae")
      (rhododendron-bell                      "#03C013")

      (rhododendron-tab-1                     rhododendron-mode-line)
      (rhododendron-tab-2                     rhododendron-mode-line-inactive)
      (rhododendron-tab-3                     rhododendron-alt-bright-pink)

      (rhododendron-fl-comment                rhododendron-deep-green)
      (rhododendron-fl-string                 rhododendron-purple-red)
      (rhododendron-fl-keyword                rhododendron-bright-pink)
      (rhododendron-fl-builtin                rhododendron-deep-blue)
      (rhododendron-fl-type                   rhododendron-forest-green)
      (rhododendron-fl-function-name          rhododendron-deep-pink)
      (rhododendron-fl-variable-name          rhododendron-bright-orange)
      (rhododendron-fl-constant               rhododendron-purple-blue)
      (rhododendron-fl-warning                rhododendron-warning)
      (rhododendron-fl-punctuation            rhododendron-red)
      (rhododendron-fl-negation-char          rhododendron-red-orange)

      (rhododendron-diff-added                "#c8f0c8")
      (rhododendron-diff-removed              "#f0c8c8")
      (rhododendron-diff-refine-added         "#78f078")
      (rhododendron-diff-refine-removed       "#f07878")
      (rhododendron-diff-header               "#b4b4b4")
      (rhododendron-diff-file-header          "#8c8c8c")
      (rhododendron-diff-context              "#dcdcdc")
      (rhododendron-smerge-base               "#C8C8F0")

      (rhododendron-orderless-0               "#af50c8")
      (rhododendron-orderless-1               "#28a03c")
      (rhododendron-orderless-2               "#ff6400")
      (rhododendron-orderless-3               "#3c82ff")

      (rhododendron-prescient-0               "#4680DE")
      (rhododendron-prescient-1               "#DE4680")

      (rhododendron-vc-change                 rhododendron-blue)
      (rhododendron-vc-insert                 rhododendron-success)
      (rhododendron-vc-delete                 rhododendron-error))

  (custom-theme-set-faces
   'guava-themes-rhododendron

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,rhododendron-class (:foreground ,rhododendron-fg :background ,rhododendron-bg))))

   ;; error, warning, success
   `(error ((,rhododendron-class (:foreground ,rhododendron-error :weight bold))))
   `(warning ((,rhododendron-class (:foreground ,rhododendron-warning :weight bold))))
   `(success ((,rhododendron-class (:foreground ,rhododendron-success :weight bold))))

   ;; highlight
   `(highlight ((,rhododendron-class (:background ,rhododendron-highlight))))

   ;; shadow
   `(shadow ((,rhododendron-class (:foreground ,rhododendron-shadow))))

   ;; region
   `(region ((,rhododendron-class (:background ,rhododendron-alt-bright-pink :extend t))))
   `(secondary-selection ((,rhododendron-class (:background ,rhododendron-light-green :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,rhododendron-class (:foreground ,rhododendron-fl-comment :weight medium))))
   `(font-lock-string-face ((,rhododendron-class (:foreground ,rhododendron-fl-string :weight medium))))
   `(font-lock-keyword-face ((,rhododendron-class (:foreground ,rhododendron-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,rhododendron-class (:foreground ,rhododendron-fl-builtin :weight medium))))
   `(font-lock-type-face ((,rhododendron-class (:foreground ,rhododendron-fl-type :weight medium))))
   `(font-lock-function-name-face ((,rhododendron-class (:foreground ,rhododendron-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,rhododendron-class (:foreground ,rhododendron-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,rhododendron-class (:foreground ,rhododendron-fl-constant :weight medium))))
   `(font-lock-warning-face ((,rhododendron-class (:foreground ,rhododendron-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,rhododendron-class (:foreground ,rhododendron-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,rhododendron-class (:foreground ,rhododendron-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-mode-line))))

   ;; fringe
   `(fringe ((,rhododendron-class (:foreground ,rhododendron-red :background ,rhododendron-bg))))
   `(diff-hl-change ((,rhododendron-class (:foreground ,rhododendron-vc-change :background ,rhododendron-vc-change))))
   `(diff-hl-insert ((,rhododendron-class (:foreground ,rhododendron-vc-insert :background ,rhododendron-vc-insert))))
   `(diff-hl-delete ((,rhododendron-class (:foreground ,rhododendron-vc-delete :background ,rhododendron-vc-delete))))

   ;; line-number
   `(line-number ((,rhododendron-class (:foreground ,rhododendron-fg :inherit default))))
   `(line-number-current-line ((,rhododendron-class (:foreground ,rhododendron-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,rhododendron-class (:background ,rhododendron-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,rhododendron-class (:background ,rhododendron-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-mode-line))))
   `(mode-line-inactive ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,rhododendron-class (:foreground ,rhododendron-black))))

   ;; borders
   `(vertical-border ((,rhododendron-class (:foreground ,rhododendron-mode-line))))

   ;; header-line
   `(header-line ((,rhododendron-class (:inherit mode-line))))
   `(which-func ((,rhododendron-class (:foreground ,rhododendron-white))))

   ;; tab-bar
   `(tab-bar ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,rhododendron-class (:foreground ,rhododendron-purple-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,rhododendron-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-bell))))
   `(show-paren-mismatch ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,rhododendron-class (:background ,rhododendron-error))))

   ;; links
   `(link ((,rhododendron-class (:foreground ,rhododendron-purple-blue :underline t :weight bold))))
   `(link-visited ((,rhododendron-class (:foreground ,rhododendron-purple-pink :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,rhododendron-class (:foreground ,rhododendron-purple-red :weight medium))))
   `(outline-2 ((,rhododendron-class (:foreground ,rhododendron-deep-green :weight medium))))
   `(outline-3 ((,rhododendron-class (:foreground ,rhododendron-bright-orange :weight medium))))
   `(outline-4 ((,rhododendron-class (:foreground ,rhododendron-purple-blue :weight medium))))
   `(outline-5 ((,rhododendron-class (:inherit outline-1))))
   `(outline-6 ((,rhododendron-class (:inherit outline-2))))
   `(outline-7 ((,rhododendron-class (:inherit outline-3))))
   `(outline-8 ((,rhododendron-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,rhododendron-class (:foreground ,rhododendron-blue))))
   `(escape-glyph ((,rhododendron-class (:inherit homoglyph))))
   `(nobreak-space ((,rhododendron-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,rhododendron-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,rhododendron-class (:background ,rhododendron-blue))))

   ;; help-key-binding
   `(help-key-binding ((,rhododendron-class (:foreground ,rhododendron-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-header :extend t))))
   `(diff-file-header ((,rhododendron-class (:weight bold :foreground ,rhododendron-black :background ,rhododendron-diff-file-header :extend t))))
   `(diff-context ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,rhododendron-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,rhododendron-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,rhododendron-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,rhododendron-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,rhododendron-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,rhododendron-class (:foreground ,rhododendron-vc-change :weight bold))))
   `(completions-first-difference ((,rhododendron-class (:foreground ,rhododendron-error :weight bold))))

   ;; org-faces
   `(org-todo ((,rhododendron-class (:foreground ,rhododendron-vc-delete :weight bold))))
   `(org-done ((,rhododendron-class (:foreground ,rhododendron-vc-insert :weight bold))))
   `(org-hide ((,rhododendron-class (:foreground ,rhododendron-bg))))
   `(org-table ((,rhododendron-class (:foreground ,rhododendron-red))))
   `(org-date ((,rhododendron-class (:foreground ,rhododendron-red-orange))))
   `(org-date-selected ((,rhododendron-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,rhododendron-class (:foreground ,rhododendron-orderless-0))))
   `(org-headline-done ((,rhododendron-class (:foreground ,rhododendron-orderless-3))))
   `(org-document-title ((,rhododendron-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,rhododendron-class (:inherit shadow))))
   `(org-meta-line ((,rhododendron-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,rhododendron-class (:foreground ,rhododendron-tab-3))))
   `(window-divider-first-pixel ((,rhododendron-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,rhododendron-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-orderless-2))))
   `(isearch-fail ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-error))))
   `(lazy-highlight ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-orderless-3))))
   `(isearch-group-1 ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-orderless-0))))
   `(isearch-group-2 ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,rhododendron-class (:inherit isearch))))
   `(match ((,rhododendron-class (:inherit lazy-highlight))))

   ;; custom-button
   `(custom-button ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-mode-line :box (:line-width 2 :style released-button)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,rhododendron-class (:foreground ,rhododendron-bright-pink))))
   `(elfeed-search-date-face ((,rhododendron-class (:foreground ,rhododendron-forest-green))))
   `(elfeed-search-feed-face ((,rhododendron-class (:foreground ,rhododendron-purple-blue))))
   `(elfeed-search-title-face ((,rhododendron-class (:foreground ,rhododendron-deep-blue))))
   `(elfeed-search-unread-title-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-purple-red))))
   `(elfeed-search-filter-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-light-purple))))
   `(elfeed-search-last-update-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-light-purple))))
   `(elfeed-search-unread-count-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-light-purple))))

   `(elfeed-show-header-face ((,rhododendron-class (:foreground ,rhododendron-purple-red))))
   `(elfeed-show-author-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-deep-blue))))
   `(elfeed-show-title-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-deep-blue))))
   `(elfeed-show-date-face ((,rhododendron-class (:foreground ,rhododendron-forest-green))))
   `(elfeed-show-feed-face ((,rhododendron-class (:foreground ,rhododendron-forest-green))))
   `(elfeed-show-tags-face ((,rhododendron-class (:foreground ,rhododendron-purple-pink))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,rhododendron-class (:foreground ,rhododendron-light-purple :inherit italic))))
   `(doom-modeline-project-parent-dir ((,rhododendron-class (:foreground ,rhododendron-light-purple))))
   `(doom-modeline-buffer-minor-mode ((,rhododendron-class (:foreground ,rhododendron-bright-orange))))

   ;; corfu
   `(corfu-default ((,rhododendron-class (:foreground ,rhododendron-fg :background ,rhododendron-bg))))
   `(corfu-current ((,rhododendron-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,rhododendron-class (:background ,rhododendron-shadow))))
   `(corfu-border ((,rhododendron-class (:background ,rhododendron-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,rhododendron-class (:foreground ,rhododendron-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,rhododendron-class (:foreground ,rhododendron-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,rhododendron-class (:foreground ,rhododendron-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,rhododendron-class (:foreground ,rhododendron-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,rhododendron-class (:foreground ,rhododendron-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,rhododendron-class (:foreground ,rhododendron-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,rhododendron-class (:inherit error))))
   `(envrc-mode-line-none-face ((,rhododendron-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,rhododendron-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,rhododendron-class (:weight bold :background ,rhododendron-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,rhododendron-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-rhododendron)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-rhododendron-theme.el ends here
