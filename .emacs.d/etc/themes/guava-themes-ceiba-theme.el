;;; guava-themes-ceiba-theme.el --- A theme inspired by the ceiba tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 21, 2026
;; Version: 0.17.0
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
;; A theme inspired by the ceiba tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-ceiba "A theme inspired by the ceiba tree colors.")

(let* (
      (ceiba-class '((class color) (min-colors 257)))
      (ceiba-black                     "#000000")
      (ceiba-white                     "#FFFFFF")

      ;; (ceiba-light-gray                "#bab49e")
      (ceiba-gray-blue                 "#798585")

      (ceiba-light-green               "#61ff96")
      (ceiba-green                     "#5b6452")
      (ceiba-deep-green                "#2b5535")
      (ceiba-green-forest              "#006441")
      (ceiba-green-blue                "#11645a")

      (ceiba-light-orange              "#f1a147")
      (ceiba-orange                    "#ca6f39")
      (ceiba-deep-orange               "#a85639")

      (ceiba-steel-blue                "#aabed8")
      (ceiba-blue                      "#2327dc")
      (ceiba-alt-blue                  "#2268a7")

      (ceiba-light-purple              "#bec8ff")
      (ceiba-purple                    "#4e466d")
      (ceiba-purple-red                "#762362")

      (ceiba-light-brown               "#a08c6e")
      (ceiba-brown                     "#6d4b30")
      (ceiba-brown-sand                "#826e51")
      (ceiba-brown-wood                "#53453d")

      (ceiba-fg                        "#000000")
      (ceiba-bg                        "#bab49e")
      (ceiba-highlight                 "#a6a08a")
      (ceiba-shadow                    "#7f7f7f")

      (ceiba-error                     "#ff0000")
      (ceiba-warning                   "#f6c911")
      (ceiba-success                   "#1ea01e")

      (ceiba-mode-line                 "#5b6452")
      (ceiba-mode-line-inactive        "#826e51")

      (ceiba-tab-1                     ceiba-mode-line)
      (ceiba-tab-2                     ceiba-mode-line-inactive)
      (ceiba-tab-3                     ceiba-light-brown)

      (ceiba-fl-comment                ceiba-deep-green)
      (ceiba-fl-string                 ceiba-brown)
      (ceiba-fl-keyword                ceiba-purple)
      (ceiba-fl-builtin                ceiba-alt-blue)
      (ceiba-fl-type                   ceiba-green-blue)
      (ceiba-fl-function-name          ceiba-brown-wood)
      (ceiba-fl-variable-name          ceiba-green)
      (ceiba-fl-constant               ceiba-deep-orange)
      (ceiba-fl-warning                ceiba-warning)
      (ceiba-fl-punctuation            ceiba-green-forest)
      (ceiba-fl-negation-char          ceiba-orange)

      (ceiba-diff-added                "#c8f0c8")
      (ceiba-diff-removed              "#f0c8c8")
      (ceiba-diff-refine-added         "#78f078")
      (ceiba-diff-refine-removed       "#f07878")
      (ceiba-diff-header               "#b4b4b4")
      (ceiba-diff-file-header          "#8c8c8c")
      (ceiba-diff-context              "#dcdcdc")
      (ceiba-smerge-base               "#C8C8F0")

      (ceiba-orderless-0               "#af37b9")
      (ceiba-orderless-1               "#147828")
      (ceiba-orderless-2               "#b45a00")
      (ceiba-orderless-3               "#3c82e6")

      (ceiba-prescient-0               "#4680DE")
      (ceiba-prescient-1               "#DE4680")

      (ceiba-vc-change                 ceiba-blue)
      (ceiba-vc-insert                 ceiba-success)
      (ceiba-vc-delete                 ceiba-error))

  (custom-theme-set-faces
   'guava-themes-ceiba

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,ceiba-class (:foreground ,ceiba-fg :background ,ceiba-bg))))

   ;; error, warning, success
   `(error ((,ceiba-class (:foreground ,ceiba-error :weight bold))))
   `(warning ((,ceiba-class (:foreground ,ceiba-warning :weight bold))))
   `(success ((,ceiba-class (:foreground ,ceiba-success :weight bold))))

   ;; highlight
   `(highlight ((,ceiba-class (:background ,ceiba-highlight))))

   ;; shadow
   `(shadow ((,ceiba-class (:foreground ,ceiba-shadow))))

   ;; region
   `(region ((,ceiba-class (:background ,ceiba-gray-blue :extend t))))
   `(secondary-selection ((,ceiba-class (:background ,ceiba-brown-sand :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,ceiba-class (:foreground ,ceiba-fl-comment :weight medium))))
   `(font-lock-string-face ((,ceiba-class (:foreground ,ceiba-fl-string :weight medium))))
   `(font-lock-keyword-face ((,ceiba-class (:foreground ,ceiba-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,ceiba-class (:foreground ,ceiba-fl-builtin :weight medium))))
   `(font-lock-type-face ((,ceiba-class (:foreground ,ceiba-fl-type :weight medium))))
   `(font-lock-function-name-face ((,ceiba-class (:foreground ,ceiba-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,ceiba-class (:foreground ,ceiba-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,ceiba-class (:foreground ,ceiba-fl-constant :weight medium))))
   `(font-lock-warning-face ((,ceiba-class (:foreground ,ceiba-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,ceiba-class (:foreground ,ceiba-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,ceiba-class (:foreground ,ceiba-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,ceiba-class (:foreground ,ceiba-fg :background ,ceiba-green-forest))))

   ;; fringe
   `(fringe ((,ceiba-class (:foreground ,ceiba-blue :background ,ceiba-bg))))
   `(diff-hl-change ((,ceiba-class (:foreground ,ceiba-vc-change :background ,ceiba-vc-change))))
   `(diff-hl-insert ((,ceiba-class (:foreground ,ceiba-vc-insert :background ,ceiba-vc-insert))))
   `(diff-hl-delete ((,ceiba-class (:foreground ,ceiba-vc-delete :background ,ceiba-vc-delete))))

   ;; line-number
   `(line-number ((,ceiba-class (:foreground ,ceiba-fg :inherit default))))
   `(line-number-current-line ((,ceiba-class (:foreground ,ceiba-deep-green :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,ceiba-class (:background ,ceiba-light-brown :inherit line-number))))
   `(line-number-major-tick ((,ceiba-class (:background ,ceiba-brown-sand :inherit line-number))))

   ;; mode-line
   `(mode-line ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-mode-line))))
   `(mode-line-inactive ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-steel-blue))))

   ;; minibuffer
   `(minibuffer-prompt ((,ceiba-class (:foreground ,ceiba-black))))

   ;; borders
   `(vertical-border ((,ceiba-class (:foreground ,ceiba-mode-line))))

   ;; header-line
   `(header-line ((,ceiba-class (:inherit mode-line))))
   `(which-func ((,ceiba-class (:foreground ,ceiba-white))))

   ;; tab-bar
   `(tab-bar ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,ceiba-class (:foreground ,ceiba-purple-red :weight bold :height 0.9))))
   `(tab-line-tab-special ((,ceiba-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-alt-blue))))
   `(show-paren-mismatch ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,ceiba-class (:background ,ceiba-error))))

   ;; links
   `(link ((,ceiba-class (:foreground ,ceiba-purple :underline t :weight bold))))
   `(link-visited ((,ceiba-class (:foreground ,ceiba-purple-red :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,ceiba-class (:foreground ,ceiba-alt-blue :weight medium))))
   `(outline-2 ((,ceiba-class (:foreground ,ceiba-purple-red :weight medium))))
   `(outline-3 ((,ceiba-class (:foreground ,ceiba-green-blue :weight medium))))
   `(outline-4 ((,ceiba-class (:foreground ,ceiba-purple :weight medium))))
   `(outline-5 ((,ceiba-class (:inherit outline-1))))
   `(outline-6 ((,ceiba-class (:inherit outline-2))))
   `(outline-7 ((,ceiba-class (:inherit outline-3))))
   `(outline-8 ((,ceiba-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,ceiba-class (:foreground ,ceiba-blue))))
   `(escape-glyph ((,ceiba-class (:inherit homoglyph))))
   `(nobreak-space ((,ceiba-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,ceiba-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,ceiba-class (:background ,ceiba-alt-blue))))

   ;; help-key-binding
   `(help-key-binding ((,ceiba-class (:foreground ,ceiba-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-diff-header :extend t))))
   `(diff-file-header ((,ceiba-class (:weight bold :foreground ,ceiba-black :background ,ceiba-diff-file-header :extend t))))
   `(diff-context ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,ceiba-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,ceiba-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,ceiba-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,ceiba-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,ceiba-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,ceiba-class (:foreground ,ceiba-vc-change :weight bold))))
   `(completions-first-difference ((,ceiba-class (:foreground ,ceiba-error :weight bold))))

   ;; org-faces
   `(org-todo ((,ceiba-class (:foreground ,ceiba-vc-delete :weight bold))))
   `(org-done ((,ceiba-class (:foreground ,ceiba-vc-insert :weight bold))))
   `(org-hide ((,ceiba-class (:foreground ,ceiba-bg))))
   `(org-table ((,ceiba-class (:foreground ,ceiba-brown-wood))))
   `(org-date ((,ceiba-class (:foreground ,ceiba-orange))))
   `(org-date-selected ((,ceiba-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,ceiba-class (:foreground ,ceiba-orderless-2))))
   `(org-headline-done ((,ceiba-class (:foreground ,ceiba-orderless-1))))
   `(org-document-title ((,ceiba-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,ceiba-class (:inherit shadow))))
   `(org-meta-line ((,ceiba-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,ceiba-class (:foreground ,ceiba-mode-line-inactive))))
   `(window-divider-first-pixel ((,ceiba-class (:foreground ,ceiba-mode-line-inactive))))
   `(window-divider-last-pixel ((,ceiba-class (:foreground ,ceiba-mode-line-inactive))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-orderless-2))))
   `(isearch-fail ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-error))))
   `(lazy-highlight ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-orderless-3))))
   `(isearch-group-1 ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-orderless-0))))
   `(isearch-group-2 ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,ceiba-class (:inherit isearch))))
   `(match ((,ceiba-class (:inherit lazy-highlight))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,ceiba-class (:foreground ,ceiba-alt-blue))))
   `(elfeed-search-date-face ((,ceiba-class (:foreground ,ceiba-purple-red))))
   `(elfeed-search-feed-face ((,ceiba-class (:foreground ,ceiba-green-blue))))
   `(elfeed-search-title-face ((,ceiba-class (:foreground ,ceiba-brown))))
   `(elfeed-search-unread-title-face ((,ceiba-class (:weight bold :foreground ,ceiba-deep-green))))
   `(elfeed-search-filter-face ((,ceiba-class (:weight bold :foreground ,ceiba-light-orange))))
   `(elfeed-search-last-update-face ((,ceiba-class (:weight bold :foreground ,ceiba-light-orange))))
   `(elfeed-search-unread-count-face ((,ceiba-class (:weight bold :foreground ,ceiba-light-orange))))

   `(elfeed-show-header-face ((,ceiba-class (:foreground ,ceiba-green-forest))))
   `(elfeed-show-author-face ((,ceiba-class (:weight bold :foreground ,ceiba-purple-red))))
   `(elfeed-show-title-face ((,ceiba-class (:weight bold :foreground ,ceiba-purple-red))))
   `(elfeed-show-date-face ((,ceiba-class (:foreground ,ceiba-brown-sand))))
   `(elfeed-show-feed-face ((,ceiba-class (:foreground ,ceiba-brown-sand))))
   `(elfeed-show-tags-face ((,ceiba-class (:foreground ,ceiba-alt-blue))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,ceiba-class (:foreground ,ceiba-steel-blue :inherit italic))))
   `(doom-modeline-project-parent-dir ((,ceiba-class (:foreground ,ceiba-steel-blue))))
   `(doom-modeline-buffer-minor-mode ((,ceiba-class (:foreground ,ceiba-shadow))))

   ;; corfu
   `(corfu-default ((,ceiba-class (:foreground ,ceiba-fg :background ,ceiba-bg))))
   `(corfu-current ((,ceiba-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,ceiba-class (:background ,ceiba-shadow))))
   `(corfu-border ((,ceiba-class (:background ,ceiba-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,ceiba-class (:foreground ,ceiba-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,ceiba-class (:foreground ,ceiba-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,ceiba-class (:foreground ,ceiba-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,ceiba-class (:foreground ,ceiba-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,ceiba-class (:foreground ,ceiba-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,ceiba-class (:foreground ,ceiba-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,ceiba-class (:inherit error))))
   `(envrc-mode-line-none-face ((,ceiba-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,ceiba-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,ceiba-class (:weight bold :background ,ceiba-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,ceiba-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-ceiba)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-ceiba-theme.el ends here
