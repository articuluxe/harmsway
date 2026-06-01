;;; guava-themes-rubus-theme.el --- A theme inspired by raspberry colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Mar 27, 2026
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
;; A theme inspired by raspberry, blackberry, and dewberry colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-rubus "A theme inspired by raspberry, blackberry, and dewberry colors.")

(let* (
      (rubus-class '((class color) (min-colors 257)))
      (rubus-black                     "#000000")
      (rubus-white                     "#FFFFFF")

      (rubus-raspberry                 "#aa3232")
      (rubus-alt-raspberry             "#6e504b")
      (rubus-deep-raspberry            "#663a43")
      (rubus-red                       "#c23f39")
      (rubus-pink-cream                "#cd7378")
      (rubus-alt-pink-cream            "#aa5055")
      (rubus-orange                    "#ffa07a")

      (rubus-yellow                    "#fde8b9")

      (rubus-light-green               "#4ed77f")
      (rubus-green                     "#1e8264")
      (rubus-green-forest              "#007841")
      (rubus-green-blue                "#5b8a8a")

      (rubus-light-blue                "#5096f1")
      (rubus-blue                      "#1455f1")
      (rubus-blue-subdued              "#1c81a1")
      (rubus-cyan                      "#00FFFF")

      (rubus-light-purple              "#beb4d7")
      (rubus-purple                    "#8264e6")
      (rubus-purple-pink               "#965087")

      (rubus-fg                        "#FFFFFF")
      (rubus-bg                        "#0e1216")
      (rubus-highlight                 "#22262a")
      (rubus-shadow                    "#b3b3b3")

      (rubus-error                     "#ff1e00")
      (rubus-warning                   "#f6d909")
      (rubus-success                   "#1ebe1e")

      (rubus-mode-line                 "#aa3232")
      (rubus-mode-line-inactive        "#cd7378")

      (rubus-tab-1                     rubus-mode-line)
      (rubus-tab-2                     rubus-mode-line-inactive)
      (rubus-tab-3                     rubus-alt-pink-cream)

      (rubus-fl-comment                rubus-green-forest)
      (rubus-fl-string                 rubus-orange)
      (rubus-fl-keyword                rubus-red)
      (rubus-fl-builtin                rubus-pink-cream)
      (rubus-fl-type                   rubus-blue-subdued)
      (rubus-fl-function-name          rubus-purple-pink)
      (rubus-fl-variable-name          rubus-yellow)
      (rubus-fl-constant               rubus-purple)
      (rubus-fl-warning                rubus-warning)
      (rubus-fl-punctuation            rubus-light-purple)
      (rubus-fl-negation-char          rubus-green-blue)

      (rubus-diff-added                "#5aa05a")
      (rubus-diff-removed              "#a05a5a")
      (rubus-diff-refine-added         "#007800")
      (rubus-diff-refine-removed       "#780000")
      (rubus-diff-header               "#5a5a5a")
      (rubus-diff-file-header          "#3c3c3c")
      (rubus-diff-context              "#828282")
      (rubus-smerge-base               "#5A5AA0")

      (rubus-orderless-0               "#af50c8")
      (rubus-orderless-1               "#28a03c")
      (rubus-orderless-2               "#ff6400")
      (rubus-orderless-3               "#3c82ff")

      (rubus-prescient-0               "#46C8A5")
      (rubus-prescient-1               "#C84669")

      (rubus-vc-change                 rubus-blue)
      (rubus-vc-insert                 rubus-success)
      (rubus-vc-delete                 rubus-error))

  (custom-theme-set-faces
   'guava-themes-rubus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,rubus-class (:foreground ,rubus-fg :background ,rubus-bg))))

   ;; error, warning, success
   `(error ((,rubus-class (:foreground ,rubus-error :weight bold))))
   `(warning ((,rubus-class (:foreground ,rubus-warning :weight bold))))
   `(success ((,rubus-class (:foreground ,rubus-success :weight bold))))

   ;; highlight
   `(highlight ((,rubus-class (:background ,rubus-highlight))))

   ;; shadow
   `(shadow ((,rubus-class (:foreground ,rubus-shadow))))

   ;; region
   `(region ((,rubus-class (:background ,rubus-deep-raspberry :extend t))))
   `(secondary-selection ((,rubus-class (:background ,rubus-alt-raspberry :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,rubus-class (:foreground ,rubus-fl-comment :weight medium))))
   `(font-lock-string-face ((,rubus-class (:foreground ,rubus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,rubus-class (:foreground ,rubus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,rubus-class (:foreground ,rubus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,rubus-class (:foreground ,rubus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,rubus-class (:foreground ,rubus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,rubus-class (:foreground ,rubus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,rubus-class (:foreground ,rubus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,rubus-class (:foreground ,rubus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,rubus-class (:foreground ,rubus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,rubus-class (:foreground ,rubus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,rubus-class (:foreground ,rubus-black :background ,rubus-raspberry))))

   ;; fringe
   `(fringe ((,rubus-class (:foreground ,rubus-yellow :background ,rubus-bg))))
   `(diff-hl-change ((,rubus-class (:foreground ,rubus-vc-change :background ,rubus-vc-change))))
   `(diff-hl-insert ((,rubus-class (:foreground ,rubus-vc-insert :background ,rubus-vc-insert))))
   `(diff-hl-delete ((,rubus-class (:foreground ,rubus-vc-delete :background ,rubus-vc-delete))))

   ;; line-number
   `(line-number ((,rubus-class (:foreground ,rubus-fg :inherit default))))
   `(line-number-current-line ((,rubus-class (:foreground ,rubus-pink-cream :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,rubus-class (:background ,rubus-purple-pink :inherit line-number))))
   `(line-number-major-tick ((,rubus-class (:background ,rubus-purple :inherit line-number))))

   ;; mode-line
   `(mode-line ((,rubus-class (:foreground ,rubus-white :background ,rubus-mode-line))))
   `(mode-line-inactive ((,rubus-class (:foreground ,rubus-white :background ,rubus-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,rubus-class (:foreground ,rubus-white :background ,rubus-purple-pink))))

   ;; minibuffer
   `(minibuffer-prompt ((,rubus-class (:foreground ,rubus-raspberry))))

   ;; borders
   `(vertical-border ((,rubus-class (:foreground ,rubus-mode-line))))

   ;; header-line
   `(header-line ((,rubus-class (:inherit mode-line))))
   `(which-func ((,rubus-class (:foreground ,rubus-white))))

   ;; tab-bar
   `(tab-bar ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,rubus-class (:foreground ,rubus-purple :weight bold :height 0.9))))
   `(tab-line-tab-special ((,rubus-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,rubus-class (:foreground ,rubus-black :background ,rubus-orange))))
   `(show-paren-mismatch ((,rubus-class (:foreground ,rubus-white :background ,rubus-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,rubus-class (:background ,rubus-error))))

   ;; links
   `(link ((,rubus-class (:foreground ,rubus-light-green :underline t :weight bold))))
   `(link-visited ((,rubus-class (:foreground ,rubus-green :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,rubus-class (:foreground ,rubus-pink-cream :weight medium))))
   `(outline-2 ((,rubus-class (:foreground ,rubus-blue-subdued :weight medium))))
   `(outline-3 ((,rubus-class (:foreground ,rubus-green-forest :weight medium))))
   `(outline-4 ((,rubus-class (:foreground ,rubus-purple :weight medium))))
   `(outline-5 ((,rubus-class (:inherit outline-1))))
   `(outline-6 ((,rubus-class (:inherit outline-2))))
   `(outline-7 ((,rubus-class (:inherit outline-3))))
   `(outline-8 ((,rubus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,rubus-class (:foreground ,rubus-cyan))))
   `(escape-glyph ((,rubus-class (:inherit homoglyph))))
   `(nobreak-space ((,rubus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,rubus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,rubus-class (:background ,rubus-light-blue))))

   ;; help-key-binding
   `(help-key-binding ((,rubus-class (:foreground ,rubus-cyan :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-header :extend t))))
   `(diff-file-header ((,rubus-class (:weight bold :foreground ,rubus-white :background ,rubus-diff-file-header :extend t))))
   `(diff-context ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,rubus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,rubus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,rubus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,rubus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,rubus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,rubus-class (:foreground ,rubus-white :background ,rubus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,rubus-class (:foreground ,rubus-warning :weight bold))))
   `(completions-first-difference ((,rubus-class (:foreground ,rubus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,rubus-class (:foreground ,rubus-vc-delete :weight bold))))
   `(org-done ((,rubus-class (:foreground ,rubus-vc-insert :weight bold))))
   `(org-hide ((,rubus-class (:foreground ,rubus-bg))))
   `(org-table ((,rubus-class (:foreground ,rubus-pink-cream))))
   `(org-date ((,rubus-class (:foreground ,rubus-green-blue))))
   `(org-date-selected ((,rubus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,rubus-class (:foreground ,rubus-orderless-2))))
   `(org-headline-done ((,rubus-class (:foreground ,rubus-orderless-1))))
   `(org-document-title ((,rubus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,rubus-class (:inherit shadow))))
   `(org-meta-line ((,rubus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,rubus-class (:foreground ,rubus-mode-line-inactive))))
   `(window-divider-first-pixel ((,rubus-class (:foreground ,rubus-mode-line-inactive))))
   `(window-divider-last-pixel ((,rubus-class (:foreground ,rubus-mode-line-inactive))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,rubus-class (:foreground ,rubus-white :background ,rubus-orderless-2))))
   `(isearch-fail ((,rubus-class (:foreground ,rubus-white :background ,rubus-error))))
   `(lazy-highlight ((,rubus-class (:foreground ,rubus-white :background ,rubus-orderless-3))))
   `(isearch-group-1 ((,rubus-class (:foreground ,rubus-white :background ,rubus-orderless-0))))
   `(isearch-group-2 ((,rubus-class (:foreground ,rubus-white :background ,rubus-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,rubus-class (:inherit isearch))))
   `(match ((,rubus-class (:inherit lazy-highlight))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,rubus-class (:foreground ,rubus-green-blue))))
   `(elfeed-search-date-face ((,rubus-class (:foreground ,rubus-pink-cream))))
   `(elfeed-search-feed-face ((,rubus-class (:foreground ,rubus-raspberry))))
   `(elfeed-search-title-face ((,rubus-class (:foreground ,rubus-orange))))
   `(elfeed-search-unread-title-face ((,rubus-class (:weight bold :foreground ,rubus-green-forest))))
   `(elfeed-search-filter-face ((,rubus-class (:weight bold :foreground ,rubus-yellow))))
   `(elfeed-search-last-update-face ((,rubus-class (:weight bold :foreground ,rubus-yellow))))
   `(elfeed-search-unread-count-face ((,rubus-class (:weight bold :foreground ,rubus-yellow))))

   `(elfeed-show-header-face ((,rubus-class (:foreground ,rubus-purple))))
   `(elfeed-show-author-face ((,rubus-class (:weight bold :foreground ,rubus-raspberry))))
   `(elfeed-show-title-face ((,rubus-class (:weight bold :foreground ,rubus-raspberry))))
   `(elfeed-show-date-face ((,rubus-class (:foreground ,rubus-green-blue))))
   `(elfeed-show-feed-face ((,rubus-class (:foreground ,rubus-green-blue))))
   `(elfeed-show-tags-face ((,rubus-class (:foreground ,rubus-orange))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,rubus-class (:foreground ,rubus-light-purple :inherit italic))))
   `(doom-modeline-project-parent-dir ((,rubus-class (:foreground ,rubus-light-purple))))
   `(doom-modeline-buffer-minor-mode ((,rubus-class (:foreground ,rubus-orange))))

   ;; corfu
   `(corfu-default ((,rubus-class (:foreground ,rubus-fg :background ,rubus-bg))))
   `(corfu-current ((,rubus-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,rubus-class (:background ,rubus-shadow))))
   `(corfu-border ((,rubus-class (:background ,rubus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,rubus-class (:foreground ,rubus-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,rubus-class (:foreground ,rubus-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,rubus-class (:foreground ,rubus-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,rubus-class (:foreground ,rubus-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,rubus-class (:foreground ,rubus-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,rubus-class (:foreground ,rubus-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,rubus-class (:inherit error))))
   `(envrc-mode-line-none-face ((,rubus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,rubus-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,rubus-class (:weight bold :background ,rubus-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,rubus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-rubus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-rubus-theme.el ends here
