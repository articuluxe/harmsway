;;; guava-themes-pyrus-theme.el --- A theme inspired by pear colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Apr 16, 2026
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
;; A theme inspired by pear colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-pyrus "A theme inspired by pear colors.")

(let* (
      (pyrus-class '((class color) (min-colors 257)))
      (pyrus-black                     "#000000")
      (pyrus-white                     "#FFFFFF")

      (pyrus-yellow-green              "#e5e73b")
      (pyrus-alt-yellow-green          "#c8c887")

      (pyrus-light-green               "#97ad24")
      (pyrus-green                     "#228b22")
      (pyrus-green-subdued             "#316355")

      (pyrus-deep-orange               "#eb4b37")
      (pyrus-orange-subdued            "#c06747")
      (pyrus-red-pink                  "#ab3755")
      (pyrus-red                       "#dc2828")
      (pyrus-orange-pink               "#f38866")

      (pyrus-light-blue                "#4ba5e6")
      (pyrus-blue                      "#2a4ad9")
      (pyrus-deep-blue                 "#483d8b")
      (pyrus-cyan                      "#0080ff")

      (pyrus-light-purple              "#8787e1")
      (pyrus-purple                    "#874be1")
      (pyrus-deep-magenta              "#642864")

      (pyrus-fg                        "#780000")
      (pyrus-bg                        "#f9fefd")
      (pyrus-highlight                 "#e5eae9")
      (pyrus-shadow                    "#898989")

      (pyrus-error                     "#ff0000")
      (pyrus-warning                   "#ffd200")
      (pyrus-success                   "#228B22")

      (pyrus-mode-line                 "#295323")
      (pyrus-mode-line-inactive        "#228b22")
      (pyrus-bell                      "#532341")

      (pyrus-tab-1                     pyrus-mode-line)
      (pyrus-tab-2                     pyrus-mode-line-inactive)
      (pyrus-tab-3                     pyrus-green-subdued)

      (pyrus-fl-comment                pyrus-light-green)
      (pyrus-fl-string                 pyrus-deep-orange)
      (pyrus-fl-keyword                pyrus-green-subdued)
      (pyrus-fl-builtin                pyrus-deep-blue)
      (pyrus-fl-type                   pyrus-green)
      (pyrus-fl-function-name          pyrus-red-pink)
      (pyrus-fl-variable-name          pyrus-orange-pink)
      (pyrus-fl-constant               pyrus-light-purple)
      (pyrus-fl-warning                pyrus-warning)
      (pyrus-fl-punctuation            pyrus-deep-magenta)
      (pyrus-fl-negation-char          pyrus-orange-subdued)

      (pyrus-diff-added                "#c8f0c8")
      (pyrus-diff-removed              "#f0c8c8")
      (pyrus-diff-refine-added         "#78f078")
      (pyrus-diff-refine-removed       "#f07878")
      (pyrus-diff-header               "#b4b4b4")
      (pyrus-diff-file-header          "#8c8c8c")
      (pyrus-diff-context              "#dcdcdc")
      (pyrus-smerge-base               "#C8C8F0")

      (pyrus-orderless-0               "#c846e6")
      (pyrus-orderless-1               "#28a03c")
      (pyrus-orderless-2               "#ff6400")
      (pyrus-orderless-3               "#3c82ff")

      (pyrus-prescient-0               "#4680DE")
      (pyrus-prescient-1               "#DE4680")

      (pyrus-vc-change                 pyrus-blue)
      (pyrus-vc-insert                 pyrus-success)
      (pyrus-vc-delete                 pyrus-error))

  (custom-theme-set-faces
   'guava-themes-pyrus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,pyrus-class (:foreground ,pyrus-fg :background ,pyrus-bg))))

   ;; error, warning, success
   `(error ((,pyrus-class (:foreground ,pyrus-error :weight bold))))
   `(warning ((,pyrus-class (:foreground ,pyrus-warning :weight bold))))
   `(success ((,pyrus-class (:foreground ,pyrus-success :weight bold))))

   ;; highlight
   `(highlight ((,pyrus-class (:background ,pyrus-highlight))))

   ;; shadow
   `(shadow ((,pyrus-class (:foreground ,pyrus-shadow))))

   ;; region
   `(region ((,pyrus-class (:background ,pyrus-alt-yellow-green :extend t))))
   `(secondary-selection ((,pyrus-class (:background ,pyrus-yellow-green :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,pyrus-class (:foreground ,pyrus-fl-comment :weight medium))))
   `(font-lock-string-face ((,pyrus-class (:foreground ,pyrus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,pyrus-class (:foreground ,pyrus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,pyrus-class (:foreground ,pyrus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,pyrus-class (:foreground ,pyrus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,pyrus-class (:foreground ,pyrus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,pyrus-class (:foreground ,pyrus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,pyrus-class (:foreground ,pyrus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,pyrus-class (:foreground ,pyrus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,pyrus-class (:foreground ,pyrus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,pyrus-class (:foreground ,pyrus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-mode-line))))

   ;; fringe
   `(fringe ((,pyrus-class (:foreground ,pyrus-blue :background ,pyrus-bg))))
   `(diff-hl-change ((,pyrus-class (:foreground ,pyrus-vc-change :background ,pyrus-vc-change))))
   `(diff-hl-insert ((,pyrus-class (:foreground ,pyrus-vc-insert :background ,pyrus-vc-insert))))
   `(diff-hl-delete ((,pyrus-class (:foreground ,pyrus-vc-delete :background ,pyrus-vc-delete))))

   ;; line-number
   `(line-number ((,pyrus-class (:foreground ,pyrus-fg :inherit default))))
   `(line-number-current-line ((,pyrus-class (:foreground ,pyrus-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,pyrus-class (:background ,pyrus-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,pyrus-class (:background ,pyrus-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-mode-line))))
   `(mode-line-inactive ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,pyrus-class (:foreground ,pyrus-deep-orange))))

   ;; borders
   `(vertical-border ((,pyrus-class (:foreground ,pyrus-mode-line))))

   ;; header-line
   `(header-line ((,pyrus-class (:inherit mode-line))))
   `(which-func ((,pyrus-class (:foreground ,pyrus-white))))

   ;; tab-bar
   `(tab-bar ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,pyrus-class (:foreground ,pyrus-orange-pink :weight bold :height 0.9))))
   `(tab-line-tab-special ((,pyrus-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-bell))))
   `(show-paren-mismatch ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,pyrus-class (:background ,pyrus-error))))

   ;; links
   `(link ((,pyrus-class (:foreground ,pyrus-light-blue :underline t :weight bold))))
   `(link-visited ((,pyrus-class (:foreground ,pyrus-green-subdued :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,pyrus-class (:foreground ,pyrus-green :weight medium))))
   `(outline-2 ((,pyrus-class (:foreground ,pyrus-cyan :weight medium))))
   `(outline-3 ((,pyrus-class (:foreground ,pyrus-deep-orange :weight medium))))
   `(outline-4 ((,pyrus-class (:foreground ,pyrus-purple :weight medium))))
   `(outline-5 ((,pyrus-class (:inherit outline-1))))
   `(outline-6 ((,pyrus-class (:inherit outline-2))))
   `(outline-7 ((,pyrus-class (:inherit outline-3))))
   `(outline-8 ((,pyrus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,pyrus-class (:foreground ,pyrus-cyan))))
   `(escape-glyph ((,pyrus-class (:inherit homoglyph))))
   `(nobreak-space ((,pyrus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,pyrus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,pyrus-class (:background ,pyrus-light-blue))))

   ;; help-key-binding
   `(help-key-binding ((,pyrus-class (:foreground ,pyrus-cyan :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-header :extend t))))
   `(diff-file-header ((,pyrus-class (:weight bold :foreground ,pyrus-black :background ,pyrus-diff-file-header :extend t))))
   `(diff-context ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,pyrus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,pyrus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,pyrus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,pyrus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,pyrus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,pyrus-class (:foreground ,pyrus-vc-change :weight bold))))
   `(completions-first-difference ((,pyrus-class (:foreground ,pyrus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,pyrus-class (:foreground ,pyrus-vc-delete :weight bold))))
   `(org-done ((,pyrus-class (:foreground ,pyrus-vc-insert :weight bold))))
   `(org-hide ((,pyrus-class (:foreground ,pyrus-bg))))
   `(org-table ((,pyrus-class (:foreground ,pyrus-green))))
   `(org-date ((,pyrus-class (:foreground ,pyrus-orange-subdued))))
   `(org-date-selected ((,pyrus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,pyrus-class (:foreground ,pyrus-orderless-0))))
   `(org-headline-done ((,pyrus-class (:foreground ,pyrus-orderless-3))))
   `(org-document-title ((,pyrus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,pyrus-class (:inherit shadow))))
   `(org-meta-line ((,pyrus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,pyrus-class (:foreground ,pyrus-tab-3))))
   `(window-divider-first-pixel ((,pyrus-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,pyrus-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-orderless-2))))
   `(isearch-fail ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-error))))
   `(lazy-highlight ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-orderless-3))))
   `(isearch-group-1 ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-orderless-0))))
   `(isearch-group-2 ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,pyrus-class (:inherit isearch))))
   `(match ((,pyrus-class (:inherit lazy-highlight))))

   ;; custom-button
   `(custom-button ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-mode-line :box (:line-width 2 :style released-button)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,pyrus-class (:foreground ,pyrus-deep-orange))))
   `(elfeed-search-date-face ((,pyrus-class (:foreground ,pyrus-deep-blue))))
   `(elfeed-search-feed-face ((,pyrus-class (:foreground ,pyrus-green))))
   `(elfeed-search-title-face ((,pyrus-class (:foreground ,pyrus-green-subdued))))
   `(elfeed-search-unread-title-face ((,pyrus-class (:weight bold :foreground ,pyrus-cyan))))
   `(elfeed-search-filter-face ((,pyrus-class (:weight bold :foreground ,pyrus-light-blue))))
   `(elfeed-search-last-update-face ((,pyrus-class (:weight bold :foreground ,pyrus-light-blue))))
   `(elfeed-search-unread-count-face ((,pyrus-class (:weight bold :foreground ,pyrus-light-blue))))

   `(elfeed-show-header-face ((,pyrus-class (:foreground ,pyrus-green))))
   `(elfeed-show-author-face ((,pyrus-class (:weight bold :foreground ,pyrus-cyan))))
   `(elfeed-show-title-face ((,pyrus-class (:weight bold :foreground ,pyrus-cyan))))
   `(elfeed-show-date-face ((,pyrus-class (:foreground ,pyrus-deep-blue))))
   `(elfeed-show-feed-face ((,pyrus-class (:foreground ,pyrus-deep-blue))))
   `(elfeed-show-tags-face ((,pyrus-class (:foreground ,pyrus-deep-orange))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,pyrus-class (:foreground ,pyrus-light-green :inherit italic))))
   `(doom-modeline-project-parent-dir ((,pyrus-class (:foreground ,pyrus-light-green))))
   `(doom-modeline-buffer-minor-mode ((,pyrus-class (:foreground ,pyrus-orange-pink))))

   ;; corfu
   `(corfu-default ((,pyrus-class (:foreground ,pyrus-fg :background ,pyrus-bg))))
   `(corfu-current ((,pyrus-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,pyrus-class (:background ,pyrus-shadow))))
   `(corfu-border ((,pyrus-class (:background ,pyrus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,pyrus-class (:foreground ,pyrus-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,pyrus-class (:foreground ,pyrus-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,pyrus-class (:foreground ,pyrus-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,pyrus-class (:foreground ,pyrus-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,pyrus-class (:foreground ,pyrus-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,pyrus-class (:foreground ,pyrus-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,pyrus-class (:inherit error))))
   `(envrc-mode-line-none-face ((,pyrus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,pyrus-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,pyrus-class (:weight bold :background ,pyrus-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,pyrus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-pyrus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-pyrus-theme.el ends here
