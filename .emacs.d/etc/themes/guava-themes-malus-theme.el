;;; guava-themes-malus-theme.el --- A theme inspired by apple colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: May 15, 2026
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
;; A theme inspired by apple colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-malus "A theme inspired by apple colors.")

(let* (
      (malus-class '((class color) (min-colors 257)))
      ;; (malus-black                     "#000000")
      (malus-white                     "#FFFFFF")

      (malus-brown                     "#826e5a")

      (malus-yellow-subdued            "#d2c864")

      (malus-green-subdued             "#788250")
      (malus-alt-green-subdued         "#8c9632")
      (malus-green-granny              "#96dc50")
      (malus-green-forest              "#27ab50")

      (malus-red-sweetango             "#cd5a5f")
      (malus-alt-red-sweetango         "#7d5a5f")
      (malus-deep-red                  "#a03227")

      (malus-light-blue                "#50bebe")
      (malus-blue                      "#4548e3")
      (malus-blue-subdued              "#9cc4de")
      (malus-oceanic-blue              "#49835f")
      (malus-cyan                      "#00ffff")

      (malus-purple                    "#8468ed")

      (malus-fg                        "#fafbb7")
      (malus-bg                        "#1e0910")
      (malus-highlight                 "#321d24")
      (malus-shadow                    "#b3b3b3")

      (malus-error                     "#d70000")
      (malus-warning                   "#f6d911")
      (malus-success                   "#23d723")

      (malus-mode-line                 "#cd5a5f")
      (malus-mode-line-inactive        "#49835f")

      (malus-tab-1                     malus-mode-line)
      (malus-tab-2                     malus-mode-line-inactive)
      (malus-tab-3                     malus-deep-red)

      (malus-fl-comment                malus-green-granny)
      (malus-fl-string                 malus-red-sweetango)
      (malus-fl-keyword                malus-green-forest)
      (malus-fl-builtin                malus-light-blue)
      (malus-fl-type                   malus-blue-subdued)
      (malus-fl-function-name          malus-deep-red)
      (malus-fl-variable-name          malus-yellow-subdued)
      (malus-fl-constant               malus-purple)
      (malus-fl-warning                malus-warning)
      (malus-fl-punctuation            malus-brown)
      (malus-fl-negation-char          malus-oceanic-blue)

      (malus-diff-added                "#5aa05a")
      (malus-diff-removed              "#a05a5a")
      (malus-diff-refine-added         "#007800")
      (malus-diff-refine-removed       "#780000")
      (malus-diff-header               "#5a5a5a")
      (malus-diff-file-header          "#3c3c3c")
      (malus-diff-context              "#828282")
      (malus-smerge-base               "#5A5AA0")

      (malus-orderless-0               "#af50c8")
      (malus-orderless-1               "#28a03c")
      (malus-orderless-2               "#ff6400")
      (malus-orderless-3               "#3c82ff")

      (malus-prescient-0               "#46C8A5")
      (malus-prescient-1               "#C84669")

      (malus-vc-change                 malus-blue)
      (malus-vc-insert                 malus-success)
      (malus-vc-delete                 malus-error))

  (custom-theme-set-faces
   'guava-themes-malus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,malus-class (:foreground ,malus-fg :background ,malus-bg))))

   ;; error, warning, success
   `(error ((,malus-class (:foreground ,malus-error :weight bold))))
   `(warning ((,malus-class (:foreground ,malus-warning :weight bold))))
   `(success ((,malus-class (:foreground ,malus-success :weight bold))))

   ;; highlight
   `(highlight ((,malus-class (:background ,malus-highlight))))

   ;; shadow
   `(shadow ((,malus-class (:foreground ,malus-shadow))))

   ;; region
   `(region ((,malus-class (:background ,malus-green-subdued :extend t))))
   `(secondary-selection ((,malus-class (:background ,malus-alt-green-subdued :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,malus-class (:foreground ,malus-fl-comment :weight medium))))
   `(font-lock-string-face ((,malus-class (:foreground ,malus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,malus-class (:foreground ,malus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,malus-class (:foreground ,malus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,malus-class (:foreground ,malus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,malus-class (:foreground ,malus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,malus-class (:foreground ,malus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,malus-class (:foreground ,malus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,malus-class (:foreground ,malus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,malus-class (:foreground ,malus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,malus-class (:foreground ,malus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,malus-class (:foreground ,malus-white :background ,malus-red-sweetango))))

   ;; fringe
   `(fringe ((,malus-class (:foreground ,malus-cyan :background ,malus-bg))))
   `(diff-hl-change ((,malus-class (:foreground ,malus-vc-change :background ,malus-vc-change))))
   `(diff-hl-insert ((,malus-class (:foreground ,malus-vc-insert :background ,malus-vc-insert))))
   `(diff-hl-delete ((,malus-class (:foreground ,malus-vc-delete :background ,malus-vc-delete))))

   ;; line-number
   `(line-number ((,malus-class (:foreground ,malus-yellow-subdued :inherit default))))
   `(line-number-current-line ((,malus-class (:foreground ,malus-fg :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,malus-class (:background ,malus-red-sweetango :inherit line-number))))
   `(line-number-major-tick ((,malus-class (:background ,malus-deep-red :inherit line-number))))

   ;; mode-line
   `(mode-line ((,malus-class (:foreground ,malus-white :background ,malus-mode-line))))
   `(mode-line-inactive ((,malus-class (:foreground ,malus-white :background ,malus-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,malus-class (:foreground ,malus-white :background ,malus-blue-subdued))))

   ;; minibuffer
   `(minibuffer-prompt ((,malus-class (:foreground ,malus-red-sweetango))))

   ;; borders
   `(vertical-border ((,malus-class (:foreground ,malus-mode-line))))

   ;; header-line
   `(header-line ((,malus-class (:inherit mode-line))))
   `(which-func ((,malus-class (:foreground ,malus-white))))

   ;; tab-bar
   `(tab-bar ((,malus-class (:foreground ,malus-white :background ,malus-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,malus-class (:foreground ,malus-white :background ,malus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,malus-class (:foreground ,malus-white :background ,malus-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,malus-class (:foreground ,malus-white :background ,malus-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,malus-class (:foreground ,malus-white :background ,malus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,malus-class (:foreground ,malus-white :background ,malus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,malus-class (:foreground ,malus-white :background ,malus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,malus-class (:foreground ,malus-white :background ,malus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,malus-class (:foreground ,malus-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,malus-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,malus-class (:foreground ,malus-white :background ,malus-light-blue))))
   `(show-paren-mismatch ((,malus-class (:foreground ,malus-white :background ,malus-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,malus-class (:background ,malus-error))))

   ;; links
   `(link ((,malus-class (:foreground ,malus-light-blue :underline t :weight bold))))
   `(link-visited ((,malus-class (:foreground ,malus-oceanic-blue :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,malus-class (:foreground ,malus-deep-red :weight medium))))
   `(outline-2 ((,malus-class (:foreground ,malus-light-blue :weight medium))))
   `(outline-3 ((,malus-class (:foreground ,malus-yellow-subdued :weight medium))))
   `(outline-4 ((,malus-class (:foreground ,malus-green-forest :weight medium))))
   `(outline-5 ((,malus-class (:inherit outline-1))))
   `(outline-6 ((,malus-class (:inherit outline-2))))
   `(outline-7 ((,malus-class (:inherit outline-3))))
   `(outline-8 ((,malus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,malus-class (:foreground ,malus-cyan))))
   `(escape-glyph ((,malus-class (:inherit homoglyph))))
   `(nobreak-space ((,malus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,malus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,malus-class (:background ,malus-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,malus-class (:foreground ,malus-cyan :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,malus-class (:foreground ,malus-white :background ,malus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,malus-class (:foreground ,malus-white :background ,malus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,malus-class (:foreground ,malus-white :background ,malus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,malus-class (:foreground ,malus-white :background ,malus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,malus-class (:foreground ,malus-white :background ,malus-diff-header :extend t))))
   `(diff-file-header ((,malus-class (:weight bold :foreground ,malus-white :background ,malus-diff-file-header :extend t))))
   `(diff-context ((,malus-class (:foreground ,malus-white :background ,malus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,malus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,malus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,malus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,malus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,malus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,malus-class (:foreground ,malus-white :background ,malus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,malus-class (:foreground ,malus-warning :weight bold))))
   `(completions-first-difference ((,malus-class (:foreground ,malus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,malus-class (:foreground ,malus-vc-delete :weight bold))))
   `(org-done ((,malus-class (:foreground ,malus-vc-insert :weight bold))))
   `(org-hide ((,malus-class (:foreground ,malus-bg))))
   `(org-table ((,malus-class (:foreground ,malus-red-sweetango))))
   `(org-date ((,malus-class (:foreground ,malus-oceanic-blue))))
   `(org-date-selected ((,malus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,malus-class (:foreground ,malus-orderless-2))))
   `(org-headline-done ((,malus-class (:foreground ,malus-orderless-1))))
   `(org-document-title ((,malus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,malus-class (:inherit shadow))))
   `(org-meta-line ((,malus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,malus-class (:foreground ,malus-mode-line-inactive))))
   `(window-divider-first-pixel ((,malus-class (:foreground ,malus-mode-line-inactive))))
   `(window-divider-last-pixel ((,malus-class (:foreground ,malus-mode-line-inactive))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,malus-class (:foreground ,malus-white :background ,malus-orderless-2))))
   `(isearch-fail ((,malus-class (:foreground ,malus-white :background ,malus-error))))
   `(lazy-highlight ((,malus-class (:foreground ,malus-white :background ,malus-orderless-3))))
   `(isearch-group-1 ((,malus-class (:foreground ,malus-white :background ,malus-orderless-0))))
   `(isearch-group-2 ((,malus-class (:foreground ,malus-white :background ,malus-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,malus-class (:inherit isearch))))
   `(match ((,malus-class (:inherit lazy-highlight))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,malus-class (:foreground ,malus-yellow-subdued))))
   `(elfeed-search-date-face ((,malus-class (:foreground ,malus-green-granny))))
   `(elfeed-search-feed-face ((,malus-class (:foreground ,malus-light-blue))))
   `(elfeed-search-title-face ((,malus-class (:foreground ,malus-brown))))
   `(elfeed-search-unread-title-face ((,malus-class (:weight bold :foreground ,malus-deep-red))))
   `(elfeed-search-filter-face ((,malus-class (:weight bold :foreground ,malus-blue))))
   `(elfeed-search-last-update-face ((,malus-class (:weight bold :foreground ,malus-blue))))
   `(elfeed-search-unread-count-face ((,malus-class (:weight bold :foreground ,malus-blue))))

   `(elfeed-show-header-face ((,malus-class (:foreground ,malus-green-granny))))
   `(elfeed-show-author-face ((,malus-class (:weight bold :foreground ,malus-purple))))
   `(elfeed-show-title-face ((,malus-class (:weight bold :foreground ,malus-purple))))
   `(elfeed-show-date-face ((,malus-class (:foreground ,malus-red-sweetango))))
   `(elfeed-show-feed-face ((,malus-class (:foreground ,malus-red-sweetango))))
   `(elfeed-show-tags-face ((,malus-class (:foreground ,malus-yellow-subdued))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,malus-class (:foreground ,malus-green-granny :inherit italic))))
   `(doom-modeline-project-parent-dir ((,malus-class (:foreground ,malus-green-granny))))
   `(doom-modeline-buffer-minor-mode ((,malus-class (:foreground ,malus-shadow))))

   ;; corfu
   `(corfu-default ((,malus-class (:foreground ,malus-fg :background ,malus-bg))))
   `(corfu-current ((,malus-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,malus-class (:background ,malus-shadow))))
   `(corfu-border ((,malus-class (:background ,malus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,malus-class (:foreground ,malus-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,malus-class (:foreground ,malus-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,malus-class (:foreground ,malus-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,malus-class (:foreground ,malus-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,malus-class (:foreground ,malus-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,malus-class (:foreground ,malus-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,malus-class (:inherit error))))
   `(envrc-mode-line-none-face ((,malus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,malus-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,malus-class (:weight bold :background ,malus-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,malus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-malus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-malus-theme.el ends here
