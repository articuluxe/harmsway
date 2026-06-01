;;; guava-themes-solanum-theme.el --- A theme inspired by eggplant colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Feb 22, 2026
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
;; A theme inspired by eggplant, potato, and tomato colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-solanum "A theme inspired by eggplant, potato, and tomato colors.")

(let* (
      (solanum-class '((class color) (min-colors 257)))
      (solanum-black                     "#000000")
      (solanum-white                     "#FFFFFF")

      (solanum-red-tomato                "#cd151f")
      (solanum-orange                    "#e98c85")

      (solanum-yellow-potato             "#fde8b9")

      (solanum-light-green               "#4ec9b0")
      (solanum-alt-light-green           "#61ff96")
      (solanum-green                     "#3a6b43")
      (solanum-deep-green                "#007d37")

      (solanum-light-blue                "#8ec4de")
      (solanum-blue                      "#0d62b2")
      (solanum-cyan                      "#00ffff")

      (solanum-light-purple              "#bec8ff")
      (solanum-purple                    "#9c69e8")
      (solanum-alt-purple                "#a394ff")
      (solanum-purple-pink               "#915d93")
      (solanum-purple-blue               "#492b91")
      (solanum-dark-purple               "#672b5f")
      (solanum-purple-black              "#130d1a")

      (solanum-fg                        "#FFFFFF")
      (solanum-bg                        "#130d1a")
      (solanum-highlight                 "#27212e")
      (solanum-shadow                    "#b3b3b3")

      (solanum-error                     "#FF0000")
      (solanum-warning                   "#f6d909")
      (solanum-success                   "#23a334")

      (solanum-mode-line                 "#672b5f")
      (solanum-mode-line-inactive        "#3b3550")

      (solanum-tab-1                     solanum-mode-line)
      (solanum-tab-2                     solanum-mode-line-inactive)
      (solanum-tab-3                     solanum-purple-black)

      (solanum-fl-comment                solanum-red-tomato)
      (solanum-fl-string                 solanum-yellow-potato)
      (solanum-fl-keyword                solanum-deep-green)
      (solanum-fl-builtin                solanum-light-blue)
      (solanum-fl-type                   solanum-orange)
      (solanum-fl-function-name          solanum-purple)
      (solanum-fl-variable-name          solanum-light-green)
      (solanum-fl-constant               solanum-light-purple)
      (solanum-fl-warning                solanum-warning)
      (solanum-fl-punctuation            solanum-purple-pink)
      (solanum-fl-negation-char          solanum-alt-light-green)

      (solanum-diff-added                "#5aa05a")
      (solanum-diff-removed              "#a05a5a")
      (solanum-diff-refine-added         "#007800")
      (solanum-diff-refine-removed       "#780000")
      (solanum-diff-header               "#5a5a5a")
      (solanum-diff-file-header          "#3c3c3c")
      (solanum-diff-context              "#828282")
      (solanum-smerge-base               "#5A5AA0")

      (solanum-orderless-0               "#af50c8")
      (solanum-orderless-1               "#28a03c")
      (solanum-orderless-2               "#ff6400")
      (solanum-orderless-3               "#3c82ff")

      (solanum-prescient-0               "#46C8A5")
      (solanum-prescient-1               "#C84669")

      (solanum-vc-change                 solanum-blue)
      (solanum-vc-insert                 solanum-success)
      (solanum-vc-delete                 solanum-error))

  (custom-theme-set-faces
   'guava-themes-solanum

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,solanum-class (:foreground ,solanum-fg :background ,solanum-bg))))

   ;; error, warning, success
   `(error ((,solanum-class (:foreground ,solanum-error :weight bold))))
   `(warning ((,solanum-class (:foreground ,solanum-warning :weight bold))))
   `(success ((,solanum-class (:foreground ,solanum-success :weight bold))))

   ;; highlight
   `(highlight ((,solanum-class (:background ,solanum-highlight))))

   ;; shadow
   `(shadow ((,solanum-class (:foreground ,solanum-shadow))))

   ;; region
   `(region ((,solanum-class (:background ,solanum-purple-blue :extend t))))
   `(secondary-selection ((,solanum-class (:background ,solanum-green :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,solanum-class (:foreground ,solanum-fl-comment :weight medium))))
   `(font-lock-string-face ((,solanum-class (:foreground ,solanum-fl-string :weight medium))))
   `(font-lock-keyword-face ((,solanum-class (:foreground ,solanum-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,solanum-class (:foreground ,solanum-fl-builtin :weight medium))))
   `(font-lock-type-face ((,solanum-class (:foreground ,solanum-fl-type :weight medium))))
   `(font-lock-function-name-face ((,solanum-class (:foreground ,solanum-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,solanum-class (:foreground ,solanum-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,solanum-class (:foreground ,solanum-fl-constant :weight medium))))
   `(font-lock-warning-face ((,solanum-class (:foreground ,solanum-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,solanum-class (:foreground ,solanum-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,solanum-class (:foreground ,solanum-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,solanum-class (:foreground ,solanum-black :background ,solanum-yellow-potato))))

   ;; fringe
   `(fringe ((,solanum-class (:foreground ,solanum-red-tomato :background ,solanum-bg))))
   `(diff-hl-change ((,solanum-class (:foreground ,solanum-vc-change :background ,solanum-vc-change))))
   `(diff-hl-insert ((,solanum-class (:foreground ,solanum-vc-insert :background ,solanum-vc-insert))))
   `(diff-hl-delete ((,solanum-class (:foreground ,solanum-vc-delete :background ,solanum-vc-delete))))

   ;; line-number
   `(line-number ((,solanum-class (:foreground ,solanum-fg :inherit default))))
   `(line-number-current-line ((,solanum-class (:foreground ,solanum-light-green :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,solanum-class (:background ,solanum-alt-purple :inherit line-number))))
   `(line-number-major-tick ((,solanum-class (:background ,solanum-purple-blue :inherit line-number))))

   ;; mode-line
   `(mode-line ((,solanum-class (:foreground ,solanum-white :background ,solanum-mode-line))))
   `(mode-line-inactive ((,solanum-class (:foreground ,solanum-white :background ,solanum-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,solanum-class (:foreground ,solanum-white :background ,solanum-orange))))

   ;; minibuffer
   `(minibuffer-prompt ((,solanum-class (:foreground ,solanum-yellow-potato))))

   ;; borders
   `(vertical-border ((,solanum-class (:foreground ,solanum-mode-line))))

   ;; header-line
   `(header-line ((,solanum-class (:inherit mode-line))))
   `(which-func ((,solanum-class (:foreground ,solanum-white))))

   ;; tab-bar
   `(tab-bar ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,solanum-class (:foreground ,solanum-red-tomato :weight bold :height 0.9))))
   `(tab-line-tab-special ((,solanum-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,solanum-class (:foreground ,solanum-black :background ,solanum-light-purple))))
   `(show-paren-mismatch ((,solanum-class (:foreground ,solanum-white :background ,solanum-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,solanum-class (:background ,solanum-error))))

   ;; links
   `(link ((,solanum-class (:foreground ,solanum-yellow-potato :underline t :weight bold))))
   `(link-visited ((,solanum-class (:foreground ,solanum-orange :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,solanum-class (:foreground ,solanum-purple :weight medium))))
   `(outline-2 ((,solanum-class (:foreground ,solanum-red-tomato :weight medium))))
   `(outline-3 ((,solanum-class (:foreground ,solanum-blue :weight medium))))
   `(outline-4 ((,solanum-class (:foreground ,solanum-deep-green :weight medium))))
   `(outline-5 ((,solanum-class (:inherit outline-1))))
   `(outline-6 ((,solanum-class (:inherit outline-2))))
   `(outline-7 ((,solanum-class (:inherit outline-3))))
   `(outline-8 ((,solanum-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,solanum-class (:foreground ,solanum-cyan))))
   `(escape-glyph ((,solanum-class (:inherit homoglyph))))
   `(nobreak-space ((,solanum-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,solanum-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,solanum-class (:background ,solanum-light-blue))))

   ;; help-key-binding
   `(help-key-binding ((,solanum-class (:foreground ,solanum-light-purple :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-header :extend t))))
   `(diff-file-header ((,solanum-class (:weight bold :foreground ,solanum-white :background ,solanum-diff-file-header :extend t))))
   `(diff-context ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,solanum-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,solanum-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,solanum-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,solanum-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,solanum-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,solanum-class (:foreground ,solanum-white :background ,solanum-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,solanum-class (:foreground ,solanum-warning :weight bold))))
   `(completions-first-difference ((,solanum-class (:foreground ,solanum-error :weight bold))))

   ;; org-faces
   `(org-todo ((,solanum-class (:foreground ,solanum-vc-delete :weight bold))))
   `(org-done ((,solanum-class (:foreground ,solanum-vc-insert :weight bold))))
   `(org-hide ((,solanum-class (:foreground ,solanum-bg))))
   `(org-table ((,solanum-class (:foreground ,solanum-yellow-potato))))
   `(org-date ((,solanum-class (:foreground ,solanum-alt-light-green))))
   `(org-date-selected ((,solanum-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,solanum-class (:foreground ,solanum-orderless-2))))
   `(org-headline-done ((,solanum-class (:foreground ,solanum-orderless-1))))
   `(org-document-title ((,solanum-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,solanum-class (:inherit shadow))))
   `(org-meta-line ((,solanum-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,solanum-class (:foreground ,solanum-mode-line-inactive))))
   `(window-divider-first-pixel ((,solanum-class (:foreground ,solanum-mode-line-inactive))))
   `(window-divider-last-pixel ((,solanum-class (:foreground ,solanum-mode-line-inactive))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,solanum-class (:foreground ,solanum-white :background ,solanum-orderless-2))))
   `(isearch-fail ((,solanum-class (:foreground ,solanum-white :background ,solanum-error))))
   `(lazy-highlight ((,solanum-class (:foreground ,solanum-white :background ,solanum-orderless-3))))
   `(isearch-group-1 ((,solanum-class (:foreground ,solanum-white :background ,solanum-orderless-0))))
   `(isearch-group-2 ((,solanum-class (:foreground ,solanum-white :background ,solanum-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,solanum-class (:inherit isearch))))
   `(match ((,solanum-class (:inherit lazy-highlight))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,solanum-class (:foreground ,solanum-light-blue))))
   `(elfeed-search-date-face ((,solanum-class (:foreground ,solanum-orange))))
   `(elfeed-search-feed-face ((,solanum-class (:foreground ,solanum-light-green))))
   `(elfeed-search-title-face ((,solanum-class (:foreground ,solanum-yellow-potato))))
   `(elfeed-search-unread-title-face ((,solanum-class (:weight bold :foreground ,solanum-red-tomato))))
   `(elfeed-search-filter-face ((,solanum-class (:weight bold :foreground ,solanum-light-purple))))
   `(elfeed-search-last-update-face ((,solanum-class (:weight bold :foreground ,solanum-light-purple))))
   `(elfeed-search-unread-count-face ((,solanum-class (:weight bold :foreground ,solanum-light-purple))))

   `(elfeed-show-header-face ((,solanum-class (:foreground ,solanum-deep-green))))
   `(elfeed-show-author-face ((,solanum-class (:weight bold :foreground ,solanum-red-tomato))))
   `(elfeed-show-title-face ((,solanum-class (:weight bold :foreground ,solanum-red-tomato))))
   `(elfeed-show-date-face ((,solanum-class (:foreground ,solanum-light-green))))
   `(elfeed-show-feed-face ((,solanum-class (:foreground ,solanum-light-green))))
   `(elfeed-show-tags-face ((,solanum-class (:foreground ,solanum-purple))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,solanum-class (:foreground ,solanum-red-tomato :inherit italic))))
   `(doom-modeline-project-parent-dir ((,solanum-class (:foreground ,solanum-red-tomato))))
   `(doom-modeline-buffer-minor-mode ((,solanum-class (:foreground ,solanum-yellow-potato))))

   ;; corfu
   `(corfu-default ((,solanum-class (:foreground ,solanum-fg :background ,solanum-bg))))
   `(corfu-current ((,solanum-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,solanum-class (:background ,solanum-shadow))))
   `(corfu-border ((,solanum-class (:background ,solanum-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,solanum-class (:foreground ,solanum-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,solanum-class (:foreground ,solanum-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,solanum-class (:foreground ,solanum-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,solanum-class (:foreground ,solanum-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,solanum-class (:foreground ,solanum-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,solanum-class (:foreground ,solanum-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,solanum-class (:inherit error))))
   `(envrc-mode-line-none-face ((,solanum-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,solanum-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,solanum-class (:weight bold :background ,solanum-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,solanum-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-solanum)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-solanum-theme.el ends here
