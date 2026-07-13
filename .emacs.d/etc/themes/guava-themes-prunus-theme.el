;;; guava-themes-prunus-theme.el --- A theme inspired by cherry colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Dec 29, 2025
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
;; A theme inspired by cherry colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-prunus "A theme inspired by cherry colors.")

(let* (
      (prunus-class '((class color) (min-colors 257)))
      ;;(prunus-black                     "#000000")
      (prunus-white                     "#FFFFFF")

      (prunus-light-brown               "#785a4b")
      (prunus-alt-light-brown           "#9b7d6e")
      (prunus-brown                     "#5f4132")
      (prunus-alt-brown                 "#5a5046")
      (prunus-deep-brown                "#553c23")

      (prunus-light-green               "#50a05f")
      (prunus-green-forest              "#007341")
      (prunus-green-subdued             "#2d5519")
      (prunus-oxidized-green            "#4e584e")

      (prunus-red                       "#a01e14")
      (prunus-deep-red                  "#781e14")
      (prunus-light-pink                "#cd7888")
      (prunus-pink                      "#cd3c88")

      (prunus-blue                      "#4650e1")
      (prunus-deep-blue                 "#4650af")
      (prunus-cyan                      "#00aaaa")

      (prunus-purple                    "#7350a0")
      (prunus-purple-red                "#8B2252")

      (prunus-fg                        "#ebafc8")
      (prunus-bg                        "#190f05")
      (prunus-highlight                 "#2d2319")
      (prunus-shadow                    "#b3b3b3")

      (prunus-error                     "#FF0000")
      (prunus-warning                   "#f6d911")
      (prunus-success                   "#23D734")

      (prunus-mode-line                 "#463c32")
      (prunus-mode-line-inactive        "#5a5046")
      (prunus-bell                      "#324646")

      (prunus-tab-1                     prunus-mode-line)
      (prunus-tab-2                     prunus-mode-line-inactive)
      (prunus-tab-3                     prunus-deep-brown)

      (prunus-fl-comment                prunus-green-forest)
      (prunus-fl-string                 prunus-purple-red)
      (prunus-fl-keyword                prunus-light-brown)
      (prunus-fl-builtin                prunus-pink)
      (prunus-fl-type                   prunus-alt-light-brown)
      (prunus-fl-function-name          prunus-red)
      (prunus-fl-variable-name          prunus-green-subdued)
      (prunus-fl-constant               prunus-purple)
      (prunus-fl-warning                prunus-warning)
      (prunus-fl-punctuation            prunus-oxidized-green)
      (prunus-fl-negation-char          prunus-deep-red)

      (prunus-diff-added                "#5aa05a")
      (prunus-diff-removed              "#a05a5a")
      (prunus-diff-refine-added         "#007800")
      (prunus-diff-refine-removed       "#780000")
      (prunus-diff-header               "#5a5a5a")
      (prunus-diff-file-header          "#3c3c3c")
      (prunus-diff-context              "#828282")
      (prunus-smerge-base               "#5A5AA0")

      (prunus-orderless-0               "#af50c8")
      (prunus-orderless-1               "#28a03c")
      (prunus-orderless-2               "#ff6400")
      (prunus-orderless-3               "#3c82ff")

      (prunus-prescient-0               "#46C8A5")
      (prunus-prescient-1               "#C84669")

      (prunus-vc-change                 prunus-blue)
      (prunus-vc-insert                 prunus-success)
      (prunus-vc-delete                 prunus-error))

  (custom-theme-set-faces
   'guava-themes-prunus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,prunus-class (:foreground ,prunus-fg :background ,prunus-bg))))

   ;; error, warning, success
   `(error ((,prunus-class (:foreground ,prunus-error :weight bold))))
   `(warning ((,prunus-class (:foreground ,prunus-warning :weight bold))))
   `(success ((,prunus-class (:foreground ,prunus-success :weight bold))))

   ;; highlight
   `(highlight ((,prunus-class (:background ,prunus-highlight))))

   ;; shadow
   `(shadow ((,prunus-class (:foreground ,prunus-shadow))))

   ;; region
   `(region ((,prunus-class (:background ,prunus-brown :extend t))))
   `(secondary-selection ((,prunus-class (:background ,prunus-alt-brown :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,prunus-class (:foreground ,prunus-fl-comment :weight medium))))
   `(font-lock-string-face ((,prunus-class (:foreground ,prunus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,prunus-class (:foreground ,prunus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,prunus-class (:foreground ,prunus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,prunus-class (:foreground ,prunus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,prunus-class (:foreground ,prunus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,prunus-class (:foreground ,prunus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,prunus-class (:foreground ,prunus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,prunus-class (:foreground ,prunus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,prunus-class (:foreground ,prunus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,prunus-class (:foreground ,prunus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,prunus-class (:foreground ,prunus-white :background ,prunus-mode-line))))

   ;; fringe
   `(fringe ((,prunus-class (:foreground ,prunus-cyan :background ,prunus-bg))))
   `(diff-hl-change ((,prunus-class (:foreground ,prunus-vc-change :background ,prunus-vc-change))))
   `(diff-hl-insert ((,prunus-class (:foreground ,prunus-vc-insert :background ,prunus-vc-insert))))
   `(diff-hl-delete ((,prunus-class (:foreground ,prunus-vc-delete :background ,prunus-vc-delete))))

   ;; line-number
   `(line-number ((,prunus-class (:foreground ,prunus-purple-red :inherit default))))
   `(line-number-current-line ((,prunus-class (:foreground ,prunus-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,prunus-class (:background ,prunus-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,prunus-class (:background ,prunus-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,prunus-class (:foreground ,prunus-white :background ,prunus-mode-line))))
   `(mode-line-inactive ((,prunus-class (:foreground ,prunus-white :background ,prunus-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,prunus-class (:foreground ,prunus-white :background ,prunus-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,prunus-class (:foreground ,prunus-white))))

   ;; borders
   `(vertical-border ((,prunus-class (:foreground ,prunus-mode-line))))

   ;; header-line
   `(header-line ((,prunus-class (:inherit mode-line))))
   `(which-func ((,prunus-class (:foreground ,prunus-white))))

   ;; tab-bar
   `(tab-bar ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-2 :inherit tab-bar))))

   ;; tab-line
   `(tab-line ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,prunus-class (:foreground ,prunus-white :background ,prunus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,prunus-class (:foreground ,prunus-pink :weight bold :height 0.9))))
   `(tab-line-tab-special ((,prunus-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,prunus-class (:foreground ,prunus-white :background ,prunus-bell))))
   `(show-paren-mismatch ((,prunus-class (:foreground ,prunus-white :background ,prunus-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,prunus-class (:background ,prunus-error))))

   ;; links
   `(link ((,prunus-class (:foreground ,prunus-deep-blue :underline t :weight bold))))
   `(link-visited ((,prunus-class (:foreground ,prunus-cyan :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,prunus-class (:foreground ,prunus-alt-light-brown :weight medium))))
   `(outline-2 ((,prunus-class (:foreground ,prunus-green-forest :weight medium))))
   `(outline-4 ((,prunus-class (:foreground ,prunus-deep-blue :weight medium))))
   `(outline-3 ((,prunus-class (:foreground ,prunus-red :weight medium))))
   `(outline-5 ((,prunus-class (:inherit outline-1))))
   `(outline-6 ((,prunus-class (:inherit outline-2))))
   `(outline-7 ((,prunus-class (:inherit outline-3))))
   `(outline-8 ((,prunus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,prunus-class (:foreground ,prunus-cyan))))
   `(escape-glyph ((,prunus-class (:inherit homoglyph))))
   `(nobreak-space ((,prunus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,prunus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,prunus-class (:background ,prunus-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,prunus-class (:foreground ,prunus-cyan :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-header :extend t))))
   `(diff-file-header ((,prunus-class (:weight bold :foreground ,prunus-white :background ,prunus-diff-file-header :extend t))))
   `(diff-context ((,prunus-class (:foreground ,prunus-white :background ,prunus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,prunus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,prunus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,prunus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,prunus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,prunus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,prunus-class (:foreground ,prunus-white :background ,prunus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,prunus-class (:foreground ,prunus-warning :weight bold))))
   `(completions-first-difference ((,prunus-class (:foreground ,prunus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,prunus-class (:foreground ,prunus-vc-delete :weight bold))))
   `(org-done ((,prunus-class (:foreground ,prunus-vc-insert :weight bold))))
   `(org-hide ((,prunus-class (:foreground ,prunus-bg))))
   `(org-table ((,prunus-class (:foreground ,prunus-light-pink))))
   `(org-date ((,prunus-class (:foreground ,prunus-deep-red))))
   `(org-date-selected ((,prunus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,prunus-class (:foreground ,prunus-orderless-2))))
   `(org-headline-done ((,prunus-class (:foreground ,prunus-orderless-3))))
   `(org-document-title ((,prunus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,prunus-class (:inherit shadow))))
   `(org-meta-line ((,prunus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,prunus-class (:foreground ,prunus-tab-3))))
   `(window-divider-first-pixel ((,prunus-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,prunus-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,prunus-class (:foreground ,prunus-white :background ,prunus-orderless-2))))
   `(isearch-fail ((,prunus-class (:foreground ,prunus-white :background ,prunus-error))))
   `(lazy-highlight ((,prunus-class (:foreground ,prunus-white :background ,prunus-orderless-3))))
   `(isearch-group-1 ((,prunus-class (:foreground ,prunus-white :background ,prunus-orderless-0))))
   `(isearch-group-2 ((,prunus-class (:foreground ,prunus-white :background ,prunus-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,prunus-class (:inherit isearch))))
   `(match ((,prunus-class (:inherit lazy-highlight))))

   ;; custom-button
   `(custom-button ((,prunus-class (:foreground ,prunus-white :background ,prunus-mode-line :box (:line-width 2 :style released-button)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,prunus-class (:foreground ,prunus-deep-blue))))
   `(elfeed-search-date-face ((,prunus-class (:foreground ,prunus-red))))
   `(elfeed-search-feed-face ((,prunus-class (:foreground ,prunus-purple-red))))
   `(elfeed-search-title-face ((,prunus-class (:foreground ,prunus-oxidized-green))))
   `(elfeed-search-unread-title-face ((,prunus-class (:weight bold :foreground ,prunus-green-forest))))
   `(elfeed-search-filter-face ((,prunus-class (:weight bold :foreground ,prunus-light-pink))))
   `(elfeed-search-last-update-face ((,prunus-class (:weight bold :foreground ,prunus-light-pink))))
   `(elfeed-search-unread-count-face ((,prunus-class (:weight bold :foreground ,prunus-light-pink))))

   `(elfeed-show-header-face ((,prunus-class (:foreground ,prunus-alt-light-brown))))
   `(elfeed-show-author-face ((,prunus-class (:weight bold :foreground ,prunus-red))))
   `(elfeed-show-title-face ((,prunus-class (:weight bold :foreground ,prunus-red))))
   `(elfeed-show-date-face ((,prunus-class (:foreground ,prunus-light-pink))))
   `(elfeed-show-feed-face ((,prunus-class (:foreground ,prunus-light-pink))))
   `(elfeed-show-tags-face ((,prunus-class (:foreground ,prunus-green-forest))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,prunus-class (:foreground ,prunus-green-forest :inherit italic))))
   `(doom-modeline-project-parent-dir ((,prunus-class (:foreground ,prunus-green-forest))))
   `(doom-modeline-buffer-minor-mode ((,prunus-class (:foreground ,prunus-shadow))))

   ;; corfu
   `(corfu-default ((,prunus-class (:foreground ,prunus-fg :background ,prunus-bg))))
   `(corfu-current ((,prunus-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,prunus-class (:background ,prunus-shadow))))
   `(corfu-border ((,prunus-class (:background ,prunus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,prunus-class (:foreground ,prunus-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,prunus-class (:foreground ,prunus-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,prunus-class (:foreground ,prunus-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,prunus-class (:foreground ,prunus-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,prunus-class (:foreground ,prunus-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,prunus-class (:foreground ,prunus-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,prunus-class (:inherit error))))
   `(envrc-mode-line-none-face ((,prunus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,prunus-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,prunus-class (:weight bold :background ,prunus-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,prunus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-prunus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-prunus-theme.el ends here
