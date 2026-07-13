;;; guava-themes-cordyline-theme.el --- A theme inspired by the ti plant colors -*- lexical-binding: t; -*-

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
;; A theme inspired by the ti plant colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-cordyline "A theme inspired by the ti plant colors.")

(let* (
      (cordyline-class '((class color) (min-colors 257)))
      ;; (cordyline-black                     "#000000")
      (cordyline-white                     "#FFFFFF")

      (cordyline-orange-red                "#ce462c")
      (cordyline-pink-red                  "#cb646e")
      (cordyline-pink-purple               "#da70d6")
      (cordyline-deep-fuchsia              "#6e1551")

      (cordyline-light-blue                "#5f70cb")
      (cordyline-blue                      "#3150af")
      (cordyline-deep-blue                 "#214bd5")
      (cordyline-steel-blue                "#4f94cd")
      (cordyline-cyan                      "#00ffff")
      (cordyline-dark-cyan                 "#007896")

      (cordyline-light-green               "#afd2b9")
      (cordyline-green                     "#005f55")

      (cordyline-light-purple              "#a25ad1")
      (cordyline-alt-light-purple          "#a29cf1")
      (cordyline-purple                    "#7050af")
      (cordyline-alt-purple                "#493d4e")
      (cordyline-deep-purple               "#3c193c")
      (cordyline-purple-red                "#a03c5a")

      (cordyline-fg                        "#FFFFFF")
      (cordyline-bg                        "#392b38")
      (cordyline-highlight                 "#251724")
      (cordyline-shadow                    "#b3b3b3")

      (cordyline-error                     "#FF0000")
      (cordyline-warning                   "#f6d909")
      (cordyline-success                   "#23a334")

      (cordyline-mode-line                 "#a03c5a")
      (cordyline-mode-line-inactive        "#3c193c")
      (cordyline-bell                      "#3C8CA0")

      (cordyline-tab-1                     cordyline-mode-line)
      (cordyline-tab-2                     cordyline-mode-line-inactive)
      (cordyline-tab-3                     cordyline-alt-purple)

      (cordyline-fl-comment                cordyline-light-blue)
      (cordyline-fl-string                 cordyline-pink-red)
      (cordyline-fl-keyword                cordyline-purple)
      (cordyline-fl-builtin                cordyline-purple-red)
      (cordyline-fl-type                   cordyline-light-purple)
      (cordyline-fl-function-name          cordyline-pink-purple)
      (cordyline-fl-variable-name          cordyline-blue)
      (cordyline-fl-constant               cordyline-dark-cyan)
      (cordyline-fl-warning                cordyline-warning)
      (cordyline-fl-punctuation            cordyline-light-green)
      (cordyline-fl-negation-char          cordyline-orange-red)

      (cordyline-diff-added                "#5aa05a")
      (cordyline-diff-removed              "#a05a5a")
      (cordyline-diff-refine-added         "#007800")
      (cordyline-diff-refine-removed       "#780000")
      (cordyline-diff-header               "#5a5a5a")
      (cordyline-diff-file-header          "#3c3c3c")
      (cordyline-diff-context              "#828282")
      (cordyline-smerge-base               "#5A5AA0")

      (cordyline-orderless-0               "#af50b9")
      (cordyline-orderless-1               "#28a03c")
      (cordyline-orderless-2               "#ff6400")
      (cordyline-orderless-3               "#3c82ff")

      (cordyline-prescient-0               "#46C8A5")
      (cordyline-prescient-1               "#C84669")

      (cordyline-vc-change                 cordyline-deep-blue)
      (cordyline-vc-insert                 cordyline-success)
      (cordyline-vc-delete                 cordyline-error))

  (custom-theme-set-faces
   'guava-themes-cordyline

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,cordyline-class (:foreground ,cordyline-fg :background ,cordyline-bg))))

   ;; error, warning, success
   `(error ((,cordyline-class (:foreground ,cordyline-error :weight bold))))
   `(warning ((,cordyline-class (:foreground ,cordyline-warning :weight bold))))
   `(success ((,cordyline-class (:foreground ,cordyline-success :weight bold))))

   ;; highlight
   `(highlight ((,cordyline-class (:background ,cordyline-highlight))))

   ;; shadow
   `(shadow ((,cordyline-class (:foreground ,cordyline-shadow))))

   ;; region
   `(region ((,cordyline-class (:background ,cordyline-deep-fuchsia :extend t))))
   `(secondary-selection ((,cordyline-class (:background ,cordyline-green :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,cordyline-class (:foreground ,cordyline-fl-comment :weight medium))))
   `(font-lock-string-face ((,cordyline-class (:foreground ,cordyline-fl-string :weight medium))))
   `(font-lock-keyword-face ((,cordyline-class (:foreground ,cordyline-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,cordyline-class (:foreground ,cordyline-fl-builtin :weight medium))))
   `(font-lock-type-face ((,cordyline-class (:foreground ,cordyline-fl-type :weight medium))))
   `(font-lock-function-name-face ((,cordyline-class (:foreground ,cordyline-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,cordyline-class (:foreground ,cordyline-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,cordyline-class (:foreground ,cordyline-fl-constant :weight medium))))
   `(font-lock-warning-face ((,cordyline-class (:foreground ,cordyline-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,cordyline-class (:foreground ,cordyline-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,cordyline-class (:foreground ,cordyline-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-mode-line))))

   ;; fringe
   `(fringe ((,cordyline-class (:foreground ,cordyline-cyan :background ,cordyline-bg))))
   `(diff-hl-change ((,cordyline-class (:foreground ,cordyline-vc-change :background ,cordyline-vc-change))))
   `(diff-hl-insert ((,cordyline-class (:foreground ,cordyline-vc-insert :background ,cordyline-vc-insert))))
   `(diff-hl-delete ((,cordyline-class (:foreground ,cordyline-vc-delete :background ,cordyline-vc-delete))))

   ;; line-number
   `(line-number ((,cordyline-class (:foreground ,cordyline-fg :inherit default))))
   `(line-number-current-line ((,cordyline-class (:foreground ,cordyline-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,cordyline-class (:background ,cordyline-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,cordyline-class (:background ,cordyline-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-mode-line))))
   `(mode-line-inactive ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,cordyline-class (:foreground ,cordyline-white))))

   ;; borders
   `(vertical-border ((,cordyline-class (:foreground ,cordyline-mode-line))))

   ;; header-line
   `(header-line ((,cordyline-class (:inherit mode-line))))
   `(which-func ((,cordyline-class (:foreground ,cordyline-white))))

   ;; tab-bar
   `(tab-bar ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,cordyline-class (:foreground ,cordyline-steel-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,cordyline-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-bell))))
   `(show-paren-mismatch ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,cordyline-class (:background ,cordyline-error))))

   ;; links
   `(link ((,cordyline-class (:foreground ,cordyline-steel-blue :underline t :weight bold))))
   `(link-visited ((,cordyline-class (:foreground ,cordyline-purple :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,cordyline-class (:foreground ,cordyline-light-blue :weight medium))))
   `(outline-2 ((,cordyline-class (:foreground ,cordyline-light-purple :weight medium))))
   `(outline-3 ((,cordyline-class (:foreground ,cordyline-dark-cyan :weight medium))))
   `(outline-4 ((,cordyline-class (:foreground ,cordyline-purple-red :weight medium))))
   `(outline-5 ((,cordyline-class (:inherit outline-1))))
   `(outline-6 ((,cordyline-class (:inherit outline-2))))
   `(outline-7 ((,cordyline-class (:inherit outline-3))))
   `(outline-8 ((,cordyline-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,cordyline-class (:foreground ,cordyline-cyan))))
   `(escape-glyph ((,cordyline-class (:inherit homoglyph))))
   `(nobreak-space ((,cordyline-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,cordyline-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,cordyline-class (:background ,cordyline-steel-blue))))

   ;; help-key-binding
   `(help-key-binding ((,cordyline-class (:foreground ,cordyline-alt-light-purple :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-header :extend t))))
   `(diff-file-header ((,cordyline-class (:weight bold :foreground ,cordyline-white :background ,cordyline-diff-file-header :extend t))))
   `(diff-context ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,cordyline-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,cordyline-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,cordyline-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,cordyline-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,cordyline-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,cordyline-class (:foreground ,cordyline-warning :weight bold))))
   `(completions-first-difference ((,cordyline-class (:foreground ,cordyline-error :weight bold))))

   ;; org-faces
   `(org-todo ((,cordyline-class (:foreground ,cordyline-vc-delete :weight bold))))
   `(org-done ((,cordyline-class (:foreground ,cordyline-vc-insert :weight bold))))
   `(org-hide ((,cordyline-class (:foreground ,cordyline-bg))))
   `(org-table ((,cordyline-class (:foreground ,cordyline-light-purple))))
   `(org-date ((,cordyline-class (:foreground ,cordyline-orange-red))))
   `(org-date-selected ((,cordyline-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,cordyline-class (:foreground ,cordyline-orderless-2))))
   `(org-headline-done ((,cordyline-class (:foreground ,cordyline-orderless-1))))
   `(org-document-title ((,cordyline-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,cordyline-class (:inherit shadow))))
   `(org-meta-line ((,cordyline-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,cordyline-class (:foreground ,cordyline-tab-3))))
   `(window-divider-first-pixel ((,cordyline-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,cordyline-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-orderless-2))))
   `(isearch-fail ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-error))))
   `(lazy-highlight ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-orderless-3))))
   `(isearch-group-1 ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-orderless-0))))
   `(isearch-group-2 ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,cordyline-class (:inherit isearch))))
   `(match ((,cordyline-class (:inherit lazy-highlight))))

   ;; custom-button
   `(custom-button ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-mode-line :box (:line-width 2 :style released-button)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,cordyline-class (:foreground ,cordyline-light-blue))))
   `(elfeed-search-date-face ((,cordyline-class (:foreground ,cordyline-green))))
   `(elfeed-search-feed-face ((,cordyline-class (:foreground ,cordyline-dark-cyan))))
   `(elfeed-search-title-face ((,cordyline-class (:foreground ,cordyline-pink-red))))
   `(elfeed-search-unread-title-face ((,cordyline-class (:weight bold :foreground ,cordyline-light-purple))))
   `(elfeed-search-filter-face ((,cordyline-class (:weight bold :foreground ,cordyline-steel-blue))))
   `(elfeed-search-last-update-face ((,cordyline-class (:weight bold :foreground ,cordyline-steel-blue))))
   `(elfeed-search-unread-count-face ((,cordyline-class (:weight bold :foreground ,cordyline-steel-blue))))

   `(elfeed-show-header-face ((,cordyline-class (:foreground ,cordyline-purple-red))))
   `(elfeed-show-author-face ((,cordyline-class (:weight bold :foreground ,cordyline-light-blue))))
   `(elfeed-show-title-face ((,cordyline-class (:weight bold :foreground ,cordyline-light-blue))))
   `(elfeed-show-date-face ((,cordyline-class (:foreground ,cordyline-dark-cyan))))
   `(elfeed-show-feed-face ((,cordyline-class (:foreground ,cordyline-dark-cyan))))
   `(elfeed-show-tags-face ((,cordyline-class (:foreground ,cordyline-orange-red))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,cordyline-class (:foreground ,cordyline-steel-blue :inherit italic))))
   `(doom-modeline-project-parent-dir ((,cordyline-class (:foreground ,cordyline-steel-blue))))
   `(doom-modeline-buffer-minor-mode ((,cordyline-class (:foreground ,cordyline-pink-red))))

   ;; corfu
   `(corfu-default ((,cordyline-class (:foreground ,cordyline-fg :background ,cordyline-bg))))
   `(corfu-current ((,cordyline-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,cordyline-class (:background ,cordyline-shadow))))
   `(corfu-border ((,cordyline-class (:background ,cordyline-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,cordyline-class (:foreground ,cordyline-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,cordyline-class (:foreground ,cordyline-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,cordyline-class (:foreground ,cordyline-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,cordyline-class (:foreground ,cordyline-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,cordyline-class (:foreground ,cordyline-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,cordyline-class (:foreground ,cordyline-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,cordyline-class (:inherit error))))
   `(envrc-mode-line-none-face ((,cordyline-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,cordyline-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,cordyline-class (:weight bold :background ,cordyline-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,cordyline-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-cordyline)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-cordyline-theme.el ends here
