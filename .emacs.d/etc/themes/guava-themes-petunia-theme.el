;;; guava-themes-petunia-theme.el --- A theme inspired by petunia colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Apr 19, 2026
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
;; A theme inspired by petunia colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-petunia "A theme inspired by petunia colors.")

(let* (
      (petunia-class '((class color) (min-colors 257)))
      (petunia-black                     "#000000")
      (petunia-white                     "#FFFFFF")

      (petunia-red                       "#e60046")
      (petunia-pink                      "#be64aa")
      (petunia-cream                     "#ffa0a0")
      (petunia-light-orange              "#ffb478")
      (petunia-orange                    "#ff8728")

      (petunia-yellow                    "#e6e678")

      (petunia-light-green               "#78c878")
      (petunia-green-forest              "#008741")

      (petunia-light-blue                "#00c3d7")
      (petunia-alt-light-blue            "#32a0cd")
      (petunia-blue                      "#3246cd")
      (petunia-deep-blue                 "#004b82")
      (petunia-cyan                      "#00ffff")

      (petunia-light-purple              "#a88dac")
      (petunia-purple                    "#8a5aff")

      (petunia-fg                        "#FFFFFF")
      (petunia-bg                        "#000000")
      (petunia-highlight                 "#1e1e1e")
      (petunia-shadow                    "#b3b3b3")

      (petunia-error                     "#FF0000")
      (petunia-warning                   "#ffff00")
      (petunia-success                   "#00ff00")

      (petunia-mode-line                 "#8a5aff")
      (petunia-mode-line-inactive        "#008741")
      (petunia-bell                      "#FFDB5A")

      (petunia-tab-1                     petunia-mode-line)
      (petunia-tab-2                     petunia-mode-line-inactive)
      (petunia-tab-3                     petunia-light-green)

      (petunia-fl-comment                petunia-light-green)
      (petunia-fl-string                 petunia-orange)
      (petunia-fl-keyword                petunia-purple)
      (petunia-fl-builtin                petunia-light-blue)
      (petunia-fl-type                   petunia-yellow)
      (petunia-fl-function-name          petunia-red)
      (petunia-fl-variable-name          petunia-pink)
      (petunia-fl-constant               petunia-green-forest)
      (petunia-fl-warning                petunia-warning)
      (petunia-fl-punctuation            petunia-light-purple)
      (petunia-fl-negation-char          petunia-light-orange)

      (petunia-diff-added                "#5aa05a")
      (petunia-diff-removed              "#a05a5a")
      (petunia-diff-refine-added         "#007800")
      (petunia-diff-refine-removed       "#780000")
      (petunia-diff-header               "#5a5a5a")
      (petunia-diff-file-header          "#3c3c3c")
      (petunia-diff-context              "#828282")
      (petunia-smerge-base               "#5A5AA0")

      (petunia-orderless-0               "#c878dc")
      (petunia-orderless-1               "#28b43c")
      (petunia-orderless-2               "#ff7300")
      (petunia-orderless-3               "#009bff")

      (petunia-prescient-0               "#46C8A5")
      (petunia-prescient-1               "#C84669")

      (petunia-vc-change                 petunia-blue)
      (petunia-vc-insert                 petunia-success)
      (petunia-vc-delete                 petunia-error))

  (custom-theme-set-faces
   'guava-themes-petunia

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,petunia-class (:foreground ,petunia-fg :background ,petunia-bg))))

   ;; error, warning, success
   `(error ((,petunia-class (:foreground ,petunia-error :weight bold))))
   `(warning ((,petunia-class (:foreground ,petunia-warning :weight bold))))
   `(success ((,petunia-class (:foreground ,petunia-success :weight bold))))

   ;; highlight
   `(highlight ((,petunia-class (:background ,petunia-highlight))))

   ;; shadow
   `(shadow ((,petunia-class (:foreground ,petunia-shadow))))

   ;; region
   `(region ((,petunia-class (:background ,petunia-blue :extend t))))
   `(secondary-selection ((,petunia-class (:background ,petunia-alt-light-blue :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,petunia-class (:foreground ,petunia-fl-comment :weight medium))))
   `(font-lock-string-face ((,petunia-class (:foreground ,petunia-fl-string :weight medium))))
   `(font-lock-keyword-face ((,petunia-class (:foreground ,petunia-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,petunia-class (:foreground ,petunia-fl-builtin :weight medium))))
   `(font-lock-type-face ((,petunia-class (:foreground ,petunia-fl-type :weight medium))))
   `(font-lock-function-name-face ((,petunia-class (:foreground ,petunia-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,petunia-class (:foreground ,petunia-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,petunia-class (:foreground ,petunia-fl-constant :weight medium))))
   `(font-lock-warning-face ((,petunia-class (:foreground ,petunia-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,petunia-class (:foreground ,petunia-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,petunia-class (:foreground ,petunia-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,petunia-class (:foreground ,petunia-white :background ,petunia-mode-line))))

   ;; fringe
   `(fringe ((,petunia-class (:foreground ,petunia-red :background ,petunia-bg))))
   `(diff-hl-change ((,petunia-class (:foreground ,petunia-vc-change :background ,petunia-vc-change))))
   `(diff-hl-insert ((,petunia-class (:foreground ,petunia-vc-insert :background ,petunia-vc-insert))))
   `(diff-hl-delete ((,petunia-class (:foreground ,petunia-vc-delete :background ,petunia-vc-delete))))

   ;; line-number
   `(line-number ((,petunia-class (:foreground ,petunia-fg :inherit default))))
   `(line-number-current-line ((,petunia-class (:foreground ,petunia-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,petunia-class (:background ,petunia-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,petunia-class (:background ,petunia-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,petunia-class (:foreground ,petunia-white :background ,petunia-mode-line))))
   `(mode-line-inactive ((,petunia-class (:foreground ,petunia-white :background ,petunia-mode-line-inactive :inherit mode-line))))
   `(guava-themes-visible-bell ((,petunia-class (:foreground ,petunia-white :background ,petunia-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,petunia-class (:foreground ,petunia-orange))))

   ;; borders
   `(vertical-border ((,petunia-class (:foreground ,petunia-mode-line))))

   ;; header-line
   `(header-line ((,petunia-class (:inherit mode-line))))
   `(which-func ((,petunia-class (:foreground ,petunia-white))))

   ;; tab-bar
   `(tab-bar ((,petunia-class (:foreground ,petunia-white :background ,petunia-tab-2 :weight bold :height 1.0))))
   `(tab-bar-tab ((,petunia-class (:foreground ,petunia-white :background ,petunia-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,petunia-class (:foreground ,petunia-white :background ,petunia-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,petunia-class (:foreground ,petunia-white :background ,petunia-tab-2 :weight bold :height 0.9))))
   `(tab-line-tab ((,petunia-class (:foreground ,petunia-white :background ,petunia-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,petunia-class (:foreground ,petunia-white :background ,petunia-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,petunia-class (:foreground ,petunia-white :background ,petunia-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,petunia-class (:foreground ,petunia-white :background ,petunia-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,petunia-class (:foreground ,petunia-light-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,petunia-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,petunia-class (:foreground ,petunia-black :background ,petunia-bell))))
   `(show-paren-mismatch ((,petunia-class (:foreground ,petunia-white :background ,petunia-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,petunia-class (:background ,petunia-error))))

   ;; links
   `(link ((,petunia-class (:foreground ,petunia-light-blue :underline t :weight bold))))
   `(link-visited ((,petunia-class (:foreground ,petunia-orange :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,petunia-class (:foreground ,petunia-red :weight medium))))
   `(outline-2 ((,petunia-class (:foreground ,petunia-green-forest :weight medium))))
   `(outline-3 ((,petunia-class (:foreground ,petunia-yellow :weight medium))))
   `(outline-4 ((,petunia-class (:foreground ,petunia-purple :weight medium))))
   `(outline-5 ((,petunia-class (:inherit outline-1))))
   `(outline-6 ((,petunia-class (:inherit outline-2))))
   `(outline-7 ((,petunia-class (:inherit outline-3))))
   `(outline-8 ((,petunia-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,petunia-class (:foreground ,petunia-cyan))))
   `(escape-glyph ((,petunia-class (:inherit homoglyph))))
   `(nobreak-space ((,petunia-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,petunia-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,petunia-class (:background ,petunia-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,petunia-class (:foreground ,petunia-cyan :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-header :extend t))))
   `(diff-file-header ((,petunia-class (:weight bold :foreground ,petunia-white :background ,petunia-diff-file-header :extend t))))
   `(diff-context ((,petunia-class (:foreground ,petunia-white :background ,petunia-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,petunia-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,petunia-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,petunia-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,petunia-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,petunia-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,petunia-class (:foreground ,petunia-white :background ,petunia-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,petunia-class (:foreground ,petunia-warning :weight bold))))
   `(completions-first-difference ((,petunia-class (:foreground ,petunia-error :weight bold))))

   ;; org-faces
   `(org-todo ((,petunia-class (:foreground ,petunia-vc-delete :weight bold))))
   `(org-done ((,petunia-class (:foreground ,petunia-vc-insert :weight bold))))
   `(org-hide ((,petunia-class (:foreground ,petunia-bg))))
   `(org-table ((,petunia-class (:foreground ,petunia-green-forest))))
   `(org-date ((,petunia-class (:foreground ,petunia-light-orange))))
   `(org-date-selected ((,petunia-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,petunia-class (:foreground ,petunia-orderless-0))))
   `(org-headline-done ((,petunia-class (:foreground ,petunia-orderless-1))))
   `(org-document-title ((,petunia-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,petunia-class (:inherit shadow))))
   `(org-meta-line ((,petunia-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,petunia-class (:foreground ,petunia-tab-3))))
   `(window-divider-first-pixel ((,petunia-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,petunia-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,petunia-class (:foreground ,petunia-white :background ,petunia-orderless-2))))
   `(isearch-fail ((,petunia-class (:foreground ,petunia-white :background ,petunia-error))))
   `(lazy-highlight ((,petunia-class (:foreground ,petunia-white :background ,petunia-orderless-3))))
   `(isearch-group-1 ((,petunia-class (:foreground ,petunia-white :background ,petunia-orderless-0))))
   `(isearch-group-2 ((,petunia-class (:foreground ,petunia-white :background ,petunia-orderless-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,petunia-class (:inherit isearch))))
   `(match ((,petunia-class (:inherit lazy-highlight))))

   ;; custom-button
   `(custom-button ((,petunia-class (:foreground ,petunia-white :background ,petunia-mode-line :box (:line-width 2 :style released-button)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,petunia-class (:foreground ,petunia-pink))))
   `(elfeed-search-date-face ((,petunia-class (:foreground ,petunia-light-blue))))
   `(elfeed-search-feed-face ((,petunia-class (:foreground ,petunia-purple))))
   `(elfeed-search-title-face ((,petunia-class (:foreground ,petunia-deep-blue))))
   `(elfeed-search-unread-title-face ((,petunia-class (:weight bold :foreground ,petunia-green-forest))))
   `(elfeed-search-filter-face ((,petunia-class (:weight bold :foreground ,petunia-light-orange))))
   `(elfeed-search-last-update-face ((,petunia-class (:weight bold :foreground ,petunia-light-orange))))
   `(elfeed-search-unread-count-face ((,petunia-class (:weight bold :foreground ,petunia-light-orange))))

   `(elfeed-show-header-face ((,petunia-class (:foreground ,petunia-green-forest))))
   `(elfeed-show-author-face ((,petunia-class (:weight bold :foreground ,petunia-red))))
   `(elfeed-show-title-face ((,petunia-class (:weight bold :foreground ,petunia-red))))
   `(elfeed-show-date-face ((,petunia-class (:foreground ,petunia-orange))))
   `(elfeed-show-feed-face ((,petunia-class (:foreground ,petunia-orange))))
   `(elfeed-show-tags-face ((,petunia-class (:foreground ,petunia-pink))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,petunia-class (:foreground ,petunia-light-green :inherit italic))))
   `(doom-modeline-project-parent-dir ((,petunia-class (:foreground ,petunia-light-green))))
   `(doom-modeline-buffer-minor-mode ((,petunia-class (:foreground ,petunia-yellow))))

   ;; corfu
   `(corfu-default ((,petunia-class (:foreground ,petunia-fg :background ,petunia-bg))))
   `(corfu-current ((,petunia-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,petunia-class (:background ,petunia-shadow))))
   `(corfu-border ((,petunia-class (:background ,petunia-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,petunia-class (:foreground ,petunia-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,petunia-class (:foreground ,petunia-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,petunia-class (:foreground ,petunia-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,petunia-class (:foreground ,petunia-orderless-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,petunia-class (:foreground ,petunia-prescient-0 :weight bold))))
   `(prescient-secondary-highlight ((,petunia-class (:foreground ,petunia-prescient-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,petunia-class (:inherit error))))
   `(envrc-mode-line-none-face ((,petunia-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,petunia-class (:inherit success))))

   ;; devdocs
   `(devdocs-code-block ((,petunia-class (:weight bold :background ,petunia-highlight))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,petunia-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-petunia)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-petunia-theme.el ends here
