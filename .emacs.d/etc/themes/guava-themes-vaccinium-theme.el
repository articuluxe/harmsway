;;; guava-themes-vaccinium-theme.el --- A theme inspired by blueberry colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Mar 22, 2026
;; Version: 0.19.0
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
;; A theme inspired by blueberry, huckleberry, cranberry, and bilberry colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-vaccinium "A theme inspired by blueberry, huckleberry, cranberry, and bilberry colors.")

(let* (
      (vaccinium-class '((class color) (min-colors 257)))
      (vaccinium-black                     "#000000")
      (vaccinium-white                     "#FFFFFF")

      (vaccinium-red                       "#b92e33")
      (vaccinium-pink                      "#df7ce7")
      (vaccinium-orange                    "#ffa07a")

      (vaccinium-yellow                    "#fde8b9")

      (vaccinium-light-green               "#41aa64")
      (vaccinium-green-forest              "#006e41")

      (vaccinium-blueberry                 "#5582d7")
      (vaccinium-blue                      "#0f69b4")
      (vaccinium-alt-blue                  "#1455f1")
      (vaccinium-deep-blue                 "#1c5367")
      (vaccinium-steel-blue                "#6a7e98")
      (vaccinium-alt-steel-blue            "#566a84")
      (vaccinium-cyan                      "#00ffff")

      (vaccinium-light-purple              "#8282af")
      (vaccinium-purple                    "#6464af")
      (vaccinium-purple-pink               "#aa78cf")

      (vaccinium-fg-1                      "#FFFFFF")
      (vaccinium-fg-2                      "#c3c3c3")
      (vaccinium-bg                        "#1c1c26")
      (vaccinium-highlight                 "#30303a")
      (vaccinium-shadow                    "#b3b3b3")
      (vaccinium-tooltip                   "#ffffe0")
      (vaccinium-region-1                  "#1c5367")
      (vaccinium-region-2                  "#3755a0")

      (vaccinium-error                     "#FF0000")
      (vaccinium-warning                   "#f6d909")
      (vaccinium-success                   "#1ebe1e")
      (vaccinium-change                    "#1455f1")

      (vaccinium-mode-line                 "#5582d7")
      (vaccinium-mode-line-inactive        "#6a7e98")
      (vaccinium-mode-line-modified        "#ffa07a")
      (vaccinium-bell                      "#D76955")

      (vaccinium-tab-1                     vaccinium-mode-line)
      (vaccinium-tab-2                     vaccinium-mode-line-inactive)
      (vaccinium-tab-3                     vaccinium-alt-steel-blue)

      (vaccinium-fl-comment                vaccinium-green-forest)
      (vaccinium-fl-string                 vaccinium-orange)
      (vaccinium-fl-keyword                vaccinium-light-green)
      (vaccinium-fl-builtin                vaccinium-steel-blue)
      (vaccinium-fl-type                   vaccinium-purple)
      (vaccinium-fl-function-name          vaccinium-blue)
      (vaccinium-fl-variable-name          vaccinium-red)
      (vaccinium-fl-constant               vaccinium-pink)
      (vaccinium-fl-warning                vaccinium-warning)
      (vaccinium-fl-punctuation            vaccinium-fg-2)
      (vaccinium-fl-negation-char          vaccinium-purple-pink)

      (vaccinium-diff-added                "#5aa05a")
      (vaccinium-diff-removed              "#a05a5a")
      (vaccinium-diff-refine-added         "#007800")
      (vaccinium-diff-refine-removed       "#780000")
      (vaccinium-diff-header               "#5a5a5a")
      (vaccinium-diff-file-header          "#3c3c3c")
      (vaccinium-diff-context              "#828282")
      (vaccinium-smerge-base               "#5A5AA0")

      (vaccinium-square-a-0                "#af50c8")
      (vaccinium-square-a-1                "#28a03c")
      (vaccinium-square-a-2                "#ff6400")
      (vaccinium-square-a-3                "#3c82ff")

      (vaccinium-complementary-a-0         "#46C8A5")
      (vaccinium-complementary-a-1         "#C84669"))

  (custom-theme-set-faces
   'guava-themes-vaccinium

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,vaccinium-class (:foreground ,vaccinium-fg-1 :background ,vaccinium-bg))))

   ;; error, warning, success
   `(error ((,vaccinium-class (:foreground ,vaccinium-error :weight bold))))
   `(warning ((,vaccinium-class (:foreground ,vaccinium-warning :weight bold))))
   `(success ((,vaccinium-class (:foreground ,vaccinium-success :weight bold))))

   ;; highlight
   `(highlight ((,vaccinium-class (:background ,vaccinium-highlight))))

   ;; shadow
   `(shadow ((,vaccinium-class (:foreground ,vaccinium-shadow))))

   ;; tooltip
   `(tooltip ((,vaccinium-class (:weight bold :foreground ,vaccinium-black :background ,vaccinium-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,vaccinium-class (:background ,vaccinium-region-1 :extend t))))
   `(secondary-selection ((,vaccinium-class (:background ,vaccinium-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,vaccinium-class (:foreground ,vaccinium-fl-comment :weight medium))))
   `(font-lock-string-face ((,vaccinium-class (:foreground ,vaccinium-fl-string :weight medium))))
   `(font-lock-keyword-face ((,vaccinium-class (:foreground ,vaccinium-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,vaccinium-class (:foreground ,vaccinium-fl-builtin :weight medium))))
   `(font-lock-type-face ((,vaccinium-class (:foreground ,vaccinium-fl-type :weight medium))))
   `(font-lock-function-name-face ((,vaccinium-class (:foreground ,vaccinium-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,vaccinium-class (:foreground ,vaccinium-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,vaccinium-class (:foreground ,vaccinium-fl-constant :weight medium))))
   `(font-lock-warning-face ((,vaccinium-class (:foreground ,vaccinium-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,vaccinium-class (:foreground ,vaccinium-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,vaccinium-class (:foreground ,vaccinium-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-mode-line))))

   ;; fringe
   `(fringe ((,vaccinium-class (:foreground ,vaccinium-red :background ,vaccinium-bg))))
   `(diff-hl-change ((,vaccinium-class (:foreground ,vaccinium-change :background ,vaccinium-change))))
   `(diff-hl-insert ((,vaccinium-class (:foreground ,vaccinium-success :background ,vaccinium-success))))
   `(diff-hl-delete ((,vaccinium-class (:foreground ,vaccinium-error :background ,vaccinium-error))))

   ;; line-number
   `(line-number ((,vaccinium-class (:foreground ,vaccinium-fg-1 :inherit default))))
   `(line-number-current-line ((,vaccinium-class (:foreground ,vaccinium-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,vaccinium-class (:background ,vaccinium-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,vaccinium-class (:background ,vaccinium-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-mode-line
                                               :box ,(if guava-themes-boxed-mode-line
                                                         '(:line-width -1 :style released-button)
                                                       'unspecified)))))
   `(mode-line-inactive ((,vaccinium-class (:background ,vaccinium-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,vaccinium-class (:weight bold))))
   `(mode-line-highlight ((,vaccinium-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,vaccinium-class (:weight bold :foreground ,vaccinium-mode-line))))

   ;; borders
   `(vertical-border ((,vaccinium-class (:foreground ,vaccinium-mode-line))))

   ;; header-line
   `(header-line ((,vaccinium-class (:inherit mode-line))))
   `(which-func ((,vaccinium-class (:weight bold :foreground ,vaccinium-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,vaccinium-class (:foreground ,vaccinium-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,vaccinium-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,vaccinium-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-bell))))
   `(show-paren-mismatch ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,vaccinium-class (:background ,vaccinium-error))))

   ;; links
   `(link ((,vaccinium-class (:foreground ,vaccinium-yellow :underline t :weight bold))))
   `(link-visited ((,vaccinium-class (:foreground ,vaccinium-orange :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,vaccinium-class (:foreground ,vaccinium-blue :weight medium))))
   `(outline-2 ((,vaccinium-class (:foreground ,vaccinium-red :weight medium))))
   `(outline-3 ((,vaccinium-class (:foreground ,vaccinium-purple :weight medium))))
   `(outline-4 ((,vaccinium-class (:foreground ,vaccinium-light-green :weight medium))))
   `(outline-5 ((,vaccinium-class (:inherit outline-1))))
   `(outline-6 ((,vaccinium-class (:inherit outline-2))))
   `(outline-7 ((,vaccinium-class (:inherit outline-3))))
   `(outline-8 ((,vaccinium-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,vaccinium-class (:foreground ,vaccinium-cyan))))
   `(escape-glyph ((,vaccinium-class (:inherit homoglyph))))
   `(nobreak-space ((,vaccinium-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,vaccinium-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,vaccinium-class (:background ,vaccinium-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,vaccinium-class (:foreground ,vaccinium-cyan :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-diff-header :extend t))))
   `(diff-file-header ((,vaccinium-class (:weight bold :foreground ,vaccinium-white :background ,vaccinium-diff-file-header :extend t))))
   `(diff-context ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,vaccinium-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,vaccinium-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,vaccinium-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,vaccinium-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,vaccinium-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,vaccinium-class (:foreground ,vaccinium-warning :weight bold))))
   `(completions-first-difference ((,vaccinium-class (:foreground ,vaccinium-error :weight bold))))

   ;; org-faces
   `(org-todo ((,vaccinium-class (:foreground ,vaccinium-error :weight bold))))
   `(org-done ((,vaccinium-class (:foreground ,vaccinium-success :weight bold))))
   `(org-hide ((,vaccinium-class (:foreground ,vaccinium-bg))))
   `(org-table ((,vaccinium-class (:foreground ,vaccinium-blueberry))))
   `(org-table-header ((,vaccinium-class (:inverse-video t :inherit org-table))))
   `(org-date ((,vaccinium-class (:foreground ,vaccinium-purple-pink))))
   `(org-date-selected ((,vaccinium-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,vaccinium-class (:foreground ,vaccinium-square-a-2))))
   `(org-headline-done ((,vaccinium-class (:foreground ,vaccinium-square-a-1))))
   `(org-document-title ((,vaccinium-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,vaccinium-class (:inherit shadow))))
   `(org-meta-line ((,vaccinium-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,vaccinium-class (:foreground ,vaccinium-tab-3))))
   `(window-divider-first-pixel ((,vaccinium-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,vaccinium-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-square-a-2))))
   `(isearch-fail ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-error))))
   `(lazy-highlight ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-square-a-3))))
   `(isearch-group-1 ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-square-a-0))))
   `(isearch-group-2 ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,vaccinium-class (:inherit isearch))))
   `(match ((,vaccinium-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-mode-line :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-mode-line :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,vaccinium-class (:foreground ,vaccinium-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,vaccinium-class (:foreground ,vaccinium-bell :inherit custom-button-unraised))))
   `(custom-comment ((,vaccinium-class (:background ,vaccinium-tab-3))))
   `(custom-variable-tag ((,vaccinium-class (:weight bold :foreground ,vaccinium-tab-2))))
   `(custom-group-tag ((,vaccinium-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,vaccinium-class (:foreground ,vaccinium-fl-keyword))))

   ;; widget
   `(widget-field ((,vaccinium-class (:background ,vaccinium-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,vaccinium-class (:background ,vaccinium-tab-3))))

   ;; package
   `(package-status-dependency ((,vaccinium-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,vaccinium-class (:weight bold :foreground ,vaccinium-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,vaccinium-class (:weight bold :foreground ,vaccinium-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,vaccinium-class (:foreground ,vaccinium-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,vaccinium-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,vaccinium-class (:underline (:style wave :color ,vaccinium-success)))))
   `(flymake-warning ((,vaccinium-class (:underline (:style wave :color ,vaccinium-warning)))))
   `(flymake-error ((,vaccinium-class (:underline (:style wave :color ,vaccinium-error)))))

   ;; eww
   `(eww-valid-certificate ((,vaccinium-class (:weight bold :foreground ,vaccinium-success))))
   `(eww-invalid-certificate ((,vaccinium-class (:weight bold :foreground ,vaccinium-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,vaccinium-class (:foreground ,vaccinium-fg-1 :background ,vaccinium-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,vaccinium-class (:foreground ,vaccinium-fg-1 :background ,vaccinium-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,vaccinium-class (:foreground ,vaccinium-fg-1 :background ,vaccinium-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,vaccinium-class (:foreground ,vaccinium-fg-1 :background ,vaccinium-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,vaccinium-class (:foreground ,vaccinium-fg-1 :background ,vaccinium-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,vaccinium-class (:foreground ,vaccinium-fg-1 :background ,vaccinium-tab-3 :box (:line-width 1 :color ,vaccinium-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,vaccinium-class (:foreground ,vaccinium-light-purple))))
   `(elfeed-search-date-face ((,vaccinium-class (:foreground ,vaccinium-steel-blue))))
   `(elfeed-search-feed-face ((,vaccinium-class (:foreground ,vaccinium-blueberry))))
   `(elfeed-search-title-face ((,vaccinium-class (:foreground ,vaccinium-orange))))
   `(elfeed-search-unread-title-face ((,vaccinium-class (:weight bold :foreground ,vaccinium-light-green))))
   `(elfeed-search-filter-face ((,vaccinium-class (:weight bold :foreground ,vaccinium-deep-blue))))
   `(elfeed-search-last-update-face ((,vaccinium-class (:weight bold :foreground ,vaccinium-deep-blue))))
   `(elfeed-search-unread-count-face ((,vaccinium-class (:weight bold :foreground ,vaccinium-deep-blue))))

   `(elfeed-show-header-face ((,vaccinium-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,vaccinium-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,vaccinium-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,vaccinium-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,vaccinium-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,vaccinium-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,vaccinium-class (:foreground ,vaccinium-deep-blue :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,vaccinium-class (:foreground ,vaccinium-deep-blue :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,vaccinium-class (:foreground ,vaccinium-yellow :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,vaccinium-class (:foreground ,vaccinium-yellow :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,vaccinium-class (:foreground ,vaccinium-yellow :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,vaccinium-class (:foreground ,vaccinium-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,vaccinium-class (:foreground ,vaccinium-fg-1 :background ,vaccinium-bg))))
   `(corfu-current ((,vaccinium-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,vaccinium-class (:background ,vaccinium-shadow))))
   `(corfu-border ((,vaccinium-class (:background ,vaccinium-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,vaccinium-class (:foreground ,vaccinium-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,vaccinium-class (:foreground ,vaccinium-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,vaccinium-class (:foreground ,vaccinium-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,vaccinium-class (:foreground ,vaccinium-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,vaccinium-class (:foreground ,vaccinium-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,vaccinium-class (:foreground ,vaccinium-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,vaccinium-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,vaccinium-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,vaccinium-class (:inherit success))))
   `(envrc-mode-line-none-face ((,vaccinium-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,vaccinium-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,vaccinium-class (:weight bold :background ,vaccinium-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-fl-builtin :inherit region))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,vaccinium-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-vaccinium)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-vaccinium-theme.el ends here
