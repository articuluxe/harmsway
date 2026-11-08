;;; guava-themes-lavandula-theme.el --- A theme inspired by lavender colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Oct 29, 2026
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
;; A theme inspired by lavender colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-lavandula "A theme inspired by lavender colors.")

(let* (
      (lavandula-class '((class color) (min-colors 257)))
      (lavandula-black                     "#000000")
      (lavandula-white                     "#FFFFFF")

      (lavandula-gray-black                "#626262")

      (lavandula-light-green               "#64a464")
      (lavandula-alt-light-green           "#78c278")
      (lavandula-green                     "#1e8246")
      (lavandula-deep-green                "#0a6432")

      (lavandula-light-orange              "#cd5f46")
      (lavandula-orange                    "#ff9f79")
      (lavandula-red                       "#e01e50")
      (lavandula-deep-red                  "#ae1e50")

      (lavandula-alt-light-blue            "#b1c6c0")
      (lavandula-blue                      "#1e9be0")
      (lavandula-deep-blue                 "#2862b0")
      (lavandula-light-cyan                "#00cfcf")

      (lavandula-purple                    "#7d509b")
      (lavandula-lavender                  "#8264b9")

      (lavandula-fg-1                      "#505050")
      (lavandula-fg-2                      "#a5a5a5")
      (lavandula-bg                        "#eee8d9")
      (lavandula-highlight                 "#dad4c5")
      (lavandula-shadow                    "#898989")
      (lavandula-tooltip                   "#ffffe0")
      (lavandula-region-1                  "#E4C5B1")
      (lavandula-region-2                  "#b1e4de")

      (lavandula-error                     "#FF0000")
      (lavandula-warning                   "#ebb515")
      (lavandula-success                   "#00c200")
      (lavandula-change                    "#453ee3")

      (lavandula-mode-line                 "#8061b8")
      (lavandula-mode-line-inactive        "#3a3a3a")
      (lavandula-mode-line-modified        "#ff9f79")
      (lavandula-bell                      "#6EB861")

      (lavandula-tab-1                     lavandula-mode-line)
      (lavandula-tab-2                     lavandula-mode-line-inactive)
      (lavandula-tab-3                     lavandula-gray-black)

      (lavandula-fl-comment                lavandula-light-green)
      (lavandula-fl-string                 lavandula-light-orange)
      (lavandula-fl-keyword                lavandula-red)
      (lavandula-fl-builtin                lavandula-lavender)
      (lavandula-fl-type                   lavandula-green)
      (lavandula-fl-function-name          lavandula-purple)
      (lavandula-fl-variable-name          lavandula-blue)
      (lavandula-fl-constant               lavandula-deep-blue)
      (lavandula-fl-warning                lavandula-warning)
      (lavandula-fl-punctuation            lavandula-fg-2)
      (lavandula-fl-negation-char          lavandula-deep-red)

      (lavandula-diff-added                "#c8f0c8")
      (lavandula-diff-removed              "#f0c8c8")
      (lavandula-diff-refine-added         "#78f078")
      (lavandula-diff-refine-removed       "#f07878")
      (lavandula-diff-header               "#b4b4b4")
      (lavandula-diff-file-header          "#8c8c8c")
      (lavandula-diff-context              "#dcdcdc")
      (lavandula-smerge-base               "#C8C8F0")

      (lavandula-square-a-0                "#af50b9")
      (lavandula-square-a-1                "#28a03c")
      (lavandula-square-a-2                "#f55a00")
      (lavandula-square-a-3                "#3c82ff")

      (lavandula-complementary-a-0         "#4680DE")
      (lavandula-complementary-a-1         "#DE4680"))

  (custom-theme-set-faces
   'guava-themes-lavandula

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,lavandula-class (:foreground ,lavandula-fg-1 :background ,lavandula-bg))))

   ;; error, warning, success
   `(error ((,lavandula-class (:foreground ,lavandula-error :weight bold))))
   `(warning ((,lavandula-class (:foreground ,lavandula-warning :weight bold))))
   `(success ((,lavandula-class (:foreground ,lavandula-success :weight bold))))

   ;; highlight
   `(highlight ((,lavandula-class (:background ,lavandula-highlight))))

   ;; shadow
   `(shadow ((,lavandula-class (:foreground ,lavandula-shadow))))

   ;; tooltip
   `(tooltip ((,lavandula-class (:weight bold :foreground ,lavandula-black :background ,lavandula-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,lavandula-class (:background ,lavandula-region-1 :extend t))))
   `(secondary-selection ((,lavandula-class (:background ,lavandula-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,lavandula-class (:foreground ,lavandula-fl-comment :weight medium))))
   `(font-lock-string-face ((,lavandula-class (:foreground ,lavandula-fl-string :weight medium))))
   `(font-lock-keyword-face ((,lavandula-class (:foreground ,lavandula-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,lavandula-class (:foreground ,lavandula-fl-builtin :weight medium))))
   `(font-lock-type-face ((,lavandula-class (:foreground ,lavandula-fl-type :weight medium))))
   `(font-lock-function-name-face ((,lavandula-class (:foreground ,lavandula-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,lavandula-class (:foreground ,lavandula-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,lavandula-class (:foreground ,lavandula-fl-constant :weight medium))))
   `(font-lock-warning-face ((,lavandula-class (:foreground ,lavandula-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,lavandula-class (:foreground ,lavandula-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,lavandula-class (:foreground ,lavandula-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-mode-line))))

   ;; fringe
   `(fringe ((,lavandula-class (:foreground ,lavandula-deep-blue :background ,lavandula-bg))))
   `(diff-hl-change ((,lavandula-class (:foreground ,lavandula-change :background ,lavandula-change))))
   `(diff-hl-insert ((,lavandula-class (:foreground ,lavandula-success :background ,lavandula-success))))
   `(diff-hl-delete ((,lavandula-class (:foreground ,lavandula-error :background ,lavandula-error))))

   ;; line-number
   `(line-number ((,lavandula-class (:foreground ,lavandula-fg-1 :inherit default))))
   `(line-number-current-line ((,lavandula-class (:foreground ,lavandula-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-3 :inherit line-number))))
   `(line-number-major-tick ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-2 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-mode-line
                                               :box ,(if guava-themes-boxed-mode-line
                                                         '(:line-width -1 :style released-button)
                                                       'unspecified)))))
   `(mode-line-inactive ((,lavandula-class (:background ,lavandula-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,lavandula-class (:weight bold))))
   `(mode-line-highlight ((,lavandula-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,lavandula-class (:weight bold :foreground ,lavandula-mode-line))))

   ;; borders
   `(vertical-border ((,lavandula-class (:foreground ,lavandula-mode-line))))

   ;; header-line
   `(header-line ((,lavandula-class (:inherit mode-line))))
   `(which-func ((,lavandula-class (:weight bold :foreground ,lavandula-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,lavandula-class (:foreground ,lavandula-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,lavandula-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,lavandula-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,lavandula-class (:foreground ,lavandula-black :background ,lavandula-bell))))
   `(show-paren-mismatch ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,lavandula-class (:background ,lavandula-error))))

   ;; links
   `(link ((,lavandula-class (:foreground ,lavandula-blue :underline t :weight bold))))
   `(link-visited ((,lavandula-class (:foreground ,lavandula-deep-blue :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,lavandula-class (:foreground ,lavandula-purple :weight medium))))
   `(outline-2 ((,lavandula-class (:foreground ,lavandula-deep-green :weight medium))))
   `(outline-3 ((,lavandula-class (:foreground ,lavandula-deep-red :weight medium))))
   `(outline-4 ((,lavandula-class (:foreground ,lavandula-deep-blue :weight medium))))
   `(outline-5 ((,lavandula-class (:inherit outline-1))))
   `(outline-6 ((,lavandula-class (:inherit outline-2))))
   `(outline-7 ((,lavandula-class (:inherit outline-3))))
   `(outline-8 ((,lavandula-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,lavandula-class (:foreground ,lavandula-light-cyan))))
   `(escape-glyph ((,lavandula-class (:inherit homoglyph))))
   `(nobreak-space ((,lavandula-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,lavandula-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,lavandula-class (:background ,lavandula-light-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,lavandula-class (:foreground ,lavandula-deep-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,lavandula-class (:foreground ,lavandula-black :background ,lavandula-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,lavandula-class (:foreground ,lavandula-black :background ,lavandula-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,lavandula-class (:foreground ,lavandula-black :background ,lavandula-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,lavandula-class (:foreground ,lavandula-black :background ,lavandula-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,lavandula-class (:foreground ,lavandula-black :background ,lavandula-diff-header :extend t))))
   `(diff-file-header ((,lavandula-class (:weight bold :foreground ,lavandula-black :background ,lavandula-diff-file-header :extend t))))
   `(diff-context ((,lavandula-class (:foreground ,lavandula-black :background ,lavandula-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,lavandula-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,lavandula-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,lavandula-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,lavandula-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,lavandula-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,lavandula-class (:foreground ,lavandula-black :background ,lavandula-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,lavandula-class (:foreground ,lavandula-change :weight bold))))
   `(completions-first-difference ((,lavandula-class (:foreground ,lavandula-error :weight bold))))

   ;; org-faces
   `(org-todo ((,lavandula-class (:foreground ,lavandula-error :weight bold))))
   `(org-done ((,lavandula-class (:foreground ,lavandula-success :weight bold))))
   `(org-hide ((,lavandula-class (:foreground ,lavandula-bg))))
   `(org-table ((,lavandula-class (:foreground ,lavandula-lavender))))
   `(org-table-header ((,lavandula-class (:inverse-video t :inherit org-table))))
   `(org-date ((,lavandula-class (:foreground ,lavandula-red))))
   `(org-date-selected ((,lavandula-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,lavandula-class (:foreground ,lavandula-square-a-2))))
   `(org-headline-done ((,lavandula-class (:foreground ,lavandula-square-a-1))))
   `(org-document-title ((,lavandula-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,lavandula-class (:inherit shadow))))
   `(org-meta-line ((,lavandula-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,lavandula-class (:foreground ,lavandula-tab-3))))
   `(window-divider-first-pixel ((,lavandula-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,lavandula-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-square-a-2))))
   `(isearch-fail ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-error))))
   `(lazy-highlight ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-square-a-3))))
   `(isearch-group-1 ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-square-a-0))))
   `(isearch-group-2 ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,lavandula-class (:inherit isearch))))
   `(match ((,lavandula-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-mode-line :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-mode-line :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,lavandula-class (:foreground ,lavandula-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,lavandula-class (:foreground ,lavandula-bell :inherit custom-button-unraised))))
   `(custom-comment ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-3))))
   `(custom-variable-tag ((,lavandula-class (:weight bold :foreground ,lavandula-tab-2))))
   `(custom-group-tag ((,lavandula-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,lavandula-class (:foreground ,lavandula-fl-keyword))))

   ;; widget
   `(widget-field ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-3))))

   ;; package
   `(package-status-dependency ((,lavandula-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,lavandula-class (:weight bold :foreground ,lavandula-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,lavandula-class (:weight bold :foreground ,lavandula-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,lavandula-class (:foreground ,lavandula-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,lavandula-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,lavandula-class (:underline (:style wave :color ,lavandula-success)))))
   `(flymake-warning ((,lavandula-class (:underline (:style wave :color ,lavandula-warning)))))
   `(flymake-error ((,lavandula-class (:underline (:style wave :color ,lavandula-error)))))

   ;; eww
   `(eww-valid-certificate ((,lavandula-class (:weight bold :foreground ,lavandula-success))))
   `(eww-invalid-certificate ((,lavandula-class (:weight bold :foreground ,lavandula-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-tab-3 :box (:line-width 1 :color ,lavandula-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,lavandula-class (:foreground ,lavandula-blue))))
   `(elfeed-search-date-face ((,lavandula-class (:foreground ,lavandula-deep-blue))))
   `(elfeed-search-feed-face ((,lavandula-class (:foreground ,lavandula-purple))))
   `(elfeed-search-title-face ((,lavandula-class (:foreground ,lavandula-gray-black))))
   `(elfeed-search-unread-title-face ((,lavandula-class (:weight bold :foreground ,lavandula-green))))
   `(elfeed-search-filter-face ((,lavandula-class (:weight bold :foreground ,lavandula-orange))))
   `(elfeed-search-last-update-face ((,lavandula-class (:weight bold :foreground ,lavandula-orange))))
   `(elfeed-search-unread-count-face ((,lavandula-class (:weight bold :foreground ,lavandula-orange))))

   `(elfeed-show-header-face ((,lavandula-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,lavandula-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,lavandula-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,lavandula-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,lavandula-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,lavandula-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,lavandula-class (:foreground ,lavandula-alt-light-green :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,lavandula-class (:foreground ,lavandula-alt-light-green :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,lavandula-class (:foreground ,lavandula-alt-light-blue :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,lavandula-class (:foreground ,lavandula-alt-light-blue :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,lavandula-class (:foreground ,lavandula-alt-light-blue :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,lavandula-class (:foreground ,lavandula-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,lavandula-class (:foreground ,lavandula-fg-1 :background ,lavandula-bg))))
   `(corfu-current ((,lavandula-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,lavandula-class (:background ,lavandula-shadow))))
   `(corfu-border ((,lavandula-class (:background ,lavandula-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,lavandula-class (:foreground ,lavandula-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,lavandula-class (:foreground ,lavandula-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,lavandula-class (:foreground ,lavandula-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,lavandula-class (:foreground ,lavandula-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,lavandula-class (:foreground ,lavandula-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,lavandula-class (:foreground ,lavandula-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,lavandula-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,lavandula-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,lavandula-class (:inherit success))))
   `(envrc-mode-line-none-face ((,lavandula-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,lavandula-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,lavandula-class (:weight bold :background ,lavandula-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,lavandula-class (:foreground ,lavandula-white :background ,lavandula-fl-builtin :inherit region))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,lavandula-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-lavandula)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-lavandula-theme.el ends here
