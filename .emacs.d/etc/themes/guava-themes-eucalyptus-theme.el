;;; guava-themes-eucalyptus-theme.el --- A theme inspired by the eucalyptus tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Jun 12, 2026
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
;; A theme inspired by the eucalyptus tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-eucalyptus "A theme inspired by the eucalyptus tree colors.")

(let* (
      (eucalyptus-class '((class color) (min-colors 257)))
      (eucalyptus-black                     "#000000")
      (eucalyptus-white                     "#FFFFFF")

      (eucalyptus-light-gray                "#9196a5")
      (eucalyptus-gray                      "#646464")
      (eucalyptus-gray-green                "#505a46")

      (eucalyptus-green                     "#465f32")
      (eucalyptus-alt-green                 "#307d0f")

      (eucalyptus-red                       "#82312e")
      (eucalyptus-deep-red                  "#693c50")

      (eucalyptus-light-blue                "#2d8296")
      (eucalyptus-blue                      "#2327dc")
      (eucalyptus-deep-blue                 "#46466e")
      (eucalyptus-cyan                      "#00ffff")
      (eucalyptus-alt-cyan                  "#008cd2")

      (eucalyptus-purple-red                "#874164")

      (eucalyptus-fg-1                      "#2d3241")
      (eucalyptus-fg-2                      "#696e7d")
      (eucalyptus-bg                        "#a5aa96")
      (eucalyptus-highlight                 "#9ba08c")
      (eucalyptus-shadow                    "#717171")
      (eucalyptus-tooltip                   "#ffffe0")
      (eucalyptus-region-1                  "#e1e696")
      (eucalyptus-region-2                  "#96B9E6")

      (eucalyptus-error                     "#e10000")
      (eucalyptus-warning                   "#f6c911")
      (eucalyptus-success                   "#1ea01e")
      (eucalyptus-change                    "#2327dc")

      (eucalyptus-mode-line                 "#373c4b")
      (eucalyptus-mode-line-inactive        "#737887")
      (eucalyptus-mode-line-modified        "#aa5956")
      (eucalyptus-bell                      "#5a5f4b")

      (eucalyptus-tab-1                     eucalyptus-mode-line)
      (eucalyptus-tab-2                     eucalyptus-mode-line-inactive)
      (eucalyptus-tab-3                     eucalyptus-light-gray)

      (eucalyptus-fl-comment                eucalyptus-green)
      (eucalyptus-fl-string                 eucalyptus-purple-red)
      (eucalyptus-fl-keyword                eucalyptus-red)
      (eucalyptus-fl-builtin                eucalyptus-deep-blue)
      (eucalyptus-fl-type                   eucalyptus-alt-green)
      (eucalyptus-fl-function-name          eucalyptus-gray)
      (eucalyptus-fl-variable-name          eucalyptus-gray-green)
      (eucalyptus-fl-constant               eucalyptus-light-blue)
      (eucalyptus-fl-warning                eucalyptus-warning)
      (eucalyptus-fl-punctuation            eucalyptus-fg-2)
      (eucalyptus-fl-negation-char          eucalyptus-alt-cyan)

      (eucalyptus-diff-added                "#c8f0c8")
      (eucalyptus-diff-removed              "#f0c8c8")
      (eucalyptus-diff-refine-added         "#78f078")
      (eucalyptus-diff-refine-removed       "#f07878")
      (eucalyptus-diff-header               "#b4b4b4")
      (eucalyptus-diff-file-header          "#8c8c8c")
      (eucalyptus-diff-context              "#dcdcdc")
      (eucalyptus-smerge-base               "#C8C8F0")

      (eucalyptus-square-a-0                "#af37b9")
      (eucalyptus-square-a-1                "#147828")
      (eucalyptus-square-a-2                "#b45a00")
      (eucalyptus-square-a-3                "#3c82e6")

      (eucalyptus-complementary-a-0         "#4680DE")
      (eucalyptus-complementary-a-1         "#DE4680"))

  (custom-theme-set-faces
   'guava-themes-eucalyptus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,eucalyptus-class (:foreground ,eucalyptus-fg-1 :background ,eucalyptus-bg))))

   ;; error, warning, success
   `(error ((,eucalyptus-class (:foreground ,eucalyptus-error :weight bold))))
   `(warning ((,eucalyptus-class (:foreground ,eucalyptus-warning :weight bold))))
   `(success ((,eucalyptus-class (:foreground ,eucalyptus-success :weight bold))))

   ;; highlight
   `(highlight ((,eucalyptus-class (:background ,eucalyptus-highlight))))

   ;; shadow
   `(shadow ((,eucalyptus-class (:foreground ,eucalyptus-shadow))))

   ;; tooltip
   `(tooltip ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-black :background ,eucalyptus-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,eucalyptus-class (:background ,eucalyptus-region-1 :extend t))))
   `(secondary-selection ((,eucalyptus-class (:background ,eucalyptus-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-comment :weight medium))))
   `(font-lock-string-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,eucalyptus-class (:foreground ,eucalyptus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-mode-line))))

   ;; fringe
   `(fringe ((,eucalyptus-class (:foreground ,eucalyptus-blue :background ,eucalyptus-bg))))
   `(diff-hl-change ((,eucalyptus-class (:foreground ,eucalyptus-change :background ,eucalyptus-change))))
   `(diff-hl-insert ((,eucalyptus-class (:foreground ,eucalyptus-success :background ,eucalyptus-success))))
   `(diff-hl-delete ((,eucalyptus-class (:foreground ,eucalyptus-error :background ,eucalyptus-error))))

   ;; line-number
   `(line-number ((,eucalyptus-class (:foreground ,eucalyptus-fg-1 :inherit default))))
   `(line-number-current-line ((,eucalyptus-class (:foreground ,eucalyptus-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,eucalyptus-class (:background ,eucalyptus-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,eucalyptus-class (:background ,eucalyptus-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-mode-line
                                                :box ,(if guava-themes-boxed-mode-line
                                                          '(:line-width -1 :style released-button)
                                                        'unspecified)))))
   `(mode-line-inactive ((,eucalyptus-class (:background ,eucalyptus-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,eucalyptus-class (:weight bold))))
   `(mode-line-highlight ((,eucalyptus-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-mode-line))))

   ;; borders
   `(vertical-border ((,eucalyptus-class (:foreground ,eucalyptus-mode-line))))

   ;; header-line
   `(header-line ((,eucalyptus-class (:inherit mode-line))))
   `(which-func ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,eucalyptus-class (:foreground ,eucalyptus-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,eucalyptus-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,eucalyptus-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-bell))))
   `(show-paren-mismatch ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,eucalyptus-class (:background ,eucalyptus-error))))

   ;; links
   `(link ((,eucalyptus-class (:foreground ,eucalyptus-light-blue :underline t :weight bold))))
   `(link-visited ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue :weight medium))))
   `(outline-2 ((,eucalyptus-class (:foreground ,eucalyptus-gray :weight medium))))
   `(outline-3 ((,eucalyptus-class (:foreground ,eucalyptus-red :weight medium))))
   `(outline-4 ((,eucalyptus-class (:foreground ,eucalyptus-gray-green :weight medium))))
   `(outline-5 ((,eucalyptus-class (:inherit outline-1))))
   `(outline-6 ((,eucalyptus-class (:inherit outline-2))))
   `(outline-7 ((,eucalyptus-class (:inherit outline-3))))
   `(outline-8 ((,eucalyptus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,eucalyptus-class (:foreground ,eucalyptus-cyan))))
   `(escape-glyph ((,eucalyptus-class (:inherit homoglyph))))
   `(nobreak-space ((,eucalyptus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,eucalyptus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,eucalyptus-class (:background ,eucalyptus-alt-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-header :extend t))))
   `(diff-file-header ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-black :background ,eucalyptus-diff-file-header :extend t))))
   `(diff-context ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,eucalyptus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,eucalyptus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,eucalyptus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,eucalyptus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,eucalyptus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,eucalyptus-class (:foreground ,eucalyptus-black :background ,eucalyptus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,eucalyptus-class (:foreground ,eucalyptus-change :weight bold))))
   `(completions-first-difference ((,eucalyptus-class (:foreground ,eucalyptus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,eucalyptus-class (:foreground ,eucalyptus-error :weight bold))))
   `(org-done ((,eucalyptus-class (:foreground ,eucalyptus-success :weight bold))))
   `(org-hide ((,eucalyptus-class (:foreground ,eucalyptus-bg))))
   `(org-table ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue))))
   `(org-table-header ((,eucalyptus-class (:inverse-video t :inherit org-table))))
   `(org-date ((,eucalyptus-class (:foreground ,eucalyptus-purple-red))))
   `(org-date-selected ((,eucalyptus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,eucalyptus-class (:foreground ,eucalyptus-square-a-2))))
   `(org-headline-done ((,eucalyptus-class (:foreground ,eucalyptus-square-a-1))))
   `(org-document-title ((,eucalyptus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,eucalyptus-class (:inherit shadow))))
   `(org-meta-line ((,eucalyptus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,eucalyptus-class (:foreground ,eucalyptus-tab-3))))
   `(window-divider-first-pixel ((,eucalyptus-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,eucalyptus-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-square-a-2))))
   `(isearch-fail ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-error))))
   `(lazy-highlight ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-square-a-3))))
   `(isearch-group-1 ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-square-a-0))))
   `(isearch-group-2 ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,eucalyptus-class (:inherit isearch))))
   `(match ((,eucalyptus-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-mode-line :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-mode-line :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,eucalyptus-class (:foreground ,eucalyptus-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,eucalyptus-class (:foreground ,eucalyptus-bell :inherit custom-button-unraised))))
   `(custom-comment ((,eucalyptus-class (:background ,eucalyptus-tab-3))))
   `(custom-variable-tag ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-tab-2))))
   `(custom-group-tag ((,eucalyptus-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,eucalyptus-class (:foreground ,eucalyptus-fl-keyword))))

   ;; widget
   `(widget-field ((,eucalyptus-class (:background ,eucalyptus-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,eucalyptus-class (:background ,eucalyptus-tab-3))))

   ;; package
   `(package-status-dependency ((,eucalyptus-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,eucalyptus-class (:foreground ,eucalyptus-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,eucalyptus-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,eucalyptus-class (:underline (:style wave :color ,eucalyptus-success)))))
   `(flymake-warning ((,eucalyptus-class (:underline (:style wave :color ,eucalyptus-warning)))))
   `(flymake-error ((,eucalyptus-class (:underline (:style wave :color ,eucalyptus-error)))))

   ;; eww
   `(eww-valid-certificate ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-success))))
   `(eww-invalid-certificate ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,eucalyptus-class (:foreground ,eucalyptus-fg-1 :background ,eucalyptus-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,eucalyptus-class (:foreground ,eucalyptus-fg-1 :background ,eucalyptus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,eucalyptus-class (:foreground ,eucalyptus-fg-1 :background ,eucalyptus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,eucalyptus-class (:foreground ,eucalyptus-fg-1 :background ,eucalyptus-tab-3 :box (:line-width 1 :color ,eucalyptus-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,eucalyptus-class (:foreground ,eucalyptus-gray-green))))
   `(elfeed-search-date-face ((,eucalyptus-class (:foreground ,eucalyptus-deep-blue))))
   `(elfeed-search-feed-face ((,eucalyptus-class (:foreground ,eucalyptus-deep-red))))
   `(elfeed-search-title-face ((,eucalyptus-class (:foreground ,eucalyptus-gray))))
   `(elfeed-search-unread-title-face ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-fg-1))))
   `(elfeed-search-filter-face ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-light-blue))))
   `(elfeed-search-last-update-face ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-light-blue))))
   `(elfeed-search-unread-count-face ((,eucalyptus-class (:weight bold :foreground ,eucalyptus-light-blue))))

   `(elfeed-show-header-face ((,eucalyptus-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,eucalyptus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,eucalyptus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,eucalyptus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,eucalyptus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,eucalyptus-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,eucalyptus-class (:foreground ,eucalyptus-light-blue :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,eucalyptus-class (:foreground ,eucalyptus-light-blue :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,eucalyptus-class (:foreground ,eucalyptus-light-gray :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,eucalyptus-class (:foreground ,eucalyptus-light-gray :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,eucalyptus-class (:foreground ,eucalyptus-light-gray :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,eucalyptus-class (:foreground ,eucalyptus-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,eucalyptus-class (:foreground ,eucalyptus-fg-1 :background ,eucalyptus-bg))))
   `(corfu-current ((,eucalyptus-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,eucalyptus-class (:background ,eucalyptus-shadow))))
   `(corfu-border ((,eucalyptus-class (:background ,eucalyptus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,eucalyptus-class (:foreground ,eucalyptus-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,eucalyptus-class (:foreground ,eucalyptus-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,eucalyptus-class (:foreground ,eucalyptus-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,eucalyptus-class (:foreground ,eucalyptus-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,eucalyptus-class (:foreground ,eucalyptus-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,eucalyptus-class (:foreground ,eucalyptus-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,eucalyptus-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,eucalyptus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,eucalyptus-class (:inherit success))))
   `(envrc-mode-line-none-face ((,eucalyptus-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,eucalyptus-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,eucalyptus-class (:weight bold :background ,eucalyptus-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,eucalyptus-class (:foreground ,eucalyptus-white :background ,eucalyptus-fl-builtin :inherit region))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,eucalyptus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-eucalyptus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-eucalyptus-theme.el ends here
