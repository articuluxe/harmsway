;;; guava-themes-rhododendron-theme.el --- A theme inspired by the azalea tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Jan 19, 2026
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
;; A theme inspired by the azalea tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-rhododendron "A theme inspired by the azalea tree colors.")

(let* (
      (rhododendron-class '((class color) (min-colors 257)))
      (rhododendron-black                     "#000000")
      (rhododendron-white                     "#FFFFFF")

      (rhododendron-forest-green              "#228b22")
      (rhododendron-deep-green                "#3e7411")

      (rhododendron-red                       "#c00353")
      (rhododendron-red-orange                "#cd605f")
      (rhododendron-light-orange              "#ff967c")
      (rhododendron-bright-orange             "#ff5b4c")
      (rhododendron-cream                     "#fcbdb2")

      (rhododendron-blue                      "#3c3cee")
      (rhododendron-deep-blue                 "#3e3d8b")

      (rhododendron-bright-pink               "#fd3aae")
      (rhododendron-alt-bright-pink           "#e78deb")
      (rhododendron-deep-pink                 "#c00e88")

      (rhododendron-light-purple              "#e0bde7")
      (rhododendron-purple                    "#826ec0")
      (rhododendron-purple-pink               "#ad20f0")
      (rhododendron-purple-blue               "#5346cc")
      (rhododendron-purple-red                "#a8206f")
      (rhododendron-dark-purple-red           "#7d2061")

      (rhododendron-fg-1                      "#000000")
      (rhododendron-fg-2                      "#3c3c3c")
      (rhododendron-bg                        "#e8c7e3")
      (rhododendron-highlight                 "#d4b3cf")
      (rhododendron-shadow                    "#898989")
      (rhododendron-tooltip                   "#ffffe0")
      (rhododendron-region-1                  "#e78deb")
      (rhododendron-region-2                  "#64d264")

      (rhododendron-error                     "#FF0000")
      (rhododendron-warning                   "#F68511")
      (rhododendron-success                   "#29b425")
      (rhododendron-change                    "#3c3cee")

      (rhododendron-mode-line                 "#c00353")
      (rhododendron-mode-line-inactive        "#fd3aae")
      (rhododendron-mode-line-modified        "#3AAFFD")
      (rhododendron-bell                      "#03C013")

      (rhododendron-tab-1                     rhododendron-mode-line)
      (rhododendron-tab-2                     rhododendron-mode-line-inactive)
      (rhododendron-tab-3                     rhododendron-alt-bright-pink)

      (rhododendron-fl-comment                rhododendron-deep-green)
      (rhododendron-fl-string                 rhododendron-purple-red)
      (rhododendron-fl-keyword                rhododendron-deep-blue)
      (rhododendron-fl-builtin                rhododendron-red)
      (rhododendron-fl-type                   rhododendron-forest-green)
      (rhododendron-fl-function-name          rhododendron-deep-pink)
      (rhododendron-fl-variable-name          rhododendron-bright-orange)
      (rhododendron-fl-constant               rhododendron-bright-pink)
      (rhododendron-fl-warning                rhododendron-warning)
      (rhododendron-fl-punctuation            rhododendron-fg-2)
      (rhododendron-fl-negation-char          rhododendron-red-orange)

      (rhododendron-diff-added                "#c8f0c8")
      (rhododendron-diff-removed              "#f0c8c8")
      (rhododendron-diff-refine-added         "#78f078")
      (rhododendron-diff-refine-removed       "#f07878")
      (rhododendron-diff-header               "#b4b4b4")
      (rhododendron-diff-file-header          "#8c8c8c")
      (rhododendron-diff-context              "#dcdcdc")
      (rhododendron-smerge-base               "#C8C8F0")

      (rhododendron-square-a-0                "#af50c8")
      (rhododendron-square-a-1                "#28a03c")
      (rhododendron-square-a-2                "#ff6400")
      (rhododendron-square-a-3                "#3c82ff")

      (rhododendron-complementary-a-0         "#4680DE")
      (rhododendron-complementary-a-1         "#DE4680"))

  (custom-theme-set-faces
   'guava-themes-rhododendron

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,rhododendron-class (:foreground ,rhododendron-fg-1 :background ,rhododendron-bg))))

   ;; error, warning, success
   `(error ((,rhododendron-class (:foreground ,rhododendron-error :weight bold))))
   `(warning ((,rhododendron-class (:foreground ,rhododendron-warning :weight bold))))
   `(success ((,rhododendron-class (:foreground ,rhododendron-success :weight bold))))

   ;; highlight
   `(highlight ((,rhododendron-class (:background ,rhododendron-highlight))))

   ;; shadow
   `(shadow ((,rhododendron-class (:foreground ,rhododendron-shadow))))

   ;; tooltip
   `(tooltip ((,rhododendron-class (:weight bold :foreground ,rhododendron-black :background ,rhododendron-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,rhododendron-class (:background ,rhododendron-region-1 :extend t))))
   `(secondary-selection ((,rhododendron-class (:background ,rhododendron-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,rhododendron-class (:foreground ,rhododendron-fl-comment :weight medium))))
   `(font-lock-string-face ((,rhododendron-class (:foreground ,rhododendron-fl-string :weight medium))))
   `(font-lock-keyword-face ((,rhododendron-class (:foreground ,rhododendron-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,rhododendron-class (:foreground ,rhododendron-fl-builtin :weight medium))))
   `(font-lock-type-face ((,rhododendron-class (:foreground ,rhododendron-fl-type :weight medium))))
   `(font-lock-function-name-face ((,rhododendron-class (:foreground ,rhododendron-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,rhododendron-class (:foreground ,rhododendron-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,rhododendron-class (:foreground ,rhododendron-fl-constant :weight medium))))
   `(font-lock-warning-face ((,rhododendron-class (:foreground ,rhododendron-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,rhododendron-class (:foreground ,rhododendron-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,rhododendron-class (:foreground ,rhododendron-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-mode-line))))

   ;; fringe
   `(fringe ((,rhododendron-class (:foreground ,rhododendron-red :background ,rhododendron-bg))))
   `(diff-hl-change ((,rhododendron-class (:foreground ,rhododendron-change :background ,rhododendron-change))))
   `(diff-hl-insert ((,rhododendron-class (:foreground ,rhododendron-success :background ,rhododendron-success))))
   `(diff-hl-delete ((,rhododendron-class (:foreground ,rhododendron-error :background ,rhododendron-error))))

   ;; line-number
   `(line-number ((,rhododendron-class (:foreground ,rhododendron-fg-1 :inherit default))))
   `(line-number-current-line ((,rhododendron-class (:foreground ,rhododendron-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,rhododendron-class (:background ,rhododendron-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,rhododendron-class (:background ,rhododendron-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-mode-line
                                                  :box ,(if guava-themes-boxed-mode-line
                                                            '(:line-width -1 :style released-button)
                                                          'unspecified)))))
   `(mode-line-inactive ((,rhododendron-class (:background ,rhododendron-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,rhododendron-class (:weight bold))))
   `(mode-line-highlight ((,rhododendron-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,rhododendron-class (:weight bold :foreground ,rhododendron-mode-line))))

   ;; borders
   `(vertical-border ((,rhododendron-class (:foreground ,rhododendron-mode-line))))

   ;; header-line
   `(header-line ((,rhododendron-class (:inherit mode-line))))
   `(which-func ((,rhododendron-class (:weight bold :foreground ,rhododendron-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,rhododendron-class (:foreground ,rhododendron-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,rhododendron-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,rhododendron-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-bell))))
   `(show-paren-mismatch ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,rhododendron-class (:background ,rhododendron-error))))

   ;; links
   `(link ((,rhododendron-class (:foreground ,rhododendron-purple-blue :underline t :weight bold))))
   `(link-visited ((,rhododendron-class (:foreground ,rhododendron-purple-pink :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,rhododendron-class (:foreground ,rhododendron-purple-red :weight medium))))
   `(outline-2 ((,rhododendron-class (:foreground ,rhododendron-deep-green :weight medium))))
   `(outline-3 ((,rhododendron-class (:foreground ,rhododendron-bright-orange :weight medium))))
   `(outline-4 ((,rhododendron-class (:foreground ,rhododendron-purple-blue :weight medium))))
   `(outline-5 ((,rhododendron-class (:inherit outline-1))))
   `(outline-6 ((,rhododendron-class (:inherit outline-2))))
   `(outline-7 ((,rhododendron-class (:inherit outline-3))))
   `(outline-8 ((,rhododendron-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,rhododendron-class (:foreground ,rhododendron-blue))))
   `(escape-glyph ((,rhododendron-class (:inherit homoglyph))))
   `(nobreak-space ((,rhododendron-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,rhododendron-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,rhododendron-class (:background ,rhododendron-blue))))

   ;; help-key-binding
   `(help-key-binding ((,rhododendron-class (:foreground ,rhododendron-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-header :extend t))))
   `(diff-file-header ((,rhododendron-class (:weight bold :foreground ,rhododendron-black :background ,rhododendron-diff-file-header :extend t))))
   `(diff-context ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,rhododendron-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,rhododendron-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,rhododendron-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,rhododendron-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,rhododendron-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,rhododendron-class (:foreground ,rhododendron-change :weight bold))))
   `(completions-first-difference ((,rhododendron-class (:foreground ,rhododendron-error :weight bold))))

   ;; org-faces
   `(org-todo ((,rhododendron-class (:foreground ,rhododendron-error :weight bold))))
   `(org-done ((,rhododendron-class (:foreground ,rhododendron-success :weight bold))))
   `(org-hide ((,rhododendron-class (:foreground ,rhododendron-bg))))
   `(org-table ((,rhododendron-class (:foreground ,rhododendron-red))))
   `(org-table-header ((,rhododendron-class (:inverse-video t :inherit org-table))))
   `(org-date ((,rhododendron-class (:foreground ,rhododendron-red-orange))))
   `(org-date-selected ((,rhododendron-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,rhododendron-class (:foreground ,rhododendron-square-a-0))))
   `(org-headline-done ((,rhododendron-class (:foreground ,rhododendron-square-a-3))))
   `(org-document-title ((,rhododendron-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,rhododendron-class (:inherit shadow))))
   `(org-meta-line ((,rhododendron-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,rhododendron-class (:foreground ,rhododendron-tab-3))))
   `(window-divider-first-pixel ((,rhododendron-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,rhododendron-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-square-a-2))))
   `(isearch-fail ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-error))))
   `(lazy-highlight ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-square-a-3))))
   `(isearch-group-1 ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-square-a-0))))
   `(isearch-group-2 ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,rhododendron-class (:inherit isearch))))
   `(match ((,rhododendron-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-mode-line :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-mode-line :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,rhododendron-class (:foreground ,rhododendron-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,rhododendron-class (:foreground ,rhododendron-bell :inherit custom-button-unraised))))
   `(custom-comment ((,rhododendron-class (:background ,rhododendron-tab-3))))
   `(custom-variable-tag ((,rhododendron-class (:weight bold :foreground ,rhododendron-tab-2))))
   `(custom-group-tag ((,rhododendron-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,rhododendron-class (:foreground ,rhododendron-fl-keyword))))

   ;; widget
   `(widget-field ((,rhododendron-class (:background ,rhododendron-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,rhododendron-class (:background ,rhododendron-tab-3))))

   ;; package
   `(package-status-dependency ((,rhododendron-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,rhododendron-class (:weight bold :foreground ,rhododendron-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,rhododendron-class (:weight bold :foreground ,rhododendron-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,rhododendron-class (:foreground ,rhododendron-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,rhododendron-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,rhododendron-class (:underline (:style wave :color ,rhododendron-success)))))
   `(flymake-warning ((,rhododendron-class (:underline (:style wave :color ,rhododendron-warning)))))
   `(flymake-error ((,rhododendron-class (:underline (:style wave :color ,rhododendron-error)))))

   ;; eww
   `(eww-valid-certificate ((,rhododendron-class (:weight bold :foreground ,rhododendron-success))))
   `(eww-invalid-certificate ((,rhododendron-class (:weight bold :foreground ,rhododendron-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,rhododendron-class (:foreground ,rhododendron-fg-1 :background ,rhododendron-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,rhododendron-class (:foreground ,rhododendron-fg-1 :background ,rhododendron-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,rhododendron-class (:foreground ,rhododendron-fg-1 :background ,rhododendron-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,rhododendron-class (:foreground ,rhododendron-fg-1 :background ,rhododendron-tab-3 :box (:line-width 1 :color ,rhododendron-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,rhododendron-class (:foreground ,rhododendron-dark-purple-red))))
   `(elfeed-search-date-face ((,rhododendron-class (:foreground ,rhododendron-purple-red))))
   `(elfeed-search-feed-face ((,rhododendron-class (:foreground ,rhododendron-purple-blue))))
   `(elfeed-search-title-face ((,rhododendron-class (:foreground ,rhododendron-purple))))
   `(elfeed-search-unread-title-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-deep-blue))))
   `(elfeed-search-filter-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-light-purple))))
   `(elfeed-search-last-update-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-light-purple))))
   `(elfeed-search-unread-count-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-light-purple))))

   `(elfeed-show-header-face ((,rhododendron-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,rhododendron-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,rhododendron-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,rhododendron-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,rhododendron-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,rhododendron-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,rhododendron-class (:foreground ,rhododendron-light-purple :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,rhododendron-class (:foreground ,rhododendron-light-purple :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,rhododendron-class (:foreground ,rhododendron-bright-orange :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,rhododendron-class (:foreground ,rhododendron-bright-orange :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,rhododendron-class (:foreground ,rhododendron-bright-orange :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,rhododendron-class (:foreground ,rhododendron-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,rhododendron-class (:foreground ,rhododendron-fg-1 :background ,rhododendron-bg))))
   `(corfu-current ((,rhododendron-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,rhododendron-class (:background ,rhododendron-shadow))))
   `(corfu-border ((,rhododendron-class (:background ,rhododendron-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,rhododendron-class (:foreground ,rhododendron-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,rhododendron-class (:foreground ,rhododendron-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,rhododendron-class (:foreground ,rhododendron-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,rhododendron-class (:foreground ,rhododendron-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,rhododendron-class (:foreground ,rhododendron-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,rhododendron-class (:foreground ,rhododendron-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,rhododendron-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,rhododendron-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,rhododendron-class (:inherit success))))
   `(envrc-mode-line-none-face ((,rhododendron-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,rhododendron-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,rhododendron-class (:weight bold :background ,rhododendron-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-fl-builtin :inherit region))))

   ;; keycast
   `(keycast-command ((,rhododendron-class (:weight bold))))
   `(keycast-key ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-tab-3 :box (:line-width -3 :style released-button) :inherit fixed-pitch))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,rhododendron-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-rhododendron)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-rhododendron-theme.el ends here
