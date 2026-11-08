;;; guava-themes-jacaranda-theme.el --- A theme inspired by jacaranda colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Dec 27, 2025
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
;; A theme inspired by jacaranda colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-jacaranda "A theme inspired by jacaranda colors.")

(let* (
      (jacaranda-class '((class color) (min-colors 257)))
      (jacaranda-black                     "#000000")
      (jacaranda-white                     "#FFFFFF")

      (jacaranda-light-green               "#52aa63")
      (jacaranda-green                     "#8ec654")
      (jacaranda-deep-green                "#267a63")
      (jacaranda-oceanic-green             "#10a575")
      (jacaranda-gray-green                "#646e64")

      (jacaranda-orange                    "#ff9f79")
      (jacaranda-red                       "#af2a36")

      (jacaranda-blue                      "#4534e3")
      (jacaranda-deep-blue                 "#655db0")
      (jacaranda-antarctic-blue            "#8cb2e6")
      (jacaranda-light-cyan                "#60a1ba")
      (jacaranda-cyan                      "#00778b")

      (jacaranda-light-purple              "#be87e6")
      (jacaranda-purple                    "#aa69e6")
      (jacaranda-deep-purple               "#640cbe")
      (jacaranda-purple-red                "#8b2252")

      (jacaranda-fg-1                      "#000000")
      (jacaranda-fg-2                      "#3c3c3c")
      (jacaranda-bg                        "#e9d9f9")
      (jacaranda-highlight                 "#d5c5e5")
      (jacaranda-shadow                    "#898989")
      (jacaranda-tooltip                   "#ffffe0")
      (jacaranda-region-1                  "#be87e6")
      (jacaranda-region-2                  "#ff9f79")

      (jacaranda-error                     "#FF0000")
      (jacaranda-warning                   "#ebb515")
      (jacaranda-success                   "#00c200")
      (jacaranda-change                    "#4534e3")

      (jacaranda-mode-line                 "#655db0")
      (jacaranda-mode-line-inactive        "#aa69e6")
      (jacaranda-mode-line-modified        "#ff9f79")
      (jacaranda-bell                      "#7EB05D")

      (jacaranda-tab-1                     jacaranda-mode-line)
      (jacaranda-tab-2                     jacaranda-mode-line-inactive)
      (jacaranda-tab-3                     jacaranda-light-purple)

      (jacaranda-fl-comment                jacaranda-deep-green)
      (jacaranda-fl-string                 jacaranda-purple-red)
      (jacaranda-fl-keyword                jacaranda-deep-purple)
      (jacaranda-fl-builtin                jacaranda-deep-blue)
      (jacaranda-fl-type                   jacaranda-cyan)
      (jacaranda-fl-function-name          jacaranda-blue)
      (jacaranda-fl-variable-name          jacaranda-light-cyan)
      (jacaranda-fl-constant               jacaranda-oceanic-green)
      (jacaranda-fl-warning                jacaranda-warning)
      (jacaranda-fl-punctuation            jacaranda-fg-2)
      (jacaranda-fl-negation-char          jacaranda-red)

      (jacaranda-diff-added                "#c8f0c8")
      (jacaranda-diff-removed              "#f0c8c8")
      (jacaranda-diff-refine-added         "#78f078")
      (jacaranda-diff-refine-removed       "#f07878")
      (jacaranda-diff-header               "#b4b4b4")
      (jacaranda-diff-file-header          "#8c8c8c")
      (jacaranda-diff-context              "#dcdcdc")
      (jacaranda-smerge-base               "#C8C8F0")

      (jacaranda-square-a-0                "#af50b9")
      (jacaranda-square-a-1                "#28a03c")
      (jacaranda-square-a-2                "#ff6400")
      (jacaranda-square-a-3                "#3c82ff")

      (jacaranda-complementary-a-0         "#4680DE")
      (jacaranda-complementary-a-1         "#DE4680"))

  (custom-theme-set-faces
   'guava-themes-jacaranda

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,jacaranda-class (:foreground ,jacaranda-fg-1 :background ,jacaranda-bg))))

   ;; error, warning, success
   `(error ((,jacaranda-class (:foreground ,jacaranda-error :weight bold))))
   `(warning ((,jacaranda-class (:foreground ,jacaranda-warning :weight bold))))
   `(success ((,jacaranda-class (:foreground ,jacaranda-success :weight bold))))

   ;; highlight
   `(highlight ((,jacaranda-class (:background ,jacaranda-highlight))))

   ;; shadow
   `(shadow ((,jacaranda-class (:foreground ,jacaranda-shadow))))

   ;; tooltip
   `(tooltip ((,jacaranda-class (:weight bold :foreground ,jacaranda-black :background ,jacaranda-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,jacaranda-class (:background ,jacaranda-region-1 :extend t))))
   `(secondary-selection ((,jacaranda-class (:background ,jacaranda-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,jacaranda-class (:foreground ,jacaranda-fl-comment :weight medium))))
   `(font-lock-string-face ((,jacaranda-class (:foreground ,jacaranda-fl-string :weight medium))))
   `(font-lock-keyword-face ((,jacaranda-class (:foreground ,jacaranda-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,jacaranda-class (:foreground ,jacaranda-fl-builtin :weight medium))))
   `(font-lock-type-face ((,jacaranda-class (:foreground ,jacaranda-fl-type :weight medium))))
   `(font-lock-function-name-face ((,jacaranda-class (:foreground ,jacaranda-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,jacaranda-class (:foreground ,jacaranda-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,jacaranda-class (:foreground ,jacaranda-fl-constant :weight medium))))
   `(font-lock-warning-face ((,jacaranda-class (:foreground ,jacaranda-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,jacaranda-class (:foreground ,jacaranda-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,jacaranda-class (:foreground ,jacaranda-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-mode-line))))

   ;; fringe
   `(fringe ((,jacaranda-class (:foreground ,jacaranda-purple-red :background ,jacaranda-bg))))
   `(diff-hl-change ((,jacaranda-class (:foreground ,jacaranda-change :background ,jacaranda-change))))
   `(diff-hl-insert ((,jacaranda-class (:foreground ,jacaranda-success :background ,jacaranda-success))))
   `(diff-hl-delete ((,jacaranda-class (:foreground ,jacaranda-error :background ,jacaranda-error))))

   ;; line-number
   `(line-number ((,jacaranda-class (:foreground ,jacaranda-fg-1 :inherit default))))
   `(line-number-current-line ((,jacaranda-class (:foreground ,jacaranda-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,jacaranda-class (:background ,jacaranda-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,jacaranda-class (:background ,jacaranda-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-mode-line
                                               :box ,(if guava-themes-boxed-mode-line
                                                         '(:line-width -1 :style released-button)
                                                       'unspecified)))))
   `(mode-line-inactive ((,jacaranda-class (:background ,jacaranda-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,jacaranda-class (:weight bold))))
   `(mode-line-highlight ((,jacaranda-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,jacaranda-class (:weight bold :foreground ,jacaranda-mode-line))))

   ;; borders
   `(vertical-border ((,jacaranda-class (:foreground ,jacaranda-mode-line))))

   ;; header-line
   `(header-line ((,jacaranda-class (:inherit mode-line))))
   `(which-func ((,jacaranda-class (:weight bold :foreground ,jacaranda-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,jacaranda-class (:foreground ,jacaranda-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,jacaranda-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,jacaranda-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-bell))))
   `(show-paren-mismatch ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,jacaranda-class (:background ,jacaranda-error))))

   ;; links
   `(link ((,jacaranda-class (:foreground ,jacaranda-oceanic-green :underline t :weight bold))))
   `(link-visited ((,jacaranda-class (:foreground ,jacaranda-deep-green :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,jacaranda-class (:foreground ,jacaranda-purple :weight medium))))
   `(outline-2 ((,jacaranda-class (:foreground ,jacaranda-light-cyan :weight medium))))
   `(outline-3 ((,jacaranda-class (:foreground ,jacaranda-light-green :weight medium))))
   `(outline-4 ((,jacaranda-class (:foreground ,jacaranda-red :weight medium))))
   `(outline-5 ((,jacaranda-class (:inherit outline-1))))
   `(outline-6 ((,jacaranda-class (:inherit outline-2))))
   `(outline-7 ((,jacaranda-class (:inherit outline-3))))
   `(outline-8 ((,jacaranda-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,jacaranda-class (:foreground ,jacaranda-blue))))
   `(escape-glyph ((,jacaranda-class (:inherit homoglyph))))
   `(nobreak-space ((,jacaranda-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,jacaranda-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,jacaranda-class (:background ,jacaranda-light-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,jacaranda-class (:foreground ,jacaranda-deep-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-diff-header :extend t))))
   `(diff-file-header ((,jacaranda-class (:weight bold :foreground ,jacaranda-black :background ,jacaranda-diff-file-header :extend t))))
   `(diff-context ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,jacaranda-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,jacaranda-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,jacaranda-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,jacaranda-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,jacaranda-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,jacaranda-class (:foreground ,jacaranda-black :background ,jacaranda-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,jacaranda-class (:foreground ,jacaranda-change :weight bold))))
   `(completions-first-difference ((,jacaranda-class (:foreground ,jacaranda-error :weight bold))))

   ;; org-faces
   `(org-todo ((,jacaranda-class (:foreground ,jacaranda-error :weight bold))))
   `(org-done ((,jacaranda-class (:foreground ,jacaranda-success :weight bold))))
   `(org-hide ((,jacaranda-class (:foreground ,jacaranda-bg))))
   `(org-table ((,jacaranda-class (:foreground ,jacaranda-deep-blue))))
   `(org-table-header ((,jacaranda-class (:inverse-video t :inherit org-table))))
   `(org-date ((,jacaranda-class (:foreground ,jacaranda-red))))
   `(org-date-selected ((,jacaranda-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,jacaranda-class (:foreground ,jacaranda-square-a-2))))
   `(org-headline-done ((,jacaranda-class (:foreground ,jacaranda-square-a-1))))
   `(org-document-title ((,jacaranda-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,jacaranda-class (:inherit shadow))))
   `(org-meta-line ((,jacaranda-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,jacaranda-class (:foreground ,jacaranda-tab-3))))
   `(window-divider-first-pixel ((,jacaranda-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,jacaranda-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-square-a-2))))
   `(isearch-fail ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-error))))
   `(lazy-highlight ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-square-a-3))))
   `(isearch-group-1 ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-square-a-0))))
   `(isearch-group-2 ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,jacaranda-class (:inherit isearch))))
   `(match ((,jacaranda-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-mode-line :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-mode-line :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,jacaranda-class (:foreground ,jacaranda-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,jacaranda-class (:foreground ,jacaranda-bell :inherit custom-button-unraised))))
   `(custom-comment ((,jacaranda-class (:background ,jacaranda-tab-3))))
   `(custom-variable-tag ((,jacaranda-class (:weight bold :foreground ,jacaranda-tab-2))))
   `(custom-group-tag ((,jacaranda-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,jacaranda-class (:foreground ,jacaranda-fl-keyword))))

   ;; widget
   `(widget-field ((,jacaranda-class (:background ,jacaranda-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,jacaranda-class (:background ,jacaranda-tab-3))))

   ;; package
   `(package-status-dependency ((,jacaranda-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,jacaranda-class (:weight bold :foreground ,jacaranda-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,jacaranda-class (:weight bold :foreground ,jacaranda-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,jacaranda-class (:foreground ,jacaranda-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,jacaranda-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,jacaranda-class (:underline (:style wave :color ,jacaranda-success)))))
   `(flymake-warning ((,jacaranda-class (:underline (:style wave :color ,jacaranda-warning)))))
   `(flymake-error ((,jacaranda-class (:underline (:style wave :color ,jacaranda-error)))))

   ;; eww
   `(eww-valid-certificate ((,jacaranda-class (:weight bold :foreground ,jacaranda-success))))
   `(eww-invalid-certificate ((,jacaranda-class (:weight bold :foreground ,jacaranda-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,jacaranda-class (:foreground ,jacaranda-fg-1 :background ,jacaranda-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,jacaranda-class (:foreground ,jacaranda-fg-1 :background ,jacaranda-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,jacaranda-class (:foreground ,jacaranda-fg-1 :background ,jacaranda-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,jacaranda-class (:foreground ,jacaranda-fg-1 :background ,jacaranda-tab-3 :box (:line-width 1 :color ,jacaranda-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,jacaranda-class (:foreground ,jacaranda-purple-red))))
   `(elfeed-search-date-face ((,jacaranda-class (:foreground ,jacaranda-deep-blue))))
   `(elfeed-search-feed-face ((,jacaranda-class (:foreground ,jacaranda-deep-purple))))
   `(elfeed-search-title-face ((,jacaranda-class (:foreground ,jacaranda-gray-green))))
   `(elfeed-search-unread-title-face ((,jacaranda-class (:weight bold :foreground ,jacaranda-deep-green))))
   `(elfeed-search-filter-face ((,jacaranda-class (:weight bold :foreground ,jacaranda-orange))))
   `(elfeed-search-last-update-face ((,jacaranda-class (:weight bold :foreground ,jacaranda-orange))))
   `(elfeed-search-unread-count-face ((,jacaranda-class (:weight bold :foreground ,jacaranda-orange))))

   `(elfeed-show-header-face ((,jacaranda-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,jacaranda-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,jacaranda-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,jacaranda-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,jacaranda-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,jacaranda-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,jacaranda-class (:foreground ,jacaranda-light-green :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,jacaranda-class (:foreground ,jacaranda-light-green :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,jacaranda-class (:foreground ,jacaranda-antarctic-blue :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,jacaranda-class (:foreground ,jacaranda-antarctic-blue :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,jacaranda-class (:foreground ,jacaranda-antarctic-blue :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,jacaranda-class (:foreground ,jacaranda-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,jacaranda-class (:foreground ,jacaranda-fg-1 :background ,jacaranda-bg))))
   `(corfu-current ((,jacaranda-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,jacaranda-class (:background ,jacaranda-shadow))))
   `(corfu-border ((,jacaranda-class (:background ,jacaranda-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,jacaranda-class (:foreground ,jacaranda-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,jacaranda-class (:foreground ,jacaranda-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,jacaranda-class (:foreground ,jacaranda-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,jacaranda-class (:foreground ,jacaranda-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,jacaranda-class (:foreground ,jacaranda-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,jacaranda-class (:foreground ,jacaranda-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,jacaranda-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,jacaranda-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,jacaranda-class (:inherit success))))
   `(envrc-mode-line-none-face ((,jacaranda-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,jacaranda-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,jacaranda-class (:weight bold :background ,jacaranda-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,jacaranda-class (:foreground ,jacaranda-white :background ,jacaranda-fl-builtin :inherit region))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,jacaranda-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-jacaranda)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-jacaranda-theme.el ends here
