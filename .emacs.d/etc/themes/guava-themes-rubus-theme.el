;;; guava-themes-rubus-theme.el --- A theme inspired by raspberry colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Mar 27, 2026
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
;; A theme inspired by raspberry, blackberry, and dewberry colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-rubus "A theme inspired by raspberry, blackberry, and dewberry colors.")

(let* (
      (rubus-class '((class color) (min-colors 257)))
      (rubus-black                     "#000000")
      (rubus-white                     "#FFFFFF")

      (rubus-raspberry                 "#aa3232")
      (rubus-red-pink                  "#c83c64")
      (rubus-pink-cream                "#c87873")
      (rubus-alt-pink-cream            "#aa5550")
      (rubus-orange                    "#ffa07a")

      (rubus-yellow                    "#fde8b9")

      (rubus-light-green               "#4ecd75")
      (rubus-green                     "#1e8264")
      (rubus-green-forest              "#00824b")
      (rubus-green-blue                "#5b9696")

      (rubus-light-blue                "#5096f1")
      (rubus-blue                      "#1455f1")
      (rubus-blue-subdued              "#1c81a1")
      (rubus-cyan                      "#00FFFF")

      (rubus-light-purple              "#beaadc")
      (rubus-purple                    "#8c78dc")
      (rubus-purple-pink               "#aa649b")

      (rubus-fg-1                      "#FFFFFF")
      (rubus-fg-2                      "#c3c3c3")
      (rubus-bg                        "#0e1216")
      (rubus-highlight                 "#22262a")
      (rubus-shadow                    "#b3b3b3")
      (rubus-tooltip                   "#ffffe0")
      (rubus-region-1                  "#663a43")
      (rubus-region-2                  "#6e504b")

      (rubus-error                     "#ff1e00")
      (rubus-warning                   "#f6d909")
      (rubus-success                   "#1ebe1e")
      (rubus-change                    "#1455f1")

      (rubus-mode-line                 "#aa3232")
      (rubus-mode-line-inactive        "#cd7378")
      (rubus-mode-line-modified        "#9682fa")
      (rubus-bell                      "#326EAA")

      (rubus-tab-1                     rubus-mode-line)
      (rubus-tab-2                     rubus-mode-line-inactive)
      (rubus-tab-3                     rubus-alt-pink-cream)

      (rubus-fl-comment                rubus-green-forest)
      (rubus-fl-string                 rubus-orange)
      (rubus-fl-keyword                rubus-red-pink)
      (rubus-fl-builtin                rubus-raspberry)
      (rubus-fl-type                   rubus-blue-subdued)
      (rubus-fl-function-name          rubus-purple-pink)
      (rubus-fl-variable-name          rubus-yellow)
      (rubus-fl-constant               rubus-purple)
      (rubus-fl-warning                rubus-warning)
      (rubus-fl-punctuation            rubus-fg-2)
      (rubus-fl-negation-char          rubus-green-blue)

      (rubus-diff-added                "#5aa05a")
      (rubus-diff-removed              "#a05a5a")
      (rubus-diff-refine-added         "#007800")
      (rubus-diff-refine-removed       "#780000")
      (rubus-diff-header               "#5a5a5a")
      (rubus-diff-file-header          "#3c3c3c")
      (rubus-diff-context              "#828282")
      (rubus-smerge-base               "#5A5AA0")

      (rubus-square-a-0                "#af50c8")
      (rubus-square-a-1                "#28a03c")
      (rubus-square-a-2                "#ff6400")
      (rubus-square-a-3                "#3c82ff")

      (rubus-complementary-a-0         "#46C8A5")
      (rubus-complementary-a-1         "#C84669"))

  (custom-theme-set-faces
   'guava-themes-rubus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,rubus-class (:foreground ,rubus-fg-1 :background ,rubus-bg))))

   ;; error, warning, success
   `(error ((,rubus-class (:foreground ,rubus-error :weight bold))))
   `(warning ((,rubus-class (:foreground ,rubus-warning :weight bold))))
   `(success ((,rubus-class (:foreground ,rubus-success :weight bold))))

   ;; highlight
   `(highlight ((,rubus-class (:background ,rubus-highlight))))

   ;; shadow
   `(shadow ((,rubus-class (:foreground ,rubus-shadow))))

   ;; tooltip
   `(tooltip ((,rubus-class (:weight bold :foreground ,rubus-black :background ,rubus-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,rubus-class (:background ,rubus-region-1 :extend t))))
   `(secondary-selection ((,rubus-class (:background ,rubus-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,rubus-class (:foreground ,rubus-fl-comment :weight medium))))
   `(font-lock-string-face ((,rubus-class (:foreground ,rubus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,rubus-class (:foreground ,rubus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,rubus-class (:foreground ,rubus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,rubus-class (:foreground ,rubus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,rubus-class (:foreground ,rubus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,rubus-class (:foreground ,rubus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,rubus-class (:foreground ,rubus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,rubus-class (:foreground ,rubus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,rubus-class (:foreground ,rubus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,rubus-class (:foreground ,rubus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,rubus-class (:foreground ,rubus-white :background ,rubus-mode-line))))

   ;; fringe
   `(fringe ((,rubus-class (:foreground ,rubus-yellow :background ,rubus-bg))))
   `(diff-hl-change ((,rubus-class (:foreground ,rubus-change :background ,rubus-change))))
   `(diff-hl-insert ((,rubus-class (:foreground ,rubus-success :background ,rubus-success))))
   `(diff-hl-delete ((,rubus-class (:foreground ,rubus-error :background ,rubus-error))))

   ;; line-number
   `(line-number ((,rubus-class (:foreground ,rubus-fg-1 :inherit default))))
   `(line-number-current-line ((,rubus-class (:foreground ,rubus-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,rubus-class (:background ,rubus-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,rubus-class (:background ,rubus-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,rubus-class (:foreground ,rubus-white :background ,rubus-mode-line
                                           :box ,(if guava-themes-boxed-mode-line
                                                     '(:line-width -1 :style released-button)
                                                   'unspecified)))))
   `(mode-line-inactive ((,rubus-class (:background ,rubus-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,rubus-class (:weight bold))))
   `(mode-line-highlight ((,rubus-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,rubus-class (:foreground ,rubus-white :background ,rubus-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,rubus-class (:weight bold :foreground ,rubus-mode-line))))

   ;; borders
   `(vertical-border ((,rubus-class (:foreground ,rubus-mode-line))))

   ;; header-line
   `(header-line ((,rubus-class (:inherit mode-line))))
   `(which-func ((,rubus-class (:weight bold :foreground ,rubus-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,rubus-class (:foreground ,rubus-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,rubus-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,rubus-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,rubus-class (:foreground ,rubus-white :background ,rubus-bell))))
   `(show-paren-mismatch ((,rubus-class (:foreground ,rubus-white :background ,rubus-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,rubus-class (:background ,rubus-error))))

   ;; links
   `(link ((,rubus-class (:foreground ,rubus-light-green :underline t :weight bold))))
   `(link-visited ((,rubus-class (:foreground ,rubus-green :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,rubus-class (:foreground ,rubus-raspberry :weight medium))))
   `(outline-2 ((,rubus-class (:foreground ,rubus-blue-subdued :weight medium))))
   `(outline-3 ((,rubus-class (:foreground ,rubus-green-forest :weight medium))))
   `(outline-4 ((,rubus-class (:foreground ,rubus-purple :weight medium))))
   `(outline-5 ((,rubus-class (:inherit outline-1))))
   `(outline-6 ((,rubus-class (:inherit outline-2))))
   `(outline-7 ((,rubus-class (:inherit outline-3))))
   `(outline-8 ((,rubus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,rubus-class (:foreground ,rubus-cyan))))
   `(escape-glyph ((,rubus-class (:inherit homoglyph))))
   `(nobreak-space ((,rubus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,rubus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,rubus-class (:background ,rubus-light-blue))))

   ;; help-key-binding
   `(help-key-binding ((,rubus-class (:foreground ,rubus-cyan :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-header :extend t))))
   `(diff-file-header ((,rubus-class (:weight bold :foreground ,rubus-white :background ,rubus-diff-file-header :extend t))))
   `(diff-context ((,rubus-class (:foreground ,rubus-white :background ,rubus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,rubus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,rubus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,rubus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,rubus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,rubus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,rubus-class (:foreground ,rubus-white :background ,rubus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,rubus-class (:foreground ,rubus-warning :weight bold))))
   `(completions-first-difference ((,rubus-class (:foreground ,rubus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,rubus-class (:foreground ,rubus-error :weight bold))))
   `(org-done ((,rubus-class (:foreground ,rubus-success :weight bold))))
   `(org-hide ((,rubus-class (:foreground ,rubus-bg))))
   `(org-table ((,rubus-class (:foreground ,rubus-pink-cream))))
   `(org-table-header ((,rubus-class (:inverse-video t :inherit org-table))))
   `(org-date ((,rubus-class (:foreground ,rubus-green-blue))))
   `(org-date-selected ((,rubus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,rubus-class (:foreground ,rubus-square-a-2))))
   `(org-headline-done ((,rubus-class (:foreground ,rubus-square-a-1))))
   `(org-document-title ((,rubus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,rubus-class (:inherit shadow))))
   `(org-meta-line ((,rubus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,rubus-class (:foreground ,rubus-tab-3))))
   `(window-divider-first-pixel ((,rubus-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,rubus-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,rubus-class (:foreground ,rubus-white :background ,rubus-square-a-2))))
   `(isearch-fail ((,rubus-class (:foreground ,rubus-white :background ,rubus-error))))
   `(lazy-highlight ((,rubus-class (:foreground ,rubus-white :background ,rubus-square-a-3))))
   `(isearch-group-1 ((,rubus-class (:foreground ,rubus-white :background ,rubus-square-a-0))))
   `(isearch-group-2 ((,rubus-class (:foreground ,rubus-white :background ,rubus-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,rubus-class (:inherit isearch))))
   `(match ((,rubus-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,rubus-class (:foreground ,rubus-white :background ,rubus-mode-line :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,rubus-class (:foreground ,rubus-white :background ,rubus-mode-line :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,rubus-class (:foreground ,rubus-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,rubus-class (:foreground ,rubus-bell :inherit custom-button-unraised))))
   `(custom-comment ((,rubus-class (:background ,rubus-tab-3))))
   `(custom-variable-tag ((,rubus-class (:weight bold :foreground ,rubus-tab-2))))
   `(custom-group-tag ((,rubus-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,rubus-class (:foreground ,rubus-fl-keyword))))

   ;; widget
   `(widget-field ((,rubus-class (:background ,rubus-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,rubus-class (:background ,rubus-tab-3))))

   ;; package
   `(package-status-dependency ((,rubus-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,rubus-class (:weight bold :foreground ,rubus-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,rubus-class (:weight bold :foreground ,rubus-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,rubus-class (:foreground ,rubus-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,rubus-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,rubus-class (:underline (:style wave :color ,rubus-success)))))
   `(flymake-warning ((,rubus-class (:underline (:style wave :color ,rubus-warning)))))
   `(flymake-error ((,rubus-class (:underline (:style wave :color ,rubus-error)))))

   ;; eww
   `(eww-valid-certificate ((,rubus-class (:weight bold :foreground ,rubus-success))))
   `(eww-invalid-certificate ((,rubus-class (:weight bold :foreground ,rubus-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,rubus-class (:foreground ,rubus-fg-1 :background ,rubus-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,rubus-class (:foreground ,rubus-fg-1 :background ,rubus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,rubus-class (:foreground ,rubus-fg-1 :background ,rubus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,rubus-class (:foreground ,rubus-fg-1 :background ,rubus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,rubus-class (:foreground ,rubus-fg-1 :background ,rubus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,rubus-class (:foreground ,rubus-fg-1 :background ,rubus-tab-3 :box (:line-width 1 :color ,rubus-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,rubus-class (:foreground ,rubus-green-blue))))
   `(elfeed-search-date-face ((,rubus-class (:foreground ,rubus-green))))
   `(elfeed-search-feed-face ((,rubus-class (:foreground ,rubus-light-blue))))
   `(elfeed-search-title-face ((,rubus-class (:foreground ,rubus-green-forest))))
   `(elfeed-search-unread-title-face ((,rubus-class (:weight bold :foreground ,rubus-light-green))))
   `(elfeed-search-filter-face ((,rubus-class (:weight bold :foreground ,rubus-yellow))))
   `(elfeed-search-last-update-face ((,rubus-class (:weight bold :foreground ,rubus-yellow))))
   `(elfeed-search-unread-count-face ((,rubus-class (:weight bold :foreground ,rubus-yellow))))

   `(elfeed-show-header-face ((,rubus-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,rubus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,rubus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,rubus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,rubus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,rubus-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,rubus-class (:foreground ,rubus-light-purple :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,rubus-class (:foreground ,rubus-light-purple :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,rubus-class (:foreground ,rubus-orange :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,rubus-class (:foreground ,rubus-orange :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,rubus-class (:foreground ,rubus-orange :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,rubus-class (:foreground ,rubus-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,rubus-class (:foreground ,rubus-fg-1 :background ,rubus-bg))))
   `(corfu-current ((,rubus-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,rubus-class (:background ,rubus-shadow))))
   `(corfu-border ((,rubus-class (:background ,rubus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,rubus-class (:foreground ,rubus-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,rubus-class (:foreground ,rubus-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,rubus-class (:foreground ,rubus-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,rubus-class (:foreground ,rubus-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,rubus-class (:foreground ,rubus-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,rubus-class (:foreground ,rubus-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,rubus-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,rubus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,rubus-class (:inherit success))))
   `(envrc-mode-line-none-face ((,rubus-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,rubus-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,rubus-class (:weight bold :background ,rubus-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,rubus-class (:foreground ,rubus-white :background ,rubus-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,rubus-class (:foreground ,rubus-white :background ,rubus-fl-builtin :inherit region))))

   ;; keycast
   `(keycast-command ((,rubus-class (:weight bold))))
   `(keycast-key ((,rubus-class (:foreground ,rubus-white :background ,rubus-tab-3 :box (:line-width -3 :style released-button) :inherit fixed-pitch))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,rubus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-rubus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-rubus-theme.el ends here
