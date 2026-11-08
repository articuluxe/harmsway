;;; guava-themes-malus-theme.el --- A theme inspired by apple colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: May 15, 2026
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
;; A theme inspired by apple colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-malus "A theme inspired by apple colors.")

(let* (
      (malus-class '((class color) (min-colors 257)))
      ;; (malus-black                     "#000000")
      (malus-white                     "#FFFFFF")
      (malus-black                     "#000000")

      (malus-brown                     "#826e5a")

      (malus-yellow-subdued            "#d2c864")

      (malus-green-granny              "#96dc50")
      (malus-green-forest              "#27ab50")

      (malus-red-sweetango             "#d76469")
      (malus-alt-red-sweetango         "#7d5a5f")
      (malus-deep-red                  "#aa3c32")

      (malus-light-blue                "#50bebe")
      (malus-blue                      "#4548e3")
      (malus-blue-subdued              "#9cc4de")
      (malus-oceanic-blue              "#49835f")
      (malus-cyan                      "#00ffff")

      (malus-purple                    "#8468ed")

      (malus-fg-1                      "#fafbb7")
      (malus-fg-2                      "#bebf7b")
      (malus-bg                        "#1e0910")
      (malus-highlight                 "#321d24")
      (malus-shadow                    "#b3b3b3")
      (malus-tooltip                   "#ffffe0")
      (malus-region-1                  "#788250")
      (malus-region-2                  "#8c9632")

      (malus-error                     "#d70000")
      (malus-warning                   "#f6d911")
      (malus-success                   "#23d723")
      (malus-change                    "#4548e3")

      (malus-mode-line                 "#cd5a5f")
      (malus-mode-line-inactive        "#49835f")
      (malus-mode-line-modified        "#60CD5A")
      (malus-bell                      "#5A99CD")

      (malus-tab-1                     malus-mode-line)
      (malus-tab-2                     malus-mode-line-inactive)
      (malus-tab-3                     malus-deep-red)

      (malus-fl-comment                malus-green-granny)
      (malus-fl-string                 malus-red-sweetango)
      (malus-fl-keyword                malus-green-forest)
      (malus-fl-builtin                malus-light-blue)
      (malus-fl-type                   malus-blue-subdued)
      (malus-fl-function-name          malus-deep-red)
      (malus-fl-variable-name          malus-yellow-subdued)
      (malus-fl-constant               malus-purple)
      (malus-fl-warning                malus-warning)
      (malus-fl-punctuation            malus-fg-2)
      (malus-fl-negation-char          malus-oceanic-blue)

      (malus-diff-added                "#5aa05a")
      (malus-diff-removed              "#a05a5a")
      (malus-diff-refine-added         "#007800")
      (malus-diff-refine-removed       "#780000")
      (malus-diff-header               "#5a5a5a")
      (malus-diff-file-header          "#3c3c3c")
      (malus-diff-context              "#828282")
      (malus-smerge-base               "#5A5AA0")

      (malus-square-a-0                "#af50c8")
      (malus-square-a-1                "#28a03c")
      (malus-square-a-2                "#ff6400")
      (malus-square-a-3                "#3c82ff")

      (malus-complementary-a-0         "#46C8A5")
      (malus-complementary-a-1         "#C84669"))

  (custom-theme-set-faces
   'guava-themes-malus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,malus-class (:foreground ,malus-fg-1 :background ,malus-bg))))

   ;; error, warning, success
   `(error ((,malus-class (:foreground ,malus-error :weight bold))))
   `(warning ((,malus-class (:foreground ,malus-warning :weight bold))))
   `(success ((,malus-class (:foreground ,malus-success :weight bold))))

   ;; highlight
   `(highlight ((,malus-class (:background ,malus-highlight))))

   ;; shadow
   `(shadow ((,malus-class (:foreground ,malus-shadow))))

   ;; tooltip
   `(tooltip ((,malus-class (:weight bold :foreground ,malus-black :background ,malus-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,malus-class (:background ,malus-region-1 :extend t))))
   `(secondary-selection ((,malus-class (:background ,malus-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,malus-class (:foreground ,malus-fl-comment :weight medium))))
   `(font-lock-string-face ((,malus-class (:foreground ,malus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,malus-class (:foreground ,malus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,malus-class (:foreground ,malus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,malus-class (:foreground ,malus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,malus-class (:foreground ,malus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,malus-class (:foreground ,malus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,malus-class (:foreground ,malus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,malus-class (:foreground ,malus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,malus-class (:foreground ,malus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,malus-class (:foreground ,malus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,malus-class (:foreground ,malus-white :background ,malus-mode-line))))

   ;; fringe
   `(fringe ((,malus-class (:foreground ,malus-cyan :background ,malus-bg))))
   `(diff-hl-change ((,malus-class (:foreground ,malus-change :background ,malus-change))))
   `(diff-hl-insert ((,malus-class (:foreground ,malus-success :background ,malus-success))))
   `(diff-hl-delete ((,malus-class (:foreground ,malus-error :background ,malus-error))))

   ;; line-number
   `(line-number ((,malus-class (:foreground ,malus-fg-1 :inherit default))))
   `(line-number-current-line ((,malus-class (:foreground ,malus-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,malus-class (:background ,malus-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,malus-class (:background ,malus-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,malus-class (:foreground ,malus-white :background ,malus-mode-line
                                           :box ,(if guava-themes-boxed-mode-line
                                                     '(:line-width -1 :style released-button)
                                                   'unspecified)))))
   `(mode-line-inactive ((,malus-class (:background ,malus-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,malus-class (:weight bold))))
   `(mode-line-highlight ((,malus-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,malus-class (:foreground ,malus-white :background ,malus-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,malus-class (:weight bold :foreground ,malus-mode-line))))

   ;; borders
   `(vertical-border ((,malus-class (:foreground ,malus-mode-line))))

   ;; header-line
   `(header-line ((,malus-class (:inherit mode-line))))
   `(which-func ((,malus-class (:weight bold :foreground ,malus-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,malus-class (:foreground ,malus-white :background ,malus-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,malus-class (:foreground ,malus-white :background ,malus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,malus-class (:foreground ,malus-white :background ,malus-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,malus-class (:foreground ,malus-white :background ,malus-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,malus-class (:foreground ,malus-white :background ,malus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,malus-class (:foreground ,malus-white :background ,malus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,malus-class (:foreground ,malus-white :background ,malus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,malus-class (:foreground ,malus-white :background ,malus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,malus-class (:foreground ,malus-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,malus-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,malus-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,malus-class (:foreground ,malus-white :background ,malus-bell))))
   `(show-paren-mismatch ((,malus-class (:foreground ,malus-white :background ,malus-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,malus-class (:background ,malus-error))))

   ;; links
   `(link ((,malus-class (:foreground ,malus-light-blue :underline t :weight bold))))
   `(link-visited ((,malus-class (:foreground ,malus-oceanic-blue :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,malus-class (:foreground ,malus-deep-red :weight medium))))
   `(outline-2 ((,malus-class (:foreground ,malus-light-blue :weight medium))))
   `(outline-3 ((,malus-class (:foreground ,malus-yellow-subdued :weight medium))))
   `(outline-4 ((,malus-class (:foreground ,malus-green-forest :weight medium))))
   `(outline-5 ((,malus-class (:inherit outline-1))))
   `(outline-6 ((,malus-class (:inherit outline-2))))
   `(outline-7 ((,malus-class (:inherit outline-3))))
   `(outline-8 ((,malus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,malus-class (:foreground ,malus-cyan))))
   `(escape-glyph ((,malus-class (:inherit homoglyph))))
   `(nobreak-space ((,malus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,malus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,malus-class (:background ,malus-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,malus-class (:foreground ,malus-cyan :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,malus-class (:foreground ,malus-white :background ,malus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,malus-class (:foreground ,malus-white :background ,malus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,malus-class (:foreground ,malus-white :background ,malus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,malus-class (:foreground ,malus-white :background ,malus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,malus-class (:foreground ,malus-white :background ,malus-diff-header :extend t))))
   `(diff-file-header ((,malus-class (:weight bold :foreground ,malus-white :background ,malus-diff-file-header :extend t))))
   `(diff-context ((,malus-class (:foreground ,malus-white :background ,malus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,malus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,malus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,malus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,malus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,malus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,malus-class (:foreground ,malus-white :background ,malus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,malus-class (:foreground ,malus-warning :weight bold))))
   `(completions-first-difference ((,malus-class (:foreground ,malus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,malus-class (:foreground ,malus-error :weight bold))))
   `(org-done ((,malus-class (:foreground ,malus-success :weight bold))))
   `(org-hide ((,malus-class (:foreground ,malus-bg))))
   `(org-table ((,malus-class (:foreground ,malus-red-sweetango))))
   `(org-table-header ((,malus-class (:inverse-video t :inherit org-table))))
   `(org-date ((,malus-class (:foreground ,malus-oceanic-blue))))
   `(org-date-selected ((,malus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,malus-class (:foreground ,malus-square-a-2))))
   `(org-headline-done ((,malus-class (:foreground ,malus-square-a-1))))
   `(org-document-title ((,malus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,malus-class (:inherit shadow))))
   `(org-meta-line ((,malus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,malus-class (:foreground ,malus-tab-3))))
   `(window-divider-first-pixel ((,malus-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,malus-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,malus-class (:foreground ,malus-white :background ,malus-square-a-2))))
   `(isearch-fail ((,malus-class (:foreground ,malus-white :background ,malus-error))))
   `(lazy-highlight ((,malus-class (:foreground ,malus-white :background ,malus-square-a-3))))
   `(isearch-group-1 ((,malus-class (:foreground ,malus-white :background ,malus-square-a-0))))
   `(isearch-group-2 ((,malus-class (:foreground ,malus-white :background ,malus-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,malus-class (:inherit isearch))))
   `(match ((,malus-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,malus-class (:foreground ,malus-white :background ,malus-mode-line :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,malus-class (:foreground ,malus-white :background ,malus-mode-line :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,malus-class (:foreground ,malus-white :background ,malus-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,malus-class (:foreground ,malus-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,malus-class (:foreground ,malus-bell :inherit custom-button-unraised))))
   `(custom-comment ((,malus-class (:background ,malus-tab-3))))
   `(custom-variable-tag ((,malus-class (:weight bold :foreground ,malus-tab-2))))
   `(custom-group-tag ((,malus-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,malus-class (:foreground ,malus-fl-keyword))))

   ;; widget
   `(widget-field ((,malus-class (:background ,malus-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,malus-class (:background ,malus-tab-3))))

   ;; package
   `(package-status-dependency ((,malus-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,malus-class (:weight bold :foreground ,malus-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,malus-class (:weight bold :foreground ,malus-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,malus-class (:foreground ,malus-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,malus-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,malus-class (:underline (:style wave :color ,malus-success)))))
   `(flymake-warning ((,malus-class (:underline (:style wave :color ,malus-warning)))))
   `(flymake-error ((,malus-class (:underline (:style wave :color ,malus-error)))))

   ;; eww
   `(eww-valid-certificate ((,malus-class (:weight bold :foreground ,malus-success))))
   `(eww-invalid-certificate ((,malus-class (:weight bold :foreground ,malus-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,malus-class (:foreground ,malus-fg-1 :background ,malus-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,malus-class (:foreground ,malus-fg-1 :background ,malus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,malus-class (:foreground ,malus-fg-1 :background ,malus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,malus-class (:foreground ,malus-fg-1 :background ,malus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,malus-class (:foreground ,malus-fg-1 :background ,malus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,malus-class (:foreground ,malus-fg-1 :background ,malus-tab-3 :box (:line-width 1 :color ,malus-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,malus-class (:foreground ,malus-blue-subdued))))
   `(elfeed-search-date-face ((,malus-class (:foreground ,malus-deep-red))))
   `(elfeed-search-feed-face ((,malus-class (:foreground ,malus-green-forest))))
   `(elfeed-search-title-face ((,malus-class (:foreground ,malus-alt-red-sweetango))))
   `(elfeed-search-unread-title-face ((,malus-class (:weight bold :foreground ,malus-red-sweetango))))
   `(elfeed-search-filter-face ((,malus-class (:weight bold :foreground ,malus-blue))))
   `(elfeed-search-last-update-face ((,malus-class (:weight bold :foreground ,malus-blue))))
   `(elfeed-search-unread-count-face ((,malus-class (:weight bold :foreground ,malus-blue))))

   `(elfeed-show-header-face ((,malus-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,malus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,malus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,malus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,malus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,malus-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,malus-class (:foreground ,malus-green-granny :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,malus-class (:foreground ,malus-green-granny :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,malus-class (:foreground ,malus-shadow :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,malus-class (:foreground ,malus-shadow :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,malus-class (:foreground ,malus-shadow :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,malus-class (:foreground ,malus-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,malus-class (:foreground ,malus-fg-1 :background ,malus-bg))))
   `(corfu-current ((,malus-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,malus-class (:background ,malus-shadow))))
   `(corfu-border ((,malus-class (:background ,malus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,malus-class (:foreground ,malus-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,malus-class (:foreground ,malus-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,malus-class (:foreground ,malus-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,malus-class (:foreground ,malus-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,malus-class (:foreground ,malus-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,malus-class (:foreground ,malus-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,malus-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,malus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,malus-class (:inherit success))))
   `(envrc-mode-line-none-face ((,malus-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,malus-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,malus-class (:weight bold :background ,malus-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,malus-class (:foreground ,malus-white :background ,malus-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,malus-class (:foreground ,malus-black :background ,malus-fl-builtin :inherit region))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,malus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-malus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-malus-theme.el ends here
