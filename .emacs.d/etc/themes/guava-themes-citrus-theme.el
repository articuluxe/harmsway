;;; guava-themes-citrus-theme.el --- A theme inspired by orange colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Feb 15, 2026
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
;; A theme inspired by orange, lime, lemon, and grapefruit colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-citrus "A theme inspired by orange, lime, lemon, and grapefruit colors.")

(let* (
      (citrus-class '((class color) (min-colors 257)))
      (citrus-black                     "#000000")
      (citrus-white                     "#edf2ed")

      (citrus-light-green               "#6ea56e")
      (citrus-green-lime                "#589337")
      (citrus-deep-green                "#0a6b0a")
      (citrus-green-blue                "#197d5a")

      (citrus-yellow                    "#f5d49b")
      (citrus-alt-yellow                "#e1b69b")
      (citrus-deep-yellow               "#b99869")

      (citrus-red                       "#df352c")
      (citrus-deep-red                  "#a0352c")
      (citrus-orange-red                "#ce462c")
      (citrus-orange-orange             "#f29a43")
      (citrus-alt-orange                "#f26e43")
      (citrus-deep-orange               "#c85802")

      (citrus-brown                     "#735f50")

      (citrus-light-blue                "#4eb096")
      (citrus-blue                      "#3275a5")
      (citrus-deep-blue                 "#2327dc")
      (citrus-light-purple              "#bec8ff")
      (citrus-deep-purple               "#6d4393")
      (citrus-purple-red                "#77003a")

      (citrus-fg-1                      "#000000")
      (citrus-fg-2                      "#3c3c3c")
      (citrus-bg                        "#edf2ed")
      (citrus-highlight                 "#d9ded9")
      (citrus-shadow                    "#6b6b6b")
      (citrus-tooltip                   "#ffffe0")
      (citrus-region-1                  "#f5d49b")
      (citrus-region-2                  "#f29a43")

      (citrus-error                     "#FF0000")
      (citrus-warning                   "#ffbe00")
      (citrus-success                   "#2bdc26")
      (citrus-change                    "#2327dc")

      (citrus-mode-line                 "#589337")
      (citrus-mode-line-inactive        "#6ea56e")
      (citrus-mode-line-modified        "#43F29B")
      (citrus-bell                      "#933787")

      (citrus-tab-1                     citrus-orange-red)
      (citrus-tab-2                     citrus-orange-orange)
      (citrus-tab-3                     citrus-alt-orange)

      (citrus-fl-comment                citrus-green-blue)
      (citrus-fl-string                 citrus-light-green)
      (citrus-fl-keyword                citrus-red)
      (citrus-fl-builtin                citrus-deep-orange)
      (citrus-fl-type                   citrus-blue)
      (citrus-fl-function-name          citrus-deep-green)
      (citrus-fl-variable-name          citrus-green-lime)
      (citrus-fl-constant               citrus-deep-blue)
      (citrus-fl-warning                citrus-warning)
      (citrus-fl-punctuation            citrus-fg-2)
      (citrus-fl-negation-char          citrus-purple-red)

      (citrus-diff-added                "#c8f0c8")
      (citrus-diff-removed              "#f0c8c8")
      (citrus-diff-refine-added         "#78f078")
      (citrus-diff-refine-removed       "#f07878")
      (citrus-diff-header               "#b4b4b4")
      (citrus-diff-file-header          "#8c8c8c")
      (citrus-diff-context              "#dcdcdc")
      (citrus-smerge-base               "#C8C8F0")

      (citrus-square-a-0                "#af28b9")
      (citrus-square-a-1                "#28a03c")
      (citrus-square-a-2                "#ff6400")
      (citrus-square-a-3                "#3c82ff")

      (citrus-complementary-a-0         "#4680DE")
      (citrus-complementary-a-1         "#DE4680"))

  (custom-theme-set-faces
   'guava-themes-citrus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,citrus-class (:foreground ,citrus-fg-1 :background ,citrus-bg))))

   ;; error, warning, success
   `(error ((,citrus-class (:foreground ,citrus-error :weight bold))))
   `(warning ((,citrus-class (:foreground ,citrus-warning :weight bold))))
   `(success ((,citrus-class (:foreground ,citrus-success :weight bold))))

   ;; highlight
   `(highlight ((,citrus-class (:background ,citrus-highlight))))

   ;; shadow
   `(shadow ((,citrus-class (:foreground ,citrus-shadow))))

   ;; tooltip
   `(tooltip ((,citrus-class (:weight bold :foreground ,citrus-black :background ,citrus-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,citrus-class (:background ,citrus-region-1 :extend t))))
   `(secondary-selection ((,citrus-class (:background ,citrus-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,citrus-class (:foreground ,citrus-fl-comment :weight medium))))
   `(font-lock-string-face ((,citrus-class (:foreground ,citrus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,citrus-class (:foreground ,citrus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,citrus-class (:foreground ,citrus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,citrus-class (:foreground ,citrus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,citrus-class (:foreground ,citrus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,citrus-class (:foreground ,citrus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,citrus-class (:foreground ,citrus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,citrus-class (:foreground ,citrus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,citrus-class (:foreground ,citrus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,citrus-class (:foreground ,citrus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,citrus-class (:foreground ,citrus-white :background ,citrus-mode-line))))

   ;; fringe
   `(fringe ((,citrus-class (:foreground ,citrus-red :background ,citrus-bg))))
   `(diff-hl-change ((,citrus-class (:foreground ,citrus-change :background ,citrus-change))))
   `(diff-hl-insert ((,citrus-class (:foreground ,citrus-success :background ,citrus-success))))
   `(diff-hl-delete ((,citrus-class (:foreground ,citrus-error :background ,citrus-error))))

   ;; line-number
   `(line-number ((,citrus-class (:foreground ,citrus-fg-1 :inherit default))))
   `(line-number-current-line ((,citrus-class (:foreground ,citrus-tab-1 :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,citrus-class (:background ,citrus-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,citrus-class (:background ,citrus-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,citrus-class (:foreground ,citrus-white :background ,citrus-mode-line
                                            :box ,(if guava-themes-boxed-mode-line
                                                      '(:line-width -1 :style released-button)
                                                    'unspecified)))))
   `(mode-line-inactive ((,citrus-class (:background ,citrus-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,citrus-class (:weight bold))))
   `(mode-line-highlight ((,citrus-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,citrus-class (:foreground ,citrus-white :background ,citrus-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,citrus-class (:weight bold :foreground ,citrus-mode-line))))

   ;; borders
   `(vertical-border ((,citrus-class (:foreground ,citrus-mode-line))))

   ;; header-line
   `(header-line ((,citrus-class (:background ,citrus-tab-1 :inherit mode-line))))
   `(which-func ((,citrus-class (:weight bold :foreground ,citrus-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,citrus-class (:foreground ,citrus-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,citrus-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,citrus-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,citrus-class (:foreground ,citrus-white :background ,citrus-bell))))
   `(show-paren-mismatch ((,citrus-class (:foreground ,citrus-white :background ,citrus-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,citrus-class (:background ,citrus-error))))

   ;; links
   `(link ((,citrus-class (:foreground ,citrus-light-blue :underline t :weight bold))))
   `(link-visited ((,citrus-class (:foreground ,citrus-deep-blue :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,citrus-class (:foreground ,citrus-blue :weight medium))))
   `(outline-2 ((,citrus-class (:foreground ,citrus-green-blue :weight medium))))
   `(outline-3 ((,citrus-class (:foreground ,citrus-deep-purple :weight medium))))
   `(outline-4 ((,citrus-class (:foreground ,citrus-deep-red :weight medium))))
   `(outline-5 ((,citrus-class (:inherit outline-1))))
   `(outline-6 ((,citrus-class (:inherit outline-2))))
   `(outline-7 ((,citrus-class (:inherit outline-3))))
   `(outline-8 ((,citrus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,citrus-class (:foreground ,citrus-deep-blue))))
   `(escape-glyph ((,citrus-class (:inherit homoglyph))))
   `(nobreak-space ((,citrus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,citrus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,citrus-class (:background ,citrus-light-blue))))

   ;; help-key-binding
   `(help-key-binding ((,citrus-class (:foreground ,citrus-deep-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,citrus-class (:foreground ,citrus-black :background ,citrus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,citrus-class (:foreground ,citrus-black :background ,citrus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,citrus-class (:foreground ,citrus-black :background ,citrus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,citrus-class (:foreground ,citrus-black :background ,citrus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,citrus-class (:foreground ,citrus-black :background ,citrus-diff-header :extend t))))
   `(diff-file-header ((,citrus-class (:weight bold :foreground ,citrus-black :background ,citrus-diff-file-header :extend t))))
   `(diff-context ((,citrus-class (:foreground ,citrus-black :background ,citrus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,citrus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,citrus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,citrus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,citrus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,citrus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,citrus-class (:foreground ,citrus-black :background ,citrus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,citrus-class (:foreground ,citrus-change :weight bold))))
   `(completions-first-difference ((,citrus-class (:foreground ,citrus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,citrus-class (:foreground ,citrus-error :weight bold))))
   `(org-done ((,citrus-class (:foreground ,citrus-success :weight bold))))
   `(org-hide ((,citrus-class (:foreground ,citrus-bg))))
   `(org-table ((,citrus-class (:foreground ,citrus-orange-red))))
   `(org-table-header ((,citrus-class (:inverse-video t :inherit org-table))))
   `(org-date ((,citrus-class (:foreground ,citrus-purple-red))))
   `(org-date-selected ((,citrus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,citrus-class (:foreground ,citrus-square-a-2))))
   `(org-headline-done ((,citrus-class (:foreground ,citrus-square-a-1))))
   `(org-document-title ((,citrus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,citrus-class (:inherit shadow))))
   `(org-meta-line ((,citrus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,citrus-class (:foreground ,citrus-tab-3))))
   `(window-divider-first-pixel ((,citrus-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,citrus-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,citrus-class (:foreground ,citrus-white :background ,citrus-square-a-2))))
   `(isearch-fail ((,citrus-class (:foreground ,citrus-white :background ,citrus-error))))
   `(lazy-highlight ((,citrus-class (:foreground ,citrus-white :background ,citrus-square-a-3))))
   `(isearch-group-1 ((,citrus-class (:foreground ,citrus-white :background ,citrus-square-a-0))))
   `(isearch-group-2 ((,citrus-class (:foreground ,citrus-white :background ,citrus-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,citrus-class (:inherit isearch))))
   `(match ((,citrus-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-1 :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-1 :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,citrus-class (:foreground ,citrus-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,citrus-class (:foreground ,citrus-bell :inherit custom-button-unraised))))
   `(custom-comment ((,citrus-class (:background ,citrus-tab-3))))
   `(custom-variable-tag ((,citrus-class (:weight bold :foreground ,citrus-tab-2))))
   `(custom-group-tag ((,citrus-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,citrus-class (:foreground ,citrus-fl-keyword))))

   ;; widget
   `(widget-field ((,citrus-class (:background ,citrus-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,citrus-class (:background ,citrus-tab-3))))

   ;; package
   `(package-status-dependency ((,citrus-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,citrus-class (:weight bold :foreground ,citrus-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,citrus-class (:weight bold :foreground ,citrus-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,citrus-class (:foreground ,citrus-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,citrus-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,citrus-class (:underline (:style wave :color ,citrus-success)))))
   `(flymake-warning ((,citrus-class (:underline (:style wave :color ,citrus-warning)))))
   `(flymake-error ((,citrus-class (:underline (:style wave :color ,citrus-error)))))

   ;; eww
   `(eww-valid-certificate ((,citrus-class (:weight bold :foreground ,citrus-success))))
   `(eww-invalid-certificate ((,citrus-class (:weight bold :foreground ,citrus-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,citrus-class (:foreground ,citrus-fg-1 :background ,citrus-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,citrus-class (:foreground ,citrus-fg-1 :background ,citrus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,citrus-class (:foreground ,citrus-fg-1 :background ,citrus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,citrus-class (:foreground ,citrus-fg-1 :background ,citrus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,citrus-class (:foreground ,citrus-fg-1 :background ,citrus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,citrus-class (:foreground ,citrus-fg-1 :background ,citrus-tab-3 :box (:line-width 1 :color ,citrus-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,citrus-class (:foreground ,citrus-deep-yellow))))
   `(elfeed-search-date-face ((,citrus-class (:foreground ,citrus-green-blue))))
   `(elfeed-search-feed-face ((,citrus-class (:foreground ,citrus-green-lime))))
   `(elfeed-search-title-face ((,citrus-class (:foreground ,citrus-brown))))
   `(elfeed-search-unread-title-face ((,citrus-class (:weight bold :foreground ,citrus-deep-orange))))
   `(elfeed-search-filter-face ((,citrus-class (:weight bold :foreground ,citrus-yellow))))
   `(elfeed-search-last-update-face ((,citrus-class (:weight bold :foreground ,citrus-yellow))))
   `(elfeed-search-unread-count-face ((,citrus-class (:weight bold :foreground ,citrus-yellow))))

   `(elfeed-show-header-face ((,citrus-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,citrus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,citrus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,citrus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,citrus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,citrus-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,citrus-class (:foreground ,citrus-yellow :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,citrus-class (:foreground ,citrus-yellow :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,citrus-class (:foreground ,citrus-alt-yellow :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,citrus-class (:foreground ,citrus-alt-yellow :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,citrus-class (:foreground ,citrus-alt-yellow :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,citrus-class (:foreground ,citrus-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,citrus-class (:foreground ,citrus-fg-1 :background ,citrus-bg))))
   `(corfu-current ((,citrus-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,citrus-class (:background ,citrus-shadow))))
   `(corfu-border ((,citrus-class (:background ,citrus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,citrus-class (:foreground ,citrus-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,citrus-class (:foreground ,citrus-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,citrus-class (:foreground ,citrus-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,citrus-class (:foreground ,citrus-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,citrus-class (:foreground ,citrus-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,citrus-class (:foreground ,citrus-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,citrus-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,citrus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,citrus-class (:inherit success))))
   `(envrc-mode-line-none-face ((,citrus-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,citrus-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,citrus-class (:weight bold :background ,citrus-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,citrus-class (:foreground ,citrus-white :background ,citrus-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,citrus-class (:foreground ,citrus-white :background ,citrus-fl-builtin :inherit region))))

   ;; keycast
   `(keycast-command ((,citrus-class (:weight bold))))
   `(keycast-key ((,citrus-class (:foreground ,citrus-white :background ,citrus-tab-3 :box (:line-width -3 :style released-button) :inherit fixed-pitch))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,citrus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-citrus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-citrus-theme.el ends here
