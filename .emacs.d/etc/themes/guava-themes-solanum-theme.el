;;; guava-themes-solanum-theme.el --- A theme inspired by eggplant colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <geralldborbon@gmail.com>
;; Created: Feb 22, 2026
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
;; A theme inspired by eggplant, potato, and tomato colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-solanum "A theme inspired by eggplant, potato, and tomato colors.")

(let* (
      (solanum-class '((class color) (min-colors 257)))
      (solanum-black                     "#000000")
      (solanum-white                     "#FFFFFF")

      (solanum-red-tomato                "#cd151f")
      (solanum-orange                    "#e98c85")

      (solanum-yellow-potato             "#fde8b9")

      (solanum-light-green               "#4ec9b0")
      (solanum-alt-light-green           "#61ff96")
      (solanum-deep-green                "#007d37")

      (solanum-light-blue                "#8ec4de")
      (solanum-blue                      "#0d62b2")
      (solanum-cyan                      "#00ffff")

      (solanum-light-purple              "#bec8ff")
      (solanum-purple                    "#786ed7")
      (solanum-purple-pink               "#915d93")
      (solanum-dark-purple               "#73466e")
      (solanum-alt-dark-purple           "#59536e")

      (solanum-fg-1                      "#FFFFFF")
      (solanum-fg-2                      "#c3c3c3")
      (solanum-bg                        "#130d1a")
      (solanum-highlight                 "#27212e")
      (solanum-shadow                    "#b3b3b3")
      (solanum-tooltip                   "#ffffe0")
      (solanum-region-1                  "#492b87")
      (solanum-region-2                  "#3a6b43")

      (solanum-error                     "#FF0000")
      (solanum-warning                   "#f6d909")
      (solanum-success                   "#23a334")
      (solanum-change                    "#0d62b2")

      (solanum-mode-line                 "#73466e")
      (solanum-mode-line-inactive        "#3b3550")
      (solanum-mode-line-modified        "#3C8CA0")
      (solanum-bell                      "#4b7337")

      (solanum-tab-1                     solanum-mode-line)
      (solanum-tab-2                     solanum-mode-line-inactive)
      (solanum-tab-3                     solanum-alt-dark-purple)

      (solanum-fl-comment                solanum-red-tomato)
      (solanum-fl-string                 solanum-yellow-potato)
      (solanum-fl-keyword                solanum-deep-green)
      (solanum-fl-builtin                solanum-dark-purple)
      (solanum-fl-type                   solanum-orange)
      (solanum-fl-function-name          solanum-purple)
      (solanum-fl-variable-name          solanum-light-green)
      (solanum-fl-constant               solanum-light-blue)
      (solanum-fl-warning                solanum-warning)
      (solanum-fl-punctuation            solanum-fg-2)
      (solanum-fl-negation-char          solanum-alt-light-green)

      (solanum-diff-added                "#5aa05a")
      (solanum-diff-removed              "#a05a5a")
      (solanum-diff-refine-added         "#007800")
      (solanum-diff-refine-removed       "#780000")
      (solanum-diff-header               "#5a5a5a")
      (solanum-diff-file-header          "#3c3c3c")
      (solanum-diff-context              "#828282")
      (solanum-smerge-base               "#5A5AA0")

      (solanum-square-a-0                "#af50c8")
      (solanum-square-a-1                "#28a03c")
      (solanum-square-a-2                "#ff6400")
      (solanum-square-a-3                "#3c82ff")

      (solanum-complementary-a-0         "#46C8A5")
      (solanum-complementary-a-1         "#C84669"))

  (custom-theme-set-faces
   'guava-themes-solanum

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,solanum-class (:foreground ,solanum-fg-1 :background ,solanum-bg))))

   ;; error, warning, success
   `(error ((,solanum-class (:foreground ,solanum-error :weight bold))))
   `(warning ((,solanum-class (:foreground ,solanum-warning :weight bold))))
   `(success ((,solanum-class (:foreground ,solanum-success :weight bold))))

   ;; highlight
   `(highlight ((,solanum-class (:background ,solanum-highlight))))

   ;; shadow
   `(shadow ((,solanum-class (:foreground ,solanum-shadow))))

   ;; tooltip
   `(tooltip ((,solanum-class (:weight bold :foreground ,solanum-black :background ,solanum-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,solanum-class (:background ,solanum-region-1 :extend t))))
   `(secondary-selection ((,solanum-class (:background ,solanum-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,solanum-class (:foreground ,solanum-fl-comment :weight medium))))
   `(font-lock-string-face ((,solanum-class (:foreground ,solanum-fl-string :weight medium))))
   `(font-lock-keyword-face ((,solanum-class (:foreground ,solanum-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,solanum-class (:foreground ,solanum-fl-builtin :weight medium))))
   `(font-lock-type-face ((,solanum-class (:foreground ,solanum-fl-type :weight medium))))
   `(font-lock-function-name-face ((,solanum-class (:foreground ,solanum-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,solanum-class (:foreground ,solanum-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,solanum-class (:foreground ,solanum-fl-constant :weight medium))))
   `(font-lock-warning-face ((,solanum-class (:foreground ,solanum-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,solanum-class (:foreground ,solanum-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,solanum-class (:foreground ,solanum-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,solanum-class (:foreground ,solanum-white :background ,solanum-mode-line))))

   ;; fringe
   `(fringe ((,solanum-class (:foreground ,solanum-red-tomato :background ,solanum-bg))))
   `(diff-hl-change ((,solanum-class (:foreground ,solanum-change :background ,solanum-change))))
   `(diff-hl-insert ((,solanum-class (:foreground ,solanum-success :background ,solanum-success))))
   `(diff-hl-delete ((,solanum-class (:foreground ,solanum-error :background ,solanum-error))))

   ;; line-number
   `(line-number ((,solanum-class (:foreground ,solanum-fg-1 :inherit default))))
   `(line-number-current-line ((,solanum-class (:foreground ,solanum-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,solanum-class (:background ,solanum-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,solanum-class (:background ,solanum-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,solanum-class (:foreground ,solanum-white :background ,solanum-mode-line
                                             :box ,(if guava-themes-boxed-mode-line
                                                       '(:line-width -1 :style released-button)
                                                     'unspecified)))))
   `(mode-line-inactive ((,solanum-class (:background ,solanum-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,solanum-class (:weight bold))))
   `(mode-line-highlight ((,solanum-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,solanum-class (:foreground ,solanum-white :background ,solanum-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,solanum-class (:weight bold :foreground ,solanum-mode-line))))

   ;; borders
   `(vertical-border ((,solanum-class (:foreground ,solanum-mode-line))))

   ;; header-line
   `(header-line ((,solanum-class (:inherit mode-line))))
   `(which-func ((,solanum-class (:weight bold :foreground ,solanum-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,solanum-class (:foreground ,solanum-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,solanum-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,solanum-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,solanum-class (:foreground ,solanum-white :background ,solanum-bell))))
   `(show-paren-mismatch ((,solanum-class (:foreground ,solanum-white :background ,solanum-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,solanum-class (:background ,solanum-error))))

   ;; links
   `(link ((,solanum-class (:foreground ,solanum-yellow-potato :underline t :weight bold))))
   `(link-visited ((,solanum-class (:foreground ,solanum-orange :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,solanum-class (:foreground ,solanum-purple :weight medium))))
   `(outline-2 ((,solanum-class (:foreground ,solanum-red-tomato :weight medium))))
   `(outline-3 ((,solanum-class (:foreground ,solanum-blue :weight medium))))
   `(outline-4 ((,solanum-class (:foreground ,solanum-deep-green :weight medium))))
   `(outline-5 ((,solanum-class (:inherit outline-1))))
   `(outline-6 ((,solanum-class (:inherit outline-2))))
   `(outline-7 ((,solanum-class (:inherit outline-3))))
   `(outline-8 ((,solanum-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,solanum-class (:foreground ,solanum-cyan))))
   `(escape-glyph ((,solanum-class (:inherit homoglyph))))
   `(nobreak-space ((,solanum-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,solanum-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,solanum-class (:background ,solanum-light-blue))))

   ;; help-key-binding
   `(help-key-binding ((,solanum-class (:foreground ,solanum-light-purple :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-header :extend t))))
   `(diff-file-header ((,solanum-class (:weight bold :foreground ,solanum-white :background ,solanum-diff-file-header :extend t))))
   `(diff-context ((,solanum-class (:foreground ,solanum-white :background ,solanum-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,solanum-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,solanum-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,solanum-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,solanum-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,solanum-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,solanum-class (:foreground ,solanum-white :background ,solanum-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,solanum-class (:foreground ,solanum-warning :weight bold))))
   `(completions-first-difference ((,solanum-class (:foreground ,solanum-error :weight bold))))

   ;; org-faces
   `(org-todo ((,solanum-class (:foreground ,solanum-error :weight bold))))
   `(org-done ((,solanum-class (:foreground ,solanum-success :weight bold))))
   `(org-hide ((,solanum-class (:foreground ,solanum-bg))))
   `(org-table ((,solanum-class (:foreground ,solanum-yellow-potato))))
   `(org-table-header ((,solanum-class (:inverse-video t :inherit org-table))))
   `(org-date ((,solanum-class (:foreground ,solanum-alt-light-green))))
   `(org-date-selected ((,solanum-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,solanum-class (:foreground ,solanum-square-a-2))))
   `(org-headline-done ((,solanum-class (:foreground ,solanum-square-a-1))))
   `(org-document-title ((,solanum-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,solanum-class (:inherit shadow))))
   `(org-meta-line ((,solanum-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,solanum-class (:foreground ,solanum-tab-3))))
   `(window-divider-first-pixel ((,solanum-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,solanum-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,solanum-class (:foreground ,solanum-white :background ,solanum-square-a-2))))
   `(isearch-fail ((,solanum-class (:foreground ,solanum-white :background ,solanum-error))))
   `(lazy-highlight ((,solanum-class (:foreground ,solanum-white :background ,solanum-square-a-3))))
   `(isearch-group-1 ((,solanum-class (:foreground ,solanum-white :background ,solanum-square-a-0))))
   `(isearch-group-2 ((,solanum-class (:foreground ,solanum-white :background ,solanum-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,solanum-class (:inherit isearch))))
   `(match ((,solanum-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,solanum-class (:foreground ,solanum-white :background ,solanum-mode-line :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,solanum-class (:foreground ,solanum-white :background ,solanum-mode-line :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,solanum-class (:foreground ,solanum-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,solanum-class (:foreground ,solanum-bell :inherit custom-button-unraised))))
   `(custom-comment ((,solanum-class (:background ,solanum-tab-3))))
   `(custom-variable-tag ((,solanum-class (:weight bold :foreground ,solanum-tab-2))))
   `(custom-group-tag ((,solanum-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,solanum-class (:foreground ,solanum-fl-keyword))))

   ;; widget
   `(widget-field ((,solanum-class (:background ,solanum-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,solanum-class (:background ,solanum-tab-3))))

   ;; package
   `(package-status-dependency ((,solanum-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,solanum-class (:weight bold :foreground ,solanum-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,solanum-class (:weight bold :foreground ,solanum-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,solanum-class (:foreground ,solanum-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,solanum-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,solanum-class (:underline (:style wave :color ,solanum-success)))))
   `(flymake-warning ((,solanum-class (:underline (:style wave :color ,solanum-warning)))))
   `(flymake-error ((,solanum-class (:underline (:style wave :color ,solanum-error)))))

   ;; eww
   `(eww-valid-certificate ((,solanum-class (:weight bold :foreground ,solanum-success))))
   `(eww-invalid-certificate ((,solanum-class (:weight bold :foreground ,solanum-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,solanum-class (:foreground ,solanum-fg-1 :background ,solanum-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,solanum-class (:foreground ,solanum-fg-1 :background ,solanum-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,solanum-class (:foreground ,solanum-fg-1 :background ,solanum-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,solanum-class (:foreground ,solanum-fg-1 :background ,solanum-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,solanum-class (:foreground ,solanum-fg-1 :background ,solanum-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,solanum-class (:foreground ,solanum-fg-1 :background ,solanum-tab-3 :box (:line-width 1 :color ,solanum-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,solanum-class (:foreground ,solanum-light-blue))))
   `(elfeed-search-date-face ((,solanum-class (:foreground ,solanum-purple))))
   `(elfeed-search-feed-face ((,solanum-class (:foreground ,solanum-light-green))))
   `(elfeed-search-title-face ((,solanum-class (:foreground ,solanum-alt-dark-purple))))
   `(elfeed-search-unread-title-face ((,solanum-class (:weight bold :foreground ,solanum-deep-green))))
   `(elfeed-search-filter-face ((,solanum-class (:weight bold :foreground ,solanum-light-purple))))
   `(elfeed-search-last-update-face ((,solanum-class (:weight bold :foreground ,solanum-light-purple))))
   `(elfeed-search-unread-count-face ((,solanum-class (:weight bold :foreground ,solanum-light-purple))))

   `(elfeed-show-header-face ((,solanum-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,solanum-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,solanum-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,solanum-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,solanum-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,solanum-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,solanum-class (:foreground ,solanum-red-tomato :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,solanum-class (:foreground ,solanum-red-tomato :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,solanum-class (:foreground ,solanum-yellow-potato :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,solanum-class (:foreground ,solanum-yellow-potato :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,solanum-class (:foreground ,solanum-yellow-potato :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,solanum-class (:foreground ,solanum-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,solanum-class (:foreground ,solanum-fg-1 :background ,solanum-bg))))
   `(corfu-current ((,solanum-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,solanum-class (:background ,solanum-shadow))))
   `(corfu-border ((,solanum-class (:background ,solanum-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,solanum-class (:foreground ,solanum-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,solanum-class (:foreground ,solanum-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,solanum-class (:foreground ,solanum-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,solanum-class (:foreground ,solanum-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,solanum-class (:foreground ,solanum-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,solanum-class (:foreground ,solanum-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,solanum-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,solanum-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,solanum-class (:inherit success))))
   `(envrc-mode-line-none-face ((,solanum-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,solanum-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,solanum-class (:weight bold :background ,solanum-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,solanum-class (:foreground ,solanum-white :background ,solanum-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,solanum-class (:foreground ,solanum-black :background ,solanum-fl-builtin :inherit region))))

   ;; keycast
   `(keycast-command ((,solanum-class (:weight bold))))
   `(keycast-key ((,solanum-class (:foreground ,solanum-white :background ,solanum-tab-3 :box (:line-width -3 :style released-button) :inherit fixed-pitch))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,solanum-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-solanum)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-solanum-theme.el ends here
