;;; guava-themes-ficus-theme.el --- A theme inspired by fig colors -*- lexical-binding: t; -*-

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
;; A theme inspired by fig colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-ficus "A theme inspired by fig colors.")

(let* (
      (ficus-class '((class color) (min-colors 257)))
      (ficus-black                     "#000000")
      (ficus-white                     "#FFFFFF")

      (ficus-light-gray                "#9196a5")
      (ficus-gray                      "#646464")

      (ficus-light-green               "#6e7d64")
      (ficus-green                     "#3c7328")
      (ficus-alt-green                 "#44695a")

      (ficus-light-red                 "#af5050")
      (ficus-red                       "#82322d")
      ;; (ficus-deep-red                  "#693c50")

      (ficus-light-brown               "#b49d97")

      (ficus-light-blue                "#46cee0")
      ;; (ficus-alt-light-blue            "#96B9E6")
      ;; (ficus-blue                      "#2327dc")
      (ficus-blue                      "#2d8296")
      (ficus-deep-blue                 "#465a6e")
      (ficus-cyan                      "#00ffff")
      (ficus-alt-cyan                  "#008cd2")

      (ficus-purple-red                "#874164")

      (ficus-fg-1                      "#d7d7d7")
      (ficus-fg-2                      "#878787")
      (ficus-bg                        "#232020")
      (ficus-highlight                 "#322f2f")
      (ficus-shadow                    "#7b7b7b")
      (ficus-tooltip                   "#ffffe0")
      (ficus-region-1                  "#464b50")
      (ficus-region-2                  "#46504b")

      (ficus-error                     "#f50000")
      (ficus-warning                   "#f6c911")
      (ficus-success                   "#1ea01e")
      (ficus-change                    "#144be7")

      (ficus-mode-line                 "#b05051")
      (ficus-mode-line-inactive        "#967f79")
      (ficus-mode-line-modified        "#46cee0")
      (ficus-bell                      "#5082B0")


      (ficus-tab-1                     ficus-mode-line)
      (ficus-tab-2                     ficus-mode-line-inactive)
      (ficus-tab-3                     ficus-light-brown)

      (ficus-fl-comment                ficus-light-green)
      (ficus-fl-string                 ficus-purple-red)
      (ficus-fl-keyword                ficus-red)
      (ficus-fl-builtin                ficus-light-red)
      (ficus-fl-type                   ficus-green)
      (ficus-fl-function-name          ficus-deep-blue)
      (ficus-fl-variable-name          ficus-alt-green)
      (ficus-fl-constant               ficus-blue)
      (ficus-fl-warning                ficus-warning)
      (ficus-fl-punctuation            ficus-fg-2)
      (ficus-fl-negation-char          ficus-alt-cyan)

      (ficus-diff-added                "#5aa05a")
      (ficus-diff-removed              "#a05a5a")
      (ficus-diff-refine-added         "#007800")
      (ficus-diff-refine-removed       "#780000")
      (ficus-diff-header               "#5a5a5a")
      (ficus-diff-file-header          "#3c3c3c")
      (ficus-diff-context              "#828282")
      (ficus-smerge-base               "#5A5AA0")

      (ficus-square-a-0                "#a550be")
      (ficus-square-a-1                "#28a03c")
      (ficus-square-a-2                "#ff6400")
      (ficus-square-a-3                "#3c82ff")

      (ficus-complementary-a-0         "#4680DE")
      (ficus-complementary-a-1         "#DE4680"))

  (custom-theme-set-faces
   'guava-themes-ficus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,ficus-class (:foreground ,ficus-fg-1 :background ,ficus-bg))))

   ;; error, warning, success
   `(error ((,ficus-class (:foreground ,ficus-error :weight bold))))
   `(warning ((,ficus-class (:foreground ,ficus-warning :weight bold))))
   `(success ((,ficus-class (:foreground ,ficus-success :weight bold))))

   ;; highlight
   `(highlight ((,ficus-class (:background ,ficus-highlight))))

   ;; shadow
   `(shadow ((,ficus-class (:foreground ,ficus-shadow))))

   ;; tooltip
   `(tooltip ((,ficus-class (:weight bold :foreground ,ficus-black :background ,ficus-tooltip :inherit variable-pitch))))

   ;; region
   `(region ((,ficus-class (:background ,ficus-region-1 :extend t))))
   `(secondary-selection ((,ficus-class (:background ,ficus-region-2 :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,ficus-class (:foreground ,ficus-fl-comment :weight medium))))
   `(font-lock-string-face ((,ficus-class (:foreground ,ficus-fl-string :weight medium))))
   `(font-lock-keyword-face ((,ficus-class (:foreground ,ficus-fl-keyword :weight medium))))
   `(font-lock-builtin-face ((,ficus-class (:foreground ,ficus-fl-builtin :weight medium))))
   `(font-lock-type-face ((,ficus-class (:foreground ,ficus-fl-type :weight medium))))
   `(font-lock-function-name-face ((,ficus-class (:foreground ,ficus-fl-function-name :weight medium))))
   `(font-lock-variable-name-face ((,ficus-class (:foreground ,ficus-fl-variable-name :weight medium))))
   `(font-lock-constant-face ((,ficus-class (:foreground ,ficus-fl-constant :weight medium))))
   `(font-lock-warning-face ((,ficus-class (:foreground ,ficus-fl-warning :weight bold))))
   `(font-lock-punctuation-face ((,ficus-class (:foreground ,ficus-fl-punctuation :weight medium))))
   `(font-lock-negation-char-face ((,ficus-class (:foreground ,ficus-fl-negation-char :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,ficus-class (:foreground ,ficus-white :background ,ficus-mode-line))))

   ;; fringe
   `(fringe ((,ficus-class (:foreground ,ficus-light-red :background ,ficus-bg))))
   `(diff-hl-change ((,ficus-class (:foreground ,ficus-change :background ,ficus-change))))
   `(diff-hl-insert ((,ficus-class (:foreground ,ficus-success :background ,ficus-success))))
   `(diff-hl-delete ((,ficus-class (:foreground ,ficus-error :background ,ficus-error))))

   ;; line-number
   `(line-number ((,ficus-class (:foreground ,ficus-fg-1 :inherit default))))
   `(line-number-current-line ((,ficus-class (:foreground ,ficus-mode-line :weight bold :inherit (highlight line-number)))))
   `(line-number-minor-tick ((,ficus-class (:background ,ficus-tab-2 :inherit line-number))))
   `(line-number-major-tick ((,ficus-class (:background ,ficus-tab-3 :inherit line-number))))

   ;; mode-line
   `(mode-line ((,ficus-class (:foreground ,ficus-white :background ,ficus-mode-line
                                                :box ,(if guava-themes-boxed-mode-line
                                                          '(:line-width -1 :style released-button)
                                                        'unspecified)))))
   `(mode-line-inactive ((,ficus-class (:background ,ficus-mode-line-inactive :inherit mode-line))))
   `(mode-line-emphasis ((,ficus-class (:weight bold))))
   `(mode-line-highlight ((,ficus-class (:foreground unspecified :background unspecified :box (:line-width 2 :color "grey40" :style released-button)))))
   `(guava-themes-visible-bell ((,ficus-class (:foreground ,ficus-white :background ,ficus-bell))))

   ;; minibuffer
   `(minibuffer-prompt ((,ficus-class (:weight bold :foreground ,ficus-mode-line))))

   ;; borders
   `(vertical-border ((,ficus-class (:foreground ,ficus-mode-line))))

   ;; header-line
   `(header-line ((,ficus-class (:inherit mode-line))))
   `(which-func ((,ficus-class (:weight bold :foreground ,ficus-mode-line-modified))))

   ;; tab-bar
   `(tab-bar ((,ficus-class (:foreground ,ficus-white :background ,ficus-tab-2 :weight bold :inherit mode-line))))
   `(tab-bar-tab ((,ficus-class (:foreground ,ficus-white :background ,ficus-tab-1 :inherit tab-bar))))
   `(tab-bar-tab-inactive ((,ficus-class (:foreground ,ficus-white :background ,ficus-tab-2 :inherit tab-bar-tab))))

   ;; tab-line
   `(tab-line ((,ficus-class (:foreground ,ficus-white :background ,ficus-tab-2 :weight bold :inherit mode-line))))
   `(tab-line-tab ((,ficus-class (:foreground ,ficus-white :background ,ficus-tab-2 :inherit tab-line))))
   `(tab-line-tab-current ((,ficus-class (:foreground ,ficus-white :background ,ficus-tab-1 :inherit tab-line-tab))))
   `(tab-line-tab-inactive ((,ficus-class (:foreground ,ficus-white :background ,ficus-tab-2 :inherit tab-line-tab))))
   `(tab-line-tab-inactive-alternate ((,ficus-class (:foreground ,ficus-white :background ,ficus-tab-3 :inherit tab-line-tab))))
   `(tab-line-tab-modified ((,ficus-class (:foreground ,ficus-mode-line-modified :weight bold))))
   `(tab-line-tab-special ((,ficus-class (:slant italic :weight bold))))
   `(tab-line-highlight ((,ficus-class (:foreground unspecified :background unspecified :box (:line-width 2 :style released-button)))))

   ;; parentheses
   `(show-paren-match ((,ficus-class (:foreground ,ficus-white :background ,ficus-bell))))
   `(show-paren-mismatch ((,ficus-class (:foreground ,ficus-white :background ,ficus-error :inherit show-paren-match))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,ficus-class (:background ,ficus-error))))

   ;; links
   `(link ((,ficus-class (:foreground ,ficus-blue :underline t :weight bold))))
   `(link-visited ((,ficus-class (:foreground ,ficus-deep-blue :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,ficus-class (:foreground ,ficus-deep-blue :weight medium))))
   `(outline-2 ((,ficus-class (:foreground ,ficus-gray :weight medium))))
   `(outline-3 ((,ficus-class (:foreground ,ficus-red :weight medium))))
   `(outline-4 ((,ficus-class (:foreground ,ficus-light-green :weight medium))))
   `(outline-5 ((,ficus-class (:inherit outline-1))))
   `(outline-6 ((,ficus-class (:inherit outline-2))))
   `(outline-7 ((,ficus-class (:inherit outline-3))))
   `(outline-8 ((,ficus-class (:inherit outline-4))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,ficus-class (:foreground ,ficus-cyan))))
   `(escape-glyph ((,ficus-class (:inherit homoglyph))))
   `(nobreak-space ((,ficus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))
   `(nobreak-hyphen ((,ficus-class (:inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,ficus-class (:background ,ficus-alt-cyan))))

   ;; help-key-binding
   `(help-key-binding ((,ficus-class (:foreground ,ficus-light-red :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,ficus-class (:foreground ,ficus-white :background ,ficus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,ficus-class (:foreground ,ficus-white :background ,ficus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,ficus-class (:foreground ,ficus-white :background ,ficus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,ficus-class (:foreground ,ficus-white :background ,ficus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,ficus-class (:foreground ,ficus-white :background ,ficus-diff-header :extend t))))
   `(diff-file-header ((,ficus-class (:weight bold :foreground ,ficus-white :background ,ficus-diff-file-header :extend t))))
   `(diff-context ((,ficus-class (:foreground ,ficus-white :background ,ficus-diff-context :extend t))))

   ;; smerge
   `(smerge-lower ((,ficus-class (:extend t :inherit diff-added))))
   `(smerge-upper ((,ficus-class (:extend t :inherit diff-removed))))
   `(smerge-markers ((,ficus-class (:extend t :inherit diff-context))))
   `(smerge-refined-added ((,ficus-class (:extend t :inherit diff-refine-added))))
   `(smerge-refined-removed ((,ficus-class (:extend t :inherit diff-refine-removed))))
   `(smerge-base ((,ficus-class (:foreground ,ficus-white :background ,ficus-smerge-base :extend t))))

   ;; completions
   `(completions-common-part ((,ficus-class (:foreground ,ficus-warning :weight bold))))
   `(completions-first-difference ((,ficus-class (:foreground ,ficus-error :weight bold))))

   ;; org-faces
   `(org-todo ((,ficus-class (:foreground ,ficus-error :weight bold))))
   `(org-done ((,ficus-class (:foreground ,ficus-success :weight bold))))
   `(org-hide ((,ficus-class (:foreground ,ficus-bg))))
   `(org-table ((,ficus-class (:foreground ,ficus-light-red))))
   `(org-table-header ((,ficus-class (:inverse-video t :inherit org-table))))
   `(org-date ((,ficus-class (:foreground ,ficus-purple-red))))
   `(org-date-selected ((,ficus-class (:foreground unspecified :inverse-video t :inherit org-date))))
   `(org-headline-todo ((,ficus-class (:foreground ,ficus-square-a-2))))
   `(org-headline-done ((,ficus-class (:foreground ,ficus-square-a-1))))
   `(org-document-title ((,ficus-class (:inherit font-lock-keyword-face))))
   `(org-document-info-keyword ((,ficus-class (:inherit shadow))))
   `(org-meta-line ((,ficus-class (:inherit font-lock-comment-face))))

   ;; window-divider
   `(window-divider ((,ficus-class (:foreground ,ficus-tab-3))))
   `(window-divider-first-pixel ((,ficus-class (:inherit window-divider))))
   `(window-divider-last-pixel ((,ficus-class (:inherit window-divider))))

   ;; isearch (use "M-x isearch-forward-regexp foo-\([0-9]+\)\([a-z]+\)" to check the group faces)
   `(isearch ((,ficus-class (:foreground ,ficus-white :background ,ficus-square-a-2))))
   `(isearch-fail ((,ficus-class (:foreground ,ficus-white :background ,ficus-error))))
   `(lazy-highlight ((,ficus-class (:foreground ,ficus-white :background ,ficus-square-a-3))))
   `(isearch-group-1 ((,ficus-class (:foreground ,ficus-white :background ,ficus-square-a-0))))
   `(isearch-group-2 ((,ficus-class (:foreground ,ficus-white :background ,ficus-square-a-1))))

   ;; replace (use "M-x occur" to check the match face)
   `(query-replace ((,ficus-class (:inherit isearch))))
   `(match ((,ficus-class (:inherit lazy-highlight))))

   ;; custom
   ;; (progn (setopt custom-raised-buttons nil) (setq mouse-highlight t))
   `(custom-button ((,ficus-class (:foreground ,ficus-white :background ,ficus-mode-line :box (:line-width 2 :style released-button)))))
   `(custom-button-pressed ((,ficus-class (:foreground ,ficus-white :background ,ficus-mode-line :box (:line-width 2 :style pressed-button)))))
   `(custom-button-mouse ((,ficus-class (:foreground ,ficus-white :background ,ficus-tab-3 :box (:line-width 2 :style released-button)))))
   `(custom-button-unraised ((,ficus-class (:foreground ,ficus-mode-line :inherit underline))))
   `(custom-button-pressed-unraised ((,ficus-class (:foreground ,ficus-bell :inherit custom-button-unraised))))
   `(custom-comment ((,ficus-class (:background ,ficus-tab-3))))
   `(custom-variable-tag ((,ficus-class (:weight bold :foreground ,ficus-tab-2))))
   `(custom-group-tag ((,ficus-class (:height 1.2 :inherit (custom-variable-tag variable-pitch)))))
   `(custom-state ((,ficus-class (:foreground ,ficus-fl-keyword))))

   ;; widget
   `(widget-field ((,ficus-class (:background ,ficus-tab-3 :box (:line-width (1 . -1) :color "gray46") :extend t))))
   `(widget-single-line-field ((,ficus-class (:background ,ficus-tab-3))))

   ;; package
   `(package-status-dependency ((,ficus-class (:inherit (font-lock-string-face)))))

   ;; icomplete
   `(icomplete-selected-match ((,ficus-class (:weight bold :foreground ,ficus-mode-line :inherit (highlight)))))
   `(icomplete-first-match ((,ficus-class (:weight bold :foreground ,ficus-mode-line :inherit (highlight)))))

   ;; bookmark
   `(bookmark-face ((,ficus-class (:foreground ,ficus-mode-line :distant-foreground unspecified))))
   `(bookmark-menu-bookmark ((,ficus-class (:weight bold :inherit bookmark-face))))

   ;; flymake
   `(flymake-note ((,ficus-class (:underline (:style wave :color ,ficus-success)))))
   `(flymake-warning ((,ficus-class (:underline (:style wave :color ,ficus-warning)))))
   `(flymake-error ((,ficus-class (:underline (:style wave :color ,ficus-error)))))

   ;; eww
   `(eww-valid-certificate ((,ficus-class (:weight bold :foreground ,ficus-success))))
   `(eww-invalid-certificate ((,ficus-class (:weight bold :foreground ,ficus-error))))
   ;; Example of eww-form-text: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-text ((,ficus-class (:foreground ,ficus-black :background ,ficus-tab-3 :box (:line-width 1)))))
   ;; Example of eww-form-file: https://html.com/input-type-file/
   `(eww-form-file ((,ficus-class (:foreground ,ficus-fg-1 :background ,ficus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-select: http://savannah.gnu.org/projects/emacs/
   ;; Example of eww-form-select: https://git.kernel.org/pub/scm/editors/uemacs/uemacs.git/
   `(eww-form-select ((,ficus-class (:foreground ,ficus-fg-1 :background ,ficus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-submit: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-submit ((,ficus-class (:foreground ,ficus-fg-1 :background ,ficus-tab-2 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-checkbox: https://en.wikipedia.org/wiki/Main_Page
   `(eww-form-checkbox ((,ficus-class (:foreground ,ficus-fg-1 :background ,ficus-tab-1 :box (:line-width 2 :style released-button)))))
   ;; Example of eww-form-textarea: https://en.wikipedia.org/w/index.php?title=Main_Page&action=edit
   `(eww-form-textarea ((,ficus-class (:foreground ,ficus-black :background ,ficus-tab-3 :box (:line-width 1 :color ,ficus-tab-1)))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,ficus-class (:foreground ,ficus-light-green))))
   `(elfeed-search-date-face ((,ficus-class (:foreground ,ficus-deep-blue))))
   `(elfeed-search-feed-face ((,ficus-class (:foreground ,ficus-purple-red))))
   `(elfeed-search-title-face ((,ficus-class (:foreground ,ficus-gray))))
   `(elfeed-search-unread-title-face ((,ficus-class (:weight bold :foreground ,ficus-green))))
   `(elfeed-search-filter-face ((,ficus-class (:weight bold :foreground ,ficus-light-blue))))
   `(elfeed-search-last-update-face ((,ficus-class (:weight bold :foreground ,ficus-light-blue))))
   `(elfeed-search-unread-count-face ((,ficus-class (:weight bold :foreground ,ficus-light-blue))))

   `(elfeed-show-header-face ((,ficus-class (:inherit elfeed-search-date-face))))
   `(elfeed-show-author-face ((,ficus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-title-face ((,ficus-class (:inherit elfeed-search-unread-title-face))))
   `(elfeed-show-date-face ((,ficus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-feed-face ((,ficus-class (:inherit elfeed-search-feed-face))))
   `(elfeed-show-tags-face ((,ficus-class (:inherit elfeed-search-tag-face))))

   ;; doom-modeline
   ;; doom-modeline-buffer-file-name-style
   `(doom-modeline-project-name ((,ficus-class (:foreground ,ficus-deep-blue :inherit (doom-modeline bold italic)))))
   `(doom-modeline-project-dir ((,ficus-class (:foreground ,ficus-deep-blue :inherit (doom-modeline bold)))))
   `(doom-modeline-project-root-dir ((,ficus-class (:foreground ,ficus-light-gray :inherit (doom-modeline bold)))))
   `(doom-modeline-project-parent-dir ((,ficus-class (:foreground ,ficus-light-gray :inherit (doom-modeline bold)))))
   `(doom-modeline-buffer-minor-mode ((,ficus-class (:foreground ,ficus-light-gray :inherit (doom-modeline)))))
   `(doom-modeline-buffer-modified ((,ficus-class (:foreground ,ficus-mode-line-modified :inherit (doom-modeline warning bold)))))

   ;; corfu
   `(corfu-default ((,ficus-class (:foreground ,ficus-fg-1 :background ,ficus-bg))))
   `(corfu-current ((,ficus-class (:foreground unspecified :background unspecified :extend t :inherit highlight))))
   `(corfu-bar ((,ficus-class (:background ,ficus-shadow))))
   `(corfu-border ((,ficus-class (:background ,ficus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,ficus-class (:foreground ,ficus-square-a-0 :weight bold))))
   `(orderless-match-face-1 ((,ficus-class (:foreground ,ficus-square-a-1 :weight bold))))
   `(orderless-match-face-2 ((,ficus-class (:foreground ,ficus-square-a-2 :weight bold))))
   `(orderless-match-face-3 ((,ficus-class (:foreground ,ficus-square-a-3 :weight bold))))

   ;; prescient (vertico-prescient-enable-filtering, corfu-prescient-enable-filtering)
   `(prescient-primary-highlight ((,ficus-class (:foreground ,ficus-complementary-a-0 :weight bold))))
   `(prescient-secondary-highlight ((,ficus-class (:foreground ,ficus-complementary-a-1 :underline t :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,ficus-class (:inherit error))))
   `(envrc-mode-line-denied-face ((,ficus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,ficus-class (:inherit success))))
   `(envrc-mode-line-none-face ((,ficus-class (:inherit warning))))
   `(envrc-mode-line-running-face ((,ficus-class (:inherit warning))))

   ;; devdocs
   `(devdocs-code-block ((,ficus-class (:weight bold :background ,ficus-highlight :extend t))))

   ;; pdf-tools
   `(pdf-view-region ((,ficus-class (:foreground ,ficus-white :background ,ficus-square-a-3 :inherit region))))

   ;; idle-highlight
   `(idle-highlight ((,ficus-class (:foreground ,ficus-white :background ,ficus-fl-builtin :inherit region))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,ficus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-ficus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-ficus-theme.el ends here
