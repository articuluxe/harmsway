;;; guava-themes-cordyline-theme.el --- A theme inspired by the ti plant colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 12, 2026
;; Version: 0.14.0
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
;; A theme inspired by the ti plant colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-cordyline "A theme inspired by the ti plant colors.")

(let* (
      (cordyline-class '((class color) (min-colors 257)))
      ;; (cordyline-black                     "#000000")
      (cordyline-white                     "#FFFFFF")

      (cordyline-shadow                    "#b3b3b3")

      (cordyline-orange-red                "#ce462c")
      (cordyline-pink-red                  "#cb646e");CB5F68
      (cordyline-pink-purple               "#da70d6")
      (cordyline-deep-fuchsia              "#6e1551");69374e,741551

      (cordyline-light-blue                "#5f70cb")
      (cordyline-blue                      "#3150af");1330af
      (cordyline-deep-blue                 "#214bd5");2134d5
      (cordyline-steel-blue                "#4f94cd")
      (cordyline-cyan                      "#00ffff")
      (cordyline-dark-cyan                 "#007896")

      (cordyline-light-green               "#c5ff6e")
      (cordyline-alt-light-green           "#afd2b9");006455,00512c
      (cordyline-green                     "#005f55");006455

      (cordyline-light-purple              "#a246d1");8b1cb0,9c1cbc,991cbc,a21cd1,a226d1
      (cordyline-alt-light-purple          "#a29cf1")
      (cordyline-purple                    "#7050af");54366d,583675,583683,583d83,59338f,673b94,703aaf,7044af
      (cordyline-deep-purple               "#392b38");3f323c
      (cordyline-alt-purple                "#493d4e");433640,43364c,473b4c
      (cordyline-purple-red                "#983251");8d2a46,982a46,982a51
      (cordyline-alt-deep-purple           "#211730")

      (cordyline-error                     "#FF0000")
      (cordyline-warning                   "#f6d909");F68511
      (cordyline-success                   "#23a334");239834

      (cordyline-diff-added                "#5aa05a");335533
      (cordyline-diff-removed              "#a05a5a");553333
      (cordyline-diff-refine-added         "#007800");22aa22
      (cordyline-diff-refine-removed       "#780000");aa2222
      (cordyline-diff-header               "#5a5a5a");737373
      (cordyline-diff-file-header          "#3c3c3c");999999
      (cordyline-diff-context              "#828282");999999

      (cordyline-orderless-0               "#af50b9")
      (cordyline-orderless-1               "#28a03c")
      (cordyline-orderless-2               "#ff6400")
      (cordyline-orderless-3               "#3c82ff")

      (cordyline-vc-change                 cordyline-deep-blue)
      (cordyline-vc-insert                 cordyline-success)
      (cordyline-vc-delete                 cordyline-error))

  (custom-theme-set-faces
   'guava-themes-cordyline

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-deep-purple))))

   ;; error, warning, success
   `(error ((,cordyline-class (:foreground ,cordyline-error :weight bold))))
   `(warning ((,cordyline-class (:foreground ,cordyline-warning :weight bold))))
   `(success ((,cordyline-class (:foreground ,cordyline-success :weight bold))))

   ;; highlight
   `(highlight ((,cordyline-class (:background ,cordyline-alt-purple))))

   ;; shadow
   `(shadow ((,cordyline-class (:foreground ,cordyline-shadow))))

   ;; region
   `(region ((,cordyline-class (:background ,cordyline-deep-fuchsia))))
   `(secondary-selection ((,cordyline-class (:background ,cordyline-green :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,cordyline-class (:foreground ,cordyline-light-blue :weight medium))))
   `(font-lock-string-face ((,cordyline-class (:foreground ,cordyline-pink-red :weight medium))))
   `(font-lock-keyword-face ((,cordyline-class (:foreground ,cordyline-purple :weight medium))))
   `(font-lock-builtin-face ((,cordyline-class (:foreground ,cordyline-purple-red :weight medium))))
   `(font-lock-warning-face ((,cordyline-class (:foreground ,cordyline-warning :weight bold))))
   `(font-lock-type-face ((,cordyline-class (:foreground ,cordyline-light-purple :weight medium))))
   `(font-lock-constant-face ((,cordyline-class (:foreground ,cordyline-dark-cyan :weight medium))))
   `(font-lock-function-name-face ((,cordyline-class (:foreground ,cordyline-pink-purple :weight medium))))
   `(font-lock-punctuation-face ((,cordyline-class (:foreground ,cordyline-alt-light-green :weight medium))))
   `(font-lock-variable-name-face ((,cordyline-class (:foreground ,cordyline-blue :weight medium))))
   `(font-lock-negation-char-face ((,cordyline-class (:foreground ,cordyline-orange-red :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-pink-red))))

   ;; fringe
   `(fringe ((,cordyline-class (:foreground ,cordyline-light-green :background ,cordyline-deep-purple))))
   `(diff-hl-change ((,cordyline-class (:foreground ,cordyline-vc-change :background ,cordyline-vc-change))))
   `(diff-hl-insert ((,cordyline-class (:foreground ,cordyline-vc-insert :background ,cordyline-vc-insert))))
   `(diff-hl-delete ((,cordyline-class (:foreground ,cordyline-vc-delete :background ,cordyline-vc-delete))))

   ;; line-number
   `(line-number ((,cordyline-class (:foreground ,cordyline-white))))
   `(line-number-current-line ((,cordyline-class (:foreground ,cordyline-light-blue :weight bold :inherit highlight))))
   `(line-number-minor-tick ((,cordyline-class (:background ,cordyline-alt-light-purple :inherit line-number))))
   `(line-number-major-tick ((,cordyline-class (:background ,cordyline-light-purple :inherit line-number))))

   ;; mode-line
   `(mode-line ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-purple-red))))
   `(mode-line-inactive ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-alt-deep-purple))))
   `(guava-themes-visible-bell ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-light-blue))))

   ;; minibuffer
   `(minibuffer-prompt ((,cordyline-class (:foreground ,cordyline-white))))

   ;; borders
   `(vertical-border ((,cordyline-class (:foreground ,cordyline-purple-red))))

   ;; header-line
   `(header-line ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-purple-red))))
   `(which-func ((,cordyline-class (:foreground ,cordyline-white))))

   ;; tab-bar
   `(tab-bar ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-deep-purple))))
   `(tab-bar-tab ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-purple-red :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-deep-purple :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-deep-purple))))
   `(tab-line-tab ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-alt-deep-purple :weight bold :height 0.9))))
   `(tab-line-tab-current ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-purple-red :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-deep-purple :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-alt-purple :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,cordyline-class (:foreground ,cordyline-steel-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,cordyline-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-steel-blue))))
   `(show-paren-mismatch ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,cordyline-class (:background ,cordyline-error))))

   ;; links
   `(link ((,cordyline-class (:foreground ,cordyline-steel-blue :underline t :weight bold))))
   `(link-visited ((,cordyline-class (:foreground ,cordyline-purple :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,cordyline-class (:foreground ,cordyline-pink-purple :weight medium))))
   `(outline-2 ((,cordyline-class (:foreground ,cordyline-blue :weight medium))))
   `(outline-3 ((,cordyline-class (:foreground ,cordyline-purple :weight medium))))
   `(outline-4 ((,cordyline-class (:foreground ,cordyline-light-blue :weight medium))))
   `(outline-5 ((,cordyline-class (:foreground ,cordyline-light-purple :weight medium))))
   `(outline-6 ((,cordyline-class (:foreground ,cordyline-dark-cyan :weight medium))))
   `(outline-7 ((,cordyline-class (:foreground ,cordyline-purple-red :weight medium))))
   `(outline-8 ((,cordyline-class (:foreground ,cordyline-pink-red :weight medium))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,cordyline-class (:foreground ,cordyline-cyan))))
   `(escape-glyph ((,cordyline-class (:inherit homoglyph))))
   `(nobreak-space ((,cordyline-class (:box (:line-width (2 . 2)) :inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,cordyline-class (:background ,cordyline-steel-blue))))

   ;; help-key-binding
   `(help-key-binding ((,cordyline-class (:foreground ,cordyline-alt-light-purple :background "grey19" :box (:line-width (-1 . -1) :color "grey35") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-header :extend t))))
   `(diff-file-header ((,cordyline-class (:weight bold :foreground ,cordyline-white :background ,cordyline-diff-file-header :extend t))))
   `(diff-context ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-diff-context :extend t))))

   ;; completions
   `(completions-common-part ((,cordyline-class (:foreground ,cordyline-warning :weight bold))))
   `(completions-first-difference ((,cordyline-class (:foreground ,cordyline-error :weight bold))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,cordyline-class (:foreground ,cordyline-light-blue))))
   `(elfeed-search-date-face ((,cordyline-class (:foreground ,cordyline-green))))
   `(elfeed-search-feed-face ((,cordyline-class (:foreground ,cordyline-dark-cyan))))
   `(elfeed-search-title-face ((,cordyline-class (:foreground ,cordyline-pink-red))))
   `(elfeed-search-filter-face ((,cordyline-class (:foreground ,cordyline-alt-light-purple))))
   `(elfeed-search-last-update-face ((,cordyline-class (:foreground ,cordyline-steel-blue))))
   `(elfeed-search-unread-title-face ((,cordyline-class (:weight bold :foreground ,cordyline-light-purple))))
   `(elfeed-search-unread-count-face ((,cordyline-class (:weight bold :foreground ,cordyline-light-green))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,cordyline-class (:foreground ,cordyline-steel-blue :inherit italic))))
   `(doom-modeline-project-parent-dir ((,cordyline-class (:foreground ,cordyline-steel-blue))))
   `(doom-modeline-buffer-minor-mode ((,cordyline-class (:foreground ,cordyline-pink-red))))

   ;; corfu
   `(corfu-default ((,cordyline-class (:foreground ,cordyline-white :background ,cordyline-deep-purple))))
   `(corfu-current ((,cordyline-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,cordyline-class (:background ,cordyline-shadow))))
   `(corfu-border ((,cordyline-class (:background ,cordyline-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,cordyline-class (:foreground ,cordyline-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,cordyline-class (:foreground ,cordyline-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,cordyline-class (:foreground ,cordyline-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,cordyline-class (:foreground ,cordyline-orderless-3 :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,cordyline-class (:inherit error))))
   `(envrc-mode-line-none-face ((,cordyline-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,cordyline-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,cordyline-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-cordyline)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-cordyline-theme.el ends here
