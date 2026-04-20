;;; guava-themes-acer-theme.el --- A theme inspired by maple colors -*- lexical-binding: t; -*-

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
;; A theme inspired by maple colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-acer "A theme inspired by maple colors.")

(let* (
      (acer-class '((class color) (min-colors 257)))
      (acer-black                     "#000000")
      (acer-white                     "#FFFFFF")

      (acer-shadow                    "#7f7f7f")

      (acer-green                     "#237c34");239834,237c34,3b7b27
      (acer-green-cyan                "#277a6a")
      (acer-deep-green                "#006041");8a8a8a,828282,787878,746c72,747474,606060,60607e,59597e,505069

      (acer-orange                    "#e76144");fe7457,e76144,d75541
      (acer-deep-orange               "#d44400");d44400,cd4100
      (acer-orange-pink               "#cd475f");ec31a3,ff514a,ff474a,ff475f
      (acer-yellow                    "#f7bb78");fca148,fca347,f5a24e,f5a44e,F7B36A,f7a95a,f7ac5f,f7bb78
      (acer-autumn                    "#ff8c4e");c14c5c,f46157,db3d32,e14337,ed7038,f77b44,f3814f,e8674a,ed674a,f68b47

      (acer-cream                     "#dca56e");f68e64,dc8e64,dc9964,dc9964
      (acer-brown                     "#c88550");7d4826,754014,a58464,a07f5f
      (acer-deep-brown                "#8c6950");7d6250,6a4e39,8c6a50

      (acer-light-blue                "#3a9187");bacce4
      (acer-blue                      "#2134d5")
      (acer-deep-blue                 "#1B3B4D")

      (acer-purple                    "#9e4d76")
      (acer-deep-purple               "#60366e");663c6c,62386c
      (acer-purple-red                "#9b234b");9f234b
      (acer-purple-pink               "#5f2258");233e4d

      (acer-error                     "#d70000");FF0000,c80000
      (acer-warning                   "#f0dc00");F68511,f2e16b,f0dc67
      (acer-success                   "#28823c");23D734,239834,237c34,23d934,50b450

      (acer-diff-added                "#c8f0c8");335533
      (acer-diff-removed              "#f0c8c8");553333
      (acer-diff-refine-added         "#78f078");22aa22
      (acer-diff-refine-removed       "#f07878");aa2222
      (acer-diff-header               "#b4b4b4");737373
      (acer-diff-file-header          "#8c8c8c");999999
      (acer-diff-context              "#dcdcdc");999999

      (acer-orderless-0               "#af2ab9")
      (acer-orderless-1               "#28823c")
      (acer-orderless-2               "#d44400")
      (acer-orderless-3               "#6496d7")

      (acer-vc-change                 acer-blue)
      (acer-vc-insert                 acer-success)
      (acer-vc-delete                 acer-error))

  (custom-theme-set-faces
   'guava-themes-acer

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,acer-class (:foreground ,acer-black :background ,acer-yellow))))

   ;; error, warning, success
   `(error ((,acer-class (:foreground ,acer-error :weight bold))))
   `(warning ((,acer-class (:foreground ,acer-warning :weight bold))))
   `(success ((,acer-class (:foreground ,acer-success :weight bold))))

   ;; highlight
   `(highlight ((,acer-class (:background ,acer-cream))))

   ;; shadow
   `(shadow ((,acer-class (:foreground ,acer-shadow))))

   ;; region
   `(region ((,acer-class (:background ,acer-brown))))
   `(secondary-selection ((,acer-class (:background ,acer-orange :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,acer-class (:foreground ,acer-deep-green :weight medium))))
   `(font-lock-string-face ((,acer-class (:foreground ,acer-deep-orange :weight medium))))
   `(font-lock-keyword-face ((,acer-class (:foreground ,acer-deep-purple :weight medium))))
   `(font-lock-builtin-face ((,acer-class (:foreground ,acer-purple :weight medium))))
   `(font-lock-warning-face ((,acer-class (:foreground ,acer-warning :weight bold))))
   `(font-lock-type-face ((,acer-class (:foreground ,acer-green :weight medium))))
   `(font-lock-constant-face ((,acer-class (:foreground ,acer-green-cyan :weight medium))))
   `(font-lock-function-name-face ((,acer-class (:foreground ,acer-purple-red :weight medium))))
   `(font-lock-punctuation-face ((,acer-class (:foreground ,acer-purple-pink :weight medium))))
   `(font-lock-variable-name-face ((,acer-class (:foreground ,acer-orange-pink :weight medium))))
   `(font-lock-negation-char-face ((,acer-class (:foreground ,acer-light-blue :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,acer-class (:foreground ,acer-black :background ,acer-orange))))

   ;; fringe
   `(fringe ((,acer-class (:foreground ,acer-deep-blue :background ,acer-yellow))))
   `(diff-hl-change ((,acer-class (:foreground ,acer-vc-change :background ,acer-vc-change))))
   `(diff-hl-insert ((,acer-class (:foreground ,acer-vc-insert :background ,acer-vc-insert))))
   `(diff-hl-delete ((,acer-class (:foreground ,acer-vc-delete :background ,acer-vc-delete))))

   ;; line-number
   `(line-number ((,acer-class (:foreground ,acer-black))))
   `(line-number-current-line ((,acer-class (:foreground ,acer-deep-orange :weight bold :inherit highlight))))
   `(line-number-minor-tick ((,acer-class (:background ,acer-brown :inherit line-number))))
   `(line-number-major-tick ((,acer-class (:background ,acer-deep-brown :inherit line-number))))

   ;; mode-line
   `(mode-line ((,acer-class (:foreground ,acer-white :background ,acer-orange))))
   `(mode-line-inactive ((,acer-class (:foreground ,acer-white :background ,acer-purple-red))))
   `(guava-themes-visible-bell ((,acer-class (:foreground ,acer-white :background ,acer-green))))

   ;; minibuffer
   `(minibuffer-prompt ((,acer-class (:foreground ,acer-black))))

   ;; borders
   `(vertical-border ((,acer-class (:foreground ,acer-orange))))

   ;; header-line
   `(header-line ((,acer-class (:foreground ,acer-white :background ,acer-orange))))
   `(which-func ((,acer-class (:foreground ,acer-white))))

   ;; tab-bar
   `(tab-bar ((,acer-class (:foreground ,acer-white :background ,acer-autumn))))
   `(tab-bar-tab ((,acer-class (:foreground ,acer-white :background ,acer-orange :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,acer-class (:foreground ,acer-white :background ,acer-autumn :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,acer-class (:foreground ,acer-white :background ,acer-autumn))))
   `(tab-line-tab ((,acer-class (:foreground ,acer-white :background ,acer-purple-red :weight bold :height 0.9))))
   `(tab-line-tab-current ((,acer-class (:foreground ,acer-white :background ,acer-orange :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,acer-class (:foreground ,acer-white :background ,acer-autumn :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,acer-class (:foreground ,acer-white :background ,acer-cream :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,acer-class (:foreground ,acer-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,acer-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,acer-class (:foreground ,acer-white :background ,acer-green))))
   `(show-paren-mismatch ((,acer-class (:foreground ,acer-white :background ,acer-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,acer-class (:background ,acer-error))))

   ;; links
   `(link ((,acer-class (:foreground ,acer-green-cyan :underline t :weight bold))))
   `(link-visited ((,acer-class (:foreground ,acer-blue :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,acer-class (:foreground ,acer-purple-red :weight medium))))
   `(outline-2 ((,acer-class (:foreground ,acer-orange-pink :weight medium))))
   `(outline-3 ((,acer-class (:foreground ,acer-deep-purple :weight medium))))
   `(outline-4 ((,acer-class (:foreground ,acer-deep-green :weight medium))))
   `(outline-5 ((,acer-class (:foreground ,acer-green :weight medium))))
   `(outline-6 ((,acer-class (:foreground ,acer-purple :weight medium))))
   `(outline-7 ((,acer-class (:foreground ,acer-green-cyan :weight medium))))
   `(outline-8 ((,acer-class (:foreground ,acer-deep-orange :weight medium))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,acer-class (:foreground ,acer-blue))))
   `(escape-glyph ((,acer-class (:inherit homoglyph))))
   `(nobreak-space ((,acer-class (:box (:line-width (2 . 2)) :inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,acer-class (:background ,acer-light-blue))))

   ;; help-key-binding
   `(help-key-binding ((,acer-class (:foreground ,acer-blue :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,acer-class (:foreground ,acer-black :background ,acer-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,acer-class (:foreground ,acer-black :background ,acer-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,acer-class (:foreground ,acer-black :background ,acer-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,acer-class (:foreground ,acer-black :background ,acer-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,acer-class (:foreground ,acer-black :background ,acer-diff-header :extend t))))
   `(diff-file-header ((,acer-class (:weight bold :foreground ,acer-black :background ,acer-diff-file-header :extend t))))
   `(diff-context ((,acer-class (:foreground ,acer-black :background ,acer-diff-context :extend t))))

   ;; completions
   `(completions-common-part ((,acer-class (:foreground ,acer-vc-change :weight bold))))
   `(completions-first-difference ((,acer-class (:foreground ,acer-error :weight bold))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,acer-class (:foreground ,acer-light-blue))))
   `(elfeed-search-date-face ((,acer-class (:foreground ,acer-deep-orange))))
   `(elfeed-search-feed-face ((,acer-class (:foreground ,acer-deep-purple))))
   `(elfeed-search-title-face ((,acer-class (:foreground ,acer-deep-brown))))
   `(elfeed-search-filter-face ((,acer-class (:foreground ,acer-purple-pink))))
   `(elfeed-search-last-update-face ((,acer-class (:foreground ,acer-purple-red))))
   `(elfeed-search-unread-title-face ((,acer-class (:weight bold :foreground ,acer-deep-green))))
   `(elfeed-search-unread-count-face ((,acer-class (:weight bold :foreground ,acer-green))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,acer-class (:foreground ,acer-deep-purple :inherit italic))))
   `(doom-modeline-project-parent-dir ((,acer-class (:foreground ,acer-deep-purple))))
   `(doom-modeline-buffer-minor-mode ((,acer-class (:foreground ,acer-purple))))

   ;; corfu
   `(corfu-default ((,acer-class (:foreground ,acer-black :background ,acer-yellow))))
   `(corfu-current ((,acer-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,acer-class (:background ,acer-shadow))))
   `(corfu-border ((,acer-class (:background ,acer-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,acer-class (:foreground ,acer-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,acer-class (:foreground ,acer-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,acer-class (:foreground ,acer-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,acer-class (:foreground ,acer-orderless-3 :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,acer-class (:inherit error))))
   `(envrc-mode-line-none-face ((,acer-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,acer-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,acer-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-acer)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-acer-theme.el ends here
