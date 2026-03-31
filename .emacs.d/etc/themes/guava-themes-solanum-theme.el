;;; guava-themes-solanum-theme.el --- A theme inspired by eggplant colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Feb 22, 2026
;; Version: 0.13.0
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
      (solanum-black             "#000000")
      (solanum-white             "#FFFFFF")

      (solanum-shadow            "#b3b3b3")

      (solanum-red-tomato        "#cd151f")
      (solanum-orange            "#e98c85")

      (solanum-yellow-potato     "#fde8b9")

      (solanum-light-green       "#4ec9b0");61ff96
      (solanum-alt-light-green   "#61ff96");2a4ad9,2a8a6d,448a7a,288f7a
      (solanum-green             "#3a6b43");3f7a56,2f824b,2f822f,2d872d
      (solanum-deep-green        "#007d37");0d6d4b,0d7c4b,207c31

      (solanum-light-blue        "#8ec4de")
      (solanum-blue              "#0d62b2")
      (solanum-cyan              "#00ffff")

      (solanum-light-purple      "#bec8ff");c4aeff,ccbcff,d4cbff
      (solanum-purple            "#9c69e8");9e7ae8,9c69e8
      (solanum-alt-purple        "#a394ff");9e7ae8,9c69e8
      (solanum-purple-red        "#64143d");6a143d,64143d
      (solanum-purple-pink       "#915d93");bb5d93,7d5d93
      (solanum-purple-blue       "#492b91")
      (solanum-dark-purple       "#672b5f");41143d,672b5f
      (solanum-purple-black      "#130d1a")
      (solanum-alt-purple-black  "#3b3542")

      (solanum-error             "#FF0000")
      (solanum-warning           "#f6d909");F68511
      (solanum-success           "#23a334");239834

      (solanum-vc-change         solanum-blue)
      (solanum-vc-insert         solanum-success)
      (solanum-vc-delete         solanum-error))

  (custom-theme-set-faces
   'guava-themes-solanum

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,solanum-class (:foreground ,solanum-white :background ,solanum-purple-black))))

   ;; error, warning, success
   `(error ((,solanum-class (:foreground ,solanum-error :weight bold))))
   `(warning ((,solanum-class (:foreground ,solanum-warning :weight bold))))
   `(success ((,solanum-class (:foreground ,solanum-success :weight bold))))

   ;; highlight
   `(highlight ((,solanum-class (:background ,solanum-purple-red))))

   ;; shadow
   `(shadow ((,solanum-class (:foreground ,solanum-shadow))))

   ;; region
   `(region ((,solanum-class (:background ,solanum-purple-blue))))
   `(secondary-selection ((,solanum-class (:background ,solanum-green :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,solanum-class (:foreground ,solanum-red-tomato :weight medium))))
   `(font-lock-string-face ((,solanum-class (:foreground ,solanum-yellow-potato :weight medium))))
   `(font-lock-keyword-face ((,solanum-class (:foreground ,solanum-deep-green :weight medium))))
   `(font-lock-builtin-face ((,solanum-class (:foreground ,solanum-light-blue :weight medium))))
   `(font-lock-warning-face ((,solanum-class (:foreground ,solanum-warning :weight bold))))
   `(font-lock-type-face ((,solanum-class (:foreground ,solanum-orange :weight medium))))
   `(font-lock-constant-face ((,solanum-class (:foreground ,solanum-light-purple :weight medium))))
   `(font-lock-function-name-face ((,solanum-class (:foreground ,solanum-purple :weight medium))))
   `(font-lock-punctuation-face ((,solanum-class (:foreground ,solanum-purple-pink :weight medium))))
   `(font-lock-variable-name-face ((,solanum-class (:foreground ,solanum-light-green :weight medium))))
   `(font-lock-negation-char-face ((,solanum-class (:foreground ,solanum-alt-light-green :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,solanum-class (:foreground ,solanum-black :background ,solanum-yellow-potato))))

   ;; fringe
   `(fringe ((,solanum-class (:foreground ,solanum-red-tomato :background ,solanum-purple-black))))
   `(diff-hl-change ((,solanum-class (:foreground ,solanum-vc-change :background ,solanum-vc-change))))
   `(diff-hl-insert ((,solanum-class (:foreground ,solanum-vc-insert :background ,solanum-vc-insert))))
   `(diff-hl-delete ((,solanum-class (:foreground ,solanum-vc-delete :background ,solanum-vc-delete))))

   ;; line-number
   `(line-number ((,solanum-class (:foreground ,solanum-white))))
   `(line-number-current-line ((,solanum-class (:foreground ,solanum-light-green :weight bold :inherit highlight))))
   `(line-number-minor-tick ((,solanum-class (:background ,solanum-alt-purple))))
   `(line-number-major-tick ((,solanum-class (:background ,solanum-purple-blue))))

   ;; mode-line
   `(mode-line ((,solanum-class (:foreground ,solanum-white :background ,solanum-dark-purple))))
   `(mode-line-inactive ((,solanum-class (:foreground ,solanum-white :background ,solanum-purple-red))))
   `(guava-themes-visible-bell ((,solanum-class (:foreground ,solanum-white :background ,solanum-orange))))

   ;; minibuffer
   `(minibuffer-prompt ((,solanum-class (:foreground ,solanum-yellow-potato))))

   ;; borders
   `(vertical-border ((,solanum-class (:foreground ,solanum-dark-purple))))

   ;; header-line
   `(header-line ((,solanum-class (:foreground ,solanum-white :background ,solanum-dark-purple))))
   `(which-func ((,solanum-class (:foreground ,solanum-white))))

   ;; tab-bar
   `(tab-bar ((,solanum-class (:foreground ,solanum-white :background ,solanum-purple-black))))
   `(tab-bar-tab ((,solanum-class (:foreground ,solanum-white :background ,solanum-dark-purple :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,solanum-class (:foreground ,solanum-white :background ,solanum-purple-black :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,solanum-class (:foreground ,solanum-white :background ,solanum-purple-black))))
   `(tab-line-tab ((,solanum-class (:foreground ,solanum-white :background ,solanum-purple-red :weight bold :height 0.9))))
   `(tab-line-tab-current ((,solanum-class (:foreground ,solanum-white :background ,solanum-dark-purple :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,solanum-class (:foreground ,solanum-white :background ,solanum-purple-black :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,solanum-class (:foreground ,solanum-white :background ,solanum-alt-purple-black :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,solanum-class (:foreground ,solanum-red-tomato :weight bold :height 0.9))))
   `(tab-line-tab-special ((,solanum-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,solanum-class (:foreground ,solanum-black :background ,solanum-light-purple))))
   `(show-paren-mismatch ((,solanum-class (:foreground ,solanum-white :background ,solanum-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,solanum-class (:background ,solanum-error))))

   ;; links
   `(link ((,solanum-class (:foreground ,solanum-yellow-potato :underline t :weight bold))))
   `(link-visited ((,solanum-class (:foreground ,solanum-orange :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,solanum-class (:foreground ,solanum-purple :weight medium))))
   `(outline-2 ((,solanum-class (:foreground ,solanum-light-green :weight medium))))
   `(outline-3 ((,solanum-class (:foreground ,solanum-red-tomato :weight medium))))
   `(outline-4 ((,solanum-class (:foreground ,solanum-deep-green :weight medium))))
   `(outline-5 ((,solanum-class (:foreground ,solanum-orange :weight medium))))
   `(outline-6 ((,solanum-class (:foreground ,solanum-light-purple :weight medium))))
   `(outline-7 ((,solanum-class (:foreground ,solanum-yellow-potato :weight medium))))
   `(outline-8 ((,solanum-class (:foreground ,solanum-light-blue :weight medium))))

   ;; homoglyph, escape-glyph, nobreak-space
   `(homoglyph ((,solanum-class (:foreground ,solanum-cyan))))
   `(escape-glyph ((,solanum-class (:inherit homoglyph))))
   `(nobreak-space ((,solanum-class (:box (:line-width (2 . 2)) :inherit homoglyph))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,solanum-class (:foreground ,solanum-light-blue))))
   `(elfeed-search-date-face ((,solanum-class (:foreground ,solanum-orange))))
   `(elfeed-search-feed-face ((,solanum-class (:foreground ,solanum-light-green))))
   `(elfeed-search-title-face ((,solanum-class (:foreground ,solanum-yellow-potato))))
   `(elfeed-search-filter-face ((,solanum-class (:foreground ,solanum-purple))))
   `(elfeed-search-last-update-face ((,solanum-class (:foreground ,solanum-light-purple))))
   `(elfeed-search-unread-title-face ((,solanum-class (:weight bold :foreground ,solanum-red-tomato))))
   `(elfeed-search-unread-count-face ((,solanum-class (:weight bold :foreground ,solanum-alt-light-green))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,solanum-class (:foreground ,solanum-red-tomato))))
   `(doom-modeline-project-parent-dir ((,solanum-class (:foreground ,solanum-red-tomato))))
   `(doom-modeline-buffer-minor-mode ((,solanum-class (:foreground ,solanum-yellow-potato))))

   ;; corfu
   `(corfu-default ((,solanum-class (:foreground ,solanum-white :background ,solanum-purple-black))))
   `(corfu-current ((,solanum-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,solanum-class (:background ,solanum-shadow))))
   `(corfu-border ((,solanum-class (:background ,solanum-shadow))))

   ;; envrc
   `(envrc-mode-line-error-face ((,solanum-class (:inherit error))))
   `(envrc-mode-line-none-face ((,solanum-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,solanum-class (:inherit success))))

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
