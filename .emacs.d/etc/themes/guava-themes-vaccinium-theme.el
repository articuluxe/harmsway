;;; guava-themes-vaccinium-theme.el --- A theme inspired by blueberry colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Mar 22, 2026
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
;; A theme inspired by blueberry, huckleberry, cranberry, and bilberry colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-vaccinium "A theme inspired by blueberry, huckleberry, cranberry, and bilberry colors.")

(let* (
      (vaccinium-class '((class color) (min-colors 257)))
      (vaccinium-black             "#000000")
      (vaccinium-white             "#FFFFFF")

      (vaccinium-black-gray        "#1c1c26");202028
      (vaccinium-alt-black-gray    "#2b2b35")

      (vaccinium-shadow            "#b3b3b3")

      (vaccinium-red               "#b92e33")
      (vaccinium-pink              "#e986f1")
      (vaccinium-orange            "#ffa07a")

      (vaccinium-yellow            "#fde8b9")

      (vaccinium-green-forest      "#007841")
      (vaccinium-green-subdued     "#378962")

      (vaccinium-blueberry         "#5582d7");77aff9,5587d2
      (vaccinium-alt-blueberry     "#3755a0");77aff9,5587d2,3764b9
      (vaccinium-blue              "#0f69b4");0d62b2
      (vaccinium-alt-blue          "#1455f1");1559f1
      (vaccinium-deep-blue         "#1c5367");303053,1c3053,1c3f53
      (vaccinium-steel-blue        "#6a7e98")
      (vaccinium-alt-steel-blue    "#566a84")
      (vaccinium-cyan              "#00ffff")

      (vaccinium-light-purple      "#5978b9")
      (vaccinium-purple            "#625fac")
      (vaccinium-purple-pink       "#aa78cf")
      (vaccinium-purple-red        "#632a63");6816f3,5d2a74,632a63

      (vaccinium-error             "#FF0000")
      (vaccinium-warning           "#f6d909");F68511
      (vaccinium-success           "#1ebe1e");23a334

      (vaccinium-vc-change         vaccinium-alt-blue)
      (vaccinium-vc-insert         vaccinium-success)
      (vaccinium-vc-delete         vaccinium-error))

  (custom-theme-set-faces
   'guava-themes-vaccinium

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-black-gray))))

   ;; error, warning, success
   `(error ((,vaccinium-class (:foreground ,vaccinium-error :weight bold))))
   `(warning ((,vaccinium-class (:foreground ,vaccinium-warning :weight bold))))
   `(success ((,vaccinium-class (:foreground ,vaccinium-success :weight bold))))

   ;; highlight
   `(highlight ((,vaccinium-class (:background ,vaccinium-alt-black-gray))))

   ;; shadow
   `(shadow ((,vaccinium-class (:foreground ,vaccinium-shadow))))

   ;; region
   `(region ((,vaccinium-class (:background ,vaccinium-deep-blue))))
   `(secondary-selection ((,vaccinium-class (:background ,vaccinium-alt-blueberry :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,vaccinium-class (:foreground ,vaccinium-green-forest :weight medium))))
   `(font-lock-string-face ((,vaccinium-class (:foreground ,vaccinium-orange :weight medium))))
   `(font-lock-keyword-face ((,vaccinium-class (:foreground ,vaccinium-green-subdued :weight medium))))
   `(font-lock-builtin-face ((,vaccinium-class (:foreground ,vaccinium-steel-blue :weight medium))))
   `(font-lock-warning-face ((,vaccinium-class (:foreground ,vaccinium-warning :weight bold))))
   `(font-lock-type-face ((,vaccinium-class (:foreground ,vaccinium-purple :weight medium))))
   `(font-lock-constant-face ((,vaccinium-class (:foreground ,vaccinium-pink :weight medium))))
   `(font-lock-function-name-face ((,vaccinium-class (:foreground ,vaccinium-blue :weight medium))))
   `(font-lock-punctuation-face ((,vaccinium-class (:foreground ,vaccinium-light-purple :weight medium))))
   `(font-lock-variable-name-face ((,vaccinium-class (:foreground ,vaccinium-red :weight medium))))
   `(font-lock-negation-char-face ((,vaccinium-class (:foreground ,vaccinium-purple-pink :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,vaccinium-class (:foreground ,vaccinium-black :background ,vaccinium-blueberry))))

   ;; fringe
   `(fringe ((,vaccinium-class (:foreground ,vaccinium-red :background ,vaccinium-black-gray))))
   `(diff-hl-change ((,vaccinium-class (:foreground ,vaccinium-vc-change :background ,vaccinium-vc-change))))
   `(diff-hl-insert ((,vaccinium-class (:foreground ,vaccinium-vc-insert :background ,vaccinium-vc-insert))))
   `(diff-hl-delete ((,vaccinium-class (:foreground ,vaccinium-vc-delete :background ,vaccinium-vc-delete))))

   ;; line-number
   `(line-number ((,vaccinium-class (:foreground ,vaccinium-white))))
   `(line-number-current-line ((,vaccinium-class (:foreground ,vaccinium-steel-blue :weight bold :inherit highlight))))
   `(line-number-minor-tick ((,vaccinium-class (:background ,vaccinium-orange))))
   `(line-number-major-tick ((,vaccinium-class (:background ,vaccinium-purple))))

   ;; mode-line
   `(mode-line ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-blueberry))))
   `(mode-line-inactive ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-alt-blueberry))))
   `(guava-themes-visible-bell ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-orange))))

   ;; minibuffer
   `(minibuffer-prompt ((,vaccinium-class (:foreground ,vaccinium-blueberry))))

   ;; borders
   `(vertical-border ((,vaccinium-class (:foreground ,vaccinium-blueberry))))

   ;; header-line
   `(header-line ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-blueberry))))
   `(which-func ((,vaccinium-class (:foreground ,vaccinium-white))))

   ;; tab-bar
   `(tab-bar ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-steel-blue))))
   `(tab-bar-tab ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-blueberry :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-steel-blue :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-steel-blue))))
   `(tab-line-tab ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-alt-blueberry :weight bold :height 0.9))))
   `(tab-line-tab-current ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-blueberry :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-steel-blue :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-alt-steel-blue :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,vaccinium-class (:foreground ,vaccinium-orange :weight bold :height 0.9))))
   `(tab-line-tab-special ((,vaccinium-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,vaccinium-class (:foreground ,vaccinium-black :background ,vaccinium-orange))))
   `(show-paren-mismatch ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,vaccinium-class (:background ,vaccinium-error))))

   ;; links
   `(link ((,vaccinium-class (:foreground ,vaccinium-yellow :underline t :weight bold))))
   `(link-visited ((,vaccinium-class (:foreground ,vaccinium-orange :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,vaccinium-class (:foreground ,vaccinium-blue :weight medium))))
   `(outline-2 ((,vaccinium-class (:foreground ,vaccinium-red :weight medium))))
   `(outline-3 ((,vaccinium-class (:foreground ,vaccinium-green-subdued :weight medium))))
   `(outline-4 ((,vaccinium-class (:foreground ,vaccinium-purple :weight medium))))
   `(outline-5 ((,vaccinium-class (:foreground ,vaccinium-pink :weight medium))))
   `(outline-6 ((,vaccinium-class (:foreground ,vaccinium-green-forest :weight medium))))
   `(outline-7 ((,vaccinium-class (:foreground ,vaccinium-steel-blue :weight medium))))
   `(outline-8 ((,vaccinium-class (:foreground ,vaccinium-orange :weight medium))))

   ;; homoglyph, escape-glyph, nobreak-space
   `(homoglyph ((,vaccinium-class (:foreground ,vaccinium-cyan))))
   `(escape-glyph ((,vaccinium-class (:inherit homoglyph))))
   `(nobreak-space ((,vaccinium-class (:box (:line-width (2 . 2)) :inherit homoglyph))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,vaccinium-class (:foreground ,vaccinium-purple))))
   `(elfeed-search-date-face ((,vaccinium-class (:foreground ,vaccinium-steel-blue))))
   `(elfeed-search-feed-face ((,vaccinium-class (:foreground ,vaccinium-blueberry))))
   `(elfeed-search-title-face ((,vaccinium-class (:foreground ,vaccinium-orange))))
   `(elfeed-search-filter-face ((,vaccinium-class (:foreground ,vaccinium-deep-blue))))
   `(elfeed-search-last-update-face ((,vaccinium-class (:foreground ,vaccinium-purple-red))))
   `(elfeed-search-unread-title-face ((,vaccinium-class (:weight bold :foreground ,vaccinium-green-subdued))))
   `(elfeed-search-unread-count-face ((,vaccinium-class (:weight bold :foreground ,vaccinium-alt-blueberry))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,vaccinium-class (:foreground ,vaccinium-purple-red))))
   `(doom-modeline-project-parent-dir ((,vaccinium-class (:foreground ,vaccinium-purple-red))))
   `(doom-modeline-buffer-minor-mode ((,vaccinium-class (:foreground ,vaccinium-yellow))))

   ;; corfu
   `(corfu-default ((,vaccinium-class (:foreground ,vaccinium-white :background ,vaccinium-black-gray))))
   `(corfu-current ((,vaccinium-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,vaccinium-class (:background ,vaccinium-shadow))))
   `(corfu-border ((,vaccinium-class (:background ,vaccinium-shadow))))

   ;; envrc
   `(envrc-mode-line-error-face ((,vaccinium-class (:inherit error))))
   `(envrc-mode-line-none-face ((,vaccinium-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,vaccinium-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,vaccinium-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-vaccinium)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-vaccinium-theme.el ends here
