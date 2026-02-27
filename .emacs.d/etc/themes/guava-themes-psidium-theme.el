;;; guava-themes-psidium-theme.el --- A theme inspired by guava colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 07, 2025
;; Version: 0.11.1
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
;; A theme inspired by guava colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-psidium "A theme inspired by guava colors.")

(let* (
      (guava-themes-psidium-class '((class color) (min-colors 257)))
      (guava-themes-psidium-black             "#000000")
      (guava-themes-psidium-white             "#FFFFFF")

      (guava-themes-psidium-shadow            "#7f7f7f")

      (guava-themes-psidium-cream             "#F1EECE");F7DEB6

      (guava-themes-psidium-green             "#599F48")
      (guava-themes-psidium-oceanic-green     "#3ab992");13765e,13845e,139570,3ab488
      (guava-themes-psidium-guava-green       "#AED734");AECA41,AED234
      (guava-themes-psidium-deep-green        "#097d2c");09662c

      (guava-themes-psidium-orange            "#FF7D5F")
      (guava-themes-psidium-deep-orange       "#D43D1A")
      (guava-themes-psidium-red               "#c1153b");f02d1b
      (guava-themes-psidium-light-pink        "#FCD0C9");F8917C
      (guava-themes-psidium-pink              "#F8767C");F84865,F8767C,F88686,F85A65

      (guava-themes-psidium-light-brown       "#735944")
      (guava-themes-psidium-brown             "#7D5E45")

      (guava-themes-psidium-light-blue        "#41C3CA")
      (guava-themes-psidium-blue              "#008B8B");245feb
      (guava-themes-psidium-oceanic-blue      "#2a4ad9")
      (guava-themes-psidium-deep-blue         "#483d8b");004F5D
      (guava-themes-psidium-antarctic-blue    "#8d76ca");6a5997

      (guava-themes-psidium-light-purple      "#a62d90");D7137C,C0137C,B00CE0
      (guava-themes-psidium-purple            "#a62db2")

      (guava-themes-psidium-error             "#FF0000")
      (guava-themes-psidium-warning           "#F68511");FF8C00,f08020,f68511
      (guava-themes-psidium-success           "#228B22")

      (guava-themes-psidium-vc-change         guava-themes-psidium-light-blue)
      (guava-themes-psidium-vc-insert         guava-themes-psidium-green)
      (guava-themes-psidium-vc-delete         guava-themes-psidium-orange))

  (custom-theme-set-faces
   'guava-themes-psidium

   ;; default for guava-themes-psidium
   `(default ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-black :background ,guava-themes-psidium-cream))))

   ;; error, warning, success
   `(error ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-error :weight bold))))
   `(warning ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-warning :weight bold))))
   `(success ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-themes-psidium-class (:background ,guava-themes-psidium-green :foreground ,guava-themes-psidium-white))))

   ;; fringe
   `(fringe ((,guava-themes-psidium-class (:background ,guava-themes-psidium-cream :foreground ,guava-themes-psidium-cream))))
   `(diff-hl-change ((,guava-themes-psidium-class (:background ,guava-themes-psidium-vc-change :foreground ,guava-themes-psidium-vc-change))))
   `(diff-hl-insert ((,guava-themes-psidium-class (:background ,guava-themes-psidium-vc-insert :foreground ,guava-themes-psidium-vc-insert))))
   `(diff-hl-delete ((,guava-themes-psidium-class (:background ,guava-themes-psidium-vc-delete :foreground ,guava-themes-psidium-vc-delete))))

   ;; line-number
   `(line-number ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-brown :height 1.35))))
   `(line-number-current-line ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-black :background ,guava-themes-psidium-light-pink :weight bold :height 1.35))))

   ;; highlight
   `(highlight ((,guava-themes-psidium-class (:background ,guava-themes-psidium-light-pink))))

   ;; shadow
   `(shadow ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-shadow))))

   ;; region
   `(region ((,guava-themes-psidium-class (:background ,guava-themes-psidium-oceanic-green))))

   ;; mode-line
   `(mode-line ((,guava-themes-psidium-class (:background ,guava-themes-psidium-guava-green :foreground ,guava-themes-psidium-black))))
   `(mode-line-inactive ((,guava-themes-psidium-class (:background ,guava-themes-psidium-green :foreground ,guava-themes-psidium-white))))
   `(guava-themes-visible-bell ((,guava-themes-psidium-class (:background ,guava-themes-psidium-deep-green :foreground ,guava-themes-psidium-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-black))))

   ;;borders
   `(vertical-border ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-light-brown))))

   ;; header-line
   `(header-line ((,guava-themes-psidium-class (:background ,guava-themes-psidium-pink :foreground ,guava-themes-psidium-white))))
   `(which-func ((,guava-themes-psidium-class (:background ,guava-themes-psidium-pink :foreground ,guava-themes-psidium-white))))

   ;; tab-bar
   `(tab-bar ((,guava-themes-psidium-class (:background ,guava-themes-psidium-guava-green :foreground ,guava-themes-psidium-white))))
   `(tab-bar-tab ((,guava-themes-psidium-class (:background ,guava-themes-psidium-pink :foreground ,guava-themes-psidium-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-themes-psidium-class (:background ,guava-themes-psidium-guava-green :foreground ,guava-themes-psidium-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-themes-psidium-class (:background ,guava-themes-psidium-guava-green :foreground ,guava-themes-psidium-white))))
   `(tab-line-tab ((,guava-themes-psidium-class (:background ,guava-themes-psidium-guava-green :foreground ,guava-themes-psidium-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-themes-psidium-class (:background ,guava-themes-psidium-pink :foreground ,guava-themes-psidium-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-themes-psidium-class (:background ,guava-themes-psidium-guava-green :foreground ,guava-themes-psidium-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-deep-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-themes-psidium-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-green :weight medium))))
   `(font-lock-string-face ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-brown :weight bold))))
   `(font-lock-keyword-face ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-red :weight medium))))
   `(font-lock-builtin-face ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-deep-blue :weight medium))))
   `(font-lock-warning-face ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-error :weight medium))))
   `(font-lock-type-face ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-deep-green :weight medium))))
   `(font-lock-constant-face ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-oceanic-blue :weight medium))))
   `(font-lock-function-name-face ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-blue :weight medium))))
   `(font-lock-bracket-face ((,guava-themes-psidium-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-purple :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-themes-psidium-class (:background ,guava-themes-psidium-orange))))

   ;; buttons
   `(link ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-light-blue :underline t :weight bold))))
   `(link-visited ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-light-purple :underline t :weight bold))))
   `(button ((,guava-themes-psidium-class (:foreground ,guava-themes-psidium-blue :underline t :weight bold))))))

(provide-theme 'guava-themes-psidium)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes-psidium-theme.el ends here
