;;; guava-themes-prunus-theme.el --- A theme inspired by cherry colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 29, 2025
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
;; A theme inspired by cherry colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-prunus "A theme inspired by cherry colors.")

(let* (
      (guava-themes-prunus-class '((class color) (min-colors 257)))
      ;;(guava-themes-prunus-black             "#000000")
      (guava-themes-prunus-white             "#FFFFFF")

      (guava-themes-prunus-shadow            "#b3b3b3")

      (guava-themes-prunus-cream             "#DEA2BD");fffef5,EBDCF5
      (guava-themes-prunus-light-brown       "#735944")
      (guava-themes-prunus-brown             "#4A301B");583c25,
      (guava-themes-prunus-dark-brown        "#1A0E05");E9E4F9,3F271D,2E1E03,281A04,1A0E05,1C0E06,1C0F07

      (guava-themes-prunus-light-green       "#52BC63")
      (guava-themes-prunus-oceanic-green     "#3AC3A2")

      (guava-themes-prunus-deep-orange       "#C46935");a0522d
      (guava-themes-prunus-red               "#88190C");cb001e,d2191e
      (guava-themes-prunus-pink              "#CD2788");dc6199,cd4f88

      (guava-themes-prunus-blue              "#4534E3");4534e3,120cdc
      (guava-themes-prunus-deep-blue         "#655DB0")
      (guava-themes-prunus-antarctic-blue    "#8D76CA")
      (guava-themes-prunus-cyan              "#008B8B")

      (guava-themes-prunus-deep-purple       "#740CBE");800080
      (guava-themes-prunus-purple-red        "#8B2252")

      (guava-themes-prunus-error             "#FF0000");FF0000,bc0000,890014
      (guava-themes-prunus-warning           "#F68511");F68511,ffc333
      (guava-themes-prunus-success           "#23D734");228B22,007900

      (guava-themes-prunus-vc-change         guava-themes-prunus-blue)
      (guava-themes-prunus-vc-insert         guava-themes-prunus-success)
      (guava-themes-prunus-vc-delete         guava-themes-prunus-error))

  (custom-theme-set-faces
   'guava-themes-prunus

   ;; default for guava-themes-prunus
   `(default ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-cream :background ,guava-themes-prunus-dark-brown))))

   ;; error, warning, success
   `(error ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-error :weight bold))))
   `(warning ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-warning :weight bold))))
   `(success ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-themes-prunus-class (:background ,guava-themes-prunus-deep-blue :foreground ,guava-themes-prunus-white))))

   ;; fringe
   `(fringe ((,guava-themes-prunus-class (:background ,guava-themes-prunus-dark-brown :foreground ,guava-themes-prunus-cream))))
   `(diff-hl-change ((,guava-themes-prunus-class (:background ,guava-themes-prunus-vc-change :foreground ,guava-themes-prunus-vc-change))))
   `(diff-hl-insert ((,guava-themes-prunus-class (:background ,guava-themes-prunus-vc-insert :foreground ,guava-themes-prunus-vc-insert))))
   `(diff-hl-delete ((,guava-themes-prunus-class (:background ,guava-themes-prunus-vc-delete :foreground ,guava-themes-prunus-vc-delete))))

   ;; line-number
   `(line-number ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-purple-red :height 1.35))))
   `(line-number-current-line ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-cream :background ,guava-themes-prunus-brown :weight bold :height 1.35))))

   ;; highlight
   `(highlight ((,guava-themes-prunus-class (:background ,guava-themes-prunus-brown))))

   ;; shadow
   `(shadow ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-shadow))))

   ;; region
   `(region ((,guava-themes-prunus-class (:background ,guava-themes-prunus-light-brown))))

   ;; mode-line
   `(mode-line ((,guava-themes-prunus-class (:background ,guava-themes-prunus-red :foreground ,guava-themes-prunus-white))))
   `(mode-line-inactive ((,guava-themes-prunus-class (:background ,guava-themes-prunus-deep-orange :foreground ,guava-themes-prunus-white))))
   `(guava-themes-visible-bell ((,guava-themes-prunus-class (:background ,guava-themes-prunus-antarctic-blue :foreground ,guava-themes-prunus-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-white))))

   ;; borders
   `(vertical-border ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-pink))))

   ;; header-line
   `(header-line ((,guava-themes-prunus-class (:background ,guava-themes-prunus-red :foreground ,guava-themes-prunus-white))))
   `(which-func ((,guava-themes-prunus-class (:background ,guava-themes-prunus-red :foreground ,guava-themes-prunus-white))))

   ;; tab-bar
   `(tab-bar ((,guava-themes-prunus-class (:background ,guava-themes-prunus-pink :foreground ,guava-themes-prunus-white))))
   `(tab-bar-tab ((,guava-themes-prunus-class (:background ,guava-themes-prunus-red :foreground ,guava-themes-prunus-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-themes-prunus-class (:background ,guava-themes-prunus-pink :foreground ,guava-themes-prunus-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-themes-prunus-class (:background ,guava-themes-prunus-pink :foreground ,guava-themes-prunus-white))))
   `(tab-line-tab ((,guava-themes-prunus-class (:background ,guava-themes-prunus-pink :foreground ,guava-themes-prunus-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-themes-prunus-class (:background ,guava-themes-prunus-red :foreground ,guava-themes-prunus-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-themes-prunus-class (:background ,guava-themes-prunus-pink :foreground ,guava-themes-prunus-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-themes-prunus-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-light-green :weight medium))))
   `(font-lock-string-face ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-purple-red :weight bold))))
   `(font-lock-keyword-face ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-deep-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-deep-blue :weight medium))))
   `(font-lock-warning-face ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-error :weight medium))))
   `(font-lock-type-face ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-antarctic-blue :weight medium))))
   `(font-lock-constant-face ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-cyan :weight medium))))
   `(font-lock-function-name-face ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-blue :weight medium))))
   `(font-lock-bracket-face ((,guava-themes-prunus-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-deep-orange :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-themes-prunus-class (:background ,guava-themes-prunus-deep-orange))))

   ;; buttons
   `(link ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-blue :underline t :weight bold))))
   `(link-visited ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-oceanic-green :underline t :weight bold))))
   `(button ((,guava-themes-prunus-class (:foreground ,guava-themes-prunus-blue :underline t :weight bold))))))

(provide-theme 'guava-themes-prunus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes-prunus-theme.el ends here
