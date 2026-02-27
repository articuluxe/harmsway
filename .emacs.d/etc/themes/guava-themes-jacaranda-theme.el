;;; guava-themes-jacaranda-theme.el --- A theme inspired by jacaranda colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 27, 2025
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
;; A theme inspired by jacaranda colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-jacaranda "A theme inspired by jacaranda colors.")

(let* (
      (guava-themes-jacaranda-class '((class color) (min-colors 257)))
      (guava-themes-jacaranda-black             "#000000")
      (guava-themes-jacaranda-white             "#FFFFFF")

      (guava-themes-jacaranda-shadow            "#7f7f7f")

      (guava-themes-jacaranda-cream             "#e9d9f9");e9e4f9

      (guava-themes-jacaranda-light-green       "#52bc63")
      (guava-themes-jacaranda-green             "#8ec654")
      (guava-themes-jacaranda-deep-green        "#267a63")
      (guava-themes-jacaranda-oceanic-green     "#3ab992")

      (guava-themes-jacaranda-orange            "#ff9f79");ff9535
      (guava-themes-jacaranda-deep-orange       "#c46935");a0522d
      (guava-themes-jacaranda-red               "#ca0036")

      (guava-themes-jacaranda-light-blue        "#C0B4E4")
      (guava-themes-jacaranda-blue              "#4534e3")
      (guava-themes-jacaranda-deep-blue         "#655db0")
      (guava-themes-jacaranda-antarctic-blue    "#8d76ca")
      (guava-themes-jacaranda-cyan              "#008b8b")

      (guava-themes-jacaranda-light-purple      "#dbd0fd")
      (guava-themes-jacaranda-purple            "#aa69e6");984ee6
      (guava-themes-jacaranda-deep-purple       "#740cbe");800080
      (guava-themes-jacaranda-purple-red        "#8b2252")

      (guava-themes-jacaranda-error             "#bc0000");FF0000
      (guava-themes-jacaranda-warning           "#ffc333");F68511
      (guava-themes-jacaranda-success           "#007900");228B22

      (guava-themes-jacaranda-vc-change         guava-themes-jacaranda-blue)
      (guava-themes-jacaranda-vc-insert         guava-themes-jacaranda-green)
      (guava-themes-jacaranda-vc-delete         guava-themes-jacaranda-red))

  (custom-theme-set-faces
   'guava-themes-jacaranda

   ;; default for guava-themes-jacaranda
   `(default ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-black :background ,guava-themes-jacaranda-cream))))

   ;; error, warning, success
   `(error ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-error :weight bold))))
   `(warning ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-warning :weight bold))))
   `(success ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-purple :foreground ,guava-themes-jacaranda-white))))

   ;; fringe
   `(fringe ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-cream :foreground ,guava-themes-jacaranda-cream))))
   `(diff-hl-change ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-vc-change :foreground ,guava-themes-jacaranda-vc-change))))
   `(diff-hl-insert ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-vc-insert :foreground ,guava-themes-jacaranda-vc-insert))))
   `(diff-hl-delete ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-vc-delete :foreground ,guava-themes-jacaranda-vc-delete))))

   ;; line-number
   `(line-number ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-antarctic-blue :height 1.35))))
   `(line-number-current-line ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-black :background ,guava-themes-jacaranda-light-purple :weight bold :height 1.35))))

   ;; highlight
   `(highlight ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-light-purple))))

   ;; shadow
   `(shadow ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-shadow))))

   ;; region
   `(region ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-light-blue))))

   ;; mode-line
   `(mode-line ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-purple :foreground ,guava-themes-jacaranda-white))))
   `(mode-line-inactive ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-light-blue :foreground ,guava-themes-jacaranda-white))))
   `(guava-themes-visible-bell ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-orange :foreground ,guava-themes-jacaranda-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-black))))

   ;; borders
   `(vertical-border ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-light-purple))))

   ;; header-line
   `(header-line ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-deep-blue :foreground ,guava-themes-jacaranda-white))))
   `(which-func ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-deep-blue :foreground ,guava-themes-jacaranda-white))))

   ;; tab-bar
   `(tab-bar ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-purple :foreground ,guava-themes-jacaranda-white))))
   `(tab-bar-tab ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-deep-blue :foreground ,guava-themes-jacaranda-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-purple :foreground ,guava-themes-jacaranda-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-purple :foreground ,guava-themes-jacaranda-white))))
   `(tab-line-tab ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-purple :foreground ,guava-themes-jacaranda-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-deep-blue :foreground ,guava-themes-jacaranda-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-purple :foreground ,guava-themes-jacaranda-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-orange :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-themes-jacaranda-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-light-green :weight medium))))
   `(font-lock-string-face ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-purple-red :weight bold))))
   `(font-lock-keyword-face ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-deep-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-deep-blue :weight medium))))
   `(font-lock-warning-face ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-red :weight medium))))
   `(font-lock-type-face ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-oceanic-green :weight medium))))
   `(font-lock-constant-face ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-cyan :weight medium))))
   `(font-lock-function-name-face ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-blue :weight medium))))
   `(font-lock-bracket-face ((,guava-themes-jacaranda-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-deep-orange :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-themes-jacaranda-class (:background ,guava-themes-jacaranda-orange))))

   ;; buttons
   `(link ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-oceanic-green :underline t :weight bold))))
   `(link-visited ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-deep-green :underline t :weight bold))))
   `(button ((,guava-themes-jacaranda-class (:foreground ,guava-themes-jacaranda-blue :underline t :weight bold))))))

(provide-theme 'guava-themes-jacaranda)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes-jacaranda-theme.el ends here
