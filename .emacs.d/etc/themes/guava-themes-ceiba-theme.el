;;; guava-themes-ceiba-theme.el --- A theme inspired by ceiba colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 21, 2026
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
;; A theme inspired by ceiba colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-ceiba "A theme inspired by ceiba tree colors.")

(let* (
      (guava-themes-ceiba-class '((class color) (min-colors 257)))
      (guava-themes-ceiba-black             "#000000")
      (guava-themes-ceiba-white             "#FFFFFF")

      (guava-themes-ceiba-shadow            "#7f7f7f")

      ;; (guava-themes-ceiba-light-gray        "#bab49e");dcdcdc,d4d4d4
      (guava-themes-ceiba-gray-green        "#bab49e");8c857b,8d8d8b,8c7f76,817a68,817a6a
      (guava-themes-ceiba-gray              "#9d9d9d");dcdcdc,656865,7f7f89,787882
      (guava-themes-ceiba-gray-blue         "#798585");8c857b,8d8d8b,6d726a,656865

      (guava-themes-ceiba-light-green       "#5b6452");5a6352
      (guava-themes-ceiba-deep-green        "#2b5535");375033,395235,3a5435,3a5835,385635,375535
      (guava-themes-ceiba-green-blue        "#116452");5a6352

      (guava-themes-ceiba-orange            "#a85639")

      (guava-themes-ceiba-steel-blue        "#aabed8");b0c4de
      (guava-themes-ceiba-blue              "#2327dc")
      (guava-themes-ceiba-alt-blue          "#2268a7");3a5ba7

      (guava-themes-ceiba-purple            "#4e466d");49206d
      (guava-themes-ceiba-purple-red        "#762362");862060,892362

      (guava-themes-ceiba-brown-sand        "#826e51");796041
      (guava-themes-ceiba-light-brown       "#6d4b30");6b492e
      (guava-themes-ceiba-brown-wood        "#53453d");9c6d85,bf8987,514141,53423e

      (guava-themes-ceiba-error             "#d70000");ff0000
      (guava-themes-ceiba-warning           "#f6c911");F68511
      (guava-themes-ceiba-success           "#29c825")

      (guava-themes-ceiba-vc-change         guava-themes-ceiba-blue)
      (guava-themes-ceiba-vc-insert         guava-themes-ceiba-success)
      (guava-themes-ceiba-vc-delete         guava-themes-ceiba-error))

  (custom-theme-set-faces
   'guava-themes-ceiba

   ;; default for guava-themes-ceiba
   `(default ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-black :background ,guava-themes-ceiba-gray-green))))

   ;; error, warning, success
   `(error ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-error :weight bold))))
   `(warning ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-warning :weight bold))))
   `(success ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-brown-wood :foreground ,guava-themes-ceiba-black))))

   ;; fringe
   `(fringe ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-gray-green :foreground ,guava-themes-ceiba-black))))
   `(diff-hl-change ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-vc-change :foreground ,guava-themes-ceiba-vc-change))))
   `(diff-hl-insert ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-vc-insert :foreground ,guava-themes-ceiba-vc-insert))))
   `(diff-hl-delete ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-vc-delete :foreground ,guava-themes-ceiba-vc-delete))))

   ;; line-number
   `(line-number ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-black :height 1.35))))
   `(line-number-current-line ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-deep-green :background ,guava-themes-ceiba-gray :weight bold :height 1.35))))

   ;; highlight
   `(highlight ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-gray))))

   ;; shadow
   `(shadow ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-shadow))))

   ;; region
   `(region ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-gray-blue))))

   ;; mode-line
   `(mode-line ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-light-green :foreground ,guava-themes-ceiba-white))))
   `(mode-line-inactive ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-gray-blue :foreground ,guava-themes-ceiba-white))))
   `(guava-themes-visible-bell ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-steel-blue :foreground ,guava-themes-ceiba-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-black))))

   ;; borders
   `(vertical-border ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-gray-green))))

   ;; header-line
   `(header-line ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-light-green :foreground ,guava-themes-ceiba-white))))
   `(which-func ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-light-green :foreground ,guava-themes-ceiba-white))))

   ;; tab-bar
   `(tab-bar ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-brown-sand :foreground ,guava-themes-ceiba-white))))
   `(tab-bar-tab ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-light-green :foreground ,guava-themes-ceiba-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-brown-sand :foreground ,guava-themes-ceiba-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-brown-sand :foreground ,guava-themes-ceiba-white))))
   `(tab-line-tab ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-brown-sand :foreground ,guava-themes-ceiba-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-light-green :foreground ,guava-themes-ceiba-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-brown-sand :foreground ,guava-themes-ceiba-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-purple-red :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-themes-ceiba-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-deep-green :weight medium))))
   `(font-lock-string-face ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-light-brown :weight bold))))
   `(font-lock-keyword-face ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-alt-blue :weight medium))))
   `(font-lock-warning-face ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-error :weight medium))))
   `(font-lock-type-face ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-green-blue :weight medium))))
   `(font-lock-constant-face ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-orange :weight medium))))
   `(font-lock-function-name-face ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-brown-wood :weight medium))))
   `(font-lock-bracket-face ((,guava-themes-ceiba-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-light-green :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-themes-ceiba-class (:background ,guava-themes-ceiba-deep-green))))

   ;; buttons
   `(link ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-purple :underline t :weight bold))))
   `(link-visited ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-purple-red :underline t :weight bold))))
   `(button ((,guava-themes-ceiba-class (:foreground ,guava-themes-ceiba-purple :underline t :weight bold))))))

(provide-theme 'guava-themes-ceiba)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes-ceiba-theme.el ends here
