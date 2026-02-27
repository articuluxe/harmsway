;;; guava-themes-solanum-theme.el --- A theme inspired by eggplant colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Feb 22, 2026
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
;; A theme inspired by potato, tomato, and eggplant colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-solanum "A theme inspired by potato, tomato, and eggplant colors.")

(let* (
      (guava-themes-solanum-class '((class color) (min-colors 257)))
      (guava-themes-solanum-black             "#000000")
      (guava-themes-solanum-white             "#FFFFFF")

      (guava-themes-solanum-shadow            "#b3b3b3")

      (guava-themes-solanum-red-tomato        "#cd151f")
      (guava-themes-solanum-orange            "#e98c85")

      (guava-themes-solanum-yellow-potato     "#fde8b9")

      (guava-themes-solanum-light-green       "#61ff96")
      (guava-themes-solanum-green             "#0d6d4b")

      (guava-themes-solanum-light-blue        "#8ec4de")
      (guava-themes-solanum-blue              "#0d62b2")

      (guava-themes-solanum-light-purple      "#d4cbff");c4aeff,ccbcff
      (guava-themes-solanum-purple            "#9e7ae8")
      (guava-themes-solanum-purple-red        "#64143d");6a143d
      (guava-themes-solanum-dark-purple       "#672b5f");41143d
      (guava-themes-solanum-purple-black      "#130d1a")

      (guava-themes-solanum-error             "#FF0000")
      (guava-themes-solanum-warning           "#f6d909");F68511
      (guava-themes-solanum-success           "#23a334");239834

      (guava-themes-solanum-vc-change         guava-themes-solanum-blue)
      (guava-themes-solanum-vc-insert         guava-themes-solanum-success)
      (guava-themes-solanum-vc-delete         guava-themes-solanum-error))

  (custom-theme-set-faces
   'guava-themes-solanum

   ;; default for guava-themes-solanum
   `(default ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-white :background ,guava-themes-solanum-purple-black))))

   ;; error, warning, success
   `(error ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-error :weight bold))))
   `(warning ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-warning :weight bold))))
   `(success ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-themes-solanum-class (:background ,guava-themes-solanum-yellow-potato :foreground ,guava-themes-solanum-black))))

   ;; fringe
   `(fringe ((,guava-themes-solanum-class (:background ,guava-themes-solanum-purple-black :foreground ,guava-themes-solanum-purple-black))))
   `(diff-hl-change ((,guava-themes-solanum-class (:background ,guava-themes-solanum-vc-change :foreground ,guava-themes-solanum-vc-change))))
   `(diff-hl-insert ((,guava-themes-solanum-class (:background ,guava-themes-solanum-vc-insert :foreground ,guava-themes-solanum-vc-insert))))
   `(diff-hl-delete ((,guava-themes-solanum-class (:background ,guava-themes-solanum-vc-delete :foreground ,guava-themes-solanum-vc-delete))))

   ;; line-number
   `(line-number ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-white :height 1.35))))
   `(line-number-current-line ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-light-green :background ,guava-themes-solanum-purple-red :weight bold :height 1.35))))

   ;; highlight
   `(highlight ((,guava-themes-solanum-class (:background ,guava-themes-solanum-purple-red))))

   ;; shadow
   `(shadow ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-shadow))))

   ;; region
   `(region ((,guava-themes-solanum-class (:background ,guava-themes-solanum-dark-purple))))

   ;; mode-line
   `(mode-line ((,guava-themes-solanum-class (:background ,guava-themes-solanum-dark-purple :foreground ,guava-themes-solanum-white))))
   `(mode-line-inactive ((,guava-themes-solanum-class (:background ,guava-themes-solanum-purple-red :foreground ,guava-themes-solanum-white))))
   `(guava-themes-visible-bell ((,guava-themes-solanum-class (:background ,guava-themes-solanum-orange :foreground ,guava-themes-solanum-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-yellow-potato))))

   ;; borders
   `(vertical-border ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-purple-black))))

   ;; header-line
   `(header-line ((,guava-themes-solanum-class (:background ,guava-themes-solanum-dark-purple :foreground ,guava-themes-solanum-white))))
   `(which-func ((,guava-themes-solanum-class (:background ,guava-themes-solanum-dark-purple :foreground ,guava-themes-solanum-white))))

   ;; tab-bar
   `(tab-bar ((,guava-themes-solanum-class (:background ,guava-themes-solanum-purple-black :foreground ,guava-themes-solanum-white))))
   `(tab-bar-tab ((,guava-themes-solanum-class (:background ,guava-themes-solanum-dark-purple :foreground ,guava-themes-solanum-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-themes-solanum-class (:background ,guava-themes-solanum-purple-black :foreground ,guava-themes-solanum-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-themes-solanum-class (:background ,guava-themes-solanum-purple-black :foreground ,guava-themes-solanum-white))))
   `(tab-line-tab ((,guava-themes-solanum-class (:background ,guava-themes-solanum-purple-black :foreground ,guava-themes-solanum-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-themes-solanum-class (:background ,guava-themes-solanum-dark-purple :foreground ,guava-themes-solanum-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-themes-solanum-class (:background ,guava-themes-solanum-purple-black :foreground ,guava-themes-solanum-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-red-tomato :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-themes-solanum-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-red-tomato :weight medium))))
   `(font-lock-string-face ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-yellow-potato :weight bold))))
   `(font-lock-keyword-face ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-green :weight medium))))
   `(font-lock-builtin-face ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-light-blue :weight medium))))
   `(font-lock-warning-face ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-error :weight medium))))
   `(font-lock-type-face ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-orange :weight medium))))
   `(font-lock-constant-face ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-light-purple :weight medium))))
   `(font-lock-function-name-face ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-purple :weight medium))))
   `(font-lock-bracket-face ((,guava-themes-solanum-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-light-green :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-themes-solanum-class (:background ,guava-themes-solanum-light-blue))))

   ;; buttons
   `(link ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-light-purple :underline t :weight bold))))
   `(link-visited ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-dark-purple :underline t :weight bold))))
   `(button ((,guava-themes-solanum-class (:foreground ,guava-themes-solanum-light-purple :underline t :weight bold))))))

(provide-theme 'guava-themes-solanum)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes-solanum-theme.el ends here
