;;; guava-themes-dracaena-theme.el --- A theme inspired by dragon tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 06, 2026
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
;; A theme inspired by dragon tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-dracaena "A theme inspired by dragon tree colors.")

(let* (
      (guava-themes-dracaena-class '((class color) (min-colors 257)))
      (guava-themes-dracaena-black             "#000000")
      (guava-themes-dracaena-white             "#FFFFFF")

      (guava-themes-dracaena-shadow            "#b3b3b3")

      (guava-themes-dracaena-light-gray        "#4f4f4f")
      (guava-themes-dracaena-gray              "#424845");404242,3e4040,3d3f3f,424545
      (guava-themes-dracaena-dark-gray         "#353838")

      (guava-themes-dracaena-guava-green       "#AED734")
      (guava-themes-dracaena-deep-green        "#298e25")

      (guava-themes-dracaena-snakeplant-yellow "#d9d389")

      (guava-themes-dracaena-light-orange      "#ffa07a")
      (guava-themes-dracaena-red               "#d32333");c91628,cc192a,d7192a,ed2725,e92333,db2333
      (guava-themes-dracaena-deep-red          "#792725")

      (guava-themes-dracaena-light-brown       "#8b6c4d")

      (guava-themes-dracaena-light-blue        "#425fd5")
      (guava-themes-dracaena-blue              "#2134d5")
      (guava-themes-dracaena-antarctic-blue    "#bacce4");4f62be
      (guava-themes-dracaena-steel-blue        "#4f94cd");b0c4de

      (guava-themes-dracaena-pink-purple       "#a5225c");961250,b7125c
      (guava-themes-dracaena-deep-purple       "#8f1ac8")

      (guava-themes-dracaena-error             "#FF0000")
      (guava-themes-dracaena-warning           "#F68511")
      (guava-themes-dracaena-success           "#29d925")

      (guava-themes-dracaena-vc-change         guava-themes-dracaena-blue)
      (guava-themes-dracaena-vc-insert         guava-themes-dracaena-deep-green)
      (guava-themes-dracaena-vc-delete         guava-themes-dracaena-error))

  (custom-theme-set-faces
   'guava-themes-dracaena

   ;; default for guava-themes-dracaena
   `(default ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-antarctic-blue :background ,guava-themes-dracaena-gray))))

   ;; error, warning, success
   `(error ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-error :weight bold))))
   `(warning ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-warning :weight bold))))
   `(success ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-light-orange :foreground ,guava-themes-dracaena-white))))

   ;; fringe
   `(fringe ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-gray :foreground ,guava-themes-dracaena-gray))))
   `(diff-hl-change ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-vc-change :foreground ,guava-themes-dracaena-vc-change))))
   `(diff-hl-insert ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-vc-insert :foreground ,guava-themes-dracaena-vc-insert))))
   `(diff-hl-delete ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-vc-delete :foreground ,guava-themes-dracaena-vc-delete))))

   ;; line-number
   `(line-number ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-antarctic-blue :height 1.35))))
   `(line-number-current-line ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-light-orange :background ,guava-themes-dracaena-light-gray :weight bold :height 1.35))))

   ;; highlight
   `(highlight ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-light-gray))))

   ;; shadow
   `(shadow ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-shadow))))

   ;; region
   `(region ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-deep-red))))

   ;; mode-line
   `(mode-line ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-deep-red :foreground ,guava-themes-dracaena-white))))
   `(mode-line-inactive ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-dark-gray :foreground ,guava-themes-dracaena-white))))
   `(guava-themes-visible-bell ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-light-orange :foreground ,guava-themes-dracaena-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-antarctic-blue))))

   ;; borders
   `(vertical-border ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-light-orange))))

   ;; header-line
   `(header-line ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-deep-red :foreground ,guava-themes-dracaena-white))))
   `(which-func ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-deep-red :foreground ,guava-themes-dracaena-white))))

   ;; tab-bar
   `(tab-bar ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-dark-gray :foreground ,guava-themes-dracaena-white))))
   `(tab-bar-tab ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-deep-red :foreground ,guava-themes-dracaena-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-dark-gray :foreground ,guava-themes-dracaena-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-dark-gray :foreground ,guava-themes-dracaena-white))))
   `(tab-line-tab ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-dark-gray :foreground ,guava-themes-dracaena-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-deep-red :foreground ,guava-themes-dracaena-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-dark-gray :foreground ,guava-themes-dracaena-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-themes-dracaena-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-deep-green :weight medium))))
   `(font-lock-string-face ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-snakeplant-yellow :weight bold))))
   `(font-lock-keyword-face ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-pink-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-red :weight medium))))
   `(font-lock-warning-face ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-error :weight medium))))
   `(font-lock-type-face ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-guava-green :weight medium))))
   `(font-lock-constant-face ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-light-blue :weight medium))))
   `(font-lock-function-name-face ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-light-orange :weight medium))))
   `(font-lock-bracket-face ((,guava-themes-dracaena-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-deep-purple :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-themes-dracaena-class (:background ,guava-themes-dracaena-steel-blue))))

   ;; buttons
   `(link ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-red :underline t :weight bold))))
   `(link-visited ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-light-orange :underline t :weight bold))))
   `(button ((,guava-themes-dracaena-class (:foreground ,guava-themes-dracaena-red :underline t :weight bold))))))

(provide-theme 'guava-themes-dracaena)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes-dracaena-theme.el ends here
