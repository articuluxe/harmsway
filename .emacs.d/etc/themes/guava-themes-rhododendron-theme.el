;;; guava-themes-rhododendron-theme.el --- A theme inspired by azalea colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 19, 2026
;; Version: 0.9.0
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
;; A theme inspired by azalea colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-rhododendron "A theme inspired by azalea tree colors.")

(let* (
      (guava-themes-rhododendron-class '((class color) (min-colors 257)))
      (guava-themes-rhododendron-black             "#000000")
      (guava-themes-rhododendron-white             "#FFFFFF")

      (guava-themes-rhododendron-forest-green      "#228b22")
      (guava-themes-rhododendron-deep-green        "#3e7411");3e7011

      (guava-themes-rhododendron-red               "#c00353")
      
      (guava-themes-rhododendron-bright-orange     "#ff5b4c")

      (guava-themes-rhododendron-blue              "#3c3cee")
      (guava-themes-rhododendron-deep-blue         "#3e3d8b")

      (guava-themes-rhododendron-light-pink        "#e8c7e3");e8c5e3
      (guava-themes-rhododendron-bright-pink       "#fd3aae");fd31ae
      (guava-themes-rhododendron-alt-bright-pink   "#f197f5");f194f5
      (guava-themes-rhododendron-deep-pink         "#c00e88");d00e88
      
      (guava-themes-rhododendron-light-purple      "#e0bde7")
      (guava-themes-rhododendron-purple-pink       "#ad20f0");a020f0
      (guava-themes-rhododendron-purple-blue       "#5346cc");534bcc
      (guava-themes-rhododendron-purple-red        "#a8206f")

      (guava-themes-rhododendron-error             "#FF0000")
      (guava-themes-rhododendron-warning           "#F68511")
      (guava-themes-rhododendron-success           "#29d925")

      (guava-themes-rhododendron-vc-change         guava-themes-rhododendron-blue)
      (guava-themes-rhododendron-vc-insert         guava-themes-rhododendron-success)
      (guava-themes-rhododendron-vc-delete         guava-themes-rhododendron-error))

  (custom-theme-set-faces
   'guava-themes-rhododendron

   ;; default for guava-themes-rhododendron
   `(default ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-black :background ,guava-themes-rhododendron-light-pink))))

   ;; error, warning, success
   `(error ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-error :weight bold))))
   `(warning ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-warning :weight bold))))
   `(success ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-purple-red :foreground ,guava-themes-rhododendron-white))))

   ;; fringe
   `(fringe ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-light-pink :foreground ,guava-themes-rhododendron-white))))
   `(diff-hl-change ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-vc-change :foreground ,guava-themes-rhododendron-vc-change))))
   `(diff-hl-insert ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-vc-insert :foreground ,guava-themes-rhododendron-vc-insert))))
   `(diff-hl-delete ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-vc-delete :foreground ,guava-themes-rhododendron-vc-delete))))

   ;; line-number
   `(line-number ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-black :height 1.35))))
   `(line-number-current-line ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-purple-red :background ,guava-themes-rhododendron-light-purple :weight bold :height 1.35))))

   ;; hl-line
   `(hl-line ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-light-purple))))

   ;; region
   `(region ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-alt-bright-pink))))

   ;; mode-line
   `(mode-line ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-red :foreground ,guava-themes-rhododendron-white))))
   `(mode-line-inactive ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-bright-pink :foreground ,guava-themes-rhododendron-white))))
   `(guava-themes-visible-bell ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-purple-blue :foreground ,guava-themes-rhododendron-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-black))))

   ;; borders
   `(vertical-border ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-bright-pink))))

   ;; header-line
   `(header-line ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-red :foreground ,guava-themes-rhododendron-white))))
   `(which-func ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-red :foreground ,guava-themes-rhododendron-white))))

   ;; tab-bar
   `(tab-bar ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-bright-pink :foreground ,guava-themes-rhododendron-white))))
   `(tab-bar-tab ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-red :foreground ,guava-themes-rhododendron-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-bright-pink :foreground ,guava-themes-rhododendron-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-bright-pink :foreground ,guava-themes-rhododendron-white))))
   `(tab-line-tab ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-bright-pink :foreground ,guava-themes-rhododendron-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-red :foreground ,guava-themes-rhododendron-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-bright-pink :foreground ,guava-themes-rhododendron-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-purple-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-themes-rhododendron-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-deep-green :weight medium))))
   `(font-lock-string-face ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-purple-red :weight bold))))
   `(font-lock-keyword-face ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-bright-pink :weight medium))))
   `(font-lock-builtin-face ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-deep-blue :weight medium))))
   `(font-lock-warning-face ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-error :weight medium))))
   `(font-lock-type-face ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-forest-green :weight medium))))
   `(font-lock-constant-face ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-purple-blue :weight medium))))
   `(font-lock-function-name-face ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-deep-pink :weight medium))))
   `(font-lock-bracket-face ((,guava-themes-rhododendron-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-bright-orange :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-themes-rhododendron-class (:background ,guava-themes-rhododendron-blue))))

   ;; buttons
   `(link ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-purple-blue :underline t :weight bold))))
   `(link-visited ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-purple-pink :underline t :weight bold))))
   `(button ((,guava-themes-rhododendron-class (:foreground ,guava-themes-rhododendron-purple-blue :underline t :weight bold))))))

(provide-theme 'guava-themes-rhododendron)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes-rhododendron-theme.el ends here
