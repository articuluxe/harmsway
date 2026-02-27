;;; guava-themes-cordyline-theme.el --- A theme inspired by the ti plant colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 12, 2026
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
;; A theme inspired by the ti plant colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-cordyline "A theme inspired by the ti plant colors.")

(let* (
      (guava-themes-cordyline-class '((class color) (min-colors 257)))
      (guava-themes-cordyline-black             "#000000")
      (guava-themes-cordyline-white             "#FFFFFF")

      (guava-themes-cordyline-shadow            "#b3b3b3")

      (guava-themes-cordyline-pink              "#CB5F68")
      (guava-themes-cordyline-pink-purple       "#da70d6")

      (guava-themes-cordyline-light-blue        "#5f70cb")
      (guava-themes-cordyline-blue              "#2134d5")
      (guava-themes-cordyline-deep-blue         "#1330af")
      (guava-themes-cordyline-steel-blue        "#4f94cd")
      (guava-themes-cordyline-dark-cyan         "#00708b")

      (guava-themes-cordyline-light-purple      "#a21cd1");8b1cb0,9c1cbc,991cbc
      (guava-themes-cordyline-purple            "#703aaf");54366d,583675,583683,583d83,59338f,673b94
      (guava-themes-cordyline-deep-purple       "#392b38");3f323c
      (guava-themes-cordyline-alt-purple        "#473b4c");433640,43364c
      (guava-themes-cordyline-purple-red        "#982a51");8d2a46,982a46,982a51
      (guava-themes-cordyline-dark-purple       "#211730")

      (guava-themes-cordyline-error             "#FF0000")
      (guava-themes-cordyline-warning           "#f6d909");F68511
      (guava-themes-cordyline-success           "#23a334");239834

      (guava-themes-cordyline-vc-change         guava-themes-cordyline-blue)
      (guava-themes-cordyline-vc-insert         guava-themes-cordyline-success)
      (guava-themes-cordyline-vc-delete         guava-themes-cordyline-error))

  (custom-theme-set-faces
   'guava-themes-cordyline

   ;; default for guava-themes-cordyline
   `(default ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-white :background ,guava-themes-cordyline-deep-purple))))

   ;; error, warning, success
   `(error ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-error :weight bold))))
   `(warning ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-warning :weight bold))))
   `(success ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-pink :foreground ,guava-themes-cordyline-white))))

   ;; fringe
   `(fringe ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-deep-purple :foreground ,guava-themes-cordyline-deep-purple))))
   `(diff-hl-change ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-vc-change :foreground ,guava-themes-cordyline-vc-change))))
   `(diff-hl-insert ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-vc-insert :foreground ,guava-themes-cordyline-vc-insert))))
   `(diff-hl-delete ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-vc-delete :foreground ,guava-themes-cordyline-vc-delete))))

   ;; line-number
   `(line-number ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-white :height 1.35))))
   `(line-number-current-line ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-light-blue :background ,guava-themes-cordyline-alt-purple :weight bold :height 1.35))))

   ;; highlight
   `(highlight ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-alt-purple))))

   ;; shadow
   `(shadow ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-shadow))))

   ;; region
   `(region ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-dark-purple))))

   ;; mode-line
   `(mode-line ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-purple-red :foreground ,guava-themes-cordyline-white))))
   `(mode-line-inactive ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-dark-purple :foreground ,guava-themes-cordyline-white))))
   `(guava-themes-visible-bell ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-light-blue :foreground ,guava-themes-cordyline-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-white))))

   ;; borders
   `(vertical-border ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-deep-purple))))

   ;; header-line
   `(header-line ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-purple-red :foreground ,guava-themes-cordyline-white))))
   `(which-func ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-purple-red :foreground ,guava-themes-cordyline-white))))

   ;; tab-bar
   `(tab-bar ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-deep-purple :foreground ,guava-themes-cordyline-white))))
   `(tab-bar-tab ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-purple-red :foreground ,guava-themes-cordyline-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-deep-purple :foreground ,guava-themes-cordyline-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-deep-purple :foreground ,guava-themes-cordyline-white))))
   `(tab-line-tab ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-deep-purple :foreground ,guava-themes-cordyline-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-purple-red :foreground ,guava-themes-cordyline-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-deep-purple :foreground ,guava-themes-cordyline-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-light-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-themes-cordyline-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-light-blue :weight medium))))
   `(font-lock-string-face ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-pink :weight bold))))
   `(font-lock-keyword-face ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-purple-red :weight medium))))
   `(font-lock-warning-face ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-error :weight medium))))
   `(font-lock-type-face ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-light-purple :weight medium))))
   `(font-lock-constant-face ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-dark-cyan :weight medium))))
   `(font-lock-function-name-face ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-pink-purple :weight medium))))
   `(font-lock-bracket-face ((,guava-themes-cordyline-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-deep-blue :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-themes-cordyline-class (:background ,guava-themes-cordyline-steel-blue))))

   ;; buttons
   `(link ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-steel-blue :underline t :weight bold))))
   `(link-visited ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-purple :underline t :weight bold))))
   `(button ((,guava-themes-cordyline-class (:foreground ,guava-themes-cordyline-steel-blue :underline t :weight bold))))))

(provide-theme 'guava-themes-cordyline)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes-cordyline-theme.el ends here
