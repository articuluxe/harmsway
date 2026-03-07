;;; guava-themes-acer-theme.el --- A theme inspired by maple colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 12, 2026
;; Version: 0.11.3
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
;; A theme inspired by maple colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-acer "A theme inspired by maple colors.")

(let* (
      (guava-themes-acer-class '((class color) (min-colors 257)))
      (guava-themes-acer-black             "#000000")
      (guava-themes-acer-white             "#FFFFFF")

      (guava-themes-acer-shadow            "#7f7f7f")

      (guava-themes-acer-green             "#237c34");239834,237c34,3b7b27
      (guava-themes-acer-green-cyan        "#277a6a")
      (guava-themes-acer-deep-green        "#006041");8a8a8a,828282,787878,746c72,747474,606060,60607e,59597e,505069

      (guava-themes-acer-orange            "#e76144");fe7457
      (guava-themes-acer-deep-orange       "#d44400")
      (guava-themes-acer-orange-pink       "#cd475f");ec31a3,ff514a,ff474a,ff475f
      (guava-themes-acer-yellow            "#f7a95a");fca148,fca347,f5a24e,f5a44e,F7B36A,f7a95a
      (guava-themes-acer-autumn            "#ff8c4e");c14c5c,f46157,db3d32,e14337,ed7038,f77b44,f3814f,e8674a,ed674a,f68b47

      (guava-themes-acer-cream             "#dc9964");f68e64,dc8e64,dc9964
      (guava-themes-acer-brown             "#c88550");7d4826,754014,a58464,a07f5f

      (guava-themes-acer-blue              "#2134d5")

      (guava-themes-acer-purple            "#9e4d76")
      (guava-themes-acer-deep-purple       "#60366e");663c6c,62386c
      (guava-themes-acer-purple-red        "#9b234b");9f234b

      (guava-themes-acer-error             "#c80000");FF0000
      (guava-themes-acer-warning           "#f2e16b");F68511
      (guava-themes-acer-success           "#237c34");23D734,239834

      (guava-themes-acer-vc-change         guava-themes-acer-blue)
      (guava-themes-acer-vc-insert         guava-themes-acer-success)
      (guava-themes-acer-vc-delete         guava-themes-acer-error))

  (custom-theme-set-faces
   'guava-themes-acer

   ;; default for guava-themes-acer
   `(default ((,guava-themes-acer-class (:foreground ,guava-themes-acer-black :background ,guava-themes-acer-yellow))))

   ;; error, warning, success
   `(error ((,guava-themes-acer-class (:foreground ,guava-themes-acer-error :weight bold))))
   `(warning ((,guava-themes-acer-class (:foreground ,guava-themes-acer-warning :weight bold))))
   `(success ((,guava-themes-acer-class (:foreground ,guava-themes-acer-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-themes-acer-class (:background ,guava-themes-acer-green :foreground ,guava-themes-acer-black))))

   ;; fringe
   `(fringe ((,guava-themes-acer-class (:background ,guava-themes-acer-yellow :foreground ,guava-themes-acer-black))))
   `(diff-hl-change ((,guava-themes-acer-class (:background ,guava-themes-acer-vc-change :foreground ,guava-themes-acer-vc-change))))
   `(diff-hl-insert ((,guava-themes-acer-class (:background ,guava-themes-acer-vc-insert :foreground ,guava-themes-acer-vc-insert))))
   `(diff-hl-delete ((,guava-themes-acer-class (:background ,guava-themes-acer-vc-delete :foreground ,guava-themes-acer-vc-delete))))

   ;; line-number
   `(line-number ((,guava-themes-acer-class (:foreground ,guava-themes-acer-black))))
   `(line-number-current-line ((,guava-themes-acer-class (:foreground ,guava-themes-acer-deep-orange :background ,guava-themes-acer-cream :weight bold))))

   ;; highlight
   `(highlight ((,guava-themes-acer-class (:background ,guava-themes-acer-cream))))

   ;; shadow
   `(shadow ((,guava-themes-acer-class (:foreground ,guava-themes-acer-shadow))))

   ;; region
   `(region ((,guava-themes-acer-class (:background ,guava-themes-acer-brown))))

   ;; mode-line
   `(mode-line ((,guava-themes-acer-class (:background ,guava-themes-acer-orange :foreground ,guava-themes-acer-white))))
   `(mode-line-inactive ((,guava-themes-acer-class (:background ,guava-themes-acer-purple-red :foreground ,guava-themes-acer-white))))
   `(guava-themes-visible-bell ((,guava-themes-acer-class (:background ,guava-themes-acer-green :foreground ,guava-themes-acer-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-themes-acer-class (:foreground ,guava-themes-acer-black))))

   ;; borders
   `(vertical-border ((,guava-themes-acer-class (:foreground ,guava-themes-acer-yellow))))

   ;; header-line
   `(header-line ((,guava-themes-acer-class (:background ,guava-themes-acer-orange :foreground ,guava-themes-acer-white))))
   `(which-func ((,guava-themes-acer-class (:background ,guava-themes-acer-orange :foreground ,guava-themes-acer-white))))

   ;; tab-bar
   `(tab-bar ((,guava-themes-acer-class (:background ,guava-themes-acer-autumn :foreground ,guava-themes-acer-white))))
   `(tab-bar-tab ((,guava-themes-acer-class (:background ,guava-themes-acer-orange :foreground ,guava-themes-acer-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-themes-acer-class (:background ,guava-themes-acer-autumn :foreground ,guava-themes-acer-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-themes-acer-class (:background ,guava-themes-acer-autumn :foreground ,guava-themes-acer-white))))
   `(tab-line-tab ((,guava-themes-acer-class (:background ,guava-themes-acer-autumn :foreground ,guava-themes-acer-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-themes-acer-class (:background ,guava-themes-acer-orange :foreground ,guava-themes-acer-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-themes-acer-class (:background ,guava-themes-acer-autumn :foreground ,guava-themes-acer-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-themes-acer-class (:foreground ,guava-themes-acer-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-themes-acer-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-themes-acer-class (:foreground ,guava-themes-acer-deep-green :weight medium))))
   `(font-lock-string-face ((,guava-themes-acer-class (:foreground ,guava-themes-acer-deep-orange :weight medium))))
   `(font-lock-keyword-face ((,guava-themes-acer-class (:foreground ,guava-themes-acer-deep-purple :weight medium))))
   `(font-lock-builtin-face ((,guava-themes-acer-class (:foreground ,guava-themes-acer-purple :weight medium))))
   `(font-lock-warning-face ((,guava-themes-acer-class (:foreground ,guava-themes-acer-error :weight bold))))
   `(font-lock-type-face ((,guava-themes-acer-class (:foreground ,guava-themes-acer-green :weight medium))))
   `(font-lock-constant-face ((,guava-themes-acer-class (:foreground ,guava-themes-acer-green-cyan :weight medium))))
   `(font-lock-function-name-face ((,guava-themes-acer-class (:foreground ,guava-themes-acer-purple-red :weight medium))))
   `(font-lock-bracket-face ((,guava-themes-acer-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-themes-acer-class (:foreground ,guava-themes-acer-orange-pink :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-themes-acer-class (:foreground ,guava-themes-acer-white :background ,guava-themes-acer-green))))
   `(show-paren-mismatch ((,guava-themes-acer-class (:foreground ,guava-themes-acer-white :background ,guava-themes-acer-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,guava-themes-acer-class (:background ,guava-themes-acer-error))))

   ;; buttons
   `(link ((,guava-themes-acer-class (:foreground ,guava-themes-acer-blue :underline t :weight bold))))
   `(link-visited ((,guava-themes-acer-class (:foreground ,guava-themes-acer-green-cyan :underline t :weight bold))))
   `(button ((,guava-themes-acer-class (:foreground ,guava-themes-acer-blue :underline t :weight bold))))


   ;; external packages

   ;; doom-modeline
   `(doom-modeline-project-name ((,guava-themes-acer-class (:foreground ,guava-themes-acer-deep-purple))))
   `(doom-modeline-project-parent-dir ((,guava-themes-acer-class (:foreground ,guava-themes-acer-deep-purple))))
   `(doom-modeline-buffer-minor-mode ((,guava-themes-acer-class (:foreground ,guava-themes-acer-purple))))))

(provide-theme 'guava-themes-acer)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes-acer-theme.el ends here
