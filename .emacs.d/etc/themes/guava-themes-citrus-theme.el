;;; guava-themes-citrus-theme.el --- A theme inspired by citrus colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Feb 15, 2026
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
;; A theme inspired by orange, lime, lemon, and grapefruit colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-citrus "A theme inspired by orange, lime, lemon, and grapefruit colors.")

(let* (
      (guava-themes-citrus-class '((class color) (min-colors 257)))
      (guava-themes-citrus-black                   "#000000")
      (guava-themes-citrus-white                   "#f1f0f5");FFFFFF,eeeeee,f0eff5

      (guava-themes-citrus-shadow                  "#7f7f7f")

      (guava-themes-citrus-lime-green              "#589337");589337
      (guava-themes-citrus-green                   "#136b16")

      (guava-themes-citrus-lemon-light-yellow      "#f5cb6d")
      (guava-themes-citrus-lemon-yellow            "#c3af50")

      (guava-themes-citrus-orange-orange           "#f29a43")
      (guava-themes-citrus-orange-red              "#ce462c")
      (guava-themes-citrus-deep-orange             "#da5802")

      (guava-themes-citrus-brown                   "#86626f")

      (guava-themes-citrus-blue                    "#443f8d");2246d2
      (guava-themes-citrus-light-blue              "#4e9496")
      (guava-themes-citrus-purple-red              "#77003a")

      (guava-themes-citrus-error                   "#FF0000")
      (guava-themes-citrus-warning                 "#F68511")
      (guava-themes-citrus-success                 "#2b5b26");2b5726

      (guava-themes-citrus-vc-change               guava-themes-citrus-blue)
      (guava-themes-citrus-vc-insert               guava-themes-citrus-success)
      (guava-themes-citrus-vc-delete               guava-themes-citrus-error))

  (custom-theme-set-faces
   'guava-themes-citrus

   ;; default for guava-themes-citrus
   `(default ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-black :background ,guava-themes-citrus-white))))

   ;; error, warning, success
   `(error ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-error :weight bold))))
   `(warning ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-warning :weight bold))))
   `(success ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-success :weight bold))))

   ;; cursor
   `(cursor ((,guava-themes-citrus-class (:background ,guava-themes-citrus-green :foreground ,guava-themes-citrus-black))))

   ;; fringe
   `(fringe ((,guava-themes-citrus-class (:background ,guava-themes-citrus-white :foreground ,guava-themes-citrus-black))))
   `(diff-hl-change ((,guava-themes-citrus-class (:background ,guava-themes-citrus-vc-change :foreground ,guava-themes-citrus-vc-change))))
   `(diff-hl-insert ((,guava-themes-citrus-class (:background ,guava-themes-citrus-vc-insert :foreground ,guava-themes-citrus-vc-insert))))
   `(diff-hl-delete ((,guava-themes-citrus-class (:background ,guava-themes-citrus-vc-delete :foreground ,guava-themes-citrus-vc-delete))))

   ;; line-number
   `(line-number ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-black :height 1.35))))
   `(line-number-current-line ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-green :background ,guava-themes-citrus-lemon-light-yellow :weight bold :height 1.35))))

   ;; highlight
   `(highlight ((,guava-themes-citrus-class (:background ,guava-themes-citrus-lemon-light-yellow))))

   ;; shadow
   `(shadow ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-shadow))))

   ;; region
   `(region ((,guava-themes-citrus-class (:background ,guava-themes-citrus-lemon-yellow))))

   ;; mode-line
   `(mode-line ((,guava-themes-citrus-class (:background ,guava-themes-citrus-lime-green :foreground ,guava-themes-citrus-white))))
   `(mode-line-inactive ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-orange :foreground ,guava-themes-citrus-white))))
   `(guava-themes-visible-bell ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-red :foreground ,guava-themes-citrus-white))))

   ;; minibuffer
   `(minibuffer-prompt ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-black))))

   ;; borders
   `(vertical-border ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-white))))

   ;; header-line
   `(header-line ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-red :foreground ,guava-themes-citrus-white))))
   `(which-func ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-red :foreground ,guava-themes-citrus-white))))

   ;; tab-bar
   `(tab-bar ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-orange :foreground ,guava-themes-citrus-white))))
   `(tab-bar-tab ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-red :foreground ,guava-themes-citrus-white :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-orange :foreground ,guava-themes-citrus-white :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-orange :foreground ,guava-themes-citrus-white))))
   `(tab-line-tab ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-orange :foreground ,guava-themes-citrus-white :weight bold :height 0.9))))
   `(tab-line-tab-current ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-red :foreground ,guava-themes-citrus-white :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,guava-themes-citrus-class (:background ,guava-themes-citrus-orange-orange :foreground ,guava-themes-citrus-white :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,guava-themes-citrus-class (:slant italic :weight bold :height 0.9))))

   ;; font-lock
   `(font-lock-comment-face ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-green :weight medium))))
   `(font-lock-string-face ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-success :weight bold))))
   `(font-lock-keyword-face ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-deep-orange :weight medium))))
   `(font-lock-builtin-face ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-blue :weight medium))))
   `(font-lock-warning-face ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-error :weight medium))))
   `(font-lock-type-face ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-purple-red :weight medium))))
   `(font-lock-constant-face ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-light-blue :weight medium))))
   `(font-lock-function-name-face ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-brown :weight medium))))
   `(font-lock-bracket-face ((,guava-themes-citrus-class (:weight medium))))
   `(font-lock-variable-name-face ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-green :weight medium))))

   ;; parentheses
   `(show-paren-match ((,guava-themes-citrus-class (:background ,guava-themes-citrus-light-blue))))

   ;; buttons
   `(link ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-light-blue :underline t :weight bold))))
   `(link-visited ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-blue :underline t :weight bold))))
   `(button ((,guava-themes-citrus-class (:foreground ,guava-themes-citrus-light-blue :underline t :weight bold))))))

(provide-theme 'guava-themes-citrus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; guava-themes-citrus-theme.el ends here
