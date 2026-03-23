;;; guava-themes-citrus-theme.el --- A theme inspired by orange colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Feb 15, 2026
;; Version: 0.12.0
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
      (citrus-class '((class color) (min-colors 257)))
      (citrus-black                   "#000000")
      (citrus-white                   "#f1f0f5");FFFFFF,eeeeee,f0eff5

      (citrus-shadow                  "#7f7f7f")

      (citrus-lime-green              "#589337");589337
      (citrus-green                   "#136b16")
      (citrus-deep-green              "#2b5b26");2b5726

      (citrus-light-yellow            "#f5cb6d")
      (citrus-yellow                  "#c3af50")

      (citrus-red                     "#960000");a50000
      (citrus-orange-red              "#ce462c")
      (citrus-orange-orange           "#f29a43")
      (citrus-alt-orange              "#f26e43")
      (citrus-deep-orange             "#da5802")

      (citrus-brown                   "#86626f")

      (citrus-light-blue              "#4eb096");4e9496
      (citrus-blue                    "#3275a5")
      (citrus-deep-blue               "#2327dc");2246d2,443f8d,2327dc
      (citrus-light-purple            "#bec8ff")
      (citrus-purple-red              "#77003a")
      (citrus-purple-blue             "#6b4a82")

      (citrus-error                   "#FF0000")
      (citrus-warning                 "#F68511")
      (citrus-success                 "#2b5b26");2b5726

      (citrus-vc-change               citrus-deep-blue)
      (citrus-vc-insert               citrus-success)
      (citrus-vc-delete               citrus-error))

  (custom-theme-set-faces
   'guava-themes-citrus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,citrus-class (:foreground ,citrus-black :background ,citrus-white))))

   ;; error, warning, success
   `(error ((,citrus-class (:foreground ,citrus-error :weight bold))))
   `(warning ((,citrus-class (:foreground ,citrus-warning :weight bold))))
   `(success ((,citrus-class (:foreground ,citrus-success :weight bold))))

   ;; highlight
   `(highlight ((,citrus-class (:background ,citrus-light-yellow))))

   ;; shadow
   `(shadow ((,citrus-class (:foreground ,citrus-shadow))))

   ;; region
   `(region ((,citrus-class (:background ,citrus-yellow))))
   `(secondary-selection ((,citrus-class (:background ,citrus-orange-orange :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,citrus-class (:foreground ,citrus-green :weight medium))))
   `(font-lock-string-face ((,citrus-class (:foreground ,citrus-brown :weight medium))))
   `(font-lock-keyword-face ((,citrus-class (:foreground ,citrus-purple-red :weight medium))))
   `(font-lock-builtin-face ((,citrus-class (:foreground ,citrus-deep-blue :weight medium))))
   `(font-lock-warning-face ((,citrus-class (:foreground ,citrus-warning :weight bold))))
   `(font-lock-type-face ((,citrus-class (:foreground ,citrus-deep-orange :weight medium))))
   `(font-lock-constant-face ((,citrus-class (:foreground ,citrus-blue :weight medium))))
   `(font-lock-function-name-face ((,citrus-class (:foreground ,citrus-lime-green :weight medium))))
   `(font-lock-punctuation-face ((,citrus-class (:foreground ,citrus-purple-blue :weight medium))))
   `(font-lock-variable-name-face ((,citrus-class (:foreground ,citrus-deep-green :weight medium))))
   `(font-lock-negation-char-face ((,citrus-class (:foreground ,citrus-orange-red :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,citrus-class (:foreground ,citrus-black :background ,citrus-alt-orange))))

   ;; fringe
   `(fringe ((,citrus-class (:foreground ,citrus-red :background ,citrus-white))))
   `(diff-hl-change ((,citrus-class (:foreground ,citrus-vc-change :background ,citrus-vc-change))))
   `(diff-hl-insert ((,citrus-class (:foreground ,citrus-vc-insert :background ,citrus-vc-insert))))
   `(diff-hl-delete ((,citrus-class (:foreground ,citrus-vc-delete :background ,citrus-vc-delete))))

   ;; line-number
   `(line-number ((,citrus-class (:foreground ,citrus-black))))
   `(line-number-current-line ((,citrus-class (:foreground ,citrus-green :weight bold :inherit highlight))))

   ;; mode-line
   `(mode-line ((,citrus-class (:foreground ,citrus-white :background ,citrus-lime-green))))
   `(mode-line-inactive ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-orange))))
   `(guava-themes-visible-bell ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-red))))

   ;; minibuffer
   `(minibuffer-prompt ((,citrus-class (:foreground ,citrus-black))))

   ;; borders
   `(vertical-border ((,citrus-class (:foreground ,citrus-white))))

   ;; header-line
   `(header-line ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-red))))
   `(which-func ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-red))))

   ;; tab-bar
   `(tab-bar ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-orange))))
   `(tab-bar-tab ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-red :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-orange :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-orange))))
   `(tab-line-tab ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-orange :weight bold :height 0.9))))
   `(tab-line-tab-current ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-red :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,citrus-class (:foreground ,citrus-white :background ,citrus-orange-orange :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,citrus-class (:foreground ,citrus-white :background ,citrus-alt-orange :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,citrus-class (:foreground ,citrus-deep-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,citrus-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,citrus-class (:foreground ,citrus-white :background ,citrus-lime-green))))
   `(show-paren-mismatch ((,citrus-class (:foreground ,citrus-white :background ,citrus-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,citrus-class (:background ,citrus-error))))

   ;; links
   `(link ((,citrus-class (:foreground ,citrus-light-blue :underline t :weight bold))))
   `(link-visited ((,citrus-class (:foreground ,citrus-deep-blue :underline t :weight bold))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,citrus-class (:foreground ,citrus-lime-green))))
   `(elfeed-search-date-face ((,citrus-class (:foreground ,citrus-deep-blue))))
   `(elfeed-search-feed-face ((,citrus-class (:foreground ,citrus-alt-orange))))
   `(elfeed-search-title-face ((,citrus-class (:foreground ,citrus-purple-red))))
   `(elfeed-search-filter-face ((,citrus-class (:foreground ,citrus-light-yellow))))
   `(elfeed-search-last-update-face ((,citrus-class (:foreground ,citrus-light-purple))))
   `(elfeed-search-unread-title-face ((,citrus-class (:weight bold :foreground ,citrus-green))))
   `(elfeed-search-unread-count-face ((,citrus-class (:weight bold :foreground ,citrus-light-blue))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,citrus-class (:foreground ,citrus-purple-red))))
   `(doom-modeline-project-parent-dir ((,citrus-class (:foreground ,citrus-purple-red))))
   `(doom-modeline-buffer-minor-mode ((,citrus-class (:foreground ,citrus-light-yellow))))

   ;; corfu
   `(corfu-default ((,citrus-class (:foreground ,citrus-black :background ,citrus-white))))
   `(corfu-current ((,citrus-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,citrus-class (:background ,citrus-shadow))))
   `(corfu-border ((,citrus-class (:background ,citrus-shadow))))

   ;; envrc
   `(envrc-mode-line-error-face ((,citrus-class (:inherit error))))
   `(envrc-mode-line-none-face ((,citrus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,citrus-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,citrus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-citrus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-citrus-theme.el ends here
