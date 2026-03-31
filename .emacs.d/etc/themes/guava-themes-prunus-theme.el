;;; guava-themes-prunus-theme.el --- A theme inspired by cherry colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 29, 2025
;; Version: 0.13.0
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
      (prunus-class '((class color) (min-colors 257)))
      ;;(prunus-black             "#000000")
      (prunus-white             "#FFFFFF")

      (prunus-shadow            "#b3b3b3")

      (prunus-cream             "#DEA2BD");fffef5,EBDCF5
      (prunus-brown             "#4a2b1b");583c25,4A301B
      (prunus-dark-brown        "#1A0E05");E9E4F9,3F271D,2E1E03,281A04,1A0E05,1C0E06,1C0F07
      (prunus-alt-dark-brown    "#2b1f16")

      (prunus-yellow            "#999844")

      (prunus-light-green       "#52BC63")
      (prunus-green-forest      "#007341")
      (prunus-oceanic-green     "#3AC3A2")

      (prunus-red               "#88190C");cb001e,d2191e
      (prunus-light-pink        "#cd7888")
      (prunus-pink              "#CD2788");dc6199,cd4f88

      (prunus-blue              "#4548e3");4534e3,120cdc,4534E3,453ee3
      (prunus-deep-blue         "#655DB0")
      (prunus-antarctic-blue    "#8d82ca");8D76CA
      (prunus-cyan              "#00988b")
      (prunus-dark-cyan         "#005555")

      (prunus-deep-purple       "#742fbe");800080,740CBE,7420be
      (prunus-indigo            "#2c4154");2C2C54
      (prunus-purple-red        "#8B2252")

      (prunus-error             "#FF0000");FF0000,bc0000,890014
      (prunus-warning           "#f6d911");F68511,ffc333,F68511
      (prunus-success           "#23D734");228B22,007900

      (prunus-vc-change         prunus-blue)
      (prunus-vc-insert         prunus-success)
      (prunus-vc-delete         prunus-error))

  (custom-theme-set-faces
   'guava-themes-prunus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,prunus-class (:foreground ,prunus-cream :background ,prunus-dark-brown))))

   ;; error, warning, success
   `(error ((,prunus-class (:foreground ,prunus-error :weight bold))))
   `(warning ((,prunus-class (:foreground ,prunus-warning :weight bold))))
   `(success ((,prunus-class (:foreground ,prunus-success :weight bold))))

   ;; highlight
   `(highlight ((,prunus-class (:background ,prunus-alt-dark-brown))))

   ;; shadow
   `(shadow ((,prunus-class (:foreground ,prunus-shadow))))

   ;; region
   `(region ((,prunus-class (:background ,prunus-brown))))
   `(secondary-selection ((,prunus-class (:background ,prunus-dark-cyan :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,prunus-class (:foreground ,prunus-light-green :weight medium))))
   `(font-lock-string-face ((,prunus-class (:foreground ,prunus-purple-red :weight medium))))
   `(font-lock-keyword-face ((,prunus-class (:foreground ,prunus-deep-purple :weight medium))))
   `(font-lock-builtin-face ((,prunus-class (:foreground ,prunus-blue :weight medium))))
   `(font-lock-warning-face ((,prunus-class (:foreground ,prunus-warning :weight bold))))
   `(font-lock-type-face ((,prunus-class (:foreground ,prunus-antarctic-blue :weight medium))))
   `(font-lock-constant-face ((,prunus-class (:foreground ,prunus-cyan :weight medium))))
   `(font-lock-function-name-face ((,prunus-class (:foreground ,prunus-deep-blue :weight medium))))
   `(font-lock-punctuation-face ((,prunus-class (:foreground ,prunus-yellow :weight medium))))
   `(font-lock-variable-name-face ((,prunus-class (:foreground ,prunus-oceanic-green :weight medium))))
   `(font-lock-negation-char-face ((,prunus-class (:foreground ,prunus-red :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,prunus-class (:foreground ,prunus-white :background ,prunus-red))))

   ;; fringe
   `(fringe ((,prunus-class (:foreground ,prunus-cyan :background ,prunus-dark-brown))))
   `(diff-hl-change ((,prunus-class (:foreground ,prunus-vc-change :background ,prunus-vc-change))))
   `(diff-hl-insert ((,prunus-class (:foreground ,prunus-vc-insert :background ,prunus-vc-insert))))
   `(diff-hl-delete ((,prunus-class (:foreground ,prunus-vc-delete :background ,prunus-vc-delete))))

   ;; line-number
   `(line-number ((,prunus-class (:foreground ,prunus-purple-red))))
   `(line-number-current-line ((,prunus-class (:foreground ,prunus-cream :weight bold :inherit highlight))))
   `(line-number-minor-tick ((,prunus-class (:background ,prunus-light-pink))))
   `(line-number-major-tick ((,prunus-class (:background ,prunus-pink))))

   ;; mode-line
   `(mode-line ((,prunus-class (:foreground ,prunus-white :background ,prunus-red))))
   `(mode-line-inactive ((,prunus-class (:foreground ,prunus-white :background ,prunus-brown))))
   `(guava-themes-visible-bell ((,prunus-class (:foreground ,prunus-white :background ,prunus-antarctic-blue))))

   ;; minibuffer
   `(minibuffer-prompt ((,prunus-class (:foreground ,prunus-white))))

   ;; borders
   `(vertical-border ((,prunus-class (:foreground ,prunus-red))))

   ;; header-line
   `(header-line ((,prunus-class (:foreground ,prunus-white :background ,prunus-red))))
   `(which-func ((,prunus-class (:foreground ,prunus-white))))

   ;; tab-bar
   `(tab-bar ((,prunus-class (:foreground ,prunus-white :background ,prunus-pink))))
   `(tab-bar-tab ((,prunus-class (:foreground ,prunus-white :background ,prunus-red :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,prunus-class (:foreground ,prunus-white :background ,prunus-pink :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,prunus-class (:foreground ,prunus-white :background ,prunus-pink))))
   `(tab-line-tab ((,prunus-class (:foreground ,prunus-white :background ,prunus-brown :weight bold :height 0.9))))
   `(tab-line-tab-current ((,prunus-class (:foreground ,prunus-white :background ,prunus-red :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,prunus-class (:foreground ,prunus-white :background ,prunus-pink :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,prunus-class (:foreground ,prunus-white :background ,prunus-light-pink :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,prunus-class (:foreground ,prunus-indigo :weight bold :height 0.9))))
   `(tab-line-tab-special ((,prunus-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,prunus-class (:foreground ,prunus-white :background ,prunus-deep-blue))))
   `(show-paren-mismatch ((,prunus-class (:foreground ,prunus-white :background ,prunus-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,prunus-class (:background ,prunus-error))))

   ;; links
   `(link ((,prunus-class (:foreground ,prunus-blue :underline t :weight bold))))
   `(link-visited ((,prunus-class (:foreground ,prunus-oceanic-green :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,prunus-class (:foreground ,prunus-antarctic-blue :weight medium))))
   `(outline-2 ((,prunus-class (:foreground ,prunus-oceanic-green :weight medium))))
   `(outline-3 ((,prunus-class (:foreground ,prunus-deep-blue :weight medium))))
   `(outline-4 ((,prunus-class (:foreground ,prunus-cyan :weight medium))))
   `(outline-5 ((,prunus-class (:foreground ,prunus-deep-purple :weight medium))))
   `(outline-6 ((,prunus-class (:foreground ,prunus-light-green :weight medium))))
   `(outline-7 ((,prunus-class (:foreground ,prunus-purple-red :weight medium))))
   `(outline-8 ((,prunus-class (:foreground ,prunus-indigo :weight medium))))

   ;; homoglyph, escape-glyph, nobreak-space
   `(homoglyph ((,prunus-class (:foreground ,prunus-cyan))))
   `(escape-glyph ((,prunus-class (:inherit homoglyph))))
   `(nobreak-space ((,prunus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,prunus-class (:foreground ,prunus-deep-blue))))
   `(elfeed-search-date-face ((,prunus-class (:foreground ,prunus-red))))
   `(elfeed-search-feed-face ((,prunus-class (:foreground ,prunus-purple-red))))
   `(elfeed-search-title-face ((,prunus-class (:foreground ,prunus-light-pink))))
   `(elfeed-search-filter-face ((,prunus-class (:foreground ,prunus-antarctic-blue))))
   `(elfeed-search-last-update-face ((,prunus-class (:foreground ,prunus-oceanic-green))))
   `(elfeed-search-unread-title-face ((,prunus-class (:weight bold :foreground ,prunus-green-forest))))
   `(elfeed-search-unread-count-face ((,prunus-class (:weight bold :foreground ,prunus-light-green))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,prunus-class (:foreground ,prunus-light-green))))
   `(doom-modeline-project-parent-dir ((,prunus-class (:foreground ,prunus-light-green))))
   `(doom-modeline-buffer-minor-mode ((,prunus-class (:foreground ,prunus-shadow))))

   ;; corfu
   `(corfu-default ((,prunus-class (:foreground ,prunus-cream :background ,prunus-dark-brown))))
   `(corfu-current ((,prunus-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,prunus-class (:background ,prunus-shadow))))
   `(corfu-border ((,prunus-class (:background ,prunus-shadow))))

   ;; envrc
   `(envrc-mode-line-error-face ((,prunus-class (:inherit error))))
   `(envrc-mode-line-none-face ((,prunus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,prunus-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,prunus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-prunus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-prunus-theme.el ends here
