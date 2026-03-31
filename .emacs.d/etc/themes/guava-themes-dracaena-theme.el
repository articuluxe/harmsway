;;; guava-themes-dracaena-theme.el --- A theme inspired by the dragon tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 06, 2026
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
;; A theme inspired by the dragon tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-dracaena "A theme inspired by the dragon tree colors.")

(let* (
      (dracaena-class '((class color) (min-colors 257)))
      (dracaena-black             "#000000")
      (dracaena-white             "#FFFFFF")

      (dracaena-shadow            "#b3b3b3")

      (dracaena-light-gray        "#4f4f4f")
      (dracaena-gray              "#424845");404242,3e4040,3d3f3f,424545
      (dracaena-dark-gray         "#353838")

      (dracaena-guava-green       "#AED734")
      (dracaena-deep-green        "#298e25")

      (dracaena-snakeplant-yellow "#d9d389")

      (dracaena-light-orange      "#ffb38f");ffae7a
      (dracaena-orange            "#e88e6b");ffa07a
      (dracaena-deep-orange       "#c95909")
      (dracaena-red               "#c82333");c91628,cc192a,d7192a,ed2725,e92333,db2333,d32333
      (dracaena-alt-deep-red      "#751e1e")
      (dracaena-deep-red          "#792725")

      (dracaena-light-brown       "#a89d92");8b6c4d

      (dracaena-light-blue        "#4272d5");425fd5
      (dracaena-blue              "#2d44d5");2134d5
      (dracaena-antarctic-blue    "#bacce4");4f62be
      (dracaena-steel-blue        "#4f94cd");b0c4de
      (dracaena-cyan              "#00ffff")
      (dracaena-dark-cyan         "#005f55")

      (dracaena-pink-purple       "#a54666");961250,b7125c,a5225c,a53c5c
      (dracaena-deep-purple       "#7f6ddd");8f1ac8,8a34d7,7f3edd,7f46dd,7f53dd

      (dracaena-error             "#FF0000")
      (dracaena-warning           "#f6be14");F68511,f6ad11
      (dracaena-success           "#29d925")

      (dracaena-vc-change         dracaena-blue)
      (dracaena-vc-insert         dracaena-deep-green)
      (dracaena-vc-delete         dracaena-error))

  (custom-theme-set-faces
   'guava-themes-dracaena

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,dracaena-class (:foreground ,dracaena-antarctic-blue :background ,dracaena-gray))))

   ;; error, warning, success
   `(error ((,dracaena-class (:foreground ,dracaena-error :weight bold))))
   `(warning ((,dracaena-class (:foreground ,dracaena-warning :weight bold))))
   `(success ((,dracaena-class (:foreground ,dracaena-success :weight bold))))

   ;; highlight
   `(highlight ((,dracaena-class (:background ,dracaena-light-gray))))

   ;; shadow
   `(shadow ((,dracaena-class (:foreground ,dracaena-shadow))))

   ;; region
   `(region ((,dracaena-class (:background ,dracaena-alt-deep-red))))
   `(secondary-selection ((,dracaena-class (:background ,dracaena-dark-cyan :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,dracaena-class (:foreground ,dracaena-deep-green :weight medium))))
   `(font-lock-string-face ((,dracaena-class (:foreground ,dracaena-snakeplant-yellow :weight medium))))
   `(font-lock-keyword-face ((,dracaena-class (:foreground ,dracaena-pink-purple :weight medium))))
   `(font-lock-builtin-face ((,dracaena-class (:foreground ,dracaena-red :weight medium))))
   `(font-lock-warning-face ((,dracaena-class (:foreground ,dracaena-warning :weight bold))))
   `(font-lock-type-face ((,dracaena-class (:foreground ,dracaena-guava-green :weight medium))))
   `(font-lock-constant-face ((,dracaena-class (:foreground ,dracaena-light-blue :weight medium))))
   `(font-lock-function-name-face ((,dracaena-class (:foreground ,dracaena-light-orange :weight medium))))
   `(font-lock-punctuation-face ((,dracaena-class (:foreground ,dracaena-light-brown :weight medium))))
   `(font-lock-variable-name-face ((,dracaena-class (:foreground ,dracaena-deep-purple :weight medium))))
   `(font-lock-negation-char-face ((,dracaena-class (:foreground ,dracaena-orange :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-orange))))

   ;; fringe
   `(fringe ((,dracaena-class (:foreground ,dracaena-light-orange :background ,dracaena-gray))))
   `(diff-hl-change ((,dracaena-class (:foreground ,dracaena-vc-change :background ,dracaena-vc-change))))
   `(diff-hl-insert ((,dracaena-class (:foreground ,dracaena-vc-insert :background ,dracaena-vc-insert))))
   `(diff-hl-delete ((,dracaena-class (:foreground ,dracaena-vc-delete :background ,dracaena-vc-delete))))

   ;; line-number
   `(line-number ((,dracaena-class (:foreground ,dracaena-antarctic-blue))))
   `(line-number-current-line ((,dracaena-class (:foreground ,dracaena-orange :weight bold :inherit highlight))))
   `(line-number-minor-tick ((,dracaena-class (:background ,dracaena-steel-blue))))
   `(line-number-major-tick ((,dracaena-class (:background ,dracaena-orange))))

   ;; mode-line
   `(mode-line ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-deep-red))))
   `(mode-line-inactive ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-deep-orange))))
   `(guava-themes-visible-bell ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-orange))))

   ;; minibuffer
   `(minibuffer-prompt ((,dracaena-class (:foreground ,dracaena-antarctic-blue))))

   ;; borders
   `(vertical-border ((,dracaena-class (:foreground ,dracaena-deep-red))))

   ;; header-line
   `(header-line ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-deep-red))))
   `(which-func ((,dracaena-class (:foreground ,dracaena-white))))

   ;; tab-bar
   `(tab-bar ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-dark-gray))))
   `(tab-bar-tab ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-deep-red :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-dark-gray :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-dark-gray))))
   `(tab-line-tab ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-deep-orange :weight bold :height 0.9))))
   `(tab-line-tab-current ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-deep-red :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-dark-gray :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-light-gray :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,dracaena-class (:foreground ,dracaena-orange :weight bold :height 0.9))))
   `(tab-line-tab-special ((,dracaena-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,dracaena-class (:foreground ,dracaena-black :background ,dracaena-steel-blue))))
   `(show-paren-mismatch ((,dracaena-class (:foreground ,dracaena-white :background ,dracaena-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,dracaena-class (:background ,dracaena-error))))

   ;; links
   `(link ((,dracaena-class (:foreground ,dracaena-red :underline t :weight bold))))
   `(link-visited ((,dracaena-class (:foreground ,dracaena-orange :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,dracaena-class (:foreground ,dracaena-light-orange :weight medium))))
   `(outline-2 ((,dracaena-class (:foreground ,dracaena-deep-purple :weight medium))))
   `(outline-3 ((,dracaena-class (:foreground ,dracaena-pink-purple :weight medium))))
   `(outline-4 ((,dracaena-class (:foreground ,dracaena-deep-green :weight medium))))
   `(outline-5 ((,dracaena-class (:foreground ,dracaena-guava-green :weight medium))))
   `(outline-6 ((,dracaena-class (:foreground ,dracaena-light-blue :weight medium))))
   `(outline-7 ((,dracaena-class (:foreground ,dracaena-red :weight medium))))
   `(outline-8 ((,dracaena-class (:foreground ,dracaena-snakeplant-yellow :weight medium))))

   ;; homoglyph, escape-glyph, nobreak-space
   `(homoglyph ((,dracaena-class (:foreground ,dracaena-cyan))))
   `(escape-glyph ((,dracaena-class (:inherit homoglyph))))
   `(nobreak-space ((,dracaena-class (:box (:line-width (2 . 2)) :inherit homoglyph))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,dracaena-class (:foreground ,dracaena-steel-blue))))
   `(elfeed-search-date-face ((,dracaena-class (:foreground ,dracaena-light-blue))))
   `(elfeed-search-feed-face ((,dracaena-class (:foreground ,dracaena-pink-purple))))
   `(elfeed-search-title-face ((,dracaena-class (:foreground ,dracaena-snakeplant-yellow))))
   `(elfeed-search-filter-face ((,dracaena-class (:foreground ,dracaena-deep-purple))))
   `(elfeed-search-last-update-face ((,dracaena-class (:foreground ,dracaena-light-orange))))
   `(elfeed-search-unread-title-face ((,dracaena-class (:weight bold :foreground ,dracaena-deep-green))))
   `(elfeed-search-unread-count-face ((,dracaena-class (:weight bold :foreground ,dracaena-guava-green))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,dracaena-class (:foreground ,dracaena-snakeplant-yellow))))
   `(doom-modeline-project-parent-dir ((,dracaena-class (:foreground ,dracaena-snakeplant-yellow))))
   `(doom-modeline-buffer-minor-mode ((,dracaena-class (:foreground ,dracaena-deep-green))))

   ;; corfu
   `(corfu-default ((,dracaena-class (:foreground ,dracaena-antarctic-blue :background ,dracaena-gray))))
   `(corfu-current ((,dracaena-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,dracaena-class (:background ,dracaena-shadow))))
   `(corfu-border ((,dracaena-class (:background ,dracaena-shadow))))

   ;; envrc
   `(envrc-mode-line-error-face ((,dracaena-class (:inherit error))))
   `(envrc-mode-line-none-face ((,dracaena-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,dracaena-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,dracaena-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-dracaena)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-dracaena-theme.el ends here
