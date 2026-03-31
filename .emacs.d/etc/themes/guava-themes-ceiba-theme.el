;;; guava-themes-ceiba-theme.el --- A theme inspired by the ceiba tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 21, 2026
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
;; A theme inspired by the ceiba tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-ceiba "A theme inspired by the ceiba tree colors.")

(let* (
      (ceiba-class '((class color) (min-colors 257)))
      (ceiba-black             "#000000")
      (ceiba-white             "#FFFFFF")

      (ceiba-shadow            "#7f7f7f")

      ;; (ceiba-light-gray        "#bab49e");dcdcdc,d4d4d4
      (ceiba-gray-green        "#bab49e");8c857b,8d8d8b,8c7f76,817a68,817a6a
      (ceiba-gray              "#9d9d9d");dcdcdc,656865,7f7f89,787882
      (ceiba-gray-blue         "#798585");8c857b,8d8d8b,6d726a,656865

      (ceiba-light-green       "#61ff96")
      (ceiba-green             "#5b6452");5a6352
      (ceiba-deep-green        "#2b5535");375033,395235,3a5435,3a5835,385635,375535
      (ceiba-green-forest      "#006441")
      (ceiba-green-blue        "#11645a");5a6352,116452

      (ceiba-light-orange      "#f1a147")
      (ceiba-orange            "#ca6f39")
      (ceiba-deep-orange       "#a85639")

      (ceiba-steel-blue        "#aabed8");b0c4de
      (ceiba-blue              "#2327dc")
      (ceiba-alt-blue          "#2268a7");3a5ba7

      (ceiba-light-purple      "#bec8ff")
      (ceiba-purple            "#4e466d");49206d
      (ceiba-purple-red        "#762362");862060,892362

      (ceiba-light-brown       "#a08c6e")
      (ceiba-brown             "#6d4b30");6b492e
      (ceiba-brown-sand        "#826e51");796041
      (ceiba-brown-wood        "#53453d");9c6d85,bf8987,514141,53423e

      (ceiba-error             "#ff0000");ff0000,d70000
      (ceiba-warning           "#f6c911");F68511
      (ceiba-success           "#1ea01e");29c825

      (ceiba-vc-change         ceiba-blue)
      (ceiba-vc-insert         ceiba-success)
      (ceiba-vc-delete         ceiba-error))

  (custom-theme-set-faces
   'guava-themes-ceiba

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-gray-green))))

   ;; error, warning, success
   `(error ((,ceiba-class (:foreground ,ceiba-error :weight bold))))
   `(warning ((,ceiba-class (:foreground ,ceiba-warning :weight bold))))
   `(success ((,ceiba-class (:foreground ,ceiba-success :weight bold))))

   ;; highlight
   `(highlight ((,ceiba-class (:background ,ceiba-gray))))

   ;; shadow
   `(shadow ((,ceiba-class (:foreground ,ceiba-shadow))))

   ;; region
   `(region ((,ceiba-class (:background ,ceiba-gray-blue))))
   `(secondary-selection ((,ceiba-class (:background ,ceiba-brown-sand :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,ceiba-class (:foreground ,ceiba-deep-green :weight medium))))
   `(font-lock-string-face ((,ceiba-class (:foreground ,ceiba-brown :weight medium))))
   `(font-lock-keyword-face ((,ceiba-class (:foreground ,ceiba-purple :weight medium))))
   `(font-lock-builtin-face ((,ceiba-class (:foreground ,ceiba-alt-blue :weight medium))))
   `(font-lock-warning-face ((,ceiba-class (:foreground ,ceiba-warning :weight bold))))
   `(font-lock-type-face ((,ceiba-class (:foreground ,ceiba-green-blue :weight medium))))
   `(font-lock-constant-face ((,ceiba-class (:foreground ,ceiba-deep-orange :weight medium))))
   `(font-lock-function-name-face ((,ceiba-class (:foreground ,ceiba-brown-wood :weight medium))))
   `(font-lock-punctuation-face ((,ceiba-class (:foreground ,ceiba-green-forest :weight medium))))
   `(font-lock-variable-name-face ((,ceiba-class (:foreground ,ceiba-green :weight medium))))
   `(font-lock-negation-char-face ((,ceiba-class (:foreground ,ceiba-orange :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-green-forest))))

   ;; fringe
   `(fringe ((,ceiba-class (:foreground ,ceiba-blue :background ,ceiba-gray-green))))
   `(diff-hl-change ((,ceiba-class (:foreground ,ceiba-vc-change :background ,ceiba-vc-change))))
   `(diff-hl-insert ((,ceiba-class (:foreground ,ceiba-vc-insert :background ,ceiba-vc-insert))))
   `(diff-hl-delete ((,ceiba-class (:foreground ,ceiba-vc-delete :background ,ceiba-vc-delete))))

   ;; line-number
   `(line-number ((,ceiba-class (:foreground ,ceiba-black))))
   `(line-number-current-line ((,ceiba-class (:foreground ,ceiba-deep-green :weight bold :inherit highlight))))
   `(line-number-minor-tick ((,ceiba-class (:background ,ceiba-light-brown))))
   `(line-number-major-tick ((,ceiba-class (:background ,ceiba-brown-sand))))

   ;; mode-line
   `(mode-line ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-green))))
   `(mode-line-inactive ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-gray-blue))))
   `(guava-themes-visible-bell ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-steel-blue))))

   ;; minibuffer
   `(minibuffer-prompt ((,ceiba-class (:foreground ,ceiba-black))))

   ;; borders
   `(vertical-border ((,ceiba-class (:foreground ,ceiba-green))))

   ;; header-line
   `(header-line ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-green))))
   `(which-func ((,ceiba-class (:foreground ,ceiba-white))))

   ;; tab-bar
   `(tab-bar ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-brown-sand))))
   `(tab-bar-tab ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-green :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-brown-sand :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-brown-sand))))
   `(tab-line-tab ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-gray-blue :weight bold :height 0.9))))
   `(tab-line-tab-current ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-green :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-brown-sand :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-light-brown :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,ceiba-class (:foreground ,ceiba-purple-red :weight bold :height 0.9))))
   `(tab-line-tab-special ((,ceiba-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-alt-blue))))
   `(show-paren-mismatch ((,ceiba-class (:foreground ,ceiba-white :background ,ceiba-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,ceiba-class (:background ,ceiba-error))))

   ;; links
   `(link ((,ceiba-class (:foreground ,ceiba-purple :underline t :weight bold))))
   `(link-visited ((,ceiba-class (:foreground ,ceiba-purple-red :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,ceiba-class (:foreground ,ceiba-alt-blue :weight medium))))
   `(outline-2 ((,ceiba-class (:foreground ,ceiba-deep-green :weight medium))))
   `(outline-3 ((,ceiba-class (:foreground ,ceiba-purple-red :weight medium))))
   `(outline-4 ((,ceiba-class (:foreground ,ceiba-green :weight medium))))
   `(outline-5 ((,ceiba-class (:foreground ,ceiba-green-blue :weight medium))))
   `(outline-6 ((,ceiba-class (:foreground ,ceiba-brown-wood :weight medium))))
   `(outline-7 ((,ceiba-class (:foreground ,ceiba-deep-orange :weight medium))))
   `(outline-8 ((,ceiba-class (:foreground ,ceiba-purple :weight medium))))

   ;; homoglyph, escape-glyph, nobreak-space
   `(homoglyph ((,ceiba-class (:foreground ,ceiba-blue))))
   `(escape-glyph ((,ceiba-class (:inherit homoglyph))))
   `(nobreak-space ((,ceiba-class (:box (:line-width (2 . 2)) :inherit homoglyph))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,ceiba-class (:foreground ,ceiba-alt-blue))))
   `(elfeed-search-date-face ((,ceiba-class (:foreground ,ceiba-purple-red))))
   `(elfeed-search-feed-face ((,ceiba-class (:foreground ,ceiba-green-blue))))
   `(elfeed-search-title-face ((,ceiba-class (:foreground ,ceiba-brown))))
   `(elfeed-search-filter-face ((,ceiba-class (:foreground ,ceiba-light-orange))))
   `(elfeed-search-last-update-face ((,ceiba-class (:foreground ,ceiba-light-purple))))
   `(elfeed-search-unread-title-face ((,ceiba-class (:weight bold :foreground ,ceiba-deep-green))))
   `(elfeed-search-unread-count-face ((,ceiba-class (:weight bold :foreground ,ceiba-light-green))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,ceiba-class (:foreground ,ceiba-steel-blue))))
   `(doom-modeline-project-parent-dir ((,ceiba-class (:foreground ,ceiba-steel-blue))))
   `(doom-modeline-buffer-minor-mode ((,ceiba-class (:foreground ,ceiba-shadow))))

   ;; corfu
   `(corfu-default ((,ceiba-class (:foreground ,ceiba-black :background ,ceiba-gray-green))))
   `(corfu-current ((,ceiba-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,ceiba-class (:background ,ceiba-shadow))))
   `(corfu-border ((,ceiba-class (:background ,ceiba-shadow))))

   ;; envrc
   `(envrc-mode-line-error-face ((,ceiba-class (:inherit error))))
   `(envrc-mode-line-none-face ((,ceiba-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,ceiba-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,ceiba-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-ceiba)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-ceiba-theme.el ends here
