;;; guava-themes-psidium-theme.el --- A theme inspired by guava colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 07, 2025
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
;; A theme inspired by guava colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-psidium "A theme inspired by guava colors.")

(let* (
      (psidium-class '((class color) (min-colors 257)))
      (psidium-black             "#000000")
      (psidium-white             "#FFFFFF")

      (psidium-shadow            "#7f7f7f")

      (psidium-cream             "#F1EECE");F7DEB6

      (psidium-green             "#599b48");599F48
      (psidium-light-green       "#28bb6b");13765e,13845e,139570,3ab488,3ab992
      (psidium-guava-green       "#aecd34");AECA41,AED234,AED734
      (psidium-deep-green        "#097d2c");09662c

      (psidium-orange            "#ffa55f");FF7D5F,ff8f5f
      (psidium-red-orange        "#cd605f");ff605f,cd605f
      (psidium-red               "#c1153b");f02d1b
      (psidium-light-pink        "#FCD0C9");F8917C
      (psidium-pink              "#F8767C");F84865,F8767C,F88686,F85A65

      (psidium-brown             "#816249");7D5E45

      (psidium-light-blue        "#41C3CA")
      (psidium-oceanic-blue      "#2a4ad9")
      (psidium-deep-blue         "#483d8b");004F5D

      (psidium-light-purple      "#89afd1");817fb2,818fb2,819cb2,89a6d1
      (psidium-purple            "#812db2");D7137C,C0137C,B00CE0,a62db2,8e2db2

      (psidium-error             "#FF0000")
      (psidium-warning           "#d6c800");FF8C00,f08020,f68511,dfe300
      (psidium-success           "#228B22")

      (psidium-vc-change         psidium-light-blue)
      (psidium-vc-insert         psidium-green)
      (psidium-vc-delete         psidium-error))

  (custom-theme-set-faces
   'guava-themes-psidium

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,psidium-class (:foreground ,psidium-black :background ,psidium-cream))))

   ;; error, warning, success
   `(error ((,psidium-class (:foreground ,psidium-error :weight bold))))
   `(warning ((,psidium-class (:foreground ,psidium-warning :weight bold))))
   `(success ((,psidium-class (:foreground ,psidium-success :weight bold))))

   ;; highlight
   `(highlight ((,psidium-class (:background ,psidium-light-pink))))

   ;; shadow
   `(shadow ((,psidium-class (:foreground ,psidium-shadow))))

   ;; region
   `(region ((,psidium-class (:background ,psidium-orange))))
   `(secondary-selection ((,psidium-class (:background ,psidium-light-purple :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,psidium-class (:foreground ,psidium-green :weight medium))))
   `(font-lock-string-face ((,psidium-class (:foreground ,psidium-brown :weight medium))))
   `(font-lock-keyword-face ((,psidium-class (:foreground ,psidium-red :weight medium))))
   `(font-lock-builtin-face ((,psidium-class (:foreground ,psidium-deep-blue :weight medium))))
   `(font-lock-warning-face ((,psidium-class (:foreground ,psidium-warning :weight bold))))
   `(font-lock-type-face ((,psidium-class (:foreground ,psidium-deep-green :weight medium))))
   `(font-lock-constant-face ((,psidium-class (:foreground ,psidium-oceanic-blue :weight medium))))
   `(font-lock-function-name-face ((,psidium-class (:foreground ,psidium-light-green :weight medium))))
   `(font-lock-punctuation-face ((,psidium-class (:foreground ,psidium-red-orange :weight medium))))
   `(font-lock-variable-name-face ((,psidium-class (:foreground ,psidium-purple :weight medium))))
   `(font-lock-negation-char-face ((,psidium-class (:foreground ,psidium-pink :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,psidium-class (:foreground ,psidium-white :background ,psidium-green))))

   ;; fringe
   `(fringe ((,psidium-class (:foreground ,psidium-oceanic-blue :background ,psidium-cream))))
   `(diff-hl-change ((,psidium-class (:foreground ,psidium-vc-change :background ,psidium-vc-change))))
   `(diff-hl-insert ((,psidium-class (:foreground ,psidium-vc-insert :background ,psidium-vc-insert))))
   `(diff-hl-delete ((,psidium-class (:foreground ,psidium-vc-delete :background ,psidium-vc-delete))))

   ;; line-number
   `(line-number ((,psidium-class (:foreground ,psidium-brown))))
   `(line-number-current-line ((,psidium-class (:foreground ,psidium-black :weight bold :inherit highlight))))

   ;; mode-line
   `(mode-line ((,psidium-class (:foreground ,psidium-white :background ,psidium-pink))))
   `(mode-line-inactive ((,psidium-class (:foreground ,psidium-white :background ,psidium-green))))
   `(guava-themes-visible-bell ((,psidium-class (:foreground ,psidium-white :background ,psidium-deep-green))))

   ;; minibuffer
   `(minibuffer-prompt ((,psidium-class (:foreground ,psidium-black))))

   ;;borders
   `(vertical-border ((,psidium-class (:foreground ,psidium-cream))))

   ;; header-line
   `(header-line ((,psidium-class (:foreground ,psidium-white :background ,psidium-pink))))
   `(which-func ((,psidium-class (:foreground ,psidium-white :background ,psidium-pink))))

   ;; tab-bar
   `(tab-bar ((,psidium-class (:foreground ,psidium-white :background ,psidium-guava-green))))
   `(tab-bar-tab ((,psidium-class (:foreground ,psidium-white :background ,psidium-pink :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,psidium-class (:foreground ,psidium-white :background ,psidium-guava-green :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,psidium-class (:foreground ,psidium-white :background ,psidium-guava-green))))
   `(tab-line-tab ((,psidium-class (:foreground ,psidium-white :background ,psidium-guava-green :weight bold :height 0.9))))
   `(tab-line-tab-current ((,psidium-class (:foreground ,psidium-white :background ,psidium-pink :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,psidium-class (:foreground ,psidium-white :background ,psidium-guava-green :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,psidium-class (:foreground ,psidium-white :background ,psidium-light-green :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,psidium-class (:foreground ,psidium-deep-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,psidium-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,psidium-class (:foreground ,psidium-white :background ,psidium-pink))))
   `(show-paren-mismatch ((,psidium-class (:foreground ,psidium-white :background ,psidium-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,psidium-class (:background ,psidium-error))))

   ;; links
   `(link ((,psidium-class (:foreground ,psidium-light-blue :underline t :weight bold))))
   `(link-visited ((,psidium-class (:foreground ,psidium-purple :underline t :weight bold))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,psidium-class (:foreground ,psidium-red))))
   `(elfeed-search-date-face ((,psidium-class (:foreground ,psidium-light-blue))))
   `(elfeed-search-feed-face ((,psidium-class (:foreground ,psidium-brown))))
   `(elfeed-search-title-face ((,psidium-class (:foreground ,psidium-pink))))
   `(elfeed-search-filter-face ((,psidium-class (:foreground ,psidium-purple))))
   `(elfeed-search-last-update-face ((,psidium-class (:foreground ,psidium-oceanic-blue))))
   `(elfeed-search-unread-title-face ((,psidium-class (:weight bold :foreground ,psidium-green))))
   `(elfeed-search-unread-count-face ((,psidium-class (:weight bold :foreground ,psidium-deep-blue))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,psidium-class (:foreground ,psidium-deep-blue))))
   `(doom-modeline-project-parent-dir ((,psidium-class (:foreground ,psidium-deep-blue))))
   `(doom-modeline-buffer-minor-mode ((,psidium-class (:foreground ,psidium-shadow))))

   ;; corfu
   `(corfu-default ((,psidium-class (:foreground ,psidium-black :background ,psidium-cream))))
   `(corfu-current ((,psidium-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,psidium-class (:background ,psidium-shadow))))
   `(corfu-border ((,psidium-class (:background ,psidium-shadow))))

   ;; envrc
   `(envrc-mode-line-error-face ((,psidium-class (:inherit error))))
   `(envrc-mode-line-none-face ((,psidium-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,psidium-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,psidium-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-psidium)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-psidium-theme.el ends here
