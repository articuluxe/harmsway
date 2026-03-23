;;; guava-themes-rhododendron-theme.el --- A theme inspired by the azalea tree colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Jan 19, 2026
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
;; A theme inspired by the azalea tree colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-rhododendron "A theme inspired by the azalea tree colors.")

(let* (
      (rhododendron-class '((class color) (min-colors 257)))
      (rhododendron-black             "#000000")
      (rhododendron-white             "#FFFFFF")

      (rhododendron-shadow            "#7f7f7f")

      (rhododendron-light-green       "#64d264");2ad22a,46d246
      (rhododendron-forest-green      "#228b22")
      (rhododendron-deep-green        "#3e7411");3e7011

      (rhododendron-red               "#c00353")
      (rhododendron-red-orange        "#cd605f");ff605f,cd605f
      (rhododendron-bright-orange     "#ff5b4c")
      (rhododendron-cream             "#fcbdb2")

      (rhododendron-blue              "#3c3cee")
      (rhododendron-deep-blue         "#3e3d8b")

      (rhododendron-light-pink        "#e8c7e3");e8c5e3
      (rhododendron-bright-pink       "#fd3aae");fd31ae
      (rhododendron-alt-bright-pink   "#f197f5");f194f5
      (rhododendron-deep-pink         "#c00e88");d00e88

      (rhododendron-light-purple      "#e0bde7")
      (rhododendron-purple-pink       "#ad20f0");a020f0
      (rhododendron-purple-blue       "#5346cc");534bcc
      (rhododendron-purple-red        "#a8206f")

      (rhododendron-error             "#FF0000")
      (rhododendron-warning           "#F68511")
      (rhododendron-success           "#29d925")

      (rhododendron-vc-change         rhododendron-blue)
      (rhododendron-vc-insert         rhododendron-success)
      (rhododendron-vc-delete         rhododendron-error))

  (custom-theme-set-faces
   'guava-themes-rhododendron

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-light-pink))))

   ;; error, warning, success
   `(error ((,rhododendron-class (:foreground ,rhododendron-error :weight bold))))
   `(warning ((,rhododendron-class (:foreground ,rhododendron-warning :weight bold))))
   `(success ((,rhododendron-class (:foreground ,rhododendron-success :weight bold))))

   ;; highlight
   `(highlight ((,rhododendron-class (:background ,rhododendron-light-purple))))

   ;; shadow
   `(shadow ((,rhododendron-class (:foreground ,rhododendron-shadow))))

   ;; region
   `(region ((,rhododendron-class (:background ,rhododendron-alt-bright-pink))))
   `(secondary-selection ((,rhododendron-class (:background ,rhododendron-light-green :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,rhododendron-class (:foreground ,rhododendron-deep-green :weight medium))))
   `(font-lock-string-face ((,rhododendron-class (:foreground ,rhododendron-purple-red :weight medium))))
   `(font-lock-keyword-face ((,rhododendron-class (:foreground ,rhododendron-bright-pink :weight medium))))
   `(font-lock-builtin-face ((,rhododendron-class (:foreground ,rhododendron-deep-blue :weight medium))))
   `(font-lock-warning-face ((,rhododendron-class (:foreground ,rhododendron-warning :weight bold))))
   `(font-lock-type-face ((,rhododendron-class (:foreground ,rhododendron-forest-green :weight medium))))
   `(font-lock-constant-face ((,rhododendron-class (:foreground ,rhododendron-purple-blue :weight medium))))
   `(font-lock-function-name-face ((,rhododendron-class (:foreground ,rhododendron-deep-pink :weight medium))))
   `(font-lock-punctuation-face ((,rhododendron-class (:foreground ,rhododendron-red :weight medium))))
   `(font-lock-variable-name-face ((,rhododendron-class (:foreground ,rhododendron-bright-orange :weight medium))))
   `(font-lock-negation-char-face ((,rhododendron-class (:foreground ,rhododendron-red-orange :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-red))))

   ;; fringe
   `(fringe ((,rhododendron-class (:foreground ,rhododendron-red :background ,rhododendron-light-pink))))
   `(diff-hl-change ((,rhododendron-class (:foreground ,rhododendron-vc-change :background ,rhododendron-vc-change))))
   `(diff-hl-insert ((,rhododendron-class (:foreground ,rhododendron-vc-insert :background ,rhododendron-vc-insert))))
   `(diff-hl-delete ((,rhododendron-class (:foreground ,rhododendron-vc-delete :background ,rhododendron-vc-delete))))

   ;; line-number
   `(line-number ((,rhododendron-class (:foreground ,rhododendron-black))))
   `(line-number-current-line ((,rhododendron-class (:foreground ,rhododendron-purple-red :weight bold :inherit highlight))))

   ;; mode-line
   `(mode-line ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-red))))
   `(mode-line-inactive ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-bright-pink))))
   `(guava-themes-visible-bell ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-purple-blue))))

   ;; minibuffer
   `(minibuffer-prompt ((,rhododendron-class (:foreground ,rhododendron-black))))

   ;; borders
   `(vertical-border ((,rhododendron-class (:foreground ,rhododendron-bright-pink))))

   ;; header-line
   `(header-line ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-red))))
   `(which-func ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-red))))

   ;; tab-bar
   `(tab-bar ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-bright-pink))))
   `(tab-bar-tab ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-red :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-bright-pink :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-bright-pink))))
   `(tab-line-tab ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-bright-pink :weight bold :height 0.9))))
   `(tab-line-tab-current ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-red :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-bright-pink :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-purple-red :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,rhododendron-class (:foreground ,rhododendron-purple-blue :weight bold :height 0.9))))
   `(tab-line-tab-special ((,rhododendron-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-bright-pink))))
   `(show-paren-mismatch ((,rhododendron-class (:foreground ,rhododendron-white :background ,rhododendron-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,rhododendron-class (:background ,rhododendron-error))))

   ;; links
   `(link ((,rhododendron-class (:foreground ,rhododendron-purple-blue :underline t :weight bold))))
   `(link-visited ((,rhododendron-class (:foreground ,rhododendron-purple-pink :underline t :weight bold))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,rhododendron-class (:foreground ,rhododendron-bright-pink))))
   `(elfeed-search-date-face ((,rhododendron-class (:foreground ,rhododendron-forest-green))))
   `(elfeed-search-feed-face ((,rhododendron-class (:foreground ,rhododendron-purple-blue))))
   `(elfeed-search-title-face ((,rhododendron-class (:foreground ,rhododendron-deep-blue))))
   `(elfeed-search-filter-face ((,rhododendron-class (:foreground ,rhododendron-light-purple))))
   `(elfeed-search-last-update-face ((,rhododendron-class (:foreground ,rhododendron-cream))))
   `(elfeed-search-unread-title-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-purple-red))))
   `(elfeed-search-unread-count-face ((,rhododendron-class (:weight bold :foreground ,rhododendron-light-green))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,rhododendron-class (:foreground ,rhododendron-light-purple))))
   `(doom-modeline-project-parent-dir ((,rhododendron-class (:foreground ,rhododendron-light-purple))))
   `(doom-modeline-buffer-minor-mode ((,rhododendron-class (:foreground ,rhododendron-bright-orange))))

   ;; corfu
   `(corfu-default ((,rhododendron-class (:foreground ,rhododendron-black :background ,rhododendron-light-pink))))
   `(corfu-current ((,rhododendron-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,rhododendron-class (:background ,rhododendron-shadow))))
   `(corfu-border ((,rhododendron-class (:background ,rhododendron-shadow))))

   ;; envrc
   `(envrc-mode-line-error-face ((,rhododendron-class (:inherit error))))
   `(envrc-mode-line-none-face ((,rhododendron-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,rhododendron-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,rhododendron-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-rhododendron)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-rhododendron-theme.el ends here
