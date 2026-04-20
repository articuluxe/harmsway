;;; guava-themes-pyrus-theme.el --- A theme inspired by pear colors -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Apr 16, 2026
;; Version: 0.14.0
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
;; A theme inspired by pear colors.
;;
;;; Code:

(require 'guava-themes)

(deftheme guava-themes-pyrus "A theme inspired by pear colors.")

(let* (
      (pyrus-class '((class color) (min-colors 257)))
      (pyrus-black                     "#000000")
      (pyrus-white                     "#FFFFFF")

      (pyrus-shadow                    "#7f7f7f")

      (pyrus-snow                      "#f9fefd");fafffd,f9fefc
      (pyrus-alt-snow                  "#e6ebe9")
      (pyrus-dark-red                  "#780000");5a0f0f,822425,782525

      (pyrus-yellow-green              "#e5e73b")
      (pyrus-alt-yellow-green          "#c8c887")

      (pyrus-light-green               "#97ad24")
      (pyrus-green                     "#228b22")
      (pyrus-green-subdued             "#316355")
      (pyrus-deep-green                "#295323")

      (pyrus-deep-orange               "#eb4b37")
      (pyrus-orange-subdued            "#c06747")
      (pyrus-red-pink                  "#ab3755")
      (pyrus-red                       "#dc2828")
      (pyrus-orange-pink               "#f38866")

      (pyrus-light-blue                "#4ba5e6");4b91e6
      (pyrus-blue                      "#2a4ad9")
      (pyrus-deep-blue                 "#483d8b")
      (pyrus-cyan                      "#0080ff")

      (pyrus-light-purple              "#8787e1")
      (pyrus-purple                    "#874be1")
      (pyrus-deep-magenta              "#782566")

      (pyrus-error                     "#ff0000")
      (pyrus-warning                   "#ffd200")
      (pyrus-success                   "#228B22")

      (pyrus-diff-added                "#c8f0c8");335533
      (pyrus-diff-removed              "#f0c8c8");553333
      (pyrus-diff-refine-added         "#78f078");22aa22
      (pyrus-diff-refine-removed       "#f07878");aa2222
      (pyrus-diff-header               "#b4b4b4");737373
      (pyrus-diff-file-header          "#8c8c8c");999999
      (pyrus-diff-context              "#dcdcdc");999999

      (pyrus-orderless-0               "#c846e6");af50c8,af46c8
      (pyrus-orderless-1               "#28a03c")
      (pyrus-orderless-2               "#ff6400")
      (pyrus-orderless-3               "#3c82ff")

      (pyrus-vc-change                 pyrus-blue)
      (pyrus-vc-insert                 pyrus-success)
      (pyrus-vc-delete                 pyrus-error))

  (custom-theme-set-faces
   'guava-themes-pyrus

   ;; built-in faces
   ;; with unique colors

   ;; default
   `(default ((,pyrus-class (:foreground ,pyrus-dark-red :background ,pyrus-snow))))

   ;; error, warning, success
   `(error ((,pyrus-class (:foreground ,pyrus-error :weight bold))))
   `(warning ((,pyrus-class (:foreground ,pyrus-warning :weight bold))))
   `(success ((,pyrus-class (:foreground ,pyrus-success :weight bold))))

   ;; highlight
   `(highlight ((,pyrus-class (:background ,pyrus-alt-snow))))

   ;; shadow
   `(shadow ((,pyrus-class (:foreground ,pyrus-shadow))))

   ;; region
   `(region ((,pyrus-class (:background ,pyrus-alt-yellow-green))))
   `(secondary-selection ((,pyrus-class (:background ,pyrus-yellow-green :extend t))))

   ;; font-lock
   `(font-lock-comment-face ((,pyrus-class (:foreground ,pyrus-light-green :weight medium))))
   `(font-lock-string-face ((,pyrus-class (:foreground ,pyrus-deep-orange :weight medium))))
   `(font-lock-keyword-face ((,pyrus-class (:foreground ,pyrus-light-purple :weight medium))))
   `(font-lock-builtin-face ((,pyrus-class (:foreground ,pyrus-deep-blue :weight medium))))
   `(font-lock-warning-face ((,pyrus-class (:foreground ,pyrus-warning :weight bold))))
   `(font-lock-type-face ((,pyrus-class (:foreground ,pyrus-green :weight medium))))
   `(font-lock-constant-face ((,pyrus-class (:foreground ,pyrus-green-subdued :weight medium))))
   `(font-lock-function-name-face ((,pyrus-class (:foreground ,pyrus-red-pink :weight medium))))
   `(font-lock-punctuation-face ((,pyrus-class (:foreground ,pyrus-deep-magenta :weight medium))))
   `(font-lock-variable-name-face ((,pyrus-class (:foreground ,pyrus-orange-pink :weight medium))))
   `(font-lock-negation-char-face ((,pyrus-class (:foreground ,pyrus-orange-subdued :weight medium))))

   ;; built-in faces
   ;; with non-unique colors

   ;; cursor
   `(cursor ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-deep-blue))))

   ;; fringe
   `(fringe ((,pyrus-class (:foreground ,pyrus-blue :background ,pyrus-snow))))
   `(diff-hl-change ((,pyrus-class (:foreground ,pyrus-vc-change :background ,pyrus-vc-change))))
   `(diff-hl-insert ((,pyrus-class (:foreground ,pyrus-vc-insert :background ,pyrus-vc-insert))))
   `(diff-hl-delete ((,pyrus-class (:foreground ,pyrus-vc-delete :background ,pyrus-vc-delete))))

   ;; line-number
   `(line-number ((,pyrus-class (:foreground ,pyrus-dark-red))))
   `(line-number-current-line ((,pyrus-class (:foreground ,pyrus-black :weight bold :inherit highlight))))
   `(line-number-minor-tick ((,pyrus-class (:background ,pyrus-light-green :inherit line-number))))
   `(line-number-major-tick ((,pyrus-class (:background ,pyrus-orange-pink :inherit line-number))))

   ;; mode-line
   `(mode-line ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-deep-green))))
   `(mode-line-inactive ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-light-green))))
   `(guava-themes-visible-bell ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-light-blue))))

   ;; minibuffer
   `(minibuffer-prompt ((,pyrus-class (:foreground ,pyrus-deep-orange))))

   ;; borders
   `(vertical-border ((,pyrus-class (:foreground ,pyrus-deep-green))))

   ;; header-line
   `(header-line ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-deep-green))))
   `(which-func ((,pyrus-class (:foreground ,pyrus-white))))

   ;; tab-bar
   `(tab-bar ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-green))))
   `(tab-bar-tab ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-deep-green :weight bold :height 1.0))))
   `(tab-bar-tab-inactive ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-green :weight bold :height 1.0))))

   ;; tab-line
   `(tab-line ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-green))))
   `(tab-line-tab ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-light-green :weight bold :height 0.9))))
   `(tab-line-tab-current ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-deep-green :weight bold :height 0.9))))
   `(tab-line-tab-inactive ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-green :weight bold :height 0.9))))
   `(tab-line-tab-inactive-alternate ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-green-subdued :weight bold :height 0.9))))
   `(tab-line-tab-modified ((,pyrus-class (:foreground ,pyrus-orange-pink :weight bold :height 0.9))))
   `(tab-line-tab-special ((,pyrus-class (:slant italic :weight bold :height 0.9))))

   ;; parentheses
   `(show-paren-match ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-light-green))))
   `(show-paren-mismatch ((,pyrus-class (:foreground ,pyrus-white :background ,pyrus-error))))

   ;; trailing whitespaces
   `(trailing-whitespace ((,pyrus-class (:background ,pyrus-error))))

   ;; links
   `(link ((,pyrus-class (:foreground ,pyrus-light-blue :underline t :weight bold))))
   `(link-visited ((,pyrus-class (:foreground ,pyrus-green-subdued :underline t :weight bold))))

   ;; outline
   `(outline-1 ((,pyrus-class (:foreground ,pyrus-light-purple :weight medium))))
   `(outline-2 ((,pyrus-class (:foreground ,pyrus-light-green :weight medium))))
   `(outline-3 ((,pyrus-class (:foreground ,pyrus-light-blue :weight medium))))
   `(outline-4 ((,pyrus-class (:foreground ,pyrus-orange-pink :weight medium))))
   `(outline-5 ((,pyrus-class (:foreground ,pyrus-purple :weight medium))))
   `(outline-6 ((,pyrus-class (:foreground ,pyrus-green :weight medium))))
   `(outline-7 ((,pyrus-class (:foreground ,pyrus-cyan :weight medium))))
   `(outline-8 ((,pyrus-class (:foreground ,pyrus-deep-orange :weight medium))))

   ;; homoglyph, escape-glyph, nobreak-space (C-x 8 RET "FORM FEED") (C-x 8 RET "NO-BREAK SPACE")
   `(homoglyph ((,pyrus-class (:foreground ,pyrus-cyan))))
   `(escape-glyph ((,pyrus-class (:inherit homoglyph))))
   `(nobreak-space ((,pyrus-class (:box (:line-width (2 . 2)) :inherit homoglyph))))

   ;; pulse-highlight-start-face
   ;; M-: (pulse-momentary-highlight-region (point-min) (point-max))
   `(pulse-highlight-start-face ((,pyrus-class (:background ,pyrus-light-blue))))

   ;; help-key-binding
   `(help-key-binding ((,pyrus-class (:foreground ,pyrus-cyan :background "grey96" :box (:line-width (-1 . -1) :color "grey80") :inherit fixed-pitch))))

   ;; diff
   `(diff-added ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-added :extend t :inherit diff-changed))))
   `(diff-removed ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-removed :extend t :inherit diff-changed))))
   `(diff-refine-added ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-refine-added :inherit diff-refine-changed))))
   `(diff-refine-removed ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-refine-removed :inherit diff-refine-changed))))
   `(diff-header ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-header :extend t))))
   `(diff-file-header ((,pyrus-class (:weight bold :foreground ,pyrus-black :background ,pyrus-diff-file-header :extend t))))
   `(diff-context ((,pyrus-class (:foreground ,pyrus-black :background ,pyrus-diff-context :extend t))))

   ;; completions
   `(completions-common-part ((,pyrus-class (:foreground ,pyrus-vc-change :weight bold))))
   `(completions-first-difference ((,pyrus-class (:foreground ,pyrus-error :weight bold))))


   ;; external packages

   ;; elfeed
   `(elfeed-search-tag-face ((,pyrus-class (:foreground ,pyrus-deep-orange))))
   `(elfeed-search-date-face ((,pyrus-class (:foreground ,pyrus-deep-blue))))
   `(elfeed-search-feed-face ((,pyrus-class (:foreground ,pyrus-green))))
   `(elfeed-search-title-face ((,pyrus-class (:foreground ,pyrus-green-subdued))))
   `(elfeed-search-filter-face ((,pyrus-class (:foreground ,pyrus-light-blue))))
   `(elfeed-search-last-update-face ((,pyrus-class (:foreground ,pyrus-orange-subdued))))
   `(elfeed-search-unread-title-face ((,pyrus-class (:weight bold :foreground ,pyrus-cyan))))
   `(elfeed-search-unread-count-face ((,pyrus-class (:weight bold :foreground ,pyrus-light-green))))

   ;; doom-modeline
   `(doom-modeline-project-name ((,pyrus-class (:foreground ,pyrus-light-green :inherit italic))))
   `(doom-modeline-project-parent-dir ((,pyrus-class (:foreground ,pyrus-light-green))))
   `(doom-modeline-buffer-minor-mode ((,pyrus-class (:foreground ,pyrus-orange-pink))))

   ;; corfu
   `(corfu-default ((,pyrus-class (:foreground ,pyrus-dark-red :background ,pyrus-snow))))
   `(corfu-current ((,pyrus-class (:foreground unspecified :background unspecified :inherit region))))
   `(corfu-bar ((,pyrus-class (:background ,pyrus-shadow))))
   `(corfu-border ((,pyrus-class (:background ,pyrus-shadow))))

   ;; orderless
   `(orderless-match-face-0 ((,pyrus-class (:foreground ,pyrus-orderless-0 :weight bold))))
   `(orderless-match-face-1 ((,pyrus-class (:foreground ,pyrus-orderless-1 :weight bold))))
   `(orderless-match-face-2 ((,pyrus-class (:foreground ,pyrus-orderless-2 :weight bold))))
   `(orderless-match-face-3 ((,pyrus-class (:foreground ,pyrus-orderless-3 :weight bold))))

   ;; envrc
   `(envrc-mode-line-error-face ((,pyrus-class (:inherit error))))
   `(envrc-mode-line-none-face ((,pyrus-class (:inherit warning))))
   `(envrc-mode-line-on-face ((,pyrus-class (:inherit success))))

   ;; nerd-icons
   ;; nerd-icons-completion
   `(nerd-icons-completion-dir-face ((,pyrus-class (:foreground unspecified :inherit font-lock-function-name-face))))))

(provide-theme 'guava-themes-pyrus)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes-pyrus-theme.el ends here
