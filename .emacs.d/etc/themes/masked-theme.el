;;; masked-theme.el --- Masked color theme for GNU Emacs.

;; Copyright (C) 2024-2025 M. Enes Kaya

;; Author: M. Enes Kaya
;; E-mail: enoks@tutanota.com
;; URL: https://github.com/itix-enoks/masked-theme
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <https://www.gnu.org/licenses/>.

(deftheme masked ()
          "A Masked - low-contrast - color theme for GNU Emacs.")

;; colors with `+c' are lighter; and with `-c' darker
(let ((masked-fg          "#fafadf")

      (masked-bg+2        "#208448")
      (masked-bg+1        "#227260")
      (masked-bg          "#215854")
      (masked-bg-1        "#1e4c4b")

      (masked-warning     "#441144")

      (masked-black       "#000000")
      (masked-white       "#ffffff")

      (masked-red         "#ffd9d9")
      (masked-orange      "#ffeed9")
      (masked-yellow      "#fff9d9")
      (masked-green       "#dbffd9")
      (masked-blue        "#d9e9ff")
      (masked-magenta     "#f4d9ff")
      (masked-cyan        "#d9ffff")

      ;; to disable bold/italic change these to 'normal'
      (bold               'bold)
      (italic             'italic))

  (custom-theme-set-faces
   'masked

   ;; ansi-term / vterm
   `(term-color-black ((t (:foreground ,masked-black :background ,masked-black))))
   `(term-color-red ((t (:foreground ,masked-red :background ,masked-red))))
   `(term-color-green ((t (:foreground ,masked-green :background ,masked-green))))
   `(term-color-blue ((t (:foreground ,masked-blue :background ,masked-blue))))
   `(term-color-yellow ((t (:foreground ,masked-yellow :background ,masked-yellow))))
   `(term-color-magenta ((t (:foreground ,masked-magenta :background ,masked-magenta))))
   `(term-color-cyan ((t (:foreground ,masked-cyan :background ,masked-cyan))))
   `(term-color-white ((t (:foreground ,masked-fg :background ,masked-fg))))

   ;; company
   `(company-preview ((t (:foreground ,masked-bg+1 :background ,masked-bg-1))))

   ;; compilation
   `(compilation-info ((t (:foreground ,masked-green))))
   `(compilation-warning ((t (:foreground ,masked-yellow))))
   `(compilation-error ((t (:foreground ,masked-red))))
   `(compilation-mode-line-fail ((t (:foreground ,masked-red :weight ,bold))))
   `(compilation-mode-line-exit ((t (:foreground ,masked-green :weight ,bold))))

   ;; diff
   `(diff-refine-added ((t (:background ,masked-green :weight ,bold))))
   `(diff-refine-removed ((t (:background ,masked-red :weight ,bold))))
   `(diff-refine-changed ((t (:foreground ,masked-bg+1 :background ,masked-green :weight ,bold))))

   ;; dired
   `(dired-directory ((t (:foreground ,masked-green :weight ,bold))))
   `(dired-ignored ((t (:foreground ,masked-cyan))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,masked-green :weight ,bold))))
   `(font-lock-comment-face ((t (:foreground ,masked-bg+2 :slant ,italic :weight ,bold))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((t (:foreground ,masked-green))))
   `(font-lock-doc-face ((t (:foreground ,masked-blue :slant ,italic))))
   `(font-lock-function-name-face ((t (:foreground ,masked-fg))))
   `(font-lock-keyword-face ((t (:foreground ,masked-yellow :weight ,bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,masked-magenta))))
   `(font-lock-string-face ((t (:foreground ,masked-red))))
   `(font-lock-type-face ((t (:foreground ,masked-cyan))))
   `(font-lock-variable-name-face ((t (:foreground ,masked-fg))))
   `(font-lock-warning-face ((t (:foreground ,masked-red))))
   `(font-lock-negation-char-face ((t (:foreground ,masked-red))))

   ;; general
   `(cursor ((t (:background ,masked-white))))
   `(default ((t (:foreground ,masked-fg :background ,masked-bg))))
   `(fringe ((t (:foreground ,masked-fg :background ,masked-bg))))
   `(minibuffer-prompt ((t (:foreground ,masked-yellow :weight ,bold))))
   `(region ((t (:background ,masked-bg-1))))
   `(link ((t (:foreground ,masked-yellow :underline t))))
   `(link-visited ((t (:foreground ,masked-yellow :underline t))))
   `(show-paren-match ((t (:foreground ,masked-white :background ,masked-bg+2 :bold t))))
   `(show-paren-mismatch ((t (:background ,masked-warning :bold t))))

   ;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,masked-cyan))))

   ;; isearch
   `(isearch ((t (:foreground ,masked-white :background ,masked-red))))

   ;; line-numbers
   `(line-number ((t (:inherit default :foreground ,masked-bg+1 :background ,masked-bg-1))))
   `(line-number-current-line ((t (:inherit line-number :weight ,bold))))

   ;; mode-line
   `(mode-line ((t (:foreground ,masked-green :background ,masked-bg-1 :box (:line-width (1 . 1) :color ,masked-bg+1)))))
   `(mode-line-inactive ((t (:foreground ,masked-bg+2 :background ,masked-bg-1 :box (:line-width (1 . 1) :color ,masked-bg+1)))))
   `(mode-line-buffer-id ((t (:weight normal))))

   ;; multiple cursors
   `(mc/cursor-face ((t (:inherit cursor))))

   ;; org
   `(org-date ((t (:foreground ,masked-blue :background ,masked-bg))))
   `(org-hide ((t (:foreground ,masked-bg+1 :background ,masked-bg))))
   `(org-todo ((t (:foreground ,masked-red :background ,masked-bg))))
   `(org-done ((t (:foreground ,masked-green :background ,masked-bg))))
   `(org-level-1 ((t (:foreground ,masked-red :background ,masked-bg))))
   `(org-level-2 ((t (:foreground ,masked-magenta :background ,masked-bg))))
   `(org-level-3 ((t (:foreground ,masked-blue :background ,masked-bg))))
   `(org-level-4 ((t (:foreground ,masked-cyan :background ,masked-bg))))
   `(org-level-5 ((t (:foreground ,masked-green :background ,masked-bg))))
   `(org-level-6 ((t (:foreground ,masked-yellow :background ,masked-bg))))
   `(org-level-7 ((t (:foreground ,masked-white :background ,masked-bg))))
   `(org-headline-done ((t (:inherit org-done))))

   ;; powerline
   `(powerline-active0 ((t (:foreground ,masked-white :background ,masked-bg+1))))
   `(powerline-active1 ((t (:foreground ,masked-white :background ,masked-bg-1))))
   `(powerline-active2 ((t (:foreground ,masked-white :background ,masked-bg-1))))
   `(powerline-inactive0 ((t (:foreground ,masked-bg+1 :background ,masked-bg-1))))
   `(powerline-inactive1 ((t (:foreground ,masked-bg+1 :background ,masked-bg-1))))
   `(powerline-inactive2 ((t (:foreground ,masked-bg+1 :background ,masked-bg-1))))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,masked-orange))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,masked-yellow))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,masked-green))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,masked-blue))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,masked-magenta))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,masked-red))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,masked-orange))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,masked-yellow))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,masked-green))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,masked-blue))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,masked-magenta))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,masked-red))))

   ;; tab-bar
   `(tab-bar ((t (:inherit mode-line))))
   `(tab-bar-tab ((t (:inherit mode-line :foreground ,masked-cyan  :background ,masked-bg+1))))
   `(tab-bar-tab-inactive ((t (:inherit mode-line-inactive))))

   ;; whitespace
   `(whitespace-space ((t (:foreground ,masked-bg+2 :background ,masked-bg ))))
   `(whitespace-tab ((t (:foreground ,masked-bg+2 :background ,masked-bg ))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'masked)

;;; masked-theme.el ends here.
