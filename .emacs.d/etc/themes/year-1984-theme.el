;;; year-1984-theme.el --- A retro-futuristic theme -*- lexical-binding: t; -*-

;; Author: Davide Mastromatteo <mastro35@gmail.com>
;; URL: https://github.com/mastro35/year-1984-theme
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: themes, faces, colors, Apple, beige, light, pastel, vintage
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; A retro-futuristic Emacs theme inspired by the original Apple beige color of the first
;; 1984 Macintosh.

;;; Code:


(deftheme year-1984
  "A theme based on the original Putty color from Apple.
Created on 2025-05-07 by Davide Mastromatteo.")

(custom-theme-set-faces
 'year-1984
 '(default ((t (:foundry "nil" :width normal :height 170 :weight regular :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "#2e2a26" :background "#c4c0aa" :stipple nil :inherit nil))))
 '(cursor ((t (:background "#7a7265"))))
 '(hl-line ((t (:background "#d9d6c6"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(escape-glyph ((t (:foreground "#0e0e0a"))))
 '(homoglyph ((t (:foreground "#0e0e0a"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "#0e0e0a"))))
 '(highlight ((t (:background "#a49f90" :foreground "#2e2a26" :weight bold))))
 '(region ((t (:extend t :foreground "#2e2a26" :background "#a49f90"))))
 '(shadow ((t (:weight thin :foreground "#0e0e0a"))))
 '(secondary-selection ((t (:background "#c8c3b4"))))
 '(trailing-whitespace ((t (:background "#E23838"))))
 '(success ((t (:foreground "#5EBD3E"))))
 '(warning ((t (:foreground "#F78200"))))
 '(error ((t (:foreground "#E23838"))))
 '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-builtin-face ((t (:foreground "#0e0e0a"))))
 '(font-lock-comment-delimiter-face ((t (:slant italic :inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#7d7165"))))
 '(font-lock-constant-face ((t (:foreground "#0e0e0a"))))
 '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-doc-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
 '(font-lock-function-call-face ((t (:inherit (font-lock-function-name-face)))))
 '(font-lock-function-name-face ((t (:underline (:color foreground-color :style line :position nil)))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "#0e0e0a"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-number-face ((t nil)))
 '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-operator-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-property-name-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-property-use-face ((t (:inherit (font-lock-property-name-face)))))
 '(font-lock-punctuation-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#0e0e0a"))))
 '(font-lock-type-face ((t (:foreground "#0e0e0a"))))
 '(font-lock-variable-name-face ((t (:foreground "#1a1a1a"))))
 '(font-lock-variable-use-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-warning-face ((t (:inherit (warning)))))
 '(button ((t (:inherit (link)))))
 '(line-number ((t (:foreground "#7d7165" :background "#c4c0aa"))))
 '(line-number-current-line ((t (:foreground "#0e0e0a" :weight bold))))
 '(help-key-binding ((t (:box (:line-width (1 . 1) :color "#0e0e0a" :style released-button)
                              :background "#e3e1d6"
                              :foreground "#0e0e0a"
                              :weight bold))))
 '(link ((t (:underline (:color foreground-color :style line :position nil)))))
 '(link-visited ((t (:foreground "#0e0e0a" :inherit (link)))))
 '(fringe ((t (:background "#7d7165"))))
 '(header-line ((t (:box nil :foreground "#f7f6f3" :background "#e3e1d6" :inherit (mode-line)))))
 '(tooltip ((t (:foreground "#1a1a1a" :background "#c4c0aa" :inherit (variable-pitch)))))
 '(mode-line ((t (:box (:line-width (1 . 1) :color "#7a7265" :style nil) :foreground "#f0ede3" :background "#7a7265"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width (2 . 2) :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:weight light :box (:line-width (1 . 1) :color "#0e0e0a" :style nil) :foreground "#0e0e0a" :background "#c4c0aa" :inherit (mode-line)))))
 '(isearch ((t (:weight bold :background "#b2a891" :foreground "#2e2a26"))))
 '(isearch-fail ((t (:background "#e23838"))))
 '(lazy-highlight ((t (:background "#d9d6c6" :foreground "#2e2a26"))))
 '(match ((t (:foreground "#2e2a26" :background "#a49f90" :weight bold))))
 '(orderless-match-face-0 ((t (:foreground "#2e2a26" :background "#a49f90"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(completions-common-part ((t (:foreground "#2e2a26" :background "#d9d6c6"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#5EBD3E"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#FFB900"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#F78200"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#E23838"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#973999"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#009CDF"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#5EBD3E"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#FFB900"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#F78200"))))
 '(variable-pitch ((t (:family "Serif" :height 180 :foreground "#2e2a26"))))
 '(org-level-1 ((t (:weight bold :height 1.3 :foreground "#0e0e0a"))))
 '(org-level-2 ((t (:weight bold :height 1.2 :foreground "#1a1a1a"))))
 '(org-level-3 ((t (:weight normal :height 1.1 :foreground "#2e2a26"))))
 '(org-block ((t (:background "#e3e1d6" :extend t))))
 '(org-code ((t (:inherit (fixed-pitch)))))
 '(org-verbatim ((t (:inherit (fixed-pitch)))))
 '(company-tooltip ((t (:background "#e3e1d6" :foreground "#2e2a26"))))
 '(company-tooltip-selection ((t (:background "#a49f90" :foreground "#2e2a26" :weight bold))))
 '(company-tooltip-common ((t (:foreground "#0e0e0a" :weight bold))))
 '(company-tooltip-annotation ((t (:foreground "#7d7165"))))
 '(company-scrollbar-bg ((t (:background "#d9d6c6"))))
 '(company-scrollbar-fg ((t (:background "#a49f90"))))
 '(company-preview ((t (:background "#c4c0aa" :foreground "#0e0e0a"))))
 '(company-preview-common ((t (:foreground "#0e0e0a" :weight bold))))
 '(show-paren-match ((t (:background "#b2a891" :foreground "#2e2a26" :weight bold)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'year-1984)

;;; year-1984-theme.el ends here
