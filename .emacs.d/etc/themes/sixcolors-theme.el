;;; sixcolors-theme.el --- Just another theme -*- lexical-binding: t;-*-

;; Author: Davide Mastromatteo <mastro35@gmail.com>
;; URL: https://github.com/mastro35/sixcolors-theme
;; Keywords: faces, colors
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-2.0-only

;;; Commentary:

;; Just another theme for Emacs, based on the
;; six colors of the original Apple logo

;;; Code:


(deftheme sixcolors
  "A theme based on the six colors of the old Apple logo.
Created by Davide Mastromatteo on 2024-07-08.")

(custom-theme-set-faces
 'sixcolors
 '(default ((t (:foreground "ivory" :background "gray10" :stipple nil :inherit nil))))
 '(cursor ((t (:foreground "#000000" :background "#f78200"))))
 '(escape-glyph ((t (:foreground "#009cdf"))))
 '(homoglyph ((t (:foreground "#009cdf"))))
 '(minibuffer-prompt ((t (:foreground "#f78200"))))
 '(highlight ((t (:background "#f78200" :weight bold))))
 '(region ((t (:background "grey20"  :extend t))))
 '(shadow ((t (:foreground "#FFB900" :weight thin))))
 '(secondary-selection ((t (:extend t :background "#F78200"))))
 '(trailing-whitespace ((t (:background "#E23838"))))
 '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-builtin-face ((t (:foreground "#5EBD3E"))))
 '(font-lock-comment-delimiter-face ((t (:slant italic :inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:slant italic :foreground "gray30"))))
 '(font-lock-constant-face ((t (:foreground "#009CDF"))))
 '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-doc-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-doc-markup-face ((t (:inherit (font-lock-constant-face)))))
 '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
 '(font-lock-function-call-face ((t (:inherit font-lock-function-name-face))))
 '(font-lock-function-name-face ((t (:foreground "#FFB900"))))
 '(font-lock-keyword-face ((t (:foreground "#973999"))))
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
 '(font-lock-string-face ((t (:foreground "#009cdf"))))
 '(font-lock-type-face ((((class grayscale) (background light)) (:weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:underline (:color foreground-color :style line :position nil) :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#5eBD3E"))))
 '(font-lock-variable-use-face ((t (:inherit (font-lock-variable-name-face)))))
 '(font-lock-warning-face ((t (:foreground "#F78200"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line :position nil)))))
 '(link-visited ((t (:inherit link :foreground "#973999"))))
 '(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey5")) (t (:background "gray"))))
 '(header-line ((t (:box nil :foreground "grey20" :background "grey90" :inherit (mode-line)))))
 '(tooltip ((t (:foreground "systeminfotext" :background "systeminfowindow" :inherit (variable-pitch)))))
 '(mode-line ((t (:background "#000000" :foreground "#5Ebd3e" :box (:line-width (1 . 1) :color "#009cdf")))))
 '(mode-line-buffer-id ((t (:foreground "#F78200" :weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width (2 . 2) :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:weight light :box (:line-width (1 . -1) :color "grey75" :style nil) :foreground "grey20" :background "grey90" :inherit (mode-line)))))
 '(isearch ((t (:background "#f78200" :weight bold))))
 '(isearch-fail ((t (:background "#e23838"))))
 '(lazy-highlight ((t (:background "#f78200"))))
 '(match ((t (:background "#f78200" :Weight bold))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(orderless-match-face-0 ((((class color) (min-colors 88) (background dark)) (:foreground "#5EBD3E" :weight bold))))
 '(show-paren-match ((t (:foreground "#000000" :background "#F78200" :weight bold))))
 '(company-tooltip-common ((t (:foreground "#000000" :background "#5EBD3E" :weight bold))))
 '(company-tooltip-common-selection ((t (:foreground "#000000" :background "#5ebd3e" :weight bold))))
 '(company-tooltip-quick-access ((t (:foreground "#000000" :background "#5EBD3E" :weight bold))))
 '(company-tooltip-quick-access-selection ((t (:foreground "#000000" :background "#5ebd3e" :weight bold))))
 '(company-tooltip-annotation ((t (:foreground "#000000" :background "#5EBD3E" :weight bold))))
 '(company-tooltip-annotation-selection ((t (:foreground "#000000" :background "#5ebd3e" :weight bold))))
 '(show-paren-mismatch ((t (:background "#e23838"))))
 '(completions-common-part ((t (:foreground "#5ebd3e" :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#5EBD3E"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#FFB900"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#F78200"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#E23838"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#973999"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#009CDF"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#5EBD3E"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#FFB900"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#F78200"))))
 '(hl-line ((t (:background "gray15")))))


;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))

;; Automatically add this theme to the load path
(provide-theme 'sixcolors)

;;; sixcolors-theme.el ends here
