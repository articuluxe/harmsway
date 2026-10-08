;; base16-github-dark-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Tinted Theming (https://github.com/tinted-theming)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-github-dark-theme-colors
  '(:base00 "#0d1117"
    :base01 "#151b23"
    :base02 "#2f3742"
    :base03 "#656c76"
    :base04 "#9198a1"
    :base05 "#d1d7e0"
    :base06 "#f0f6fc"
    :base07 "#ffffff"
    :base08 "#ff7b72"
    :base09 "#ffa657"
    :base0A "#d29922"
    :base0B "#3fb950"
    :base0C "#39c5cf"
    :base0D "#58a6ff"
    :base0E "#be8fff"
    :base0F "#ffa198")
  "All colors for Base16 Github Dark are defined here.")

;; Define the theme
(deftheme base16-github-dark)

;; Add all the faces to the theme
(base16-theme-define 'base16-github-dark base16-github-dark-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-github-dark)

(provide 'base16-github-dark-theme)

;;; base16-github-dark-theme.el ends here
