;; base16-github-dark-colorblind-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Tinted Theming (https://github.com/tinted-theming)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-github-dark-colorblind-theme-colors
  '(:base00 "#0d1117"
    :base01 "#151b23"
    :base02 "#2f3742"
    :base03 "#656c76"
    :base04 "#9198a1"
    :base05 "#d1d7e0"
    :base06 "#f0f6fc"
    :base07 "#ffffff"
    :base08 "#f0883e"
    :base09 "#ffa657"
    :base0A "#d29922"
    :base0B "#58a6ff"
    :base0C "#39c5cf"
    :base0D "#58a6ff"
    :base0E "#be8fff"
    :base0F "#ffa657")
  "All colors for Base16 Github Dark Colorblind are defined here.")

;; Define the theme
(deftheme base16-github-dark-colorblind)

;; Add all the faces to the theme
(base16-theme-define 'base16-github-dark-colorblind base16-github-dark-colorblind-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-github-dark-colorblind)

(provide 'base16-github-dark-colorblind-theme)

;;; base16-github-dark-colorblind-theme.el ends here
