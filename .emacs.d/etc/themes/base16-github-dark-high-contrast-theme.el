;; base16-github-dark-high-contrast-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Tinted Theming (https://github.com/tinted-theming)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-github-dark-high-contrast-theme-colors
  '(:base00 "#0d1117"
    :base01 "#151b23"
    :base02 "#2f3742"
    :base03 "#656c76"
    :base04 "#9198a1"
    :base05 "#d1d7e0"
    :base06 "#f0f6fc"
    :base07 "#ffffff"
    :base08 "#ff9492"
    :base09 "#ffb757"
    :base0A "#f0b72f"
    :base0B "#28d751"
    :base0C "#39c5cf"
    :base0D "#71b7ff"
    :base0E "#cb9eff"
    :base0F "#ffb1af")
  "All colors for Base16 Github Dark High Contrast are defined here.")

;; Define the theme
(deftheme base16-github-dark-high-contrast)

;; Add all the faces to the theme
(base16-theme-define 'base16-github-dark-high-contrast base16-github-dark-high-contrast-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-github-dark-high-contrast)

(provide 'base16-github-dark-high-contrast-theme)

;;; base16-github-dark-high-contrast-theme.el ends here
