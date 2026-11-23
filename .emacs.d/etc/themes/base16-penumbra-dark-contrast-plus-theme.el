;; base16-penumbra-dark-contrast-plus-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Neal McKee (https://github.com/nealmckee/penumbra)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-penumbra-dark-contrast-plus-theme-colors
  '(:base00 "#1d2024"
    :base01 "#292b30"
    :base02 "#404246"
    :base03 "#5b5d62"
    :base04 "#7f7f81"
    :base05 "#999999"
    :base06 "#bab8b4"
    :base07 "#efe8dd"
    :base08 "#cd6b76"
    :base09 "#c58145"
    :base0A "#a39720"
    :base0B "#58a061"
    :base0C "#009894"
    :base0D "#008fc6"
    :base0E "#7d82c0"
    :base0F "#ac72b2")
  "All colors for Base16 Penumbra Dark Contrast Plus are defined here.")

;; Define the theme
(deftheme base16-penumbra-dark-contrast-plus)

;; Add all the faces to the theme
(base16-theme-define 'base16-penumbra-dark-contrast-plus base16-penumbra-dark-contrast-plus-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-penumbra-dark-contrast-plus)

(provide 'base16-penumbra-dark-contrast-plus-theme)

;;; base16-penumbra-dark-contrast-plus-theme.el ends here
