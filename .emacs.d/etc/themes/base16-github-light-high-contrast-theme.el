;; base16-github-light-high-contrast-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Tinted Theming (https://github.com/tinted-theming)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-github-light-high-contrast-theme-colors
  '(:base00 "#ffffff"
    :base01 "#f6f8fa"
    :base02 "#d1d9e0"
    :base03 "#818b98"
    :base04 "#59636e"
    :base05 "#454c54"
    :base06 "#25292e"
    :base07 "#010409"
    :base08 "#a0111f"
    :base09 "#702c00"
    :base0A "#603700"
    :base0B "#024c1a"
    :base0C "#1b7c83"
    :base0D "#0349b4"
    :base0E "#622cbc"
    :base0F "#86061d")
  "All colors for Base16 Github Light High Contrast are defined here.")

;; Define the theme
(deftheme base16-github-light-high-contrast)

;; Add all the faces to the theme
(base16-theme-define 'base16-github-light-high-contrast base16-github-light-high-contrast-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-github-light-high-contrast)

(provide 'base16-github-light-high-contrast-theme)

;;; base16-github-light-high-contrast-theme.el ends here
