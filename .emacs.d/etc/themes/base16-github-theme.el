;; base16-github-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Tinted Theming (https://github.com/tinted-theming)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-github-theme-colors
  '(:base00 "#ffffff"
    :base01 "#f6f8fa"
    :base02 "#d1d9e0"
    :base03 "#818b98"
    :base04 "#59636e"
    :base05 "#454c54"
    :base06 "#25292e"
    :base07 "#1f2328"
    :base08 "#cf222e"
    :base09 "#953800"
    :base0A "#9a6700"
    :base0B "#116329"
    :base0C "#1b7c83"
    :base0D "#0969da"
    :base0E "#8250df"
    :base0F "#a40e26")
  "All colors for Base16 Github are defined here.")

;; Define the theme
(deftheme base16-github)

;; Add all the faces to the theme
(base16-theme-define 'base16-github base16-github-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-github)

(provide 'base16-github-theme)

;;; base16-github-theme.el ends here
