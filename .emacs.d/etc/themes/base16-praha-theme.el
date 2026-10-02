;; base16-praha-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-praha-theme-colors
  '(:base00 "#1a1a1a"
    :base01 "#2a2a2a"
    :base02 "#353535"
    :base03 "#6272a4"
    :base04 "#989898"
    :base05 "#ffffff"
    :base06 "#ffffff"
    :base07 "#ffffff"
    :base08 "#ff5555"
    :base09 "#ff9c7c"
    :base0A "#ffe4a3"
    :base0B "#b8e6a0"
    :base0C "#8be9fd"
    :base0D "#bd93f9"
    :base0E "#ff9aa2"
    :base0F "#af6e5a")
  "All colors for Base16 Praha are defined here.")

;; Define the theme
(deftheme base16-praha)

;; Add all the faces to the theme
(base16-theme-define 'base16-praha base16-praha-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-praha)

(provide 'base16-praha-theme)

;;; base16-praha-theme.el ends here
