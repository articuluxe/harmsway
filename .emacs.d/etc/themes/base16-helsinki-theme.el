;; base16-helsinki-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-helsinki-theme-colors
  '(:base00 "#f8fafe"
    :base01 "#edeef1"
    :base02 "#e4e5e7"
    :base03 "#b0a999"
    :base04 "#8d8a82"
    :base05 "#544d40"
    :base06 "#3b362d"
    :base07 "#000000"
    :base08 "#1faa9e"
    :base09 "#268da6"
    :base0A "#2e70ad"
    :base0B "#733d9a"
    :base0C "#bd4c3d"
    :base0D "#b55a0f"
    :base0E "#3e9d21"
    :base0F "#1b6374")
  "All colors for Base16 Helsinki are defined here.")

;; Define the theme
(deftheme base16-helsinki)

;; Add all the faces to the theme
(base16-theme-define 'base16-helsinki base16-helsinki-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-helsinki)

(provide 'base16-helsinki-theme)

;;; base16-helsinki-theme.el ends here
