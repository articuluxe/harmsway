;; base16-oslo-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-oslo-theme-colors
  '(:base00 "#3f4451"
    :base01 "#474c59"
    :base02 "#4c515e"
    :base03 "#6c727f"
    :base04 "#7a808e"
    :base05 "#abb2bf"
    :base06 "#bcc1cc"
    :base07 "#ffffff"
    :base08 "#e05561"
    :base09 "#d8725a"
    :base0A "#d18f52"
    :base0B "#8cc265"
    :base0C "#42b3c2"
    :base0D "#4aa5f0"
    :base0E "#c162de"
    :base0F "#a26257")
  "All colors for Base16 Oslo are defined here.")

;; Define the theme
(deftheme base16-oslo)

;; Add all the faces to the theme
(base16-theme-define 'base16-oslo base16-oslo-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-oslo)

(provide 'base16-oslo-theme)

;;; base16-oslo-theme.el ends here
