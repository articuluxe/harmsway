;; base16-bogota-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-bogota-theme-colors
  '(:base00 "#200b0a"
    :base01 "#2f1b1b"
    :base02 "#3a2727"
    :base03 "#525053"
    :base04 "#968a91"
    :base05 "#f7f1ff"
    :base06 "#f9f4ff"
    :base07 "#f7f1ff"
    :base08 "#fc618d"
    :base09 "#fea78b"
    :base0A "#ffed89"
    :base0B "#7bd88f"
    :base0C "#47e6ff"
    :base0D "#47e6ff"
    :base0E "#ff9999"
    :base0F "#b0705e")
  "All colors for Base16 Bogota are defined here.")

;; Define the theme
(deftheme base16-bogota)

;; Add all the faces to the theme
(base16-theme-define 'base16-bogota base16-bogota-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-bogota)

(provide 'base16-bogota-theme)

;;; base16-bogota-theme.el ends here
