;; base16-penumbra-dark-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Neal McKee (https://github.com/nealmckee/penumbra)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-penumbra-dark-theme-colors
  '(:base00 "#2c2e31"
    :base01 "#383a3d"
    :base02 "#484a4d"
    :base03 "#5e6063"
    :base04 "#777879"
    :base05 "#8b8b8b"
    :base06 "#b4b1ad"
    :base07 "#efe8dd"
    :base08 "#ba616a"
    :base09 "#b3763e"
    :base0A "#948a1d"
    :base0B "#4f9158"
    :base0C "#008986"
    :base0D "#0081b4"
    :base0E "#7176ae"
    :base0F "#9c67a1")
  "All colors for Base16 Penumbra Dark are defined here.")

;; Define the theme
(deftheme base16-penumbra-dark)

;; Add all the faces to the theme
(base16-theme-define 'base16-penumbra-dark base16-penumbra-dark-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-penumbra-dark)

(provide 'base16-penumbra-dark-theme)

;;; base16-penumbra-dark-theme.el ends here
