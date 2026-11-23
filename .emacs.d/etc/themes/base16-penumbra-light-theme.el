;; base16-penumbra-light-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Neal McKee (https://github.com/nealmckee/penumbra)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-penumbra-light-theme-colors
  '(:base00 "#efe8dd"
    :base01 "#dfd9d0"
    :base02 "#d1ccc5"
    :base03 "#b4b1ad"
    :base04 "#a19f9d"
    :base05 "#8b8b8b"
    :base06 "#5e6063"
    :base07 "#2c2e31"
    :base08 "#bb626b"
    :base09 "#b4763e"
    :base0A "#958a1c"
    :base0B "#509259"
    :base0C "#008a87"
    :base0D "#0082b5"
    :base0E "#7177b0"
    :base0F "#9d68a2")
  "All colors for Base16 Penumbra Light are defined here.")

;; Define the theme
(deftheme base16-penumbra-light)

;; Add all the faces to the theme
(base16-theme-define 'base16-penumbra-light base16-penumbra-light-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-penumbra-light)

(provide 'base16-penumbra-light-theme)

;;; base16-penumbra-light-theme.el ends here
