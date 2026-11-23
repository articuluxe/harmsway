;; base16-penumbra-dark-contrast-plus-plus-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Neal McKee (https://github.com/nealmckee/penumbra)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-penumbra-dark-contrast-plus-plus-theme-colors
  '(:base00 "#101317"
    :base01 "#1b1e23"
    :base02 "#393c42"
    :base03 "#575b62"
    :base04 "#88898b"
    :base05 "#a9a9a9"
    :base06 "#c2c0bd"
    :base07 "#eee8dd"
    :base08 "#e17782"
    :base09 "#d88f4c"
    :base0A "#b4a724"
    :base0B "#62b06c"
    :base0C "#00a8a4"
    :base0D "#009edb"
    :base0E "#8a90d4"
    :base0F "#be7fc4")
  "All colors for Base16 Penumbra Dark Contrast Plus Plus are defined here.")

;; Define the theme
(deftheme base16-penumbra-dark-contrast-plus-plus)

;; Add all the faces to the theme
(base16-theme-define 'base16-penumbra-dark-contrast-plus-plus base16-penumbra-dark-contrast-plus-plus-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-penumbra-dark-contrast-plus-plus)

(provide 'base16-penumbra-dark-contrast-plus-plus-theme)

;;; base16-penumbra-dark-contrast-plus-plus-theme.el ends here
