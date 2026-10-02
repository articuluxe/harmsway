;; base16-madrid-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-madrid-theme-colors
  '(:base00 "#fafafa"
    :base01 "#eaeaea"
    :base02 "#dfdfdf"
    :base03 "#4d4d4d"
    :base04 "#686868"
    :base05 "#1a1a1a"
    :base06 "#121212"
    :base07 "#1a1a1a"
    :base08 "#990026"
    :base09 "#923217"
    :base0A "#8a6408"
    :base0B "#007a28"
    :base0C "#007a9e"
    :base0D "#007a9e"
    :base0E "#4d2699"
    :base0F "#662310")
  "All colors for Base16 Madrid are defined here.")

;; Define the theme
(deftheme base16-madrid)

;; Add all the faces to the theme
(base16-theme-define 'base16-madrid base16-madrid-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-madrid)

(provide 'base16-madrid-theme)

;;; base16-madrid-theme.el ends here
