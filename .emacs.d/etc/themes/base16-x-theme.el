;; base16-x-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-x-theme-colors
  '(:base00 "#050505"
    :base01 "#161616"
    :base02 "#222123"
    :base03 "#6b686e"
    :base04 "#8a878e"
    :base05 "#f7f1ff"
    :base06 "#f9f4ff"
    :base07 "#f7f1ff"
    :base08 "#fc618d"
    :base09 "#fca37a"
    :base0A "#fce566"
    :base0B "#7bd88f"
    :base0C "#5ad4e6"
    :base0D "#fd9353"
    :base0E "#948ae3"
    :base0F "#a66c51")
  "All colors for Base16 X are defined here.")

;; Define the theme
(deftheme base16-x)

;; Add all the faces to the theme
(base16-theme-define 'base16-x base16-x-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-x)

(provide 'base16-x-theme)

;;; base16-x-theme.el ends here
