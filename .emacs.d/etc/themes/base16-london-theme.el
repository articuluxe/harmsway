;; base16-london-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-london-theme-colors
  '(:base00 "#ffffff"
    :base01 "#f1f1f1"
    :base02 "#e7e7e7"
    :base03 "#333333"
    :base04 "#7a7a7a"
    :base05 "#333333"
    :base06 "#242424"
    :base07 "#aaaaaa"
    :base08 "#333333"
    :base09 "#444444"
    :base0A "#555555"
    :base0B "#444444"
    :base0C "#888888"
    :base0D "#666666"
    :base0E "#777777"
    :base0F "#303030")
  "All colors for Base16 London are defined here.")

;; Define the theme
(deftheme base16-london)

;; Add all the faces to the theme
(base16-theme-define 'base16-london base16-london-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-london)

(provide 'base16-london-theme)

;;; base16-london-theme.el ends here
