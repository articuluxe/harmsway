;; base16-berlin-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-berlin-theme-colors
  '(:base00 "#000000"
    :base01 "#0e0e0e"
    :base02 "#181818"
    :base03 "#333333"
    :base04 "#707070"
    :base05 "#cccccc"
    :base06 "#d6d6d6"
    :base07 "#ffffff"
    :base08 "#999999"
    :base09 "#bbbbbb"
    :base0A "#dddddd"
    :base0B "#bbbbbb"
    :base0C "#cccccc"
    :base0D "#888888"
    :base0E "#aaaaaa"
    :base0F "#7a7a7a")
  "All colors for Base16 Berlin are defined here.")

;; Define the theme
(deftheme base16-berlin)

;; Add all the faces to the theme
(base16-theme-define 'base16-berlin base16-berlin-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-berlin)

(provide 'base16-berlin-theme)

;;; base16-berlin-theme.el ends here
