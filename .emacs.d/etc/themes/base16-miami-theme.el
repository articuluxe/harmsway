;; base16-miami-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-miami-theme-colors
  '(:base00 "#000000"
    :base01 "#111112"
    :base02 "#1e1d1f"
    :base03 "#69676c"
    :base04 "#88858c"
    :base05 "#f7f1ff"
    :base06 "#f9f4ff"
    :base07 "#f7f1ff"
    :base08 "#ff4c8b"
    :base09 "#ff926c"
    :base0A "#ffd84c"
    :base0B "#7fffd4"
    :base0C "#47cfff"
    :base0D "#00ffa8"
    :base0E "#d36cff"
    :base0F "#a65f46")
  "All colors for Base16 Miami are defined here.")

;; Define the theme
(deftheme base16-miami)

;; Add all the faces to the theme
(base16-theme-define 'base16-miami base16-miami-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-miami)

(provide 'base16-miami-theme)

;;; base16-miami-theme.el ends here
