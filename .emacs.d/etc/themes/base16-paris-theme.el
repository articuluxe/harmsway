;; base16-paris-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-paris-theme-colors
  '(:base00 "#1a0a30"
    :base01 "#291a3e"
    :base02 "#352649"
    :base03 "#c4bdff"
    :base04 "#9489a2"
    :base05 "#f7f1ff"
    :base06 "#f9f4ff"
    :base07 "#f7f1ff"
    :base08 "#fc618d"
    :base09 "#fca37a"
    :base0A "#fce566"
    :base0B "#7bd88f"
    :base0C "#a3f3ff"
    :base0D "#a3f3ff"
    :base0E "#c4bdff"
    :base0F "#ad6d60")
  "All colors for Base16 Paris are defined here.")

;; Define the theme
(deftheme base16-paris)

;; Add all the faces to the theme
(base16-theme-define 'base16-paris base16-paris-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-paris)

(provide 'base16-paris-theme)

;;; base16-paris-theme.el ends here
