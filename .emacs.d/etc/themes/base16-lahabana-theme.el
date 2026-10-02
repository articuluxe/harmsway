;; base16-lahabana-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-lahabana-theme-colors
  '(:base00 "#19191a"
    :base01 "#29282a"
    :base02 "#343335"
    :base03 "#76747a"
    :base04 "#939098"
    :base05 "#f7f1ff"
    :base06 "#f9f4ff"
    :base07 "#f7f1ff"
    :base08 "#fc618d"
    :base09 "#f0b095"
    :base0A "#e5ff9d"
    :base0B "#7bd88f"
    :base0C "#5ad4e6"
    :base0D "#fd9353"
    :base0E "#948ae3"
    :base0F "#a57b6a")
  "All colors for Base16 Lahabana are defined here.")

;; Define the theme
(deftheme base16-lahabana)

;; Add all the faces to the theme
(base16-theme-define 'base16-lahabana base16-lahabana-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-lahabana)

(provide 'base16-lahabana-theme)

;;; base16-lahabana-theme.el ends here
