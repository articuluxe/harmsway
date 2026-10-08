;; base16-sequoia-moonlight-light-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Micheal Andreuzza (https://michaelandreuzza.com/)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-sequoia-moonlight-light-theme-colors
  '(:base00 "#edeef2"
    :base01 "#e2e3e8"
    :base02 "#e2e3e8"
    :base03 "#565760"
    :base04 "#42434e"
    :base05 "#282930"
    :base06 "#282930"
    :base07 "#0f1014"
    :base08 "#c94da8"
    :base09 "#c94da8"
    :base0A "#6a6a78"
    :base0B "#4a85d4"
    :base0C "#d9884a"
    :base0D "#9a5fd9"
    :base0E "#6a6a78"
    :base0F "#4a85d4")
  "All colors for Base16 Sequoia Moonlight Light are defined here.")

;; Define the theme
(deftheme base16-sequoia-moonlight-light)

;; Add all the faces to the theme
(base16-theme-define 'base16-sequoia-moonlight-light base16-sequoia-moonlight-light-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-sequoia-moonlight-light)

(provide 'base16-sequoia-moonlight-light-theme)

;;; base16-sequoia-moonlight-light-theme.el ends here
