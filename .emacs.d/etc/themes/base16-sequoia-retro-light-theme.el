;; base16-sequoia-retro-light-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Micheal Andreuzza (https://michaelandreuzza.com/)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-sequoia-retro-light-theme-colors
  '(:base00 "#edeef2"
    :base01 "#e2e3e8"
    :base02 "#e2e3e8"
    :base03 "#565760"
    :base04 "#42434e"
    :base05 "#282930"
    :base06 "#282930"
    :base07 "#0f1014"
    :base08 "#5f7982"
    :base09 "#5f7982"
    :base0A "#bf5238"
    :base0B "#4a704e"
    :base0C "#8a6539"
    :base0D "#456a82"
    :base0E "#bf5238"
    :base0F "#4a704e")
  "All colors for Base16 Sequoia Retro Light are defined here.")

;; Define the theme
(deftheme base16-sequoia-retro-light)

;; Add all the faces to the theme
(base16-theme-define 'base16-sequoia-retro-light base16-sequoia-retro-light-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-sequoia-retro-light)

(provide 'base16-sequoia-retro-light-theme)

;;; base16-sequoia-retro-light-theme.el ends here
