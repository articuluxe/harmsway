;; base16-sequoia-monochrome-dark-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Micheal Andreuzza (https://michaelandreuzza.com/)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-sequoia-monochrome-dark-theme-colors
  '(:base00 "#0f1014"
    :base01 "#111216"
    :base02 "#111216"
    :base03 "#43444d"
    :base04 "#575861"
    :base05 "#868690"
    :base06 "#868690"
    :base07 "#0f1014"
    :base08 "#999eb2"
    :base09 "#999eb2"
    :base0A "#d3d5de"
    :base0B "#626983"
    :base0C "#b6bac8"
    :base0D "#7c829d"
    :base0E "#d3d5de"
    :base0F "#626983")
  "All colors for Base16 Sequoia Monochrome Dark are defined here.")

;; Define the theme
(deftheme base16-sequoia-monochrome-dark)

;; Add all the faces to the theme
(base16-theme-define 'base16-sequoia-monochrome-dark base16-sequoia-monochrome-dark-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-sequoia-monochrome-dark)

(provide 'base16-sequoia-monochrome-dark-theme)

;;; base16-sequoia-monochrome-dark-theme.el ends here
