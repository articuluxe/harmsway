;; base16-sequoia-moonlight-dark-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Micheal Andreuzza (https://michaelandreuzza.com/)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-sequoia-moonlight-dark-theme-colors
  '(:base00 "#0f1014"
    :base01 "#111216"
    :base02 "#111216"
    :base03 "#43444d"
    :base04 "#575861"
    :base05 "#868690"
    :base06 "#868690"
    :base07 "#0f1014"
    :base08 "#f58ee0"
    :base09 "#f58ee0"
    :base0A "#9898a6"
    :base0B "#8eb6f5"
    :base0C "#ffbb88"
    :base0D "#c58fff"
    :base0E "#9898a6"
    :base0F "#8eb6f5")
  "All colors for Base16 Sequoia Moonlight Dark are defined here.")

;; Define the theme
(deftheme base16-sequoia-moonlight-dark)

;; Add all the faces to the theme
(base16-theme-define 'base16-sequoia-moonlight-dark base16-sequoia-moonlight-dark-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-sequoia-moonlight-dark)

(provide 'base16-sequoia-moonlight-dark-theme)

;;; base16-sequoia-moonlight-dark-theme.el ends here
