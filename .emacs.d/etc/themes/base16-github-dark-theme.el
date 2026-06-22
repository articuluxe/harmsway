;; base16-github-dark-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Tinted Theming (https://github.com/tinted-theming)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-github-dark-theme-colors
  '(:base00 "#0d1117"
    :base01 "#161b22"
    :base02 "#484f58"
    :base03 "#6e7681"
    :base04 "#8b949e"
    :base05 "#c9d1d9"
    :base06 "#f0f6fc"
    :base07 "#ffffff"
    :base08 "#ffa657"
    :base09 "#79c0ff"
    :base0A "#bb8009"
    :base0B "#a5d6ff"
    :base0C "#7ee787"
    :base0D "#d2a8ff"
    :base0E "#ff7b72"
    :base0F "#ffa198")
  "All colors for Base16 Github Dark are defined here.")

;; Define the theme
(deftheme base16-github-dark)

;; Add all the faces to the theme
(base16-theme-define 'base16-github-dark base16-github-dark-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-github-dark)

(provide 'base16-github-dark-theme)

;;; base16-github-dark-theme.el ends here
