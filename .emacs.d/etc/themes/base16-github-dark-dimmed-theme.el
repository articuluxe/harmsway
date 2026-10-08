;; base16-github-dark-dimmed-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Tinted Theming (https://github.com/tinted-theming)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-github-dark-dimmed-theme-colors
  '(:base00 "#0d1117"
    :base01 "#151b23"
    :base02 "#2f3742"
    :base03 "#656c76"
    :base04 "#9198a1"
    :base05 "#d1d7e0"
    :base06 "#f0f6fc"
    :base07 "#cdd9e5"
    :base08 "#f47067"
    :base09 "#f69d50"
    :base0A "#c69026"
    :base0B "#57ab5a"
    :base0C "#39c5cf"
    :base0D "#539bf5"
    :base0E "#b083f0"
    :base0F "#ff938a")
  "All colors for Base16 Github Dark Dimmed are defined here.")

;; Define the theme
(deftheme base16-github-dark-dimmed)

;; Add all the faces to the theme
(base16-theme-define 'base16-github-dark-dimmed base16-github-dark-dimmed-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-github-dark-dimmed)

(provide 'base16-github-dark-dimmed-theme)

;;; base16-github-dark-dimmed-theme.el ends here
