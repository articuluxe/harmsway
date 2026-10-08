;; base16-sequoia-retro-dark-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Micheal Andreuzza (https://michaelandreuzza.com/)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-sequoia-retro-dark-theme-colors
  '(:base00 "#0f1014"
    :base01 "#111216"
    :base02 "#111216"
    :base03 "#43444d"
    :base04 "#575861"
    :base05 "#868690"
    :base06 "#868690"
    :base07 "#0f1014"
    :base08 "#829fa7"
    :base09 "#829fa7"
    :base0A "#da674b"
    :base0B "#648f68"
    :base0C "#a27e57"
    :base0D "#5c87a4"
    :base0E "#da674b"
    :base0F "#648f68")
  "All colors for Base16 Sequoia Retro Dark are defined here.")

;; Define the theme
(deftheme base16-sequoia-retro-dark)

;; Add all the faces to the theme
(base16-theme-define 'base16-sequoia-retro-dark base16-sequoia-retro-dark-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-sequoia-retro-dark)

(provide 'base16-sequoia-retro-dark-theme)

;;; base16-sequoia-retro-dark-theme.el ends here
