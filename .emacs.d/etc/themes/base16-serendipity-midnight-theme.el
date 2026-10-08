;; base16-serendipity-midnight-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Micheal Andreuzza (https://michaelandreuzza.com/)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-serendipity-midnight-theme-colors
  '(:base00 "#151726"
    :base01 "#1c1e2d"
    :base02 "#1c1e2d"
    :base03 "#6b6d7c"
    :base04 "#8d8f9e"
    :base05 "#dee0ef"
    :base06 "#dee0ef"
    :base07 "#151726"
    :base08 "#ee8679"
    :base09 "#ee8679"
    :base0A "#a78bfa"
    :base0B "#5ba2d0"
    :base0C "#f8d2c9"
    :base0D "#94b8ff"
    :base0E "#a78bfa"
    :base0F "#5ba2d0")
  "All colors for Base16 Serendipity Midnight are defined here.")

;; Define the theme
(deftheme base16-serendipity-midnight)

;; Add all the faces to the theme
(base16-theme-define 'base16-serendipity-midnight base16-serendipity-midnight-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-serendipity-midnight)

(provide 'base16-serendipity-midnight-theme)

;;; base16-serendipity-midnight-theme.el ends here
