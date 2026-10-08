;; base16-serendipity-morning-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Micheal Andreuzza (https://michaelandreuzza.com/)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-serendipity-morning-theme-colors
  '(:base00 "#f6f7fb"
    :base01 "#eaebee"
    :base02 "#eaebee"
    :base03 "#6d7296"
    :base04 "#505575"
    :base05 "#3f4363"
    :base06 "#3f4363"
    :base07 "#f6f7fb"
    :base08 "#c25a4d"
    :base09 "#c25a4d"
    :base0A "#785fd0"
    :base0B "#2f7aab"
    :base0C "#e58678"
    :base0D "#6288d8"
    :base0E "#785fd0"
    :base0F "#2f7aab")
  "All colors for Base16 Serendipity Morning are defined here.")

;; Define the theme
(deftheme base16-serendipity-morning)

;; Add all the faces to the theme
(base16-theme-define 'base16-serendipity-morning base16-serendipity-morning-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-serendipity-morning)

(provide 'base16-serendipity-morning-theme)

;;; base16-serendipity-morning-theme.el ends here
