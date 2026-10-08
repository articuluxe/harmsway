;; base16-serendipity-sunset-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Micheal Andreuzza (https://michaelandreuzza.com/)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-serendipity-sunset-theme-colors
  '(:base00 "#202231"
    :base01 "#272938"
    :base02 "#272938"
    :base03 "#8d8f9e"
    :base04 "#6b6d7c"
    :base05 "#dee0ef"
    :base06 "#dee0ef"
    :base07 "#202231"
    :base08 "#d1918f"
    :base09 "#d1918f"
    :base0A "#a392dc"
    :base0B "#709bbd"
    :base0C "#d6b4b4"
    :base0D "#a0b6e8"
    :base0E "#a392dc"
    :base0F "#709bbd")
  "All colors for Base16 Serendipity Sunset are defined here.")

;; Define the theme
(deftheme base16-serendipity-sunset)

;; Add all the faces to the theme
(base16-theme-define 'base16-serendipity-sunset base16-serendipity-sunset-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-serendipity-sunset)

(provide 'base16-serendipity-sunset-theme)

;;; base16-serendipity-sunset-theme.el ends here
