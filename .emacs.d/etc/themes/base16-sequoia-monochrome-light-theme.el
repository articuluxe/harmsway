;; base16-sequoia-monochrome-light-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: Micheal Andreuzza (https://michaelandreuzza.com/)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-sequoia-monochrome-light-theme-colors
  '(:base00 "#edeef2"
    :base01 "#e2e3e8"
    :base02 "#e2e3e8"
    :base03 "#9da2ad"
    :base04 "#42434e"
    :base05 "#282930"
    :base06 "#282930"
    :base07 "#0f1014"
    :base08 "#525666"
    :base09 "#525666"
    :base0A "#50535e"
    :base0B "#2e3038"
    :base0C "#454752"
    :base0D "#5f6370"
    :base0E "#50535e"
    :base0F "#2e3038")
  "All colors for Base16 Sequoia Monochrome Light are defined here.")

;; Define the theme
(deftheme base16-sequoia-monochrome-light)

;; Add all the faces to the theme
(base16-theme-define 'base16-sequoia-monochrome-light base16-sequoia-monochrome-light-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-sequoia-monochrome-light)

(provide 'base16-sequoia-monochrome-light-theme)

;;; base16-sequoia-monochrome-light-theme.el ends here
