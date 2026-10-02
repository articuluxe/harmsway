;; base16-tokio-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/tinted-theming/home)

;;; Authors:
;; Scheme: xscriptor (https://github.com/xscriptor)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-tokio-theme-colors
  '(:base00 "#1c1c1d"
    :base01 "#2b2b2d"
    :base02 "#363638"
    :base03 "#78757c"
    :base04 "#949199"
    :base05 "#f7f1ff"
    :base06 "#f9f4ff"
    :base07 "#f7f1ff"
    :base08 "#fc618d"
    :base09 "#fca37a"
    :base0A "#fce566"
    :base0B "#7bd88f"
    :base0C "#5ad4e6"
    :base0D "#fd9353"
    :base0E "#948ae3"
    :base0F "#ae7459")
  "All colors for Base16 Tokio are defined here.")

;; Define the theme
(deftheme base16-tokio)

;; Add all the faces to the theme
(base16-theme-define 'base16-tokio base16-tokio-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-tokio)

(provide 'base16-tokio-theme)

;;; base16-tokio-theme.el ends here
