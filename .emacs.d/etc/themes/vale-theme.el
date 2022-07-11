;;; vale-theme.el --- bringing space to your emacs -*- lexical-binding: t -*-

;; Author: Ash <ext0l@riseup.net>
;; URL: https://git.catgirl.ai/ext0l/vale
;; Version: 0.1
;; Keywords: color, theme

;; This file is not part of Emacs.

;;; Commentary:

;; Purples, reds, blues. Designed to be aesthetic, but not
;; super-saturated; this is a theme you should be able to look at for
;; 8 hours a day.

;;; Code:

(require 'color) ; HSL is nicer for specifying colors

(deftheme vale "A nebula-inspired theme with purples, reds, and blues.")

(defun vale-theme--hsl-to-hex (h s l)
  "Converts the given HSL triple to a hex color code."
  ; specify 2 here to use 2 hex digits per color
  (apply (lambda (r g b) (color-rgb-to-hex r g b 2))
	 (color-hsl-to-rgb (/ h 360.0) s l)))

; Colors are deifned in HSL because it makes them easier to
; tweak. Theoretically, a color space that uses human light perception
; would work better (`light-blue' has a lower lightness than `blue'?),
; but color.el gives us HSL.
(let ((fg (vale-theme--hsl-to-hex 231 0.50 0.96))
      (dark-fg (vale-theme--hsl-to-hex 233 0.10 0.50))
      (light-bg (vale-theme--hsl-to-hex 0 0.00 0.20))
      (highlight-bg (vale-theme--hsl-to-hex 338 0.10 0.20))
      (bg (vale-theme--hsl-to-hex 240 0.10 0.10))
      (light-green (vale-theme--hsl-to-hex 120 0.96 0.90))
      (green (vale-theme--hsl-to-hex 85 0.50 0.65))
      (light-orange (vale-theme--hsl-to-hex 38 0.98 0.67))
      (light-blue (vale-theme--hsl-to-hex 192 0.95 0.54))
      (blue (vale-theme--hsl-to-hex 232 0.70 0.72))
      (light-purple (vale-theme--hsl-to-hex 263 1.00 0.80))
      (purple (vale-theme--hsl-to-hex 263 1.00 0.70))
      (pink (vale-theme--hsl-to-hex 338 1.00 0.70)))

  (custom-theme-set-faces
   'vale
   `(default ((t (:background ,bg :foreground ,fg))))
   `(font-lock-builtin-face ((t (:foreground ,light-blue))))
   `(font-lock-comment-face ((t (:foreground ,dark-fg))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,dark-fg))))
   `(font-lock-constant-face ((t (:foreground ,light-purple))))
   `(font-lock-doc-face ((t (:foreground ,light-orange))))
   `(font-lock-function-name-face ((t (:foreground ,pink))))

   `(font-lock-keyword-face ((t (:foreground ,blue))))
   `(font-lock-preprocessor-face ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground ,light-purple))))
   `(font-lock-type-face ((t (:foreground ,light-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,pink))))

   `(company-tooltip ((t (:background ,light-bg :foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,fg :foreground ,light-bg))))
   `(company-tooltip-annotation ((t (:background ,light-bg :foreground ,light-blue))))
   `(company-tooltip-common ((t (:inherit company-tooltip :foreground ,light-orange))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection))))

   `(region ((t (:background ,light-bg))))

   `(mode-line ((t (:background "#444444" :foreground ,fg))))
   `(highlight ((t (:background ,highlight-bg))))
   ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'vale)
(provide 'vale-theme)
