;;; modukai-sun-theme.el --- Monokai Pro Light Sun based on modus-themes -*- lexical-binding:t -*-

;; Author: Tobias Mock <tobiasjammer@googlemail.com>
;; Maintainer: Tobias Mock <tobiasjammer@googlemail.com>
;; URL: https://codeberg.org/tjammer/modukai-sun-theme
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (modus-themes "5.2.0"))
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This theme doesn't try to be a complete port of Monokai Pro Sun theme to
;; Emacs, its goal is to capture the general vibe instead.

;;; Code:

(require 'modus-themes)

(defvar modukai-sun-theme-palette
  '((cursor "#72696d")
    (bg-main "#f8efe7")
    (bg-dim "#ded5d0")
    (fg-main "#2c232e")
    (fg-dim "#a59c9c")
    (bg-hl-line "#eee5de")
    (bg-inactive "#eee5de")
    (bg-active "#d2c9c4")
    (bg-region "#d2c9c4")
    (bg-diff-context "#eee5de")
    (red "#ce4770")
    (red-cooler "#ce4770")
    (green "#218871")
    (yellow "#b16803")
    (yellow-cooler "#d4572b")
    (yellow-warmer "#d4572b")
    (blue "#2473b6")
    (magenta "#6851a2")
    (magenta-cooler "#6851a2")
    (cyan "#2473b6")
    (cyan-cooler "#2473b6")
    (string yellow)
    (docstring yellow-cooler)
    (constant magenta)
    (keyword red)
    (fnname green)
    (fnname-call green))
  "Like `modus-operandi-palette'.")

(defvar modukai-sun-theme-custom-faces
  '(
    `(tuareg-font-lock-constructor-face ((,c :foreground ,magenta)))
    `(schmu-font-lock-module-face ((,c :foreground ,blue)))
    `(tuareg-font-lock-module-face ((,c :foreground ,blue)))
    `(tuareg-font-lock-governing-face ((,c :foreground ,red))))
  "Custom faces that deviate from---or complement---those in the Modus themes.")

(modus-themes-theme
 'modukai-sun
 'modukai-themes
 "Monokai Pro Light Sun based on modus-themes"
 'light
 'modus-operandi-palette
 'modukai-sun-theme-palette
 nil
 'modukai-sun-theme-custom-faces)

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (add-to-list 'custom-theme-load-path dir)))

(provide 'modukai-sun-theme)
;;; modukai-sun-theme.el ends here
