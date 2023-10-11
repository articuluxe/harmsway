;;; pumpkin-spice-theme.el --- Spice up your day with a delightful pumpkin colored theme -*- lexical-binding:t -*-

;; Copyright (C) 2023 Grant Shangreaux

;; Author: Grant Shangreaux <shoshin@cicadas.surf>
;; Maintainer: Grant Shangreaux <shoshin@cicadas.surf>
;; URL: https://cicadas.surf/cgit/pumpkin-spice-theme.git
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (autothemer "0.2"))
;; Keywords: faces, theme, halloween, pumpkin

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 🎃 👻 BOO!

;;; Code:

(require 'autothemer)

(autothemer-deftheme
 pumpkin-spice "The most frighteningest spook in town!"
 ((((class color) (min-colors #xFFFFFF)))
  
  (pumpkin-spice-cream    "#faeadc")
  (pumpkin-spice-ginger   "#f1ec99")
  (pumpkin-spice-caramel  "#c5964d")
  (pumpkin-spice-latte    "#f1c099")
  (pumpkin-spice-pumpkin  "#d06c1d")
  (pumpkin-spice-cinnamon "#a55724")
  (pumpkin-spice-fluff    "#f199ca")
  (pumpkin-spice-cran     "#FF495F")
  (pumpkin-spice-nutmeg   "#650900")
  (pumpkin-spice-clove    "#513323")
  (pumpkin-spice-berry    "#99caf1")
  ;; (pumpkin-spice-stem     "#2d3e40")
  ;; (pumpkin-spice-lavendar "#c099f1")
  ;; (pumpkin-spice-mint     "#99f1c0")
  (pumpkin-spice-gourd    "#343338"))

 ((default                      (:foreground pumpkin-spice-gourd :background pumpkin-spice-latte))
  (error                        (:foreground pumpkin-spice-cran :weight 'semi-bold))
  (cursor                       (:background pumpkin-spice-pumpkin))
  (region                       (:background pumpkin-spice-ginger))
  (hl-line                      (:background pumpkin-spice-fluff))
  (link                         (:background pumpkin-spice-berry))
  (fringe                       (:background pumpkin-spice-caramel))
  (mode-line                    (:background pumpkin-spice-clove :foreground pumpkin-spice-cream))
  (mode-line-emphasis           (:foreground pumpkin-spice-cream :weight 'bold))
  (mode-line-inactive           (:background pumpkin-spice-cream))
  (tab-bar                      (:background pumpkin-spice-clove))
  (tab-bar-tab                  (:background pumpkin-spice-cinnamon :foreground pumpkin-spice-cream :box '(:style released-button)))
  (tab-bar-tab-inactive         (:background pumpkin-spice-clove :foreground pumpkin-spice-cream :box '(:style released-button)))
  (font-lock-comment-face       (:foreground pumpkin-spice-pumpkin :slant 'italic))
  (font-lock-string-face        (:foreground pumpkin-spice-nutmeg))
  (font-lock-type-face          (:foreground pumpkin-spice-nutmeg :weight 'bold))
  (font-lock-constant-face      (:foreground pumpkin-spice-nutmeg :weight 'bold))
  (font-lock-variable-name-face (:foreground pumpkin-spice-cinnamon :weight 'bold ))
  (font-lock-function-name-face (:foreground pumpkin-spice-ginger :background pumpkin-spice-caramel 'bold))
  (font-lock-builtin-face       (:foreground pumpkin-spice-clove :weight 'semi-bold))
  (font-lock-keyword-face       (:foreground pumpkin-spice-clove :weight 'extra-bold))))

(provide-theme 'pumpkin-spice)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'pumpkin-spice-theme)

;;; pumpkin-spice-theme.el ends here
