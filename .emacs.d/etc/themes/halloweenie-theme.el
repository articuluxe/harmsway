;;; halloweenie-theme.el --- Dark and spooky Halloween color theme -*- lexical-binding:t -*-

;; Copyright (C) 2019-2023 Colin Okay

;; Author: Colin Okay <colin@cicadas.surf>
;; Maintainer: Colin Okay <colin@cicadas.surf>
;; URL: https://cicadas.surf/cgit/halloweenie-theme.git
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
 halloweenie "Ghost, Ghouls, and worst of all: Pumpkin Spice"
 ((((class color) (min-colors #xFFFFFF)))

  (halloweenie-pitch "#1c1c1c")
  (halloweenie-night "#1f213a")
  (halloweenie-blood "#a2270d")
  (halloweenie-potion "#832ea1")
  (halloweenie-slime "#15ed0a")
  (halloweenie-jackolantern "#FF9430")
  (halloweenie-spellglow "#d478e0")
  (halloweenie-bone "#E8E7D5")
  (halloweenie-rot "#6Bc980")
  ;; (halloweenie-ghost "#C1CAE8")
  ;; (halloweenie-shine "#70c2ca")
  (halloweenie-cateyes "#ffde38")
  (halloweenie-evileyes "#FF1F1F"))

 ((default                      (:foreground halloweenie-bone :background halloweenie-pitch))
  (error                        (:foreground halloweenie-evileyes :weight 'semi-bold))
  (cursor                       (:background halloweenie-slime))
  (region                       (:background "black"))
  (hl-line                      (:background halloweenie-night))
  (link                         (:background halloweenie-potion))
  (mode-line                    (:background halloweenie-jackolantern :foreground halloweenie-pitch))
  (mode-line-emphasis           (:foreground halloweenie-pitch :weight 'bold))
  (font-lock-comment-face       (:foreground halloweenie-slime))
  (font-lock-string-face        (:foreground halloweenie-rot))
  (font-lock-type-face          (:foreground halloweenie-cateyes :weight 'ultra-bold))
  (font-lock-constant-face      (:foreground halloweenie-evileyes :weight 'extra-bold))
  (font-lock-variable-name-face ( :weight 'bold :foreground halloweenie-spellglow))

  (font-lock-function-name-face (:foreground halloweenie-spellglow :weight 'bold :slant 'italic))
  (font-lock-builtin-face       (:foreground halloweenie-jackolantern :weight 'semi-bold))
  (font-lock-keyword-face       (:foreground halloweenie-jackolantern :weight 'extra-bold))

  ;; org
  (org-todo                    (:weight 'bold :foreground halloweenie-blood))
  (org-done                    (:weight 'bold :foreground halloweenie-rot ))

  ;; doom modeline

 (doom-modeline-project-dir
  (:foreground halloweenie-pitch :weight 'bold ))
 (doom-modeline-project-root-dir
  (:foreground halloweenie-blood :weight 'bold))

 ;; persp

 (persp-selected-face
  (:foreground halloweenie-blood :weight 'bold :slant 'italic))))

(provide-theme 'halloweenie)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'halloweenie-theme)

;;; halloweenie-theme.el ends here
