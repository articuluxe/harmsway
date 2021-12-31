;;; tok-palettes.el --- Palettes for my theme -*- lexical-binding: t; -*-

;; Copyright (C) 2021, Topi Kettunen <mail@topikettunen.com>

;; Author: Topi Kettunen <mail@topikettunen.com>
;; URL: https://github.com/topikettunen/tok-theme
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; Color palettes for the my theme.

;;; Code:

(defvar tok-dark-color-palette-alist
  '((fg1 . "#ffffff")
    (fg2 . "#e6e6e6")
    (fg3 . "#b8b8b8")
    (fg4 . "#a6a6a6")
    (bg1 . "#000000")
    (bg2 . "#272b30")
    (bg3 . "#3a3e42")
    (hl . "#171717")
    (bg-active . "#323232")
    (fg-active . "#f4f4f4")
    (bg-inactive . "#1e1e1e")
    (fg-inactive . "#bfc0c4")
    (comment . "#a8a8a8"))
  "Dark palette")

(defvar tok-light-color-palette-alist
  '((fg1 . "#000000")
    (fg2 . "#1a1a1a")
    (fg3 . "#515151")
    (fg4 . "#626262")
    (bg1 . "#ffffff")
    (bg2 . "#ebebeb")
    (bg3 . "#d6d6d6")
    (hl . "#f8f8f8")
    (bg-active . "#d7d7d7")
    (fg-active . "#0a0a0a")
    (bg-inactive . "#efefef")
    (fg-inactive . "#404148")
    (comment . "#505050"))
  "Light palette")

(provide 'tok-palettes)

;;; tok-palettes.el ends here
