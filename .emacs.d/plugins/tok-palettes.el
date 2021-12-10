;;; tok-palettes.el --- Palettes for my theme -*- lexical-binding: t; -*-

;; Copyright (C) 2021, Topi Kettunen <mail@topikettunen.com>

;; Author: Topi Kettunen <mail@topikettunen.com>
;; URL: https://github.com/topikettunen/tok-theme
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

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
  '((fg1 . "#e6e6e6")
    (bg1 . "#101010"))
  "Dark palette")

(defvar tok-light-color-palette-alist
  '((fg1 . "#1a1a1a")
    (bg1 . "#ebebeb"))
  "Light palette")

(defvar tok-minimal-dark-color-palette-alist
  '((fg1 . "#ffffff")
    (fg2 . "#e6e6e6")
    (fg3 . "#b8b8b8")
    (fg4 . "#a6a6a6")
    (bg1 . "#101010")
    (bg2 . "#272b30")
    (bg3 . "#3a3e42")
    (hl . "#171717")
    (comment . "#ffffff"))
  "Minmal dark palette")

(defvar tok-minimal-light-color-palette-alist
  '((fg1 . "#000000")
    (fg2 . "#1a1a1a")
    (fg3 . "#515151")
    (fg4 . "#626262")
    (bg1 . "#ffffff")
    (bg2 . "#ebebeb")
    (bg3 . "#d6d6d6")
    (hl . "#f8f8f8")
    (comment . "#000000"))
  "Minimal light palette")

(provide 'tok-palettes)

;;; tok-palettes.el ends here
