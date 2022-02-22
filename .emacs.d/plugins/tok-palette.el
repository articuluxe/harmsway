;;; tok-palette.el --- Palette for my theme -*- lexical-binding: t; -*-

;; Copyright (C) 2021, Topi Kettunen <topi@topikettunen.com>

;; Author: Topi Kettunen <topi@topikettunen.com>
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

;; Color palette for the my theme.

;;; Code:

(defvar tok-theme-color-palette-alist
  '((primary . "#00FF00")
    (primary2 . "#008000")
    (primary3 . "#004d00")
    (primary4 . "#003300")
    (primary5 . "#001900")
    (bg . "#000000")
    (fg . primary)
    (fg-highlight . fg)
    (bg-highlight . primary5)
    (fg-active . primary)
    (bg-active . primary3)
    (fg-inactive . primary2)
    (bg-inactive . primary5)
    (hl-line . primary4)
    (region . primary4)
    (comment . primary2)
    (link . primary))
  "My theme's palette")

(provide 'tok-palette)

;;; tok-palette.el ends here
