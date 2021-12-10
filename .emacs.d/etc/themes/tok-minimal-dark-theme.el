;;; tok-minimal-dark-theme.el --- My dark theme -*- lexical-binding: t; -*-

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

;; Minimal dark variant of my theme.

;;; Code:

(require 'tok)
(eval-when-compile
  (require 'tok-palettes))

(deftheme tok-minimal-dark
  "The dark variant of my theme.")

(tok-minimal-with-color-variables 'dark 'tok-minimal-dark
  tok-minimal-dark-color-palette-alist)

(provide-theme 'tok-minimal-dark)
(provide 'tok-minimal-dark)

;;; tok-minimal-dark-theme.el ends here
