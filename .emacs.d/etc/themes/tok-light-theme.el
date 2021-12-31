;;; tok-light-theme.el --- My light theme -*- lexical-binding: t; -*-

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

;; Light variant of my theme.

;;; Code:

(require 'tok)
(eval-when-compile
  (require 'tok-palettes))

(deftheme tok-light
  "The light variant of my theme.")

(tok-with-color-variables 'light 'tok-light
  tok-light-color-palette-alist)

(provide-theme 'tok-light)
(provide 'tok-light-theme)

;;; tok-light-theme.el ends here
