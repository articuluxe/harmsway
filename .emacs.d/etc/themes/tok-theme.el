;;; tok-theme.el --- My theme -*- lexical-binding: t; -*-

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

;; Personal preferences for Emacs colours.

;;; Code:

(unless (>= emacs-major-version 24.3)
  (error "tok-theme requires Emacs 24 or later."))

(require 'tok)

(eval-when-compile
  (require 'tok-palette))

(deftheme tok
  "My theme.")

(tok-with-color-variables 'tok
  tok-theme-color-palette-alist)

(provide-theme 'tok)
(provide 'tok-theme)

;;; tok-theme.el ends here
