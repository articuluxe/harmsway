;;; evenok-dark-theme.el --- Dark theme with perceptively evenly distributed colors  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author:                  Mekeor Melire <mekeor@posteo.de>
;; Homepage:                https://codeberg.org/mekeor/evenok-themes
;; Maintainer:              Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This feature provides a dark theme with perceptively evenly
;; distributed colors.

;;; Code:

(deftheme evenok-dark
  :family 'evenok
  :kind 'color-scheme
  :background-mode 'dark)

(require 'evenok)

(evenok-theme 'evenok-dark evenok-palette-dark)

(evenok-with-palette evenok-palette-dark
  (custom-theme-set-variables
   'evenok-dark
   '(frame-background-mode 'dark)
   '(pdf-view-midnight-colors (cons "white" "black"))))

(provide-theme 'evenok-dark)

;;; evenok-dark-theme.el ends here
