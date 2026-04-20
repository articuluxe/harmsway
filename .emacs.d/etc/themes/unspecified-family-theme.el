;;; unspecified-family-theme.el --- Theme unspecifying family face-attribute  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
;; Copyright (C) 2026 Mekeor Melire

;; Author:                  Mekeor Melire <mekeor@posteo.de>
;; Maintainer:              Mekeor Melire <mekeor@posteo.de>
;; SPDX-License-Identifier: GPL-3.0-only

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

;; This file provides the `unspecified-family' theme which sets the
;; face-attribute `:family' of all faces (but the `default' face) to
;; `unspecified'.  This allows you for example to force all faces to
;; use the default font family, i.e. the `:family' that is specified
;; for the `default' face.

;;; Code:

(require 'most-faces)

(deftheme unspecified-family
  "Theme that unspecifies the `:family' attributes of all faces.

Only the `default' face is not themed at all.")

(defvar unspecified-family-theme-spec '((t :family unspecified))
  "Face specification unspecifying `:family' attribute.")

(apply #'custom-theme-set-faces 'unspecified-family
       (mapcar
        (lambda (face) (list face unspecified-family-theme-spec))
        ;; `most-faces-as-faces' promises to keep `default' face as
        ;; its very first element.  We skip it because unspecifying
        ;; the default face yields unexpected behavior.
        (cdr most-faces-as-faces)))

;;;###autoload
(when load-file-name
  (require 'custom)
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'unspecified-family)

;;; unspecified-family-theme.el ends here
