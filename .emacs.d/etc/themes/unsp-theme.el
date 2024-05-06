;;; unsp-theme.el --- Theme unspecifying most face attributes  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author:                  Mekeor Melire <mekeor@posteo.de>
;; Created:                 2024
;; Homepage:                https://codeberg.org/mekeor/emacs-unsp
;; Keywords:                faces, theme
;; Maintainer:              Mekeor Melire <mekeor@posteo.de>
;; Package-Requires:        ((emacs "28.1") (most-faces "0.0.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version:                 0.0.2

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

;; `unsp-theme' is a package providing an equally named theme which
;; sets the attributes of all faces to `unspecified' -- except for the
;; `default' face.  In particular, it thus makes the default
;; attributes of (almost) all defined faces ineffective.  This is
;; useful at least in following scenarios:

;; - As an Emacs user, when you can load this theme and customize only
;;   the `default' face, all faces will adhere to its specification.

;; - As an Emacs theme developer, you use `unsp-theme' to debug your
;;   theme: You can load `unsp-theme' before loading your own theme in
;;   order to check if yours depends on default face attributes.  You
;;   might even require users of your theme to load `unsp-theme'
;;   before loading yours in order to erase the default face
;;   attributes.

;;;; Details:

;; Beside setting face attributes to `unspecified', `unsp-theme' also
;; sets some face-related variables.  Take a look at the code for the
;; details.

;; `unsp-theme' does not theme the `default' face because that can
;; lead to unexpected behavior in its author's experience.

;; `unsp-theme' depends on the `most-faces' package:
;; https://codeberg.org/mekeor/emacs-most-faces

;;;; Usage:

;; `unsp-theme' can be used just like any other theme.  After
;; installation, i.e. ensuring it is in a directory that is member of
;; your `load-path', evaluate the following:
;;
;;   (load-theme 'unsp)

;; In addition, as mentioned before, you may want to customize the
;; `default' face in your `user' theme.  For example, if you want
;; everything to be white on black, then also evaluate the following:
;;
;;   (custom-set-faces
;;     '(default ((t :background "#000000" :foreground "#ffffff")) t))

;;;; Screenshot:

;; A screenshot is available in the `screenshot' branch and thus
;; accessible on the web:
;; https://codeberg.org/mekeor/emacs-unsp/raw/branch/screenshot/screenshot.png

;;; Code:

(require 'most-faces)

(deftheme unsp)

(defvar unsp-theme-unspecified-spec
  '((t
      :background     unspecified
      :box            unspecified
      :extend         unspecified
      :family         unspecified
      :foreground     unspecified
      :foundry        unspecified
      :height         unspecified
      :inherit        unspecified
      :inverse-video  unspecified
      :overline       unspecified
      :slant          unspecified
      :stipple        unspecified
      :strike-through unspecified
      :underline      unspecified
      :weight         unspecified
      :width          unspecified))
  "Specification of a face with all attributes unspecified.")

(defvar unsp-theme-unspecified-face
  unsp-theme-unspecified-spec
  "Face with all attributes unspecified.")

(apply #'custom-theme-set-faces 'unsp
  (seq-map
    (lambda (face) (list face unsp-theme-unspecified-spec))
    ;; `most-faces-as-faces' promises to keep `default' face as its
    ;; very first element.  We skip it because unspecifying the
    ;; default face yields unexpected behavior.
    (cdr most-faces-as-faces)))

(apply #'custom-theme-set-variables 'unsp
  (seq-map
    (lambda (var) (list var ''unsp-theme-unspecified-spec))
    most-faces-as-variables))

(custom-theme-set-variables 'unsp
  `(highlight-parentheses-colors nil)
  `(ibuffer-fontification-alist nil))

(provide-theme 'unsp)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-directory load-file-name)))

(provide 'unsp-theme)

;;; unsp-theme.el ends here
