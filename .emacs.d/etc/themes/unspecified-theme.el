;;; unspecified-theme.el --- Theme that unspecifies all attributes of all faces  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author:                  Mekeor Melire <mekeor@posteo.de>
;; Created:                 2024
;; Homepage:                https://codeberg.org/mekeor/unspecified-theme
;; Keywords:                faces, theme
;; Maintainer:              Mekeor Melire <mekeor@posteo.de>
;; Package-Requires:        ((emacs "25") (most-faces "0.0.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version:                 0.2

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

;; `unspecified-theme' is a package providing an equally named theme
;; which sets the attributes of all faces to `unspecified' -- except
;; for the `default' face.  In particular, it thus makes the default
;; attributes of (almost) all defined faces ineffective.  This is
;; useful at least in following scenarios:

;; - As an Emacs user, when you load this theme and customize only the
;;   `default' face, all faces will adhere to its specification.  Used
;;   as such, `unspecified-theme' provides a radically minimalist
;;   theme.

;; - As an Emacs theme developer, you use `unspecified-theme' to debug
;;   the composability of your theme: You can load `unspecified-theme'
;;   before loading your own theme in order to check if yours depends
;;   on default face attributes.  You might even require users of your
;;   theme to load `unspecified-theme' before loading yours in order
;;   to erase the default face attributes.

;;;; Usage:

;; `unspecified-theme' can be used just like any other theme.  After
;; installation, i.e. ensuring it is in a directory that is member of
;; your `load-path', evaluate the following:
;;
;;   (load-theme 'unspecified 'no-confirm)

;; In addition, as mentioned before, you may want to customize the
;; `default' face in your `user' theme.  For example, if you want
;; everything to be white on black, then also evaluate the following:
;;
;;   (custom-set-faces
;;     '(default ((t :background "#000000" :foreground "#ffffff")) t))

;;;; Screenshot:

;; A screenshot is available in the `screenshot' branch and thus
;; accessible on the web:
;; https://codeberg.org/mekeor/unspecified-theme/raw/branch/screenshot/screenshot.png

;;;; Details:

;; Beside setting face attributes to `unspecified',
;; `unspecified-theme' also sets some face-related variables.  Take a
;; look at the code for the details.

;; `unspecified-theme' does not theme the `default' face because that
;; can lead to unexpected behavior in its author's experience.

;;;; Dependencies:

;; `unspecified-theme' depends on the `most-faces' package:
;; https://codeberg.org/mekeor/most-faces

;;;; Roadmap:

;; Split this theme into two or three: (1.) A theme that unspecifies
;; all faces-as-faces; (2.) A theme that unspecifies all
;; faces-as-variables; (3.) A theme that unspecifies more complex
;; variables referring to faces, like `highlight-parentheses-colors'
;; and `ibuffer-fontification-alist'.

;;; Code:

(require 'most-faces)

(deftheme unspecified
  "Theme that unspecifies all attributes of all faces.

Only the `default' face is not themed at all.")

(defvar unspecified-theme-unspecified-spec
  '((t :background     unspecified
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

(defvar unspecified-theme-unspecified-face
  unspecified-theme-unspecified-spec
  "Face with all attributes unspecified.")

(apply #'custom-theme-set-faces 'unspecified
       (mapcar
        (lambda (face) (list face unspecified-theme-unspecified-spec))
        ;; `most-faces-as-faces' promises to keep `default' face as
        ;; its very first element.  We skip it because unspecified the
        ;; default face yields unexpected behavior.
        (cdr most-faces-as-faces)))

(apply #'custom-theme-set-variables 'unspecified
       (mapcar
        (lambda (var) (list var ''unspecified-theme-unspecified-spec))
        most-faces-as-variables))

(custom-theme-set-variables
 'unspecified
 '(highlight-parentheses-colors nil)
 '(ibuffer-fontification-alist nil))

;;;###autoload
(when load-file-name
  (require 'custom)
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'unspecified)

;;; unspecified-theme.el ends here
