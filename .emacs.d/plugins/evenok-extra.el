;;; evenok-extra.el --- Opinionated extras to evenok themes  -*- lexical-binding: t; -*-

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

;; This feature provides a framework for defining opinionated
;; extra-themes to be used on top of to (regular) evenok themes.

;;; Code:

(require 'nnheader)

(require 'evenok)

(defgroup evenok-extra nil
  "Opinionated extras to `evenok'."
  :group 'evenok
  :prefix "evenok-extra-")

(defface evenok-extra-flymake-error-bitmap nil nil)
(defface evenok-extra-flymake-warning-bitmap nil nil)
(defface evenok-extra-flymake-note-bitmap nil nil)

(defcustom evenok-extra-gnus-summary-dummy-line-format
  "                                        ╤ "
  "String to which `evenok-extra-gnus-summary-dummy' face will be applied.

The result will be put in place of `%uE' within
`gnus-summary-dummy-line-format'."
  :type 'string)

(defface evenok-extra-gnus-summary-dummy nil nil)

;; For the following definition, `package-lint' will report `error:
;; "gnus-user-format-function-E" doesn't start with package's prefix
;; "evenok-extra"'.  But `gnus' relies on this function to be named
;; like this.  So there's nothing we can do about this.
(defun gnus-user-format-function-E (header)
  "Dummy faced `evenok-extra-gnus-summary-dummy-line-format' and subject.

Argument HEADER is a Gnus message header."
  (propertize (concat evenok-extra-gnus-summary-dummy-line-format
                      (mail-header-subject header))
              'face 'evenok-extra-gnus-summary-dummy))

(defface evenok-extra-hi-lock-1 nil nil)
(defface evenok-extra-hi-lock-2 nil nil)
(defface evenok-extra-hi-lock-3 nil nil)
(defface evenok-extra-hi-lock-4 nil nil)
(defface evenok-extra-hi-lock-5 nil nil)
(defface evenok-extra-hi-lock-6 nil nil)
(defface evenok-extra-hi-lock-7 nil nil)
(defface evenok-extra-hi-lock-8 nil nil)

(defface evenok-extra-hl-todo-done nil nil)
(defface evenok-extra-hl-todo-fixme nil nil)
(defface evenok-extra-hl-todo-info nil nil)
(defface evenok-extra-hl-todo-pndg nil nil)
(defface evenok-extra-hl-todo-prgs nil nil)
(defface evenok-extra-hl-todo-todo nil nil)
(defface evenok-extra-hl-todo-xxx nil nil)

(defface evenok-extra-org-info nil nil)
(defface evenok-extra-org-cncl nil nil)
(defface evenok-extra-org-pndg nil nil)
(defface evenok-extra-org-prgs nil nil)

(defun evenok-extra-theme (name palette)
  "Set variables and faces of theme NAME using colors from PALETTE."
  (evenok-with-palette palette

    (custom-theme-set-variables name
      `(erc-log-match-format
         (concat
           (propertize "%t" 'face (list :foreground ,faded))
           " "
           (propertize "%c" 'face (list :foreground ,bright-blue))
           " "
           (propertize "%n" 'face (list :foreground ,bright-yellow))
           ": "
           (propertize "%m"
             'wrap-prefix (list 'space :width 4)
             'line-prefix (list 'space :width 4))))
      '(flymake-error-bitmap
         (list 'exclamation-mark 'evenok-extra-flymake-error-bitmap))
      '(flymake-warning-bitmap
         (list 'question-mark 'evenok-extra-flymake-warning-bitmap))
      '(flymake-note-bitmap
         (list 'question-mark 'evenok-extra-flymake-note-bitmap))
      '(hi-lock-face-defaults
        '("evenok-extra-hi-lock-1"
          "evenok-extra-hi-lock-2"
          "evenok-extra-hi-lock-3"
          "evenok-extra-hi-lock-4"
          "evenok-extra-hi-lock-5"
          "evenok-extra-hi-lock-6"
          "evenok-extra-hi-lock-7"
          "evenok-extra-hi-lock-8"))
      '(hl-todo-keyword-faces
         (list
           ;; Apply `upcase' on lower-cased strings to avoid
           ;; highlighting by `hl-todo-mode'.
           (cons (upcase "done")  'evenok-extra-hl-todo-done)
           (cons (upcase "fixme") 'evenok-extra-hl-todo-fixme)
           (cons (upcase "info")  'evenok-extra-hl-todo-info)
           (cons (upcase "pndg")  'evenok-extra-hl-todo-pndg)
           (cons (upcase "prgs")  'evenok-extra-hl-todo-prgs)
           (cons (upcase "todo")  'evenok-extra-hl-todo-todo)
           (cons (upcase "xxx+")  'evenok-extra-hl-todo-xxx)))
      '(org-todo-keyword-faces
         (list
           ;; Apply `upcase' on lower-cased strings to avoid
           ;; highlighting by `hl-todo-mode'.
           (cons (upcase "info") 'evenok-extra-org-info)
           (cons (upcase "cncl") 'evenok-extra-org-cncl)
           (cons (upcase "prgs") 'evenok-extra-org-prgs)
           (cons (upcase "pndg") 'evenok-extra-org-pndg))))

    (custom-theme-set-faces name
      `(evenok-extra-flymake-error-bitmap   ((t :foreground ,bright-red)))
      `(evenok-extra-flymake-warning-bitmap ((t :foreground ,bright-orange)))
      `(evenok-extra-flymake-note-bitmap    ((t :foreground ,bright-blue)))
      `(evenok-extra-gnus-summary-dummy     ((t :foreground ,faded)))
      `(evenok-extra-hi-lock-1              ((t :background ,bright-yellow :foreground ,black :weight bold)))
      `(evenok-extra-hi-lock-2              ((t :background ,bright-orange :foreground ,black :weight bold)))
      `(evenok-extra-hi-lock-3              ((t :background ,bright-red :foreground ,black :weight bold)))
      `(evenok-extra-hi-lock-4              ((t :background ,bright-magenta :foreground ,black :weight bold)))
      `(evenok-extra-hi-lock-5              ((t :background ,bright-purple :foreground ,black :weight bold)))
      `(evenok-extra-hi-lock-6              ((t :background ,bright-blue :foreground ,black :weight bold)))
      `(evenok-extra-hi-lock-7              ((t :background ,bright-cyan :foreground ,black :weight bold)))
      `(evenok-extra-hi-lock-8              ((t :background ,bright-green :foreground ,black :weight bold)))
      `(evenok-extra-hl-todo-done           ((t :foreground ,bright-green)))
      `(evenok-extra-hl-todo-fixme          ((t :foreground ,bright-red)))
      `(evenok-extra-hl-todo-info           ((t :foreground ,bright-green)))
      `(evenok-extra-hl-todo-pndg           ((t :foreground ,bright-orange)))
      `(evenok-extra-hl-todo-prgs           ((t :foreground ,bright-purple)))
      `(evenok-extra-hl-todo-todo           ((t :foreground ,bright-red)))
      `(evenok-extra-hl-todo-xxx            ((t :foreground ,bright-red)))
      `(evenok-extra-org-info               ((t :foreground ,bright-green)))
      `(evenok-extra-org-cncl               ((t :foreground ,bright-green)))
      `(evenok-extra-org-pndg               ((t :foreground ,bright-orange)))
      `(evenok-extra-org-prgs               ((t :foreground ,bright-purple))))))

(provide 'evenok-extra)

;;; evenok-extra.el ends here
