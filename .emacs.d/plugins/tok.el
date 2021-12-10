;;; tok.el --- My theme -*- lexical-binding: t; -*-

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

;; Main file

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'tok-faces)

;;; Setup Start
(defmacro tok-with-color-variables (variant theme-name color-palette)
  "Eval `tok-definition' in tok COLOR-PALETTE for THEME-NAME.
VARIANT is 'dark or 'light."
  (declare (indent defun))
  (let ((color-palette* (eval color-palette)))
    `(let* ((class '((class color) (min-colors 89)))
            (light-class (append '((background light)) class))
            (dark-class (append '((background dark)) class))
            (theme-name ,theme-name)
            (variant ,variant)
            ,@(mapcar (lambda (elm) `(,(car elm) ,(cdr elm))) color-palette*))
       (let ((custom--inhibit-theme-enable nil))
         ,@tok-definition))))

;;; Setup Start
(defmacro tok-minimal-with-color-variables (variant theme-name color-palette)
  "Eval `tok-color-definition' in tok COLOR-PALETTE for THEME-NAME.
VARIANT is 'dark or 'light."
  (declare (indent defun))
  (let ((color-palette* (eval color-palette)))
    `(let* ((class '((class color) (min-colors 89)))
            (light-class (append '((background light)) class))
            (dark-class (append '((background dark)) class))
            (theme-name ,theme-name)
            (variant ,variant)
            ,@(mapcar (lambda (elm) `(,(car elm) ,(cdr elm))) color-palette*))
       (let ((custom--inhibit-theme-enable nil))
         ,@tok-minimal-definition))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'tok)

;;; tok.el ends here
