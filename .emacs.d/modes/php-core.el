;;; php-core.el --- Core definitions shared by PHP packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; URL: https://github.com/emacs-php/php-mode
;; Keywords: languages, php
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file holds the handful of definitions that every PHP package
;; needs and that depend on nothing else: the `php' customization group,
;; the `php-executable' path, the `syntax-ppss' predicates and the
;; `php-base-mode' parent mode.
;;
;; It exists so that a module can reach those primitives without pulling
;; in the whole of php.el, which requires `flymake' and `php-project'
;; (and, for now, CC Mode).  php.el itself requires this file and
;; re-exports everything here, so `(require 'php)' keeps working exactly
;; as before; nothing needs to migrate.
;;
;; Keep this file free of dependencies.  Anything that needs `flymake',
;; `php-project', CC Mode or a PHP syntax table belongs in php.el, not
;; here.

;;; Code:


;;; Customization group

;;;###autoload
(defgroup php nil
  "Language support for PHP."
  :tag "PHP"
  :group 'languages
  :link '(url-link :tag "Official Site" "https://github.com/emacs-php/php-mode")
  :link '(url-link :tag "PHP Mode Wiki" "https://github.com/emacs-php/php-mode/wiki"))

(defcustom php-executable (or (executable-find "php") "php")
  "The location of the PHP executable."
  :tag "PHP Executable"
  :type 'string)


;;; Utility for locating language constructs

(defsubst php-in-string-p ()
  "Return non-nil if inside a string.
It is the character that will terminate the string, or t if the string should
be terminated by a generic string delimiter."
  (nth 3 (syntax-ppss)))

(defsubst php-in-comment-p ()
  "Return NIL if outside a comment, T if inside a non-nestable comment, else
an integer (the current comment nesting)."
  (nth 4 (syntax-ppss)))

(defsubst php-in-string-or-comment-p ()
  "Return character address of start of comment or string; nil if not in one."
  (nth 8 (syntax-ppss)))

(defsubst php-in-poly-php-html-mode ()
  "Return T if current buffer is in `poly-html-mode'."
  (bound-and-true-p poly-php-html-mode))


;;; Base major mode

;;;###autoload
(define-derived-mode php-base-mode prog-mode "PHP"
  "Generic major mode for editing PHP.

This mode is intended to be inherited by concrete major modes.
Currently there are `php-mode' and `php-ts-mode'."
  nil)

(provide 'php-core)
;;; php-core.el ends here
