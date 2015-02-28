;; -*- Mode: Emacs-Lisp -*-
;; auto-insert-choose+.el --- Enhancements to auto-insert-choose.el
;; Copyright (C) 2015  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Friday, February 27, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-02-27 19:25:33 dharms>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:

;;

;; Code:
(require 'auto-insert-choose)

(defun auto-insert-choose-yas-expand ()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(defun auto-insert-choose-and-call-ido (template-alist)
   "Interactively choose (using ido) and call a function from TEMPLATE-ALIST.
TEMPLATE-ALIST should be a list whose elements are (STRING FUNCTION).
Intended for use in `auto-insert-alist'"
   (let* ((name (ido-completing-read "Choose template: "
                                     template-alist nil t))
          (cell (assoc name template-alist)))
     (when cell
       (funcall (cadr cell)))))

(defun auto-insert-choose-and-call-popup (template-alist)
   "Interactively choose (via popup) and call a function from TEMPLATE-ALIST.
TEMPLATE-ALIST should be a list whose elements are (STRING FUNCTION).
Intended for use in `auto-insert-alist'"
   (let* ((list (mapcar 'car template-alist))
          (name (popup-menu* list))
          (cell (assoc name template-alist)))
     (when cell
       (funcall (cadr cell)))))

(defmacro auto-insert-choose+-add-entry (alist name)
  "Helper macro to add template file NAME to ALIST."
  `(auto-insert-choose-add-entry ,alist ,name
                                 (lambda ()
                                   (insert-file-contents
                                    (concat auto-insert-directory ,name))
                                   (auto-insert-choose-yas-expand))))

(provide 'auto-insert-choose+)

;; auto-insert-choose+.el ends here
