;;; custom-completion.el --- custom completion utilities
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Friday, April 15, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-04-15 17:49:16 dharms>
;; Modified by: Dan Harms
;; Keywords: completion

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

;;; Commentary:

;;

;;; Code:

(defun choose-via-popup (alist prompt)
  "Make a choice among ALIST, a list of choices, with PROMPT as a possible
prompt.  ALIST can either be a list of strings, or an alist, where every
element is a cons cell, the car of which is the display string given to the
user, and the cdr of which is the resultant value to be used if that cell
is selected."
  (popup-menu*
   (delete-consecutive-dups
    (mapcar (lambda(elt)
              (if (consp elt)
                  (popup-make-item (car elt) :value (cdr elt))
                (popup-make-item elt :value elt)))
            alist))
   :isearch t
   :isearch-filter (popup-imenu--filter)
   :prompt prompt
   ))

(defun choose-via-ido (alist prompt)
  "Make a choice among ALIST, a list of choices, with PROMPT as a possible
prompt.  ALIST can either be a list of strings, or an alist, where every
element is a cons cell, the car of which is the display string given to the
user, and the cdr of which is the resultant value to be used if that cell
is selected."
  (let ((res
         (ido-completing-read
          prompt
          (mapcar (lambda(elt)
                    (if (consp elt)
                        (car elt)
                      elt))
                  alist))))
    (or (cdr (assoc res alist))
        res)))

(defun choose-via-ivy (alist prompt)
  "Make a choice among ALIST, a list of choices, with PROMPT as a possible
prompt.  ALIST can either be a list of strings, or an alist, where every
element is a cons cell, the car of which is the display string given to the
user, and the cdr of which is the resultant value to be used if that cell
is selected."
  (let ((res
         (ivy-completing-read
          prompt
          (mapcar (lambda(elt)
                    (if (consp elt)
                        (car elt)
                      elt))
                  alist))))
    (or (cdr (assoc res alist))
        res)))

(defvar my/choose-func 'choose-via-popup
  "Make a selection among a list of choices.")

(defun my/choose-choose-func ()
  "Toggle between available selection frameworks.  Current choices include
 `ido', `popup' and `ivy'."
  (interactive)
  (if (eq my/choose-func 'choose-via-popup)
      (progn
        (setq my/choose-func 'choose-via-ido)
        (message "Choosing via ido"))
    (if (eq my/choose-func 'choose-via-ido)
        (progn
          (setq my/choose-func 'choose-via-ivy)
          (message "Choosing via ivy"))
      (setq my/choose-func 'choose-via-popup)
      (message "Choosing via popup"))))


(provide 'custom-completion)
;;; custom-completion.el ends here
