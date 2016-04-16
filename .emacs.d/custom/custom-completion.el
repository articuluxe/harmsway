;;; custom-completion.el --- custom completion utilities
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Friday, April 15, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-04-16 00:25:22 dharms>
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

(defun my/activate-ido ()
  "Activate ido as a completion framework."
  (when (boundp 'ivy-mode)
    (ivy-mode 0))
  ;; find file
  (global-set-key "\C-x\C-f" 'find-file)
  ;; magit
  (setq magit-completing-read-function 'magit-ido-completing-read)
  ;; recentf
  (setq uniquify-recentf-func 'uniquify-recentf-ido-recentf-open)
  ;; M-x
  (global-set-key (kbd "M-x") 'smex)
  (if (boundp 'ivy-mode)
      (global-set-key "\e\ex" 'counsel-M-x)
    (global-set-key "\e\ex" 'execute-extended-command))
  (ido-mode 1)
  (message "Using ido for completion"))

(defun my/activate-ivy ()
  "Activate ivy as a completion framework."
  (when (boundp 'ido-mode)
    (ido-mode 0))
  ;; find file
  (global-set-key "\C-x\C-f" 'counsel-find-file)
  ;; magit
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; recentf
  (setq uniquify-recentf-func 'uniquify-recentf-ivy-recentf-open)
  ;; M-x
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key "\e\ex" 'smex)
  (ivy-mode 1)
  (message "Using ivy for completion"))

;; choose the completion framework in use
(defun my/choose-completion ()
  "Choose among completion frameworks. Uses the completion framework
 specified by `my/choose-func'. Current choices are ido and ivy."
  (interactive)
  (funcall (funcall my/choose-func my/completion-framework-alist
                    "Completion framework:")))

(provide 'custom-completion)
;;; custom-completion.el ends here
