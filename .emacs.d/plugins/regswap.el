;;; regswap.el --- Functionality for swapping two regions

;;
;; COpyright (C) 2024 by Sergey Kitov
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.
;;

;; Author: Sergey Kitov
;; URL: http://github.com/skitov/regswap
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; The package provides two interactive functions:
;; regswap-mark-region and regswap-cancel
;;
;; If no region for swapping is defined,
;; regswap-mark-region defines it as current region.
;; If swap region is defined, regswap-mark-region swaps contents
;; of swap region and region and undefine swap region.
;;
;; Either of current region and swap region can be empty.
;; In this case content of non-empty region is moved to position of empty.
;;
;; If swap region and region are intersecting, no changes are made to buffer.
;;
;; When regswap-highlight is non-nil,
;; swap region is highlighted with regswap-reg-face, when non-empty.
;; When swap region is empty, it's position is heighlighted by displaying
;; ║ character in position of swap region, heighlighted with regswap-empty-face.
;; regswap-highlight value is t by default.
;;
;; regswap-cancel undefines swap region and removes highlighting overlay.
;;
;; If changes are made to the buffer, when swap region is defined:
;; - If changed area overlaps with swap region, swapping is cancelled
;; - If changes are inside of the swap region,
;; swap region is changed accordingly, modified text will be swapped.
;; - If changes are done before the swap region,
;; swap region is shifted by length difference of changed text.
;; - insertions at the borders of the swap region are treated as outside of
;; swap region (not modifying text for swapping)
;;
;; Keybindings suggested by the package are "C-x w w" for regswap-mark-region
;; and "C-x w c" for regswap-cancel.
;; These keybindings can be set by placing (regswap-setup-default-keybindings)
;; in Emacs init script.
;;
;; For setting other keybindings place
;; (global-set-key (kbd "xxx") 'regswap-mark-region)
;; (global-set-key (kbd "yyy") 'regswap-cancel)
;; where "xxx" and "yyy" are keybindings of your choice.
;;

;;; Code:

(defgroup regswap-faces nil
  "Regswap package faces"
  :group 'faces)
(defvar regswap-highlight t)
(defvar regswap-hl-color "LightCyan1")
(defvar regswap-region nil)
(defvar-local regswap-overlay nil)
(defface regswap-reg-face
  `((t :background ,regswap-hl-color))
  "Face for highlighting region to swap:")
(defface regswap-empty-face
  `((t :foreground ,regswap-hl-color))
  "Face for highlighting position to swap:")

(defun regswap-cancel (&optional silent)
  "Set `regswap-region' to nil, remove highlight overlay.
Doesn't give messages if SILENT is non-nil."
  (interactive)
  (unless silent (message "Swapping cancelled."))
  (remove-hook 'after-change-functions #'regswap-handle-change t)
  (when regswap-overlay (delete-overlay regswap-overlay))
  (setq regswap-region nil))

(defun regswap-highlight-swap ()
  "Put overlay for highlighting `regswap-region'."
  (let ((swap-begin (car regswap-region)) (swap-end (cadr regswap-region)))
    (setq regswap-overlay (make-overlay swap-begin swap-end))
    (if (< swap-begin swap-end)
	(overlay-put regswap-overlay 'face 'regswap-reg-face)
      (overlay-put regswap-overlay 'before-string
		   (propertize "\u2551" 'face 'regswap-empty-face)))))

(defun regswap-rgn ()
  "Return region for swap."
  (if (region-active-p)
      `(,(region-beginning) ,(region-end))
    `(,(point) ,(point))))

(defun regswap-mark-region ()
  "Set `regswap-region' or swap `regswap-region' with region."
  (interactive)
  (if regswap-region
      (regswap-do-swap regswap-region (regswap-rgn))
    (setq regswap-region (regswap-rgn))
    (add-hook 'after-change-functions #'regswap-handle-change nil t)
    (deactivate-mark)
    (if regswap-highlight
	(regswap-highlight-swap)
      (message "swap set to: %s" regswap-region))))

(defun regswap-do-swap (first second)
  "Swap contents of FIRST and SECOND regions.
If regions overlap give error message without changes."
  (let ((b1 (car first)) (e1 (cadr first)) (b2 (car second)) (e2 (cadr second)))
    (regswap-cancel t)
    (setq regswap-region nil)
    (if (or (and (<= b1 b2) (< b2 e1)) (and (<= b2 b1) (< b1 e2)))
	(error "Overlapping regions")
      (save-excursion
	(insert
	 (prog1 (delete-and-extract-region (max b2 b1) (max e2 e1))
	   (goto-char (max b2 b1))
	   (insert
	    (delete-and-extract-region (min b2 b1) (min e2 e1)))
	   (goto-char (min b1 b2))))))))

(defun regswap-handle-change (reg-begin reg-end old-len)
  "Keep regswap-region valid in case of changes in buffer.
REG-BEGIN and REG-END are bounds of region just changed,
OLD-LEN is length of the region before change."
  (when regswap-region
    (let ((swap-begin (car regswap-region)) (swap-end (cadr regswap-region)))
      (when (< reg-begin swap-end)
	(let ((old-end (+ reg-begin old-len)))
	  (if (or (< swap-end old-end) (and (< reg-begin swap-begin) (< swap-begin old-end)))
	      (progn (message "Cancelling swapping because swap region changed ambiguously")
		     (regswap-cancel t))
	    (let ((len-diff (- reg-end old-end)))
	      (when (<= reg-begin swap-begin)
		(setq swap-begin (+ swap-begin len-diff)))
	      (setq swap-end (+ swap-end len-diff))
	      (when regswap-highlight (move-overlay regswap-overlay swap-begin swap-end))
	      (setq regswap-region `(,swap-begin ,swap-end)))))))))

(defun regswap-setup-default-keybindings ()
  "Set key bindings for package interactive functions."
  (global-set-key (kbd "C-x w w") #'regswap-mark-region)
  (global-set-key (kbd "C-x w c") #'regswap-cancel))

(provide 'regswap)

;;; regswap.el ends here
