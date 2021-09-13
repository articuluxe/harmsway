;;; registers-completing-read.el --- Completing read for registers

;; Author: nerding_it <virtualxi99@gmail.com>
;; Created: 12 Jul 2021
;; Keywords: tools
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://gitlab.com/nerding_it/emacs-registers-completing-read

;; This file is not part of Emacs

;;        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                    Version 2, December 2004

;; Copyright (C) 2021 nerding_it <virtualxi99@gmail.com>

;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.

;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

;;  0. You just DO WHAT THE FUCK YOU WANT TO.

;;; Commentary:

;; Completing read for registers

;;; Code:

(defcustom registers-completing-read-function 'completing-read
  "Completion function for registers."
  :group 'tools
  :type 'function)

(defun registers-completing-read-jump (register)
  "Jump to register `REGISTER'."
  (interactive (list (funcall registers-completing-read-function "Register: "
				    (seq-map
				     (lambda (reg) (format "%s %s" (char-to-string (car reg)) (register-describe-oneline (car reg))))
				     (seq-filter
				      (lambda (reg) (or (markerp (cdr reg))
						   (frameset-register-p (cdr reg))
						   (and (consp (cdr reg))
							(or (window-configuration-p (cadr reg))
							    (string-equal (cadr reg) 'file)))))
				      register-alist)))))
  (string-match "^\\([[:digit:]]\\)" register)
  (jump-to-register (string-to-char (match-string 0 register))))

(defun registers-completing-read-insert (register)
  "Insert text from `REGISTER'."
  (interactive (list (funcall registers-completing-read-function "Register: "
					  (seq-map
					   (lambda (reg) (format "%s %s" (char-to-string (car reg)) (register-describe-oneline (car reg))))
					  (seq-filter
					   (lambda (reg)
					     (or
					      (stringp (cdr reg))
					      (numberp (cdr reg))
					      (and (consp (cdr reg))
						   (stringp (cadr reg)))))
					   register-alist)))))
  (string-match "^\\([[:digit:]]\\)" register)
  (insert-register (string-to-char (match-string 0 register))))

(provide 'registers-completing-read)

;;; registers-completing-read.el ends here
