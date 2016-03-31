;; folio-electric.el --- font lock settings for folio major mode
;; Copyright (C) 2015, 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-03-30 18:27:19 dharms>
;; Keywords:

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
(require 'folio-mode)
(defgroup folio-electric nil
  "Minor mode providing electric editing for folio files."
  :group 'folio)
(define-minor-mode folio-electric-mode
  "Electric mode for folio files."
 ; initial value
  nil
 ; Indicator for mode line
  " FE"
 ; keymap
  folio-mode-map
  (folio-electric-setup-keymap)
  )

(defvar folio-electric-matching-delimiter-alist
  '((?\{ . ?\})
	(?\[ . ?\])
	))
(defun folio-should-quote-char-for-regexp(arg)
  (char-equal arg ?\[))
(defun folio-electric-setup-keymap()
  (define-key folio-mode-map "{" 'folio-electric-matching-delimiter-start)
  (define-key folio-mode-map "}" 'folio-electric-matching-delimiter-end)
  (define-key folio-mode-map "[" 'folio-electric-matching-delimiter-start)
  (define-key folio-mode-map "]" 'folio-electric-matching-delimiter-end)
  )

(defun folio-electric-code-at-point-p()
  (and folio-electric-mode
	   (let* ((properties (text-properties-at (point))))
		 (and (null (memq 'font-lock-string-face properties))
			  (null (memq 'font-lock-comment-face properties))
			  ))))

(defun folio-line-contains-open-bracep ()
  "Return t if the current line contains an opening brace before point."
  (save-excursion
	(beginning-of-line)
	(search-forward-regexp
	 (let ((c (car (rassoc last-command-char folio-electric-matching-delimiter-alist)))
		   )
	   (if (folio-should-quote-char-for-regexp c)
		   (concat "\\" (make-string 1 c))
		 (make-string 1 c))
	 )
	(point-at-eol) t)
  ))

(defun folio-line-contains-text-before-pointp ()
  "Return t if the current line contains non-whitespace text before point."
  (save-excursion
	(beginning-of-line)
	(search-forward-regexp "[^ \\t]+" (point-at-eol) t)
	))

(defun folio-electric-matching-delimiter-start (arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (cond ((folio-electric-code-at-point-p)(reindent-then-newline-and-indent)
		 ))
  )
(defun folio-electric-matching-delimiter-end (arg)
  (interactive "P")
  (if (folio-electric-code-at-point-p)
	  (cond ((folio-line-contains-open-bracep)())
			((folio-line-contains-text-before-pointp)(newline-and-indent))
			(t (indent-for-tab-command))
			))
  (self-insert-command (prefix-numeric-value arg))
  (cond ((folio-electric-code-at-point-p)
		 (reindent-then-newline-and-indent)
		 ))
  )

(eval-after-load 'folio-mode '(add-hook 'folio-mode-hook 'folio-electric-mode))
(provide 'folio-electric)

;; folio-electric.el ends here
