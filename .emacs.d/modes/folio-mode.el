;; -*- Mode: Emacs-Lisp -*-
;; folio-mode.el --- folio major mode
;; Copyright (C) 2015  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-02-28 03:32:24 dharms>
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

;; Commentary:

;;

;; Code:

(defvar folio-keywords
  '("keyword" "other-keyword"))
(defvar folio-constants
  '("update" "change" "remove"))
(defvar folio-font-lock-defaults
  `((
	 ("[^\\]\"[^ ]+?[^\\]\"" . font-lock-string-face)
	 ("{\\|}\\|\\[\\|]" . font-lock-keyword-face)
	 ( ,(regexp-opt folio-keywords 'words) . font-lock-builtin-face)
	 ( ,(regexp-opt folio-constants 'words) . font-lock-constant-face)
	 )))

(defvar folio-basic-offset 4 "*Indentation ofset for `folio-mode'.")

(defun folio-indent-line() "Indent current line via FOLIO mode."
  (interactive)
  (let ((indent-col 0))
	(save-excursion
	  (beginning-of-line)
	  (condition-case nil
		  (while t
			(backward-up-list 1)
			(when (looking-at "[[{]")
			  (setq indent-col (+ indent-col folio-basic-offset))
			  )
			)
		(error nil)
		)
	  )
	(save-excursion
	  (back-to-indentation)
	  (when (and (looking-at "[]}]") (>= indent-col folio-basic-offset))
		(setq indent-col (- indent-col folio-basic-offset))
		)
	  )
	(indent-line-to indent-col)
	)
  )

(define-derived-mode folio-mode fundamental-mode "FOLIO language"
  "FOLIO is a major mode for editing folio files"
  (setq mode-name "FOLIO")
  (setq font-lock-defaults folio-font-lock-defaults)
  (setq indent-line-function 'folio-indent-line)
  (setq-default indent-tabs-mode nil)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq comment-start "<")
  (setq comment-end ">")
  (modify-syntax-entry ?# "< b" folio-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" folio-mode-syntax-table)
  (modify-syntax-entry ?< "< b" folio-mode-syntax-table)
  (modify-syntax-entry ?> "> b" folio-mode-syntax-table)
  (define-key folio-mode-map "\C-c\C-c" 'comment-region)
  (define-key folio-mode-map "\C-c\C-u" 'uncomment-region)
  )

(provide 'folio-mode)

;; folio-mode.el ends here
