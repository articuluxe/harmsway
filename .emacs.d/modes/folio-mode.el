;;; FOLIO mode ;;;
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
