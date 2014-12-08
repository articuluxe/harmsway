;; -*- Mode: Emacs-Lisp -*-
;;
;;
(defgroup log-viewer-mode nil "*log file mode" :group 'log-viewer)

(defun log-viewer-mode()
  "Log-viewer mode is a mode for browsing log files."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'log-viewer-mode)
  (setq mode-name "log-viewer")
  ;; (modify-syntax-entry ?_ "w")
  (make-local-variable 'font-lock-defaults)
  (setq log-viewer-mode-font-lock-keywords
        (list
         (list "\\(ERROR\\|FATAL\\|WARN\\|[^W]error\\)"
               '(1 font-lock-warning-face))

         ;; log statements
         ;; TODO: insert regex here
         ;; '(1 font-lock-builtin-face)
         ;; '(2 font-lock-function-name-face)
         ;; '(3 font-lock-keyword-face)
         ;; '(4 font-lock-constant-face)
         ;; '(5 font-lock-warning-face)
         ;; '(6 font-lock-variable-name-face)

         ;; single quote strings
         (list "'.*'" '(0 font-lock-string-face))
         ;; key=value pairs
         (list "\\([^[:space:];|=<',]+\\)=\\([^[:space:];|=',]*\\)"
               '(1 font-lock-variable-name-face)
               '(2 font-lock-constant-face))
         ;; (list "<.*>" '(0 font-lock-doc-face))
         ;; text within brackets
         (list "\\[.*?\\]" '(0 font-lock-keyword-face))
         ;; ip address:port (overrides prior fontification)
         (list "\\(\\(?:[[:digit:]]+\\.\\)\\{3\\}[[:digit:]]+\\):\\([[:digit:]]+\\)"
               '(1 font-lock-variable-name-face t)
               '(2 font-lock-constant-face t))
         ;; personal debug statements
         (list "drh" '(0 font-lock-comment-face t))
         ))
  (setq font-lock-defaults '(log-viewer-mode-font-lock-keywords))
  (run-hooks 'log-viewer-mode-hook)
  )

(provide 'log-viewer)
