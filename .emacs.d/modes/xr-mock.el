;; xr-mock.el --- major mode to view mock scripts
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Wednesday, June 10, 2015
;; Version: 1.2
;; Modified Time-stamp: <2016-05-10 15:22:04 dan.harms>
;; Keywords: mock script

;;; Code:

(defgroup mock-mode nil "*mock script mode" :group 'mock)

(defvar mock-mode-map
  (let ((map (make-sparse-keymap)))
    map) "Keymap for mock-mode")

(defun mock-mode()
  "Major mode for browsing mock scripts."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mock-mode)
  (setq mode-name "mock-view")
  (set-syntax-table (let ((table (make-syntax-table nil)))
                      (modify-syntax-entry ?# "<" table)
                      (modify-syntax-entry ?\n ">" table)
                      table))
  (make-local-variable 'font-lock-defaults)
  (eval-when-compile (defvar mock-mode-font-lock-keywords nil)) ;silence compilation warning
  (setq mock-mode-font-lock-keywords
        (list
         ;; opening timestamp
         (list "^[[:digit:]]+"
               '(0 font-lock-constant-face))
         ;; keywords
         (list "@@\\(startfunc\\|callfunc\\|endfunc\\|include\\|define\\|regex\\|ignoreLineWith\\|ignoreField\\(sFor\\|Always\\)\\)"
               '(0 font-lock-builtin-face))
         (list "\\_<\\(Exit\\|SetSaturdayCheck\\|\\(Dump\\(Time\\|Quotes\\|QtyAtPrice\\)\\)\\)\\_>"
               '(0 font-lock-builtin-face))
         (list "\\_<\\(Dump\\(Open\\)?Orders\\)\\|\\(\\(Set\\|Start\\|Stop\\|AddTo\\)Clock\\)\\|\\(\\_<Log\\_>\\)\\|\\(SocketReceiveDisconnect\\)\\_>"
               '(0 font-lock-builtin-face))
         (list "\\_<\\(To\\|From\\)\\(Opt\\|Cmd\\)?\\>"
               '(0 font-lock-keyword-face))
         (list "\\(CommandConsole\\)"
               '(0 font-lock-keyword-face))
         ;; functions
         (list "@@\\sw+=\\([^(]+()\\)"
               '(1 font-lock-function-name-face))
         ;; key=value pairs
         (list "\\([^[:space:];|=<',]+\\)=\\([^[:space:];|=',]*\\)"
               '(1 font-lock-variable-name-face)
               '(2 font-lock-constant-face))
         ;; 1tick instruments
         (list "\\<[A-Za-z0-9_]+@[[:alpha:]]+\\_>"
               '(0 font-lock-constant-face))
         ;; single quote strings
         (list "'.+?'" '(0 font-lock-string-face))
        ))
  (setq font-lock-defaults '(mock-mode-font-lock-keywords))
  (set (make-local-variable 'syntax-propertize-function)
       (syntax-propertize-rules
        ("\\(=\\)\\<begin\\_>" (1 "< b"))
        ("\\_<\\(b\\)egin_comment\\_>" (1 "< b"))
        ("=\\<en\\(d\\)\\_>" (1 "> b"))
        ("\\_<end_commen\\(t\\)\\_>" (1 "> b"))
        ))
  (use-local-map mock-mode-map)
  (define-key mock-mode-map "\M-sg" 'mock-goto-line)
  (setq comment-start "#" comment-end "")
  (subword-mode 1)
  (run-hooks 'mock-mode-hook)
  )

(add-to-list
 'auto-mode-alist
 '("mockobjects/testscripts/.*\\.\\(txt\\|script\\|defines\\)$"
   . mock-mode))
(add-to-list
 'ff-special-constructs
 '("^\\(?:@@\\)?\\(?:.*\\.\\)?\\(include\\|sourceFiles\\|scriptfile\\)\\s-*=\\s-*\\(.*\\)"
   lambda nil (buffer-substring (match-beginning 2) (match-end 2))) t)

(defun mock-goto-line (n)
  "Go to a line in a mock script, exclusive of continuation lines."
  (interactive "nGoto mock line: ")
  (goto-char (point-min))
  (while (> n 1)
    (unless (search-forward-regexp "\\\\$" (line-end-position) t)
      (setq n (- n 1)))
    (forward-line)))

(provide 'xr-mock)

;;; xr-mock.el ends here
