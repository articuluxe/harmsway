;;; xr-mock.el --- view mock files
;; Copyright (C) 2015, 2018  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Wednesday, June 10, 2015
;; Version: 1.0
;; Modified Time-stamp: <2018-06-04 10:59:20 dan.harms>
;; Modified by: Dan Harms
;; Keywords: tools mock

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
;; View xr mock files.
;;

;;; Code:
(defgroup xr-mock-mode nil "*mock script mode" :group 'mock)

(defvar xr-mock-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `xr-mock-mode'.")

(defun xr-mock-mode()
  "Major mode for browsing mock scripts."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'xr-mock-mode)
  (setq mode-name "mock-view")
  (set-syntax-table (let ((table (make-syntax-table nil)))
                      (modify-syntax-entry ?# "<" table)
                      (modify-syntax-entry ?\n ">" table)
                      table))
  (make-local-variable 'font-lock-defaults)
  (eval-when-compile (defvar xr-mock-mode-font-lock-keywords nil)) ;silence compilation warning
  (setq xr-mock-mode-font-lock-keywords
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
         (list "\\(CommandConsole\\|SetValidLogin\\|SetInvalidLogin\\)"
               '(0 font-lock-keyword-face))
         (list "\\_<\\(STR\\|DEC\\|I32\\|I64\\)\\_>"
               '(0 font-lock-type-face))
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
  (setq font-lock-defaults '(xr-mock-mode-font-lock-keywords))
  (set (make-local-variable 'syntax-propertize-function)
       (syntax-propertize-rules
        ("\\(=\\)\\<begin\\_>" (1 "< b"))
        ("\\_<\\(b\\)egin_comment\\_>" (1 "< b"))
        ("=\\<en\\(d\\)\\_>" (1 "> b"))
        ("\\_<end_commen\\(t\\)\\_>" (1 "> b"))
        ))
  (use-local-map xr-mock-mode-map)
  (define-key xr-mock-mode-map "\M-sg" 'xr-mock-goto-line)
  (setq comment-start "#" comment-end "")
  (subword-mode 1)
  (run-hooks 'xr-mock-mode-hook)
  )

(add-to-list
 'auto-mode-alist
 '("mockobjects/testscripts/.*\\.\\(txt\\|script\\|defines\\)$"
   . xr-mock-mode))
(add-to-list
 'ff-special-constructs
 '("^\\(?:@@\\)?\\(?:.*\\.\\)?\\(include\\|sourceFiles\\|scriptfile\\)\\s-*=\\s-*\\(.*\\)"
   lambda nil (buffer-substring (match-beginning 2) (match-end 2))) t)

(defun xr-mock-goto-line (n)
  "Go to line N in a mock script, exclusive of continuation lines."
  (interactive "nGoto mock line: ")
  (goto-char (point-min))
  (while (> n 1)
    (unless (search-forward-regexp "\\\\$" (line-end-position) t)
      (setq n (- n 1)))
    (forward-line)))

(provide 'xr-mock)

;;; xr-mock.el ends here
