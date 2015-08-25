;; -*- Mode: Emacs-Lisp -*-
;; xr-mock-mode.el --- major mode to view mock scripts
;; Copyright (C) 2015  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Wednesday, June 10, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-08-25 05:37:51 dharms>
;; Keywords: mock script

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

(defgroup mock-mode nil "*mock script mode" :group 'mock)

(defun mock-mode()
  "Major mode for browsing mock scripts."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mock-mode)
  (setq mode-name "mock-view")
  (set-syntax-table (let ((table (make-syntax-table nil)))
                      (modify-syntax-entry ?# "< b" table)
                      (modify-syntax-entry ?\n "> b" table)
                      table))
  (make-local-variable 'font-lock-defaults)
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
         (list "\\_<\\(Dump\\(Open\\)?Orders\\)\\|\\(\\(Set\\|Start\\|Stop\\|AddTo\\)Clock\\)\\_>"
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
         ;; comment delimiters
         (list "\\(=begin\\|begin_comment\\|=end\\|end_comment\\)"
               '(0 font-lock-comment-face))
         ;; 1tick instruments
         (list "\\<[A-Za-z0-9_]+@[[:alpha:]]+\\_>"
               '(0 font-lock-constant-face))
         ;; single quote strings
         (list "'.*?'" '(0 font-lock-string-face))
        ))
  (setq font-lock-defaults '(mock-mode-font-lock-keywords))
  (setq comment-start "#" comment-end "")
  (run-hooks 'mock-mode-hook)
  )

(add-to-list 'auto-mode-alist
             '("mockobjects/testscripts/.*\\.\\(txt\\|script\\|defines\\)$"
               . mock-mode))
(add-to-list 'ff-special-constructs
             '("^\\(?:@@\\)?\\(include\\|sourceFiles\\|scriptfile\\)\\s-*=\\s-*\\(.*\\)"
               lambda nil
               (buffer-substring (match-beginning 2) (match-end 2))) t)

(provide 'mock)

;; xr-mock-mode.el ends here
