;; log-viewer.el --- major mode to view log files
;; Copyright (C) 2015-2017  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2017-02-08 11:18:31 dan.harms>
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

(defgroup log-viewer-mode nil "*log file mode" :group 'log-viewer)

(defun log-viewer-hide-ctrl-a ()
  "Don't show C-a in log files.  This helps delimit fields in the
fix protocol, using a pipe `|'."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^A [?|]))

(add-hook 'log-viewer-mode-hook #'log-viewer-hide-ctrl-a)

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
         (list "\\<\\(ERROR\\|FATAL\\|WARN\\|[^W]error\\)\\>"
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
         (list "[^\\]'.+?[^\\]'" '(0 font-lock-string-face))
         ;; key=value pairs
         (list "\\([^[:space:];|=<',]+\\)=\\([^[:space:];|=',]*\\)"
               '(1 font-lock-variable-name-face)
               '(2 font-lock-constant-face))
         (list "<.+?>" '(0 font-lock-doc-face))
         ;; text within brackets
         (list "\\[.+?\\]" '(0 font-lock-keyword-face))
         ;; ip address:port (overrides prior fontification)
         (list "\\(\\(?:[[:digit:]]+\\.\\)\\{3\\}[[:digit:]]+\\)\\(:[[:digit:]]\\{2,5\\}\\)?"
               '(1 font-lock-variable-name-face t)
               '(2 font-lock-constant-face t t))
         ;; personal debug statements
         (list "drh" '(0 font-lock-comment-face t))
         ))
  (setq font-lock-defaults '(log-viewer-mode-font-lock-keywords))
  (run-hooks 'log-viewer-mode-hook)
  )

(provide 'log-viewer)

;; log-viewer.el ends here
