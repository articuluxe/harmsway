;;; company-c-preprocessor.el --- A company backend for programming language c-preprocessor

;; Copyright (C) 2018 Fréderic Francès

;; Author: Frédéric Francès
;; Keywords: development company
;; Package-Requires: ((emacs "24.1") (company "0.8"))

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;;  This is mostly inspired from company-keywords and company-c-headers.

;;; Code:
(require 'company)
(require 'cl-lib)
(require 'rx)

(defvar company-c-preprocessor-modes
  '(
    (c-mode     . ,(rx ".h" line-end))
    (c++-mode   . ,(rx (or (: line-start (one-or-more (in "A-Za-z0-9_")))
                           (or ".h" ".hpp" ".hxx" ".hh"))
                       line-end)))
  "Assoc list of supported major modes and associated header file names.")

(defvar company-c-preprocessor-trigger-rx
  '(
    (c-mode     . "#\\([a-z]+\\)")
    (c++-mode   . "#\\([a-z]+\\)"))
  "Assoc list of regex triggering auto completion depending on major modes.")
  
(defvar company-c-preprocessor-alist
  ;; Please contribute corrections or additions.
  '((c++-mode
     "define" "undef" "include" "if" "ifdef" "ifndef" "else" "elif" "endif" "line" "error" "pragma")
    ;; aliases
    (c-mode . c++-mode))
  "Alist mapping major-modes to sorted c-preprocessor for `company-c-preprocessor'.")

;;;###autoload
(defun company-c-preprocessor (command &optional arg &rest ignored)
  "`company-mode' backend for programming language keywords."
  (interactive (list 'interactive))
  (when (and (assoc major-mode company-c-preprocessor-modes)
             (looking-back (cdr (assoc major-mode company-c-preprocessor-trigger-rx))
                           (line-beginning-position)))
    (cl-case command
      (interactive (company-begin-backend 'company-c-preprocessor))
      (prefix (and (assq major-mode company-c-preprocessor-alist)
                   (not (company-in-string-or-comment))
                   (or (company-grab-symbol) 'stop)))
      (candidates
       (let ((completion-ignore-case nil)
             (symbols (cdr (assq major-mode company-c-preprocessor-alist))))
         (all-completions arg (if (consp symbols)
                                  symbols
                                (cdr (assq symbols company-c-preprocessor-alist))))))
      (sorted t))))

(provide 'company-c-preprocessor)
;;; company-c-preprocessor.el ends here
