;;; phps-mode-test.el --- Commons for tests -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-macros)

(defvar
  phps-mode-test--native-tokens
  nil
  "The native tokens of PHP parser (if available).")

(defmacro phps-mode-test--incremental-vs-intial-buffer (source &optional title &rest change)
  "Set up test buffer with SOURCE, TITLE, apply CHANGE and compare incremental values with initial values."
  `(let ((test-buffer-incremental (generate-new-buffer "test-incremental"))
         (incremental-state)
         (incremental-state-stack)
         (incremental-states)
         (incremental-heredoc-label)
         (incremental-heredoc-label-stack)
         (incremental-parse-trail)
         (incremental-parse-error)
         (incremental-tokens)
         (incremental-imenu)
         (incremental-indent)
         (incremental-buffer)
         (incremental-bookkeeping)
         (incremental-nest-location-stack)
         (test-buffer-initial (generate-new-buffer "test-initial"))
         (initial-state)
         (initial-state-stack)
         (initial-states)
         (initial-heredoc-label)
         (initial-heredoc-label-stack)
         (initial-parse-trail)
         (initial-parse-error)
         (initial-tokens)
         (initial-imenu)
         (initial-indent)
         (initial-buffer)
         (initial-bookkeeping)
         (initial-nest-location-stack))

     ;; Setup incremental buffer
     (switch-to-buffer test-buffer-incremental)
     (insert ,source)
     (goto-char 0)
     (phps-mode-debug-message
       (message "\nTesting incremental buffer '%s':\n'%s'\n" ,title ,source))
     (phps-mode)

     (phps-mode-debug-message
      (message "\nPerforming changes\n"))
     
     ,@change
     (phps-mode-lex-analyzer--process-changes test-buffer-incremental)
     (setq incremental-state phps-mode-lex-analyzer--state)
     (setq incremental-state-stack phps-mode-lex-analyzer--state-stack)
     (setq incremental-heredoc-label phps-mode-lex-analyzer--heredoc-label)
     (setq incremental-heredoc-label-stack phps-mode-lex-analyzer--heredoc-label-stack)
     (setq incremental-parse-trail phps-mode-lex-analyzer--parse-trail)
     (setq incremental-parse-error phps-mode-lex-analyzer--parse-error)
     (setq incremental-states phps-mode-lex-analyzer--states)
     (setq incremental-tokens phps-mode-lex-analyzer--tokens)
     (setq incremental-imenu phps-mode-lex-analyzer--imenu)
     (setq incremental-indent (phps-mode-test--hash-to-list phps-mode-lex-analyzer--lines-indent))
     (setq incremental-buffer (buffer-substring (point-min) (point-max)))
     (setq incremental-bookkeeping (phps-mode-test--hash-to-list phps-mode-lex-analyzer--bookkeeping t))
     (setq incremental-nest-location-stack phps-mode-lex-analyzer--nest-location-stack)

     ;; Setup incremental buffer
     (switch-to-buffer test-buffer-initial)
     (insert incremental-buffer)
     (goto-char 0)
     (phps-mode-debug-message
       (message "\nTesting initial buffer '%s':\n'%s'\n" ,title incremental-buffer))
     (phps-mode)
     (setq initial-state phps-mode-lex-analyzer--state)
     (setq initial-state-stack phps-mode-lex-analyzer--state-stack)
     (setq initial-states phps-mode-lex-analyzer--states)
     (setq initial-heredoc-label phps-mode-lex-analyzer--heredoc-label)
     (setq initial-heredoc-label-stack phps-mode-lex-analyzer--heredoc-label-stack)
     (setq initial-parse-trail phps-mode-lex-analyzer--parse-trail)
     (setq initial-parse-error phps-mode-lex-analyzer--parse-error)
     (setq initial-tokens phps-mode-lex-analyzer--tokens)
     (setq initial-imenu phps-mode-lex-analyzer--imenu)
     (setq initial-indent (phps-mode-test--hash-to-list phps-mode-lex-analyzer--lines-indent))
     (setq initial-buffer (buffer-substring (point-min) (point-max)))
     (setq initial-bookkeeping (phps-mode-test--hash-to-list phps-mode-lex-analyzer--bookkeeping t))
     (setq initial-nest-location-stack phps-mode-lex-analyzer--nest-location-stack)

     ;; Run tests
     (phps-mode-debug-message
       (message "\nComparing tokens, lines indent and imenu  between buffer:\n\n'%s'\n\nand:\n\n'%s'\n" initial-buffer incremental-buffer))
     (should (equal initial-buffer incremental-buffer))
     ;; (message "Initial tokens: %s\n" initial-tokens)
     ;; (message "Incremental tokens: %s\n" incremental-tokens)
     (should (equal initial-tokens incremental-tokens))
     (should (equal initial-state incremental-state))
     (should (equal initial-state-stack incremental-state-stack))
     (should (equal initial-states incremental-states))
     (should (equal initial-heredoc-label incremental-heredoc-label))
     (should (equal initial-heredoc-label-stack incremental-heredoc-label-stack))
     (should (equal initial-parse-trail incremental-parse-trail))
     (should (equal initial-parse-error incremental-parse-error))
     (should (equal initial-nest-location-stack incremental-nest-location-stack))
     ;; (message "Initial indent: %s\n" initial-indent)
     ;; (message "Incremental indent: %s\n" incremental-indent)
     (should (equal initial-indent incremental-indent))
     (should (equal initial-imenu incremental-imenu))
     (should (equal initial-bookkeeping incremental-bookkeeping))

     (kill-buffer test-buffer-incremental)
     (kill-buffer test-buffer-initial)

     (when ,title
       (message "\nPassed incremental tests for '%s'\n" ,title))))

(defmacro phps-mode-test--with-buffer (source &optional title &rest body)
  "Set up test buffer with SOURCE, TITLE and BODY."
  `(let ((test-buffer (generate-new-buffer "test")))
     (switch-to-buffer test-buffer)
     (insert ,source)
     (goto-char 0)
     (phps-mode-debug-message
      (message "\nTesting buffer '%s':\n'%s'\n" ,title ,source))
     (phps-mode)
     ,@body

     ;; (let ((lexer-tokens phps-mode-lex-analyzer--tokens))
     ;;   (setq phps-mode-test--native-tokens nil)
     ;;   (with-temp-buffer
     ;;     (let ((filename (expand-file-name "lexer-test.tmp"))
     ;;           (filename2 (expand-file-name "lexer-test2.tmp")))
     ;;       (insert "<?php ini_set('display_errors', false); ini_set('error_reporting', false); if (function_exists('token_get_all')) { $tokens = token_get_all(file_get_contents(\"lexer-test2.tmp\")); echo \"'(\"; foreach ($tokens as $token) { echo \"\n  \"; if (is_array($token)) { echo token_name($token[0]); } else { echo '\"' . $token . '\"'; }} echo \"\n)\";}")
     ;;       (write-file filename)
     ;;       (kill-region (point-min) (point-max))
     ;;       (insert ,source)
     ;;       (write-file filename2)
     ;;       (kill-region (point-min) (point-max))

     ;;       (let* ((native-tokens
     ;;               (call-process
     ;;                "php"
     ;;                nil
     ;;                t
     ;;                t
     ;;                (concat "-f" filename))))
     ;;         (when (= native-tokens 0)
     ;;           ;; (message "native-tokens: %S %s" native-tokens (buffer-substring-no-properties (point-min) (point-max)))
     ;;           (let ((tokens (eval (car (read-from-string (buffer-substring-no-properties (point-min) (point-max)))))))
     ;;             (setq
     ;;              phps-mode-test--native-tokens
     ;;              tokens)
     ;;             (let ((trimmed-tokens))
     ;;               (dolist (token lexer-tokens)
     ;;                 (push (car token) trimmed-tokens)
     ;;                 )
     ;;               (setq trimmed-tokens (reverse trimmed-tokens))
     ;;               (should
     ;;                (equal
     ;;                 trimmed-tokens
     ;;                 phps-mode-test--native-tokens))
     ;;               (message "Native tokens matches elisp tokens"))))))))
     
     (kill-buffer test-buffer)
     (when ,title
       (message "\nPassed tests for '%s'\n" ,title))))

(defun phps-mode-test--hash-to-list (hash-table &optional un-sorted)
  "Return a list that represent the HASH-TABLE.  Each element is a list: (list key value), optionally UN-SORTED."
  (let (result)
    (if (hash-table-p hash-table)
        (progn
          (maphash
           (lambda (k v)
             (push (list k v) result))
           hash-table)
          (if un-sorted
              (nreverse result)
            (sort (nreverse result) (lambda (a b) (< (car a) (car b))))))
      nil)))

(transient-mark-mode t)
(electric-pair-mode t)
(setq phps-mode-async-process nil)

(provide 'phps-mode-test)

;;; phps-mode-test.el ends here
