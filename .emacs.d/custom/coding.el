;; coding.el --- coding utilities
;; Copyright (C) 2015, 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-09-21 09:21:56 dan.harms>
;; Modified by: Dan Harms
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

(eval-when-compile
  (setq use-package-verbose t)
  (require 'use-package))

(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-u" 'uncomment-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cc-chainsaw ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-chainsaw :disabled t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; preproc-font-lock ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/preproc-font-lock ()
  "Toggle preprocessor font lock."
  (interactive)
  (require 'preproc-font-lock)
  (preproc-font-lock-mode
   (if preproc-font-lock-mode 0 1)))
(use-package preproc-font-lock
  :bind ("C-c #" . my/preproc-font-lock)
  :commands preproc-font-lock-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; c++-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-c-comment-delimiters()
  "Toggle the comment delimiters for c-derived programming languages."
  (interactive)
  (if (= 0 (length comment-end))
      (progn
        (setq comment-start "/*")
        (setq comment-end "*/")
        (message "/* Using comments like this */")
        )
    (progn
      (setq comment-start "//")
      (setq comment-end "")
      (message "// Using comments like this"))))

(add-hook
 'c-mode-common-hook
 (lambda ()
   (require 'compile)
   (setq-default indent-tabs-mode nil)
   (setq c-auto-newline t)
   (c-toggle-hungry-state t)
   ;; (setq comment-column 40)
   (setq hide-ifdef-lines t)
   (hide-ifdef-mode 1)
   (make-local-variable 'my/compile-command)
   (define-key c++-mode-map (kbd "\C-c RET") 'my/compile)
   (define-key c++-mode-map "\C-cm" 'my/recompile)
   (define-key c++-mode-map "\C-ck" 'kill-compilation)
   (define-key c++-mode-map "\C-c\C-c" 'comment-region)
   (define-key c++-mode-map "\C-c\C-u" 'uncomment-region)
   (setq comment-start "/*") (setq comment-end "*/")
   (define-key c++-mode-map "\C-c/" 'toggle-c-comment-delimiters)
   ;; skips the final included file, ending in `:', when traversing compile
   ;; errors.  See
   ;; `http://stackoverflow.com/questions/15489319/how-can-i-skip-in-file-included-from-in-emacs-c-compilation-mode'
   (setf (nth 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
   (c-add-style "default-style"
                (quote
                 ((c-basic-offset . 3)
                  (c-electric-pound-behavior . (alignleft))
                  (c-cleanup-list . (
                                     empty-defun-braces
                                     defun-close-semi
                                     one-liner-defun
                                     scope-operator
                                     list-close-comma
                                     compact-empty-funcall
                                     comment-close-slash
                                     ))
                  (c-offsets-alist . (
                                      (innamespace           . 0)
                                      (substatement-open     . 0)
                                      (inline-open           . 0)
                                      (statement-case-intro  . +)
                                      (statement-case-open   . +)
;(statement-cont . c-lineup-math)
                                      (access-label          . -2)
                                      (comment-intro         . c-lineup-comment)
                                      (member-init-intro     . +)
                                      (arglist-cont-nonempty . +)
;(comment-intro . 0)
;(arglist-intro . c-lineup-arglist-intro-after-paren)
;(arglist-close . c-lineup-arglist)
                                      )))))
   ))

(add-hook 'prog-mode-hook
          (lambda()
            (font-lock-add-keywords
             nil '(
                   ;; TODO declarations
                   ("\\<[tT][oO][dD][oO]\\>" 0 font-lock-warning-face t)
                   ;; FIXME
                   ("\\<[fF][iI][xX][mM][eE]\\>" 0 font-lock-warning-face t)
                   ) t)
            ) t)

(with-eval-after-load 'cc-mode (require 'modern-cpp-font-lock))

(add-hook
 'c++-mode-hook
 (lambda()
   (modern-c++-font-lock-mode 1)
   (font-lock-add-keywords
    nil '(;; complete some fundamental keywords (+ Qt)
          ;; add the new C++11 keywords (override and final already there)
          ;; ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
          ;; hexadecimal numbers
          ;; ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
          ;; Qt fontification
          ("\\<\\(Q_OBJECT\\|SIGNAL\\|SLOT\\|slots\\|signals\\)\\>" . font-lock-keyword-face)
          ("\\<QT?\\(_\\sw+\\)+\\>" . font-lock-keyword-face)
          ;; This is fairly aggressive; can reenable if desired
          ;; ("\\<Q[A-Z][A-Za-z0-9]*\\>" . font-lock-type-face)
          ) t)
   ;; add some c++-specific rotate-text keywords
   (setq rotate-text-local-symbols '(("class" "struct")))
   ) t)

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'boost-test)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(boost-test
                 "^[[:digit:]]+:\\s-*\\(.*\\):\\([[:digit:]]+\\):\\s-+\\(fatal\\s-\\)?error" 1 2)))


(defun find-my-tags-file() "Find tags file"
  (interactive)
  (let ((my-tags-file (find-file-upwards nil "TAGS")))
    (if my-tags-file
        (progn
          (message "Loading tags file: %s" my-tags-file)
          (run-with-timer 1 nil 'visit-tags-table my-tags-file))
      (message "Did not find tags file")
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;; c++11 enum class hack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: doesn't work
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a c++ \"enum class\"."
  (interactive)
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum\\s-+class\\s-+[^}]*")))) ;or end with '+'

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-                                ;or '0
    '+))

(defun fix-enum-class()
  "Setup c++-mode to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))
(add-hook 'c++-mode-hook 'fix-enum-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; c++11 lambda hack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued c++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; detect "[...](" or "[...]{" preceded by "," or "("
                     ;; and with unclosed brace
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ;no additional indent
          ad-do-it)))                   ;default behavior

;; coding.el ends here
