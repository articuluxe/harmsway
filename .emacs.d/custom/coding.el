;; coding.el --- coding utilities
;; Copyright (C) 2015-2023, 2025  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Modified Time-stamp: <2025-04-01 16:25:15 dharms>
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

(require 'use-package)
(require 'proviso-clang)

(defvar harmsway-c-mode-init-hooks '()
  "Hooks run once as c-mode-common is initialized.")
(defvar harmsway-c-mode-common-hooks '()
  "Hooks run per c-mode-common buffer.")

(global-set-key "\C-c\C-c\C-c" #'comment-region)
(global-set-key "\C-c\C-c\C-u" #'uncomment-region)

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

;; default offsets
(c-set-offset 'case-label            '+)
(c-set-offset 'arglist-cont          'c-lineup-ternary-bodies)
(c-set-offset 'arglist-cont-nonempty '(c-lineup-ternary-bodies +))
(c-set-offset 'statement-cont        '(c-lineup-ternary-bodies +))
(c-set-offset 'innamespace           '0)
(c-set-offset 'substatement-open     '0)
(c-set-offset 'inline-open           '0)
(c-set-offset 'statement-case-intro  '+)
(c-set-offset 'statement-case-open   '+)
(c-set-offset 'access-label          '-2)
(c-set-offset 'comment-intro         'c-lineup-comment)
(c-set-offset 'member-init-intro     '+)
(c-set-offset 'comment-intro         '0)

;; default cleanups
(setq c-cleanup-list
      '(empty-defun-braces
        defun-close-semi
        one-liner-defun
        scope-operator
        list-close-comma
        compact-empty-funcall
        comment-close-slash
        ))

(defconst harmsway-c-style
  '((c-basic-offset . 3)
    (c-electric-pound-behavior . (alignleft))
    (c-hanging-braces-alist . ((brace-list-open)
                               (brace-entry-open)
                               (statement-cont)
                               (substatement-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (namespace-open after)
                               (module-open after)
                               (composition-open after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               (arglist-cont-nonempty)
                               (inline-close)))
    (c-hanging-colons-alist . ((member-init-intro before)
                               (inher-intro)
                               (case-label after)
                               (label after)
                               (access-label after)))
    (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-before-nonblanks
                                      c-semi&comma-no-newlines-for-oneline-inliners
                                      c-semi&comma-inside-parenlist))
    (c-doc-comment-style       ((java-mode . javadoc)
                                (c-mode . doxygen)
                                (c++-mode . doxygen)))
    )
  "The default harmsway c style.")

;; Init hook
(defun harmsway-c-init-fn ()
  "Initialization common to all c-modes, run once when loaded."
  (c-add-style "harmsway" harmsway-c-style)
  (setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "harmsway")))
  (require 'compile)
  (setq-default c-auto-newline t)
  (define-key c++-mode-map "\C-c\C-c" nil)
  (define-key c++-mode-map "\C-c/" 'toggle-c-comment-delimiters)
  (define-key c++-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (define-key c-mode-base-map (kbd "C-S-o") 'c-context-open-line)
  (define-key c-mode-base-map (kbd "C-c C-;")
    (lambda() (interactive) (c-try-one-liner)))
  (define-key c-mode-base-map "\C-c." nil)
  (define-key c-mode-base-map (kbd "TAB") #'company-indent-or-complete-common)
  ;; skips the final included file, ending in `:', when traversing compile
  ;; errors.  See
  ;; `http://stackoverflow.com/questions/15489319/how-can-i-skip-in-file-included-from-in-emacs-c-compilation-mode'
  ;; (setf (nth 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  )

(add-hook 'c-initialization-hook #'harmsway-c-init-fn -50)

;; C-Mode common hook
(defun harmsway-c-mode-common-fn ()
  "Common initialization for `c-mode-common-hook'."
  (setq c-tab-always-indent nil)
  (setq c-insert-tab-function 'indent-for-tab-command)
  ;; handle CamelCase
  (if (version< emacs-version "24.3")
      (c-subword-mode 1)
    (subword-mode 1))
  (c-toggle-hungry-state t)
  (setq comment-start "/*") (setq comment-end "*/")
  ;; (setq comment-column 40)
  (setq hide-ifdef-initially t)
  (setq hide-ifdef-lines nil)
  (setq hide-ifdef-shadow nil)
  (hide-ifdef-mode 1)
  (let* ((exe (harmsway-lookup-language-server 'cpp))
         (file (proviso-get (proviso-current-project) :compile-db)))
    (if (and
         exe
         (executable-find exe)
         file
         (file-exists-p file))
      (progn
        (eglot-ensure)
        (setq-local company-backends
                    (list
                     (append
                      (list 'company-c-headers 'company-c-preprocessor)
                      (copy-tree
                       (remq 'company-capf (car company-backends)))))))
      (require 'flymake-collection-clang)
      (flymake-mode 1)
      (setq-local company-alt-backend 'company-clang)
      (setq-local company-backends
                  (list
                   (append
                    (list 'company-c-headers 'company-c-preprocessor)
                    (copy-tree
                     (car company-backends))))))))

(add-hook 'c-mode-common-hook #'harmsway-c-mode-common-fn -50)

;; Prog mode hook
(add-hook 'prog-mode-hook
          (lambda()
            (font-lock-add-keywords
             nil '(
                   ;; TODO declarations
                   ("\\_<[tT][oO][dD][oO]\\_>" 0 font-lock-warning-face t)
                   ;; FIXME
                   ("\\_<[fF][iI][xX][mM][eE]\\_>" 0 font-lock-warning-face t)
                   ) t)
            ))

(when (< emacs-major-version 25)
  (with-eval-after-load 'cc-mode (require 'modern-cpp-font-lock)))

;; C++ mode hook
(defun harmsway-c++-mode-fn ()
  (when (< emacs-major-version 25)
    (modern-c++-font-lock-mode 1))
  (font-lock-add-keywords
   nil '(;; complete some fundamental keywords (+ Qt)
         ;; add C++11 keywords still missing from the defaults for emacs 25
         ("\\<\\(alignas\\|static_assert\\)\\>" . font-lock-keyword-face)
         ;; Qt fontification
         ("\\<\\(Q_OBJECT\\|SIGNAL\\|SLOT\\|slots\\|signals\\)\\>" . font-lock-keyword-face)
         ("\\<QT?\\(_\\sw+\\)+\\>" . font-lock-keyword-face)
         ;; This is fairly aggressive; can reenable if desired
         ;; ("\\<Q[A-Z][A-Za-z0-9]*\\>" . font-lock-type-face)
         ) t)
  ;; disable <> electricity for now.  Need to be smarter.
  ;; (make-local-variable 'electric-pair-pairs)
  ;; (push (cons ?< ?>) electric-pair-pairs)
  ;; add some c++-specific rotate-text keywords
  (add-to-list 'c-noise-macro-names "constexpr")
  (setq rotate-text-local-symbols '(("class" "struct")
                                    ("true" "false")
                                    ("public" "protected" "private")
                                    )))
(add-hook 'c++-mode-hook #'harmsway-c++-mode-fn -50)


(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist 'boost-test)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(boost-test
                 "^[[:digit:]]+:\\s-*\\(.*\\):\\([[:digit:]]+\\):\\s-+\\(fatal\\s-\\)?error" 1 2)))

;;; coding.el ends here
