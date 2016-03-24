;;; c-includer.el --- c-style include statement utilities
;; Copyright (C) 2015, 2016  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Wednesday, December 30, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-03-24 14:31:48 dan.harms>
;; Modified by: Dan Harms
;; Keywords: c++ coding

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

(require 'makey)
(setq makey-key-mode-show-usage nil)

(defvar c-includer-brackets-kind 'bracket
  "The kind of replacement to do for the `#include' statements.")
(defvar c-includer-bracket-key ?.)
(defvar c-includer-quote-key ?')
(defvar c-includer-toggle-key ?t)
(defvar c-includer-guess-key ?g)

;; (defvar lisp-val nil)
;; (defvar ooka nil)

(defun c-includer-brackets (begin end)
  "Adjust the quotes or brackets around `#include' statements.
Operates on region, if BEGIN and END are given, or the whole buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  ;; (message "lisp value is %s, normal switch is %s" lisp-val ooka)
  (save-restriction
    (narrow-to-region (or begin (point-min))
                      (or end (point-max)))
    (goto-char (point-min))
    (let ((kind c-includer-brackets-kind)
          res)
      (catch 'break
        (while (re-search-forward
                "#\\s-*include\\s-+\\(\"\\|<\\)\\(.+?\\)\\(\"\\|>\\)"
                nil t)
          (when (eq c-includer-brackets-kind 'query)
            ;; query user for command
            (set-mark (match-beginning 0))
            (save-restriction
              (save-match-data
                ;; show entire buffer during query
                (widen)
                (setq res (read-char-choice
                           (concat "Change to ("
                                   (make-string 1 c-includer-bracket-key)
                                   ") <bracket>  ("
                                   (make-string 1 c-includer-quote-key)
                                   ") \"quote\"  ("
                                   (make-string 1 c-includer-toggle-key)
                                   ") toggle  ("
                                   (make-string 1 c-includer-guess-key)
                                   ") guess  (s) skip"
                                   )
                           `(,c-includer-bracket-key
                             ,c-includer-quote-key
                             ,c-includer-toggle-key
                             ,c-includer-guess-key
                             ?s
                             )))))
            (cond ((eq res c-includer-bracket-key)
                   (setq kind 'bracket))
                  ((eq res c-includer-quote-key)
                   (setq kind 'quote))
                  ((eq res c-includer-toggle-key)
                   (setq kind 'toggle))
                  ((eq res c-includer-guess-key)
                   (setq kind 'guess))
                  ((eq res ?s)
                   (setq kind nil))
                  (t (throw 'break nil)))
            (message "")                ; clear minibuffer
            )
          ;; adjust this match
          (cond ((or
                  (eq kind 'bracket)
                  (and (eq kind 'toggle)
                       (save-match-data (string-match "\"" (match-string 1))))
                  (and (eq kind 'guess)
                       (save-match-data (string-match "/" (match-string 2))))
                  )
                 (replace-match "<" t nil nil 1)
                 (replace-match ">" t nil nil 3))
                ((or
                  (eq kind 'quote)
                  (and (eq kind 'toggle)
                       (save-match-data (string-match "<" (match-string 1))))
                  (and (eq kind 'guess)
                       (save-match-data (not (string-match "/" (match-string 2)))))
                  )
                 (replace-match "\"" t nil nil 1)
                 (replace-match "\"" t nil nil 3))
                )
          (deactivate-mark)             ; don't want active region when done
          )))))


(makey-initialize-key-groups
 `((c-includer-brackets
    (description "Adjust quotes or brackets around #include statements.")
    (actions
     ("Include Statements"
      (,(make-string 1 c-includer-bracket-key)
       "Change to brackets" toolbox--change-to-brackets)
      (,(make-string 1 c-includer-quote-key)
       "Change to quotes" toolbox--change-to-quotes)
      (,(make-string 1 c-includer-toggle-key)
       "Toggle current" toolbox--toggle-quotes)
      (,(make-string 1 c-includer-guess-key)
       "Make best guess" toolbox--guess-quotes)
      ("?" "Query user" toolbox--query-quotes)
      ))
    ;; (lisp-switches
    ;;  ("-w" "lisp switch" lisp-val t nil))
    ;; (switches ("-r" "normal switch" ooka))
    ;; (lisp-arguments
    ;;  ("=l" "lisp arg"
    ;;   "lisp-val"
    ;;   (lambda (dummy) (interactive) (read-string "Arg is: "))))
    )))

(defun toolbox--change-to-brackets()
  "Change to brackets."
  (interactive)
  (setq c-includer-brackets-kind 'bracket)
  (if (use-region-p)
      (c-includer-brackets
       (region-beginning) (region-end))
    (c-includer-brackets nil nil)))

(defun toolbox--change-to-quotes()
  "Change to quotes."
  (interactive)
  (setq c-includer-brackets-kind 'quote)
  (if (use-region-p)
      (c-includer-brackets
       (region-beginning) (region-end))
    (c-includer-brackets nil nil)))

(defun toolbox--toggle-quotes()
  "Toggle between quotes and brackets."
  (interactive)
  (setq c-includer-brackets-kind 'toggle)
  (if (use-region-p)
      (c-includer-brackets
       (region-beginning) (region-end))
    (c-includer-brackets nil nil)))

(defun toolbox--guess-quotes()
  "Make a best guess between quotes and brackets."
  (interactive)
  (setq c-includer-brackets-kind 'guess)
  (if (use-region-p)
      (c-includer-brackets
       (region-beginning) (region-end))
    (c-includer-brackets nil nil)))

(defun toolbox--query-quotes()
  "Query the user to choose between quotes and brackets."
  (interactive)
  (setq c-includer-brackets-kind 'query)
  (if (use-region-p)
      (c-includer-brackets
       (region-beginning) (region-end))
    (c-includer-brackets nil nil)))

(provide 'c-includer)

;;; c-includer ends here
