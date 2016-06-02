;;; multi-line-shared.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

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

;; multi-line-shared defines functions that are generally useful in
;; building multi-line strategies.

;;; Code:

(require 's)

(defun multi-line-clear-whitespace-at-point ()
  "Erase any surrounding whitespace."
  (interactive)
  (re-search-backward "[^[:space:]\n]")
  (forward-char)
  (let ((start (point)))
    (re-search-forward "[^[:space:]\n]")
    (backward-char)
    (kill-region start (point))))

(defun multi-line-add-trailing-comma (index markers)
  "Add a trailing comma when at the last marker.

INDEX is the index that will be used to determine whether or not
the action should be taken.  MARKERS is the list of markers that
were generated for the statement."
  (when (equal index (- (length markers) 1))
    (re-search-backward "[^[:space:]\n]")
    (when (not (looking-at ","))
      (forward-char)
      (insert ","))))

(defun multi-line-lparenthesis-advance ()
  "Advance to the beginning of a statement that can be multi-lined."
  (re-search-forward "[[{(]"))

(defun multi-line-up-list-back ()
  "Go to the beginning of a statement from inside the statement."
  (interactive)
  (let ((string-start (nth 8 (syntax-ppss))))
    (when string-start
      (goto-char string-start)))
  (up-list) (backward-sexp))

(defun multi-line-comma-advance ()
  "Advance to the next comma."
  (re-search-forward ","))

(defun multi-line-is-newline-between-markers (first second)
  (s-contains? "\n"
               (buffer-substring (marker-position first)
                                 (marker-position second))))

(defmacro multi-line-predicate-or (&rest predicates)
  `(lambda (&rest args)
       (or ,@(cl-loop for predicate in predicates
                      collect `(apply ,predicate args)))))

(defmacro multi-line-predicate-and (&rest predicates)
  `(lambda (&rest args)
       (and ,@(cl-loop for predicate in predicates
                      collect (quote (apply predicate args))))))

(defun multi-line-last-predicate (index markers)
  (equal index (- (length markers) 1)))

(defun multi-line-first-predicate (index markers)
  (equal index 0))

(defalias 'multi-line-first-or-last-predicate
  (multi-line-predicate-or 'multi-line-first-predicate
                           'multi-line-last-predicate))

(defun multi-line-is-last-index (index alist)
  (equal index (- (length markers) 1)))

(provide 'multi-line-shared)
;;; multi-line-shared.el ends here
