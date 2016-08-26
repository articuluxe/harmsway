;;; multi-line-respace.el --- multi-line statements -*- lexical-binding: t; -*-

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

;; multi-line-respace defines various generally applicable respace
;; strategies.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)

(require 'multi-line-cycle)

(defclass multi-line-respacer () nil)

(cl-defmethod multi-line-respace ((respacer multi-line-respacer) candidates
                                  &optional _context)
  (cl-loop for candidate being the elements of candidates using (index i) do
           (goto-char (multi-line-candidate-position candidate))
           (multi-line-respace-one respacer i candidates)))

(defclass multi-line-space (multi-line-respacer)
  ((spacer :initarg :spacer :initform " ")))

(cl-defmethod multi-line-respace-one ((respacer multi-line-space)
                                      _index _candidates)
  (when (not (multi-line-spacer-at-point respacer))
    (insert (oref respacer spacer))))

(cl-defmethod multi-line-spacer-at-point ((respacer multi-line-space))
  ;; TODO/XXX: This would cause problems with a spacer that was more than one
  ;; character long.
  (save-excursion (re-search-backward (format "[^%s]" (oref respacer spacer)))
                  (forward-char)
                  (looking-at (oref respacer spacer))))

(defclass multi-line-always-newline (multi-line-respacer) nil)

(cl-defmethod multi-line-respace-one ((_respacer multi-line-always-newline)
                                      _index _candidates)
  (newline-and-indent))

(defclass multi-line-fill-respacer (multi-line-respacer)
  ((newline-respacer
    :initarg :newline-respacer
    :initform (multi-line-always-newline))
   (sl-respacer
    :initarg :sl-respacer
    :initform (multi-line-never-newline))
   (first-index :initform 0 :initarg :first-index)
   (final-index :initform -1 :initarg :final-index)))

(cl-defmethod multi-line-should-newline ((respacer multi-line-fill-respacer)
                                         index candidates)
  (let ((candidates-length (length candidates)))
    (when  (<= (multi-line-first-index respacer candidates-length)
               index (multi-line-final-index respacer candidates-length))
      (multi-line-check-fill-column respacer index candidates))))

(cl-defmethod multi-line-first-index ((respacer multi-line-fill-respacer)
                                      candidates-length)
  (mod (oref respacer first-index) candidates-length))

(cl-defmethod multi-line-final-index ((respacer multi-line-fill-respacer)
                                      candidates-length)
  (mod (oref respacer final-index) candidates-length))

(cl-defmethod multi-line-check-fill-column ((respacer multi-line-fill-respacer)
                                            index candidates)
  (let* ((candidate-length (length candidates))
         (next-index (+ index 1))
         (final-index (multi-line-final-index respacer candidate-length))
         (next-candidate (nth next-index candidates))
         (next-candidate-column
          (save-excursion
            (cond ((or
                    ;; This is the last chance to respace, so we need to
                    ;; consider anything else that is on the current line.
                    (equal index final-index)
                    ;; There is a newline in between this marker and the next
                    ;; marker, so end-of-line is the relevant consideration.
                    (let ((this-marker (oref (nth index candidates) marker))
                          (next-marker (oref next-candidate marker)))
                      (multi-line-is-newline-between-markers this-marker
                                                             next-marker)))
                   (let ((inhibit-point-motion-hooks t))
                     (end-of-line)))
                  ((<= next-index final-index)
                   ;; We look at the next index because if IT exceeds the
                   ;; fill-column, we know we need to add a newline now.
                   (goto-char (multi-line-candidate-position next-candidate))))
            (current-column))))
    (> next-candidate-column (multi-line-get-fill-column respacer))))

(cl-defmethod multi-line-respace-one ((respacer multi-line-fill-respacer)
                                      index candidates)
  (let ((selected
         (if (multi-line-should-newline respacer index candidates)
             (oref respacer newline-respacer)
           (oref respacer sl-respacer))))
    (multi-line-respace-one selected index candidates)))

(defclass multi-line-fixed-fill-respacer (multi-line-fill-respacer)
  ((newline-at :initarg :newline-at :initform 80)))

(cl-defmethod multi-line-get-fill-column ((respacer multi-line-fixed-fill-respacer))
  (oref respacer newline-at))

(defclass multi-line-fill-column-respacer (multi-line-fill-respacer) nil)

(cl-defmethod multi-line-get-fill-column ((_r multi-line-fill-column-respacer))
  fill-column)

(defclass multi-line-removing-respacer nil
  ((skip-indices :initarg :skip-indices :initform '(0 -1))
   (respacer :initarg :respacer)))

(cl-defmethod multi-line-respace ((respacer multi-line-removing-respacer)
                                  candidates &optional context)
  (multi-line-respace
   (oref respacer respacer)
   (multi-line-remove-at-indices (oref respacer skip-indices) candidates)
   context))

(defclass multi-line-selecting-respacer nil
  ((indices-to-respacer :initarg :indices-to-respacer)
   (default :initarg :default :initform nil)))

(cl-defmethod multi-line-respace-one ((respacer multi-line-selecting-respacer)
                                      index candidates)
  (let ((selected (multi-line-select-respacer respacer index candidates)))
    (when selected
      (multi-line-respace-one selected index candidates))))

(cl-defmethod multi-line-select-respacer ((respacer multi-line-selecting-respacer)
                                          index candidates)
  (cl-loop for (indices . r) in (oref respacer indices-to-respacer)
           when
           (memq index (multi-line-actual-indices indices candidates))
           return r
           finally return (oref respacer default)))

(defun multi-line-never-newline ()
  (multi-line-selecting-respacer :default (multi-line-space)
                                 :indices-to-respacer (list (cons (list 0 -1) nil))))

(provide 'multi-line-respace)
;;; multi-line-respace.el ends here
