;; -*- Mode: Emacs-Lisp -*-
;; imenu-scan.el --- scan buffer for imenu results
;; Copyright (C) 2015  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Thursday, October  1, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-10-02 14:08:18 dan.harms>
;; Modified by: Dan Harms
;; Keywords: imenu font-lock programming

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

(defvar-local imenu-scan-results '()
  "list of results")

(defun imenu-scan-buffer-properties (face)
  (let ((pt (point-min))
        (res '())
        next prop marker)
    (while pt
      (setq next (next-single-property-change pt 'face))
      (setq prop (get-text-property pt 'face))
      (when (eq prop face)
        (setq marker (make-marker))
        (set-marker marker pt)
        (setq res
              (cons
               (cons (buffer-substring-no-properties pt next)
                     marker) res)))
      (setq pt next))
    res))

(provide 'imenu-scan)

(defun imenu-scan-create-index ()
  (interactive)
  (imenu-scan-buffer-properties 'font-lock-function-name-face))

(add-hook 'c-mode-common-hook
          (lambda()
            (setq imenu-generic-expression nil)
            (setq imenu-create-index-function 'imenu-scan-create-index)
            ) t)

(global-set-key "\C-c0i" 'imenu-scan-create-index)

;; imenu-scan.el ends here
