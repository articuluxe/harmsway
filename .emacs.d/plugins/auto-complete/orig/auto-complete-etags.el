;;; auto-complete-etags.el --- using etags with auto-complete

;; Copyright (C) 2014 Emanuele Tomasi <targzeta@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;; Author: Emanuele Tomasi <targzeta@gmail.com>
;; Version: 0.1
;; URL: https://gist.github.com/targzeta/10499354
;; Maintainer: Emanuele Tomasi <targzeta@gmail.com>
;; Keywords: auto-complete

;;; Commentary:

;; Source for Auto-Complete that uses etags.
;;
;; To use this program, copy this file in a directory which is in the Emacs
;; `load-path'. Then, execute the following code either directly or in your
;; .emacs file:
;;      (require 'auto-complete-etags)
;;      (setq-default ac-sources (add-to-list 'ac-sources 'ac-source-etags))

;;; Code:

(require 'auto-complete)

;; Customization
(defface ac-etags-candidate-face
  ;; '((t (:inherit ac-candidate-face :foreground "navy")))
  '((t (:inherit ac-gtags-candidate-face)))
  "Face for etags candidate"
  :group 'auto-complete)

(defface ac-etags-selection-face
  ;; '((t (:inherit ac-selection-face :background "navy")))
    '((t (:inherit ac-gtags-selection-face)))
  "Face for the etags selected candidate."
  :group 'auto-complete)

;; Functions
(defun ac-etags-candidates ()
  "Auto-completion source for etags"
  (when (and (boundp 'tags-table-list) tags-table-list)
    (all-completions
     ac-prefix
     (delq nil
           (mapcar
            (lambda (i) (if (symbolp i) (symbol-name i)))
            (tags-completion-table))))))

;; Adding the source
(ac-define-source etags
  '((candidates . ac-etags-candidates)
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (requires . 3)
    (symbol . "s")))

(provide 'auto-complete-etags)

;;; auto-complete-etags.el ends here
