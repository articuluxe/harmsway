;;; choose-font.el --- choose among a list of fonts
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, March 28, 2018
;; Modified Time-stamp: <2018-03-30 17:02:09 dharms>
;; Modified by: Dan Harms
;; Keywords: font

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
;; A simple font selection mechanism.
;;

;;; Code:
(require 'subr-x)
(require 'ivy)
(require 'read-file)

(defvar choose-font-list '()
  "A list of fonts among which to choose.")
(defvar choose-font-history nil
  "History for `choose-font'.")
(defvar choose-font-user-file "~/.emacs_fonts"
  "A config file used to configure `choose-font'.")

(defun choose-font-read-init-file ()
  "Read the file `choose-font-user-file' if it exists.
Every line is a font to add to `choose-font-list'.  The first
line is given priority as the preferred font to activate."
  (interactive)
  (let* ((file (expand-file-name choose-font-user-file))
         lines first)
    (when (file-exists-p file)
      (setq lines (seq-remove
                   'string-empty-p
                   (read-file-transform
                    (read-file-into-lines file)
                    'read-file-strip-hash-comment
                    'string-trim)))
      (dolist (line lines)
        (unless (string-empty-p line)
          (add-to-list 'choose-font-list line)))
      (setq first (car lines))
      (when first
        (setq choose-font-list
              (cons first (remove first choose-font-list)))))))

(defun choose-font-activate (font)
  "Activate a font specified by FONT."
  (interactive)
  (set-frame-font font nil t))

;;;###autoload
(defun choose-font (&optional font)
  "Set the font to FONT.
With a prefix argument, choose among a set of fonts defined in
`choose-font-list'."
  (interactive (list (when current-prefix-arg
                       (read-string "Font: "))))
  (if font
      (choose-font-activate font)
    (choose-font-read-init-file)
    (ivy-read "Font: "
              choose-font-list
              :history choose-font-history
              :action (lambda (x)
                        (choose-font-activate x))
              :caller 'choose-font
              )))

(defun choose-font-edit-font (x)
  "Read input to edit the font X to use in `choose-font'."
  (interactive)
  (let ((font))
    (setq font (read-string "Font: " x))
    (when font
      (choose-font-activate font))))

(ivy-add-actions 'choose-font
                 '(("e" choose-font-edit-font "edit")))

(provide 'choose-font)
;;; choose-font.el ends here
