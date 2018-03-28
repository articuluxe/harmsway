;;; choose-font.el --- choose among a list of fonts
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, March 28, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-03-28 17:46:02 dharms>
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

(defvar choose-font-list '()
  "A list of fonts among which to choose.")

(defun choose-font-activate (font)
  "Activate a font specified by FONT."
  (interactive)
  (set-frame-font font nil t))

;;;###autoload
(defun choose-font ()
  "Choose among a set of fonts defined in `choose-font-list'."
  (interactive)
  (let ((font (completing-read "Font: " choose-font-list nil nil
                               (car choose-font-list))))
    (if font
        (choose-font-activate font)
      ;; else: todo
      )))

(provide 'choose-font)
;;; choose-font.el ends here
