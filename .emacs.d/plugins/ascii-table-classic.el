;;; ascii-table-classic.el --- show an ascii table
;; Copyright (C) 2016, 2020  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Monday, September 12, 2016
;; Version: 1.0
;; Modified Time-stamp: <2020-04-01 13:39:24 dharms>
;; Modified by: Dan Harms
;; Keywords: ascii

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

;;;###autoload
(defun ascii-table-classic ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (let ((lower32 '("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
                   "bs" "ht" "nl" "vt" "np" "cr" "so" "si"
                   "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
                   "can" "em" "sub" "esc" "fs" "gs" "rs" "us"
                   )))
    (switch-to-buffer "*ASCII*")
    (erase-buffer)
    (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
    (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
    (save-excursion
      (let ((i -1))
        (insert "ASCII characters 0 thru 127.\n\n")
        (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
        (while (< i 31)
          (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                          (setq i (+ 1  i)) i (elt lower32 i)
                          (setq i (+ 32 i)) i (single-key-description i)
                          (setq i (+ 32 i)) i (single-key-description i)
                          (setq i (+ 32 i)) i (single-key-description i)))
          (setq i (- i 96)))))))

(provide 'ascii-table-classic)
;;; ascii-table-classic.el ends here
