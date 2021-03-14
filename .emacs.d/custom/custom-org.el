;;; custom-org.el --- custom org helpers
;; Copyright (C) 2021  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Sunday, March 14, 2021
;; Version: 1.0
;; Modified Time-stamp: <2021-03-14 09:11:49 dharms>
;; Modified by: Dan Harms
;; Keywords: emacs tools

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

(defun print-date-duration (beginning-date ending-date)
  "Pretty print the duration between BEGINNING-DATE and ENDING-DATE."
  (let* ((beg (org-date-to-gregorian beginning-date))
         (end (org-date-to-gregorian ending-date))
         (year (- (nth 2 end) (nth 2 beg)))
         (mth (- (nth 0 end) (nth 0 beg)))
         (day (- (nth 1 end) (nth 1 beg)))
         (dec-mth 0))
    (when (< day 0)
      (setq day (+ (nth 1 end)
                   (- (days-in-month (subtract-month (nth 0 end)))
                      (nth 1 beg))))
      (setq dec-mth 1))
    (when (< mth 0)
      (setq mth (+ (nth 0 end) (- 12 (nth 0 beg))))
      (setq year (1- year)))
    (setq mth (- mth dec-mth))
    (format "%d year%s, %d month%s, %d day%s"
            year (if (eq year 1) "" "s")
            mth (if (eq mth 1) "" "s")
            day (if (eq day 1) "" "s"))))

(defun days-in-month (mth)
  "Return the number of days in month MTH.
Leap years are not accounted for, at all, sir."
  (cond ((memq mth '(1 3 5 7 8 10 12)) 31)
        ((memq mth '(4 6 9 11)) 30)
        ((eq mth 2) 28)))

(defun subtract-month (mth)
  "Subtract the month MTH."
  (if (eq mth 1) 12 (1- mth)))

(provide 'custom-org)
;;; custom-org.el ends here
