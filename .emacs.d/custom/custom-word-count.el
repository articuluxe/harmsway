;;; custom-word-count.el --- a custom word-count helper
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Thursday, April 14, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-04-14 17:51:33 dharms>
;; Modified by: Dan Harms
;; Keywords: word-count text

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

;; word count (superceded by 'count-words-region "C-u M-=" in recent emacsen)
(defun wordcount () "print buffer word count in minibuffer" (interactive)
       (save-excursion
         (let ((count 0))
           (goto-char (point-min))
           (while (< (point) (point-max))
             (forward-word 1)
             (setq count (1+ count)))
           (message "buffer contains %d words" count))))

(provide 'custom-word-count)
;;; custom-word-count.el ends here
