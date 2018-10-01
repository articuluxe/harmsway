;;; delim-cols+.el --- wrapper around delimit-region
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, October  1, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-10-01 07:05:18 dharms>
;; Modified by: Dan Harms
;; Keywords:

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
;; Wrapper around delimit-columns-region.
;; Taken from `https://emacsnotes.wordpress.com/2018/09/24/delim-col-a-handy-tool-for-creating-pretty-tables-and-converting-those-to-different-table-formats/'
;;

;;; Code:

(defun my-delimits-column-region
    (orig-fun &rest args)
  (let
      ((delimit-columns-separator
        (read-regexp
         (format "%s (%s): " "Specify the regexp which separates each column" delimit-columns-separator)
         (list delimit-columns-separator)))
       (delimit-columns-before
        (read-string
         (format "%s (%s): " "Specify a string to be inserted before each column" delimit-columns-before)
         nil nil delimit-columns-before))
       (delimit-columns-after
        (read-string
         (format "%s (%s): " "Specify a string to be inserted after each column" delimit-columns-after)
         nil nil delimit-columns-after))
       (delimit-columns-str-separator
        (read-string
         (format "%s (%s): " "Specify a string to be inserted between each column" delimit-columns-str-separator)
         nil nil delimit-columns-str-separator))
       (delimit-columns-str-before
        (read-string
         (format "%s (%s): " "Specify a string to be inserted before the first column" delimit-columns-str-before)
         nil nil delimit-columns-str-before))
       (delimit-columns-str-after
        (read-string
         (format "%s (%s): " "Specify a string to be inserted after the last column" delimit-columns-str-after)
         nil nil delimit-columns-str-after))
       (delimit-columns-format
        (let*
            ((choices
              '(("Align Columns" . t)
                ("No Formatting")
                ("Align Separators" . separator)
                ("Pad Columns" . padding)))
             (default-choice
               (car
                (rassoc delimit-columns-format choices)))
             (choice
              (completing-read
               (format "%s (%s): " "Specify how to format columns" default-choice)
               choices nil t nil nil default-choice)))
          (message "%s" choice)
          (assoc-default choice choices))))
    (apply orig-fun args)))

(advice-add 'delimit-columns-region :around #'my-delimits-column-region)
(advice-add 'delimit-columns-rectangle :around #'my-delimits-column-region)

(provide 'delim-cols+)
;;; delim-cols+.el ends here
