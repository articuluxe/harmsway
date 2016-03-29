;;; file-line-processor.el --- process a file in line-oriented fashion
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Tuesday, March 29, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-03-29 17:51:46 dharms>
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

;;; Commentary: Originally from Alin Soare, cf:
;; https://lists.gnu.org/archive/html/emacs-devel/2011-05/msg00752.html

;;

;;; Code:

(defun map-file-lines (file func &optional startline count bufsize)
  "Process file FILE line by line by calling FUNC.
Optionally start at STARTLINE, executing up to COUNT lines, in a
buffer up to size BUFSIZE,"
  (interactive)
  (let ((filepos 0)
        (linenum 0)
        (bufsize (or bufsize (* 1024 128))))
    (with-temp-buffer
      (while
          (let* ((inserted
                  (insert-file-contents file nil filepos
                                        (+ filepos bufsize) t))
                 (numlines (count-lines (point-min) (point-max)))
                 (read (nth 1 inserted))
                 (done (< 1 read))
                 result line-end)
            (while (not (zerop (decf numlines)))
              (goto-char (point-min))
              (setq line-end (line-end-position))
              (setq result
                    (if (and startline (< linenum startline))
                        ()
                      (if (and count
                               (>= (- linenum startline) count))
                          (return)
                        (funcall
                         func
                         (buffer-substring (line-beginning-position)
                                           line-end)
                         linenum))))
              (setq done (and done result))
              (incf filepos line-end)
              (forward-line)
              (incf linenum))
            done)))
    linenum))

;;; file-line-processor.el ends here
