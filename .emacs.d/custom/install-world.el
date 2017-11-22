;;; install-world.el --- utils to install harmsway
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, November 22, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-11-22 17:20:49 dharms>
;; Modified by: Dan Harms
;; Keywords: tools

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
;; Provides utilities to install harmsway.
;;

;;; Code:
;;;###autoload
(defun harmsway/install-world ()
  "Install the harmsway world."
  (interactive)
  (let* ((buf (get-buffer-create " *Install-World*"))
         proc)
    (setq proc (start-process "install-world" buf
                              "sh" "-c" "install-world.sh"))
    (with-current-buffer (pop-to-buffer buf)
      (read-only-mode 1))
    (set-process-sentinel proc #'harmsway/install-world-sentinel)))

(defun harmsway/install-world-sentinel (proc change)
  "A process sentinel to track the state of PROC, via CHANGE."
  (when (string-match-p "\\(finished\\|exited\\)" change)
    (message "install-world.sh finished.")))

(provide 'install-world)
;;; install-world.el ends here
