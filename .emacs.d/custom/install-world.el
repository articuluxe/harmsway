;;; install-world.el --- utils to install harmsway
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, November 22, 2017
;; Modified Time-stamp: <2018-04-13 17:45:26 dharms>
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
(require 's)

(defvar harmsway-install-cmd-alist '("install-world.sh")
  "List of commands to be invoked to install harmsway.")

(defvar harmsway-install--last-cmd nil "Last command run.")

;;; Code:
;;;###autoload
(defun harmsway/install-world ()
  "Install the harmsway world."
  (interactive)
  (let* ((cmd (cond ((eq (length harmsway-install-cmd-alist) 1)
                     (car harmsway-install-cmd-alist))
                    (t (completing-read "Command: " harmsway-install-cmd-alist))))
         (bufname (concat " *" (s-upcase (file-name-base cmd)) "*"))
         (buf (get-buffer-create bufname))
         proc)
    (setq harmsway-install--last-cmd cmd)
    (setq proc (start-process cmd buf
                              "bash" "--rcfile" "~/.bashrc" "-ci"
                              cmd))
    (with-current-buffer (pop-to-buffer buf)
      (setq-local window-point-insertion-type t)
      (let ((map (make-sparse-keymap)))
        (define-key map "q" #'quit-window)
        (set-keymap-parent map (current-local-map))
        (use-local-map map)))
    (set-process-sentinel proc #'harmsway/install-world-sentinel)))

(defun harmsway/install-world-sentinel (proc change)
  "A process sentinel to track the state of PROC, via CHANGE."
  (when (string-match-p "\\(finished\\|exited\\)" change)
    (message "%s finished." harmsway-install--last-cmd)))

(provide 'install-world)
;;; install-world.el ends here
