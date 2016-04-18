;;; remote-host-connector.el --- manages connections to remote hosts
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Monday, April 18, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-04-18 17:46:11 dharms>
;; Modified by: Dan Harms
;; Keywords: remote hosts

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

(defvar my/remote-hosts-file ""
  "Location of remote hosts file.")
(defvar my/remote-host-list '()
  "List of remote hosts.")
(defvar my/remote-host--inited nil
  "True if the remote hosts file been read in yet.")

(defun my/connect-to-remote-host(&optional arg)
  "Connect to a remote host from `my/remote-host-list'."
  (interactive "P")
  (and (not my/remote-host--inited)
       (file-exists-p my/remote-hosts-file)
       (load my/remote-hosts-file t)
       (setq my/remote-host--inited t))
  (let ((hosts
         (mapcar (lambda (plist)
                   (let* ((delim (char-to-string ?:))
                          (stem (plist-get plist :host))
                          (user (or (unless arg
                                      (plist-get plist :user))
                                    my/user-name))
                          (pwd (plist-get plist :password))
                          (desc (plist-get plist :description))
                          (category (plist-get plist :category))
                          (display (concat user delim stem))
                          (connect (concat
                                    "/"
                                    tramp-default-method
                                    ":" user "@" stem ":~")))
                     (when category
                       (setq display
                             (concat category delim display)))
                     (when desc
                       (setq display
                             (concat display delim desc)))
                     (and stem (cons display connect))))
                 my/remote-host-list))
        result cell)
    (setq result
          (funcall my/choose-func
                   (mapcar 'car hosts)
                   "Remote host: "))
    (when result
      (setq cell (assoc result hosts))
      (find-file (cdr cell)))))

(provide 'remote-host-connector)
;;; remote-host-connector.el ends here
