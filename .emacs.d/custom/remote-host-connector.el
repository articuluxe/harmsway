;;; remote-host-connector.el --- manages connections to remote hosts
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Monday, April 18, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-07-08 17:05:29 dan.harms>
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

(require 'tramp)
(require 'custom-completion)

(defvar my/remote-hosts-file ""
  "Location of remote hosts file.")
(defvar my/remote-host-list '()
  "List of remote hosts.")

(when (file-exists-p my/remote-hosts-file)
  (load my/remote-hosts-file t))

(defun my/connect-to-remote-host(&optional arg)
  "Connect to a remote host from `my/remote-host-list'."
  (interactive "P")
  (let ((delim (char-to-string ?:))
        stem user desc cat disp conn hosts result cell)
    (setq hosts
          (mapcar (lambda (plist)
                    (setq stem (plist-get plist :host))
                    (setq desc (plist-get plist :description))
                    (setq cat (plist-get plist :category))
                    (setq user (or (unless arg
                                     (plist-get plist :user))
                                   my/user-name))
                    (setq disp (concat user delim stem))
                    (setq conn
                          (concat "/" tramp-default-method
                                  ":" user "@" stem ":~"))
                  (when cat
                    (setq disp
                          (concat cat delim disp)))
                  (when desc
                    (setq disp
                          (concat disp delim desc)))
                  (and stem (cons disp conn)))
                  my/remote-host-list))
    (setq result
          (funcall my/choose-func
                   (mapcar 'car hosts)
                   "Remote host: "))
    (when result
      (setq cell (assoc result hosts))
      (find-file (cdr cell)))))

(provide 'remote-host-connector)
;;; remote-host-connector.el ends here
