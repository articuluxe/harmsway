;;; remotehost-connect.el --- manages connections to remote hosts
;; Copyright (C) 2016-2019  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Monday, April 18, 2016
;; Version: 1.0
;; Modified Time-stamp: <2019-06-05 08:46:31 dharms>
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

(require 'subr-x)
(require 'tramp)
(require 'ivy)
(require 'read-file)
(require 'cl-lib)

(defvar remotehost-connect-hosts '()
  "List of hosts to connect to.")
(defvar remotehost-connect-history nil
  "History for `remotehost-connect'.")

(defun remotehost-connect--derive-remote-name (plist)
  "Derive the remote file name implied by the settings in PLIST."
  (format "/%s:%s@%s:%s"
          (plist-get plist :method)
          (plist-get plist :user)
          (plist-get plist :host)
          (plist-get plist :dir)))

(defun remotehost-connect--find-file (plist)
  "Opens a remote host location, whose properties are contained in PLIST."
  (let ((file (remotehost-connect--derive-remote-name plist)))
    (find-file file)
    ))

;;;###autoload
(defun remotehost-connect-read-file (file)
  "Read the contents of FILE into `remotehost-connect-hosts'.
Eacn line should start with the host name, then
followed by an optional description, separated by whitespace."
  (interactive "FFile: ")
  (let ((lines (read-file-into-lines file 'trim))
        (lst '())
        host desc elts)
    (dolist (line (seq-remove 'string-empty-p lines))
      (setq elts (split-string line))
      (setq host (car elts))
      (setq desc (string-join (cdr elts) " "))
      (unless (char-equal ?# (string-to-char host))
        (push `(:host ,host :description ,desc) lst)))
    lst))

;;;###autoload
(defun remotehost-connect (&optional user dir)
  "Connect to a remote host from `remotehost-connect-hosts'.
Optional argument USER allows overriding the remote user,
when called interactively with a prefix argument.
Optional argument DIR allows overriding the remote directory,
when called interactively with a prefix argument."
  (interactive (list (if current-prefix-arg
                         (read-string "User: " tramp-default-user) nil)
                     (if current-prefix-arg
                         (read-string "Dir: " "~"))))
  (let ((method tramp-default-method)
        hosts host desc disp conn)
    (setq hosts
          (mapcar (lambda (plist)
                    (setq host (plist-get plist :host))
                    (when host
                      (setq desc (plist-get plist :description))
                      (setq disp (format "%-18s %s" host desc))
                      (cons disp
                            (list :method method
                                  :user (or user tramp-default-user)
                                  :host host
                                  :dir (or dir "~")
                                  ))))
                  remotehost-connect-hosts))
    (setq hosts (sort hosts (lambda (left right)
                              (string-lessp (car left) (car right)))))
    (setq hosts (cl-remove-duplicates hosts :test (lambda (x y)
                                                    (equal (car x)
                                                           (car y)))))
    (ivy-read "Remote host: " hosts
              :history 'remotehost-connect-history
              :action (lambda (x)
                        (remotehost-connect--find-file (cdr x)))
              :caller 'remotehost-connect
              )))

(defun remotehost-connect-change-user (x)
  "Read input to specify the user to use in `remotehost-connect'.
X is a plist containing the connection properties."
  (interactive)
  (let ((plist (cdr x))
        user)
    (setq user (read-string "User: " (plist-get plist :user)))
    (when user
      (plist-put plist :user user)
      (remotehost-connect--find-file plist))))

(defun remotehost-connect-change-dir (x)
  "Read input to specify the directory to open via `remotehost-connect'.
X is a plist containing the connection properties."
  (interactive)
  (let ((plist (cdr x))
        dir)
    (setq dir (read-directory-name "Remote dir: " "~"))
    (when dir
      (plist-put plist :dir dir)
      (remotehost-connect--find-file plist))))

(ivy-add-actions 'remotehost-connect
                 '(("u" remotehost-connect-change-user "user")
                   ("d" remotehost-connect-change-dir "directory")))

(provide 'remotehost-connect)
;;; remotehost-connect.el ends here
