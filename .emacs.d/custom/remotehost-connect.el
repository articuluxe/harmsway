;;; remotehost-connect.el --- Manages connections to remote hosts
;; Copyright (C) 2016-2020  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Monday, April 18, 2016
;; Modified Time-stamp: <2020-05-02 09:08:32 dharms>
;; Modified by: Dan Harms
;; Keywords: tools remote hosts

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
;; Provides an interface to connect to remote hosts.
;;

;;; Code:

(require 'subr-x)
(require 'tramp)
(require 'ivy)
(require 'read-file)
(require 'cl-lib)
(require 'subr-x)

(defvar remotehost-connect-hosts '()
  "List of hosts to connect to.")
(defvar remotehost-connect-history nil
  "History for `remotehost-connect'.")

(defun remotehost-connect--derive-remote-name (method user host dir
                                                      &optional hops)
  "Derive the remote file name defined by METHOD, USER, HOST, DIR and HOPS."
  (let ((str (format "/%s:%s@%s" method user host)))
    (dolist (hop hops)
      (setq str (concat str "|" hop)))
    (concat str (format ":%s" dir))))

(defun remotehost-connect--derive-remote-name-plist (plist
                                                     &optional method user dir)
  "Derive the remote file name implied by the settings in PLIST.
METHOD, USER and DIR are optional overrides to those settings
from PLIST."
  (remotehost-connect--derive-remote-name
   (or method (plist-get plist :method) tramp-default-method)
   (or user (plist-get plist :user) tramp-default-user)
   (plist-get plist :host)
   (or dir (plist-get plist :dir) "~")
   (plist-get plist :hops)))

(defun remotehost-connect--find-file (target &optional method user dir)
  "Opens a remote host location TARGET.
METHOD, USER and DIR are optional overrides to any values
contained in TARGET.  TARGET may be a plist containing connection
properties, or a host name."
  (let (file)
    (cond ((listp target)
           (setq file (remotehost-connect--derive-remote-name-plist
                       target method user dir))
           (find-file file))
          ((stringp target)
           (setq file (remotehost-connect--derive-remote-name
                       (or method tramp-default-method)
                       (or user tramp-default-user)
                       target
                       (or dir "~")))
           (find-file file))
          (t
           (user-error "Unknown target for remotehost-connect: %s" target)))))

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

(defun remotehost-connect--gather-hosts (method user dir
                                                &optional hops)
  "Gather hosts from `remotehost-connect-hosts'.
METHOD, USER and DIR describe the remote file, as in tramp.
Optional HOPS is a list of additional hops to add to the result.
A plist is returned that describes the result."
  (let (host)
    (cl-remove-duplicates
     (sort
      (mapcar (lambda (plist)
                (when (setq host (plist-get plist :host))
                  (cons (format "%-18s %s" host
                                (plist-get plist :description))
                        (list :method method
                              :user user
                              :host host
                              :dir dir
                              :hops hops
                              ))))
              remotehost-connect-hosts)
      (lambda (left right) (string-lessp (car left) (car right))))
     :test (lambda (x y) (equal (car x) (car y))))))

;;;###autoload
(defun remotehost-connect (&optional arg)
  "Connect to a remote host from `remotehost-connect-hosts'.
Optional argument ARG allows overriding the default values for
 method, user and starting directory."
  (interactive "P")
  (let* ((method tramp-default-method)
         (user tramp-default-user)
         (dir "~")
         (numhops 1)
         hosts hop hops)
    (when arg
      (when (>= (prefix-numeric-value arg) 16)
        (setq method (read-string "Method: " method)))
      (when (>= (prefix-numeric-value arg) 4)
        (setq user (read-string "User: " user)))
      (when (>= (prefix-numeric-value arg) 16)
        (setq dir (read-string "Directory: " dir)))
      (when (>= (prefix-numeric-value arg) 64)
        (setq hop (read-string
                   (format "Add hop #%d: " numhops)
                   "sudo:root@"))
        (while (and hop (not (string-empty-p hop)))
          (push hop hops)
          (incf numhops)
          (setq hop (read-string
                     (format "Add hop #%d: " numhops)
                     hop)))))
    (setq hosts (remotehost-connect--gather-hosts method user dir hops))
    (ivy-read "Remote host: " hosts
              :history 'remotehost-connect-history
              :action (lambda (x)
                        (if (consp x)
                            (remotehost-connect--find-file
                             (cdr x) method user dir)
                          (remotehost-connect--find-file
                           x method user dir)))
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
    (setq dir (read-string "Remote dir: " "~"))
    (when dir
      (plist-put plist :dir dir)
      (remotehost-connect--find-file plist))))

(defun remotehost-connect-override-settings (x)
  "Read input to specify various settings via `remotehost-connect'.
X is a plist containing the connection properties."
  (interactive)
  (let* ((plist (cdr x))
         (method (plist-get plist :method))
         (user (plist-get plist :user))
         (dir (plist-get plist :dir))
         (numhops 1)
         hops hop)
    (when (and (setq method (read-string "Method: " method))
               (not (string-empty-p method)))
      (plist-put plist :method method))
    (when (and (setq user (read-string "User: " user))
               (not (string-empty-p user)))
      (plist-put plist :user user))
    (when (and (setq dir (read-string "Directory: " dir))
               (not (string-empty-p dir)))
      (plist-put plist :dir dir))
    (while (and (setq hop
                      (read-string
                       (format "Add hop #%d: " numhops)
                       (or (car (plist-get plist :hops))
                           "sudo:root@")))
                (not (string-empty-p hop)))
      (push hop hops)
      (incf numhops))
    (unless (seq-empty-p hops)
      (plist-put plist :hops hops))
    (remotehost-connect--find-file plist)))

(ivy-add-actions 'remotehost-connect
                 '(("u" remotehost-connect-change-user "user")
                   ("d" remotehost-connect-change-dir "directory")
                   ("v" remotehost-connect-override-settings "override")))

(provide 'remotehost-connect)
;;; remotehost-connect.el ends here
