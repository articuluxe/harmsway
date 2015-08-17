;; -*- Mode: Emacs-Lisp -*-
;; parse-remote-hosts-xml.el --- parse a homiak mremote file
;; Copyright (C) 2015  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Friday, August  7, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-08-17 16:45:49 dan.harms>
;; Modified by: Dan Harms
;; Keywords: homiak tramp remote hosts

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

;; Commentary:

;;

;; Code:

(require 'xml)
(require 'loop)
(require 'save-sexp)

(defun parse-homiak--visit-node (node category)
  (let (type hostname cat user suffix)
    (when (listp node)
      (setq type (xml-get-attribute node 'Type))
      (setq hostname (xml-get-attribute node 'Hostname))
      (setq cat (xml-get-attribute node 'Name))
      (when cat
        (setq category (concat category (when category ":") cat)))
      ;; (message "Looking at %s %s %s = %s" (xml-node-name node)
      ;;          type hostname category)
      (cond ((string= type "Connection")
             (string-match "\\`\\([^[:space:]]+\\)\\s-*-?\\s-*\\(.*\\)?\\'"
                           cat)
             (setq suffix (match-string-no-properties 2 cat))
             (when suffix
               (setq category (concat category ":" suffix)))
             (setq user (xml-get-attribute node 'Username))
             (setq my/remote-host-list
                   (cons
                    (list :host
                          (match-string-no-properties 1 cat)
                          :user
                          (if (> 0 (length user)) user user-login-name)
                          :password
                          (xml-get-attribute node 'Password)
                          :description
                          (xml-get-attribute node 'Descr)
                          :category
                          category
                          )
                    my/remote-host-list)))
            ((string= type "Container")
             (loop-for-each elt (xml-get-children node 'Node)
               (parse-homiak--visit-node elt category))
             )))))

(defun parse-homiak-mremotes ()
  "Parse mremote configuration for xr remote hosts from David Homiak."
  (interactive)
  (let ((file (ido-read-file-name "XML File: " nil nil t))
        root)
    (if file
        (progn
          (setq my/remote-host-list '())
          (setq root (car (xml-parse-file file)))
          (loop-for-each elt (xml-node-children root)
            (when (listp elt)
              (parse-homiak--visit-node elt nil))))
      (error "No input file specified."))
    (if my/remote-host-list
        (progn
          (setq file (ido-read-file-name "Output file:"))
          (if file
              (save-sexp-save-setq file 'my/remote-host-list)
            (error "No output file specified.")))
      (error "No results found."))))

(provide 'parse-remote-hosts-xml)

;; parse-remote-hosts-xml.el ends here
