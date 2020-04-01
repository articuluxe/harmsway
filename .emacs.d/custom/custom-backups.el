;;; custom-backups.el --- customize backups and autosaves
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, November 29, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-11-30 08:48:25 dharms>
;; Modified by: Dan Harms
;; Keywords: backup autosave

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-save ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/autosave-dir (concat my/user-directory "autosaves/"))
(unless (file-directory-p my/autosave-dir)
  (make-directory my/autosave-dir t))
(setq auto-save-file-name-transforms
      `((".*" ,(concat my/autosave-dir "\\1") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/backup-dir
  (concat my/user-directory "backups/" (format-time-string "%Y-%m-%d")))
(unless (file-directory-p my/backup-dir)
  (make-directory my/backup-dir t))
(setq backup-directory-alist `(("." . ,my/backup-dir)))
(setq delete-by-moving-to-trash t)
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-old-versions 0)              ;oldest versions to keep
(setq kept-new-versions 10)
(setq auto-save-timeout 60)
(setq auto-save-interval 0)             ;disable autosaves due to input events

(defvar my/backup-exclude-regex "recentf"
  "Regexp of filenmae patterns to prevent backup creation.")

(defun my/backup-predicate (path)
  "Return whether to backup the file given in PATH."
  (if (string-match-p my/backup-exclude-regex path)
      nil
    (normal-backup-enable-predicate path)))

(setq backup-enable-predicate #'my/backup-predicate)

(set-register ?\C-b (cons 'file my/backup-dir))
(set-register ?\C-a (cons 'file my/autosave-dir))

(provide 'custom-backups)
;;; custom-backups.el ends here
