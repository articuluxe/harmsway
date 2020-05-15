;;; harmsway-backup.el --- customize backups and autosaves
;; Copyright (C) 2016, 2020  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, November 29, 2016
;; Modified Time-stamp: <2020-05-15 12:24:35 Dan.Harms>
;; Modified by: Dan.Harms
;; Keywords: backup autosave tools

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
;; Configure backups and autosaves.
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto-save ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar harmsway-autosave-dir (concat my/user-directory "autosaves/"))
(unless (file-directory-p harmsway-autosave-dir)
  (make-directory harmsway-autosave-dir t))
(setq auto-save-file-name-transforms
      `((".*" ,(concat harmsway-autosave-dir "\\1") t)))
(setq tramp-auto-save-directory harmsway-autosave-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; backups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar harmsway-backup-dir
  (concat my/user-directory "backups/" (format-time-string "%Y-%m-%d")))
(unless (file-directory-p harmsway-backup-dir)
  (make-directory harmsway-backup-dir t))
(setq backup-directory-alist `(("." . ,harmsway-backup-dir)))
(setq delete-by-moving-to-trash t)
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-old-versions 0)              ;oldest versions to keep
(setq kept-new-versions 10)
(setq auto-save-timeout 60)
(setq auto-save-interval 0)             ;disable autosaves due to input events

(defvar harmsway-backup-exclude-regex
  "recentf\\|ido-last\\|emacs-bmk-bmenu-state\\|COMMIT_EDITMSG"
  "Regexp of filenmae patterns to prevent backup creation.")

(defun harmsway-backup-predicate (path)
  "Return whether to backup the file given in PATH."
  (if (string-match-p harmsway-backup-exclude-regex path)
      nil
    (normal-backup-enable-predicate path)))

(setq backup-enable-predicate #'harmsway-backup-predicate)

(set-register ?\C-b (cons 'file harmsway-backup-dir))
(set-register ?\C-a (cons 'file harmsway-autosave-dir))

(provide 'harmsway-backup)
;;; harmsway-backup.el ends here
