;; -*- Mode: Emacs-Lisp -*-
;; mergetool.el --- use ediff as git merge tool
;; Copyright (C) 2015  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Tuesday, August 18, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-08-20 09:21:36 dan.harms>
;; Modified by: Dan Harms
;; Keywords: git ediff

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

;; (defun ediff-write-merge-buffer ()
;;   (let ((file ediff-merge-store-file))
;;     (set-buffer ediff-buffer-C)
;;     (write-region (point-min) (point-max) file)
;;     (message "Merge buffer saved in: %s" file)
;;     (set-buffer-modified-p nil)
;;     (sit-for 1)))
;; (setq ediff-quit-merge-hook 'ediff-write-merge-buffer)
(setq ediff-quit-hook 'kill-emacs
      ediff-autostore-merges t)
(add-to-list 'initial-frame-alist '(fullscreen . fullwidth))
(ediff-merge-files-with-ancestor
 (getenv "LOCAL")
 (getenv "REMOTE")
 (getenv "BASE")
 nil
 (getenv "MERGED"))

;; mergetool.el ends here
