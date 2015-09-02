;; -*- Mode: Emacs-Lisp -*-
;; windows-nt.el --- windows os settings file
;; Copyright (C) 2015  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-09-02 08:52:42 dan.harms>
;; Keywords:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; everything ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq everything-ffap-integration nil)  ;for now
(setq everything-cmd "C:/Program Files/Everything/es.exe")
(require 'everything)

(setq-default comint-process-echoes t)
(setq w32-get-true-file-attributes nil)

(setq tramp-default-method "plink")

(add-to-list 'full-edit-reject-patterns "^moc")
(add-to-list 'full-edit-reject-patterns "^qrc")
(add-to-list 'full-edit-reject-patterns "^ui")

;; windows-nt.el ends here
