;; darwin.el --- os settings file
;; Copyright (C) 2015-2020  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Modified Time-stamp: <2020-05-12 09:11:46 dharms>
;; Modified by: Dan Harms
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

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (setq use-package-verbose t)
  (require 'use-package))

(when (executable-find "gls")
  (setq insert-directory-program "gls"))

(setq mac-system-move-file-to-trash-use-finder t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Process Viewer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vkill :bind ("C-c 0p" . vkill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; osx-plist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package osx-plist
  :commands (osx-plist-parse-file osx-plist-parse-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gif-screencast ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'gif-screencast
  (setq gif-screencast-args '("-x"))
  (setq gif-screencast-capture-format "ppm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; osx-dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package osx-dictionary)

;; darwin.el ends here
