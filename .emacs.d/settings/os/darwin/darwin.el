;; darwin.el --- os settings file
;; Copyright (C) 2015-2021  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Modified Time-stamp: <2021-02-11 13:55:55 dharms>
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

(set-register ?\C-i (cons 'file "~/Library/Mobile Documents/com~apple~CloudDocs"))

(when (executable-find "gls")
  (setq insert-directory-program "gls"))

(setq mac-system-move-file-to-trash-use-finder t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Process Viewer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vkill :bind ("C-c 0p" . vkill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; osx-plist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package osx-plist
  :commands (osx-plist-parse-file osx-plist-parse-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; homebrew ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'harmsway-homebrew-map)
(use-package homebrew
  :bind (("C-c 0h" . harmsway-homebrew-map)
         :map harmsway-homebrew-map
         ("i" . homebrew-install)
         ("u" . homebrew-upgrade)
         ("d" . homebrew-update)
         ("e" . homebrew-edit)
         ("i" . homebrew-info)
         ("p" . homebrew-package-info)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gif-screencast ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'gif-screencast
  (setq gif-screencast-args '("-x"))
  (setq gif-screencast-capture-format "ppm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; osx-dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package osx-dictionary)

;; darwin.el ends here
