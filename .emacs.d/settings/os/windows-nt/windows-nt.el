;; windows-nt.el --- windows os settings file
;; Copyright (C) 2015-2017  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2017-05-29 13:49:25 dan.harms>
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

;; keys
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super)
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Process Viewer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package proced :bind ("C-c 0p" . proced))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ssh-agency ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'ssh-agency)

(setq shell-file-name "sh")             ;finds msys

(setq-default comint-process-echoes t)
(setq w32-get-true-file-attributes nil)
(setq w32-pipe-read-delay 0)
(setq tramp-default-method "plink")

;; qt
(with-eval-after-load 'full-edit
  (add-to-list 'full-edit-reject-patterns "^moc")
  (add-to-list 'full-edit-reject-patterns "^qrc")
  (add-to-list 'full-edit-reject-patterns "^ui")
  )
;; counsel
(with-eval-after-load 'counsel
  ;; or "set GIT_PAGER=cat && git log --grep \"%s\""
  (setq counsel-git-log-cmd "git log --grep \"%s\""))

;; compilation example:
;; (setq compile-command "C:\VC\VCVARS.bat & devenv /nologo /build Debug proj.vcxproj")

;; windows-nt.el ends here
