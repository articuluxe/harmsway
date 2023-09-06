;; windows-nt.el --- windows os settings file
;; Copyright (C) 2015-2021, 2023  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Modified Time-stamp: <2023-09-05 16:58:26 dharms>
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
;; windows-specific settings.
;;

;;; Code:

(require 'use-package)

(setq w32-use-visible-system-caret nil)

;; keys
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super)
(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Process Viewer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package proced :bind ("C-c 0p" . proced))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sharper ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sharper
  :bind ("C-c 0vs" . sharper-main-transient))

;; explicit-shell-file-name is used to create a shell
(setq explicit-shell-file-name (executable-find "cmdproxy.exe"))
;; shell-file-name is used for sub-processes
(setq shell-file-name "sh")             ;finds msys locally, /bin/sh remotely
;;       (concat (getenv "MSYS") "\\bin\\bash.exe"))

(defun harmsway-shell ()
  "Open a shell on windows using bash."
  (interactive)
  (let ((explicit-shell-file-name "bash"))
    (shell)))

(setq-default comint-process-echoes t)
(add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
(setq w32-get-true-file-attributes nil)
(setq w32-pipe-read-delay 0)
(setq tramp-default-method "plink")
(setq inhibit-compacting-font-caches t)

;; On shells (and remote compiles) this prevents $TERM from being forced to emacs
(setq system-uses-terminfo t)

;; compilation example:
;; (setq compile-command "C:\VC\VCVARS.bat & devenv /nologo /build Debug proj.vcxproj")

;; windows-nt.el ends here
