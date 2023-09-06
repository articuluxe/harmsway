;; linux.el --- linux os settings file
;; Copyright (C) 2015-2017, 2019, 2021, 2023  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Modified Time-stamp: <2023-09-05 16:58:14 dharms>
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

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Process Viewer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package proced :bind ("C-c 0p" . proced))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mount ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package mount :bind ("C-c 0m" . mount))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; neato-graph-bar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package neato-graph-bar :bind ("C-c 0o" . neato-graph-bar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; trashed ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package trashed
  :bind ("C-c 0 DEL" . trashed)
  :init
  (setq trashed-action-confirmer 'y-or-n-p)
  )

;; linux.el ends here
