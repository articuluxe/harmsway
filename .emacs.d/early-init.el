;;; early-init.el --- Early initialization options for Emacs
;; Copyright (C) 2020-2021  Dan.Harms (Dan.Harms)
;; Author: Dan.Harms <enniomore@icloud.com>
;; Created: Tuesday, January 14, 2020
;; Modified Time-stamp: <2021-02-17 17:45:35 dharms>
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
;; Early init options.
;;

;;; Code:

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(menu-bar-mode -1)

;;; early-init.el ends here
