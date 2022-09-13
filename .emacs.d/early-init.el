;;; early-init.el --- Early initialization options for Emacs
;; Copyright (C) 2020-2022  Dan.Harms (Dan.Harms)
;; Author: Dan.Harms <enniomore@icloud.com>
;; Created: Tuesday, January 14, 2020
;; Modified Time-stamp: <2022-09-13 16:11:52 dharms>
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
(setq native-comp-async-report-warnings-errors nil)

(if (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (expand-file-name "~/.local/cache/eln-cache/"))
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "~/.local/temp/cache/eln-cache/")))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(menu-bar-mode -1)

(add-to-list 'initial-frame-alist '(cursor-type . bar))
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))

(add-to-list 'default-frame-alist '(cursor-type . bar))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))


;;; early-init.el ends here
