;; difftool.el --- use emacs as a diff tool for git
;; Copyright (C) 2015, 2016  Dan Harms (dan.harms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, August 18, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-03-30 18:27:18 dharms>
;; Modified by: Dan Harms
;; Keywords: git diff

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

(setq ediff-quit-hook 'kill-emacs
      ediff-split-window-function 'split-window-horizontally)
(add-to-list 'initial-frame-alist '(fullscreen . fullwidth))
(ediff-files
 (getenv "LOCAL")
 (getenv "REMOTE"))

;; difftool.el ends here
