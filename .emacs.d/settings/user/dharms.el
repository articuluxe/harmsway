;; dharms.el --- user settings file
;; Copyright (C) 2015-2018, 2020-2021  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Modified Time-stamp: <2021-02-17 16:12:54 dharms>
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

(setq user-mail-address "enniomore@icloud.com")
(setq copyright-names-regexp "[Hh]arms")
(setq epa-file-encrypt-to "enniomore@icloud.com")

(add-hook
 'prog-mode-hook
 (lambda() (font-lock-add-keywords
            nil '(("\\<[Dd][Rr][Hh]\\>" 0 'font-lock-warning-face t)) t)) t)

;; logview initials
(setq logview-user-initials "drh")

;;; dharms.el ends here
