;; dan.harms.el --- user settings file
;; Copyright (C) 2015-2018  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Modified Time-stamp: <2018-01-04 15:48:53 dan.harms>
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

(defconst my/user-name user-login-name)
(setq user-mail-address "dan.harms@xrtrading.com")
(setq copyright-names-regexp "[Hh]arms")

(add-hook
 'prog-mode-hook
 (lambda() (font-lock-add-keywords
            nil '(("\\<[Dd][Rr][Hh]\\>" 0 'font-lock-warning-face t)) t)) t)

;; logview initials
(setq logview-user-initials "drh")

;;; dan.harms.el ends here
