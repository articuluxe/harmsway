;; -*- Mode: Emacs-Lisp -*-
;; dan.harms.el --- user settings file
;; Copyright (C) 2015, 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-01-27 07:31:08 dharms>
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

;; Commentary:

;;

;; Code:

(defconst my/user-name user-login-name)
(setq user-mail-address "dan.harms@xrtrading.com")

(add-hook 'c-mode-common-hook
          (lambda() (font-lock-add-keywords
                     nil '(("\\<drh\\>" 0 'font-lock-warning-face t)) t)) t)

;; dan.harms.el ends here
