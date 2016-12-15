;; x.el --- x-windows gui settings file
;; Copyright (C) 2015, 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-12-15 17:48:04 dharms>
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

(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(global-set-key "\C-c0q11" 'toggle-frame-fullscreen)

;; scroll one line at a time
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse nil)

(setq default-frame-alist '((cursor-type . bar)))

(setq initial-frame-alist
      '(
        (top . 5) (left . 5)
        ))

;; x.el ends here
