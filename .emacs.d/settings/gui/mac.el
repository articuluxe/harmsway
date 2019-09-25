;; mac.el --- mac gui settings
;; Copyright (C) 2015-2016, 2018-2019  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2019-09-25 06:54:50 dharms>
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

(setq ns-use-native-fullscreen nil)
(setq ns-use-thin-smoothing t)
;; scroll one line at a time
(setq scroll-step 1)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; ;; don't accelerate scrolling
;; (setq mouse-wheel-progressive-speed nil)
;; (setq mouse-wheel-follow-mouse nil)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; toggle full-screen
(defun ns-toggle-full-screen() (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil 'fullboth)))
(global-set-key [f11] 'ns-toggle-full-screen)
(global-set-key "\C-c0q11" 'ns-toggle-full-screen)

(setq default-frame-alist '((cursor-type . bar)
                            (ns-transparent-titlebar . t)
                            (ns-appearance . dark)))

(setq initial-frame-alist
      '(
        (top . 25) (left . 50)
        ))

;; ns.el ends here
