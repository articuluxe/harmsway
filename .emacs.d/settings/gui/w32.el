;; w32.el --- windows gui settings file
;; Copyright (C) 2015-2016, 2018  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2018-08-02 08:55:42 dharms>
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
(add-hook 'after-make-frame-functions 'harmsway-disable-scroll-bars)

;; scroll one line at a time
(setq scroll-conservatively 0)
(setq scroll-step 1)
;; (setq scroll-conservatively 10000)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse nil)

;; toggle full-screen
(defun w32-maximize-frame() "Maximize current frame on windows"
  (interactive)
  (w32-send-sys-command 61488))
(defun w32-minimize-frame() "Restore (un-maximize) current frame on windows"
  (interactive)
  (w32-send-sys-command 61728))
(defun w32-toggle-full-screen() "Toggle full screen" (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil 'fullscreen))
  (if (frame-parameter nil 'fullscreen)
      (w32-maximize-frame)
    (w32-minimize-frame)))
(global-set-key [f11] 'w32-toggle-full-screen)
(global-set-key "\C-c0q11" 'w32-toggle-full-screen)

(setq default-frame-alist '((cursor-type . bar)))

(setq initial-frame-alist
      '(
        (top . 5) (left . 5)
        ))

(set-face-font 'default "Consolas-11")

;;(pos-tip-w32-max-width-height t)          ;maximize frame temporarily

;; w32.el ends here
