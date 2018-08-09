;;; harmsway-gui.el --- gui settings
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, August  8, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-08-08 17:54:34 dharms>
;; Modified by: Dan Harms
;; Keywords: emacs gui tools

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
;; harmsway gui settings.
;;

;;; Code:

(defvar harmsway-gui-post-frame '()
  "Hook of functions to run in `after-make-frame-functions'.")

(defun harmsway-gui-disable-scroll-bars (frame)
  "Disable scroll bars from frame FRAME."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))

(defun harmsway-gui-set-common-opts (frame)
  "Set common options for FRAME."
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  )

(defun harmsway-gui-load (&optional frame)
  "Load GUI settings for frame FRAME."
  (let ((guifile (concat my/gui-dir
                         (if (null window-system) "tty"
                           (symbol-name window-system)))))
    (load guifile)
    (run-hook-with-args 'harmsway-gui-post-frame
                        (or frame (selected-frame)))
    ))

(add-hook 'after-make-frame-functions #'harmsway-gui-load)
(add-hook 'harmsway-gui-post-frame #'harmsway-gui-disable-scroll-bars)
(add-hook 'harmsway-gui-post-frame #'harmsway-gui-set-common-opts)

(provide 'harmsway-gui)
;;; harmsway-gui.el ends here
