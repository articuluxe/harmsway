;;; harmsway-gui.el --- gui settings
;; Copyright (C) 2018, 2020-2021, 2023  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, August  8, 2018
;; Modified Time-stamp: <2023-09-12 08:33:59 dharms>
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

(defun harmsway-gui-disable-toolbar (frame)
  "Disable toolbar on frame FRAME."
  (modify-frame-parameters frame
                           '((tool-bar-lines . 0))))

(defun harmsway-gui-disable-scrollbar (frame)
  "Disable scroll bars from frame FRAME."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))

(defun harmsway-gui-load (&optional frame)
  "Load GUI settings for frame FRAME."
  (when window-system
    (let ((file (symbol-name window-system)))
      (message "harmsway-gui loading %s" file)
      (load file t)))
  (harmsway-gui-disable-scrollbar frame)
  (harmsway-gui-disable-toolbar frame))

(unless (bound-and-true-p byte-compile-current-file)
  (unless after-init-time
    (add-hook 'after-init-hook #'harmsway-gui-load))
  (add-hook 'after-make-frame-functions #'harmsway-gui-load))

(provide 'harmsway-gui)
;;; harmsway-gui.el ends here
