;;; custom-buffer-utils.el --- custom buffer utilities
;; Copyright (C) 2016, 2019  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Friday, April 15, 2016
;; Version: 1.0
;; Modified Time-stamp: <2019-01-07 14:39:51 dan.harms>
;; Modified by: Dan Harms
;; Keywords: buffer utilities

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

;;;###autoload
(defun harmsway-revert-buffer ()
    "Revert a buffer automatically."
    (interactive)
    (let ((truncate truncate-lines))
      (revert-buffer nil t)
      (setq truncate-lines truncate)))

(defun kill-other-buffers(&optional arg)
  "Kill all buffers (except optionally for current one)."
  (interactive)
  (if current-prefix-arg
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (mapc 'kill-buffer (buffer-list))))

(defun switch-to-most-recent-buffer()
  "Switch to most recent buffer.  Repeated calls toggle buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun window-toggle-split-direction()
  "Switch window split from horizontal to vertical, or vice versa."
  (interactive)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbor-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbor1 (windmove-find-other-window neighbor-dir win))
               (neighbor2 (if next-win (with-selected-window next-win
                                         (windmove-find-other-window
                                          neighbor-dir next-win)))))
          (setq done (and (eq neighbor1 neighbor2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbor-dir)
                                   other-buf))))))))

;; from Steve Yegge
(defun swap-buffers() "Swap the first 2 buffers" (interactive)
  (when (> (length (window-list)) 1)
    (let* ((win1 (car (window-list)))
           (win2 (cadr (window-list)))
           (buf1 (window-buffer win1))
           (buf2 (window-buffer win2))
           (pos1 (window-start win1))
           (pos2 (window-start win2)))
      (set-window-buffer win1 buf2)
      (set-window-buffer win2 buf1)
      (set-window-start win1 pos2)
      (set-window-start win2 pos1))))

(defun my/toggle-window-dedicated ()
  "Toggle whether active window is dedicated."
  (interactive)
  (message
   (if
       (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p
          window
          (not (window-dedicated-p window))))
       "Window '%s' is dedicated."
     "Window '%s' is not dedicated.")
   (current-buffer)))

(defun rename-file-and-buffer(new-name)
  "Renames both current buffer and file it's visiting."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file(dir)
  "Moves both current buffer and file it's visiting."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1)
            dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

(defun move-buffer-to-new-frame(&optional arg)
  "Moves current window to new frame, and optionally (with a prefix arg)
removes it from its original frame."
  (interactive)
  (let ((win (selected-window)))
    (make-frame-command)
    (and current-prefix-arg
         (> (length (window-list)) 1)
         (remove-window win))))

(provide 'custom-buffer-utils)
;;; custom-buffer-utils.el ends here
