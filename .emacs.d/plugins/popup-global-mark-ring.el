;;; popup-global-mark-ring.el --- Jumping Interactively through global mark ring

;; Copyright (C) 2010  whitypig

;; Author: whitypig <whitypig@gmail.com>
;; Keywords: lisp popup emacs meadow
;; Version: 0.11

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
;; This software is greatly inspired by popup-kill-ring
;; and created thanks to popup.el,
;; so I'd like to express my gratitude to its authors.
;;
;; Apparently, there still are many bugs, so please be generous.

;;; Requirement:
;;
;; * popup.el   http://github.com/m2ym/auto-complete

;;; Installation:
;;
;; Copy popup-global-mark-ring.el to your load-path and add to your .emacs:
;;
;; (require 'popup-global-mark-ring)
;; 
;; To use popup-global-mark-ring, do M-x popup-global-mark-ring.
;; Or assign the key whatever you want to to 'popup-global-mark-ring.
;; For example,
;; (global-set-key "\C-c\C-g" 'popup-global-mark-ring)

;;; Usage notes
;; When a menu is being showd, you can switch between global-mark-ring
;; menu and local-mark-ring menu by "\C-t".

;;; Code:

(require 'popup)

;;; Variables:

(defvar popup-global-mark-ring-menu-width 70
  "Width of popup menu.
If the width of a line containing a marker is over this value,
the line is trimmed to this value.")

(defvar popup-global-mark-ring-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap popup-menu-keymap)
    (define-key keymap "\C-t" 'popup-global-mark-ring-switch)
    keymap)
  "A keymap `popup-menu*' of `popup-global-mark-ring'.")

(defvar popup-global-mark-ring-menu-func 'popup-global-mark-ring-menu
  "A function for displaying popup-menu.
This can be either `popup-global-mark-ring-menu' or
`popup-global-mark-ring-local-menu'")

(defvar popup-global-mark-ring-ring-var nil
  "A variable holding a copy of either global-mark-ring or mark-ring")


;;; Functions:

(defun popup-global-mark-ring ()
  "Show global mark ring menu and go to the place selected."
  (interactive)
  (let ((item nil)
        (num 0)
        (marker nil)
        (current-marker nil))
    ;; Show menu and get selection
    (setq item (popup-global-mark-ring-get-selection))
    (unless item (message "both mark rings are empty!"))
    (when item
      (when (string-match "^\\([0-9]+\\):.*" item)
        (setq num (1- (string-to-number (match-string 1 item))))
        (setq marker (nth num popup-global-mark-ring-ring-var))
        ;; Make current-location marker from the current location
        ;; and push it into mark-ring if it is not
        (setq current-marker (point-marker))
        (unless (and (marker-position current-marker) (member current-marker global-mark-ring))
          (push-mark))
        ;; now, go to the place specified by the marker
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))))))

;; TODO
(defun popup-global-mark-ring-current ())

;; TODO
(defun popup-global-mark-ring-next ())

;; TODO
(defun popup-global-mark-ring-prev ())

(defun popup-global-mark-ring-menu ()
  "Return a list of lines with each line containing marker info.
Iterating `global-mark-ring', make and return a list consisting of
marker information that can be acquired from each element in `global-mark-ring'"
  (interactive)
  (let ((ret nil)
        (i 1)
        (empty-marker (make-marker))
        (ring nil))
    (dolist (elt global-mark-ring)
      (unless (equal elt empty-marker)
        (add-to-list 'ring elt t)))
    ;; save current ring
    (setq popup-global-mark-ring-ring-var ring)
    (dolist (elt ring)
      (let ((pos (marker-position elt))
            (bufname (buffer-name (marker-buffer elt)))
            (linenum 0)
            (start 0)
            (end 0)
            (s nil))
        ;; get one line in the buffer specified by this marker
        (save-excursion
          (set-buffer bufname)
          (setq linenum (line-number-at-pos pos))
          (goto-char pos)
          (beginning-of-line)
          (setq start (point))
          (end-of-line)
          (setq end (point))
          (setq s
                (replace-regexp-in-string "^[[:space:]]+" "" (buffer-substring-no-properties start end)))
          (when (and popup-global-mark-ring-menu-width
                      (> (length s) popup-global-mark-ring-menu-width))
            (setq s (substring-no-properties s 0 popup-global-mark-ring-menu-width)))
          (add-to-list 'ret (format "%d:(%s:%d): %s" i bufname linenum s) t))
        (setq i (1+ i))))
      ret))

(defun popup-global-mark-ring-local-menu ()
  "Return a list of string representation of mark-ring."
  (interactive)
  (let ((ret nil)
        (i 1)
        (ring nil))
    ;; remove duplication in mark-ring
    (dolist (elt mark-ring)
      (add-to-list 'ring elt t))
    ;; sort by position
    (setq ring (sort ring (lambda (a b) (< (marker-position a) (marker-position b)))))
    (setq popup-global-mark-ring-ring-var ring)
    (dolist (m ring)
      (let* ((pos (marker-position m))
            (linenum (line-number-at-pos pos))
            (start 0)
            (end 0)
            (s nil))
        (save-excursion
          (goto-char pos)
          (beginning-of-line)
          (setq start (point))
          (end-of-line)
          (setq end (point))
          (setq s (replace-regexp-in-string "^[[:space:]]+" ""
                                            (buffer-substring-no-properties start end)))
          (when (and popup-global-mark-ring-menu-width
                     (> (length s) popup-global-mark-ring-menu-width))
            (setq s (substring-no-properties s 0 popup-global-mark-ring-menu-width)))
          (add-to-list 'ret
                       (format "%d:(%d): %s" i linenum s) t)
          (setq i (1+ i)))))
    ret))

(defun popup-global-mark-ring-get-selection ()
  "Return a selected item or nil."
  (let ((ret nil))
    (cond
     ;; local mode
     ((equal popup-global-mark-ring-menu-func 'popup-global-mark-ring-local-menu)
      ;; if mark-ring is empty, switch to global mode
      (unless mark-ring
        (message "Local mark-ring is empty")
        (setq popup-global-mark-ring-menu-func 'popup-global-mark-ring-menu))
      (setq ret (popup-menu* (call-interactively
                                   popup-global-mark-ring-menu-func)
                                  :scroll-bar t
                                  :margin t
                                  :width popup-global-mark-ring-menu-width
                                  :keymap popup-global-mark-ring-keymap)))
     ;; global mode
     ((and global-mark-ring (equal popup-global-mark-ring-menu-func 'popup-global-mark-ring-menu))
      (setq ret (popup-menu* (call-interactively
                              popup-global-mark-ring-menu-func)
                             :scroll-bar t
                             :margin t
                             :width popup-global-mark-ring-menu-width
                             :keymap popup-global-mark-ring-keymap)))
     (t nil))
    ret))

(defun popup-global-mark-ring-switch ()
  "Switch from global-mark-ring to mark-ring and vice versa."
  (interactive)
  (if (equal popup-global-mark-ring-menu-func 'popup-global-mark-ring-menu)
      (setq popup-global-mark-ring-menu-func 'popup-global-mark-ring-local-menu)
    (setq popup-global-mark-ring-menu-func 'popup-global-mark-ring-menu))
  ;; variable `menu' is defined in `popup.el'
  (popup-delete menu)
  (popup-global-mark-ring))

(provide 'popup-global-mark-ring)
;;; popup-global-mark-ring.el ends here