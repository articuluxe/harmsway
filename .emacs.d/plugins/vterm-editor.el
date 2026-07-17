;;; vterm-editor.el --- Edit text in a buffer and send it to vterm -*- lexical-binding: t; -*-

;; Author: Andros Fenollosa <hi@andros.dev>
;; URL: https://git.andros.dev/andros/vterm-editor.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (vterm "0.0.1"))
;; Keywords: terminals, convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a convenient way to compose multi-line text
;; in a regular Emacs buffer and send it to a vterm terminal.
;;
;; Usage:
;;   1. From a vterm buffer, call `vterm-editor-open'.
;;   2. Write your text in the editor buffer that appears.
;;   3. Press C-c C-c to send the text to vterm, or C-c C-k to cancel.

;;; Code:

(require 'vterm)
(require 'subr-x)

(defvar-local vterm-editor--source-buffer nil
  "The vterm buffer where the edited text will be sent.")

(defvar vterm-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'vterm-editor-finish)
    (define-key map (kbd "C-c C-k") #'vterm-editor-abort)
    map)
  "Keymap for `vterm-editor-mode'.")

(define-minor-mode vterm-editor-mode
  "Minor mode active in the vterm editor buffer.
\\{vterm-editor-mode-map}"
  :lighter " VTerm-Ed"
  :keymap vterm-editor-mode-map)

;;;###autoload
(defun vterm-editor-open ()
  "Open a temporary buffer to compose text for the current vterm.
The editor buffer uses `text-mode' with `vterm-editor-mode' enabled.
Press \\[vterm-editor-finish] to send the text to vterm,
or \\[vterm-editor-abort] to cancel."
  (interactive)
  (unless (derived-mode-p 'vterm-mode)
    (user-error "Not in a vterm buffer"))
  (let ((source (current-buffer))
        (buf (get-buffer-create "*vterm-editor*")))
    (with-current-buffer buf
      (erase-buffer)
      (text-mode)
      (vterm-editor-mode 1)
      (setq vterm-editor--source-buffer source)
      (setq header-line-format
            (substitute-command-keys
             "Edit, then \\[vterm-editor-finish] to send or \\[vterm-editor-abort] to cancel")))
    (pop-to-buffer buf)))

(defun vterm-editor-finish ()
  "Send the buffer content to the source vterm and close the editor."
  (interactive)
  (let ((content (string-trim-right (buffer-string)))
        (source vterm-editor--source-buffer))
    (unless (buffer-live-p source)
      (user-error "Source vterm buffer no longer exists"))
    (quit-window t)
    (with-current-buffer source
      (vterm-send-string content t))))

(defun vterm-editor-abort ()
  "Close the editor buffer without sending anything to vterm."
  (interactive)
  (quit-window t))

(provide 'vterm-editor)
;;; vterm-editor.el ends here
