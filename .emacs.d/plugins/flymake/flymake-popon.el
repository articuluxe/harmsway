;;; flymake-popon.el --- Flymake diagnostics on cursor hover -*- lexical-binding: t -*-

;; Copyright (C) 2022 Akib Azmain Turja.

;; Author: Akib Azmain Turja <akib@disroot.org>
;; Created: 2022-04-28
;; Version: 0.5.1
;; Package-Requires: ((emacs "26.1") (flymake "1.2.2") (popon "0.1") (posframe "1.3.2"))
;; Keywords: convenience
;; Homepage: https://codeberg.org/akib/emacs-flymake-popon

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package shows Flymake diagnostics on cursor hover.  This works
;; on both graphical and non-graphical displays.  Enable it with M-x
;; flymake-popon-mode.

;;; Code:

(require 'popon)
(require 'posframe)
(require 'flymake)
(require 'compile)

(defgroup flymake-popon nil
  "Flymake diagnostics on cursor hover."
  :group 'convenience
  :link '(url-link "https://codeberg.org/akib/emacs-flymake-popon")
  :prefix "flymake-popon-")

(defcustom flymake-popon-method 'posframe
  "How to show popup.

When value is `popon', a popon (popup) is used.  When value is
`posframe', a posframe (child frame) is used.  On non-graphical
display a popon will always be used."
  :type '(choice (const :tag "Use a popon (popup)" popon)
                 (const :tag "Use a posframe (child frame)" posframe)))

(defcustom flymake-popon-diagnostic-formatter
  #'flymake-popon-format-diagnostic
  "Format diagnotics using this function.

The function should take a single argument, which is a
`flymake-diagnostic' object and return a string describing it.  The
string should not be wider than `flymake-popon-width'.  It may contain
newlines."
  :type 'function)

(defcustom flymake-popon-posframe-extra-arguments
  '(:poshandler posframe-poshandler-point-bottom-left-corner-upward)
  "Extra arguments to pass to `posframe-show'."
  :type '(plist :key-type (symbol :tag "Argument")
                :value-type (sexp :tag "Value")))

(defcustom flymake-popon-width 65
  "Width of popon/posframe."
  :type 'integer)

(defcustom flymake-popon-posframe-border-width 1
  "Border width of posframe.

When value is zero, don't show a border.  This is ignore when
`flymake-popon-method' is set to `popon'."
  :type 'integer)

(defcustom flymake-popon-delay 0.2
  "Show message after being idle for this time.

The value should be in seconds."
  :type 'number)

(defface flymake-popon
  '((t :inherit default))
  "Default face for popon/posframe.")

(defface flymake-popon-posframe-border
  '((t :foreground "black"))
  "Border face.  Only foreground is used, others are ignored.")

(defvar flymake-popon--popup nil
  "Popon or posframe currently visible.")

(defvar flymake-popon--timer nil
  "Timer to show popon.")

(defun flymake-popon-format-diagnostic (diagnostic)
  "Format DIAGNOSTIC to text."
  (with-temp-buffer
    (insert
     "* "
     (propertize
      (car (split-string (flymake-diagnostic-text diagnostic)
                         "[\n\r]"))
      'face (flymake--lookup-type-property
             (flymake-diagnostic-type diagnostic) 'mode-line-face)))

    ;; Break long lines.
    (goto-char (point-min))
    (let ((last-whitespace nil)
          (last-char-whitespace-p nil))
      (while (not (eobp))
        (if (not (member (buffer-substring-no-properties
                          (point) (1+ (point)))
                         '(" " "\t")))
            (setq last-char-whitespace-p nil)
          (unless last-char-whitespace-p
            (setq last-whitespace (point))
            (setq last-char-whitespace-p t)))
        (when (>= (current-column) flymake-popon-width)
          (save-excursion
            (when last-whitespace
              (goto-char last-whitespace))
            (insert "\n  ")
            (while (member (buffer-substring-no-properties
                            (point) (1+ (point)))
                           '(" " "\t"))
              (delete-char 1))))
        (right-char)))
    (buffer-string)))

(defun flymake-popon--popon-format-message (message)
  "Format MESSAGE to show it in a popon."
  (let* ((lines (split-string message "\n"))
         (width (min flymake-popon-width
                     (apply #'max (mapcar #'string-width lines))))
         (i 0))
    (mapcar (lambda (line)
              (with-temp-buffer
                (insert line)
                (move-to-column width t)
                (setq i (1+ i))
                (buffer-substring (point-min) (point))))
            lines)))

(defun flymake-popon--show ()
  "Show popon."
  (flymake-popon--hide)
  (when-let ((diagnostics (flymake-diagnostics (point))))
    (let ((message (mapconcat flymake-popon-diagnostic-formatter
                              diagnostics "\n")))
      (setq flymake-popon--popup
            (if (or (not (display-graphic-p))
                    (eq flymake-popon-method 'popon))
                (let* ((lines (flymake-popon--popon-format-message
                               message))
                       (str (string-join lines "\n")))
                  (add-face-text-property
                   0 (length str) 'flymake-popon t str)
                  (popon-create
                   str
                   (let ((pos (popon-x-y-at-pos (point))))
                     (if (>= (cdr pos) (length lines))
                         (cons (car pos) (- (cdr pos) (length lines)))
                       (cons (car pos) (1+ (cdr pos)))))))
              (let ((buffer (get-buffer-create " *flymake-popon*")))
                (with-current-buffer buffer
                  (face-remap-add-relative 'default 'flymake-popon))
                (apply
                 #'posframe-show buffer :string message
                 `(,@(unless
                         (zerop flymake-popon-posframe-border-width)
                       `(:border-width
                         ,flymake-popon-posframe-border-width
                         :border-color
                         ,(face-foreground
                           'flymake-popon-posframe-border nil t)))
                   ,@flymake-popon-posframe-extra-arguments))
                buffer))))))

(defun flymake-popon--hide ()
  "Hide popon."
  (when flymake-popon--popup
    (if (or (not (display-graphic-p))
            (eq flymake-popon-method 'popon))
        (popon-kill flymake-popon--popup)
      (posframe-hide flymake-popon--popup))
    (setq flymake-popon--popup nil)))

(defun flymake-popon--post-command ()
  "Start timer to show popon."
  (when flymake-popon--timer
    (cancel-timer flymake-popon--timer))
  (setq flymake-popon--timer
        (run-with-timer flymake-popon-delay nil
                        #'flymake-popon--show)))

(defun flymake-popon--update-if-shown (&rest _)
  "Update popon if it is shown."
  (when (and flymake-popon-mode flymake-popon--popup)
    (flymake-popon--show)))

;;;###autoload
(define-minor-mode flymake-popon-mode
  "Toggle show Flymake diagnostics on cursor hover."
  :lighter " Flymake-Popon"
  :keymap nil
  (if flymake-popon-mode
      (progn
        (add-hook 'pre-command-hook #'flymake-popon--hide nil t)
        (add-hook 'post-command-hook #'flymake-popon--post-command
                  nil t)
        (advice-add #'flymake--handle-report :after
                    #'flymake-popon--update-if-shown))
    (remove-hook 'pre-command-hook #'flymake-popon--hide t)
    (remove-hook 'post-command-hook #'flymake-popon--post-command t)))

;;;###autoload
(define-globalized-minor-mode global-flymake-popon-mode
  flymake-popon-mode flymake-popon-mode)

(provide 'flymake-popon)
;;; flymake-popon.el ends here
