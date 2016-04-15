;; utils.el --- misc. utilities
;; Copyright (C) 2015, 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-04-15 17:34:52 dharms>
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

(require 'popup-imenu)

(defun insert-now()
  "Insert string for current time formatted like `2:34 PM'."
  (interactive)
  (insert (now)))
(defun now()
  "Return string for current time formatted like `2:34 PM'."
  (interactive)
  (format-time-string "%D %-I:%M %p"))

(defun insert-today()
  "Insert string for today's date nicely formatted in American style,
  e.g. Sunday, September 17, 2000."
  (interactive)
  (insert (today)))
(defun today()
  "Return string for today's date nicely formatted in American style,
  e.g. Sunday, September 17, 2000."
  (interactive)
  (format-time-string "%A, %B %e, %Y"))

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))
(bind-key "C-x C-M-e" #'sanityinc/eval-last-sexp-or-region)

(defun choose-via-popup (alist prompt)
  "Make a choice among ALIST, a list of choices, with PROMPT as a possible
prompt.  ALIST can either be a list of strings, or an alist, where every
element is a cons cell, the car of which is the display string given to the
user, and the cdr of which is the resultant value to be used if that cell
is selected."
  (popup-menu*
   (delete-consecutive-dups
    (mapcar (lambda(elt)
              (if (consp elt)
                  (popup-make-item (car elt) :value (cdr elt))
                (popup-make-item elt :value elt)))
            alist))
   :isearch t
   :isearch-filter (popup-imenu--filter)
   :prompt prompt
   ))

(defun choose-via-ido (alist prompt)
  "Make a choice among ALIST, a list of choices, with PROMPT as a possible
prompt.  ALIST can either be a list of strings, or an alist, where every
element is a cons cell, the car of which is the display string given to the
user, and the cdr of which is the resultant value to be used if that cell
is selected."
  (let ((res
         (ido-completing-read
          prompt
          (mapcar (lambda(elt)
                    (if (consp elt)
                        (car elt)
                      elt))
                  alist))))
    (or (cdr (assoc res alist))
        res)))

(defun choose-via-ivy (alist prompt)
  "Make a choice among ALIST, a list of choices, with PROMPT as a possible
prompt.  ALIST can either be a list of strings, or an alist, where every
element is a cons cell, the car of which is the display string given to the
user, and the cdr of which is the resultant value to be used if that cell
is selected."
  (let ((res
         (ivy-completing-read
          prompt
          (mapcar (lambda(elt)
                    (if (consp elt)
                        (car elt)
                      elt))
                  alist))))
    (or (cdr (assoc res alist))
        res)))

(defvar my/choose-func 'choose-via-popup
  "Make a selection among a list of choices.")
(global-set-key "\C-c\C-r" 'my/choose-choose-func)

(defun my/choose-choose-func ()
  "Toggle between available selection frameworks.  Current choices include
 `ido', `popup' and `ivy'."
  (interactive)
  (if (eq my/choose-func 'choose-via-popup)
      (progn
        (setq my/choose-func 'choose-via-ido)
        (message "Choosing via ido"))
    (if (eq my/choose-func 'choose-via-ido)
        (progn
          (setq my/choose-func 'choose-via-ivy)
          (message "Choosing via ivy"))
      (setq my/choose-func 'choose-via-popup)
      (message "Choosing via popup"))))

(defun find-file-upwards (dir file-to-find)
  "Recursively search upward for file; returns path to file or nil if not found."
  (interactive)
  (let*
      ((find-file-r
        (lambda (path)
          (let* ((parent (file-name-directory path))
                 files)
            (cond
             ((or (null parent) (equal parent (directory-file-name parent))) nil)
             ((setq files (directory-files parent t file-to-find))
              (car files))              ;found
             ;; parent of ~ is nil, parent of / is itself
             ;; This terminating condition accounts for both
             (t (funcall find-file-r
                         (directory-file-name parent))))))))
    (funcall find-file-r (or dir default-directory))))

(defun find-file-dir-upwards (file-to-find)
  "Recursively search upward for file; returns file's directory or nil if not found."
  (interactive)
  (let ((file (find-file-upwards file-to-find)))
    (if file (file-name-directory file) nil)))

(defun goto-line-with-feedback()
  "Show line numbers temporarily while prompting for the target line."
  (interactive)
  (if (and (or (not (boundp 'linum-mode)) (not linum-mode))
           (not current-prefix-arg))
      (unwind-protect
          (progn
            (linum-mode 1)
            (call-interactively 'goto-line))
        (linum-mode -1))
    (call-interactively 'goto-line)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; (defvar my/find-file-root-prefix "/sudo::"
;;   "root prefix for tramp as root")

;; (defun my/find-file-as-root()
;;   "Open a file for editing by root."
;;   (interactive)
;;   (let ((root-prefix my/find-file-root-prefix))
;;     (call-interactively 'find-file)))
;; (global-set-key "\esrf" 'my/find-file-as-root)

;; (defun my/find-current-file-as-root()
;;   "Open the current file for editing by root."
;;   (interactive)
;;   (set-visited-file-name (concat my/find-file-root-prefix
;;                                  (buffer-file-name)))
;;   (setq buffer-read-only nil))
;; (global-set-key "\esrr" 'my/find-current-file-as-root)

(defun read-file-into-list-of-lines(file)
  "Read a file into a list of strings split line by line."
  (interactive)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun load-environment-variable-from-file(var file &optional sep)
  "Loads each line from the specified file into the environment var."
  (interactive)
  (unless sep (setq sep path-separator))
  (setenv var (concat (mapconcat 'convert-standard-filename
                                 (read-file-into-list-of-lines file)
                                 sep) sep (getenv var))))

(defun shell-command-redirected-output (cmd)
  "Run a shell command, with the option of separately redirecting
stdout and stderr. Use instead of `shell-command-to-string'.
Substitute the `nil' in '(t nil)' with a filename to redirect
stderr into that file."
  (interactive "sCmd: ")
  (with-output-to-string
    (with-current-buffer standard-output
      (process-file shell-file-name nil
                    '(t nil) nil
                    shell-command-switch
                    cmd))))

;; utils.el ends here
