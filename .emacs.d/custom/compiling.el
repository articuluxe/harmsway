;; compiling.el --- utilities concerned with compiling
;; Copyright (C) 2015, 2016  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2016-05-19 14:46:37 dan.harms>
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

(require 'dash)

(defvar my/compile-command)

;; automatically scroll compilation window
(setq compilation-scroll-output t)

(defun my/compilation-mode-hook()
  (setq truncate-lines nil) ; is buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook 'compilation-mode-hook 'my/compilation-mode-hook)

(defun create-compile-command-standard()
  "Initialize the compile command."
  (interactive)
  (let ((root (or (profile-current-get 'project-root-dir) "./"))
        (command (or (profile-current-get 'compile-sub-command) "make"))
        (sub-dirs (profile-current-get 'build-sub-dirs))
        sub-dir)
    (when current-prefix-arg
      (setq root (read-directory-name "Root dir:" root nil t))
      (when (file-remote-p root)
        (setq root (with-parsed-tramp-file-name root file file-localname))))
    (setq sub-dir
          (cond ((eq (length sub-dirs) 1) (car (car sub-dirs)))
                ((null sub-dirs) "")
                (t (funcall my/choose-func
                            (mapcar 'car sub-dirs)
                            "Compile in: "))))
    (format "cd %s && %s"
            (concat root sub-dir) command)
    ))

(defun create-compile-command-repo()
  "Initialize the compile command."
  (interactive)
  (let ((root (or (profile-current-get 'project-root-dir) "./"))
        (command (or (profile-current-get 'compile-sub-command) "make"))
        (sub-dirs (profile-current-get 'build-sub-dirs))
        sub-dir)
    (when current-prefix-arg
      (setq root (read-directory-name "Root dir:" root nil t))
      (when (file-remote-p root)
        (setq root (with-parsed-tramp-file-name root file file-localname))))
    (setq sub-dir
          (cond ((eq (length sub-dirs) 1) (car (car sub-dirs)))
                ((null sub-dirs) "")
                (t (funcall my/choose-func
                            (mapcar 'car sub-dirs)
                            "Compile in: "))))
    (format "source %srepo-setup.sh && cd %s && %s"
            root (concat root sub-dir) command)
    ))

(defvar my/compile-command-functions-list
  (list 'create-compile-command-standard 'create-compile-command-repo)
  "List of functions to create compilation commands.")

(defvar my/compile-command-fn #'create-compile-command-standard
  "Default create compilation command.")

(defun my/choose-compile-command-function ()
  "Choose a compile command among `my/compile-command-functions-list'."
  (interactive)
  (let* ((lst (mapcar (lambda(cmd) (cons (symbol-name cmd) cmd))
                      my/compile-command-functions-list))
         (res (funcall my/choose-func
                      lst
                      "Choose the compile command function: ")))
    (when res
      (setq my/compile-command-fn res))))

(defvar should-close-compilation-window nil)

(defun my/compile()
  "A custom compilation command.  Allows customizing the compilation,
as well as the behavior of the `*compilation*' window upon completion."
  (interactive)
  (setq should-close-compilation-window
        (not (get-buffer-window "*compilation*" 'visible)))
  (when (setq my/compile-command (funcall my/compile-command-fn))
    (setq compile-command my/compile-command)
    (call-interactively 'compile)))

(defun my/recompile()
  "A custom re-compilation command."
  (interactive)
  (setq should-close-compilation-window
        (not (get-buffer-window "*compilation*" 'visible)))
  (call-interactively 'recompile))

(add-hook 'compilation-start-hook (lambda (process)
                                        ; the compile is about to start
                                    ))

(defun my/check-compile-buffer-cmake-werror ()
  "Ignores the compile line `-Werror' that cmake echoes."
  (save-match-data
    (looking-back "-W" (- (point) 2))))
(defun my/check-compile-buffer-boost-test-output ()
  "Ignores the BOOST test output `No errors detected'."
  (and
   (save-match-data
     (looking-back "[Nn]o " (- (point) 3)))
   (save-match-data
     (looking-at "errors detected"))))

(defvar my/ignore-compile-error-functions
  `(my/check-compile-buffer-cmake-werror
    my/check-compile-buffer-boost-test-output
    )
  "List of functions to filter compile errors.
If any return true, the current error is ignored."
  )

(defun check-compile-buffer-errors(buffer)
  "Check the current buffer for compile warnings or errors."
  (with-current-buffer buffer
    (catch 'found
      (goto-char 1)
      (while (search-forward-regexp "\\([Ww]arning\\|[Ee]rror\\)" nil t)
        (goto-char (match-beginning 1))
        (unless
            (-any 'funcall my/ignore-compile-error-functions)
          (throw 'found t))
        (goto-char (match-end 1)))
      nil)))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if it succeeded without warnings or errors."
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not (check-compile-buffer-errors buffer)))
      (run-with-timer 2 nil
                      (lambda (buf)
                        (let ((win (get-buffer-window buf t)))
                          (bury-buffer buf)
                          (if should-close-compilation-window
                              (delete-window win)
                            (switch-to-prev-buffer win 'kill))))
                      buffer)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; compiling.el ends here
