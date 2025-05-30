;;; cppinsights.el --- Integration with cppinsights tool -*- lexical-binding: t; -*-

;; Author: Chris Chen <chrischen@ignity.xyz>
;; Version: 0.1
;; Keywords: c++, tools, cppinsights
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/chrischen3121/cppinsights.el
;; SPDX-License-Identifier: Apache-2.0

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This package provides integration with the cppinsights command-line tool.
;; It allows you to run cppinsights on the current buffer and view the results
;; in a popup buffer.
;;
;; Usage:
;; Call `M-x cppinsights-run` when visiting a C++ file to analyze it with cppinsights.
;; The results will be displayed in a side window.

;;; Code:
(require 'cc-mode)
(require 'project)

(defgroup cppinsights nil
  "Integration with cppinsights tool."
  :group 'cppinsights)

(defcustom cppinsights-program "insights"
  "The name or path of the cppinsights program."
  :type 'string
  :group 'cppinsights)

(defcustom cppinsights-clang-opts '("-O0" "-std=c++20")
  "Additional arguments to pass to clangs."
  :type '(repeat string)
  :group 'cppinsights)

(defvar cppinsights--window-width-percent 0.4
  "Width of the side window for displaying cppinsights results.")

(defun cppinsights--validate-file ()
  "Validate that current buffer is a C++ file with a filename.
Checks file extension to ensure it's a recognized C++ source or header file.
Returns the filename on success or signals an error if requirements aren't met."
  (let ((filename (buffer-file-name)))
    (unless filename
      (user-error "Buffer is not visiting a file"))
    
    (unless (string-match-p "\\.\\(cpp\\|cc\\|cxx\\|h\\|hpp\\|hxx\\)$" filename)
      (user-error "Not a C++ file"))
    
    filename))

(defun cppinsights--build-command (filename)
  "Build the command to run cppinsights on FILENAME.
Detects if a compile_commands.json exists in the project root and uses
an appropriate command format based on this discovery.  Will use
project-provided compilation settings when available, otherwise
falls back to configured options."
  (let* ((current-dir (file-name-directory filename))
         (current_proj (project-current))
         (proj-root (if current_proj
                        (project-root current_proj)
                      current-dir))
         (use-compile-db (file-exists-p
                          (expand-file-name "compile_commands.json" proj-root))))
    (if use-compile-db
        (append (list cppinsights-program)
                (list filename))
      (append (list cppinsights-program)
              (list filename)
              '("--")
              cppinsights-clang-opts))))

(defun cppinsights--handle-process-success (stdout-buffer stderr-buffer)
  "Handle successful cppinsights process.
Show STDOUT-BUFFER with C++ mode and clean up STDERR-BUFFER."
  (kill-buffer stderr-buffer)
  (with-current-buffer stdout-buffer
    (c++-mode)
    (read-only-mode 1)
    (let ((map (make-sparse-keymap)))
      (keymap-set map (kbd "q") 'kill-buffer-and-window)
      (use-local-map map))
    (display-buffer-in-side-window
     (current-buffer)
     `((side . right)
       (window-width . ,cppinsights--window-width-percent))))
  (select-window (get-buffer-window stdout-buffer))
  (goto-char (point-min)))

(defun cppinsights--handle-process-error (stdout-buffer stderr-buffer)
  "Handle failed cppinsights process.
STDOUT-BUFFER is the buffer with stdout content (which is discarded).
STDERR-BUFFER is the buffer with stderr content, displayed in compilation mode
to provide error navigation and context about the failure."
  (kill-buffer stdout-buffer)
  (with-current-buffer stderr-buffer
    (compilation-mode)
    (read-only-mode 1)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (keymap-set map (kbd "q") 'kill-buffer-and-window)
      (use-local-map map))
    (display-buffer-at-bottom
     (current-buffer)
     '((window-height . 0.3))))
  (select-window (get-buffer-window stderr-buffer))
  (goto-char (point-min)))

(defun cppinsights--process-sentinel (process _ignored)
  "Handle the completion of the cppinsights process.
PROCESS is the process object.
On success (exit code 0), displays formatted C++ output in a side window.
On failure, displays error messages in compilation mode for easier navigation."
  (let ((status (process-exit-status process))
        (stdout-buffer (process-buffer process))
        (stderr-buffer (process-get process 'stderr-buffer)))
    
    (if (= status 0)
        (cppinsights--handle-process-success stdout-buffer stderr-buffer)
      (cppinsights--handle-process-error stdout-buffer stderr-buffer))))

(defun cppinsights--erase-buffer (buffer)
  "Erase the contents of BUFFER.
Temporarily disables read-only mode if enabled to ensure
contents can be cleared."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;;###autoload
(defun cppinsights-run ()
  "Run cppinsihgts on the current buffer and show results.
- Is there a complile_commands.json in project root? or in current directory?
  Run `insights compile_commands.json` on the current buffer.
- Or Run `insights <filename> -- <cppinsights-clang-opts>`
- If C++ insights failed, show the error in `compilation-mode`."
  (interactive)
  (let ((buffer-name (buffer-name)))
    (if (or (not (buffer-modified-p))
            (yes-or-no-p
             (format "Buffer %s is modified.  Save changes?" buffer-name)))
        (save-buffer)
      (user-error "Changes must be saved before running cppinsights")))
  (let* ((filename (cppinsights--validate-file))
         (buffer-name (buffer-name))
         (stdout-buffer-name (format "*C++ Insights %s*" buffer-name))
         (stderr-buffer-name (format "*C++ Insights %s* stderr" buffer-name))
         (stdout-buffer (get-buffer-create stdout-buffer-name))
         (stderr-buffer (get-buffer-create stderr-buffer-name))
         (command (cppinsights--build-command filename))
         (proc nil))
    (cppinsights--erase-buffer stdout-buffer)
    (cppinsights--erase-buffer stderr-buffer)

    ;; Start the process (no buffer displayed initially)
    (setq proc (make-process
                :name "C++ Insights"
                :buffer stdout-buffer
                :command command
                :stderr stderr-buffer
                :connection-type 'pipe
                :sentinel #'cppinsights--process-sentinel))
    
    ;; Store additional information for use in the sentinel
    (process-put proc 'stderr-buffer stderr-buffer)))

(provide 'cppinsights)
;;; cppinsights.el ends here
