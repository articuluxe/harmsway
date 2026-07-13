;;; cppinsights.el --- Integration with cppinsights tool -*- lexical-binding: t; -*-

;; Author: Chris Chen <chrischen@ignity.xyz>
;; Version: 0.2
;; Keywords: c++, tools, cppinsights
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/ignity21/cppinsights.el
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
  "Additional arguments to pass to clang."
  :type '(repeat string)
  :group 'cppinsights)

(defcustom cppinsights-window-width 0.5
  "Width of the side window used to display cppinsights output.
A floating-point value between 0.0 and 1.0 is interpreted as a
fraction of the frame width.  An integer is interpreted as a
literal number of columns.  The value is passed through verbatim
to `display-buffer-in-side-window' via its `window-width' entry,
which already understands both forms."
  :type 'number
  :group 'cppinsights)

(defcustom cppinsights-output-mode
  (if (fboundp 'c++-ts-mode) #'c++-ts-mode #'c++-mode)
  "Major mode applied to the cppinsights output buffer.
Defaults to `c++-ts-mode' when available (Emacs 29+ with the C++
tree-sitter grammar installed) and falls back to the classic
`c++-mode' otherwise.  Set to any function of no arguments that
switches the current buffer into a major mode."
  :type 'function
  :group 'cppinsights)

(defun cppinsights--validate-file ()
  "Validate that current buffer is a C++ file with a filename.
Checks file extension to ensure it's a recognized C++ source or header file.
Returns the filename on success or signals an error if requirements aren't met."
  (let ((filename (buffer-file-name)))
    (unless filename
      (user-error "Buffer is not visiting a file"))

    (unless (let ((case-fold-search t))
              (string-match-p "\\.\\(?:cpp\\|cc\\|cxx\\|h\\|hpp\\|hxx\\)\\'"
                              filename))
      (user-error "Not a C++ file"))

    filename))

(defun cppinsights--project-root (filename)
  "Return the project root for FILENAME, or its directory if not in a project.
Used both to look up `compile_commands.json' for fallback-detection
and as `default-directory' when launching the `insights' subprocess,
so any relative paths in `cppinsights-clang-opts' and in compiler
diagnostics are anchored at a single, predictable location."
  (let ((proj (project-current)))
    (if proj
        (project-root proj)
      (file-name-directory filename))))

(defun cppinsights--build-command (filename)
  "Build the command to run cppinsights on FILENAME.
Detects if a compile_commands.json exists in the project root and uses
an appropriate command format based on this discovery.  Will use
project-provided compilation settings when available, otherwise
falls back to configured options."
  (let* ((proj-root (cppinsights--project-root filename))
         (use-compile-db (file-exists-p
                          (expand-file-name "compile_commands.json" proj-root)))
         (base (list cppinsights-program filename)))
    (if use-compile-db
        base
      (append base '("--") cppinsights-clang-opts))))

(defun cppinsights--handle-process-success (stdout-buffer stderr-buffer)
  "Handle successful cppinsights process.
Show STDOUT-BUFFER with C++ mode and clean up STDERR-BUFFER."
  (kill-buffer stderr-buffer)
  (with-current-buffer stdout-buffer
    (funcall cppinsights-output-mode)
    (read-only-mode 1)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (keymap-set map (kbd "q") #'kill-buffer-and-window)
      (use-local-map map))
    (display-buffer-in-side-window
     (current-buffer)
     `((side . right)
       (window-width . ,cppinsights-window-width))))
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
      (keymap-set map (kbd "q") #'kill-buffer-and-window)
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

(defun cppinsights--kill-stale-process (buffer)
  "Silently delete any live process still attached to BUFFER.
Called before launching a fresh run so the previous run's process
cannot keep writing into the buffers we are about to reuse.  The
sentinel is suppressed first so deleting the process does not
trigger the failure path with a spurious \"killed\" status."
  (let ((proc (get-buffer-process buffer)))
    (when (and proc (process-live-p proc))
      (set-process-sentinel proc #'ignore)
      (delete-process proc))))

;;;###autoload
(defun cppinsights-run ()
  "Run cppinsights on the current buffer and show results.
- Is there a compile_commands.json in project root? or in current directory?
  Run `insights compile_commands.json` on the current buffer.
- Or Run `insights <filename> -- <cppinsights-clang-opts>`
- If C++ insights failed, show the error in `compilation-mode'."
  (interactive)
  (let ((buffer-name (buffer-name)))
    (when (buffer-modified-p)
      (if (yes-or-no-p
           (format "Buffer %s is modified.  Save changes?" buffer-name))
          (save-buffer)
        (user-error "Changes must be saved before running cppinsights")))
    (let* ((filename (cppinsights--validate-file))
           (proj-root (cppinsights--project-root filename))
           (stdout-buffer-name (format "*C++ Insights %s*" buffer-name))
           (stderr-buffer-name (format "*C++ Insights %s* stderr" buffer-name))
           (stdout-buffer (get-buffer-create stdout-buffer-name))
           (stderr-buffer (get-buffer-create stderr-buffer-name))
           (command (cppinsights--build-command filename)))
      (cppinsights--kill-stale-process stdout-buffer)
      (cppinsights--kill-stale-process stderr-buffer)
      (cppinsights--erase-buffer stdout-buffer)
      (cppinsights--erase-buffer stderr-buffer)

      ;; Pin `default-directory' on both output buffers to the project
      ;; root so it matches the subprocess's cwd (see below).  This keeps
      ;; `compilation-mode' jumps in the error buffer aligned with clang's
      ;; own relative paths, which are emitted relative to that same root.
      (with-current-buffer stdout-buffer (setq default-directory proj-root))
      (with-current-buffer stderr-buffer (setq default-directory proj-root))

      ;; Start the process (no buffer displayed initially).
      ;; Bind `default-directory' to the project root for predictability:
      ;; relative paths in `cppinsights-clang-opts' and in any diagnostics
      ;; from clang are anchored there, rather than to the source file's
      ;; subdirectory.  (libclang Tooling's lookup of `compile_commands.json'
      ;; is independent of cwd -- it walks up from FILENAME's own path.)
      (let* ((default-directory proj-root)
             (proc (make-process
                    :name "C++ Insights"
                    :buffer stdout-buffer
                    :command command
                    :stderr stderr-buffer
                    :connection-type 'pipe
                    :sentinel #'cppinsights--process-sentinel)))
        ;; Store additional information for use in the sentinel
        (process-put proc 'stderr-buffer stderr-buffer)))))

(provide 'cppinsights)
;;; cppinsights.el ends here
