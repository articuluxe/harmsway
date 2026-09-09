;;; context-clues.el --- Easily copy context like the current file name and path -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Marcin Swieczkowski

;; Author: Marcin Swieczkowski <marcin@realemail.net>
;; Assisted-by: Claude:claude-opus-4-8
;; Assisted-by: Claude:claude-fable-5
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1") (transient "0.3.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/mrcnski/context-clues

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

;; context-clues provides a convenient transient menu for copying various file,
;; buffer, and code context information to the kill ring.  Useful for e.g.
;; communicating context with LLMs.
;;
;; Usage:
;;   M-x context-clues
;;
;; This opens a transient menu with options for copying file names, paths,
;; line numbers, function names, git branches, and more.  Each entry shows
;; a live preview of the value it would copy.  Options that are not
;; applicable to the current buffer will be grayed out.
;;
;; See the README for the full list of features and keybindings.

;;; Code:

(require 'imenu)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'which-func)

(defgroup context-clues nil
  "Copy file, buffer, and context information."
  :group 'convenience
  :prefix "context-clues-")

(defcustom context-clues-message-format "Copied {description}: {text}"
  "Format string for the message shown after copying.
Use {text} for the copied text and {description} for the description.
Example: \"Copied: {text} ({description})\" or \"{description}: {text}\""
  :type 'string
  :group 'context-clues)

(defcustom context-clues-breadcrumb-separator " > "
  "Separator between the components of the breadcrumb clue."
  :type 'string
  :group 'context-clues)

(defcustom context-clues-preview-max-width 50
  "Maximum width of the value previews shown in the menu.
A longer value is truncated at the front, keeping its tail -- for paths,
the most distinctive part."
  :type 'integer
  :group 'context-clues)

(defface context-clues-preview-face
  '((t :inherit shadow))
  "Face for the value previews shown next to each clue in the menu."
  :group 'context-clues)

;;; Helper Functions

(defun context-clues--in-git-repo-p ()
  "Return non-nil if current buffer is in a git repository."
  (locate-dominating-file default-directory ".git"))

(defun context-clues--copy-to-kill-ring (text description)
  "Copy TEXT to kill ring and display TEXT and DESCRIPTION."
  (kill-new text)
  (let ((msg (string-replace "{text}" text
                             (string-replace "{description}" description
                                             context-clues-message-format))))
    (message "%s" msg)))

;;; Clue Values

;; One function per clue, returning the string that would be copied, or
;; nil when the clue does not apply to the current buffer.  The copy
;; commands and the menu previews are both built on these.

(defun context-clues--file-name ()
  "The base file name, or nil for a non-file buffer."
  (when-let ((file-name (buffer-file-name)))
    (file-name-nondirectory file-name)))

(defun context-clues--full-path ()
  "The absolute file path, or nil for a non-file buffer."
  (buffer-file-name))

(defun context-clues--directory ()
  "The file's directory, or `default-directory' for a non-file buffer."
  (if-let ((file-name (buffer-file-name)))
      (file-name-directory file-name)
    default-directory))

(defun context-clues--relative-path (file-name)
  "Return FILE-NAME relative to the project root.
Both FILE-NAME and the project root are resolved with `file-truename'
first, so a symlinked tree (e.g. a dotfiles repo linked into the home
directory) does not produce a path full of leading \"../\".  Outside a
project, the path is relative to `default-directory' instead."
  (let ((project-root (or (when-let ((project (project-current)))
                            (project-root project))
                          default-directory)))
    (file-relative-name (file-truename file-name)
                        (file-truename project-root))))

(defun context-clues--relative-path-value ()
  "The project-relative file path, or nil for a non-file buffer."
  (when-let ((file-name (buffer-file-name)))
    (context-clues--relative-path file-name)))

(defun context-clues--file-with-line ()
  "The base file name with line number (file.el:123), or nil."
  (when-let ((base-name (context-clues--file-name)))
    (format "%s:%d" base-name (line-number-at-pos))))

(defun context-clues--relative-path-with-line ()
  "The project-relative path with line number (path/file.el:123), or nil."
  (when-let ((relative-path (context-clues--relative-path-value)))
    (format "%s:%d" relative-path (line-number-at-pos))))

(defun context-clues--project-name ()
  "The current project's directory name, or nil outside a project."
  (when-let* ((project (project-current))
              (project-root (project-root project)))
    (file-name-nondirectory (directory-file-name project-root))))

(defun context-clues--buffer-name ()
  "The current buffer name."
  (buffer-name))

(defun context-clues--git-branch ()
  "The current git branch, or nil when absent or undeterminable."
  (when (context-clues--in-git-repo-p)
    (let ((branch (string-trim
                   (shell-command-to-string
                    "git symbolic-ref --short HEAD 2>/dev/null"))))
      (unless (string-empty-p branch) branch))))

(defun context-clues--line-number ()
  "The current line number, as a string."
  (number-to-string (line-number-at-pos)))

(defun context-clues--function-name ()
  "The name of the function at point, or nil when undeterminable."
  (ignore-errors (which-function)))

(declare-function org-get-outline-path "org" (&optional with-self use-cache))
(declare-function treesit-parser-list "treesit.c")
(declare-function treesit-add-log-current-defun "treesit")
(defvar treesit-add-log-defun-delimiter)

(defun context-clues--imenu-join-parts (parts)
  "Join imenu PARTS with the breadcrumb separator, or nil for no parts.
Nested imenu indexes (e.g. `markdown-mode') use a \".\" leaf entry for a
section's own heading; it adds nothing to the path and is dropped."
  (when-let ((parts (seq-remove (apply-partially #'equal ".") parts)))
    (string-join parts context-clues-breadcrumb-separator)))

(defun context-clues--function-path ()
  "The full path of nested defuns or sections at point, or nil.
In tree-sitter enabled buffers, every enclosing defun contributes a
component (e.g. class, then method), joined with
`context-clues-breadcrumb-separator'.  Elsewhere the path comes from the
buffer's (possibly nested) imenu index via `which-function' -- for
example the heading path in `markdown-mode'."
  (or (and (fboundp 'treesit-parser-list)
           (treesit-parser-list)
           (ignore-errors
             (let ((treesit-add-log-defun-delimiter
                    context-clues-breadcrumb-separator))
               (treesit-add-log-current-defun))))
      (progn
        ;; `which-function' only consults an already-built imenu index.
        (unless imenu--index-alist
          (ignore-errors (imenu--make-index-alist t)))
        (let ((which-func-imenu-joiner-function
               #'context-clues--imenu-join-parts))
          (context-clues--function-name)))))

(defun context-clues--breadcrumb ()
  "The full breadcrumb: relative path, then the context path at point.
The components are joined with `context-clues-breadcrumb-separator'.  In
Org buffers the context is the complete outline path down to the current
heading; elsewhere it is the path of nested defuns at point, when there
is one.  Nil for a non-file buffer."
  (when-let ((relative-path (context-clues--relative-path-value)))
    (let ((context-path
           (if (derived-mode-p 'org-mode)
               ;; Errors before the first headline.
               (ignore-errors (org-get-outline-path t))
             (when-let ((function-path (context-clues--function-path)))
               (list function-path)))))
      (string-join (cons relative-path context-path)
                   context-clues-breadcrumb-separator))))

;;; Copy Functions

(defun context-clues-copy-file-name ()
  "Copy the base file name of the current buffer."
  (interactive)
  (if-let ((base-name (context-clues--file-name)))
      (context-clues--copy-to-kill-ring base-name "file name")
    (user-error "Buffer is not visiting a file")))

(defun context-clues-copy-full-path ()
  "Copy the absolute file path of the current buffer."
  (interactive)
  (if-let ((file-name (context-clues--full-path)))
      (context-clues--copy-to-kill-ring file-name "full path")
    (user-error "Buffer is not visiting a file")))

(defun context-clues-copy-directory ()
  "Copy the directory path of the current buffer.
For file-visiting buffers, copies the file's directory.
For non-file-visiting buffers, copies the default directory."
  (interactive)
  (context-clues--copy-to-kill-ring (context-clues--directory) "directory"))

(defun context-clues-copy-relative-path ()
  "Copy the relative file path from project root."
  (interactive)
  (if-let ((relative-path (context-clues--relative-path-value)))
      (context-clues--copy-to-kill-ring relative-path "relative path")
    (user-error "Buffer is not visiting a file")))

(defun context-clues-copy-file-with-line ()
  "Copy the file name with line number (e.g., file.el:123)."
  (interactive)
  (if-let ((file-with-line (context-clues--file-with-line)))
      (context-clues--copy-to-kill-ring file-with-line "file with line")
    (user-error "Buffer is not visiting a file")))

(defun context-clues-copy-relative-path-with-line ()
  "Copy the relative path with line number (e.g., path/file.el:123)."
  (interactive)
  (if-let ((path-with-line (context-clues--relative-path-with-line)))
      (context-clues--copy-to-kill-ring path-with-line "relative path with line")
    (user-error "Buffer is not visiting a file")))

(defun context-clues-copy-project-name ()
  "Copy the current project name."
  (interactive)
  (if-let ((project-name (context-clues--project-name)))
      (context-clues--copy-to-kill-ring project-name "project name")
    (user-error "Not in a project")))

(defun context-clues-copy-buffer-name ()
  "Copy the current buffer name."
  (interactive)
  (context-clues--copy-to-kill-ring (context-clues--buffer-name) "buffer name"))

(defun context-clues-copy-git-branch ()
  "Copy the current git branch name."
  (interactive)
  (unless (context-clues--in-git-repo-p)
    (user-error "Buffer is not in a git repository"))
  (if-let ((branch (context-clues--git-branch)))
      (context-clues--copy-to-kill-ring branch "git branch")
    (user-error "Could not determine git branch")))

(defun context-clues-copy-line-number ()
  "Copy the current line number."
  (interactive)
  (context-clues--copy-to-kill-ring (context-clues--line-number) "line number"))

(defun context-clues-copy-function-name ()
  "Copy the current function name."
  (interactive)
  (if-let ((func-name (context-clues--function-name)))
      (context-clues--copy-to-kill-ring func-name "function name")
    (user-error "Could not determine current function")))

(defun context-clues-copy-breadcrumb ()
  "Copy the full breadcrumb: relative path, then the context path at point."
  (interactive)
  (if-let ((breadcrumb (context-clues--breadcrumb)))
      (context-clues--copy-to-kill-ring breadcrumb "breadcrumb")
    (user-error "Buffer is not visiting a file")))

;;; Menu Descriptions

(defconst context-clues--label-width 25
  "Column where the value previews start in the menu.")

(defun context-clues--preview (value)
  "Return VALUE truncated and faced for display in the menu, or nil.
A value wider than `context-clues-preview-max-width' keeps its tail,
with a leading ellipsis."
  (when value
    (let ((len (length value))
          (max-width (max 2 context-clues-preview-max-width)))
      (propertize (if (> len max-width)
                      (concat "…" (substring value (- len (1- max-width))))
                    value)
                  'face 'context-clues-preview-face))))

(defun context-clues--describe (label value)
  "Return a menu description: LABEL, then a preview of VALUE.
Labels are padded to `context-clues--label-width' so the previews line
up as a column.  With a nil VALUE (an inapplicable clue), the bare LABEL
is returned."
  (if-let ((preview (context-clues--preview value)))
      (format (format "%%-%ds%%s" context-clues--label-width) label preview)
    label))

(defun context-clues--describe-file-name ()
  "Menu description for the file-name clue."
  (context-clues--describe "File name" (context-clues--file-name)))

(defun context-clues--describe-relative-path ()
  "Menu description for the relative-path clue."
  (context-clues--describe "Relative path" (context-clues--relative-path-value)))

(defun context-clues--describe-full-path ()
  "Menu description for the full-path clue."
  (context-clues--describe "Full path" (context-clues--full-path)))

(defun context-clues--describe-directory ()
  "Menu description for the directory clue."
  (context-clues--describe "Directory" (context-clues--directory)))

(defun context-clues--describe-file-with-line ()
  "Menu description for the file-with-line clue."
  (context-clues--describe "File with line" (context-clues--file-with-line)))

(defun context-clues--describe-relative-path-with-line ()
  "Menu description for the relative-path-with-line clue."
  (context-clues--describe "Relative path with line"
                           (context-clues--relative-path-with-line)))

(defun context-clues--describe-project-name ()
  "Menu description for the project-name clue."
  (context-clues--describe "Project name" (context-clues--project-name)))

(defun context-clues--describe-buffer-name ()
  "Menu description for the buffer name clue."
  (context-clues--describe "Buffer name" (context-clues--buffer-name)))

(defun context-clues--describe-git-branch ()
  "Menu description for the git-branch clue."
  (context-clues--describe "Git branch" (context-clues--git-branch)))

(defun context-clues--describe-line-number ()
  "Menu description for the line-number clue."
  (context-clues--describe "Line number" (context-clues--line-number)))

(defun context-clues--describe-function-name ()
  "Menu description for the function-name clue."
  (context-clues--describe "Function name" (context-clues--function-name)))

(defun context-clues--describe-breadcrumb ()
  "Menu description for the breadcrumb clue."
  (context-clues--describe "Breadcrumb" (context-clues--breadcrumb)))

;;; Transient Menu

;;;###autoload
(transient-define-prefix context-clues ()
  "Copy file, buffer, and context information."
  ;; Each clue is grayed out exactly when its value function returns nil
  ;; -- the same condition that leaves it without a preview.
  ["File & Path"
   ("f" context-clues-copy-file-name
    :description context-clues--describe-file-name
    :inapt-if-not context-clues--file-name)
   ("r" context-clues-copy-relative-path
    :description context-clues--describe-relative-path
    :inapt-if-not context-clues--relative-path-value)
   ("F" context-clues-copy-full-path
    :description context-clues--describe-full-path
    :inapt-if-not context-clues--full-path)
   ("d" context-clues-copy-directory
    :description context-clues--describe-directory)
   (":" context-clues-copy-file-with-line
    :description context-clues--describe-file-with-line
    :inapt-if-not context-clues--file-with-line)
   (";" context-clues-copy-relative-path-with-line
    :description context-clues--describe-relative-path-with-line
    :inapt-if-not context-clues--relative-path-with-line)
   ("p" context-clues-copy-project-name
    :description context-clues--describe-project-name
    :inapt-if-not context-clues--project-name)]
  ["Buffer & Context"
   ("b" context-clues-copy-buffer-name
    :description context-clues--describe-buffer-name)
   ("g" context-clues-copy-git-branch
    :description context-clues--describe-git-branch
    :inapt-if-not context-clues--git-branch)
   ("l" context-clues-copy-line-number
    :description context-clues--describe-line-number)
   ("n" context-clues-copy-function-name
    :description context-clues--describe-function-name
    :inapt-if-not context-clues--function-name)
   (">" context-clues-copy-breadcrumb
    :description context-clues--describe-breadcrumb
    :inapt-if-not context-clues--breadcrumb)])

(provide 'context-clues)

;;; context-clues.el ends here
