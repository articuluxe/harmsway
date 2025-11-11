;;; electric-list-directory.el --- Lightweight popup directory browser -*- lexical-binding: t; -*-
;;
;; Author: K. Shane Hartman <shane@ai.mit.edu>
;; Version: 1.4.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: files, convenience
;; URL: https://github.com/kshartman/electric-directory-list
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; Commentary:
;;
;; Lightweight “electric” popup directory browser.
;;
;; Features:
;; - Popup buffer: *Electric Directory* (reused every time).
;; - Header line shows the current directory (abbreviated).
;; - RET on a file opens it and exits; RET on a directory drills into it.
;; - d deletes file/dir at point (with prompt) and refreshes in place.
;; - ~ deletes backup/autosave files (*~ and #*#) and refreshes.
;; - Backspace (DEL) goes up one directory level.
;; - SPC or q quits and restores your previous window layout.
;; - With a prefix argument, run plain `list-directory`.
;;
;; Installation:
;;
;; Manual:
;;   (require 'electric-list-directory)
;;   ;; Bind to C-x C-d (Note: this replaces the default `list-directory` binding):
;;   (global-set-key (kbd "C-x C-d") #'electric-list-directory)
;;
;; With use-package:
;;   (use-package electric-list-directory
;;     :bind ("C-x C-d" . electric-list-directory))  ;; Replaces default `list-directory`
;;
;; Usage:
;;   M-x electric-list-directory
;;
;;; Code:

(defgroup electric-list-directory nil
  "Lightweight popup directory browser."
  :group 'files
  :prefix "electric-list-directory-")


(defvar-local electric-list-directory--winconf nil
  "Saved window configuration for restoring after quit.")

(defvar-local electric-list-directory--dir nil
  "Directory currently shown in *Electric Directory*.")

(defvar-local electric-list-directory--switches nil
  "Switches used to render the listing (sanitized).")

(define-derived-mode electric-list-directory-mode special-mode "Electric-Dir"
  "Read-only directory listing with quick navigation and delete commands.")

(let ((m electric-list-directory-mode-map))
  (define-key m (kbd "q")   #'electric-list-directory-quit)
  (define-key m (kbd "SPC") #'electric-list-directory-quit)  ;; Space always exits
  (define-key m (kbd "RET") #'electric-list-directory-visit)
  (define-key m (kbd "DEL") #'electric-list-directory-up)
  (define-key m (kbd "n")   #'next-line)
  (define-key m (kbd "p")   #'previous-line)
  (define-key m (kbd "~")   #'electric-list-directory-delete-backups)
  (define-key m (kbd "d")   #'electric-list-directory-delete-at-point))

(defun electric-list-directory--sanitize-switches (sw)
  "Return ls flags suitable for Electric view from SW.
Remove flags that break parsing (-d and -F / --directory and --classify),
but allow columns.  Keep user layout (columns -C/-x/-m or long -l)."
  (let* ((src (or sw list-directory-brief-switches))
         (tokens (split-string src "[ \t]+" t))
         (out '()))
    (dolist (tok tokens)
      (cond
       ;; Long options: drop only ones that alter names or list the dir itself.
       ((string-prefix-p "--" tok)
        (unless (member tok '("--directory" "--classify"))
          (push tok out)))
       ;; Short bundles: strip d (dir-only) and F (classify); keep the rest.
       ((string-prefix-p "-" tok)
        (let* ((flags (substring tok 1))
               (flags (replace-regexp-in-string "[dF]" "" flags)))
          (unless (string= flags "")
            (push (concat "-" flags) out))))
       ;; Anything else (rare) — keep.
       (t (push tok out))))
    (mapconcat #'identity (nreverse out) " ")))

(defun electric-list-directory--update-header ()
  "Update the header line to show the current directory."
  (setq header-line-format
        (concat "  📁 "
                (abbreviate-file-name
                 (or electric-list-directory--dir default-directory)))))

(defun electric-list-directory--refresh ()
  "Re-render the listing for current buffer settings."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Force content listing (FULL-DIRECTORY-P = t).
    (insert-directory electric-list-directory--dir
                      electric-list-directory--switches
                      nil t)
    (goto-char (point-min))
    (setq buffer-read-only t)
    (setq default-directory electric-list-directory--dir)
    (electric-list-directory--update-header)))

;;;###autoload
(defun electric-list-directory (dirname &optional switches)
  "Browse DIRNAME in a temporary electric directory buffer.
With a prefix argument, run plain `list-directory` instead.
If SWITCHES is supplied, use that for list directory."
  (interactive
   (list (read-directory-name "Electric List Directory: " nil default-directory t)
         (when current-prefix-arg
           (read-string "ls switches: " list-directory-brief-switches))))
  (if current-prefix-arg
      (list-directory dirname switches)
    (let* ((buf-name "*Electric Directory*")
           (buf  (get-buffer-create buf-name))
           (conf (current-window-configuration))
           (sw   (electric-list-directory--sanitize-switches switches))
           (dir  (file-name-as-directory (expand-file-name dirname))))
      (with-current-buffer buf
        (electric-list-directory-mode)
        (setq electric-list-directory--winconf  conf
              electric-list-directory--dir      dir
              electric-list-directory--switches sw)
        (electric-list-directory--refresh))
      (pop-to-buffer buf))))

(defun electric-list-directory--filename-at-point ()
  "Return absolute filename under point for columns or long listings.

- For `ls -l` style lines (permission char at col 0), take the last field;
  if it's a symlink (\"NAME -> TARGET\"), use NAME.
- For columnar output, take the token under point, where columns are
  separated by runs of 2+ spaces. Strip any -F/-p suffixes."
  (save-excursion
    (let* ((bol  (line-beginning-position))
           (eol  (line-end-position))
           (line (buffer-substring-no-properties bol eol)))
      ;; Ignore headers like "total N" and blank lines.
      (when (and (not (string-match-p "\\`[[:space:]]*\\'" line))
                 (not (string-prefix-p "total " line)))
        (cond
         ;; Long listing (starts with file type/perm bit)
         ((string-match-p "^[bcdlps-]" line)
          (when (string-match "\\s-\\{2,\\}\\(.+\\)$" line)
            (let ((name (match-string 1 line)))
              ;; If it's "NAME -> TARGET", keep NAME only.
              (when (string-match "\\`\\(.+?\\)\\s-*->\\s-.*\\'" name)
                (setq name (match-string 1 name)))
              ;; Strip any -F/-p trailing markers and slashes.
              (when (string-match "\\(.*?\\)[*/=>@|/]\\'" name)
                (setq name (match-string 1 name)))
              (expand-file-name name default-directory))))
         ;; Columnar output
         (t
          (let* ((pt (point)) start end token)
            ;; start: just after previous 2+ spaces (or BOL)
            (goto-char pt)
            (if (re-search-backward "[[:space:]]\\{2,\\}" bol t)
                (setq start (match-end 0))
              (setq start bol))
            ;; end: just before next 2+ spaces (or EOL)
            (goto-char pt)
            (if (re-search-forward "[[:space:]]\\{2,\\}" eol t)
                (setq end (match-beginning 0))
              (setq end eol))
            (setq token (buffer-substring-no-properties start end))
            ;; trim whitespace without requiring subr-x
            (setq token (replace-regexp-in-string
                         "\\`[[:space:]]+\\|[[:space:]]+\\'" "" token))
            (when (not (string= token ""))
              ;; Strip -F classification suffixes and trailing slash from -p.
              (when (string-match "\\(.*?\\)[*/=>@|/]\\'" token)
                (setq token (match-string 1 token)))
              (expand-file-name token default-directory)))))))))

(defun electric-list-directory-visit ()
  "If on a directory, drill into it; if on a file, visit and exit."
  (interactive)
  (let ((path (electric-list-directory--filename-at-point)))
    (cond
     ((null path)
      (message "No file at point"))
     ((file-directory-p path)
      (setq electric-list-directory--dir
            (file-name-as-directory (expand-file-name path)))
      (electric-list-directory--refresh)
      (message "Entered %s" electric-list-directory--dir))
     (t
      ;; Open the file in this window and close the popup.
      ;; DO NOT restore the saved window configuration here.
      (find-file path)
      (when (get-buffer "*Electric Directory*")
        (kill-buffer "*Electric Directory*"))))))

(defun electric-list-directory-up ()
  "Go up one directory level."
  (interactive)
  (let ((parent (file-name-directory
                 (directory-file-name electric-list-directory--dir))))
    (setq electric-list-directory--dir
          (file-name-as-directory (expand-file-name parent)))
    (electric-list-directory--refresh)
    (message "Up to %s" electric-list-directory--dir)))

(defun electric-list-directory-delete-at-point ()
  "Prompt and delete the file or directory at point, then refresh."
  (interactive)
  (let ((file (electric-list-directory--filename-at-point)))
    (cond
     ((not file)
      (message "No file at point"))
     ((file-directory-p file)
      (when (y-or-n-p (format "Delete directory %s recursively? "
                              (file-name-nondirectory
                               (directory-file-name file))))
        (delete-directory file t)
        (message "Deleted %s" file)
        (electric-list-directory--refresh)))
     (t
      (when (y-or-n-p (format "Delete file %s? "
                              (file-name-nondirectory file)))
        (delete-file file)
        (message "Deleted %s" file)
        (electric-list-directory--refresh))))))

(defun electric-list-directory-delete-backups ()
  "Delete backup and autosave files in the current directory, then refresh."
  (interactive)
  (let* ((dir (or electric-list-directory--dir default-directory))
         (cands (append (file-expand-wildcards (expand-file-name "*~" dir) t)
                        (file-expand-wildcards (expand-file-name "#*#" dir) t))))
    (if (null cands)
        (message "No backup/autosave files in %s" dir)
      (when (y-or-n-p (format "Delete %d backup/autosave files in %s? "
                              (length cands) dir))
        (dolist (f cands)
          (ignore-errors (delete-file f)))
        (message "Deleted %d backup/autosave files" (length cands))
        (electric-list-directory--refresh)))))

(defun electric-list-directory-quit ()
  "Quit the electric directory buffer and restore previous windows."
  (interactive)
  (let ((conf electric-list-directory--winconf))
    (when (get-buffer "*Electric Directory*")
      (bury-buffer "*Electric Directory*"))
    (when conf
      (set-window-configuration conf))
    (message nil)))

(provide 'electric-list-directory)

;;; electric-list-directory.el ends here
