;;; dired-duplicates.el --- Find duplicate files locally and remotely  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: Harald Judt <h.judt@gmx.at>
;; Maintainer: Harald Judt <h.judt@gmx.at>
;; Created: 2022
;; Version: 0.4
;; Package-Requires: ((emacs "27.1"))
;; Keywords: files
;; Homepage: https://codeberg.org/hjudt/dired-duplicates

;; This file is part of GNU Emacs.

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

;; This package helps to find duplicate files on local and remote filesystems.
;; It is similar to the fdupes command-line utility but written in Emacs Lisp
;; and should also work on every remote filesystem that TRAMP supports and
;; where executable commands can be called remotely.  The only external
;; requirement is a checksum program like md5 or sha256sum that generates a
;; hash value from the contents of a file used for comparison, because Emacs
;; cannot do that in a performance-efficient way.
;;
;; dired-duplicates works by first searching files of the same size, then
;; invoking the calculation of the checksum for these files, and presents the
;; grouped results in a Dired buffer that the user can work with similarly to
;; a regular Dired buffer.

;;; Code:

(require 'cl-lib)
(require 'dired)

(defgroup dired-duplicates
  nil
  "Find duplicate files on local and/or remote filesystems."
  :tag "Dired Duplicates"
  :group 'dired)

(defcustom dired-duplicates-separate-results
  t
  "Boolean value indicating whether to separate results with new-lines."
  :tag "Separate results"
  :type 'boolean)

(defcustom dired-duplicates-checksum-exec
  "sha256sum"
  "Name of the executable used for creating file checksums.

The checksums will be used for comparison of files of the same
size."
  :tag "Checksum executable"
  :type 'string)

(defcustom dired-duplicates-external-internal-algo-mapping
  '(("sha512sum" . sha512)
    ("sha384sum" . sha384)
    ("sha256sum" . sha256)
    ("sha224sum" . sha224)
    ("sha1sum" . sha1)
    ("md5sum" . md5))
  "Mappings of checksum execs to internal secure hash algorithms.

These mappings will be used in fallback cases to determine the
secure hash function to use when the desired checksum
executable (see `dired-duplicates-checksum-exec') cannot be
found."
  :tag "Checksum exec to internal algo mappings."
  :type 'list)

(defcustom dired-duplicates-internal-checksumming-size-limit
  (let ((mb (* 1024 1024)))
    ;; 1024MiB for 64-bit systems, 512MiB for 32-bit
    (if (or (< most-positive-fixnum (* 2.0 1024 mb))
            ;; 32-bit system with wide ints
            (string-match-p "--with-wide-int" system-configuration-options))
        (* 512 mb)
      (* 1024 mb)))
  "File size in bytes above which internal checksumming will not be used.

If the size of a file exceeds this limit, a warning will be
issued and checksumming using internal functions will not be
done, resulting in ignoring the file.  It has no effect on
checksumming using an executable.  This limit exists to prevent
out-of-memory situations, where the Emacs process becomes
unresponsive or gets killed."
  :tag "Internal checksumming file size limit"
  :type 'integer)

(defcustom dired-duplicates-size-comparison-function
  '<
  "The comparison function used for sorting grouped results.

The sorting can be in ascending (<) or descending (>) order."
  :tag "Ascending or descending file size sort order"
  :type '(choice (const :tag "Ascending" <)
                 (const :tag "Descending" >)))

(defcustom dired-duplicates-file-filter-functions
  '(dired-duplicates--file-filter-readable-and-regular)
  "Filter functions applied to all files found in a directory.

A filter function must accept as its single argument the file and
return boolean t if the file matches a criteria, otherwise nil."
  :tag "File filter functions"
  :type 'hook)

(defcustom dired-duplicates-search-directories-recursively
  t
  "Search directories recursively."
  :tag "Search directories recursively"
  :type 'boolean)

(defvar dired-duplicates-directories nil
  "List of directories that will be searched for duplicate files.")


(defun dired-duplicates--checksum-file (file &optional exec current total)
  "Create a checksum for FILE, optionally using EXEC.

EXEC needs to be specified with its full path.  If nil, use the
internal function `secure-hash' with the appropriate algorithm,
which will be deduced from `dired-duplicates-checksum-exec' via
the `dired-duplicates-external-internal-algo-mapping'.  Using
`secure-hash' instead of spawning a process can be faster for
very small files and will work even when the TRAMP method used
does not provide a shell, but is usually slower and could cause
memory issues for files bigger than the Emacs process or the
machine can handle because they have to be loaded into a
temporary buffer for the hash calculation.

When provided, CURRENT and TOTAL are optionally used for printing
the progress in more detail.  CURRENT is the number of the file
currently being processed out of a TOTAL number of files."
  (let ((progress (if (and current total)
                      (format "(%i/%i) " current total)
                    "")))
    (if (not exec)
        (let ((message-log-max nil)
              (hash-algo (alist-get dired-duplicates-checksum-exec
                                    dired-duplicates-external-internal-algo-mapping
                                    nil nil #'string=)))
          (unless hash-algo
            (user-error "Could not determine the correct hash algorithm for %s via %s"
                        dired-duplicates-checksum-exec
                        "`dired-duplicates-external-internal-algo-mapping'"))
          (message "%sInternal checksumming of %s" progress file)
          (with-temp-buffer
            (let ((inhibit-message t))
              (insert-file-contents-literally file))
            (secure-hash hash-algo
                         (current-buffer))))
      (let* ((default-directory (file-name-directory (expand-file-name file)))
             (file (expand-file-name (file-local-name file)))
             (message-log-max nil))
        (with-temp-buffer
          (message "%sExternal checksumming of %s" progress file)
          (unless (zerop (process-file exec nil t nil file))
            (error "Failed to start checksum program %s for file %s" exec file))
          (goto-char (point-min))
          (if (looking-at "\\`[[:alnum:]]+")
              (match-string 0)
            (error "Unexpected output from checksum program %s" exec)))))))

(defun dired-duplicates--file-filter-readable-and-regular (file)
  "Check whether FILE is readable and regular."
  (and (file-readable-p file)
       (file-regular-p file)))

(defun dired-duplicates--apply-file-filter-functions (files)
  "Apply file filter functions to FILES, returning the resulting list."
  (dolist (filter-func dired-duplicates-file-filter-functions files)
    (setf files (cl-delete-if-not filter-func files))))

(defun dired-duplicates--find-files (directories)
  "Search below DIRECTORIES for files."
  (mapcan (lambda (d)
            (if dired-duplicates-search-directories-recursively
                (directory-files-recursively d ".*" nil t)
              (seq-remove #'file-directory-p (directory-files d t nil t))))
          directories))

(defun dired-duplicates--find-checksum-exec (file table)
  "Detect whether checksum exec is available for FILE location.

The results will be cached in the hash TABLE for faster access."
  (let* ((key (file-remote-p file))
         (path (gethash key table 'does-not-exist)))
    (if (eq path 'does-not-exist)
        (let* ((default-directory (file-name-directory (expand-file-name file)))
               (exec (executable-find dired-duplicates-checksum-exec t)))
          (setf (gethash key table) exec)
          (unless exec
            (message "Checksum program %s not found in exec-path, falling back to internal routines" exec))
          exec)
      path)))

(defun dired-duplicates--detect-duplicates (files &optional inversep)
  "Find duplicates (non-duplicates when INVERSEP is t), given a list of FILES.

Any file filter functions will be applied before checking for
duplicates.  Return a hash-table with the checksums as keys and a
list of size and duplicate files as values."
  (cl-loop with files = (dired-duplicates--apply-file-filter-functions files)
           and same-size-table = (make-hash-table)
           and checksum-table = (make-hash-table :test 'equal)
           and inverse-table = (make-hash-table :test 'equal)
           for f in files
           for size = (file-attribute-size (file-attributes f))
           initially do
           (message "Collecting sizes of %d files..." (length files))
           do (setf (gethash size same-size-table)
                    (push f (gethash size same-size-table)))
           when inversep do (setf (gethash f inverse-table) t)
           finally
           (cl-loop with checksum-exec-paths = (make-hash-table :test 'equal)
                    and current = 1
                    and total = (cl-loop for same-size-files being the hash-value in same-size-table
                                         when (cdr same-size-files)
                                         sum (length same-size-files))
                    for same-size-files being the hash-value in same-size-table using (hash-key size)
                    when (cdr same-size-files) do
                    (cl-loop for f in same-size-files
                             for exec = (dired-duplicates--find-checksum-exec f checksum-exec-paths)
                             for checksum = (if exec
                                                (dired-duplicates--checksum-file f exec current total)
                                              (if (<= size dired-duplicates-internal-checksumming-size-limit)
                                                  (dired-duplicates--checksum-file f nil current total)
                                                (warn "File %s is too big to checksum using internal functions, skipping." f)
                                                nil))
                             when checksum do
                             (setf (gethash checksum checksum-table)
                                   (push f (gethash checksum checksum-table)))
                             do
                             (cl-incf current)))
           (cl-loop for same-files being the hash-value in checksum-table using (hash-key checksum)
                    do
                    (if (cdr same-files)
                        (progn
                          (setf (gethash checksum checksum-table)
                                (cons (file-attribute-size (file-attributes (car same-files)))
                                      (sort same-files #'string<)))
                          (when inversep
                            (dolist (f same-files)
                              (remhash f inverse-table))))
                      (remhash checksum checksum-table)))
           (cl-return
            (if inversep
                (sort (cl-loop for k being the hash-keys in inverse-table collect k) #'string<)
              checksum-table))))

(defun dired-duplicates--generate-grouped-results (&optional directories)
  "Generate a list of grouped duplicate files in DIRECTORIES."
  (cl-loop with directories = (or directories dired-duplicates-directories)
           with dupes-table = (dired-duplicates--detect-duplicates
                               (dired-duplicates--find-files directories))
           with sorted-sums = (cl-sort
                               (cl-loop for k being the hash-key in dupes-table using (hash-value v)
                                        collect (list k (car v)))
                               dired-duplicates-size-comparison-function
                               :key #'cl-second)
           for (checksum) in sorted-sums
           collect (cdr (gethash checksum dupes-table))))

(defun dired-duplicates--post-process-dired-buffer (results)
  "Post process the duplicate results buffer using RESULTS.

Currently, this simply adds a new-line after each results group."
  (when dired-duplicates-separate-results
    (save-mark-and-excursion
      (goto-char (point-min))
      (forward-line)
      ;; add a new-line after each group
      (cl-loop with lengths = (mapcar #'length results)
               for len in lengths
               do
               (forward-line len)
               (let ((inhibit-read-only t))
                 (beginning-of-line)
                 (unless (= (point) (point-max))
                   (insert "\n")))))))

(defun dired-duplicates-dired-revert (&optional arg noconfirm)
  "Revert function used instead of `dired-revert' for Dired buffers.

The args ARG and NOCONFIRM are passed through from
`revert-buffer' to `dired-revert'."
  (message "Looking for remaining duplicate files...")
  (let ((results (dired-duplicates--generate-grouped-results dired-duplicates-directories)))
    (setq-local dired-directory
                (append (list (car dired-directory))
                        (flatten-list results)))
    (dired-revert arg noconfirm)
    (dired-duplicates--post-process-dired-buffer results)
    (message "Reverted buffer, found %d files having duplicates." (length results))))

(when (< emacs-major-version 29)
  (defun dired-duplicates--do-delete (&optional arg)
    "Delete all marked (or next ARG) files.

This is the same as `dired-do-delete', but calls
`dired-duplicates-dired-revert' afterwards."
    (interactive)
    (dired-do-delete arg)
    (apply revert-buffer-function nil nil)))

(when (< emacs-major-version 29)
  (defun dired-duplicates--do-flagged-delete (&optional nomessage)
    "Delete flagged files.

If NOMESSAGE is non-nil, we don't display any message
if there are no flagged files.

This is the same as `dired-do-flagged-delete', but calls
`dired-duplicates-dired-revert' afterwards."
    (interactive)
    (dired-do-flagged-delete nomessage)
    (apply revert-buffer-function nil nil)))

(defvar dired-duplicates-map
  (let ((map (make-sparse-keymap)))
    ;; workaround for Emacs bug #57565
    (when (< emacs-major-version 29)
      (define-key map (kbd "x") 'dired-duplicates--do-flagged-delete)
      (define-key map (kbd "D") 'dired-duplicates--do-delete))
    map)
  "This keymap overrides the default `dired-mode-map'.

It will be local to the `dired-duplicates' buffer.")

(defun dired-duplicates--prompt-for-directories ()
  "Prompt the user for directories until an empty string is returned."
  (let ((dirs nil))
    (while (let ((dir (if (not dirs)
                       (read-file-name "Directory: " nil default-directory nil nil #'file-directory-p)
                     (read-file-name "Another directory (or RET to start): " nil "" nil nil #'file-directory-p))))
             (unless (string= dir "")
                 (push dir dirs))))
    (nreverse dirs)))

;;;###autoload
(defun dired-duplicates (directories)
  "Find a list of duplicate files inside one or more DIRECTORIES.

The results will be shown in a Dired buffer."
  (interactive (list (dired-duplicates--prompt-for-directories)))
  (unless directories
    (user-error "Please specify one or more directories to search in"))
  (let* ((directories (if (listp directories)
                          (cl-remove-duplicates (mapcar #'expand-file-name directories)
                                                :test #'string=)
                        (list directories)))
         (inversep current-prefix-arg))
    (if (not inversep)
        (progn
          (message "Finding duplicate files in %s..." (string-join directories ", "))
          (if-let ((default-directory "/")
                   (results (dired-duplicates--generate-grouped-results directories)))
              (progn
                (message "Found %d files having duplicates." (length results))
                (dired (cons "/" (flatten-list results)))
                (set-keymap-parent dired-duplicates-map dired-mode-map)
                (use-local-map dired-duplicates-map)
                (setq-local dired-duplicates-directories directories)
                (dired-duplicates--post-process-dired-buffer results)
                (setq-local revert-buffer-function 'dired-duplicates-dired-revert))
            (message "No duplicate files found.")))
      (message "Finding non-duplicate files in %s..." (string-join directories ", "))
      (if-let ((default-directory "/")
               (results (dired-duplicates--detect-duplicates
                         (dired-duplicates--find-files directories)
                         t)))
          (progn
            (message "Found %d files having no duplicates." (length results))
            (dired (cons "/" results))
            (set-keymap-parent dired-duplicates-map dired-mode-map)
            (use-local-map dired-duplicates-map))
        (message "No non-duplicate files found.")))))

(provide 'dired-duplicates)

;;; dired-duplicates.el ends here
