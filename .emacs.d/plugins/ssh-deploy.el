;;; ssh-deploy.el --- Deployment via Tramp, global or per directory.  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2019  Free Software Foundation, Inc.

;; Author: Christian Johansson <christian@cvj.se>
;; Maintainer: Christian Johansson <christian@cvj.se>
;; Created: 5 Jul 2016
;; Modified: 9 Sep 2019
;; Version: 3.1.9
;; Keywords: tools, convenience
;; URL: https://github.com/cjohansson/emacs-ssh-deploy

;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ssh-deploy enables automatic deploys on explicit-save actions, manual uploads, renaming,
;; deleting, downloads, file and directory differences, launching remote terminals (eshell, shell),
;; detection of remote changes, remote directory browsing, remote SQL database sessions and
;; running custom deployment scripts via Tramp.
;;
;; For asynchronous operations it uses package '`make-thread' or if not available '`async.el'.
;;
;; By setting the variables (globally, per directory or per file):
;; ssh-deploy-root-local,ssh-deploy-root-remote, ssh-deploy-on-explicit-save
;; you can setup a directory for Tramp deployment.
;;
;; For asynchronous transfers you need to setup ~/.authinfo.gpg or key-based authorization or equivalent for automatic authentication.
;;
;; Example contents of ~/.authinfo.gpg for password-based interaction-free authentication:
;; machine myserver.com login myuser port ftp password mypassword
;; machine myserver2.com login myuser2 port ssh password mypassword2
;; machine myserver3.com login myuser3 port sftp password mypassword3
;;
;; Set permissions to this file to 600 with your user as the owner.
;;
;; If your not using ~/.netrc for FTP information you need to specify what file your using with:
;; (setq ange-ftp-netrc-filename "~/.authinfo.gpg")
;;
;; - To setup a upload hook on save do this:
;; Add to init-script: (ssh-deploy-add-after-save-hook)
;;
;; - To setup automatic storing of base revisions and detection of remote changes do this:
;; Add to init-script: (ssh-deploy-add-find-file-hook)
;;
;; - To enable mode-line feature do this:
;; (ssh-deploy-line-mode)
;;
;; - To enable menu-bar feature do this:
;; (ssh-deploy-add-menu)
;;
;; - To set global key-bindings do something like this:
;;     (global-set-key (kbd "C-c C-z") 'ssh-deploy-prefix-map)
;;
;; - To set global key-bindings for the pre-defined hydra do something like this:
;;     (ssh-deploy-hydra "C-c C-z")
;;
;; - To install and set-up using use-package and hydra do this:
;;   (use-package ssh-deploy
;;     :ensure t
;;     :after hydra
;;     :demand
;;     :hook ((after-save . ssh-deploy-after-save)
;;            (find-file . ssh-deploy-find-file))
;;     :config
;;     (ssh-deploy-line-mode) ;; If you want mode-line feature
;;     (ssh-deploy-add-menu) ;; If you want menu-bar feature
;;     (ssh-deploy-hydra "C-c C-z") ;; If you the hydra feature
;;    )
;;
;;
;; Here is an example for SSH deployment, /Users/Chris/Web/Site1/.dir-locals.el:
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site1/")
;;   (ssh-deploy-root-remote . "/ssh:myuser@myserver.com:/var/www/site1/")
;;   (ssh-deploy-on-explicit-save . 1)
;;   (ssh-deploy-async . 1)
;; )))
;;
;; Here is an example for SFTP deployment, /Users/Chris/Web/Site2/.dir-locals.el:
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site2/")
;;   (ssh-deploy-root-remote . "/sftp:myuser@myserver.com:/var/www/site2/")
;;   (ssh-deploy-on-explicit-save . 0)
;;   (ssh-deploy-async . 0)
;;   (ssh-deploy-script . (lambda() (let ((default-directory ssh-deploy-root-remote))(shell-command "bash compile.sh"))))
;; )))
;;
;; Here is an example for FTP deployment, /Users/Chris/Web/Site3/.dir-locals.el:
;; ((nil . (
;;   (ssh-deploy-root-local . "/Users/Chris/Web/Site3/")
;;   (ssh-deploy-root-remote . "/ftp:myuser@myserver.com:/var/www/site3/")
;; )))
;;
;;
;; Here is a list of other variables you can set globally or per directory:

;; * `ssh-deploy-root-local' - The local root that should be under deployment *(string)*
;; * `ssh-deploy-root-remote' - The remote Tramp root that is used for deployment *(string)*
;; * `ssh-deploy-debug' - Enables debugging messages *(integer)*
;; * `ssh-deploy-revision-folder' - The folder used for storing local revisions *(string)*
;; * `ssh-deploy-automatically-detect-remote-changes' - Enables automatic detection of remote changes *(integer)*
;; * `ssh-deploy-on-explicit-save' - Enabled automatic uploads on save *(integer)*
;; * `ssh-deploy-exclude-list' - A list defining what file names to exclude from deployment *(list)*
;; * `ssh-deploy-async' - Enables asynchronous transfers (you need to have `(make-thread)` or `async.el` available as well) *(integer)*
;; * `ssh-deploy-remote-sql-database' - Default database when connecting to remote SQL database *(string)*
;; * `ssh-deploy-remote-sql-password' - Default password when connecting to remote SQL database *(string)*
;; * `ssh-deploy-remote-sql-port' - Default port when connecting to remote SQL database *(integer)*
;; * `ssh-deploy-remote-sql-server' - Default server when connecting to remote SQL database *(string)*
;; * `ssh-deploy-remote-sql-user' - Default user when connecting to remote SQL database *(string)*
;; * `ssh-deploy-remote-shell-executable' - Default shell executable when launching shell on remote host *(string)*
;; * `ssh-deploy-verbose' - Show messages in message buffer when starting and ending actions *(integer)*
;; * `ssh-deploy-script' - Our custom lambda function that will be called using (funcall) when running deploy script *(function)*
;; * `ssh-deploy-async-with-threads' - Whether to use threads (make threads) instead of processes (async-start) for asynchronous operations *(integer)*
;;
;; When integers are used as booleans, above zero means true, zero means false and nil means unset and fallback to global settings.
;;
;; Please see README.md from the same repository for more extended documentation.

;; FIXME: This uses "path" in lots of places to mean "a complete file name
;; starting from /", whereas the GNU convention is to only "file name" instead
;; and keep "path" for lists of directories like load-path, exec-path.

;;; Code:

(autoload 'ediff-same-file-contents "ediff-util")
(autoload 'string-remove-prefix "subr-x")
(autoload 'eshell "eshell")
(autoload 'shell "shell")

(autoload 'ssh-deploy-diff-mode "ssh-deploy-diff-mode")
(autoload 'ssh-deploy-hydra "ssh-deploy-hydra")


(defgroup ssh-deploy nil
  "Upload, download, difference, browse and terminal handler for files and directories on remote hosts via Tramp."
  :group 'tools
  :group 'convenience)

(defcustom ssh-deploy-root-local nil
  "String variable of local root, nil by default."
  :type 'string)
(put 'ssh-deploy-root-local 'permanent-local t)
(put 'ssh-deploy-root-local 'safe-local-variable 'stringp)

(defcustom ssh-deploy-root-remote nil
  "String variable of remote root, nil by default."
  :type 'string)
(put 'ssh-deploy-root-remote 'permanent-local t)
(put 'ssh-deploy-root-remote 'safe-local-variable 'stringp)

(defcustom ssh-deploy-on-explicit-save 1
  "Boolean variable if deploy should be made on explicit save, 1 by default."
  :type 'integer)
(put 'ssh-deploy-on-explicit-save 'permanent-local t)
(put 'ssh-deploy-on-explicit-save 'safe-local-variable 'integerp)

(defcustom ssh-deploy-debug 0
  "Boolean variable if debug messages should be shown, 0 by default."
  :type 'integer)
(put 'ssh-deploy-debug 'permanent-local t)
(put 'ssh-deploy-debug 'safe-local-variable 'integerp)

(defcustom ssh-deploy-verbose 1
  "Boolean variable if debug messages should be shown, 1 by default."
  :type 'integer)
(put 'ssh-deploy-verbose 'permanent-local t)
(put 'ssh-deploy-verbose 'safe-local-variable 'integerp)

(defcustom ssh-deploy-async 0
  "Boolean variable if asynchronous method for transfers should be used, 0 by default."
  :type 'integer)
(put 'ssh-deploy-async 'permanent-local t)
(put 'ssh-deploy-async 'safe-local-variable 'integerp)

(defcustom ssh-deploy-async-with-threads 0
  "Boolean variable if asynchronous method should use threads if available, 0 by default."
  :type 'integer)
(put 'ssh-deploy-async-with-threads 'permanent-local t)
(put 'ssh-deploy-async-with-threads 'safe-local-variable 'integerp)

(defcustom ssh-deploy-async-with-threads 0
  "Boolean variable if asynchronous method should use threads if available, 0 by default."
  :type 'integer)
(put 'ssh-deploy-async-with-threads 'permanent-local t)
(put 'ssh-deploy-async-with-threads 'safe-local-variable 'integerp)

(defcustom ssh-deploy-revision-folder "~/.ssh-deploy-revisions/"
  "String variable with file name to revisions with trailing slash."
  :type 'string)
(put 'ssh-deploy-revision-folder 'permanent-local t)
(put 'ssh-deploy-revision-folder 'safe-local-variable 'stringp)

(defcustom ssh-deploy-automatically-detect-remote-changes 1
  "Detect remote changes and store base revisions automatically, 1 by default."
  :type 'integer)
(put 'ssh-deploy-automatically-detect-remote-changes 'permanent-local t)
(put 'ssh-deploy-automatically-detect-remote-changes 'safe-local-variable 'integerp)

(defcustom ssh-deploy-exclude-list '("\\\.git/" "\\\.dir-locals\\\.el")
  "List of strings that if found in file name will exclude it from sync."
  :type 'list)
(put 'ssh-deploy-exclude-list 'permanent-local t)
(put 'ssh-deploy-exclude-list 'safe-local-variable 'listp)

(defcustom ssh-deploy-remote-sql-database nil
  "String variable of remote sql database, nil by default."
  :type 'string)
(put 'ssh-deploy-remote-sql-database 'permanent-local t)
(put 'ssh-deploy-remote-sql-database 'safe-local-variable 'stringp)

(defcustom ssh-deploy-remote-sql-password nil
  "String variable of remote sql password, nil by default."
  :type 'string)
(put 'ssh-deploy-remote-sql-password 'permanent-local t)
(put 'ssh-deploy-remote-sql-password 'safe-local-variable 'stringp)

(defcustom ssh-deploy-remote-sql-port nil
  "Integer variable of remote sql port, nil by default."
  :type 'number)
(put 'ssh-deploy-remote-sql-port 'permanent-local t)
(put 'ssh-deploy-remote-sql-port 'safe-local-variable 'integerp)

(defcustom ssh-deploy-remote-sql-server nil
  "String variable of remote sql server, nil by default."
  :type 'string)
(put 'ssh-deploy-remote-sql-server 'permanent-local t)
(put 'ssh-deploy-remote-sql-server 'safe-local-variable 'stringp)

(defcustom ssh-deploy-remote-sql-user nil
  "String variable of remote sql user, nil by default."
  :type 'string)
(put 'ssh-deploy-remote-sql-user 'permanent-local t)
(put 'ssh-deploy-remote-sql-user 'safe-local-variable 'stringp)

(defcustom ssh-deploy-remote-shell-executable nil
  "String variable of remote server shell executable, nil by default."
  :type 'string)
(put 'ssh-deploy-remote-shell-executable 'permanent-local t)
(put 'ssh-deploy-remote-shell-executable 'safe-local-variable 'stringp)

(defcustom ssh-deploy-script nil
  "Lambda function to run with `funcall' when `ssh-deploy-run-deploy-script-handler' is executed."
  :type 'function)
(put 'ssh-deploy-script 'permanent-local t)
(put 'ssh-deploy-script 'safe-local-variable 'functionp)

(defvar ssh-deploy--mode-line-status '()
  "The mode-line status displayed in mode-line.")

(defvar ssh-deploy--mode-line-status-text ""
  "The mode-line status text displayed in mode-line.")


;; PRIVATE FUNCTIONS
;;
;; these functions are only used internally and should be of no value to outside public and handler functions.
;; these functions MUST not use module variables in any way.


(defun ssh-deploy--async-process (start &optional finish async-with-threads)
  "Asynchronously do START and then optionally do FINISH, use multi-treading if ASYNC-WITH-THREADS is above 0 otherwise use multi processes via async.el."
  (if (and async-with-threads
           (> async-with-threads 0))
      (if (fboundp 'make-thread)
          (make-thread (lambda()
                         (let ((ssh-deploy-async 0)
                               (ssh-deploy-async-with-threads 0)
                               (ssh-deploy-on-explicit-save 0)
                               (ssh-deploy-automatically-detect-remote-changes 0))
                           (if start
                               (let ((result (funcall start)))
                                 (if finish
                                     (funcall finish result)))))))
        (display-warning 'ssh-deploy "make-thread function are not available!"))
    (if (fboundp 'async-start)
        (when start
          (let ((ftp-netrc nil)
                (root-local ssh-deploy-root-local)
                (root-remote ssh-deploy-root-remote)
                (revision-folder ssh-deploy-revision-folder)
                (exclude-list ssh-deploy-exclude-list))
            (when (boundp 'ange-ftp-netrc-filename)
              (setq ftp-netrc ange-ftp-netrc-filename))
            (let ((script-filename (file-name-directory (symbol-file 'ssh-deploy-diff-directories))))
              (async-start
               (lambda()
                 (add-to-list 'load-path script-filename)
                 (require 'ssh-deploy)
                 (let ((ssh-deploy-async 0)
                       (ssh-deploy-async-with-threads 0)
                       (ssh-deploy-on-explicit-save 0)
                       (ssh-deploy-automatically-detect-remote-changes 0)
                       (ssh-deploy-root-local root-local)
                       (ssh-deploy-root-remote root-remote)
                       (ssh-deploy-revision-folder revision-folder)
                       (ssh-deploy-exclude-list exclude-list))

                   ;; Pass ange-ftp setting to asynchronous process
                   (when ftp-netrc
                     (defvar ange-ftp-netrc-filename)
                     (setq ange-ftp-netrc-filename ftp-netrc))

                   (funcall start)))
               finish))))
      (display-warning 'ssh-deploy "async-start functions are not available!"))))

(defun ssh-deploy--mode-line-set-status-and-update (status &optional filename)
  "Set the mode line STATUS in optionally in buffer visiting FILENAME."
  (if filename
      (let ((buffer (find-buffer-visiting filename)))
        (when buffer
          (with-current-buffer buffer
            (ssh-deploy--mode-line-set-status-and-update status))))
    (progn
      (push status ssh-deploy--mode-line-status)
      ;; (message "SSH Deploy - Updated status to: %s" ssh-deploy--mode-line-status)
      (ssh-deploy--mode-line-status-refresh))))

(defun ssh-deploy--mode-line-status-refresh ()
  "Refresh the status text based on the status variable."
  (let ((status (pop ssh-deploy--mode-line-status)))
    ;; (message "SSH Deploy - Refreshing status based on: %s" status)
    (when status
      (ssh-deploy--mode-line-status-update status))))

(defun ssh-deploy--mode-line-status-update (&optional status)
  "Update the local status text variable to a text representation based on STATUS."
  (let ((status-text
         (pcase status
           ('downloading              "dl..")
           ('uploading                "ul..")
           ('deleting                 "rm..")
           ('renaming                 "mv..")
           ('file-difference          "diff..")
           ('detecting-remote-changes "chgs..")
           (_ (if (and ssh-deploy-root-local ssh-deploy-root-remote)
                  "idle" "")))))
    (set (make-local-variable 'ssh-deploy--mode-line-status-text)
         (ssh-deploy--mode-line-status-text-format status-text))))

(defun ssh-deploy--mode-line-status-text-format (text)
  "Return a formatted string based on TEXT."
  (if (string= text "")
      ""
    (format " {DPLY:%s} " text)))

(defun ssh-deploy--insert-keyword (text)
  "Insert TEXT as bold text."
  (put-text-property 0 (length text) 'face 'font-lock-keyword-face text)
  (insert text))

(defun ssh-deploy--get-revision-path (path root)
  "Generate revision-path for PATH in ROOT."
  (if (not (file-exists-p root))
      (make-directory root))
  (expand-file-name (replace-regexp-in-string "\\(/\\|@\\|:\\)" "_" path) root))

(defun ssh-deploy--file-is-in-path-p (file path)
  "Return non-nil if FILE is in the path PATH."
  (not (null (string-match path file))))

(defun ssh-deploy--file-is-included-p (path exclude-list)
  "Return non-nil if PATH is not in EXCLUDE-LIST."
  (let ((not-found t))
    (dolist (element exclude-list)
      (when (and element
                 (string-match element path))
        (setq not-found nil)))
    not-found))

(defun ssh-deploy--get-relative-path (root path)
  "Return a string for the relative path based on ROOT and PATH."
  (replace-regexp-in-string root "" path))

(defun ssh-deploy--is-not-empty-string-p (string)
  "Return non-nil if the STRING is not empty and not nil.  Expects string."
  (and string
       (not (zerop (length string)))))

(defun ssh-deploy--upload-via-tramp-async (path-local path-remote force revision-folder async-with-threads)
  "Upload PATH-LOCAL to PATH-REMOTE via Tramp asynchronously and FORCE upload despite remote change, check for revisions in REVISION-FOLDER.  Use multi-treaded async if ASYNC-WITH-THREADS is specified."
  (let ((file-or-directory (not (file-directory-p path-local))))
    (ssh-deploy--mode-line-set-status-and-update 'uploading path-local)
    (if file-or-directory
        (let ((revision-path (ssh-deploy--get-revision-path path-local revision-folder)))
          (when (> ssh-deploy-verbose 0) (message "Uploading file '%s' to '%s'.. (asynchronously)" path-local path-remote))
          (ssh-deploy--async-process
           (lambda()
             (if (or (> force 0) (not (file-exists-p path-remote))
                     (and (file-exists-p revision-path)
                          (nth 0 (ssh-deploy--diff-files revision-path path-remote))))
                 (progn
                   (unless (file-directory-p (file-name-directory path-remote))
                     (make-directory (file-name-directory path-remote) t))
                   (copy-file path-local path-remote t t t t)
                   (copy-file path-local revision-path t t t t)
                   (list 0 (format "Completed upload of file '%s'. (asynchronously)" path-remote) path-local))
               (list 1 (format "Remote file '%s' has changed please download or diff. (asynchronously)" path-remote) path-local)))
           (lambda(return)
             (ssh-deploy--mode-line-set-status-and-update 'idle (nth 2 return))
             (if (= (nth 0 return) 0)
                 (when (> ssh-deploy-verbose 0) (message (nth 1 return)))
               (display-warning 'ssh-deploy (nth 1 return) :warning)))
           async-with-threads))
      (when (> ssh-deploy-verbose 0) (message "Uploading directory '%s' to '%s'.. (asynchronously)" path-local path-remote))
      (ssh-deploy--async-process
       (lambda()
         (copy-directory path-local path-remote t t t)
         path-local)
       (lambda(return-path)
         (ssh-deploy--mode-line-set-status-and-update 'idle return-path)
         (when (> ssh-deploy-verbose 0) (message "Completed upload of directory '%s'. (asynchronously)" return-path)))))))

(defun ssh-deploy--upload-via-tramp (path-local path-remote force revision-folder)
  "Upload PATH-LOCAL to PATH-REMOTE via Tramp synchronously and FORCE despite remote change compared with copy in REVISION-FOLDER."
  (let ((file-or-directory (not (file-directory-p path-local)))
        (revision-path (ssh-deploy--get-revision-path path-local revision-folder)))
    (ssh-deploy--mode-line-set-status-and-update 'uploading)
    (if file-or-directory
        (progn
          (if (or (> force 0)
                  (not (file-exists-p path-remote))
                  (and (file-exists-p revision-path)
                       (nth 0 (ssh-deploy--diff-files revision-path path-remote))))
              (progn
                (when (> ssh-deploy-verbose 0) (message "Uploading file '%s' to '%s'.. (synchronously)" path-local path-remote))
                (unless (file-directory-p (file-name-directory path-remote))
                  (make-directory (file-name-directory path-remote) t))
                (copy-file path-local path-remote t t t t)
                (ssh-deploy-store-revision path-local revision-folder)
                (when (> ssh-deploy-verbose 0) (message "Completed upload of '%s'. (synchronously)" path-local)))
            (display-warning 'ssh-deploy (format "Remote file '%s' has changed, please download or diff. (synchronously)" path-remote) :warning))
          (ssh-deploy--mode-line-set-status-and-update 'idle))
      (when (> ssh-deploy-verbose 0) (message "Uploading directory '%s' to '%s'.. (synchronously)" path-local path-remote))
      (copy-directory path-local path-remote t t t)
      (ssh-deploy--mode-line-set-status-and-update 'idle)
      (when (> ssh-deploy-verbose 0) (message "Completed upload of '%s'. (synchronously)" path-local)))))

(defun ssh-deploy--download-via-tramp-async (path-remote path-local revision-folder async-with-threads)
  "Download PATH-REMOTE to PATH-LOCAL via Tramp asynchronously and make a copy in REVISION-FOLDER, use multi-threading if ASYNC-WITH-THREADS is above zero."
  (let ((revision-path (ssh-deploy--get-revision-path path-local revision-folder)))
    (ssh-deploy--mode-line-set-status-and-update 'downloading path-local)
    (when (> ssh-deploy-verbose 0) (message "Downloading '%s' to '%s'.. (asynchronously)" path-remote path-local))
    (ssh-deploy--async-process
     (lambda()
       (let ((file-or-directory (not (file-directory-p path-remote))))
         (if file-or-directory
             (progn
               (unless (file-directory-p (file-name-directory path-local))
                 (make-directory (file-name-directory path-local) t))
               (copy-file path-remote path-local t t t t)
               (copy-file path-local revision-path t t t t))
           (copy-directory path-remote path-local t t t))
         path-local))
     (lambda(return-path)
       (ssh-deploy--mode-line-set-status-and-update 'idle return-path)
       (when (> ssh-deploy-verbose 0) (message "Completed download of '%s'. (asynchronously)" return-path))
       (let ((local-buffer (find-buffer-visiting return-path)))
         (when local-buffer
           (with-current-buffer local-buffer
             (revert-buffer t t t)))))
     async-with-threads)))

(defun ssh-deploy--download-via-tramp (path-remote path-local revision-folder)
  "Download PATH-REMOTE to PATH-LOCAL via Tramp synchronously and store a copy in REVISION-FOLDER."
  (let ((file-or-directory (not (file-directory-p path-remote))))
    (ssh-deploy--mode-line-set-status-and-update 'downloading)
    (if file-or-directory
        (progn
          (when (> ssh-deploy-verbose 0) (message "Downloading file '%s' to '%s'.. (synchronously)" path-remote path-local))
          (unless (file-directory-p (file-name-directory path-local))
            (make-directory (file-name-directory path-local) t))
          (copy-file path-remote path-local t t t t)
          (ssh-deploy-store-revision path-local revision-folder)
          (ssh-deploy--mode-line-set-status-and-update 'idle)
          (when (> ssh-deploy-verbose 0) (message "Completed download of file '%s'. (synchronously)" path-local)))
      (message "Downloading directory '%s' to '%s'.. (synchronously)" path-remote path-local)
      (copy-directory path-remote path-local t t t)
      (ssh-deploy--mode-line-set-status-and-update 'idle)
      (message "Completed download of directory '%s'. (synchronously)" path-local))))

(defun ssh-deploy--diff-directories-data (directory-a directory-b exclude-list)
  "Find difference between DIRECTORY-A and DIRECTORY-B but exclude paths matching EXCLUDE-LIST."
  ;; (message "Comparing a: %s to b: %s" directory-a directory-b)
  (if (fboundp 'string-remove-prefix)
      (if (and (file-directory-p directory-a)
               (file-directory-p directory-b))
          (let* ((old-directory-b directory-b)
                 (directory-b (file-truename directory-b)))
            (let ((files-a (directory-files-recursively directory-a ""))
                  (files-b (directory-files-recursively directory-b ""))
                  (files-a-only (list))
                  (files-b-only (list))
                  (files-both (list))
                  (files-both-equals (list))
                  (files-both-differs (list))
                  (files-a-relative-list (list))
                  (files-b-relative-list (list))
                  (files-a-relative-hash (make-hash-table :test 'equal))
                  (files-b-relative-hash (make-hash-table :test 'equal)))

              ;; Collected included files in directory a with relative paths
              (mapc
               (lambda (file-a-tmp)
                 (let ((file-a (file-truename file-a-tmp)))
                   (let ((relative-path (string-remove-prefix directory-a file-a))
                         (included t))

                     ;; Check if file is excluded
                     (dolist (element exclude-list)
                       (when (and element
                                  (string-match element relative-path))
                         (setq included nil)))

                     ;; Add relative path file a list
                     (when included
                       (puthash relative-path file-a files-a-relative-hash)
                       (push relative-path files-a-relative-list)))))
               files-a)

              ;; Collected included files in directory b with relative paths
              (mapc
               (lambda (file-b-tmp)
                 ;; (message "file-b-tmp: %s %s" file-b-tmp (file-truename file-b-tmp))
                 (let ((file-b (file-truename file-b-tmp)))
                   (let ((relative-path (string-remove-prefix directory-b file-b))
                         (included t))

                     ;; Check if file is excluded
                     (dolist (element exclude-list)
                       (when (and element
                                  (string-match element relative-path))
                         (setq included nil)))

                     ;; Add relative path file a list
                     (when included
                       (puthash relative-path file-b files-b-relative-hash)
                       (push relative-path files-b-relative-list)))))
               files-b)

              ;; Collect files that only exists in directory a and files that exist in both directory a and b
              (mapc
               (lambda (file-a)
                 (push file-a
                       (if (gethash file-a files-b-relative-hash)
                           files-both
                         files-a-only)))
               files-a-relative-list)
              (setq files-a-only (sort files-a-only #'string<))

              ;; Collect files that only exists in directory b
              (mapc
               (lambda (file-b)
                 (when (equal (gethash file-b files-a-relative-hash) nil)
                   ;; (message "%s did not exist in hash-a" file-b)
                   (push file-b files-b-only)))
               files-b-relative-list)
              (setq files-b-only (sort files-b-only #'string<))

              ;; Collect files that differ in contents and have equal contents
              (mapc
               (lambda (file)
                 (let ((file-a (gethash file files-a-relative-hash))
                       (file-b (gethash file files-b-relative-hash)))
                   (push file
                         (if (nth 0 (ssh-deploy--diff-files file-a file-b))
                             files-both-equals
                           files-both-differs))))
               files-both)
              (setq files-both (sort files-both #'string<))
              (setq files-both-equals (sort files-both-equals #'string<))
              (setq files-both-differs (sort files-both-differs #'string<))

              ;; NOTE We sort lists to make result deterministic and testable

              (list directory-a old-directory-b exclude-list files-both files-a-only files-b-only files-both-equals files-both-differs)))
        (display-warning 'ssh-deploy "Both directories need to exist to perform difference generation." :warning))
    (display-warning 'ssh-deploy "Function 'string-remove-prefix' is missing." :warning)))

(defun ssh-deploy--diff-directories-present (diff root-local root-remote on-explicit-save debug async async-with-threads revision-folder remote-changes exclude-list)
  "Present difference data for directories from the DIFF, ROOT-LOCAL defines local root, ROOT-REMOTE defined remote root, ON-EXPLICIT-SAVE defines automatic uploads, DEBUG is the debug flag, ASYNC is for asynchronous, ASYNC-WITH-THREADS for threads instead of processes, REVISION-FOLDER is for revisions, REMOTE-CHANGES are whether to look for remote change, EXCLUDE-LIST is what files to exclude."

  (let ((buffer (generate-new-buffer "*SSH Deploy diff*")))
    (switch-to-buffer buffer)

    (ssh-deploy--insert-keyword "DIRECTORY A: ")
    (insert (nth 0 diff) "\n")

    (ssh-deploy--insert-keyword "DIRECTORY B: ")
    (insert (nth 1 diff) "\n")

    (when (> (length (nth 2 diff)) 0)
      (insert "\n")
      (ssh-deploy--insert-keyword (format "EXCLUDE-LIST: (%d)" (length (nth 2 diff))))
      (dolist (element (nth 2 diff))
        (insert "\n- " element))
      (insert "\n"))

    (insert "\n")

    (when (> (length (nth 4 diff)) 0)
      (ssh-deploy--insert-keyword (format "FILES ONLY IN A: (%d)" (length (nth 4 diff))))
      (dolist (element (nth 4 diff))
        (insert "\n- " element))
      (insert "\n\n"))

    (when (> (length (nth 5 diff)) 0)
      (ssh-deploy--insert-keyword (format "FILES ONLY IN B: (%d)" (length (nth 5 diff))))
      (dolist (element (nth 5 diff))
        (insert "\n- " element))
      (insert "\n\n"))

    (when (> (length (nth 7 diff)) 0)
      (ssh-deploy--insert-keyword (format "FILES IN BOTH BUT DIFFERS: (%d)" (length (nth 7 diff))))
      (dolist (element (nth 7 diff))
        (insert "\n- " element))
      (insert "\n\n"))

    (insert "\nHELP: quit (q), copy (C), copy A to B (a), copy B to A (b), delete (D), difference (TAB), refresh (g), open (RET)")

    (ssh-deploy-diff-mode)

    ;; Set local variables same as current directories
    (set (make-local-variable 'ssh-deploy-root-local) root-local)
    (set (make-local-variable 'ssh-deploy-root-remote) root-remote)
    (set (make-local-variable 'ssh-deploy-on-explicit-save) on-explicit-save)
    (set (make-local-variable 'ssh-deploy-debug) debug)
    (set (make-local-variable 'ssh-deploy-async) async)
    (set (make-local-variable 'ssh-deploy-async-with-threads) async-with-threads)
    (set (make-local-variable 'ssh-deploy-revision-folder) revision-folder)
    (set (make-local-variable 'ssh-deploy-automatically-detect-remote-changes) remote-changes)
    (set (make-local-variable 'ssh-deploy-exclude-list) exclude-list)))

(defun ssh-deploy--diff-files (file-a file-b)
  "Check difference between FILE-A and FILE-B."
  (let ((result (ediff-same-file-contents file-a file-b)))
    (list result file-a file-b)))


;; PUBLIC functions
;;
;; handlers use these to do things and people SHOULD be able to use these as they please themselves
;; these functions MUST only use module variables as fall-backs for missing arguments.


;;;###autoload
(defun ssh-deploy-diff-files (file-a file-b &optional async async-with-threads verbose)
  "Find difference between FILE-A and FILE-B, do it asynchronous if ASYNC is aboe zero and use threads if ASYNC-WITH-THREADS is above zero, if VERBOSE is above zero print messages."
  (message "Comparing file '%s' to '%s'.." file-a file-b)
  (let ((async (or async ssh-deploy-async))
        (async-with-threads (or async-with-threads ssh-deploy-async-with-threads))
        (verbose (or verbose ssh-deploy-verbose)))
    (ssh-deploy--mode-line-set-status-and-update 'file-difference file-a)
    (if (> async 0)
        (ssh-deploy--async-process
         (lambda() (ssh-deploy--diff-files file-a file-b))
         (lambda(result)
           (ssh-deploy--mode-line-set-status-and-update 'idle (nth 1 result))
           (if (nth 0 result)
               (when (> verbose 0)
                 (message "File '%s' and '%s' have identical contents. (asynchronously)" (nth 1 result) (nth 2 result)))
             (when (> verbose 0)
               (message "File '%s' and '%s' does not have identical contents, launching ediff.. (asynchronously)" file-a file-b))
             (ediff file-a file-b)))
         async-with-threads)
      (let ((result (ssh-deploy--diff-files file-a file-b)))
        (ssh-deploy--mode-line-set-status-and-update 'idle (nth 1 result))
        (if (nth 0 result)
            (when (> verbose 0)
              (message "File '%s' and '%s' have identical contents. (synchronously)" (nth 1 result) (nth 2 result)))
          (when (> verbose 0)
            (message "File '%s' and '%s' does not have identical contents, launching ediff.. (synchronously)" file-a file-b))
          (ediff file-a file-b))))))

;;;###autoload

(defun ssh-deploy-diff-directories (directory-a directory-b &optional on-explicit-save debug async async-with-threads revision-folder remote-changes exclude-list)
  "Find difference between DIRECTORY-A and DIRECTORY-B but exclude, ON-EXPLICIT-SAVE defines automatic uploads, DEBUG is the debug flag, ASYNC is for asynchronous, ASYNC-WITH-THREADS for threads instead of processes, REVISION-FOLDER is for revisions, REMOTE-CHANGES are whether to look for remote change, EXCLUDE-LIST is what files to exclude."
  (let ((on-explicit-save (or on-explicit-save ssh-deploy-on-explicit-save))
        (debug (or debug ssh-deploy-debug))
        (async (or async ssh-deploy-async))
        (async-with-threads (or async-with-threads ssh-deploy-async-with-threads))
        (revision-folder (or revision-folder ssh-deploy-revision-folder))
        (remote-changes (or remote-changes ssh-deploy-automatically-detect-remote-changes))
        (exclude-list (or exclude-list ssh-deploy-exclude-list)))
    (if (> async 0)
        (progn
          (message "Calculating differences between directory '%s' and '%s'.. (asynchronously)" directory-a directory-b)
          (ssh-deploy--async-process
           (lambda()
             (ssh-deploy--diff-directories-data directory-a directory-b exclude-list))
           (lambda(diff)
             (message "Completed calculation of differences between directory '%s' and '%s'. Result: %s only in A %s only in B %s differs. (asynchronously)" (nth 0 diff) (nth 1 diff) (length (nth 4 diff)) (length (nth 5 diff)) (length (nth 7 diff)))
             (when (or (> (length (nth 4 diff)) 0) (> (length (nth 5 diff)) 0) (> (length (nth 7 diff)) 0))
               (ssh-deploy--diff-directories-present diff directory-a directory-b on-explicit-save debug async async-with-threads revision-folder remote-changes exclude-list)))
           async-with-threads))
      (message "Calculating differences between directory '%s' and '%s'.. (synchronously)" directory-a directory-b)
      (let ((diff (ssh-deploy--diff-directories-data directory-a directory-b exclude-list)))
        (message "Completed calculation of differences between directory '%s' and '%s'. Result: %s only in A, %s only in B, %s differs. (synchronously)" (nth 0 diff) (nth 1 diff) (length (nth 4 diff)) (length (nth 5 diff)) (length (nth 7 diff)))
        (when (or (> (length (nth 4 diff)) 0) (> (length (nth 5 diff)) 0) (> (length (nth 7 diff)) 0))
          (ssh-deploy--diff-directories-present diff directory-a directory-b on-explicit-save debug async async-with-threads revision-folder remote-changes exclude-list))))))

(defun ssh-deploy--remote-changes-post-executor (response verbose)
  "Process RESPONSE from `ssh-deploy--remote-changes-data' with flags: VERBOSE."
  (pcase (nth 0 response)
    (0
     ;; File is outside of root
     (when (> verbose 0) (message (nth 1 response))))
    (1
     ;; File is excluded from deployment
     (when (> verbose 0) (message (nth 1 response))))
    (2
     ;; File is a directory ignore
     (when (> verbose 0) (message (nth 1 response))))
    (3
     ;; Remote file does not exist
     (when (message (nth 1 response))))
    (4
     ;; Remote file has not changed
     (when (> verbose 0) (message (nth 1 response))))
    (5
     ;; Remote file has changed in comparison with local revision but also with local file
     (display-warning 'ssh-deploy (nth 1 response) :warning))
    (6
     ;; Remote file has not changed in comparison with local file
     (copy-file (nth 2 response) (nth 3 response) t t t t)
     (when (> verbose 0) (message (nth 1 response))))
    (7
     ;; Remote file has changed in comparison with local file
     (display-warning 'ssh-deploy (nth 1 response) :warning))
    (8
     ;; Remote file has changed in comparison with local revision but not local file
     (copy-file (nth 2 response) (nth 3 response) t t t t)
     (when (> verbose 0) (message (nth 1 response))))))

(defun ssh-deploy--remote-changes-data (path-local &optional root-local root-remote revision-folder exclude-list)
  "Check if a local revision for PATH-LOCAL on ROOT-LOCAL and if remote file has changed on ROOT-REMOTE, check for copies in REVISION-FOLDER and skip if path is in EXCLUDE-LIST.  Should only return status-code and message."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote))
        (exclude-list (or exclude-list ssh-deploy-exclude-list)))

    ;; Is the file inside the local-root and should it not be excluded?
    (if (ssh-deploy--file-is-in-path-p path-local root-local)
        (if (ssh-deploy--file-is-included-p path-local exclude-list)
            (let* ((revision-folder (or revision-folder ssh-deploy-revision-folder))
                   (revision-path (ssh-deploy--get-revision-path path-local revision-folder))
                   (path-remote (expand-file-name (ssh-deploy--get-relative-path root-local path-local) root-remote)))

              ;; Is the file a regular file?
              (if (not (file-directory-p path-local))

                  ;; Does remote file exists?
                  (if (file-exists-p path-remote)

                      ;; Does a local revision of the file exist?
                      (if (file-exists-p revision-path)

                          (if (nth 0 (ssh-deploy--diff-files revision-path path-remote))
                              (list 4 (format "Remote file '%s' has not changed." path-remote) path-local)
                            (if (nth 0 (ssh-deploy--diff-files path-local path-remote))
                                (list 8 (format "Remote file '%s' has changed compared to local revision but not local file, copied local file to local revision." path-remote) path-local revision-path)
                              (list 5 (format "Remote file '%s' has changed compared to local revision and local file, please download or diff." path-remote) path-local revision-path)))

                        (if (nth 0 (ssh-deploy--diff-files path-local path-remote))
                            (list 6 (format "Remote file '%s' has not changed compared to local file, created local revision." path-remote) path-local revision-path)
                          (list 7 (format "Remote file '%s' has changed compared to local file, please download or diff." path-remote) path-local path-remote)))

                    (list 3 (format "Remote file '%s' doesn't exist." path-remote) path-local))

                ;; File is a directory
                (list 2 (format "File '%s' is a directory, ignoring remote changes check." path-local) path-local)))

          ;; File is excluded from root
          (list 1 (format "File '%s' is excluded from deployment." path-local) path-local))

      ;; File is not inside root
      (list 0 (format "File '%s' is not in root '%s'" path-local root-local) path-local))))

;;;###autoload
(defun ssh-deploy-remote-changes (path-local &optional root-local root-remote async revision-folder exclude-list async-with-threads verbose)
  "Check if a local revision for PATH-LOCAL on ROOT-LOCAL and if remote file has changed on ROOT-REMOTE, do it optionally asynchronously if ASYNC is true, check for copies in REVISION-FOLDER and skip if path is in EXCLUDE-LIST.  Use multi-threading if ASYNC-WITH-THREADS is above zero, VERBOSE if value above zero."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote))
        (exclude-list (or exclude-list ssh-deploy-exclude-list))
        (verbose (or verbose ssh-deploy-verbose))
        (revision-folder (or revision-folder ssh-deploy-revision-folder)))
    ;; Is the file inside the local-root and should it not be excluded?
    (if (and (ssh-deploy--file-is-in-path-p path-local root-local)
             (ssh-deploy--file-is-included-p path-local exclude-list))
        ;; Is the file a regular file?
        (if (not (file-directory-p path-local))
            (progn
              ;; Update mode-line status to detecting remote changes
              (ssh-deploy--mode-line-set-status-and-update 'detecting-remote-changes)
              (if (> async 0)
                  (ssh-deploy--async-process
                   (lambda()
                     (ssh-deploy--remote-changes-data path-local root-local root-remote revision-folder exclude-list))
                   (lambda(response)
                     ;; Update buffer status to idle
                     (ssh-deploy--mode-line-set-status-and-update 'idle (nth 2 response))
                     (ssh-deploy--remote-changes-post-executor response verbose))
                   async-with-threads)
                (let ((response (ssh-deploy--remote-changes-data path-local root-local root-remote revision-folder exclude-list)))
                  ;; Update buffer status to idle
                  (ssh-deploy--mode-line-set-status-and-update 'idle (nth 2 response))
                  (ssh-deploy--remote-changes-post-executor response verbose))))
          (when (> ssh-deploy-debug 0) (message "File %s is a directory, ignoring remote changes check." path-local)))
      (when (> ssh-deploy-debug 0) (message "File %s is not in root or is excluded from it." path-local)))))

(defun ssh-deploy-delete (path &optional async async-with-threads)
  "Delete PATH and use flags ASYNC.  Use multi-threading if ASYNC-WITH-THREADS is above zero."
  (let ((async (or async ssh-deploy-async))
        (async-with-threads (or async-with-threads ssh-deploy-async-with-threads)))
    (if (> async 0)
        (progn
          (ssh-deploy--mode-line-set-status-and-update 'deleting path)
          (ssh-deploy--async-process
           (lambda()
             (if (file-exists-p path)
                 (let ((file-or-directory (not (file-directory-p path))))
                   (progn
                     (if file-or-directory
                         (delete-file path t)
                       (delete-directory path t t))
                     (list path 0)))
               (list path 1)))
           (lambda(response)
             (ssh-deploy--mode-line-set-status-and-update 'idle (nth 0 response))
             (let ((local-buffer (find-buffer-visiting (nth 0 response))))
               (when local-buffer
                 (kill-buffer local-buffer)))
             (cond ((= 0 (nth 1 response)) (message "Completed deletion of '%s'. (asynchronously)" (nth 0 response)))
                   (t (display-warning 'ssh-deploy (format "Did not find '%s' for deletion. (asynchronously)" (nth 0 response)) :warning))))
           async-with-threads))
      (if (file-exists-p path)
          (let ((file-or-directory (not (file-directory-p path))))
            (ssh-deploy--mode-line-set-status-and-update 'deleting path)
            (progn
              (if file-or-directory
                  (delete-file path t)
                (delete-directory path t t))
              (ssh-deploy--mode-line-set-status-and-update 'idle path)
              (let ((local-buffer (find-buffer-visiting path)))
                (when local-buffer
                  (kill-buffer local-buffer)))
              (message "Completed deletion of '%s'. (synchronously)" path)))
        (display-warning 'ssh-deploy (format "Did not find '%s' for deletion. (synchronously)" path) :warning)))))

;;;###autoload
(defun ssh-deploy-delete-both (path-local &optional root-local root-remote async debug exclude-list async-with-threads)
  "Delete PATH-LOCAL relative to ROOT-LOCAL as well as on ROOT-REMOTE, do it asynchronously if ASYNC is non-nil, debug if DEBUG is non-nil, check if path is excluded in EXCLUDE-LIST.  Use async threads is ASYNC-WITH-THREADS is above zero."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote))
        (async (or async ssh-deploy-async))
        (debug (or debug ssh-deploy-debug))
        (exclude-list (or exclude-list ssh-deploy-exclude-list))
        (async-with-threads (or async async-with-threads)))
    (if (and (ssh-deploy--file-is-in-path-p path-local root-local)
             (ssh-deploy--file-is-included-p path-local exclude-list))
        (let ((path-remote (expand-file-name (ssh-deploy--get-relative-path root-local path-local) root-remote)))
          (ssh-deploy-delete path-local async async-with-threads)
          (ssh-deploy-delete path-remote async async-with-threads))
      (when (> debug 0) (message "Path '%s' is not in the root '%s' or is excluded from it." path-local root-local)))))

;;;###autoload
(defun ssh-deploy-rename (old-path-local new-path-local &optional root-local root-remote async debug exclude-list async-with-threads)
  "Rename OLD-PATH-LOCAL to NEW-PATH-LOCAL under ROOT-LOCAL as well as on ROOT-REMOTE, do it asynchronously if ASYNC is non-nil, debug if DEBUG is non-nil but check if path is excluded in EXCLUDE-LIST first.  Use multi-threading if ASYNC-WITH-THREADS is above zero."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote))
        (async (or async ssh-deploy-async))
        (debug (or debug ssh-deploy-debug))
        (exclude-list (or exclude-list ssh-deploy-exclude-list))
        (async-with-threads (or async-with-threads ssh-deploy-async-with-threads)))
    (if (and (ssh-deploy--file-is-in-path-p old-path-local root-local)
             (ssh-deploy--file-is-in-path-p new-path-local root-local)
             (ssh-deploy--file-is-included-p old-path-local exclude-list)
             (ssh-deploy--file-is-included-p new-path-local exclude-list))
        (let ((old-path-remote (expand-file-name (ssh-deploy--get-relative-path root-local old-path-local) root-remote))
              (new-path-remote (expand-file-name (ssh-deploy--get-relative-path root-local new-path-local) root-remote)))
          (ssh-deploy--mode-line-set-status-and-update 'renaming)
          (rename-file old-path-local new-path-local t)
          (if (not (file-directory-p new-path-local))
              (progn
                (rename-buffer new-path-local)
                (set-visited-file-name new-path-local)
                (set-buffer-modified-p nil))
            (dired new-path-local))
          (message "Renamed '%s' to '%s'." old-path-local new-path-local)
          (if (> async 0)
              (ssh-deploy--async-process
               (lambda()
                 (rename-file old-path-remote new-path-remote t)
                 (list old-path-remote new-path-remote new-path-local))
               (lambda(files)
                 (ssh-deploy--mode-line-set-status-and-update 'idle (nth 2 files))
                 (message "Renamed '%s' to '%s'. (asynchronously)" (nth 0 files) (nth 1 files)))
               async-with-threads)
            (rename-file old-path-remote new-path-remote t)
            (ssh-deploy--mode-line-set-status-and-update 'idle)
            (message "Renamed '%s' to '%s'. (synchronously)" old-path-remote new-path-remote)))
      (when (> debug 0)
        (message "Path '%s' or '%s' is not in the root '%s' or is excluded from it." old-path-local new-path-local root-local)))))

;;;###autoload
(defun ssh-deploy-remote-sql (remote-path &optional type)
  "Open remote sql on REMOTE-PATH, TYPE determines type and defaults to mysql."
  (let ((sql-type (or type "mysql"))
        (old-ssh-deploy-remote-sql-database ssh-deploy-remote-sql-database)
        (old-ssh-deploy-remote-sql-password ssh-deploy-remote-sql-password)
        (old-ssh-deploy-remote-sql-port ssh-deploy-remote-sql-port)
        (old-ssh-deploy-remote-sql-server ssh-deploy-remote-sql-server)
        (old-ssh-deploy-remote-sql-user ssh-deploy-remote-sql-user)
        (default-directory remote-path))
    (defvar sql-database)
    (set (make-local-variable 'sql-database) old-ssh-deploy-remote-sql-database)
    (defvar sql-password)
    (set (make-local-variable 'sql-password) old-ssh-deploy-remote-sql-password)
    (defvar sql-port)
    (set (make-local-variable 'sql-port) old-ssh-deploy-remote-sql-port)
    (defvar sql-server)
    (set (make-local-variable 'sql-server) old-ssh-deploy-remote-sql-server)
    (defvar sql-user)
    (set (make-local-variable 'sql-user) old-ssh-deploy-remote-sql-user)
    (cond ((string= sql-type "mysql") (sql-mysql remote-path))
          ((string= sql-type "postgres") (sql-postgres remote-path))
          (t (display-warning 'ssh-deploy (format "SQL type %s not supported" type) :warning)))))

;;;###autoload
(defun ssh-deploy-browse-remote (path-local &optional root-local root-remote exclude-list)
  "Browse PATH-LOCAL in `dired-mode' on remote where it is inside ROOT-LOCAL and mirrored on ROOT-REMOTE and not in EXCLUDE-LIST."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote))
        (exclude-list (or exclude-list ssh-deploy-exclude-list)))
    (when (and (ssh-deploy--file-is-in-path-p path-local root-local)
               (ssh-deploy--file-is-included-p path-local exclude-list))
      (let ((path-remote (expand-file-name (ssh-deploy--get-relative-path root-local path-local) root-remote)))
        (message "Opening '%s' for browsing on remote host.." path-remote)
        (dired path-remote)))))

;;;###autoload
(defun ssh-deploy-remote-terminal-eshell (path-local &optional root-local root-remote exclude-list)
  "Browse PATH-LOCAL inside ROOT-LOCAL on ROOT-REMOTE in `eshell-mode' if not in EXCLUDE-LIST."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote))
        (exclude-list (or exclude-list ssh-deploy-exclude-list)))
    (when (and (ssh-deploy--file-is-in-path-p path-local root-local)
               (ssh-deploy--file-is-included-p path-local exclude-list))
      (let ((path-remote (expand-file-name (ssh-deploy--get-relative-path root-local path-local) root-remote)))
        (message "Opening eshell on '%s'.." path-remote)
        (let ((default-directory path-remote))
          (defvar eshell-buffer-name)
          (setq eshell-buffer-name path-remote)
          (eshell))))))

;;;###autoload
(defun ssh-deploy-remote-terminal-shell (path-local &optional root-local root-remote exclude-list)
  "Browse PATH-LOCAL inside ROOT-LOCAL on ROOT-REMOTE in `eshell-mode' if not in EXCLUDE-LIST."
  (let ((root-local (or root-local ssh-deploy-root-local))
        (root-remote (or root-remote ssh-deploy-root-remote))
        (exclude-list (or exclude-list ssh-deploy-exclude-list)))
    (when (and (ssh-deploy--file-is-in-path-p path-local root-local)
               (ssh-deploy--file-is-included-p path-local exclude-list))
      (let ((path-remote (expand-file-name (ssh-deploy--get-relative-path root-local path-local) root-remote)))
        (message "Opening eshell on '%s'.." path-remote)
        (let ((default-directory path-remote)
              (explicit-shell-file-name ssh-deploy-remote-shell-executable))
          (when explicit-shell-file-name ;; NOTE This is only to trick flycheck to ignore unused error
            (shell path-remote)))))))

;;;###autoload
(defun ssh-deploy-store-revision (path &optional root)
  "Store PATH in revision-folder ROOT."
  (unless (file-directory-p path)
    (let* ((root (or root ssh-deploy-revision-folder))
           (revision-path (ssh-deploy--get-revision-path path root)))
      (when (> ssh-deploy-verbose 0) (message "Storing revision of '%s' at '%s'.." path revision-path))
      (copy-file path revision-path t t t t))))

;;;###autoload
(defun ssh-deploy-diff (path-local path-remote &optional root-local debug exclude-list async async-with-threads on-explicit-save revision-folder remote-changes verbose)
  "Find differences between PATH-LOCAL and PATH-REMOTE, where PATH-LOCAL is inside ROOT-LOCAL.  DEBUG enables feedback message, check if PATH-LOCAL is not in EXCLUDE-LIST.   ASYNC make the process work asynchronously, if ASYNC-WITH-THREADS is above zero use threads, ON-EXPLICIT-SAVE for automatic uploads, REVISION-FOLDER for revision-folder, REMOTE-CHANGES for automatic notification of remote change, VERBOSE messaging if above zero."
  (let ((file-or-directory (not (file-directory-p path-local)))
        (root-local (or root-local ssh-deploy-root-local))
        (debug (or debug ssh-deploy-debug))
        (exclude-list (or exclude-list ssh-deploy-exclude-list))
        (async (or async ssh-deploy-async))
        (async-with-threads (or async-with-threads ssh-deploy-async-with-threads))
        (on-explicit-save (or on-explicit-save ssh-deploy-on-explicit-save))
        (revision-folder (or revision-folder ssh-deploy-revision-folder))
        (remote-changes (or remote-changes ssh-deploy-automatically-detect-remote-changes))
        (verbose (or verbose ssh-deploy-verbose)))
    (if (and (ssh-deploy--file-is-in-path-p path-local root-local)
             (ssh-deploy--file-is-included-p path-local exclude-list))
        (if file-or-directory
            (ssh-deploy-diff-files path-local path-remote async async-with-threads verbose)
          (ssh-deploy-diff-directories path-local path-remote on-explicit-save debug async async-with-threads revision-folder remote-changes exclude-list))
      (when debug (message "Path '%s' is not in the root '%s' or is excluded from it." path-local root-local)))))

;;;###autoload
(defun ssh-deploy-upload (path-local path-remote &optional force async revision-folder async-with-threads)
  "Upload PATH-LOCAL to PATH-REMOTE and ROOT-LOCAL via Tramp, FORCE uploads despite remote change, ASYNC determines if transfer should be asynchronously, check version in REVISION-FOLDER.  If you want asynchronous threads pass ASYNC-WITH-THREADS above zero."
  (let ((force (or force 0))
        (async (or async ssh-deploy-async))
        (revision-folder (or revision-folder ssh-deploy-revision-folder))
        (async-with-threads (or async-with-threads ssh-deploy-async-with-threads)))
    (if (> async 0)
        (ssh-deploy--upload-via-tramp-async path-local path-remote force revision-folder async-with-threads)
      (ssh-deploy--upload-via-tramp path-local path-remote force revision-folder))))

;;;###autoload
(defun ssh-deploy-download (path-remote path-local &optional async revision-folder async-with-threads)
  "Download PATH-REMOTE to PATH-LOCAL via Tramp, ASYNC determines if transfer should be asynchrous or not, check for revisions in REVISION-FOLDER.  If you want asynchronous threads pass ASYNC-WITH-THREADS above zero."
  (let ((async (or async ssh-deploy-async))
        (revision-folder (or revision-folder ssh-deploy-revision-folder))
        (async-with-threads (or async-with-threads ssh-deploy-async-with-threads)))
    (if (> async 0)
        (ssh-deploy--download-via-tramp-async path-remote path-local revision-folder async-with-threads)
      (ssh-deploy--download-via-tramp path-remote path-local revision-folder))))


;; HANDLERS
;;
;; these functions are suited to be bound to various Emacs commands.
;; these functions MUST depend on module variables.


;;;###autoload
(defun ssh-deploy-upload-handler (&optional force)
  "Upload current path to remote if it is configured for deployment and if remote version hasn't changed or FORCE is specified."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote))
    (let ((root-local (file-truename ssh-deploy-root-local))
          (force (or force 0))
          path-local)
      (if (and (ssh-deploy--is-not-empty-string-p buffer-file-name)
               (file-exists-p buffer-file-name))
          (setq path-local (file-truename buffer-file-name))
        (when (and (ssh-deploy--is-not-empty-string-p default-directory)
                   (file-exists-p default-directory))
          (setq path-local (file-truename default-directory))))
      (if (and (ssh-deploy--is-not-empty-string-p path-local)
               (ssh-deploy--file-is-in-path-p path-local root-local)
               (ssh-deploy--file-is-included-p path-local ssh-deploy-exclude-list))
          (let ((path-remote (expand-file-name (ssh-deploy--get-relative-path root-local path-local) ssh-deploy-root-remote)))
            (ssh-deploy-upload path-local path-remote force ssh-deploy-async ssh-deploy-revision-folder ssh-deploy-async-with-threads))
        (when (> ssh-deploy-debug 0) (message "Ignoring upload, path '%s' is empty, not in the root '%s' or is excluded from it." path-local root-local))))))

;;;###autoload
(defun ssh-deploy-upload-handler-forced ()
  "Upload current path to remote host if it is configured for deployment."
  (interactive)
  (ssh-deploy-upload-handler 1))

;;;###autoload
(defun ssh-deploy-remote-changes-handler()
  "Check if local revision exists or remote file has changed if path is configured for deployment."
  (interactive)
  (if (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
           (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote)
           (ssh-deploy--is-not-empty-string-p buffer-file-name))
      (progn
        (when (> ssh-deploy-debug 0) (message "Detecting remote-changes.."))
        (ssh-deploy-remote-changes (file-truename buffer-file-name) (file-truename ssh-deploy-root-local) ssh-deploy-root-remote ssh-deploy-async ssh-deploy-revision-folder ssh-deploy-exclude-list ssh-deploy-async-with-threads ssh-deploy-verbose))
    (when (> ssh-deploy-debug 0) (message "Ignoring remote-changes check since a root is empty or the current buffer lacks a file-name."))))

;;;###autoload
(defun ssh-deploy-remote-sql-mysql-handler()
  "Open `sql-mysql' on remote path if path is configured for deployment."
  (interactive)
  (when (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote)
    (ssh-deploy-remote-sql ssh-deploy-root-remote "mysql")))

;;;###autoload
(defun ssh-deploy-remote-sql-postgres-handler()
  "Open `sql-postgres' on remote path if path is configured for deployment."
  (interactive)
  (when (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote)
    (ssh-deploy-remote-sql ssh-deploy-root-remote "postgres")))

;;;###autoload
(defun ssh-deploy-open-remote-file-handler()
  "Check if local revision exists or remote file has changed if path is configured for deployment."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote)
             (ssh-deploy--is-not-empty-string-p buffer-file-name))
    (let* ((root-local (file-truename ssh-deploy-root-local))
           (path-local (file-truename buffer-file-name))
           (path-remote (expand-file-name (ssh-deploy--get-relative-path root-local path-local) ssh-deploy-root-remote)))
      (when (> ssh-deploy-verbose 0) (message "Opening file on remote '%s'" path-remote))
      (find-file path-remote))))

;;;###autoload
(defun ssh-deploy-download-handler ()
  "Download current path from remote if it is configured for deployment."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote))
    (let ((root-local (file-truename ssh-deploy-root-local))
          path-local)
      (if (and (ssh-deploy--is-not-empty-string-p buffer-file-name)
               (file-exists-p buffer-file-name))
          (setq path-local (file-truename buffer-file-name))
        (when (and (ssh-deploy--is-not-empty-string-p default-directory)
                   (file-exists-p default-directory))
          (setq path-local (file-truename default-directory))))
      (if (and (ssh-deploy--is-not-empty-string-p path-local)
               (ssh-deploy--file-is-in-path-p path-local root-local)
               (ssh-deploy--file-is-included-p path-local ssh-deploy-exclude-list))
          (let ((path-remote (expand-file-name (ssh-deploy--get-relative-path root-local path-local) ssh-deploy-root-remote)))
            (ssh-deploy-download path-remote path-local ssh-deploy-async ssh-deploy-revision-folder ssh-deploy-async-with-threads))
        (when (> ssh-deploy-debug 0) (message "Ignoring upload, path '%s' is empty, not in the root '%s' or is excluded from it." path-local root-local))))))

;;;###autoload
(defun ssh-deploy-diff-handler ()
  "Compare current path with remote host if it is configured for deployment."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote))
    (if (and (ssh-deploy--is-not-empty-string-p buffer-file-name)
             (file-exists-p buffer-file-name))
        (let* ((path-local (file-truename buffer-file-name))
               (root-local (file-truename ssh-deploy-root-local))
               (path-remote (expand-file-name (ssh-deploy--get-relative-path root-local path-local) ssh-deploy-root-remote)))
          (ssh-deploy-diff path-local path-remote root-local ssh-deploy-debug ssh-deploy-exclude-list ssh-deploy-async ssh-deploy-async-with-threads ssh-deploy-on-explicit-save ssh-deploy-revision-folder ssh-deploy-automatically-detect-remote-changes))
      (when (and (ssh-deploy--is-not-empty-string-p default-directory)
                 (file-exists-p default-directory))
        (let* ((path-local (file-truename default-directory))
               (root-local (file-truename ssh-deploy-root-local))
               (path-remote (concat ssh-deploy-root-remote (ssh-deploy--get-relative-path root-local path-local))))
          (ssh-deploy-diff path-local path-remote root-local ssh-deploy-debug ssh-deploy-exclude-list ssh-deploy-async ssh-deploy-async-with-threads ssh-deploy-on-explicit-save ssh-deploy-revision-folder ssh-deploy-automatically-detect-remote-changes))))))

;;;###autoload
(defun ssh-deploy-delete-handler ()
  "Delete current file or directory."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote))
    (if (and (ssh-deploy--is-not-empty-string-p buffer-file-name)
             (file-exists-p buffer-file-name))
        (let* ((path-local (file-truename buffer-file-name))
               (root-local (file-truename ssh-deploy-root-local))
               (yes-no-prompt (read-string (format "Type 'yes' to confirm that you want to delete the file '%s': " path-local))))
          (when (string= yes-no-prompt "yes")
            (ssh-deploy-delete-both path-local root-local ssh-deploy-root-remote ssh-deploy-async ssh-deploy-debug ssh-deploy-exclude-list ssh-deploy-async-with-threads)))
      (when (and (ssh-deploy--is-not-empty-string-p default-directory)
                 (file-exists-p default-directory))
        (let* ((path-local (file-truename default-directory))
               (root-local (file-truename ssh-deploy-root-local))
               (yes-no-prompt (read-string (format "Type 'yes' to confirm that you want to delete the directory '%s': " path-local))))
          (when (string= yes-no-prompt "yes")
            (ssh-deploy-delete-both path-local root-local ssh-deploy-root-remote ssh-deploy-async ssh-deploy-debug ssh-deploy-exclude-list ssh-deploy-async-with-threads)))))))

;;;###autoload
(defun ssh-deploy-rename-handler ()
  "Rename current file or directory."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote))
    (if (and (ssh-deploy--is-not-empty-string-p buffer-file-name)
             (file-exists-p buffer-file-name))
        (let* ((old-path-local (file-truename buffer-file-name))
               (root-local (file-truename ssh-deploy-root-local))
               (basename (file-name-nondirectory old-path-local))
               (new-path-local-tmp (read-file-name "New file name:" (file-name-directory old-path-local) basename nil basename))
               (new-path-local (file-truename new-path-local-tmp)))
          (unless (string= old-path-local new-path-local)
            (ssh-deploy-rename old-path-local new-path-local root-local ssh-deploy-root-remote ssh-deploy-async ssh-deploy-debug ssh-deploy-exclude-list ssh-deploy-async-with-threads)))
      (when (and (ssh-deploy--is-not-empty-string-p default-directory)
                 (file-exists-p default-directory))
        (let* ((old-path-local (file-truename default-directory))
               (root-local (file-truename ssh-deploy-root-local))
               (basename (file-name-nondirectory old-path-local))
               (new-path-local-tmp (read-file-name "New directory name:" (file-name-directory old-path-local) basename nil basename))
               (new-path-local (file-truename new-path-local-tmp)))
          (unless (string= old-path-local new-path-local)
            (ssh-deploy-rename old-path-local new-path-local root-local ssh-deploy-root-remote ssh-deploy-async ssh-deploy-debug ssh-deploy-exclude-list ssh-deploy-async-with-threads)))))))

;;;###autoload
(defun ssh-deploy-remote-terminal-eshell-handler ()
  "Open current relative path on remote host in `eshell' but only if it's configured for deployment."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote)
             (ssh-deploy--is-not-empty-string-p default-directory))
    (let ((path-local (file-truename default-directory))
          (root-local (file-truename ssh-deploy-root-local)))
      (ssh-deploy-remote-terminal-eshell path-local root-local ssh-deploy-root-remote ssh-deploy-exclude-list))))

;;;###autoload
(defun ssh-deploy-remote-terminal-eshell-base-handler ()
  "Open base path on remote host in `eshell' but only if it's configured for deployment."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote))
    (let ((root-local (file-truename ssh-deploy-root-local)))
      (ssh-deploy-remote-terminal-eshell root-local root-local ssh-deploy-root-remote ssh-deploy-exclude-list))))

;;;###autoload
(defun ssh-deploy-remote-terminal-shell-handler ()
  "Open current relative path on remote host in `eshell' but only if it's configured for deployment."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote)
             (ssh-deploy--is-not-empty-string-p default-directory))
    (let ((path-local (file-truename default-directory))
          (root-local (file-truename ssh-deploy-root-local)))
      (ssh-deploy-remote-terminal-shell path-local root-local ssh-deploy-root-remote ssh-deploy-exclude-list))))

;;;###autoload
(defun ssh-deploy-remote-terminal-shell-base-handler ()
  "Open base path on remote host in `eshell' but only if it's configured for deployment."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote))
    (let ((root-local (file-truename ssh-deploy-root-local)))
      (ssh-deploy-remote-terminal-shell root-local root-local ssh-deploy-root-remote ssh-deploy-exclude-list))))

;;;###autoload
(defun ssh-deploy-browse-remote-handler ()
  "Open current relative path on remote host in `dired-mode' if it is configured for deployment."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote)
             (ssh-deploy--is-not-empty-string-p default-directory))
    (let ((path-local (file-truename default-directory))
          (root-local (file-truename ssh-deploy-root-local)))
      (ssh-deploy-browse-remote path-local root-local ssh-deploy-root-remote ssh-deploy-exclude-list))))

;;;###autoload
(defun ssh-deploy-browse-remote-base-handler ()
  "Open base path on remote host in `dired-mode' if it is configured for deployment."
  (interactive)
  (when (and (ssh-deploy--is-not-empty-string-p ssh-deploy-root-local)
             (ssh-deploy--is-not-empty-string-p ssh-deploy-root-remote))
    (let ((root-local (file-truename ssh-deploy-root-local)))
      (ssh-deploy-browse-remote root-local root-local ssh-deploy-root-remote ssh-deploy-exclude-list))))

;;;###autoload
(defun ssh-deploy-run-deploy-script-handler ()
  "Run `ssh-deploy-script' with `funcall'."
  (interactive)
  (if ssh-deploy-script
      (if (> ssh-deploy-async 0)
          (let ((root-local ssh-deploy-root-local)
                (root-remote ssh-deploy-root-remote)
                (script ssh-deploy-script))
            (message "Executing of deployment-script starting... (asynchronously)")
            (ssh-deploy--async-process
             (lambda() (let ((ssh-deploy-root-local root-local)
                             (ssh-deploy-root-remote root-remote))
                         (funcall script)))
             (lambda(result) (message "Completed execution of deployment-script. Return: '%s' (asynchronously)" result))
             ssh-deploy-async-with-threads))
        (message "Executing of deployment-script starting... (synchronously)")
        (let ((ret (funcall ssh-deploy-script)))
          (message "Completed execution of deployment-script. Return: '%s' (synchronously)" ret)))
    (display-warning 'ssh-deploy "ssh-deploy-script lacks definition!" :warning)))


;;; Menu-bar

;; Creating a new menu pane named Deployment  in the menu-bar to the right of “Tools” menu
;; This is particularly useful when key-bindings are not working because of some mode
;; overriding them.


(defvar ssh-deploy-menu-map
  (let ((map (make-sparse-keymap "Menu for SSH Deploy")))
    (define-key map [pq] '("PostgreSQL" . ssh-deploy-remote-sql-postgres-handler))
    (define-key map [mq] '("MySQL" . ssh-deploy-remote-sql-mysql-handler))
    (define-key map [sep1] '("--"))
    (define-key map [sb] '("Shell Base" . ssh-deploy-remote-terminal-shell-base-handler))
    (define-key map [ss] '("Shell" . ssh-deploy-remote-terminal-shell-handler))
    (define-key map [sep2] '("--"))
    (define-key map [eb] '("Eshell Base" . ssh-deploy-remote-terminal-eshell-base-handler))
    (define-key map [es] '("Eshell" . ssh-deploy-remote-terminal-eshell-handler))
    (define-key map [sep3] '("--"))
    (define-key map [bb] '("Browse Base" . ssh-deploy-browse-remote-base-handler))
    (define-key map [br] '("Browse" . ssh-deploy-browse-remote-handler))
    (define-key map [sep4] '("--"))
    (define-key map [df] '("Difference" . ssh-deploy-diff-handler))
    (define-key map [rc] '("Detect Remote Changes" . ssh-deploy-remote-changes-handler))
    (define-key map [sep5] '("--"))
    (define-key map [de] '("Delete" . ssh-deploy-delete-handler))
    (define-key map [rn] '("Rename" . ssh-deploy-rename-handler))
    (define-key map [op] '("Open" . ssh-deploy-open-remote-file-handler))
    (define-key map [sep6] '("--"))
    (define-key map [sc] '("Run script" . ssh-deploy-run-deploy-script-handler))
    (define-key map [sep7] '("--"))
    (define-key map [ulf] '("Forced Upload" . ssh-deploy-upload-handler-forced))
    (define-key map [ul] '("Upload" . ssh-deploy-upload-handler))
    (define-key map [dl] '("Download" . ssh-deploy-download-handler))
    map))

(defun ssh-deploy-menu-map-update ()
  "Update menu map and only show menu if deployment is active."
  (if (and ssh-deploy-root-local ssh-deploy-root-remote)
      (define-key-after global-map [menu-bar sshdeploy] (cons "Deployment" ssh-deploy-menu-map) 'tools)
    (define-key-after global-map [menu-bar sshdeploy] 'undefined 'tools)))

(defun ssh-deploy-add-menu ()
  "Add menu-bar support."
  (add-hook 'menu-bar-update-hook 'ssh-deploy-menu-map-update))


;;; Mode Line


(define-minor-mode ssh-deploy-line-mode
  "Show SSH Deploy status in mode line"
  :global t
  :require 'ssh-deploy
  (add-to-list 'global-mode-string 'ssh-deploy--mode-line-status-text t))

(ssh-deploy--mode-line-status-refresh)


;;; Usability shortcuts


(defun ssh-deploy-after-save () "Logic for automatic uploads."
       (when (and (boundp 'ssh-deploy-on-explicit-save) ssh-deploy-on-explicit-save (> ssh-deploy-on-explicit-save 0)) (ssh-deploy-upload-handler)))

;;;###autoload
(defun ssh-deploy-add-after-save-hook () "Add the `after-save-hook'."
       (when (fboundp 'ssh-deploy-after-save)
         (add-hook 'after-save-hook 'ssh-deploy-after-save)))

(defun ssh-deploy-find-file () "Logic for detecting remote change."
       (when (and (boundp 'ssh-deploy-automatically-detect-remote-changes) ssh-deploy-automatically-detect-remote-changes (> ssh-deploy-automatically-detect-remote-changes 0)) (ssh-deploy-remote-changes-handler)))

;;;###autoload
(defun ssh-deploy-add-find-file-hook () "Add the `find-file-hook'."
       (when (fboundp 'ssh-deploy-find-file) (add-hook 'find-file-hook 'ssh-deploy-find-file)))

(defvar ssh-deploy-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'ssh-deploy-upload-handler-force)
    (define-key map "u" 'ssh-deploy-upload-handle)
    (define-key map "D" 'ssh-deploy-delete-handler)
    (define-key map "d" 'ssh-deploy-download-handler)
    (define-key map "x" 'ssh-deploy-diff-handler)
    (define-key map "t" 'ssh-deploy-remote-terminal-eshell-base-handler)
    (define-key map "T" 'ssh-deploy-remote-terminal-eshell-handler)
    (define-key map "h" 'ssh-deploy-remote-terminal-shell-base-handler)
    (define-key map "H" 'ssh-deploy-remote-terminal-shell-handler)
    (define-key map "R" 'ssh-deploy-rename-handler)
    (define-key map "e" 'ssh-deploy-remote-changes-handler)
    (define-key map "b" 'ssh-deploy-browse-remote-base-handler)
    (define-key map "B" 'ssh-deploy-browse-remote-handler)
    (define-key map "o" 'ssh-deploy-open-remote-file-handler)
    (define-key map "m" 'ssh-deploy-remote-sql-mysql-handler)
    (define-key map "s" 'ssh-deploy-run-deploy-script-handler)
    map))
(fset 'ssh-deploy-prefix-map ssh-deploy-prefix-map)


(provide 'ssh-deploy)
;;; ssh-deploy.el ends here
