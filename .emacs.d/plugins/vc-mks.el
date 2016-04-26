;;; vc-mks.el --- VC backend for MKS Integrity (Source)

;; Copyright (C) 2013, 2016 Eduard Wiebe <pusto@web.de>

;; This file is NOT part of GNU Emacs.

;; Keywords: vc tools
;; Package: vc

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Known problems:
;;
;; - Check for working revision, which is not a member revision, fails
;;   without a warning.
;;
;; - `vc-merge' is not implemented
;;
;; - `vc-dir' is not implemented
;;   Study `after-' functionality. Idea: list members and non-members
;;
;; Internal:
;; - Abstract completion interface, "default" and "ido" should be
;;   possible.
;;
;; Usage:
;;  (setq vc-handled-backends '(MKS))   ; for exclusive use
;;    or
;;  (push 'MKS vc-handled-backends)     ; use in parallel with other SCMs
;;  (add-to-list 'load-path "/path/to") ; /path/to/vc-mks.el
;;
;;
;; Alternatives:
;;   https://github.com/sensorflo/mks.git


;;; Code:

(eval-when-compile
  (require 'vc)
  (require 'ido))

(defgroup vc-mks nil
  "VC MKS Integrity backend."
  :version "1.1"
  :group  'vc)

(defvar vc-mks-debug nil
  "Print debug messages if non-nil.")

(defcustom vc-mks-server ""
  "Name of your MKS server."
  :type  'string
  :group 'vc-mks)

(defcustom vc-mks-user (user-login-name)
  "Name of default user."
  :type  'string
  :group 'vc-mks)

(defcustom vc-mks-password nil
  "Default password to login."
  :type  'string
  :group 'vc-mks)

(defcustom vc-mks-diff-switches "--context=3"
  "Number of lines for context."
  :type  'string
  :group 'vc-mks)

;; Some constant definitions
(defconst vc-mks--common-switches '("--batch" "--yes" "--quiet"))
(defconst vc-mks--si-command "si")
(defconst vc-mks--cmd-buffer-name "*mks*")

(defun vc-mks-revision-granularity () 'file)

;;;###autoload
(defun vc-mks-registered (file)
  (let ((pj-files (directory-files "." nil ".*\.pj\\'")))
    ;; -- Cache anwerfen?
    (catch 'found
      (with-temp-buffer
	(dolist (pj-file pj-files)
	  (erase-buffer)
	  (insert-file-contents pj-file)
	  (goto-char (point-min))
	  (cond ((re-search-forward
		  (concat ".*\\$(projectdir)/"
			  (file-name-nondirectory file)) nil t 1)
		 (vc-file-setprop file 'sandbox pj-file)
		 (throw 'found t))))))))


(defun vc-mks-state (file)
  "MKS-specific version of `vc-state'."
  (if (not (vc-mks-registered file)) 'unregistered
    (with-temp-buffer
      (vc-mks--command (current-buffer) 0 file
                       "viewsandbox"
                       (format "--sandbox=%s"
                               (vc-file-getprop file 'sandbox)))
      (goto-char (point-min))
      (re-search-forward
       ".*archived *\\([0-9.]+\\)\\(?:.*(\\(.*\\)):exclusive\\)?")

      (let ((memrev (match-string 1))
	    (locker (match-string 2))
	    wfdelta wfrev)

	;; Hier ist die Reihenfolge der Abfragen wichtig.  Will man
	;; sich darauf nicht mehr verlassen, so bitte die Anfragen
	;; mittels `save-excursion' kapseln.
	(when (re-search-forward
	       "Working file corresponds to revision \\([0-9.]+\\)"
	       (point-max) t 1)
	  (setq wfrev (match-string 1)))

	;; Get locker if working revision is locked.
	(when (re-search-forward
	       "Working file .*locked.*\\((.*)\\)"
	       (point-max) t 1)
	  (setq locker (match-string 1)))

	;; Any changes on working revision?
	(setq wfdelta
	      (re-search-forward
	       "Working file .*\\(?:smaller\\|larger\\|newer\\|older\\)"
	       (point-max) t 1))
	(cond ((and (not wfdelta) (not wfrev) (not locker)) 'up-to-date)
	      ((and locker (not (string= locker (user-login-name)))) locker)
	      ((and (not wfdelta) wfrev) 'needs-update)
	      ((and wfdelta wfrev) 'needs-merge)
	      (wfdelta 'edited))))))


(defun vc-mks-responsible-p (file)
  (message "File: %s" file))

(defun vc-mks-working-revision (file)
  "MKS-specific version of `vc-working-revision'."
  (with-temp-buffer
    (vc-mks--command (current-buffer) 1 file
                     "viewsandbox"
                     "--fields=workingrev"
                     (format "--sandbox=%s" (vc-file-getprop file 'sandbox)))
    (buffer-substring-no-properties (point-min) (1- (point-max)))))

(defun vc-mks-checkout-model (file)
  "MKS has a strict locking model."
  'locking)

(defun vc-mks-create-repo (backend)
  (message "Ignoring `create-repo', hmm."))

(defun vc-mks-register (files &optional rev comment)
  (message "Register %S rev:%s comment:%s" files rev comment)
  (apply 'vc-mks--command-with-cpid nil 1 files "add"))

(defun vc-mks-checkin (files rev comment)
  (vc-mks--command-with-cpid nil 0 files
                             "ci" (format "--description=%s" comment)))

(defun vc-mks-find-revision (file rev buffer)
  (vc-mks--command-with-cpid buffer 0 file
                             "co" (format "--revision=%s"
                                          (cond ((string= "" rev) ":member")
                                                (t rev)))))

(defun vc-mks-checkout (file &optional editable rev)
  (vc-mks--command-with-cpid nil 0 file
                             "co"
                             (when rev
                               (format "--revision=%s"
                                       (cond ((string= rev "") ":member")
                                             ((eq rev t) ":head")
                                             (t rev))))))

(defun vc-mks-revert (file &optional contents-done)
  (if (file-directory-p file)
      (mapc 'vc-mks-revert (vc-expand-dirs (list file)))
    (vc-mks--command nil 1 file "revert")))

(defun vc-mks-print-log (files buffer &optional shortlog start-revision limit)
  (let ((coding-system-for-write 'cp850)
	(coding-system-for-read  'cp850))
    (vc-mks--command buffer 0 files "rlog")
    (when limit 'limit-unsupported)))

;; (defun vc-mks-log-outgoing (backend remote-location)
;;   )

;; (defun vc-mks-log-incomint (backend remote-location)
;;   )

(defun vc-mks-diff (files &optional rev1 rev2 buffer)
  "Get a difference report between two files."
  (apply 'vc-mks--command buffer 16
	 (vc-expand-dirs files)
	 "diff"
	 (append (list (and rev1 (concat "-r" rev1))
		       (and rev2 (concat "-r" rev2)))
		 (vc-switches 'mks 'diff))))


(defun vc-mks-merge (file r1 &optional r2)
  "Merge changes into current working copy of FILE."
  (vc-mks--command nil 0 file
                   "merge"
                   "--mergeType=automatic"
                   "--onMergeConflict=highlight"
                   "-r" (if r2 (format "%s -r %s" r1 r2) r1))
  (vc-file-setprop file 'vc-status 'edited)
  (with-current-buffer (get-buffer vc-mks--cmd-buffer-name)
    (goto-char (point-min))
    (if (re-search-forward "Warning: .* had .* overlap during merge") 1 0)))

(defun vc-mks-annotate-command (file buffer &optional rev)
  (vc-mks--command buffer 1 file "annotate" (vc-switches 'MKS 'annotate)))

(defun vc-mks-annotate-time ()
  nil)

(defun vc-mks-annotate-extract-revision-at-line ()
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward "\\<[0-9.]+\\>" (line-end-position) t 1)
      (buffer-substring (match-beginning 0) (match-end 0)))))

(defun vc-mks--after-dir-status (dir update-function)
  (message "AfterDirStatus: %s" dir)
  (let ((result nil)
	(len (length dir)))
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at "^\s+\\(.*\\)\s+\\(archived\\|\\(?:shared-\\)?subsandbox\\)\\(?:\s*\\([0-9.]+\\)?\\)")
	(setq result (cons
		      (list (replace-regexp-in-string "\\\\" "/" (substring (match-string 1) len))
			    'added
			    'hallo)
		      result)))
      (forward-line))
    (funcall update-function result nil)))

(defun vc-mks-dir-status (dir update-function)
  (vc-mks--command (current-buffer) 'async nil "viewsandbox")
  (vc-exec-after
   `(vc-mks--after-dir-status ,dir #',update-function)))

(defun vc-mks-status-files (dir files default-sate update-function)
  (vc-mks--command (current-buffer) 'async files "viewsandbox")
  (vc-exec-after
   `(vc-mks--after-dir-status ,dir ',update-function)))

(defun vc-mks-dir-extra-headers (dir)
  (concat (propertize "Server     : " 'face 'font-lock-type-face)
	  (propertize "cm1-dspace" 'face 'font-lock-variable-name-face)
	  "\n"
	  (propertize "Project    : " 'face 'font-lock-type-face)
	  (propertize dir 'face 'font-lock-variable-name-face)
	  "\n"
	  (propertize "Branch     : " 'face 'font-lock-type-face)
	  (propertize "trunk" 'face 'font-lock-variable-name-face)))

(defun vc-mks-previous-revision (file rev)
  (vc-call-backend 'RCS 'previous-revision file rev))

(defun vc-mks-next-revision (file rev)
  (vc-call-backend 'RCS 'next-revision file rev))

;; Additional MKS specific functions
(defun vc-mks-connect ()
  (interactive)
  (let ((server (or vc-mks-server (read-from-minibuffer "Server: ")))
	(user   (or vc-mks-user   (read-from-minibuffer "User: " (user-login-name)))))
    ;; Use `vc-do-command' to avoid the endless loop.
    (vc-do-command vc-mks--cmd-buffer-name 0 vc-mks--si-command nil "connect"
		   "-g"
		   (concat "--hostname=" server)
		   (concat "--user=" user))))

(defun vc-mks-disconnect ()
  (interactive)
  ;; Use `vc-do-command' to avoid the re-connection.
  (vc-do-command vc-mks--cmd-buffer-name 0 vc-mks--si-command nil "disconnect"))

;; Helper
(defun vc-mks--command-with-cpid (buffer okstatus file-or-list &rest flags)
  (let ((cpid (vc-mks--read-change-package)))
    (apply #'vc-mks--command buffer okstatus file-or-list
	   (append flags (list (format "--cpid=%s" cpid))))))

(defun vc-mks--command (buffer okstatus file-or-list &rest flags)
  (vc-mks-connect)
  (and vc-mks-debug (message "Command: mks %S" flags))
  (apply 'vc-do-command (or buffer vc-mks--cmd-buffer-name)
	 okstatus
	 vc-mks--si-command
	 file-or-list
	 (append flags vc-mks--common-switches)))

(defun vc-mks--read-change-package ()
  (let ((cps nil)
        (cp  nil))
    (with-temp-buffer
      (vc-mks--command (current-buffer) 0 nil "viewcps" "--fields=id,summary")
      (goto-char (point-min))
      (while (not (eobp))
	(setq cps (cons (buffer-substring-no-properties
			 (line-beginning-position)
			 (line-end-position))
			cps))
	(forward-line))
      (setq cps (nreverse cps)))
    (setq cp (ido-completing-read "Change package: " cps nil 'require-match))
    (string-match "^\\([0-9]+:[0-9]+\\)" cp)
    (match-string 0 cp)))

;; log-view extensions
(require 'log-view)

(defvar log-view-file-re)
(defvar log-view-per-file-logs)
(defvar log-view-font-lock-keywords)

;; Stolen from vc-git/vc-hg.el
(define-derived-mode vc-mks-log-view-mode log-view-mode "MKS-Log-View"
  (require 'add-log) ;; We need the faces add-log.
  (set (make-local-variable 'log-view-file-re) "^\\(?:member name:\\) *\\(?1:.+\\);")
  (set (make-local-variable 'log-view-per-file-logs) nil)
  (set (make-local-variable 'log-view-font-lock-keywords)
       (append
	`((,log-view-message-re . log-view-message-face)
	  (,log-view-file-re
	   (1 log-view-file-face)))
	'(("working file: *\\(.+\\)"
	   (1 'log-view-file-face))
	  ("author: *\\(.+?)\\);"
	   (1 'change-log-name))
	  ("date: *\\(.+?\\);"
	   (1 'change-log-date))
	  ("change package: *\\(.+\\)"
	   (1 'change-log-list-face))))))

(provide 'vc-mks)

;; vc-mks.el ends here
