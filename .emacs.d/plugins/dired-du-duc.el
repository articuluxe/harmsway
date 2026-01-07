;;; dired-du-duc.el --- Speed up dired-du with duc -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:   Martin Edström <meedstrom91@gmail.com>
;; URL:      https://github.com/meedstrom/dired-du-duc
;; Created:  2025-12-03
;; Keywords: files
;; Package-Requires: ((emacs "29.1") (dired-du "0.5.2"))

;;; Commentary:

;; This adapts Dired-Du to use "duc".  Get duc:

;;   https://duc.zevv.nl/
;;   https://github.com/zevv/duc

;; Enable with

;;   (global-dired-du-duc-mode)

;; You'll also want to remove any call to `dired-du-mode' in your initfiles.

;; The global mode does three things:

;; 1. Asynchronously run "duc index" each time a Dired buffer is opened.
;;    Option `dired-du-duc-index-predicate' controls this; the default is to
;;    avoid doing this for remote network directories.

;; 2. Turn `dired-du-duc-mode' on in relevant buffers when duc is ready.
;;    Option `dired-du-duc-mode-predicate' can be configured to enable it
;;    always, if you are fine with the slow "du" as a fallback.

;; 3. Regularly re-index the directories we have previously indexed.
;;    Option `dired-du-duc-delay' controls how often to do this.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'dired-du)

(defgroup dired-du-duc ()
  "Faster Dired-Du, via the Linux utility \"duc\"."
  :group 'dired-du
  :link '(url-link "https://github.com/zevv/duc")
  :link '(url-link "https://duc.zevv.nl/"))


;;;; Local mode

(defvar-local dired-du-duc-using-duc nil
  "Whether current buffer is using duc in place of du.")

(defun dired-du-duc-db-p ()
  "Non-nil if duc is in PATH and ~/.cache/duc/duc.db is writable.
Creates duc.db if it didn't exist."
  (and (executable-find "duc")
       (eq 0 (call-process "duc" nil nil nil "index" (null-device)))))

(defun dired-du-duc-indexed-p ()
  "Non-nil if current directory has been indexed."
  ;; NOTE: Do not pass `default-directory'; duc ls resolves symlinks only if
  ;; passed "." or no argument.
  (eq 0 (call-process "duc" nil nil nil "ls")))

(defvar dired-du-duc--inhibit-cyclical-revert nil)
(defun dired-du-duc--wrap (fn &rest args)
  "Apply FN to ARGS while maybe overriding `dired-du-used-space-program'.
Also ensure `dired-du-duc--handle-done' does not run too early."
  (setq dired-du-duc--inhibit-cyclical-revert t)
  (unwind-protect (if dired-du-duc-using-duc
                      (let ((dired-du-used-space-program '("duc" "ls -bD"))
                            (inhibit-message t))
                        (apply fn args))
                    (apply fn args))
    (setq dired-du-duc--inhibit-cyclical-revert nil)))

;;;###autoload
(define-minor-mode dired-du-duc-mode
  "Show real directory sizes in Dired, using du or duc.

You should not enable both `dired-du-mode' and `dired-du-duc-mode'.
They are the same, and both obey most user configurations for Dired-Du.

Unlike `dired-du-mode', the buffer-local `dired-du-duc-mode' entails
no global side effects.
To turn it on in all relevant buffers, configure
`dired-du-duc-mode-predicate' and enable `global-dired-du-duc-mode'.

-----"
  :lighter (:eval (if dired-du-duc-using-duc " duc" " du"))
  (cond
   (dired-du-duc-mode
    (when (not (derived-mode-p 'dired-mode))
      (dired-du-duc-mode 0)
      (error "Not a Dired buffer"))
    (when (and (boundp 'ls-lisp-use-insert-directory-program)
               (null ls-lisp-use-insert-directory-program)
               (not (bound-and-true-p global-dired-du-duc-mode)))
      (display-warning
       'dired-du-duc "No ls-lisp support without `global-dired-du-duc-mode'"))
    (when dired-du-mode
      (dired-du-mode 0))
    (if (and (dired-du-duc-db-p)
             (dired-du-duc-indexed-p))
        (setq-local dired-du-duc-using-duc t)
      (kill-local-variable 'dired-du-duc-using-duc))
    ;; Upstream does many gratuitous checks on variable `dired-du-mode'.
    (setq-local dired-du-mode :dired-du-duc-pretending-to-be-dired-du)
    (add-hook 'dired-before-readin-hook #'dired-du--drop-unexistent-files 0 t)
    (add-hook 'dired-after-readin-hook #'dired-du--replace 90 t)
    ;; REVIEW: Dired-Du also has its own revert-buffer-function, but it
    ;; results in calling `dired-du--replace' twice, which seems an odd
    ;; oversight.  Do we need it?
    (add-function :around (local 'revert-buffer-function) #'dired-du-duc--wrap)
    (advice-add 'dired-du--replace :around #'dired-du-duc--wrap)
    (dired-du--replace))

   (t
    (kill-local-variable 'dired-du-duc-using-duc)
    (when (eq dired-du-mode :dired-du-duc-pretending-to-be-dired-du)
      (kill-local-variable 'dired-du-mode))
    (remove-hook 'dired-before-readin-hook #'dired-du--drop-unexistent-files t)
    (remove-hook 'dired-after-readin-hook #'dired-du--replace t)
    (remove-function (local 'revert-buffer-function) #'dired-du-duc--wrap)
    (when (derived-mode-p 'dired-mode)
      (revert-buffer)))))


;;;; Mode predicate

(defcustom dired-du-duc-mode-predicate 'dired-du-duc-indexed-p
  "Predicate for whether a Dired buffer should display recursive sizes.
The sizes are taken from duc if possible, or calculated anew with du.X"
  :type '(radio (function-item dired-du-duc-indexed-p)
                (function-item always)
                (function :tag "Custom predicate" :value (lambda ()))))

(defun dired-du-duc--try-turn-on ()
  "Maybe turn on `dired-du-duc-mode' in current buffer."
  (unless dired-du-duc-mode
    (when (derived-mode-p 'dired-mode)
      (let ((file-name-handler-alist (dired-du-duc--handler-alist)))
        (when (funcall dired-du-duc-mode-predicate)
          (dired-du-duc-mode))))))


;;;; Function `dired-du-duc-index'

(defvar dired-du-duc-before-index-functions nil
  "Hook run with one argument, the list of directories to index.
Called by `dired-du-duc-index' just after starting an async job.")

(defvar dired-du-duc--process-dirs nil)
(defvar dired-du-duc--inhibit-index nil)
(defun dired-du-duc-index (dirs)
  "Run \"duc index\" on DIRS.
Also run `dired-du-duc-before-index-functions',
and arrange to run `dired-du-duc-after-re-index-hook' afterwards."
  (unless dired-du-duc--inhibit-index
    (setq dirs (cl-loop for dir in (mapcar #'expand-file-name (ensure-list dirs))
                        when (file-readable-p dir)
                        collect dir))
    (when dirs
      (cl-assert
       (not (cl-intersection dirs
                             (seq-mapcat #'cdr dired-du-duc--process-dirs)
                             :test #'equal)))
      (let ((proc (apply #'start-process
                         "duc" " *duc*"
                         "duc" "index" "-v" dirs)))
        (push (cons proc dirs) dired-du-duc--process-dirs)
        (set-process-sentinel proc #'dired-du-duc--handle-done))
      (run-hook-with-args 'dired-du-duc-before-index-functions dirs))))

(defvar dired-du-duc-after-re-index-hook nil
  "Hook run in a Dired buffer after duc finished indexing the directory.
As there may not be buffers for every directory indexed, this hook sees
fewer directories than `dired-du-duc-before-index-functions' does.")

(defun dired-du-duc--handle-done (proc _event &optional retries)
  "If PROC done, revert any buffers that show the newly indexed dirs."
  (unless retries (setq retries 0))
  (if (and (eq (process-status proc) 'exit)
           (eq (process-exit-status proc) 0))
      (if dired-du-duc--inhibit-cyclical-revert
          ;; See `dired-du-get-recursive-dir-size-in-parallel', called by
          ;; `dired-du--replace'.  It runs `sleep-for', which allows this
          ;; sentinel to run too early.
          (unless (> (cl-incf retries) 50)
            (run-with-timer .2 nil #'dired-du-duc--handle-done proc nil retries))
        (let ((newly-indexed (alist-get proc dired-du-duc--process-dirs)))
          (cl-loop for (dir . buf) in dired-buffers
                   when (and (member dir newly-indexed)
                             (buffer-live-p buf))
                   do (with-current-buffer buf
                        (when (derived-mode-p 'dired-mode)
                          (if (dired-du-duc-indexed-p)
                              (let ((dired-du-duc--inhibit-index t))
                                (setq-local dired-du-duc-using-duc t)
                                (if dired-du-duc-mode
                                    (revert-buffer)
                                  (dired-du-duc--try-turn-on))
                                (run-hooks 'dired-du-duc-after-re-index-hook))
                            (message "dired-du-duc: Could not ls here: %s" dir)))))
          (setq dired-du-duc--process-dirs
                (assq-delete-all proc dired-du-duc--process-dirs))))
    (when (buffer-live-p (get-buffer " *duc*"))
      (display-buffer " *duc*"))
    (error "Unexpected sentinel invocation for process %S" proc)))


;;;; Silly optimization: file name handlers

;; Only matters when you have a stupidly long list of Dired buffers,
;; and do something to revert them all, or toggle the global mode.

(defcustom dired-du-duc-file-handlers nil
  "(A performance knob) List of file name handlers to allow.

If t, allow everything in `file-name-handler-alist'.
If nil, allow none of them.
Otherwise, should be a list of symbols like the cdrs of that variable."
  :type '(choice (const :tag "All" t)
                 (repeat function)))

(defvar dired-du-duc--memo-table (make-hash-table :test #'eq))
(defvar dired-du-duc--memo-timer (timer-create))
(define-inline dired-du-duc--memoize (key &rest body)
  "Eval BODY like `progn' and store non-nil result at KEY in a table.
Repeated calls return the stored value instead of evaluating BODY again.

The stored value is cleared as soon as the current call stack finishes,
or when the likes of `sit-for' give Emacs a chance to run pending timers.

KEY must be a symbol, unquoted."
  (declare (indent defun))
  (inline-quote
   (or (gethash ',key dired-du-duc--memo-table)
       (prog1 (puthash ',key ,(cons 'progn body) dired-du-duc--memo-table)
         (unless (memq dired-du-duc--memo-timer timer-list)
           (setq dired-du-duc--memo-timer
                 (run-with-timer 0 nil #'clrhash dired-du-duc--memo-table))))
       (error "dired-du-duc--memoize: Tried to memoize nil at key %S" ',key))))

(defun dired-du-duc--handler-alist ()
  "Calculate an appropriate value for `file-name-handler-alist'."
  (and dired-du-duc-file-handlers
       (dired-du-duc--memoize dired-du-duc--handler-alist
         (if (eq t dired-du-duc-file-handlers)
             file-name-handler-alist
           (cl-loop for (regexp . handler) in file-name-handler-alist
                    when (member handler dired-du-duc-file-handlers)
                    collect (cons regexp handler))))))


;;;; Global mode

(defcustom dired-du-duc-delay 3600
  "Seconds between each indexing of `dired-du-duc--seen-directories'."
  :type 'number)

(defcustom dired-du-duc-index-predicate 'dired-du-duc-local-p
  "Predicate for whether a directory should be indexed with duc.
Used by `global-dired-du-duc-mode'.

If this is not set to `dired-du-duc-local-p', you may need to
configure `dired-du-duc-file-handlers'."
  :type '(radio (function-item dired-du-duc-local-p)
                (function-item always)
                (function-item ignore)
                (function :tag "Custom predicate" :value (lambda ()))))

(defvar dired-du-duc--seen-directories nil
  "List of directories that passed `dired-du-duc-index-predicate'.")

(defun dired-du-duc--try-index ()
  "Maybe run `dired-du-duc-index' on current directory."
  (unless dired-du-duc--inhibit-index
    (when (derived-mode-p 'dired-mode)
      (let ((file-name-handler-alist (dired-du-duc--handler-alist)))
        (when (funcall dired-du-duc-index-predicate)
          (let ((dir (expand-file-name default-directory)))
            (dired-du-duc-index dir)
            (unless (member dir dired-du-duc--seen-directories)
              (push dir dired-du-duc--seen-directories))))))))

(defun dired-du-duc-local-p ()
  "Non-nil if current directory is on a local filesystem."
  (not (file-remote-p default-directory)))

(defvar dired-du-duc--timer (timer-create))
(defun dired-du-duc--start-timer ()
  "Index `dired-du-duc--seen-directories' and schedule doing it again."
  (cancel-timer dired-du-duc--timer)
  (when (and (numberp dired-du-duc-delay)
             (> dired-du-duc-delay 0))
    (setq dired-du-duc--timer
          (run-with-timer dired-du-duc-delay nil #'dired-du-duc--start-timer))
    (setq dired-du-duc--seen-directories
          (seq-filter #'file-exists-p dired-du-duc--seen-directories))
    (dired-du-duc-index dired-du-duc--seen-directories)))

(defun dired-du-duc--try-turn-on-for-find-dired (fn &rest args)
  "Maybe apply Dired-Du advice to Find-Dired.
FN is presumably `find-dired-sentinel' and ARGS its args."
  (if (funcall dired-du-duc-mode-predicate)
      (apply #'dired-du--find-dired-around fn args)
    (apply fn args)))

(defvar dired-du-duc--overridden-lighter nil
  "Dired-du lighter before enabling `global-dired-du-duc-mode'.")

;;;###autoload
(define-globalized-minor-mode global-dired-du-duc-mode
  dired-du-duc-mode
  dired-du-duc--try-turn-on
  :require 'dired-du-duc
  (cond
   (global-dired-du-duc-mode
    (unless (executable-find "duc")
      (display-warning 'dired-du-duc "No executable \"duc\" in PATH"))
    (when (memq 'dired-du-mode dired-mode-hook)
      (display-warning 'dired-du-duc "Should not have `dired-du-mode' on `dired-mode-hook'"))
    (cl-loop for (_dir . buf) in dired-buffers
             do (with-current-buffer buf
                  (when dired-du-mode
                    ;; Clean up `dired-du-mode' global effects
                    (dired-du-mode 0))
                  (dired-du-duc--try-index)))
    (advice-add 'find-dired-sentinel :around #'dired-du-duc--try-turn-on-for-find-dired)
    (advice-add 'ls-lisp-handle-switches :override #'dired-du-ls-lisp-handle-switches)
    (add-hook 'dired-after-readin-hook #'dired-du-duc--try-index)
    (add-hook 'dired-after-readin-hook #'dired-du-duc--try-turn-on)
    (dired-du-duc--start-timer)
    ;; Delete the " Dired-du" mode line lighter.
    (unless dired-du-duc--overridden-lighter
      (let ((cell (assq 'dired-du-mode minor-mode-alist)))
        (setq dired-du-duc--overridden-lighter (cdr cell))
        (when dired-du-duc--overridden-lighter
          (setcdr cell `(:eval (if (eq dired-du-mode
                                       :dired-du-duc-pretending-to-be-dired-du)
                                   nil
                                 ,dired-du-duc--overridden-lighter)))))))

   (t
    (advice-remove 'find-dired-sentinel #'dired-du-duc--try-turn-on-for-find-dired)
    (advice-remove 'ls-lisp-handle-switches #'dired-du-ls-lisp-handle-switches)
    (remove-hook 'dired-after-readin-hook #'dired-du-duc--try-index)
    (remove-hook 'dired-after-readin-hook #'dired-du-duc--try-turn-on)
    (cancel-timer dired-du-duc--timer)
    ;; Restore the " Dired-du" mode line lighter.
    (when dired-du-duc--overridden-lighter
      (let ((cell (assq 'dired-du-mode minor-mode-alist)))
        (when cell (setcdr cell dired-du-duc--overridden-lighter)))
      (setq dired-du-duc--overridden-lighter nil))
    ;; Bonus but pointless cleanup
    (advice-remove 'dired-du--replace #'dired-du-duc--wrap))))

(provide 'dired-du-duc)

;;; dired-du-duc.el ends here
