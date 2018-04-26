;;; mks.el --- mks source interface for emacs
;; 
;; Copyright 2010-2012 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/mks
;; Created: 2010
;; Keywords: tools 
;; 
;; This file is not part of GNU Emacs.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;;; Commentary: 
;;
;; I always wanted a version control frontend that makes it very easy to switch
;; from each of the following views to each other
;; 
;; - cat      : (content of) file at a certain revision  
;; - annotate : annotated file (between two revisions)
;; - log      : log/history of a file (between two revisions)
;; - stat     : status of files (working revision, has local changes, ...)
;; - diff     : differences of a file (betwen two revisions)
;;
;; Also smart defaults should
;;
;; Altough its a major mode for mks, I use svn's naming instead mks' (e.g. 'cat'
;; instead 'viewrevision'), just because I like it more. 
;;
;;
;; Todo
;; - Menu
;; - Heading line for modes showing tables. header cells having tooltips.
;; - Default change package
;; - blame-mode: optionally put left info into fringe. Toggle-able
;;
;; Bugs
;; - ediff in log doesn work
;; - diff from first line in log doesnt work
;; - mks-cat: jumpt to same pos
;; 
;;; Variables
(require 'cl)
(require 'font-lock-ext) ; https://github.com/sensorflo/font-lock-ext/

(defgroup mks nil
  "Support for mks"
  :group 'tools)

(defcustom mks-blame-view 'cl
  ""
  :type '(list (const gui)
               (const cl))
  :group 'mks)

;; 
(defcustom mks-cat-create-file nil
  "If non-nil `mks-cat' creates a file."
  :type 'boolean
  :group 'mks)

(defvar mks-mode-hook nil
  "Normal hook run when entering mks mode.")

(defface mks-revision
  '((t (:foreground "blue")))
  ""
  :group 'mks)

(defface mks-date
  '((t (:foreground "darkgreen")))
  ""
  :group 'mks)

(defface mks-author
  '((t (:foreground "red")))
  ""
  :group 'mks)

(defface mks-cp
  '((t (:foreground "red")))
  ""
  :group 'mks)

(defface mks-author-initials
  '((t (:weight bold :inherit (mks-author))))
  ""
  :group 'mks)

;; Despite the comment in font-lock.el near 'defvar font-lock-comment-face', it
;; seems I still need variables to refer to faces in adoc-font-lock-keywords.
;; Not having variables and only referring to face names in
;; adoc-font-lock-keywords does not work.
(defvar mks-revision 'mks-revision)
(defvar mks-date 'mks-date)
(defvar mks-author 'mks-author)
(defvar mks-author-initials 'mks-author-initials)

(defvar mks-file-name nil
  "The file the current mks buffer is log/annotation/... of.")

(defvar mks-rev1 nil
  "")

(defvar mks-rev2 nil
  "")

(defvar mks-rev-null "."
  "String representing an unknown revision, or the revision is
  deliberatly displayed as `mks-rev-null' because the actual
  revision can easely be deduced from somewhere else.")

(defvar mks-cp-alist
  '((("calib" "cal")  . "434324:1")
    (("comments" "refactoring" "com" "ref") . "434324:1")
    (("base" "nfw") . "434324:1")
    (("bugfixes" "bug" "bf") . "434324:1"))
  "Maps an alias, actually list of aliases, to a change package id." )

(defvar mks-cp-null "." "")

(defstruct mks-defaults rev1 rev2 rev3 file sandbox cp)

;;; Code:

;;; command line interface
;; --------------------------------------------------
(defalias 'mks-checkout 'mks-co)  ; mks svn naming
(defun mks-co (file rev &optional gui) ; mks svn naming
  (interactive (mks-interactive-file-rev-gui "co"))
  (mks-cmd "co" file nil nil (concat "--revision=" rev) (when gui "--gui"))
  (revert-buffer))

(defalias 'mks-commit 'mks-ci)  ; svn naming
(defalias 'mks-checkin 'mks-ci)  ; mks naming
(defun mks-ci (file msg &optional gui) ; mks svn naming
  (interactive (mks-interactive-file-msg-gui "ci"))
  (mks-cmd "ci" file nil nil "--nocheckinUnchanged" "--nocloseCP" (concat "--description=" msg) (when gui "--gui"))
  (revert-buffer))

(defun mks-lock (file) ; mks svn naming
  (interactive)
  (mks-cmd "lock" file nil nil "--gui")
  (revert-buffer))

(defun mks-unlock (&optional file) ; mks svn naming
  (interactive)
  (mks-cmd "unlock" file nil nil "--gui")
  (revert-buffer))

(defalias 'mks-rev 'mks-revert) ; my naming
(defun mks-revert (&optional file) ; mks svn naming
  (interactive)
  (mks-cmd "revert" file nil nil "--gui")
  (revert-buffer))

(defalias 'mks-up 'mks-update) ; svn naming
(defalias 'mks-resync 'mks-update) ; mks naming
(defun mks-update (&optional file) ; svn naming
  (interactive)
  (mks-cmd "resync" file nil nil "--gui")
  (revert-buffer))

;; todo: optionally create file on disk. that would make some lisp stuff easier,
;; because then there's a real file behind the buffer
(defalias 'mks-viewrevision 'mks-cat)   ; mks naming
(defalias 'mks-get 'mks-cat) ; vss naming
(defun mks-cat (file rev-exp) ; svn naming
  "Display revision REV of FILE.
Returns the buffer name of the buffer containing it.
REV can be a revision expression"
  (interactive (mks-interactive-file-rev "cat"))
  (let ((rev (mks-rev-eval rev-exp t)))
    (if (equal rev ":wfile")
        ;; for displaying :wfile we dont't need mks/si
        (progn
          (find-file file)
          (buffer-name))
      ;; for anything else mks/si is needed
      (let* ((buf-name (concat (file-name-nondirectory file) ".~" rev "~"))
	     (buf-or-name (if mks-cat-create-file
			      (find-file (concat (file-name-directory file) buf-name))
			    buf-name))
	     orig-line-no)
	(when (and (not (member major-mode '(mks-log-mode mks-stat-mode)))
		   (string= rev ":wfile"))
	  (setq orig-line-no (line-number-at-pos)))
        (mks-cmd "viewrevision" file buf-or-name nil (concat "--revision=" rev))
        (set (make-local-variable 'mks-rev1) rev) ; todo: make that nicer
        (view-mode t)	
        (when orig-line-no (goto-line orig-line-no))
        (run-hooks 'mks-cat-mode-hook)
        buf-name))))

(defun mks-add (&optional description file) ; svn mks naming
  (interactive "sdescription: ")
  (mks-cmd "add" file nil nil (concat "--description=" description) "--gui")
  (revert-buffer))

(defalias 'mks-drop 'mks-del) ; mks naming
(defalias 'mks-rm 'mks-del)   ; svn naming
(defalias 'mks-remove 'mks-del) ; svn naming
(defalias 'mks-delete 'mks-del) ; svn naming
(defun mks-del (file) ; svn naming
  (interactive)
  (mks-cmd "drop" file nil nil "--nocloseCP" "--noconfirm")
  (kill-buffer))

;; si diff, defaults for revisions:
;; both given: everything clear
;; one(1st): 2nd=:wfile
;; none: 1st=:member  2nd=:wfile
(defalias 'mks-di 'mks-diff) ; svn naming
(defun mks-diff (file rev1-exp rev2-exp) ; svn mks naming
  ""
  (interactive (mks-interactive-file-rev1-rev2 "diff"))
  (let ((rev1 (mks-rev-eval rev1-exp t))
        (rev2 (mks-rev-eval rev2-exp t)))
    (mks-cmd "diff" file "*Diff*" 'diff-mode
             "-r" rev1
             (unless (string= rev2 ":wfile") "-r" rev2))))

;; todo: always create a new buffer; ediff cant handle the same buffer in multiple ediffs????
(defun mks-ediff (file rev1-exp rev2-exp)
  (interactive (mks-interactive-file-rev1-rev2 "diff"))
  (let* ((rev1 (mks-rev-eval rev1-exp t))
         (rev2 (mks-rev-eval rev2-exp t))
         (buf1 (mks-cat file rev1))
         (buf2 (mks-cat file rev2)))
    (ediff-buffers buf1 buf2)))

;; see examples of the different si 'log' commandos at the end of the file
(defalias 'mks-viewhistory 'mks-log)                ; mks naming
(defalias 'mks-hist 'mks-log)                       ; my naming
(defun mks-log (file rev1-exp rev2-exp)             ; svn naming
  (interactive (mks-interactive-file-rev1-rev2 "log"))
  (let* ((rev1 (mks-rev-eval rev1-exp t))
         (rev2 (mks-rev-eval rev2-exp t))
         (buf-name (concat "*Log " (file-name-nondirectory (file-name-sans-versions file)) " " rev1 "-" rev2 "*"))
	 (rev-goto (mks-defaults-rev1 (mks-cmd-defaults "logsingle"))))
    (mks-cmd "print" file buf-name 'mks-log-mode
             "--format={revision}\\t{date}\\t{author}\\n{description}\\n"
             ;; maybe better use filter or 'si viewlocks' to dedect wheter a file has locks
             ;; "--format={revision}\t{date}\t{author}\n{lockrecord}\n{description}\n"
             ;; "--lockseparator='\t'"
             ;; "--lockRecordFormat='{locker}\t{locktype}"
             "--noHeaderFormat"
             "--noTrailerFormat"
	     (unless (and (string= rev1 "1.1") (string= rev2 ":head"))
	       (concat "--range=" rev1 "-" rev2)))
    (when (and rev-goto (not (string= rev-goto ":working")))
      (goto-char (point-min))
      (search-forward-regexp (concat "^" (regexp-quote rev-goto))))))

;; todo: apparently does not work: the revision arg seems to be ignored
(defun mks-log-single (file rev-exp)
  (interactive (mks-interactive-file-rev "logsingle"))
  (let ((rev (mks-rev-eval rev-exp t)))
    (mks-cmd "print" file nil nil
             "--noHeaderFormat"
             ;; "--format={revision}\t{date}\t{author}\n{lockrecord}\n{description}\n"
             ;; "--lockseparator='\t'"
             ;; "--lockRecordFormat='{locker}\t{locktype}"
             "--format={membername}~{revision}~\\n{description}"
             (concat "--revision=" rev))))

;; wfdelta='displays an indicator when the working file is different from the member revision'
;;    Working file 5,996 bytes smaller, newer (Nov 24, 2010 3:23:43 PM)     
;;    [Working file modified, 310 bytes shorter, newer: Nov 24, 2010 3:21:53 PM vs. Nov 22, 2010 11:20:18 AM  ]
;;    [Working file 310 bytes smaller, older (Jan 1, 2010 4:21:42 PM)]
;;    [Working file newer: Nov 25, 2010 6:52:04 PM vs. Nov 25, 2010 6:51:38 PM ] // just touch
;;    [Working file newer: Nov 25, 2010 6:52:35 PM vs. Nov 25, 2010 6:51:38 PM ] // changed chars, but didn't change amount of chars
;; revsyncdelta='displays an indicator for out of sync members'
;;    Working file corresponds to revision 1.2
;;    Working revision non exclusively locked by Florian Kaufmann (flka) in sandbox /home/emp8118035/prog/m/drivers_dev/substratecamera/substratecamera.pj on host CHZGPC0552 using change package 52737:2   
;; newrevdelta='display indicators for new revisions'
;;    New revision available: 1.6
;; state=States are defined by your administrator (from help, MKS Source : Member / Revision Information View) 
;;
;; todo: von irgendeinem der 'nachtraeglichen' fields kann die message '    No working file' kommen
;;
;;
;; non exclusive-Florian Kaufmann (flka)-1.10
;; exclusive-Florian Kaufmann (flka)-1.11
;; Working file corresponds to revision 1.2 // no lock record on first line, maybe because there is a newer revision available than my lock?
;;
;; how --fields= appears to work: jedes field added ein space und fuegt sich
;; selber ein. Aber workingrev,wfdelta fuegen empty string ein, eigentlicher
;; output ist appended.
(defalias 'mks-stat 'mks-status) ; svn naming
(defalias 'mks-st 'mks-status)   ; svn naming
(defalias 'mks-vsb 'mks-status) ; my naming
(defalias 'mks-viewsandbox 'mks-status) ; my naming
(defun mks-status (sandbox) ; mks naming
  (interactive (mks-interactive-sandbox "status"))
  ;; todo: this way to find the project associated with the current file is WAY
  ;; to specific for my system
  (let* ((buf-name (concat "*status " (file-name-nondirectory sandbox) "*")))
    ;; todo: possibly add also the output of 'viewnonmembers' and intermingle
    ;; the two outputs as needed
    ;; todo: include 'writable' indication. But I don't know from where to get
    ;; that info. (setq project "/home/emp8118035/prog/m/morpheus.pj")
    (mks-cmd "viewsandbox" sandbox buf-name 'mks-stat-mode
             "--yes"
             "--fields=name,workingrev,memberrev,newrevdelta,revsyncdelta,wfdelta"
             ;; lockrecord,
             ;; "--lockRecordFormat={locktype}-{locker}-{revision}"
             "-S")))

(defalias 'mks-blame 'mks-annotate) ; svn naming
(defalias 'mks-praise 'mks-annotate)  ; svn naming
(defalias 'mks-ann 'mks-annotate) ; svn naming
(defun mks-annotate (file rev-exp) ; svn mks naming
  (interactive (mks-interactive-file-rev "blame"))
  (let* ((rev (mks-rev-eval rev-exp t))
         (buf-name (concat "*Blame " (file-name-nondirectory (file-name-sans-versions file)) " " rev "*"))
         orig-line-no
	 orig-tab-width
         orig-col-no)
    (when (not (member major-mode '(mks-log-mode mks-stat-mode)))
      (setq orig-line-no (line-number-at-pos))
      (setq orig-col-no (current-column))
      (setq orig-tab-width tab-width))
    (mks-cmd "annotate" file buf-name 'mks-blame-mode
             "--fields=revision,date,author,text"
             (concat "--revision=" rev))
    (when orig-line-no
      (setq tab-width orig-tab-width)
      (goto-line orig-line-no)
      (beginning-of-line)
      (re-search-forward "|")
      (forward-char orig-col-no))
    buf-name))

(defalias 'mks-vcps 'mks-viewcps)      ; my naming
(defun mks-viewcps ()                  ; mks naming
  (interactive)
  ;; todo: mks-cmd can't handle commands that really have no file argument
  (shell-command "si viewcps"))

(defalias 'mks-vl 'mks-view-locks)      ; my naming
(defun mks-view-locks (&optional file)  ; mks naming
  (interactive)
  (mks-cmd "viewlocks" file))

(defun mks-cmd (cmd &optional file buf-or-name mode-fun &rest args)
  "Executes the mks command CMD with ARGS as arguments upon FILE.

The output of the command goes into a buffer if requested. If
BUF-OR-NAME is a string, create a buffer with that name and put
the result in it. If it's a buffer, that buffer is used. If its
nil, the output of command is just displayed, see
`shell-command'.

If MODE-FUN is non-nil, that function is called after the command
wrote its output to the buffer; i.e. MODE-FUN is ignored when
BUF-OR-NAME is nil.

If FILE is nil, (buffer-file-name) is used. "
  (setq file (mks-default-file-name file))
  (let* ((default-args (list ))
         (all-args
          (append (list cmd)
                  (remove* nil default-args)
                  (remove* nil args)
                  (list file)))
         (buf (when buf-or-name
                (if (bufferp buf-or-name)
                    buf-or-name
                  (get-buffer-create buf-or-name))))
         (command (concat "si " (mapconcat 'shell-quote-argument all-args " "))))
    (if (null buf)
        (shell-command command) 
      (display-buffer buf)
      (set-buffer buf)
      (buffer-disable-undo)
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; todo: try to avoid that mks-cmd needs to look at cmd
      (if (string= cmd "viewsandbox")
          (cd (file-name-directory file)))
      ;; ??? why not call si direcly ??
      ;; !!! make it async, for example as grep 
      (when (< 0 (call-process shell-file-name nil buf nil shell-command-switch command))
	(let ((error-str (buffer-substring-no-properties (point-min) (point-max))))
	  (bury-buffer buf)
	  (error (concat "mks (si) returned with error:\n" error-str))))
      (if mode-fun
          (funcall mode-fun)
        ;; todo: make that better: when buffer is not visiting a real file,
        ;; normal-mode does not do what I expect. I expect that it works like
        ;; when visiting a file, just that buffer-name is used insted
        ;; buffer-file-name
        (normal-mode))
      ;; must come *after* mode-fun
      (set (make-local-variable 'mks-file-name) file)
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (pop-to-buffer buf))))

;;; helper commands
;; --------------------------------------------------
(defun mks-cat-prev()
  (interactive)
  (mks-cat (file-name-sans-versions (buffer-name)) "-1"))

(defun mks-cat-next()
  (interactive)
  (mks-cat (file-name-sans-versions (buffer-name)) "+1"))

;;; mks-cat-mode
;; --------------------------------------------------
(defvar mks-cat-mode-hook nil
  "Normal hook run when entering mks-cat mode.")

;;; mks-blame-mode
;; --------------------------------------------------
(defvar mks-blame-mode-hook nil
  "Normal hook run when entering mks-blame mode.")

;; ATTENTION: matches a line after mks-blame-reformat 
(defconst mks-blame-regexp
  "^\\(.*?\\) +\\([a-zA-Z]\\{3\\} [0-9]\\{2\\} [0-9]\\{2\\}\\) *\\([a-zA-Z]+\\) *|\\(.*\\)$"
  "Regexp describing a blame line.")

(defconst mks-blame-font-lock-keywords
  (list
   (list mks-blame-regexp
         '(1 mks-revision)
         '(2 mks-date)
         '(3 mks-author))
   ))

(defun mks-blame-info()
  "`mks-info' for `mks-blame-mode'"
  (save-excursion
    (mks-re-search-backward mks-blame-regexp)
    (when (null (match-data 0))
      (error "Point is not on (the start of) a blame line"))
    (list
     mks-file-name
     (match-string-no-properties 1)
     mks-file-name
     mks-rev-null
     mks-rev-null ; todo: find out working, member, head revision 
     mks-rev-null
     mks-rev-null)))

(defun mks-blame-set-tab-stops()
  (let ((rev 5)
        (date 20)
        (author 60)
        (sum 0))
    (setq tab-stop-list
          (mapcar
           (lambda (x) (setq sum (+ sum x)))
           (append
            (list rev date author)
            (make-list 20 tab-width))))))

(defun mks-blame-reformat()
  (interactive)
  (goto-char 0)
  (while (re-search-forward "^\\([^\t\n]*\\)\t\\([^\t\n]*\\)\t\\([^\t\n]*\\)\t\\(.*\\)$" nil t)
    (let* ((orig-rev (match-string 1))
           (orig-date (match-string 2))
           (orig-author (match-string 3))
           (orig-src-line (match-string 4))
           (rev (mks-conv-rev orig-rev))
           (date (mks-conv-date orig-date))
           (author (mks-conv-author orig-author))
	   (infos (format "%-7s %s %-5s" rev date (mks-shorten author)))
	   ;; padding so tabs in the file display correctly, i.e. the file's
	   ;; line, which starts after the "|", must be at a tab stop
;;	   (padding (make-string (- tab-width (% (1+ (length infos)) tab-width)) ?\ ))) ;1+ because of trailing |, see below
	   (padding (make-string (% 4 (- 4 (% (1+ (length infos)) 4))) ?\ ))) ;1+ because of trailing |, see below
      (delete-region (match-beginning 0) (match-end 0))
      (insert infos padding "|" orig-src-line))))

(define-derived-mode mks-blame-mode text-mode "mks-blame"
  "Major mode for viewing mks blame buffers.
Turning on mks-blame mode runs the normal hook `mks-blame-mode-hook'."
  (set (make-local-variable 'font-lock-defaults) '(mks-blame-font-lock-keywords))
  (mks-blame-set-tab-stops)
  ;; todo: find a better place for reformat, that does not belong here. Now
  ;; you'll get an error when you call mks-blame-mode multiple times in the
  ;; same buffer.
  (mks-blame-reformat) 
  (setq buffer-read-only t)
  (run-hooks 'mks-common-mode-hook 'mks-blame-mode-hook))

;;; mks-stat-mode
;; --------------------------------------------------
(defvar mks-stat-mode-hook nil
  "Normal hook run when entering mks-stat mode.")

(defconst mks-stat-regexp-mks
  (concat
   "\\([^ \n]+\\) " ; 1 member-path
   "\\([^ \n]+\\) " ; 2 workingrev
   "\\([^ \n]+\\) *\n" ; 3 memberrev
   "\\(?: *\\(New revision available.*\\)\n\\)?" ; 4 newrevdelta
   "\\(?: *\\(Working file corresponds.*\n *Working revision.*\\)\n\\)?" ; 5 revsyncdelta
   "\\(?: *\\(Working file.*\\)\n\\)?" ; 6 wfdelta
   )
  "Regexp describing a stat line as it comes from mks. WITHOUT a leading ^!")

;; ATTENTION: matches a line after mks-stat-reformat 
(defconst mks-stat-regexp
  (concat
   "^"
   "\\([^ \n]*\\) *" ; 1 workingrev
   "\\([^ \n]*\\) *" ; 2 memberrev
   "\\([^ \n]*\\) *" ; 3 headrev
   "\\([^ \n]*\\) *" ; 4 wfdelta
   "\\([^ \n]*\\) *" ; 5 sync
   "\\(\\(.*?/\\)?\\([^/\n]*\\)\\)"   ; 6 member-full-path 7 member-(pre-)path, 8 member file name
   )
  "Regexp describing a stat line.")

(defconst mks-stat-font-lock-keywords
  (list
   (list mks-stat-regexp '(7 font-lock-unimportant nil t))
   ))

(defun mks-stat-info()
  "`mks-info' for `mks-stat-mode' buffers"
  (save-excursion
    (mks-re-search-backward mks-stat-regexp)
    (when (null (match-data 0))
      (error "Point is not on (the start of) a stat line"))
    (let ((working-rev (match-string-no-properties 1))
          (member-rev (match-string-no-properties 2))
          (head-rev (match-string-no-properties 3)))
      (list (match-string-no-properties 6) ; line default filename
            member-rev ; line default rev (memberrev)
            "" ; buf default filename
            mks-rev-null ; buf default rev
            (if (string= working-rev mks-rev-null) member-rev working-rev)  ; line working rev
            member-rev ; line member rev
            (if (string= head-rev mks-rev-null) member-rev head-rev) ; line head rev
            ))))

(defun mks-stat-reformat()
  (interactive)
  (goto-char 0)
  ;; todo: error if we skip/dont recognize a line
  (while (re-search-forward mks-stat-regexp-mks nil t)
    (let* ((orig-member-path (match-string 1))
           (orig-workingrev (match-string 2))
           (orig-memberrev (match-string 3))
           (orig-newrevdelta (or (match-string 4) ""))
           (orig-revsyncdelta (or (match-string 5) ""))
           (orig-wfdelta (or (match-string 6) ""))

           (member-path (mks-conv-member-path orig-member-path))
           (workingrev (mks-conv-rev orig-workingrev))
           (memberrev (mks-conv-rev orig-memberrev))
           (headrev (mks-conv-newrevdelta orig-newrevdelta))

           (workingrev (if (mks-rev-equal workingrev memberrev) mks-rev-null workingrev))
           (headrev (if (mks-rev-equal headrev memberrev) mks-rev-null headrev))

           (revsyncdelta (mks-conv-revsyncdelta orig-revsyncdelta))
           (wfdelta (mks-conv-wfdelta  orig-wfdelta)))

      (delete-region (match-beginning 0) (match-end 0))
      (insert (format "%-5s %-5s %-5s %s %s %s\n" workingrev memberrev headrev wfdelta revsyncdelta member-path))))
  (goto-char 0)
  (insert (format "%-5s %-5s %-5s %s %s %s\n" "work" "mem" "head" "wf" "s" "member")))

(define-derived-mode mks-stat-mode text-mode "mks-stat"
  "Major mode for viewing mks stat buffers.
Turning on mks-stat mode runs the normal hook `mks-stat-mode-hook'."
  (set (make-local-variable 'font-lock-defaults) '(mks-stat-font-lock-keywords))
  ;; todo: find a better place for reformat, that does not belong here. Now
  ;; you'll get an error when you call mks-stat-mode multiple times in the
  ;; same buffer.
  (mks-stat-reformat)
  (setq buffer-read-only t)
  (run-hooks 'mks-common-mode-hook 'mks-stat-mode-hook))


;;; mks-log-mode
;; --------------------------------------------------

(defconst mks-log-regexp
  "^\\(.*?\\) +\\([a-zA-Z]\\{3\\} [0-9]\\{2\\} [0-9]\\{2\\}\\) *\\([a-zA-Z]+\\)"
  "Regexp describing a log line.
WARNING: mks-log-reformat depends upon that the first char is a ^")

(defvar mks-log-view-mode 'on-one-line
  "Choose on-one-line or verbose or mks")

(defvar mks-log-mode-hook nil
  "Normal hook run when entering mks-log mode.")

(defun mks-log-info()
  "`mks-info' for `mks-log-mode'"
  (save-excursion
    (mks-re-search-backward mks-log-regexp)
    (when (null (match-data 0))
      (error "Point is not on (the start of) a log line"))
    (list
     mks-file-name
     (match-string-no-properties 1)
     mks-file-name
     mks-rev-null ;todo: could be to-revision of buffer
     mks-rev-null ;todo: find out working, member, head revision
     mks-rev-null
     mks-rev-null
     )))

;; The output from mks log looks like this (exclusive the --- lines):
;; -------------------
;; 1.16	Nov 23, 2010 4:04:47 PM	Alexander Beck (axb)	axb: switch to new start up handling
;; ...
;; 1.1	Aug 24, 2010 5:36:48 PM	Florian Kaufmann (flka)	Initial revision
;; Member added to project /opt/MKS/projects/morpheus/drivers/dispensehandling/dispensehandling.pj 
;; ----------------------
;; 
;; todo: test how later added descriptions are parsed correctly by mks-log-reformat
;; todo: mark revision lines having a lock, + lock type
;; todo: mark working revision line
;; todo: mark member revision line
(defun mks-log-reformat(new-view-mode)
  (interactive)
  (goto-char 0)
  (let* ((old-view-mode mks-log-view-mode)
         (dummy (setq mks-log-view-mode new-view-mode))
         (start-re
          (if (eq old-view-mode 'mks)
              "\\([^\t\n]*\\)\t\\([^\t\n]*\\)\t\\([^\t\n]*\\)\n"
            (concat
             (substring mks-log-regexp 1) ; cut leading ^
             (if (eq old-view-mode 'on-one-line) " *|" " *\n"))))
         (middle-re "\\(.*\\(?:\n.+\\)*?\\)\n")
         (end-re (concat "\\(" start-re "\\|"
                         ".*\\(?:\n[ \t]*\\)*\\'\\)")) ;last non-empty line of buffer
         (full-re (concat "^" start-re middle-re end-re)))

    (while (re-search-forward full-re nil t)
      (let* ((orig-rev (match-string 1))
             (orig-date (match-string 2))
             (orig-author (match-string 3))
             (orig-log-msg (match-string 4))
             (rev (mks-conv-rev orig-rev))
             (date (mks-conv-date orig-date))
             (author (mks-conv-author orig-author))
             (log-msg (mks-conv-log-msg orig-log-msg))
             (format-str (cond
                          ((eq mks-log-view-mode 'on-one-line) "%-5s %s %-5s|%s")
                          ((eq mks-log-view-mode 'verbose) "%-5s %s %-5s\n%s")
                          (t (error "shit")))))
        ;; 5th subexpression doesn't belong to current log entry, it's already
        ;; part of the next entry, or the end of the buffer
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0) (match-end 4))
        (insert (format format-str (mks-shorten rev) date (mks-shorten author) log-msg))))

    ;; delete trailing lines which are not really part of the log table
    (delete-region (point) (point-max))))

(defun mks-log-set-tab-stops()
  (let ((rev 5)
        (date 20)
        (author 60)
        (sum 0))
    (setq tab-stop-list
          (mapcar
           (lambda (x) (setq sum (+ sum x)))
           (append
            (list rev date author)
            (make-list 20 tab-width))))))

(defconst mks-log-font-lock-keywords
  (list
   (list mks-log-regexp
         '(1 mks-revision)
         '(2 mks-date)
         '(3 mks-author))
   ))

(define-derived-mode mks-log-mode text-mode "mks-log"
  "Major mode for viewing mks log buffers.
Turning on mks-log mode runs the normal hook `mks-log-mode-hook'."
  (set (make-local-variable 'font-lock-defaults) '(mks-log-font-lock-keywords))
  (mks-log-set-tab-stops)
  ;; todo: see the analog todo comment above mks-annote-reformat in
  ;; mks-blame-mode
  (set (make-local-variable 'mks-log-view-mode) 'mks) ; right now its mks
  (mks-log-reformat (default-value 'mks-log-view-mode)) ; or use a customized to start width
  (setq buffer-read-only t)
  (run-hooks 'mks-common-mode-hook 'mks-log-mode-hook))


;;; mks-cp-mode (change package)
;; --------------------------------------------------
(defconst mks-cp-regexp
  ""
  "Regexp describing a change package line.
WARNING: mks-cp-reformat depends upon that the first char is a ^")

(defvar mks-cp-view-mode 'on-one-line
  "Choose on-one-line or verbose or mks")

(defvar mks-cp-mode-hook nil
  "Normal hook run when entering mks-cp mode.")

(defun mks-cp-info()
  "`mks-info' for `mks-cp-mode'"
  (save-excursion
    (mks-re-search-backward mks-cp-regexp)
    (when (null (match-data 0))
      (error "Point is not on (the start of) a change package line"))
    (list
     mks-file-name
     (match-string-no-properties 1)
     mks-file-name
     mks-rev-null 
     mks-rev-null 
     mks-rev-null
     mks-rev-null
     )))

(defun mks-cp-reformat(new-view-mode)
  (interactive)
  (goto-char 0)
  (let* ((old-view-mode mks-cp-view-mode)
         (dummy (setq mks-cp-view-mode new-view-mode))
         (start-re
          (if (eq old-view-mode 'mks)
              ""
            (concat
             (substring mks-cp-regexp 1) ; cut leading ^
             (if (eq old-view-mode 'on-one-line) " *|" " *\n"))))
         (middle-re "\\(.*\\(?:\n.+\\)*?\\)\n")
         (end-re (concat "\\(" start-re "\\|"
                         ".*\\(?:\n[ \t]*\\)*\\'\\)")) ;last non-empty line of buffer
         (full-re (concat "^" start-re middle-re end-re)))

    (while (re-search-forward full-re nil t)
      (let* ((orig-cp (match-string 1))
             (orig-author (match-string 3))
             (orig-desc (match-string 4))

             (cp (mks-conv-cp orig-rev))
             (author (mks-conv-author orig-author))
             (desc (mks-conv-cp-desc orig-desc))
             (format-str (cond
                          ((eq mks-cp-view-mode 'on-one-line) "%-5s %s %-4s|%s")
                          ((eq mks-cp-view-mode 'verbose) "%-5s %s %-4s\n%s")
                          (t (error "shit")))))
        ;; 5th subexpression doesn't belong to current change package entry, it's already
        ;; part of the next entry, or the end of the buffer
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0) (match-end 4))
        (insert (format format-str rev date author cp-msg))))

    ;; delete trailing lines which are not really part of the cp table
    (delete-region (point) (point-max))))

(defconst mks-cp-font-lock-keywords
  (list
   (list mks-cp-regexp
         '(1 mks-cp)
         '(2 mks-author))))

(define-derived-mode mks-cp-mode text-mode "mks-cp"
  "Major mode for viewing mks change package buffers.
Turning on mks-cp mode runs the normal hook `mks-cp-mode-hook'."
  (set (make-local-variable 'font-lock-defaults) '(mks-cp-font-lock-keywords))
  (set (make-local-variable 'mks-cp-view-mode) 'mks) ; right now its mks
  (mks-cp-reformat (default-value 'mks-cp-view-mode)) 
  (setq buffer-read-only t)
  (run-hooks 'mks-common-mode-hook 'mks-cp-mode-hook))


;;; miscellaneous
;; --------------------------------------------------
(defvar mks-common-mode-hook nil
  "Normal hook run when entering any of the mks modes.")

(defun mks-conv-cp-alias-to-mks(alias)
  "Returns the change package ALIAS is refering to according to `mks-cp-alist'."
  (let ((i mks-cp-alist)
        found-item)

    (while (and (not found-item) i)
      (if (member alias (caar i))
          (setq found-item (car i))
        (setq i (car i))))

    (when found-item
      (cdr found-item))))

(defun mks-interactive-sandbox (cmd)
  (let* ((defs (mks-cmd-defaults cmd))
         (file (mks-defaults-file defs))
         (sandbox (mks-file-to-sandbox file)))
    (if (and (not (null current-prefix-arg))
             (listp current-prefix-arg))
        ;; user shall give paramters via minibuffer
        (list
         (read-string (concat "sandbox (default " sandbox "): ") nil nil sandbox))
      ;; silently use defaults
      (list sandbox))))

(defun mks-interactive-file-rev-gui(cmd)
  (let* ((defs (mks-cmd-defaults cmd))
         (file (mks-defaults-file defs))
         (rev-exp (mks-defaults-rev1 defs)))
    (if (and (not (null current-prefix-arg))
             (listp current-prefix-arg))
        ;; user shall give paramters via gui
        (if (eq (car current-prefix-arg) 16)
            (list file "" t)
          ;; user shall give paramters via minibuffer
          (list
           (read-string (concat "file (default " file "): ") nil nil file)
           (read-string (concat "revision (default " rev-exp "): ") nil nil rev-exp)
           nil))
      ;; silently use defaults
      (list file rev-exp nil))))

(defun mks-interactive-file-rev(cmd)
  (let* ((defs (mks-cmd-defaults cmd))
         (file (mks-defaults-file defs))
         (rev-exp (mks-defaults-rev1 defs)))
    (if (and (not (null current-prefix-arg))
             (listp current-prefix-arg))
        (list
         (read-string (concat "file (default " file "): ") nil nil file)
         (read-string (concat "revision (default " rev-exp "): ") nil nil rev-exp))
      (list file rev-exp))))

(defun mks-interactive-file-msg-gui(cmd)
  (let* ((defs (mks-cmd-defaults cmd))
         (file (mks-defaults-file defs)))
    (if (and (not (null current-prefix-arg))
             (listp current-prefix-arg))
        ;; user shall give paramters via gui
        (if (eq (car current-prefix-arg) 16)
            (list file "" t)
          ;; user shall give paramters via minibuffer
          (list
           (read-string (concat "file (default " file "): ") nil nil file)
           (read-string (concat "message : "))
           nil))
      ;; silently use defaults - but message still needs to be provided
      (list
       file
       (read-string (concat "message : "))
       nil))))

(defun mks-interactive-file-rev1-rev2(cmd)
  (let* ((defs (mks-cmd-defaults cmd))
         (file (mks-defaults-file defs))
         (rev1-exp (mks-defaults-rev1 defs))
         (rev2-exp (mks-defaults-rev2 defs)))
    (if (and (not (null current-prefix-arg)) (listp current-prefix-arg))
        (list
         (read-string (concat "file (default " file "): ") nil nil file)
         (read-string (concat "revision 1 (default " rev1-exp "): ") nil nil rev1-exp)
         (read-string (concat "revision 2 (default " rev2-exp "): ") nil nil rev2-exp))
      (list file rev1-exp rev2-exp))))

(defun mks-cmd-defaults (cmd)
  "Returns an mks-default struct for given CMD and the current context.
With context the current major-mode and position of point is meant."
  (let* ((defs (mks-info2)))

    ;; its just not right somehow... not also that mks-info2 actually only
    ;; returns defautls for file and rev1, i.e. not for rev2.

    ;; How to dedect wheter the default is one that I want to overtake (e.g. the
    ;; rev on a log line), or one that I want to ignore (:working revision of a normal file)

    ;; maybe also make different stuff depending upon wheter 1 or 2 revisions have defaults. E.g.
    ;; - one rev: log goes from 1 to rev1
    ;; - two rev: log goes from rev1 to rev2

    (cond
     ((and (member cmd '("co" "cat")) (not (mks-buffer-knows-defaults)))
      (setf (mks-defaults-rev1 defs) ":member"))

     ((and (member cmd '("blame")) (not (mks-buffer-knows-defaults)))
      (setf (mks-defaults-rev1 defs) ":working"))

     ((and (member cmd '("blame")) (equal major-mode 'mks-blame-mode))
      (setf (mks-defaults-rev1 defs) (mks-rev-eval (concat (mks-defaults-rev1 defs) "-1"))))

     ((member cmd '("log"))
      (setf (mks-defaults-rev1 defs) "1.1"
	    (mks-defaults-rev2 defs) ":head"))

     ((and (member cmd '("logsingle")) (not (mks-buffer-knows-defaults)))
      (setf (mks-defaults-rev1 defs) ":working"))

     ((member cmd '("diff"))
      (if (mks-buffer-knows-defaults)
          (setf (mks-defaults-rev2 defs) (mks-defaults-rev1 defs)
                (mks-defaults-rev1 defs) (concat (mks-defaults-rev1 defs) "-1"))
        (setf (mks-defaults-rev1 defs) ":working"
              (mks-defaults-rev2 defs) ":wfile"))))

    defs))
  
;; maybe split up which defaults (file rev1 rev2 sandbox...) mks-mode know or
;; doesn't know
(defun mks-buffer-knows-defaults()
  (or
   (member major-mode '(mks-stat-mode mks-log-mode mks-blame-mode))
   (and (local-variable-p 'mks-file-name) (local-variable-p 'mks-rev1))))

(defun mks-non-empty-stringp (str)
  "Returns non-nil when STR is a string and not the empty string."
  (and (stringp str) (> (length str) 0)))

(defun mks-rev-eval (exp &optional to-cli-format)
  "Evaluates the revision expression and returns the result.
When TO-CLI-FORMAT is non-nil, the result is"
  (if (not (string-match "\\([^ \n]*\\) *\\([+-]\\) *\\([0-9]*\\)" exp))
      (when to-cli-format (mks-rev-to-cli exp) exp)
    (let* ((operator (match-string 2 exp))
           (raw-summand-str (match-string 3 exp))
           (raw-summand (if (string= raw-summand-str "") 1 (string-to-number raw-summand-str)))
           (summand (if (string= operator "-") (- raw-summand) raw-summand))
           (rev (match-string 1 exp)))
      (mks-rev-add rev summand))))

(defun mks-rev-add (rev summand)
  "Adds SUMMAND to the last part of the given REVision number.
SUMMAND can of course be negative."
  (let (nr
        (num-rev (mks-rev-to-numeric rev)))
    (when (not (string-match "[0-9]+$" num-rev)) 
      (error "Can't extract numerical part from rev '%s'" rev))
    (setq nr (string-to-number (match-string 0 num-rev)))
    (setq nr (+ nr summand))
    (when (< nr 0)
      (error "I'm currently too dumb; can only change the last part of the revision nubmer"))
    (concat
     (substring num-rev 0 (match-beginning 0))
     (number-to-string nr))))

(defun mks-rev-to-cli (rev)
  "

numeric: 1.12, 5.2.3.12 etc

cli:         mks-mode.el:         
:working     :w, :base, :b
:member      :m
:head        :h
*)           :wfile, :wf

*) "
  (cond
   ((member rev '(":working" ":w" ":base" ":b"))
    ":working")
   ((member rev '(":member" ":m"))
    ":member")
   ((member rev '(":head" ":h"))
    ":head")
   ;; not really a format recognized by mks, but by mks-... defuns directly
   ;; calling mks.
   ((member rev '(":wfile" ":wf"))
    ":wfile")
   (t
    (mks-rev-to-numeric rev t))))

;; todo, using mks-stat on the current file, which delivers the numeric
;; revision of head, working, member
(defun mks-rev-to-numeric (rev &optional no-error-on-unknown)
  (let* ()
    (cond
     ;; is already in numeric format
     ((string-match "[0-9]+\\(\\.[0-9]+\\)*" rev)
      rev)

     ((member rev '(":working" ":w" ":base" ":b"))
      (nth 4 (mks-info)))
     ((member rev '(":member" ":m"))
      (nth 5 (mks-info)))
     ((member rev '(":head" ":h"))
      (nth 6 (mks-info)))

     ((member rev '("" ":line" ":l"))   ; is also the default (note the empty string)
      (nth 1 (mks-info)))
     ((member rev '(":buf" ":b"))
      (nth 3 (mks-info)))
     ((member rev '(":wfile" ":wf"))
      (error "Can't convert working file 'revision' to a numeric format"))
     ((not no-error-on-unknown)
      (error "'%s' is an unkown revision format, can't convert to a numeric format" rev))
     (t
      rev))))

(defun mks-rev-equal (rev1 rev2)
  (or (string= rev1 rev2) (string= rev1 mks-rev-null) (string= rev2 mks-rev-null)))

(defun mks-default-file-name (&optional file)
  (or file (nth 0 (mks-info))))

;; NOTE!! Can also be a buffer made by mks-cat
(defun mks-default-info ()
  "mks-info for a buffer with a non-mks major mode.
It is assumed that it's underlying file is under mks controll."
  (list
   (or (buffer-file-name) mks-file-name) 
   (or mks-rev1 ":wfile")          

   (or (buffer-file-name) mks-file-name)
   (or mks-rev1 ":wfile")

   ":working"
   ":member"
   ":head"))

;; mmmmmmhhh, :working, ,:member, :head can (in general) be deduced from many diffrent places
;; - file associated with current line
;; - file associated with current buf
(defun mks-info ()
  "
0 line default file
1 line default rev

2 buf default file
3 buf default rev

4 :working      todo: find out wheter those 3 NEED to be in numeric rev format. If not, what are they good for?
5 :member
6 :head
"
  (cond
   ((eq major-mode 'mks-blame-mode) (mks-blame-info))
   ((eq major-mode 'mks-log-mode) (mks-log-info))
   ((eq major-mode 'mks-stat-mode) (mks-stat-info))
   ((eq major-mode 'mks-cp-mode) (mks-cp-info))
   (t (mks-default-info))))

;; rename to mks-line-defaults / mks-file-defaults
(defun mks-info2 ()
  "Returns `mks-info' output as an mks-defaults struct."
  (let ((defaults (make-mks-defaults))
        (info (mks-info)))
    (setf (mks-defaults-rev1 defaults) (nth 1 info))
    (setf (mks-defaults-file defaults) (nth 0 info))
    defaults))

(defun mks-ediff-slide (&optional arg)
  ""
  (interactive "p")
  (mks-ediff (mks-rev-add mks-rev1 arg) (mks-rev-add mks-rev2 arg) (default-value 'mks-file-name)))

(defun mks-file-to-sandbox(file)
  "Returns the sandbox file (.pj) the given FILE is a member of."
  (let* ((dir (directory-file-name (file-name-directory file)))
	 prev-dir
	 sandboxes)
    (while (and (not (setq sandboxes (directory-files dir t ".*\\.pj$" t)))
		(not (equal dir prev-dir))) ;; is equal when root is reached
      (setq prev-dir dir
            dir (directory-file-name (file-name-directory dir))))
    (cond
     ((> (length sandboxes) 1)
      (message "found multiple sandboxes, choosing the first: %S" sandboxes))
     ((null sandboxes)
      (error "no sandbox found")))
    (car sandboxes))) 

(defun mks-conv-rev(rev)
  (if (and (stringp rev) (> (length rev) 0))
      rev
      ;; (if (> (length rev) 5)
      ;; 	  (concat (substring rev 4) "\u2026") ; ellipsis
      ;; 	rev)
    mks-rev-null))

(defun mks-conv-cp(cp)
  (if (and (stringp cp) (not (equal cp ""))) cp mks-cp-null))

(defun mks-conv-date(date)
  (save-match-data
    (string-match "\\([a-zA-Z]+\\) *\\([0-9]+\\), *[0-9][0-9]\\([0-9][0-9]\\)" date)
    (let ((month (match-string 1 date))
          (day (string-to-number (match-string 2 date)))
          (year (match-string 3 date)))
      (format "%s %02d %s" month day year))))

(defun mks-shorten(str &optional len)
  (when (null len)
    (setq len 5))
  (if (> (length str) len)
      (concat (substring str 0 (- len 1)) "\u2026")
    str))

(defun mks-conv-author(author)
  (save-match-data
    (replace-regexp-in-string ".*(\\([^)]*\\)).*" "\\1" author)))

(defun mks-conv-log-msg(log-msg)
  (save-match-data
    (cond
     ((eq mks-log-view-mode 'on-one-line)
      (replace-regexp-in-string "\n" "|" log-msg))
     (t
      log-msg))))

(defun mks-conv-member-path(member-path)
  (save-match-data
    (replace-regexp-in-string "^.*?/prog/[^/\n]*/[^/\n]*/[^/\n]*/" "" member-path)))

(defun mks-conv-newrevdelta(newrevdelta)
  (save-match-data
    (let ((x (when (stringp newrevdelta)
               (replace-regexp-in-string "New revision available: *" "" newrevdelta))))
      (mks-conv-rev x))))

(defun mks-conv-revsyncdelta(revsyncdelta)
  (if (and revsyncdelta (> (length revsyncdelta) 0)) "x" "."))

;; todo:
;; 1st x: changed, only white, ...
;; 2nd x: older, newer, equal time stamp, minimal-newer, minimal-older
(defun mks-conv-wfdelta(wfdelta)
  (if (and wfdelta (> (length wfdelta) 0)) "xx" ".."))

(defun mks-universal(cmd)
  "Calls mks-CMD, CMD given as string.
Is thus only a tiny shortcut around typing M-x mks-..."
  (interactive "scommand name: ")
  (let ((f (intern-soft (concat "mks-" cmd))))
    (if (functionp f)
        (call-interactively f)
      (error "'mks-%s' is not a function" cmd))))

(defun mks-re-search-backward (regexp &optional bound noerror)
  "Does exactly what the elisp info manual tells that
  're-search-backward' is not doing. mks-re-search-backward
  mirrors re-research-forward."
  (let (found
        (saved-point (point)))
    (while (and (or (null bound) (>= (point) bound))
                (not found))
      (setq found (looking-at regexp))
      (unless found (backward-char)))
    (if (and (not found) (not noerror))
        (error "mks-re-search-backward did not find the regexp"))
    (if (not found)
        (goto-char saved-point))
    (point)))

(provide 'mks)

;;; mks.el ends here
