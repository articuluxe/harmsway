;;; forge-topics.el --- List topics  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.forge@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.forge@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'forge-topic)

;;; Options

(defcustom forge-list-buffer-default-topic-filters
  (forge--topics-spec :type 'topic :active t :state 'open :order 'newest)
  "Filters initially used to limit topics listed in list buffers.

This option controls which topics are listed when initially creating
a `forge-topics-mode' buffer.  To temporarily change which topics are
listed in a given buffer, instead use \\`N m' (`forge-topics-menu')."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type 'object)

(defcustom forge-status-buffer-default-topic-filters
  (forge--topics-spec :type 'topic :active t :state 'open :order 'newest)
  "Filters initially used to limit topics listed in status buffers.

This option controls which topics are listed when initially creating
a `magit-status-mode' buffer.  To temporarily change which topics are
listed in a given buffer, instead use \\<forge-topics-mode-map> \
\\[forge-topics-menu] (`forge-topics-menu').

To initially list no topics, set the `type' slot to nil."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type 'object)

(defcustom forge-owned-accounts nil
  "An alist of accounts that are owned by you.
This should include your username as well as any organization
that you own.  Used by the commands `forge-list-owned-issues',
`forge-list-owned-pullreqs' and `forge-fork'.

Each element has the form (ACCOUNT . PLIST).  The following
properties are currently being used:

`remote-name' The default name suggested by `forge-fork' for a
  fork created within this account.  If unspecified, then the
  name of the account is used."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(repeat (cons (string :tag "Account") plist)))

(defcustom forge-owned-ignored nil
  "A list of repositories that are ignored when listing those owned by you.
This is a list of package names.  Used by the commands
`forge-list-owned-issues' and `forge-list-owned-pullreqs'."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(repeat (string :tag "Name")))

;;; Faces

(defface forge-suffix-active
  '((t :inherit transient-value :weight bold))
  "Face used for suffixes whose effects is currently active."
  :group 'forge-faces)

(defface forge-suffix-active-and-implied
  '((t :inherit transient-value :weight semibold))
  "Face used for suffixes whose effects is currently active and implied."
  :group 'forge-faces)

(defface forge-suffix-implied
  '((t :inherit transient-value :weight normal))
  "Face used for suffixes whose effects is currently implied."
  :group 'forge-faces)

;;; Mode

(defvar-keymap forge-topics-mode-map
  :doc "Local keymap for Forge-Topic-List mode buffers."
  :parent (make-composed-keymap forge-common-map magit-mode-map)
  "RET"                        #'forge-visit-this-topic
  "<return>"                   #'forge-visit-this-topic
  "o"                          #'forge-browse-this-topic
  "<remap> <forge--list-menu>" #'forge-topics-menu
  "<remap> <forge--item-menu>" #'forge-topic-menu
  "<remap> <magit-refresh>"    #'forge-refresh-buffer)

(defvar forge-topics-mode-name '((:eval (forge-topics-buffer-desc)))
  "Information shown in the mode-line for `forge-topics-mode'.
Must be set before `forge-topics' is loaded.")

(define-derived-mode forge-topics-mode magit-mode forge-topics-mode-name
  "Major mode for browsing a list of topics."
  :interactive nil
  (magit-hack-dir-local-variables))

(defun forge-topics-setup-buffer (&optional repo spec &rest params)
  "List a set of topics in a buffer.

The buffer is determined using `forge-topics-buffer-name', which uses
the same buffer for all global lists, and likewise just one buffer per
repository for repository-local lists.  You could use `cl-letf' to use
a different buffer for certain sets.

If optional REPO is non-nil, it must be a `forge-repository' object.
It is only relevant when not showing a global topic list, as determined
by the value of `:global' in PARAMS.  Even when showing a local list,
REPO may be nil, in that case the repository is determined from context.

If optional SPEC is non-nil, it must be a `forge--topics-spec' object.
If nil, a clone of the existing filter spec from the buffer determined
above is used, provided that buffer already exists and has a local
filter spec.  A clone of `forge-list-buffer-default-topic-filters' is
used otherwise.

Optional PARAMS can be used to set slots of SPEC.  PARAMS is a plist
where each key is an initarg for a slot of the `forge--topics-spec'
class.

Usually you would use nil for SPEC, so that a clone of the currently
effective filter spec is used, and then you would set only some of
the available filters using PARAMS.

  (transient-define-suffix my-forge-list-assigned-issues ()
    \"List issues of the current repository that are assigned to me.\"
    :description \"issues\"
    (declare (interactive-only nil))
    (interactive)
    (when-let* ((repo (forge-get-repository :tracked))
                (me (ghub--username repo)))
      (forge-topics-setup-buffer repo nil :type \\='issue :assignee me)
      (transient-setup \\='forge-topics-menu)))

Grep Forge for more examples.

Alternatively you can use `forge-insert-topics' list topics in, e.g.,
the Magit status buffer."
  (let* ((global (or (plist-get params :global)
                     (and spec (oref spec global))))
         (repo (or repo
                   (and (not global)
                        (if-let* ((topic (forge-topic-at-point))
                                  (repo (forge-get-repository topic)))
                            repo
                          (forge-get-repository :tracked?)))))
         (dir (or (and repo (forge-get-worktree repo)) "/"))
         (buf (forge-topics-buffer-name repo))
         (buf (or (get-buffer buf) buf))
         (spec (cond (spec (clone spec))
                     ((and (bufferp buf)
                           (buffer-local-value 'forge--buffer-topics-spec buf)))
                     ((clone forge-list-buffer-default-topic-filters)))))
    (while-let ((key (pop params)))
      (eieio-oset spec (intern (substring (symbol-name key) 1)) (pop params)))
    (unless (oref spec type)
      (oset spec type 'topic))
    (forge--cast-topics-spec-state spec)
    (unless (or repo global)
      (error "Cannot determine repository"))
    (magit-setup-buffer #'forge-topics-mode nil
      :buffer    (get-buffer-create buf)
      :directory dir
      (forge-buffer-repository     (and repo (oref repo id)))
      (forge--buffer-topics-spec   spec)
      (forge-buffer-unassociated-p global))))

(defun forge-topics-refresh-buffer ()
  (magit-set-header-line-format (forge-topics-buffer-desc))
  (let ((topics (forge--list-topics
                 forge--buffer-topics-spec
                 (forge-get-repository :tracked?))))
    (magit-insert-section (topicbuf)
      (cond
       ((not topics)
        (insert "No matching topics\n"))
       ((not (oref forge--buffer-topics-spec grouped))
        (dolist (topic topics)
          (forge--insert-topic topic 5)))
       ((pcase-dolist (`(,_ . ,topics)
                       (seq-group-by (##oref % repository) topics))
          (let ((repo (forge-get-repository (car topics))))
            (magit-insert-section (forge-repo repo)
              (magit-insert-heading
                (concat (propertize (oref repo slug)
                                    'font-lock-face 'bold)
                        (format " (%s)" (length topics))))
              (dolist (topic topics)
                (forge--insert-topic topic 5))))))))))

(defun forge-topics-buffer-desc ()
  (capitalize (concat (symbol-name (oref forge--buffer-topics-spec type)) "s")))

(defun forge-topics-buffer-name (&optional repo)
  (if repo
      (format "*forge-topics: %s*" (oref repo slug))
    "*forge-topics*"))

;;; Commands
;;;; Menu

;;;###autoload(autoload 'forge-topics-menu "forge-topics" nil t)
(transient-define-prefix forge-topics-menu ()
  "Control list of topics displayed in the current buffer."
  :transient-suffix t
  :transient-non-suffix #'transient--do-call
  :transient-switch-frame nil
  :refresh-suffixes t
  :environment #'forge--menu-environment
  :column-widths forge--topic-menus-column-widths
  [:hide always ("q" forge-menu-quit-list)]
  [forge--topic-menus-group
   ["State"
    ("a" forge-topics-filter-active)
    ("o" forge-topics-filter-state-open)
    ("r" forge-topics-filter-state-realized)
    ("e" forge-topics-filter-state-expunged)
    ("U" forge-topics-filter-state-unplanned)
    ("O" forge-topics-filter-state-outdated)
    ("D" forge-topics-filter-state-duplicate)]
   ["Status"
    ("i" forge-topics-filter-status-inbox)
    ("u" forge-topics-filter-status-unread)
    ("p" forge-topics-filter-status-pending)
    ("d" forge-topics-filter-status-done)]
   ["Type"
    ("t t" forge-topics-all-types)
    ("t d" forge-topics-filter-discussions)
    ("t i" forge-topics-filter-issues)
    ("t p" forge-topics-filter-pullreqs)]]
  [forge--lists-group
   ["Filter                                      "
    ("-c" forge-topics-filter-category)
    ("-m" forge-topics-filter-milestone)
    ("-l" forge-topics-filter-labels)
    ("-x" forge-topics-filter-marks)
    ("-A" forge-topics-filter-author)
    ("-a" forge-topics-filter-assignee)
    ("-r" forge-topics-filter-reviewer)
    ("-s" forge-topics-filter-saved)]
   ["Display"
    ("-O" forge-topics-set-order)
    ("-L" forge-topics-set-limit)
    ("-F" forge-topics-ungroup)
    ("-G" forge-topics-group)
    ("-S" forge-toggle-display-in-status-buffer)
    ("-H" forge-toggle-topic-legend)]]
  [forge--topic-legend-group]
  (interactive)
  (cond ((derived-mode-p 'forge-topics-mode 'magit-status-mode)
         (transient-setup 'forge-topics-menu))
        ((derived-mode-p 'forge-notifications-mode)
         (setq this-command 'forge-notifications-menu)
         (transient-setup 'forge-notifications-menu))
        ((forge-list-topics))))

(transient-augment-suffix forge-topics-menu
  :transient #'transient--do-replace
  :if-not-derived '(forge-notifications-mode forge-repository-list-mode)
  :inapt-if (##eq (oref transient--prefix command) 'forge-topics-menu)
  :inapt-face 'forge-suffix-active)

(defvar-local forge--quit-keep-topic-menu nil)

(defun forge-menu-quit-list ()
  "From a transient menu, quit the list buffer and the menu.

If quitting the list buffer causes another topic, repository list or
notification list buffer to become current in the selected window,
then display the respective menu, otherwise display no menu."
  (interactive)
  (let ((keep-topic-menu forge--quit-keep-topic-menu))
    (when (derived-mode-p 'forge-topic-mode
                          'forge-topics-mode
                          'forge-repository-list-mode
                          'forge-notifications-mode)
      (kill-local-variable 'forge--quit-keep-topic-menu)
      (quit-window))
    (cond ((derived-mode-p 'forge-topic-mode)
           (setq transient--exitp 'replace)
           (transient-setup (setq this-command 'forge-topic-menu)))
          ((derived-mode-p 'forge-topics-mode)
           (unless keep-topic-menu
             (setq transient--exitp 'replace)
             (transient-setup (setq this-command 'forge-topics-menu))))
          ((derived-mode-p 'forge-repository-list-mode)
           (setq transient--exitp 'replace)
           (transient-setup (setq this-command 'forge-repositories-menu)))
          ((derived-mode-p 'forge-notifications-mode)
           (setq transient--exitp 'replace)
           (transient-setup (setq this-command 'forge-notifications-menu)))
          (t
           (setq transient--exitp t)
           (transient--pre-exit)
           (transient--stack-zap)))))

;;;; List

;;;###autoload(autoload 'forge-list-topics "forge-topics" nil t)
(transient-define-suffix forge-list-topics (&optional repo)
  "List topics of the current repository."
  :description "topics"
  :inapt-if (lambda () (or (not (forge-get-repository :tracked?))
                      (and (eq major-mode 'forge-topics-mode)
                           (not (oref forge--buffer-topics-spec global)))))
  :inapt-face (lambda () (if (not (forge-get-repository :tracked?))
                        'transient-inapt-suffix
                      'forge-suffix-active))
  (declare (interactive-only nil))
  (interactive)
  (forge-topics-setup-buffer repo)
  (transient-setup 'forge-topics-menu))

;;;###autoload(autoload 'forge-list-discussions "forge-topics" nil t)
(transient-define-suffix forge-list-discussions (&optional repo)
  "List discussions of the current repository."
  :description "discussions"
  (declare (interactive-only nil))
  (interactive)
  (forge-topics-setup-buffer repo nil :type 'discussion)
  (transient-setup 'forge-topics-menu))

;;;###autoload(autoload 'forge-list-issues "forge-topics" nil t)
(transient-define-suffix forge-list-issues (&optional repo)
  "List issues of the current repository."
  :description "issues"
  (declare (interactive-only nil))
  (interactive)
  (forge-topics-setup-buffer repo nil :type 'issue)
  (transient-setup 'forge-topics-menu))

;;;###autoload(autoload 'forge-list-pullreqs "forge-topics" nil t)
(transient-define-suffix forge-list-pullreqs (&optional repo)
  "List pull-requests of the current repository."
  :description "pull-requests"
  (declare (interactive-only nil))
  (interactive)
  (forge-topics-setup-buffer repo nil :type 'pullreq)
  (transient-setup 'forge-topics-menu))

;;;###autoload(autoload 'forge-list-global-topics "forge-topics" nil t)
(transient-define-suffix forge-list-global-topics (&optional repo)
  "List topics across all tracked repository."
  :description "topics"
  :inapt-if (lambda () (and (eq major-mode 'forge-topics-mode)
                       (oref forge--buffer-topics-spec global)))
  :inapt-face 'forge-suffix-active
  (declare (interactive-only nil))
  (interactive)
  (forge-topics-setup-buffer repo nil :global t)
  (transient-setup 'forge-topics-menu))

;;;###autoload(autoload 'forge-list-global-issues "forge-topics" nil t)
(transient-define-suffix forge-list-global-issues (&optional repo)
  "List issues across all tracked repository."
  :description "issues"
  (declare (interactive-only nil))
  (interactive)
  (forge-topics-setup-buffer repo nil :global t :type 'issue)
  (transient-setup 'forge-topics-menu))

;;;###autoload(autoload 'forge-list-global-pullreqs "forge-topics" nil t)
(transient-define-suffix forge-list-global-pullreqs (&optional repo)
  "List pull-requests across all tracked repository."
  :description "pull-requests"
  (declare (interactive-only nil))
  (interactive)
  (forge-topics-setup-buffer repo nil :global t :type 'pullreq)
  (transient-setup 'forge-topics-menu))

;;;; Type

(defclass forge--topics-filter-type-command (transient-suffix)
  ((type :initarg :type)
   (definition
    :initform (lambda (&optional repo)
                (interactive)
                (oset forge--buffer-topics-spec type
                      (oref (transient-suffix-object) type))
                (forge--cast-topics-spec-state forge--buffer-topics-spec)
                (forge-refresh-buffer)))
   (inapt-face :initform 'forge-suffix-active)
   (inapt-if
    :initform (lambda ()
                (eq (oref forge--buffer-topics-spec type)
                    (oref (transient-suffix-object) type))))))

(transient-define-suffix forge-topics-all-types ()
  :class 'forge--topics-filter-type-command :type 'topic
  :description "topics")

(transient-define-suffix forge-topics-filter-discussions ()
  "List discussions of the current repository."
  :class 'forge--topics-filter-type-command :type 'discussion
  :description "discussions")

(transient-define-suffix forge-topics-filter-issues ()
  "List issues of the current repository."
  :class 'forge--topics-filter-type-command :type 'issue
  :description "issues")

(transient-define-suffix forge-topics-filter-pullreqs ()
  "List pull-requests of the current repository."
  :class 'forge--topics-filter-type-command :type 'pullreq
  :description "pull-requests")

;;;; Active

(transient-define-suffix forge-topics-filter-active ()
  "Limit topic list to active topics."
  :description "active"
  :face (##and (oref forge--buffer-topics-spec active) 'forge-suffix-active)
  (interactive)
  (oset forge--buffer-topics-spec active
        (not (oref forge--buffer-topics-spec active)))
  (forge-refresh-buffer))

;;;; State

(defclass forge--topics-filter-state-command (transient-suffix)
  ((state :initarg :state)
   (definition
    :initform (lambda ()
                (interactive)
                (let ((want (oref (transient-suffix-object) state))
                      (spec forge--buffer-topics-spec))
                  (cond ((and (eq want 'open)
                              (oref spec active))
                         (oset spec active nil)
                         (oset spec state want))
                        ((equal (oref spec state) want)
                         (oset spec state nil))
                        (t
                         (oset spec active nil)
                         (oset spec state want))))
                (forge-refresh-buffer)))
   (description
    :initform (lambda (suffix)
                (symbol-name (oref suffix state))))
   (face
    :initform (lambda (suffix)
                (let ((want   (oref suffix state))
                      (have   (oref forge--buffer-topics-spec state))
                      (active (oref forge--buffer-topics-spec active)))
                  (cond ((and (not active)
                              (equal have want))
                         'forge-suffix-active)
                        ((and (or active
                                  (eq have 'open))
                              (eq want 'open))
                         (if (eq have want)
                             'forge-suffix-active-and-implied
                           'forge-suffix-implied))
                        ((and (memq want '(unplanned duplicate outdated))
                              (equal have
                                     '(unplanned duplicate outdated rejected))
                              (not active))
                         'forge-suffix-implied)))))))

(transient-define-suffix forge-topics-filter-state-open ()
  "Limit topic list to open topics."
  :class 'forge--topics-filter-state-command
  :state 'open)

(transient-define-suffix forge-topics-filter-state-realized ()
  "Limit topic list to realized topics.
Realized topics include:
- completed discussions,
- completed issues, and
- merged pull-requests."
  :class 'forge--topics-filter-state-command
  :state '(completed merged)
  :description (lambda ()
                 (pcase (oref forge--buffer-topics-spec type)
                   ('discussion "completed")
                   ('issue      "completed")
                   ('pullreq    "merged")
                   ('topic      "realized"))))

(transient-define-suffix forge-topics-filter-state-expunged ()
  "Limit topic list to expunged topics.
Expunged topics include:
- discussions closed as outdated,
- discussions closed as duplicates,
- issues closed as unplanned,
- issues closed as duplicates, and
- pull-requests closed without merging."
  :class 'forge--topics-filter-state-command
  :state '(unplanned duplicate outdated rejected)
  :description (lambda ()
                 (pcase (oref forge--buffer-topics-spec type)
                   ('discussion "expunged")
                   ('issue      "expunged")
                   ('pullreq    "rejected")
                   ('topic      "expunged"))))

(transient-define-suffix forge-topics-filter-state-unplanned ()
  "Limit topic list to issues closed as unplanned."
  :class 'forge--topics-filter-state-command
  :state 'unplanned
  :description "  unplanned"
  :if (##eq (oref forge--buffer-topics-spec type) 'issue))

(transient-define-suffix forge-topics-filter-state-outdated ()
  "Limit topic list to discussions closed as outdated."
  :class 'forge--topics-filter-state-command
  :state 'outdated
  :description "  outdated"
  :if (##eq (oref forge--buffer-topics-spec type) 'discussion))

(transient-define-suffix forge-topics-filter-state-duplicate ()
  "Limit topic list to discussions and issues closed as duplicates."
  :class 'forge--topics-filter-state-command
  :state 'duplicate
  :description "  duplicate"
  :if (##memq (oref forge--buffer-topics-spec type) '(discussion issue)))

;;;; Status

(defclass forge--topics-filter-status-command (transient-suffix)
  ((status :initarg :status)
   (definition
    :initform (lambda ()
                (interactive)
                (let* ((want   (oref (transient-suffix-object) status))
                       (spec   forge--buffer-topics-spec)
                       (have   (oref spec status))
                       (active (oref spec active)))
                  (cond (active
                         (oset spec active nil)
                         (oset spec status want))
                        ((eq have want)
                         (oset spec status nil))
                        ((oset spec status want))))
                (forge-refresh-buffer)))
   (description
    :initform (lambda (suffix) (symbol-name (oref suffix status))))
   (face
    :initform (lambda (suffix)
                (let ((want   (oref suffix status))
                      (have   (oref forge--buffer-topics-spec status))
                      (active (oref forge--buffer-topics-spec active)))
                  (cond ((and (not active)
                              (equal have want))
                         'forge-suffix-active)
                        ((and (or active
                                  (eq have 'inbox))
                              (memq want '(inbox unread pending)))
                         (if (eq have want)
                             'forge-suffix-active-and-implied
                           'forge-suffix-implied))))))))

(transient-define-suffix forge-topics-filter-status-inbox ()
  "Limit topic list to unread and pending topics."
  :class 'forge--topics-filter-status-command :status 'inbox)

(transient-define-suffix forge-topics-filter-status-unread ()
  "Limit topic list to unread topics."
  :class 'forge--topics-filter-status-command :status 'unread)

(transient-define-suffix forge-topics-filter-status-pending ()
  "Limit topic list to pending topics."
  :class 'forge--topics-filter-status-command :status 'pending)

(transient-define-suffix forge-topics-filter-status-done ()
  "Limit topic list to done topics."
  :class 'forge--topics-filter-status-command :status 'done)

;;;; Filter

(defclass forge--topics-filter-command (transient-suffix)
  ((slot        :initarg :slot)
   (reader      :initarg :reader)
   (formatter   :initarg :formatter :initform nil)
   (definition
    :initform (lambda ()
                (interactive)
                (with-slots (slot reader) (transient-suffix-object)
                  (eieio-oset forge--buffer-topics-spec slot
                              (if (eieio-oref forge--buffer-topics-spec slot)
                                  nil
                                (funcall reader)))
                  (forge-refresh-buffer))))
   (description
    :initform (lambda (obj)
                (with-slots (slot formatter) obj
                  (let ((value (eieio-oref forge--buffer-topics-spec slot)))
                    (if value
                        (format "%s %s" slot
                                (if formatter
                                    (funcall formatter value)
                                  (propertize (format "%s" value)
                                              'face 'forge-suffix-active)))
                      (format "%s" slot))))))))

(cl-defmethod initialize-instance :after
  ((obj forge--topics-filter-command) &optional _slots)
  (unless (slot-boundp obj 'reader)
    (oset obj reader (intern (format "forge-read-topic-%s" (oref obj slot))))))

(transient-define-suffix forge-topics-filter-category ()
  "Read a category and limit discussions to that category."
  :class 'forge--topics-filter-command
  :slot 'category
  :formatter (##propertize % 'face 'forge-topic-label))

(transient-define-suffix forge-topics-filter-milestone ()
  "Read a milestone and limit topic list to topics with that milestone."
  :class 'forge--topics-filter-command
  :slot 'milestone
  :formatter (##propertize % 'face 'forge-topic-label))

(transient-define-suffix forge-topics-filter-labels ()
  "Read labels and limit topic list to topics with one of these labels."
  :class 'forge--topics-filter-command
  :slot 'labels
  :formatter (##and % (forge--format-labels % " ")))

(transient-define-suffix forge-topics-filter-marks ()
  "Read marks and limit topic list to topics with one of these marks."
  :class 'forge--topics-filter-command
  :slot 'marks
  :formatter (##and % (forge--format-marks % " ")))

(transient-define-suffix forge-topics-filter-saved ()
  "Toggle whether to limit topic list to saved topics."
  :class 'forge--topics-filter-command
  :slot 'saved
  :reader #'always
  :description
  (##forge--format-boolean 'saved "saved" forge--buffer-topics-spec))

(transient-define-suffix forge-topics-filter-author ()
  "Read an author and limit topic list to topics created by that author."
  :class 'forge--topics-filter-command
  :slot 'author
  :reader (##forge--read-filter-by-user "Author"))

(transient-define-suffix forge-topics-filter-assignee ()
  "Read an assignee and limit topic list to topics assignee to that person."
  :class 'forge--topics-filter-command
  :slot 'assignee
  :reader (##forge--read-filter-by-user "Assignee"))

(transient-define-suffix forge-topics-filter-reviewer ()
  "Read a reviewer and limit topic list to reviews requested from that person."
  :class 'forge--topics-filter-command
  :slot 'reviewer
  :reader (##forge--read-filter-by-user "Reviewer"))

(defun forge--read-filter-by-user (prompt)
  (let* ((repo (forge-get-repository :tracked))
         (choices (mapcar #'cadr (oref repo assignees))))
    (magit-completing-read prompt choices)))

;;;; Display

(transient-define-suffix forge-topics-set-order (order)
  "Select order used to display topics in topic list."
  :description
  (lambda ()
    (format "order by %s"
            (propertize (format "%s" (oref forge--buffer-topics-spec order))
                        'face 'bold)))
  (interactive
   (list (magit-read-char-case "Order by: " t
           (?n "[n]ewest"            'newest)
           (?o "[o]ldest"            'oldest)
           (?r "[r]ecently updated"  'recently-updated)
           (?a "[a]nciently updated" 'anciently-updated))))
  (oset forge--buffer-topics-spec order order)
  (forge-refresh-buffer))

(transient-define-suffix forge-topics-set-limit (limit)
  "Read maximal number of topics to be displayed in topic list."
  :description
  (lambda ()
    (if-let ((limit (oref forge--buffer-topics-spec limit)))
        (format "limit to %s" (propertize (format "%s" limit) 'face 'bold))
      "no limit"))
  (interactive (list (read-number "Limit number (0 for no limit): ")))
  (oset forge--buffer-topics-spec limit (if (zerop limit) nil limit))
  (forge-refresh-buffer))

(transient-define-suffix forge-topics-group ()
  "Group topics by repository."
  :description "group by repo"
  :if (##oref forge--buffer-topics-spec global)
  :inapt-if (##oref forge--buffer-topics-spec grouped)
  :inapt-face 'forge-suffix-active
  (interactive)
  (oset forge--buffer-topics-spec grouped t)
  (forge-refresh-buffer))

(transient-define-suffix forge-topics-ungroup ()
  "Show a flat topic list."
  :description "single list"
  :if (##oref forge--buffer-topics-spec global)
  :inapt-if-not (##oref forge--buffer-topics-spec grouped)
  :inapt-face 'forge-suffix-active
  (interactive)
  (oset forge--buffer-topics-spec grouped nil)
  (forge-refresh-buffer))

;;; _
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("partial" . "llama--left-apply-partially")
;;   ("rpartial" . "llama--right-apply-partially"))
;; End:
(provide 'forge-topics)
;;; forge-topics.el ends here
