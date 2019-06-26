;;; forge-commands.el --- Commands                 -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Forge is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Forge is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Forge.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'forge)

;;; Options

(defcustom forge-add-pullreq-refspec t
  "Whether the pull-request refspec is added when setting up a repository.

This controls whether running `forge-pull' for the first time in
a repository also adds a refspec that fetches all pull-requests.
In repositories with huge numbers of pull-requests you might want
to not do so, in which case you should set this option to `ask'.

You can also set this to nil and later add the refspec exlicitly
for a repository using the command `forge-add-pullreq-refspec'."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(choice (const :tag "Always add refspec" t)
                 (const :tag "Ask every time" ask)
                 (const :tag "Never add refspec" nil)))

;;; Dispatch

;;;###autoload (autoload 'forge-dispatch "forge-commands" nil t)
(define-transient-command forge-dispatch ()
  "Dispatch a forge command."
  [["Fetch"
    ("f f" "topics"        forge-pull)
    ("f n" "notifications" forge-pull-notifications)]
   ["List"
    ("l i" "issues"        forge-list-issues)
    ("l p" "pull-requests" forge-list-pullreqs)
    ("l n" "notifications" forge-list-notifications)]
   ["Create"
    ("c i" "issue"         forge-create-issue)
    ("c p" "pull-request"  forge-create-pullreq)
    ("c u" "pull-request from issue" forge-create-pullreq-from-issue)]]
  [["Configure"
    ("r" "forge.repository" forge-forge.remote)]])

;;; Pull

;;;###autoload
(defun forge-pull (&optional repo until)
  "Pull topics from the forge repository.

With a prefix argument and if the repository has not been fetched
before, then read a date from the user and limit pulled topics to
those that have been updated since then.

If pulling is too slow, then also consider setting the Git variable
`forge.omitExpensive' to `true'."
  (interactive
   (list nil
         (and current-prefix-arg
              (not (forge-get-repository 'full))
              (forge-read-date "Limit pulling to topics updates since: "))))
  (let (create)
    (unless repo
      (setq repo (forge-get-repository 'full))
      (unless repo
        (setq repo (forge-get-repository 'create))
        (setq create t)))
    (setq forge--mode-line-buffer (current-buffer))
    (when-let ((remote  (oref repo remote))
               (refspec (oref repo pullreq-refspec)))
      (when (and create
                 (not (member refspec (magit-get-all "remote" remote "fetch")))
                 (or (eq forge-add-pullreq-refspec t)
                     (and (eq forge-add-pullreq-refspec 'ask)
                          (y-or-n-p (format "Also add %S refspec? " refspec)))))
        (magit-call-git "config" "--add"
                        (format "remote.%s.fetch" remote)
                        refspec)))
    (forge--msg repo t nil "Pulling REPO")
    (forge--pull repo until)))

(defun forge-read-date (prompt)
  (cl-block nil
    (while t
      (let ((str (read-from-minibuffer prompt)))
        (cond ((string-equal str "")
               (cl-return nil))
              ((string-match-p
                "\\`[0-9]\\{4\\}[-/][0-9]\\{2\\}[-/][0-9]\\{2\\}\\'" str)
               (cl-return str))))
      (message "Please enter a date in the format YYYY-MM-DD.")
      (sit-for 1))))

(cl-defmethod forge--pull ((_repo forge-noapi-repository) _until)) ; NOOP

(cl-defmethod forge--pull ((repo forge-unusedapi-repository) _until)
  (oset repo sparse-p nil)
  (magit-git-fetch (oref repo remote) (magit-fetch-arguments)))

(defun forge--git-fetch (buf dir repo)
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (magit-git-fetch (oref repo remote) (magit-fetch-arguments)))
    (let ((default-directory dir))
      (magit-git-fetch (oref repo remote) (magit-fetch-arguments)))))

;;;###autoload
(defun forge-pull-notifications ()
  "Fetch notifications for all repositories from the current forge."
  (interactive)
  (let* ((repo  (forge-get-repository 'stub))
         (class (eieio-object-class repo)))
    (if (eq class 'forge-github-repository)
        (forge--pull-notifications class (oref repo githost))
      (user-error "Fetching notifications not supported for forge %S"
                  (oref repo forge)))))

;;;###autoload
(defun forge-pull-pullreq (pullreq)
  "Pull a single pull-request from the forge repository.
Normally you wouldn't want to pull a single pull-request by
itself, but due to a bug in the Github API you might sometimes
have to do so.  See https://platform.github.community/t/7284."
  (interactive (list (forge-read-pullreq "Pull pull-request" t)))
  (forge--pull-pullreq (forge-get-repository pullreq) pullreq))

(cl-defmethod forge--pull-pullreq ((_repo forge-repository) _pullreq)) ; NOOP

;;; Browse

;;;###autoload
(defun forge-browse-dwim ()
  "Visit a topic, branch or commit using a browser.
Prefer a topic over a branch and that over a commit."
  (interactive)
  (if-let ((topic (forge-topic-at-point)))
      (forge-browse topic)
    (if-let ((branch (magit-branch-at-point)))
        (forge-browse-branch branch)
      (call-interactively 'forge-browse-commit))))

;;;###autoload
(defun forge-browse-commit (rev)
  "Visit the url corresponding to REV using a browser."
  (interactive
   (list (or (magit-completing-read "Browse commit"
                                    (magit-list-branch-names)
                                    nil nil nil 'magit-revision-history
                                    (magit-branch-or-commit-at-point))
             (user-error "Nothing selected"))))
  (let* ((repo (forge-get-repository 'stub))
         (remote (oref repo remote))
         (branches (magit-list-remote-branch-names remote))
         (available (-any-p (lambda (branch)
                              (magit-rev-ancestor-p rev branch))
                            branches)))
    (unless available
      (if-let ((branch (-some (lambda (branch)
                                (magit-rev-ancestor-p rev branch))
                              (cl-set-difference
                               (magit-list-remote-branch-names remote)
                               branches :test #'equal))))
          (setq repo (forge-get-repository
                      'stub (cdr (magit-split-branch-name branch))))
        (message "%s does not appear to be available on any remote.  %s"
                 rev "You might have to push it first.")))
    (browse-url
     (forge--format repo 'commit-url-format
                    `((?r . ,(magit-commit-p rev)))))))

;;;###autoload
(defun forge-copy-url-at-point-as-kill ()
  "Copy the url of the thing at point."
  (interactive)
  (if-let ((url (forge-get-url (or (forge-post-at-point)
                                   (forge-current-topic)))))
      (progn
        (kill-new url)
        (message "Copied %S" url))
    (user-error "Nothing at point with a URL")))

;;;###autoload
(defun forge-browse-branch (branch)
  "Visit the url corresponding BRANCH using a browser."
  (interactive (list (magit-read-branch "Browse branch")))
  (let (remote)
    (if (magit-remote-branch-p branch)
        (let ((cons (magit-split-branch-name branch)))
          (setq remote (car cons))
          (setq branch (cdr cons)))
      (or (setq remote (or (magit-get-push-remote branch)
                           (magit-get-upstream-remote branch)))
          (user-error "Cannot determine remote for %s" branch)))
    (browse-url (forge--format remote 'branch-url-format
                               `((?r . ,branch))))))

;;;###autoload
(defun forge-browse-remote (remote)
  "Visit the url corresponding to REMOTE using a browser."
  (interactive (list (magit-read-remote "Browse remote")))
  (browse-url (forge--format remote 'remote-url-format)))

;;;###autoload
(defun forge-browse-topic ()
  "Visit the current topic using a browser."
  (interactive)
  (if-let ((topic (forge-current-topic)))
      (forge-browse topic)
    (user-error "There is no topic at point")))

;;;###autoload
(defun forge-browse-pullreqs (repo)
  "Visit the url corresponding to REPO's pull-requests using a browser."
  (interactive (list (forge-get-repository 'stub)))
  (browse-url (forge--format repo 'pullreqs-url-format)))

;;;###autoload
(defun forge-browse-pullreq (pullreq)
  "Visit the url corresponding to PULLREQ using a browser."
  (interactive (list (forge-read-pullreq "Browse pull-request" t)))
  (forge-browse pullreq))

;;;###autoload
(defun forge-browse-issues (repo)
  "Visit the url corresponding to REPO's issues using a browser."
  (interactive (list (forge-get-repository 'stub)))
  (browse-url (forge--format repo 'issues-url-format)))

;;;###autoload
(defun forge-browse-issue (issue)
  "Visit the url corresponding to ISSUE using a browser."
  (interactive (list (forge-read-issue "Browse issue" t)))
  (forge-browse issue))

;;;###autoload
(defun forge-browse-post ()
  "Visit the url corresponding to the post at point using a browser."
  (interactive)
  (if-let ((post (forge-post-at-point)))
      (forge-browse post)
    (user-error "There is no post at point")))

;;; Visit

;;;###autoload
(defun forge-visit-topic ()
  "View the topic at point in a separate buffer."
  (interactive)
  (if-let ((topic (forge-topic-at-point)))
      (forge-visit topic)
    (user-error "There is no topic at point")))

;;;###autoload
(defun forge-visit-pullreq (pullreq)
  "View the pull-request at point in a separate buffer."
  (interactive (list (forge-read-pullreq "View pull-request" t)))
  (forge-visit pullreq))

;;;###autoload
(defun forge-visit-issue (issue)
  "View the issue at point in a separate buffer."
  (interactive (list (forge-read-issue "View issue" t)))
  (forge-visit issue))

;;; Create

(defun forge-create-pullreq (source target)
  "Create a new pull-request for the current repository."
  (interactive (forge-create-pullreq--read-args))
  (let* ((repo (forge-get-repository t))
         (buf (forge--prepare-post-buffer
               "new-pullreq"
               (forge--format repo "Create new pull-request on %p"))))
    (with-current-buffer buf
      (setq forge--buffer-base-branch target)
      (setq forge--buffer-head-branch source)
      (setq forge--buffer-post-object repo)
      (setq forge--submit-post-function 'forge--submit-create-pullreq))
    (forge--display-post-buffer buf)))

(defun forge-create-pullreq-from-issue (issue source target)
  "Convert an existing issue into a pull-request."
  (interactive (cons (oref (forge-read-issue "Convert issue") number)
                     (forge-create-pullreq--read-args)))
  (forge--create-pullreq-from-issue (forge-get-repository t)
                                    issue source target))

(defun forge-create-pullreq--read-args ()
  (let* ((source  (magit-completing-read
                   "Source branch"
                   (magit-list-remote-branch-names)
                   nil t nil 'magit-revision-history
                   (or (when-let ((d (magit-branch-at-point)))
                         (if (magit-remote-branch-p d)
                             d
                           (magit-get-push-branch d t)))
                       (when-let ((d (magit-get-current-branch)))
                         (if (magit-remote-branch-p d)
                             d
                           (magit-get-push-branch d t))))))
         (remote  (oref (forge-get-repository t) remote))
         (targets (delete source (magit-list-remote-branch-names remote)))
         (target  (magit-completing-read
                   "Target branch" targets nil t nil 'magit-revision-history
                   (let* ((d (cdr (magit-split-branch-name source)))
                          (d (and (magit-branch-p d) d))
                          (d (and d (magit-get-upstream-branch d)))
                          (d (and d (if (magit-remote-branch-p d)
                                        d
                                      (magit-get-upstream-branch d))))
                          (d (or d (concat remote "/master"))))
                     (car (member d targets))))))
    (list source target)))

(defun forge-create-issue ()
  "Create a new issue for the current repository."
  (interactive)
  (let* ((repo (forge-get-repository t))
         (buf (forge--prepare-post-buffer
               "new-issue"
               (forge--format repo "Create new issue on %p"))))
    (with-current-buffer buf
      (setq forge--buffer-post-object repo)
      (setq forge--submit-post-function 'forge--submit-create-issue))
    (forge--display-post-buffer buf)))

(defun forge-create-post (&optional quote)
  "Create a new post on an existing topic.
If the region is active, then quote that part of the post.
Otherwise and with a prefix argument quote the post that
point is currently on."
  (interactive (list current-prefix-arg))
  (unless (derived-mode-p 'forge-topic-mode)
    (user-error "This command is only available from topic buffers"))
  (let* ((topic forge-buffer-topic)
         (buf (forge--prepare-post-buffer
               (forge--format topic "%i:new-comment")
               (forge--format topic "New comment on #%i of %p")))
         (quote (cond
                 ((not (magit-section-match 'post)) nil)
                 ((use-region-p)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end)))
                 (quote
                  (let ((section (magit-current-section)))
                    (string-trim-right
                     (buffer-substring-no-properties (oref section content)
                                                     (oref section end))))))))
    (with-current-buffer buf
      (setq forge--buffer-post-object topic)
      (setq forge--submit-post-function 'forge--submit-create-post)
      (when quote
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n"))
        (insert (replace-regexp-in-string "^" "> " quote) "\n\n")))
    (forge--display-post-buffer buf)))

;;; Edit

(defun forge-edit-post ()
  "Edit an existing post."
  (interactive)
  (let* ((post (forge-post-at-point))
         (buf (cl-typecase post
                (forge-topic
                 (forge--prepare-post-buffer
                  (forge--format post "%i")
                  (forge--format post "Edit #%i of %p")))
                (forge-post
                 (forge--prepare-post-buffer
                  (forge--format post "%i:%I")
                  (forge--format post "Edit comment on #%i of %p"))))))
    (with-current-buffer buf
      (setq forge--buffer-post-object post)
      (setq forge--submit-post-function 'forge--submit-edit-post)
      (erase-buffer)
      (when (cl-typep post 'forge-topic)
        (insert "# " (oref post title) "\n\n"))
      (insert (oref post body)))
    (forge--display-post-buffer buf)))

(defun forge-edit-topic-title (topic)
  "Edit the title of TOPIC."
  (interactive (list (forge-read-topic "Edit title of")))
  (forge--set-topic-title
   (forge-get-repository topic) topic
   (read-string "Title: " (oref topic title))))

(defun forge-edit-topic-state (topic)
  "Close or reopen TOPIC."
  (interactive
   (let ((topic (forge-read-topic "Close/reopen")))
     (if (magit-y-or-n-p
          (format "%s %S"
                  (cl-ecase (oref topic state)
                    (merged (error "Merged pull-requests cannot be reopened"))
                    (closed "Reopen")
                    (open   "Close"))
                  (forge--topic-format-choice topic)))
         (list topic)
       (user-error "Abort"))))
  (forge--set-topic-state (forge-get-repository topic) topic))

(defun forge-edit-topic-labels (topic)
  "Edit the labels of TOPIC."
  (interactive (list (forge-read-topic "Edit labels of")))
  (let ((repo (forge-get-repository topic))
        (crm-separator ","))
    (forge--set-topic-labels
     repo topic (magit-completing-read-multiple*
                 "Labels: "
                 (mapcar #'cadr (oref repo labels))
                 nil t
                 (mapconcat #'car (closql--iref topic 'labels) ",")))))

(defun forge-edit-topic-assignees (topic)
  "Edit the assignees of TOPIC."
  (interactive (list (forge-read-topic "Edit assignees of")))
  (let* ((repo (forge-get-repository topic))
         (value (closql--iref topic 'assignees))
         (choices (mapcar #'cadr (oref repo assignees)))
         (crm-separator ","))
    (forge--set-topic-assignees
     repo topic
     (if (and (forge--childp topic 'forge-pullreq)
              (forge--childp repo  'forge-gitlab-repository))
         (list ; Gitlab merge-requests can only be assigned to a single user.
          (magit-completing-read
           "Assignee" choices nil
           nil ; Empty input removes assignee.
           (car value)))
       (magit-completing-read-multiple*
        "Assignees: " choices nil
        (if (forge--childp repo 'forge-gitlab-repository)
            t ; Selecting something else would fail later on.
          'confirm)
        (mapconcat #'car value ","))))))

;;; Delete

(defun forge-delete-comment (comment)
  "Delete the comment at point."
  (interactive (list (or (forge-comment-at-point)
                         (user-error "There is no post at point"))))
  (when (yes-or-no-p "Do you really want to delete the selected comment? ")
    (forge--delete-comment (forge-get-repository t) comment)))

;;; Branch

;;;###autoload
(defun forge-branch-pullreq (pullreq)
  "Create and configure a new branch from a pull-request.
Please see the manual for more information."
  (interactive (list (forge-read-pullreq-or-number "Branch pull request" t)))
  (forge--branch-pullreq (forge-get-repository t) pullreq))

(cl-defmethod forge--branch-pullreq ((_repo forge-unusedapi-repository) number)
  ;; We don't know enough to do a good job.
  (let ((branch (format "pr-%s" number)))
    (when (magit-branch-p branch)
      (user-error "Branch `%s' already exists" branch))
    (magit-git "branch" branch (forge--pullreq-ref number))
    ;; More often than not this is the correct target branch.
    (magit-call-git "branch" branch "--set-upstream-to=master")
    (magit-set (number-to-string number) "branch" branch "pullRequest")
    (magit-refresh)
    branch))

(cl-defmethod forge--branch-pullreq ((repo forge-repository) pullreq)
  (with-slots (number title editable-p cross-repo-p state
                      base-ref base-repo
                      head-ref head-repo head-user)
      pullreq
    (let* ((host (oref repo githost))
           (upstream (oref repo remote))
           (upstream-url (magit-git-string "remote" "get-url" upstream))
           (remote head-user)
           (branch (forge--pullreq-branch pullreq t))
           (pr-branch head-ref))
      (when (string-match-p ":" pr-branch)
        ;; Such a branch name would be invalid.  If we encounter
        ;; it anyway, then that means that the source branch and
        ;; the merge-request ref are missing.
        (error "Cannot check out this Gitlab merge-request \
because the source branch has been deleted"))
      (if (not (eq state 'open))
          (magit-git "branch" "--force" branch
                     (format "refs/pullreqs/%s" number))
        (if (not cross-repo-p)
            (let ((tracking (concat upstream "/" pr-branch)))
              (unless (magit-branch-p tracking)
                (magit-call-git "fetch" upstream))
              (magit-call-git "branch" branch tracking)
              (magit-branch-maybe-adjust-upstream branch tracking)
              (magit-set upstream "branch" branch "pushRemote")
              (magit-set upstream "branch" branch "pullRequestRemote"))
          (if (magit-remote-p remote)
              (let ((url   (magit-git-string "remote" "get-url" remote))
                    (fetch (magit-get-all "remote" remote "fetch")))
                (unless (forge--url-equal
                         url (format "git@%s:%s.git" host head-repo))
                  (user-error
                   "Remote `%s' already exists but does not point to %s"
                   remote url))
                (unless (member (format "+refs/heads/*:refs/remotes/%s/*" remote)
                                fetch)
                  (magit-git "remote" "set-branches" "--add" remote pr-branch)
                  (magit-git "fetch" remote)))
            (magit-git
             "remote" "add" "-f" "--no-tags"
             "-t" pr-branch remote
             (cond ((or (string-prefix-p "git@" upstream-url)
                        (string-prefix-p "ssh://git@" upstream-url))
                    (format "git@%s:%s.git" host head-repo))
                   ((string-prefix-p "https://" upstream-url)
                    (format "https://%s/%s.git" host head-repo))
                   ((string-prefix-p "git://" upstream-url)
                    (format "git://%s/%s.git" host head-repo))
                   (t (error "%s has an unexpected format" upstream-url))))
            (magit-git "branch" "--force" branch (concat remote "/" pr-branch)))
          (if (and editable-p
                   (equal branch pr-branch))
              (magit-set remote "branch" branch "pushRemote")
            (magit-set upstream "branch" branch "pushRemote")))
        (magit-set remote "branch" branch "pullRequestRemote")
        (magit-set "true" "branch" branch "rebase")
        (magit-git "branch" branch
                   (concat "--set-upstream-to="
                           (if magit-branch-prefer-remote-upstream
                               (concat upstream "/" base-ref)
                             base-ref))))
      (magit-set (number-to-string number) "branch" branch "pullRequest")
      (magit-set title                     "branch" branch "description")
      (magit-refresh)
      branch)))

;;;###autoload
(defun forge-checkout-pullreq (pullreq)
  "Create, configure and checkout a new branch from a pull-request.
Please see the manual for more information."
  (interactive (list (forge-read-pullreq-or-number "Checkout pull request" t)))
  (magit-checkout
   (or (if (not (eq (oref pullreq state) 'open))
           (magit-ref-p (format "refs/pullreqs/%s"
                                (oref pullreq number)))
         (magit-branch-p (forge--pullreq-branch pullreq)))
       (let ((inhibit-magit-refresh t))
         (forge-branch-pullreq pullreq)))))

;;;###autoload
(defun forge-checkout-worktree (path pullreq)
  "Create, configure and checkout a new worktree from a pull-request.
This is like `magit-checkout-pull-request', except that it
also creates a new worktree. Please see the manual for more
information."
  (interactive
   (let ((pullreq (forge-read-pullreq-or-number "Checkout pull request" t)))
     (with-slots (number head-ref) pullreq
       (let ((path (let ((branch (forge--pullreq-branch pullreq t)))
                     (read-directory-name
                      (format "Checkout #%s as `%s' in new worktree: "
                              number branch)
                      (file-name-directory
                       (directory-file-name default-directory))
                      nil nil
                      (if (string-match-p "\\`pr-[0-9]+\\'" branch)
                          (number-to-string number)
                        (format "%s-%s" number head-ref))))))
         (when (equal path "")
           (user-error "The empty string isn't a valid path"))
         (list path pullreq)))))
  (when (and (file-exists-p path)
             (not (and (file-directory-p path)
                       (= (length (directory-files "/tmp/testing/")) 2))))
    (user-error "%s already exists and isn't empty" path))
  (magit-worktree-checkout path
                           (let ((inhibit-magit-refresh t))
                             (forge-branch-pullreq pullreq))))

;;; Misc

(define-infix-command forge-forge.remote ()
  :class 'magit--git-variable:choices
  :variable "forge.remote"
  :choices 'magit-list-remotes
  :default "origin")

;;;###autoload
(defun forge-list-notifications ()
  "List notifications."
  (interactive)
  (forge-notifications-setup-buffer))

;;;###autoload
(defun forge-add-pullreq-refspec ()
  "Configure Git to fetch all pull-requests.
This is done by adding \"+refs/pull/*/head:refs/pullreqs/*\"
to the value of `remote.REMOTE.fetch', where REMOTE is the
upstream remote.  Also fetch from REMOTE."
  (interactive)
  (let* ((repo    (forge-get-repository 'stub))
         (remote  (oref repo remote))
         (fetch   (magit-get-all "remote" remote "fetch"))
         (refspec (oref repo pullreq-refspec)))
    (if (member refspec fetch)
        (message "Pull-request refspec is already active")
      (magit-call-git "config" "--add"
                      (format "remote.%s.fetch" remote)
                      refspec)
      (magit-git-fetch remote (magit-fetch-arguments)))))

;;;###autoload
(defun forge-remove-repository (repo)
  "Remove a repository from the database."
  (interactive
   (let ((repo (forge-read-repository "Remove repository from db")))
     (if (yes-or-no-p
          (format "Do you really want to remove \"%s/%s @%s\" from the db? "
                  (oref repo owner)
                  (oref repo name)
                  (oref repo githost)))
         (list repo)
       (user-error "Abort"))))
  (closql-delete repo)
  (magit-refresh))

;;;###autoload
(defun forge-reset-database ()
  "Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development."
  (interactive)
  (when (and (file-exists-p forge-database-file)
             (yes-or-no-p "Really trash Forge's database file? "))
    (when forge--db-connection
      (emacsql-close forge--db-connection))
    (delete-file forge-database-file t)
    (magit-refresh)))

;;; _
(provide 'forge-commands)
;;; forge-commands.el ends here
