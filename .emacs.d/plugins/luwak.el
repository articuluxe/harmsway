;;; luwak.el --- Web browser based on lynx -dump. -*- lexical-binding: t; -*-

;; Author: Yuchen Pei <id@ypei.org>
;; Maintainer: Yuchen Pei <id@ypei.org>
;; Created: 2022
;; Version: 1.0.0
;; Keywords: web-browser, lynx, html, tor
;; Package-Requires: ((emacs "28"))
;; Package-Type: multi
;; Homepage: https://g.ypei.me/luwak.git

;; Copyright (C) 2022  Free Software Foundation, Inc.
;; 
;; This file is part of luwak.
;; 
;; luwak is free software: you can redistribute it and/or modify it under
;; the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; luwak is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General
;; Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with luwak.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar luwak-buffer "*luwak*")

(defvar-local luwak-data '(:url nil :dump nil :history-pos nil :no-tor nil))
(defvar-local luwak-history nil)
(defvar luwak-history-file "~/.emacs.d/luwak-history")

(defun luwak-lynx-buffer (url) (format "*luwak-lynx %s*" url))

(defgroup luwak ()
  "Web browser based on lynx -dump."
  :group 'web)

(defcustom luwak-search-engine "https://html.duckduckgo.com/html?q=%s"
  "Default search engine for use in 'luwak-search'."
  :type '(string))
(defcustom luwak-url-rewrite-function 'identity
  "Function to rewrite url before loading."
  :type '(function))
(defcustom luwak-tor-switch t
  "Switch behaviour of prefix arg concerning the use of tor.

When nil, use tor by default (requires a tor daemon having been
started in the system), and not use it with a prefix arg.  When
non-nill, swap the tor-switch in prefix-arg effect."
  :type '(boolean))
(defcustom luwak-max-history-length 100
  "Maximum history length."
  :type '(natnum))
(defcustom luwak-render-link-function 'luwak-render-link-id
  "Function to render a link."
  :type '(choice (const luwak-render-link-id)
          (const luwak-render-link-forward-sexp)
          (const luwak-render-link-hide-link)))
(defcustom luwak-keep-history t
  "If non-nil, will keep history in 'luwak-history-file'."
  :type '(boolean))
(defcustom luwak-use-history t
  "If non-nil, will use history from the 'luwak-history-file' when invoking
'luwak-open'."
  :type '(boolean))

(put luwak-history 'history-length luwak-max-history-length)

(defun luwak-toggle-tor-switch ()
  (interactive)
  (setq luwak-tor-switch (not luwak-tor-switch)))

(defun luwak-mode-name ()
  (concat "luwak "
          (cond
           ((null luwak-data) "Tor unknown")
           ((plist-get luwak-data :no-tor) "Tor off")
           (t "Tor on"))))

(defvar luwak-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "\t" #'forward-button)
    (define-key kmap [backtab] #'backward-button)
    (define-key kmap "g" #'luwak-reload)
    (define-key kmap "l" #'luwak-history-backward)
    (define-key kmap "r" #'luwak-history-forward)
    (define-key kmap "w" #'luwak-copy-url)
    (define-key kmap "o" #'luwak-open)
    (define-key kmap "s" #'luwak-search)
    (define-key kmap "d" #'luwak-save-dump)
    (define-key kmap "j" #'imenu)
    (define-key kmap "t" #'luwak-toggle-links)
    (define-key kmap "a" #'luwak-follow-numbered-link)
    kmap))

(define-derived-mode luwak-mode special-mode (luwak-mode-name)
  "Major mode for browsing the web using lynx -dump."
  (setq-local imenu-create-index-function #'luwak-imenu-create-index
              imenu-space-replacement " "
              imenu-max-item-length nil
              imenu-auto-rescan t))

(defun luwak-imenu-create-index ()
  (goto-char (point-min))
  (let ((index) (position))
    (while (re-search-forward "^[^[:space:]]" nil t)
      (push (cons (buffer-substring-no-properties
                   (setq position (1- (point)))
                   (progn (end-of-line 1) (point)))
                  position)
            index))
    (reverse index)))

(defun luwak-guess-title ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[^[:space:]]" nil t)
      (buffer-substring-no-properties (1- (point))
                                      (progn (end-of-line 1) (point))))))

;;;###autoload
(defun luwak-open (url)
  "Open URL in luwak."
  (interactive
   (list
    (if luwak-use-history
        (car
         (split-string
          (completing-read "Url to open: "
                           (luwak-history-collection-from-file))))
      (read-string "Url to open: "))))
  (luwak-open-url
   (url-encode-url url)
   (xor luwak-tor-switch current-prefix-arg) #'luwak-add-to-history))

(defun luwak-history-collection-from-file ()
  (split-string
   (with-temp-buffer
     (insert-file-contents luwak-history-file)
     (buffer-string))
   "\n" t))

(defun luwak-copy-url ()
  (interactive)
  (when-let ((url (or (get-text-property (point) 'url)
                      (plist-get luwak-data :url))))
    (kill-new url)
    (message "Copied: %s" url)))

;;;###autoload
(defun luwak-search (query)
  "Search QUERY using `luwak-search-engine'."
  (interactive "sLuwak search query: ")
  (luwak-open (format luwak-search-engine query)))

(defun luwak-open-url (url no-tor &optional cb)
  (setq url (funcall luwak-url-rewrite-function url))
  (message "Loading %s..." url)
  (set-process-sentinel
   (luwak-start-process-with-torsocks
    no-tor
    "luwak-lynx" (luwak-lynx-buffer url)
    "lynx" "-dump" "--display_charset" "utf-8" url)
   (lambda (process _)
     (message "Loading %s... Done." url)
     (with-current-buffer (get-buffer-create luwak-buffer)
       (luwak-open-internal
        url
        (with-current-buffer (process-buffer process) (buffer-string))
        (or (plist-get luwak-data :history-pos) 0)
        no-tor)
       (kill-buffer (process-buffer process))
       (when cb (funcall cb))
       (goto-char (point-min)))
     (display-buffer luwak-buffer))))

(defun luwak-open-internal (url dump history-pos no-tor)
  (with-current-buffer (get-buffer-create luwak-buffer)
    (unless (derived-mode-p 'luwak-mode) (luwak-mode))
    (setq luwak-data (list :url url :no-tor no-tor
                           :history-pos history-pos :dump dump))
    (luwak-insert-and-render)
    (setq mode-name (luwak-mode-name))
    (goto-char (point-min))))

(defun luwak-toggle-links ()
  (interactive)
  (pcase luwak-render-link-function
    ('luwak-render-link-id
     (setq luwak-render-link-function 'luwak-render-link-hide-link))
    ('luwak-render-link-hide-link
     (setq luwak-render-link-function 'luwak-render-link-forward-sexp))
    (_
     (setq luwak-render-link-function 'luwak-render-link-id)))
  (save-excursion (luwak-insert-and-render)))

(defun luwak-insert-and-render ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (plist-get luwak-data :dump))
    (luwak-render-links (luwak-get-links))))

(defun luwak-add-to-history ()
  (let ((history-delete-duplicates nil))
    (setq luwak-history (nthcdr (plist-get luwak-data :history-pos)
                                luwak-history))
    (add-to-history 'luwak-history
                    (cons (plist-get luwak-data :url)
                          (plist-get luwak-data :dump)))
    (when (and (plist-get luwak-data :url)
               luwak-keep-history)
      (luwak-add-to-history-file))
    (plist-put luwak-data :history-pos 0)))

(defun luwak-add-to-history-file ()
  (let ((url (plist-get luwak-data :url))
        (title (luwak-guess-title))
        (inhibit-message t))
    (append-to-file (concat url " " title "\n") nil luwak-history-file)))

(defun luwak-history-backward ()
  (interactive)
  (let ((history-pos
         (1+ (plist-get luwak-data :history-pos))))
    (when (<= (length luwak-history) history-pos)
      (error "Already at the earliest history."))
    (luwak-history-open history-pos)))

(defun luwak-history-forward ()
  (interactive)
  (let ((history-pos
         (1- (plist-get luwak-data :history-pos))))
    (when (< history-pos 0)
      (error "Already at the latest history."))
    (luwak-history-open history-pos)))

(defun luwak-history-open (history-pos)
  (let ((pair (nth history-pos luwak-history))
        (len (length luwak-history)))
      (luwak-open-internal (car pair) (cdr pair) history-pos
                           (plist-get luwak-data :no-tor))
      (message "Loaded history %d/%d: %s"
               (- len history-pos) len (car pair))))

(defun luwak-reload ()
  (interactive)
  (let ((url (plist-get luwak-data :url)))
    (unless url
        (error "The current buffer is not associated with any url."))
    (luwak-open-url url (plist-get luwak-data :no-tor))))

(defun luwak-follow-link (marker)
  (let ((url (get-text-property marker 'url)))
    (luwak-open-url
     url (plist-get luwak-data :no-tor) #'luwak-add-to-history)))

(defun luwak-render-links (urls)
  (with-current-buffer luwak-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((i 1))
        (dolist (url urls)
          (funcall luwak-render-link-function i url)
          (setq i (1+ i)))))))

;;;###autoload
(defun luwak-render-buffer ()
  "Render the current buffer in luwak mode."
  (interactive)
  (let ((dump (buffer-string)))
    (with-current-buffer (get-buffer-create luwak-buffer)
      (luwak-open-internal
       nil
       dump
       (or (plist-get luwak-data :history-pos) 0)
       (or (plist-get luwak-data :no-tor)
           (xor luwak-tor-switch current-prefix-arg)))
      (luwak-add-to-history))
    (display-buffer luwak-buffer)))

(defun luwak-render-link-forward-sexp (idx url)
  "Render a link using forward-sexp."
  (when (re-search-forward (format "\\[%d\\]" idx) nil t)
    (replace-match "")
    (make-text-button (point) (progn (forward-sexp) (point))
                      'url url
                      'help-echo url
                      'action 'luwak-follow-link
                      'face 'button)))

(defun luwak-render-link-id (idx url)
  "Render a link by its id."
  (when (re-search-forward (format "\\[%d\\]" idx) nil t)
    (make-text-button (match-beginning 0) (match-end 0)
                      'url url
                      'help-echo url
                      'action 'luwak-follow-link
                      'face 'button)))

(defun luwak-render-link-hide-link (idx _)
  (when (re-search-forward (format "\\[%d\\]" idx) nil t)
    (replace-match "")))

(defun luwak-get-links ()
  "Get links and remove the reference section if any."
  (with-current-buffer luwak-buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^References\n\n\\(\\ *Visible links:\n\\)?" nil t)
        (let ((ref-beg (match-beginning 0))
              (results))
          (while (re-search-forward "^\\ *\\([0-9]+\\)\\.\\ *\\(.*\\)$" nil t)
            (push (match-string 2) results))
          (delete-region ref-beg (point-max))
          (reverse results))))))

(defun luwak-collect-links ()
  "Collect links into a list."
  (let ((dump (plist-get luwak-data :dump)))
    (with-temp-buffer
      (insert dump)
      (goto-char (point-min))
      (re-search-forward "^References\n\n\\(\\ *Visible links:\n\\)?" nil t)
      (delete-region (point-min) (match-end 0))
      (delq nil
       (mapcar (lambda (s)
                (when (string-match "^\\ *\\([0-9]+\\)\\. \\(.*\\)" s)
                  (concat (match-string 1 s) " " (match-string 2 s))))
               (split-string (buffer-string) "\n"))))))

(defun luwak-follow-numbered-link (link)
  "Follow a link."
  (interactive
   (list (completing-read "Select link to open: " (luwak-collect-links) nil t)))
  (luwak-open (cadr (split-string link))))

(defun luwak-start-process-with-torsocks (no-tor name buffer &rest cmd)
  (apply #'start-process name buffer
         (if no-tor cmd `("torsocks" ,@cmd))))

(defun luwak-save-dump (file-name)
  "Write dump of the current luwak buffer to FILE-NAME."
  (interactive
   (list
    (read-file-name (format "Write dump of %s to: " (plist-get luwak-data :url))
                    default-directory)))
  (let ((dump (plist-get luwak-data :dump)))
    (with-temp-buffer
      (insert dump)
      (write-file file-name)))
  (message "Wrote %s." file-name))

;; Example url rewrite function
(defun luwak-rewrite-ddg-result (url)
  "Rewrites ddg result url to save one jump."
  (let ((new-url url))
    (when (string-match
           "^https://duckduckgo.com/l/\\?uddg=\\(.*\\)&rut=.*$" url)
      (setq new-url (url-unhex-string (match-string 1 url))))
    (unless (equal url new-url)
      (message "Rewriting %s to %s" url new-url))
    new-url))

(provide 'luwak)

;;; luwak.el ends here
