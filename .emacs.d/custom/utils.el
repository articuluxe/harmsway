;; -*- Mode: Emacs-Lisp -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Dan Harms utils.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;


; Jump to matching parentheses
(defun jump-to-matching-paren() "Go to matching paren" (interactive)
  (if (looking-at "\\s\(")
      (forward-list 1)
    (backward-char)
    (if (looking-at "\\s\)")
        (progn
         (forward-char 1)
         (forward-list -1))
      (forward-char 1))))
(global-set-key "\e\ep" 'jump-to-matching-paren)

(defun highlight-paren-right()
  "search forward for a parenthesized sexp and set region if found"
   (interactive)
   (let ((pos (search-forward-regexp "\\s\(" nil t)))
     (when pos
       (set-mark-command nil)
       (backward-char)
       (forward-list 1)
       (backward-char)
       (setq deactivate-mark nil))))
(global-set-key "\M-]" 'highlight-paren-right)

(defun highlight-paren-left()
  "search backward for a parenthesized sexp and set region if found"
   (interactive)
   (let ((pos (search-backward-regexp "\\s\)" nil t)))
     (when pos
       (set-mark-command nil)
       (forward-char)
       (forward-list -1)
       (forward-char)
       (setq deactivate-mark nil))))
(global-set-key "\M-[" 'highlight-paren-left)

(defun highlight-enclosing-paren(&optional arg)
  "assume point is bounded by paren and set region to that exp"
   (interactive "P")
   (if arg
       (let ((pos (search-forward-regexp "\\s\)" nil t)))
         (when pos
           (backward-char)
           (set-mark-command nil)
           (forward-char)
           (forward-list -1)
           (forward-char)
           (setq deactivate-mark nil)))
     (let ((pos (search-backward-regexp "\\s\(" nil t)))
       (when pos
         (forward-char)
         (set-mark-command nil)
         (backward-char)
         (forward-list 1)
         (backward-char)
         (setq deactivate-mark nil)))))
(global-set-key "\M-p" 'highlight-enclosing-paren)

(defun enclose-by-braces (left right)
  "insert braces around a region or point"
  (interactive "r")
  (if (use-region-p) ; act on region
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char end)
          (insert right)
          (goto-char start)
          (insert left)))
    (progn ; act around point
      (insert left right)
      (backward-char 1))))
(global-set-key "\e\e(" (lambda()(interactive)(enclose-by-braces ?( ?) )))
(global-set-key "\e\e[" (lambda()(interactive)(enclose-by-braces ?[ ?] )))
(global-set-key "\e\e{" (lambda()(interactive)(enclose-by-braces ?{ ?} )))
(global-set-key "\e\e<" (lambda()(interactive)(enclose-by-braces ?< ?> )))

(defun highlight-current-sexp(&optional arg)
  "Highlight the current sexp around point"
  (interactive "P")
  (let ((n (if arg arg 1)))
    (unless (looking-at "\\_<")
      (backward-sexp n))
    (set-mark-command nil)
    (forward-sexp n)
    (setq deactivate-mark nil)))
(global-set-key "\e\er" 'highlight-current-sexp)

(defun align-repeat-regexp (start end regexp)
  "Repeat alignment for regexp"
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end (concat "\\(\\s-*\\)" regexp) 1 1 t))

;; word count
(defun wordcount () "print buffer word count in minibuffer" (interactive)
   (save-excursion
     (let ((count 0))
       (goto-char (point-min))
       (while (< (point) (point-max))
         (forward-word 1)
         (setq count (1+ count)))
       (message "buffer contains %d words" count))))

;; indent entire file
(defun indent-buffer () "indent entire buffer" (interactive)
   (indent-region (point-min) (point-max) nil))
(global-set-key "\C-cq" 'indent-buffer)

(defun window-toggle-split-direction()
  "Switch window split from horizontal to vertical, or vice versa."
  (interactive)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbor-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbor1 (windmove-find-other-window neighbor-dir win))
               (neighbor2 (if next-win (with-selected-window next-win
                                         (windmove-find-other-window
                                          neighbor-dir next-win)))))
          (setq done (and (eq neighbor1 neighbor2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbor-dir)
                                   other-buf))))))))
(global-set-key "\C-cx" 'window-toggle-split-direction)

(defun find-file-upwards (file-to-find)
  "Recursively search upward for file; returns path to file or nil if not found."
  (let*
      ((find-file-r (lambda (path)
                      (let* ((parent (file-name-directory path))
                             (possible-file (concat parent file-to-find)))
                        (cond
                         ((file-exists-p possible-file) possible-file) ; found
                         ; parent of ~ is nil, parent of / is itself
                         ; This terminating condition accounts for both
                         ((or (null parent) (equal parent (directory-file-name parent))) nil)
                         (t (funcall find-file-r (directory-file-name parent))))))))
    (funcall find-file-r default-directory)))

(defun goto-line-with-feedback()
  "Show line numbers temporarily while prompting for the target line."
  (interactive)
  (if (and (or (not (boundp 'linum-mode)) (not linum-mode))
           (not current-prefix-arg))
      (unwind-protect
          (progn
            (linum-mode 1)
            (call-interactively 'goto-line))
        (linum-mode -1))
    (call-interactively 'goto-line)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun read-file-into-list-of-lines(file)
  "Read a file into a list of strings split line by line."
  (interactive)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun load-environment-variable-from-file(var file &optional sep)
  "Loads each line from the specified file into the environment var."
  (interactive)
  (unless sep (setq sep ";"))
  (setenv var (concat (mapconcat 'identity
                                 (read-file-into-list-of-lines file)
                                 sep) sep (getenv var))))
