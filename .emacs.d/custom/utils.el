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

(defun find-file-upwards (file-to-find)
  "Recursively search upward for file; returns path to file or nil if not found."
  (interactive)
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

(defun find-file-dir-upwards (file-to-find)
  "Recursively search upward for file; returns file's directory or nil if not found."
  (interactive)
  (let ((file (find-file-upwards file-to-find)))
    (if file (file-name-directory file) nil)))

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
  (unless sep (setq sep path-separator))
  (setenv var (concat (mapconcat 'convert-standard-filename
                                 (read-file-into-list-of-lines file)
                                 sep) sep (getenv var))))

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
(global-set-key "\C-x4z" 'window-toggle-split-direction)

;; from Steve Yegge
(defun swap-buffers() "Swap the first 2 buffers" (interactive)
  (when (> (length (window-list)) 1)
    (let* ((win1 (car (window-list)))
           (win2 (cadr (window-list)))
           (buf1 (window-buffer win1))
           (buf2 (window-buffer win2))
           (pos1 (window-start win1))
           (pos2 (window-start win2)))
      (set-window-buffer win1 buf2)
      (set-window-buffer win2 buf1)
      (set-window-start win1 pos2)
      (set-window-start win2 pos1))))
(global-set-key "\C-x4s" 'swap-buffers)

(defun rename-file-and-buffer(new-name)
  "Renames both current buffer and file it's visiting."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file(dir)
  "Moves both current buffer and file it's visiting."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1)
            dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

(defvar full-edit-accept-patterns
  '( "\\.cpp$" "\\.cc$" "\\.cxx$" "\\.c$" "\\.C$"
     "\\.h$" "\\.hh$" "\\.hpp$" "\\.hxx$" "\\.H$"
     "\\.sh$" "\\.py$" "\\.sql$" "\\.java$" "\\.in$"
     "\\.proto$"
     "^CMakeLists.txt$" "\\.cmake$"
     "^Makefile$" "^makefile$"
     ))
(defvar full-edit-reject-patterns
  '( "\\.exe$" "\\.pdb$" "\\.obj$"
     ))

(defun test-list-for-string(list regex)
  "Check if a list contains a string by regexp."
  (let ((lst list)
        curr)
    (catch 'found
      (while lst
        (setq curr (car lst))
        (if (string-match curr regex)
            (throw 'found t)
          (setq lst (cdr lst))))
      nil)))

(defun find-all-files(dir &optional symbolic)
  "Find (open) all files recursively below a directory.  Results are
  filtered via full-edit-accept-patterns and full-edit-reject-patterns.
  With optional prefix argument, will follow symbolic targets."
  (interactive)
  (let* ((all-results
          (directory-files
           dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)" t))
         (files (remove-if 'file-directory-p all-results))
         (dirs (remove-if-not 'file-directory-p all-results)))
    (setq files (if symbolic
                    (remove-if-not 'file-symlink-p files)
                  (remove-if 'file-symlink-p files)))
    (setq dirs (if symbolic
                   (remove-if-not 'file-symlink-p dirs)
                 (remove-if 'file-symlink-p dirs)))
    (mapc #'(lambda(file)
              (and
               (test-list-for-string full-edit-accept-patterns
                                     (file-name-nondirectory file))
               (not (test-list-for-string full-edit-reject-patterns
                                          (file-name-nondirectory file)))
               (find-file-noselect file)))
          files)
    (mapc 'find-all-files dirs)
    ))

(defun full-edit(root &optional arg)
  "Find (open) all files recursively below a directory."
  (interactive
   `(,(ido-read-directory-name "Directory: " nil nil t)))
  (if root
      (find-all-files (expand-file-name root arg))
    (message "No directory given")))

(global-set-key "\C-c\C-f" 'full-edit)
