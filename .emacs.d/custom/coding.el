;; -*- Mode: Emacs-Lisp -*-
;; coding.el --- coding utilities
;; Copyright (C) 2015  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-11-20 09:14:05 dan.harms>
;; Modified by: Dan Harms
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:

;;

;; Code:

(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-u" 'uncomment-region)

(require 'cc-chainsaw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; preproc-font-lock ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'preproc-font-lock)
(global-set-key "\C-c#"
                (lambda()(interactive)
                  (preproc-font-lock-global-mode
                   (if preproc-font-lock-mode 0 1))))

(require 'grep)
(defun my/grep (&optional arg)
  "A wrapper around grep to provide convenient shortcuts to
   adjust the root directory. With a prefix arg of 64 (C-u C-u
   C-u), or if the variable 'project-root-dir is not defined in
   the current profile, the search directory will be chosen
   interactively by the user using ido. With a prefix arg of
   16 (C-u C-u), use the current directory. With a prefix arg of
   4 (C-u), the directory will be chosen interactively by the
   user among the src directories configured using the current
   profile. Otherwise, use the first src directory configured
   in the profile."
  (interactive "p")
  (let* ((root (profile-current-get 'project-root-dir))
         (dirs (profile-current-get 'grep-dirs))
         (first (cdr (car dirs)))
         (prompt "Grep root: ")
         (dir
          (cond ((or (null root) (null dirs) (= arg 64))
                 (ido-read-directory-name prompt nil nil t))
                ((= arg 16) ".")
                ((= arg 4) (funcall my/choose-func dirs prompt))
                (t first)))
         (remote (file-remote-p dir)))
    (when remote                        ;remove remote prefix if present
      (setq dir
            (replace-regexp-in-string (regexp-quote remote) "" dir)))
    (unless (file-remote-p default-directory)
      (setq dir
            ;; some variants of grep don't handle relative paths
            ;; (but expand-file-name doesn't work remotely)
            (expand-file-name dir)))
    (grep-apply-setting
     'grep-command
     (concat "find -P "
             ;; some greps dislike trailing slashes
             (directory-file-name dir)
             " \"(\" -name \"*moc_*\" -o -name \"*qrc_*\" \")\" "
             "-prune -o -type f \"(\" -name \"*.cpp\" -o -name \"*.h\" "
             "-o -name \"*.cc\" -o -name \"*.hh\" -o -name \"*.cxx\" "
             "-o -name \"*.hxx\" -o -name \"*.h\" -o -name \"*.c\" "
             "-o -name \"*.H\" -o -name \"*.C\" -o -name \"*.hpp\" "
             "-o -name \"*.in\" -o -name \"*.ac\" -o -name \"*.el\" "
             "-o -name \"*.sql\" -o -name \"*.py\" -o -name \"*.proto\" "
             "-o -name \"*.sh\" -o -name \"*.cs\" "
             "\")\" -print0 | xargs -0 grep -Isn "
             (thing-at-point 'symbol t)
             ))
    (command-execute 'grep)))
(global-set-key "\C-cg" 'my/grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; c++-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook
 'c-mode-common-hook
 (lambda ()
   (setq-default indent-tabs-mode nil)
   (setq c-auto-newline t)
   (c-toggle-hungry-state t)
   ;; (setq comment-column 40)
   (setq hide-ifdef-lines t)
   (hide-ifdef-mode 1)
   (make-local-variable 'my/compile-command)
   (define-key c++-mode-map (kbd "\C-c RET") 'my/compile)
   (define-key c++-mode-map "\C-cm" 'my/recompile)
   (define-key c++-mode-map "\C-ck" 'kill-compilation)
   (define-key c++-mode-map "\C-c\C-c" 'comment-region)
   (define-key c++-mode-map "\C-c\C-u" 'uncomment-region)
   (setq comment-start "/*") (setq comment-end "*/")
   ;; skips the final included file, ending in `:', when traversing compile
   ;; errors.  See
   ;; `http://stackoverflow.com/questions/15489319/how-can-i-skip-in-file-included-from-in-emacs-c-compilation-mode'
   (setf (nth 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
   (c-add-style "default-style"
                (quote
                 ((c-basic-offset . 3)
                  (c-electric-pound-behavior . (alignleft))
                  (c-cleanup-list . (
                                     empty-defun-braces
                                     defun-close-semi
                                     one-liner-defun
                                     scope-operator
                                     list-close-comma
                                     compact-empty-funcall
                                     comment-close-slash
                                     ))
                  (c-offsets-alist . (
                                      (innamespace           . 0)
                                      (substatement-open     . 0)
                                      (inline-open           . 0)
                                      (statement-case-intro  . +)
                                      (statement-case-open   . +)
;(statement-cont . c-lineup-math)
                                      (access-label          . -2)
                                      (comment-intro         . c-lineup-comment)
                                      (member-init-intro     . +)
                                      (arglist-cont-nonempty . +)
;(comment-intro . 0)
;(arglist-intro . c-lineup-arglist-intro-after-paren)
;(arglist-close . c-lineup-arglist)
                                      )))))
   ))

(add-hook
 'c++-mode-hook
 (lambda()
   (font-lock-add-keywords
    nil '(;; complete some fundamental keywords (+ Qt)
          ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\|slots\\|signals\\)\\>" . font-lock-keyword-face)
          ;; add the new C++11 keywords
          ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
          ;; hexadecimal numbers
          ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
          ;; TODO declarations
          ("\\<[tT][oO][dD][oO]\\>" 0 font-lock-warning-face t)
          ;; Qt fontification
          ("\\<Q_OBJECT\\|SIGNAL\\|SLOT\\>" . font-lock-keyword-face)
          ("\\<QT?\\(_\\sw+\\)+\\>" . font-lock-keyword-face)
          ("\\<Q[A-Z][A-Za-z0-9]*\\>" . font-lock-type-face)
          ) t)
   ) t)

(add-to-list 'compilation-error-regexp-alist 'boost-test)
(add-to-list 'compilation-error-regexp-alist-alist
             '(boost-test
               "^[[:digit:]]+:\\s-*\\(.*\\):\\([[:digit:]]+\\):\\s-+\\(fatal\\s-\\)?error" 1 2))

(defun print-current-function() "Print current function under point."
  (interactive)
  (message (which-function)))
(global-set-key "\C-cp" 'print-current-function)

(defun toggle-c-comment-delimiters()
  "Toggle the comment delimiters for c-derived programming languages."
  (interactive)
  (if (= 0 (length comment-end))
      (progn
        (setq comment-start "/*")
        (setq comment-end "*/")
        (message "/* Using comments like this */")
        )
    (progn
      (setq comment-start "//")
      (setq comment-end "")
      (message "// Using comments like this"))))
(global-set-key "\C-c/" 'toggle-c-comment-delimiters)

(defun find-my-tags-file() "Find tags file"
  (interactive)
  (let ((my-tags-file (find-file-upwards nil "TAGS")))
    (if my-tags-file
        (progn
          (message "Loading tags file: %s" my-tags-file)
          (run-with-timer 1 nil 'visit-tags-table my-tags-file))
      (message "Did not find tags file")
      )))

;; include ifdefs
(defvar site-name nil "A possibly empty name of the current site.")
(defun add-header-include-ifdefs (&optional arg)
  "Add header include guards. With optional prefix argument, query for the
   base name. Otherwise, the base file name is used."
  (interactive "P")
  (let* ((name (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (buffer-name)))
         (project-name (profile-current-get 'project-name))
         (str
          (replace-regexp-in-string
           "\\.\\|-" "_"
           name)))
    (if (fboundp 's-snake-case)
        (setq str (upcase (s-snake-case str)))
      (setq str (upcase str)))
    (save-excursion
      (if arg                           ;ask user for stem
          (setq str (concat
                     (read-string "Include guard stem: " nil nil str) "_H"))
        ;; no arg; if project-name or site-name are defined, prepend them
        (when project-name
          (setq str (concat project-name "_" str))))
        (when site-name
          (setq str (concat site-name "_" str)))
      (setq str (upcase (concat "__" str "__")))
      (goto-char (point-min))
      (insert "#ifndef " str "\n#define " str "\n\n")
      (goto-char (point-max))
      (insert "\n#endif")
      (insert-char ?\s c-basic-offset)
      (insert "/* #ifndef " str " */\n")
      )))
(global-set-key "\C-ci" 'add-header-include-ifdefs)

;; class header
(defun insert-class-header (&optional arg)
  "Insert a formatted class header given the current selection or position."
  (interactive "P")
  (let ((str
         (if (region-active-p)
             (buffer-substring-no-properties (region-beginning)(region-end))
           (thing-at-point 'symbol)))
        (i 0) len)
    (if (or arg (= 0 (length str)))
        (setq str (read-string "Enter the title symbol: ")))
                                        ; (message "symbol %s is %d chars long" str (length str))(read-char)
    (c-beginning-of-defun)
    (move-beginning-of-line nil)
    (insert "//")
    (insert-char ?- (- fill-column 2))
    (insert "\n")
    (insert "//---- " str " ")
    (setq len (- (- fill-column 8) (length str)))
    (while (< i len)
      (insert "-")
      (setq i (1+ i)))
    (insert "\n//")
    (insert-char ?- (- fill-column 2))
    (insert "\n")
    ))
(global-set-key "\C-ch" 'insert-class-header)

;; casting
(defvar my/cast-history-list nil)
(defun insert-cast (start end)
  "Insert code for a cast around a region."
  (interactive "r")
  (let ((initial (if my/cast-history-list
                     (car my/cast-history-list)
                   "static"))
        type str)
    (setq type (read-string "Enter the data type to cast to: "))
    (setq str (funcall my/choose-func
                       '("static" "dynamic" "reinterpret" "const")
                       "Enter the type of cast: "))
    ;; (setq str (ido-completing-read "Enter the type of cast: "
    ;;                                '("static" "dynamic" "reinterpret" "const")
    ;;                                nil t nil my/cast-history-list "static"))
    (if (= 0 (length str))
        (setq str initial))
    (save-excursion
      (goto-char end)(insert ")")
      (goto-char start)(insert str "_cast<" type ">("))))
(global-set-key "\C-cc" 'insert-cast)

;; namespace
(defun wrap-namespace-region (start end)
  "Insert enclosing namespace brackets around a region."
  (interactive "r")
  (let ((str))
    (setq str (read-string "Enter the namespace name: "))
    (save-excursion
      (goto-char end) (insert "\n}")
      (insert-char ?\s c-basic-offset)
      (insert "// end ")
      (if (= 0 (length str))
          (insert "anonymous "))
      (insert "namespace " str "\n")
      (goto-char start)
      (insert "namespace ")
      (if (not (= 0 (length str)))
          (insert str " "))
      (insert "{\n\n"))))
(global-set-key "\C-cn" 'wrap-namespace-region)

(defun remove-leading-whitespace (start end)
  "Remove a region's leading whitespace"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (looking-at "\\s-")
        (replace-match "" nil nil)))))

(defun remove-trailing-whitespace (start end)
  "Remove a region's trailing whitespace"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-max))
      (backward-char)
      (while (looking-at "\\s-")
        (replace-match "" nil nil)
        (backward-char)))))

(defun remove-embedded-newlines (start end)
  "Remove a region's embedded newlines"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
        (replace-match "" nil nil)))))

(defun cleanup-func-param-spacing (start end is-decl)
  "clean up spacing of a single function parameter"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      ;; remove multiple consecutive whitespace
      (while (re-search-forward "\\s-\\{2,\\}" nil t)
        (replace-match " " nil nil))
    (goto-char (point-min))
    ;; remove whitespace before punctuation or parentheses
    (while (re-search-forward "\\s-+\\(\\s.\\|\\s\(\\|\\s\)\\)" nil t)
      (replace-match "\\1" nil nil))
    (goto-char (point-min))
    ;; remove whitespace after punctuation or parentheses
    (while (re-search-forward "\\(\\s.\\|\\s\(\\|\\s\)\\)\\s-+" nil t)
      (replace-match "\\1" nil nil))
    (goto-char (point-min))
    (if is-decl
         ;; for declarations, add a space if not present before parameter name
         (let ((identifier "\\sw\\|_\\|:"))
           (while (re-search-forward (concat "\\(.*?\\)\\s-?\\(\\(?:"
                                             identifier
                                             "\\)+\\)\\s-*$") nil t)
             (replace-match "\\1 \\2" nil nil)))))))

(defun clean-up-func-param (start end indent do-spacing is-decl)
  "Cleans up one (comma-separated) param of a function declaration."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      ;; if we're later indenting, dispose of old newlines
      (if indent
          (remove-embedded-newlines (point-min)(point-max)))
      (when do-spacing
        (cleanup-func-param-spacing (point-min)(point-max) is-decl)
        (remove-leading-whitespace (point-min)(point-max))
        (remove-trailing-whitespace (point-min)(point-max))))))

(defun clean-up-func-params (start end indent is-decl should-comment)
  "Does the actual work of cleaning up spacing and (optionally) indentation of a function declaration"
  (let ((saved nil))
    (save-excursion
      (save-restriction
        (narrow-to-region start end) ; separate by ',' and process each parameter
        (goto-char (point-min))
        (setq saved (cons (point) saved))
        (let ((start (point-min)))
          (catch 'break
            (while (<= (point) (point-max))
              (cond
               ((or (looking-at ",") (eq (point) (point-max)))
                (progn
                  (goto-char
                   (save-excursion
                     (save-restriction
                       (narrow-to-region start (point))
                       (goto-char (point-min))
                       ;; isolate any initializer
                       (if (re-search-forward "=" nil t)
                           (let ((is-quoted nil))
                             (backward-char)
                             (clean-up-func-param (point-min)(point) indent t is-decl)
                             ;; ensure spaces around the '='
                             (insert " ")
                             (forward-char)
                             (insert " ")
                             ;; don't strip whitespace or otherwise clean up quoted strings
                             (save-excursion
                               (when (re-search-forward
                                      "\\s-*\\(\\s\"+\\)\\(.*\\)\\(\\s\"+\\)\\s-*" nil t)
                                 (setq is-quoted t)
                                 (replace-match "\\1\\2\\3" nil nil)))
                             (clean-up-func-param (point) (point-max) indent
                                                  (not is-quoted) nil)
                             (if should-comment
                                 ;; begin comment before the '='
                                 (comment-region (- (point) 3) (point-max)))
                             (point-max))
                         (clean-up-func-param (point-min) (point-max) indent t is-decl)
                         (point-max))
                         )))
                   ;; save this point to insert newline later
                   (setq saved (cons (point) saved))
                   ;; stop at region end
                   (if (eq (point) (point-max))
                       (throw 'break nil))
                   (forward-char) ; at end of buffer this would quit
                   (insert " ")
                   (setq start (point))))
                ((looking-at "\\s\(")
                 (forward-list 1))      ;skip past parentheses
                (t (forward-char))
               ))))))
      (if indent
          (mapc (lambda(pos) ; points later in buffer are processed first
                  (progn
                    (goto-char pos)
                    (newline-and-indent)))
                saved)
        ;; if not inserting newlines (maintaining those present), we may need to re-indent
        (indent-region start end))))

(global-set-key "\e\eiy" (lambda(start end)(interactive "r")
                           (clean-up-func-params start end t t t)))
(global-set-key "\e\ein" (lambda(start end)(interactive "r")
                           (clean-up-func-params start end t t nil)))
(global-set-key "\e\ed" (lambda(start end)(interactive "r")
                           (clean-up-func-params start end nil t nil)))
(global-set-key "\e\ec" (lambda(start end)(interactive "r")
                           (clean-up-func-params start end t nil nil)))
(global-set-key "\e\eu" (lambda(start end)(interactive "r")
                           (clean-up-func-params start end nil nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gud ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/gud-hook()
  (set (make-local-variable 'gdb-show-main) t)
  ;; highlight recently-changed variables
  (set (make-local-variable 'gdb-show-changed-values) t)
  ;; watch expressions sharing same variable name
  (set (make-local-variable 'gdb-use-colon-colon-notation) t)
  (set (make-local-variable 'gdb-create-source-file-list) nil)
  (gdb-many-windows 1))
(add-hook 'gud-mode-hook 'my/gud-hook)

(defun my/launch-gdb()
  "Launch gdb automatically in the test directory."
  (interactive)
  (let ((root (profile-current-get 'project-root-dir))
        (prefix (profile-current-get 'remote-prefix))
        (sub-dirs (profile-current-get 'debug-sub-dirs))
        exec-dir exec)
    (when root
      (setq exec-dir
            (concat
             prefix root
             (cond ((eq (length sub-dirs) 1) (car sub-dirs))
                   ((null sub-dirs) "")
                   (t (funcall my/choose-func
                               sub-dirs "Debug dir:"))))))
    (unless (and exec-dir (file-exists-p exec-dir))
      (setq exec-dir default-directory))
    (setq exec (ido-read-file-name "Debug executable: " exec-dir nil t))
    (gdb (concat "gdb -i=mi " exec))))
(global-set-key [f4] 'my/launch-gdb)
(global-set-key "\C-c4" 'my/launch-gdb)

;;;;;;;;;;;;;;;;;;;;;;;;;;; c++11 enum class hack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: doesn't work
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a c++ \"enum class\"."
  (interactive)
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum\\s-+class\\s-+[^}]*")))) ;or end with '+'

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-                                ;or '0
    '+))

(defun fix-enum-class()
  "Setup c++-mode to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))
(add-hook 'c++-mode-hook 'fix-enum-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; c++11 lambda hack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued c++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; detect "[...](" or "[...]{" preceded by "," or "("
                     ;; and with uclosed brace
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ;no additional indent
          ad-do-it)))                   ;default behavior

;; coding.el ends here
