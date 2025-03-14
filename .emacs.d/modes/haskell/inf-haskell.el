;;; inf-haskell.el --- Interaction with an inferior Haskell process -*- lexical-binding: t -*-

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.
;; Copyright (C) 2017 Vasantha Ganesh Kanniappan <vasanthaganesh.k@tuta.io>

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: languages, Haskell

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for the buffer that holds the inferior process

;; Todo:

;; - Check out Shim for ideas.
;; - i-h-load-buffer and i-h-send-region.

;;; Code:

(require 'comint)
(require 'shell)             ; For directory tracking.
(require 'etags)
(require 'compile)
(require 'haskell-decl-scan)
(require 'haskell-cabal)
(require 'haskell-customize)
(require 'cl-lib)
(require 'haskell-string)

(defgroup inferior-haskell nil
  "Settings for REPL interaction via `inferior-haskell-mode'"
  :link '(custom-manual "(haskell-mode)inferior-haskell-mode")
  :prefix "inferior-haskell-"
  :prefix "haskell-"
  :group 'haskell)

(defcustom inferior-haskell-hook nil
  "The hook that is called after starting inf-haskell."
  :type 'hook)

(defun haskell-program-name-with-args ()
  "Return the command with the arguments to start the repl based on the
directory structure."
  (cl-ecase (haskell-process-type)
    (ghci       (cond ((eq system-type 'cygwin) `("ghcii.sh" ,@haskell-process-args-ghci))
                      (t (append
                          (if (listp haskell-process-path-ghci)
                              haskell-process-path-ghci
                            (list haskell-process-path-ghci))
                          haskell-process-args-ghci))))
    (cabal-repl `(,haskell-process-path-cabal "repl" ,@haskell-process-args-cabal-repl))
    (stack-ghci `(,haskell-process-path-stack "ghci" ,@haskell-process-args-stack-ghci))))

(defconst inferior-haskell-info-xref-re
  "-- Defined at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?$")

(defconst inferior-haskell-module-re
  "-- Defined in \\(.+\\)$"
  "Regular expression for matching module names in :info.")

(defvar inferior-haskell-multiline-prompt-re
  "^\\*?[[:upper:]][\\._[:alnum:]]*\\(?: \\*?[[:upper:]][\\._[:alnum:]]*\\)*| "
  "Regular expression for matching multiline prompt.
the one inside :{ ... :} blocks.")

(defconst inferior-haskell-error-regexp-alist
  `(;; Format of error messages used by GHCi.
    ("^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)\\([Ww]arning\\)?"
     1 2 4 ,@(if (fboundp 'compilation-fake-loc)
                 '((6) nil (5 '(face nil font-lock-multiline t)))))
    ;; Runtime exceptions, from ghci.
    ("^\\*\\*\\* Exception: \\(.+?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\)): .*"
     1 ,@(if (fboundp 'compilation-fake-loc) '((2 . 4) (3 . 5)) '(2 3)))
    ;; GHCi uses two different forms for line/col ranges, depending on
    ;; whether it's all on the same line or not :-( In Emacs-23, I could use
    ;; explicitly numbered subgroups to merge the two patterns.
    ("^\\*\\*\\* Exception: \\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\): .*"
     1 2 ,(if (fboundp 'compilation-fake-loc) '(3 . 4) 3))
    ;; Info messages.  Not errors per se.
    ,@(when (fboundp 'compilation-fake-loc)
        `(;; Other GHCi patterns used in type errors.
          ("^[ \t]+at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)$"
           1 2 (3 . 4) 0)
          ;; Foo.hs:318:80:
          ;;     Ambiguous occurrence `Bar'
          ;;     It could refer to either `Bar', defined at Zork.hs:311:5
          ;;                  or `Bar', imported from Bars at Frob.hs:32:0-16
          ;;                       (defined at Location.hs:97:5)
          ("[ (]defined at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3 0)
          ("imported from .* at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)$"
           1 2 (3 . 4) 0)
          ;; Info xrefs.
          (,inferior-haskell-info-xref-re 1 2 (3 . 4) 0))))
  "Regexps for error messages generated by inferior Haskell processes.
The format should be the same as for `compilation-error-regexp-alist'.")

(defconst haskell-prompt-regexp
  "^[[:alnum:].*_() |λ]*> "
  "Ignore everything before the first '> '.  This allows us to
correctly interpret multi-line input even when modules are
imported.")

(defconst inferior-haskell-cont-prompt-regexp
  "^[[:alnum:].*_() |λ]*| "
  "Continuation prompt rexep.
Used to remove them from the output by the comint preoutput filter.  See
`inferior-haskell-remove-extra-prompts'.

This should be a similar regexp as `haskell-prompt-regexp', but it usually
ends with \"| \" instead of \"> \".")

(defconst inferior-haskell-maybe-cont-prompt-regexp
  "^[[:alnum:].*_() |λ]*[>|] "
  "A continuation or non-continuation prompt regexp.
This should match any prompt, a continuation or a common prompt.  This regexp
should be similar to `haskell-prompt-regexp' and
`inferior-haskell-cont-prompt-regex' as it should match both.

It is used to remove multiple prompts on the comint preoutput filter.  See
`inferior-haskell-remove-extra-prompts'.")

;;; TODO
;;; -> Make font lock work for strings, directories, hyperlinks
;;; -> Make font lock work for key words???

(defvaralias 'inferior-haskell-mode-map 'inf-haskell-map)

(defvar inf-haskell-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-d" 'comint-kill-subjob)
    map))

(define-derived-mode inferior-haskell-mode comint-mode "Inf-Haskell"
  "Major mode for interacting with an inferior Haskell process."
  :group 'inferior-haskell
  (setq-local comint-prompt-regexp haskell-prompt-regexp)

  (setq-local paragraph-start haskell-prompt-regexp)

  (setq-local comint-input-autoexpand nil)
  (setq-local comint-prompt-read-only t)

  ;; Setup directory tracking.
  (setq-local shell-cd-regexp ":cd")
  (condition-case nil
      (shell-dirtrack-mode 1)
    (error      ;The minor mode function may not exist or not accept an arg.
     (setq-local shell-dirtrackp t)
     (add-hook 'comint-input-filter-functions 'shell-directory-tracker
               nil 'local)))

  ;; Setup `compile' support so you can just use C-x ` and friends.
  (setq-local compilation-error-regexp-alist inferior-haskell-error-regexp-alist)
  (setq-local compilation-first-column 0) ;GHCI counts from 0.
  (if (and (not (boundp 'minor-mode-overriding-map-alist))
           (fboundp 'compilation-shell-minor-mode))
      ;; If we can't remove compilation-minor-mode bindings, at least try to
      ;; use compilation-shell-minor-mode, so there are fewer
      ;; annoying bindings.
      (compilation-shell-minor-mode 1)
    ;; Else just use compilation-minor-mode but without its bindings because
    ;; things like mouse-2 are simply too annoying.
    (compilation-minor-mode 1)
    (let ((map (make-sparse-keymap)))
      (dolist (keys '([menu-bar] [follow-link]))
        ;; Preserve some of the bindings.
        (define-key map keys (lookup-key compilation-minor-mode-map keys)))
      (add-to-list 'minor-mode-overriding-map-alist
                   (cons 'compilation-minor-mode map))))
  (add-hook 'inferior-haskell-hook 'inferior-haskell-init)
  
  ;; Avoid multiple prompts at the end of the output
  (add-hook 'comint-preoutput-filter-functions
            #'inferior-haskell-remove-extra-prompts nil t))

(defun inferior-haskell-remove-extra-prompts (str)
  "Remove any extra Haskell-prompts from STR.
Remove multiple prompts from STR.  All prompts indicating continuation are
completely removed.  Only remain the last non-continuantion prompt.

Examples:
The input \"Prelude> Prelude> \" will return \"Prelude> \".
The input \"Prelude| Prelude| \" will return \"\".

These kind of output are usually produced by the multiple line input (i.e. when
using \":{ ... :}\" code in the GHCi interpreter).  Sometimes, comint would note
filter the prompts out.  For this reason, this function shoud be added to the
hook `comint-preoutput-filter-functions', to be executed before comint insert
STR to the buffer.

Some libraries, such as ob-haskell.el, considers the multilple prompts as part
of the evaluation output.  Moreover, it does not provide any information to the
user. Removing these prompts provides a better reading and less code for parsing
the output."
  (let ((last-match nil))
    (while (string-match inferior-haskell-maybe-cont-prompt-regexp str)
      (setq last-match (match-string 0 str))
      (setq str (substring str (match-end 0))))
    ;; Remove prompt-cont if it is the last one.
    (if (or (null last-match)
            (string-match inferior-haskell-cont-prompt-regexp last-match))
        str
      (concat last-match str))))

(defvar inferior-haskell-buffer nil
  "The buffer in which the inferior process is running.")

(defun inferior-haskell-start-process ()
  "Start an inferior haskell process.
With universal prefix \\[universal-argument], prompts for a COMMAND,
otherwise uses `haskell-program-name-with-args'.
It runs the hook `inferior-haskell-hook' after starting the process and
setting up the inferior-haskell buffer."
  (let ((command (haskell-program-name-with-args)))
    (when inferior-haskell-root-dir
      (setq default-directory inferior-haskell-root-dir))
    (setq inferior-haskell-buffer
          (apply 'make-comint "haskell" (car command) nil (cdr command)))
    (with-current-buffer inferior-haskell-buffer
      (inferior-haskell-mode)
      (run-hooks 'inferior-haskell-hook))))

(defun inferior-haskell-process ()
  "Restart if not present."
  (cond ((and (buffer-live-p inferior-haskell-buffer)
              (comint-check-proc inferior-haskell-buffer))
         (get-buffer-process inferior-haskell-buffer))
        (t (inferior-haskell-start-process)
           (inferior-haskell-process))))

;;;###autoload
(defun run-haskell ()
  "Show the inferior-haskell buffer.  Start the process if needed."
  (interactive)
  (let ((proc (inferior-haskell-process)))
    (pop-to-buffer-same-window (process-buffer proc))))

(defvar inferior-haskell-result-history nil)

(defvar haskell-next-input ""
  "This is a temporary variable to store the intermediate results while
`accecpt-process-output' with `haskell-extract-exp'")

(defun haskell-extract-exp (str)
  (setq haskell-next-input (concat haskell-next-input str))
  (if (with-temp-buffer
        (insert haskell-next-input)
        (re-search-backward haskell-prompt-regexp nil t 1))
      (progn
        (push (substring haskell-next-input
                         0
                         (1- (with-temp-buffer
                               (insert haskell-next-input)
                               (re-search-backward haskell-prompt-regexp nil t 1))))
              inferior-haskell-result-history)
        (setq haskell-next-input ""))
    ""))

(defun inferior-haskell-no-result-return (strg)
  (let ((proc (inferior-haskell-process)))
    (with-local-quit
      (progn
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (haskell-extract-exp output)))
        (process-send-string proc strg)
        (accept-process-output proc)
        (sit-for 0.1)
        (setq comint-preoutput-filter-functions nil)))))

(defun inferior-haskell-get-result (inf-expr)
  "Submit the expression `inf-expr' to ghci and read the result."
  (let* ((times 5))
    (inferior-haskell-no-result-return (concat inf-expr "\n"))
    (while (and (> times 0)
                (not (stringp (car inferior-haskell-result-history))))
      (setq times (1- times))
      (inferior-haskell-no-result-return (concat inf-expr "\n")))
    (haskell-string-chomp (car inferior-haskell-result-history))))

(defun inferior-haskell-init ()
  "The first thing run while initalizing inferior-haskell-buffer"
  (with-local-quit
    (with-current-buffer inferior-haskell-buffer
      (process-send-string (inferior-haskell-process) "\n")
      (accept-process-output (inferior-haskell-process))
      (sit-for 0.1))))

(defvar haskell-set+c-p nil
  "t if `:set +c` else nil")

(defun haskell-set+c ()
  "set `:set +c` is not already set"
  (if (not haskell-set+c-p)
      (inferior-haskell-get-result ":set +c")))

(provide 'inf-haskell)

;;; inf-haskell.el ends here
