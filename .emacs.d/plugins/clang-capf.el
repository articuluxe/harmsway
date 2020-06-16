;;; clang-capf.el --- Completion-at-point backend for c/c++ using clang -*- lexical-binding: t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 1.0.0
;; Keywords: c, abbrev, convenience
;; Package-Requires: ((emacs "24.4"))
;; URL: https://git.sr.ht/~zge/clang-capf

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:
;;
;; Emacs built-in `completion-at-point' completion mechanism doesn't
;; support C in any meaningful by default, which this package tries to
;; remedy, by using clang's completion mechanism. Hence this package
;; requires clang to be installed (as specified in `clang-capf-clang'.
;;
;; If a header file is not automatically found or in the default path,
;; extending `clang-capf-include-paths' or `clang-capf-extra-flags' might
;; help.
;;
;; `clang-capf' is based on/inspired by:
;; - https://opensource.apple.com/source/lldb/lldb-167.2/llvm/tools/clang/utils/clang-completion-mode.el.auto.html
;; - https://github.com/company-mode/company-mode/blob/master/company-clang.el
;; - https://github.com/brianjcj/auto-complete-clang/blob/master/auto-complete-clang.el
;; - https://www.reddit.com/r/vim/comments/2wf3cn/basic_clang_autocompletion_query/
;; - https://foicica.com/wiki/cpp-clang-completion

;;; Code:

(defgroup clang-capf nil
  "Completion back-end for C using clang."
  :group 'completion
  :prefix "clang-capf-")

(defcustom clang-capf-include-paths
  '("/usr/local/include"
    "/usr/lib/llvm-7/lib/clang/7.0.1/include"
    "/usr/include/x86_64-linux-gnu"
    "/usr/include" "." ".." "../..")
  "Paths to directories with header files."
  :type '(repeat string))

(defcustom clang-capf-special-chars
  '(?\. ?, ?\t ?\n ?\ ?\; ?\( ?\) ?\[ ?\] ?\{ ?\} ?\n ?\t ? ?\" ?\')
  "List of characters that wrap a symbol."
  :type '(repeat character))

(defcustom clang-capf-extra-flags nil
  "Additional flags to call clang with."
  :type '(repeat string))

(defcustom clang-capf-clang "clang++"
  "Path to clang binary."
  :type 'file)

(defcustom clang-capf-ignore-case nil
  "Should completion ignore case."
  :type 'boolean)

(defcustom clang-capf-show-type t
  "Should completion show types."
  :type 'boolean)


(defcustom clang-capf-add-parens t
  "Should completions automatically add parentheses."
  :type 'boolean)

(defun clang-capf--parse-output ()
  "Return list of completion options."
  (when clang-capf-show-type
    (save-excursion
      (let ((re "<#\\|#>\\|\\[#\\|#]\\(?:[[:word:]]\\|_\\)+"))
        (while (re-search-forward re nil t)
          (replace-match ""))))
    (save-excursion
      (save-match-data
        (let ((re "[[:space:]]?\\(?:[[:word:]]\\|_\\)*\\([),]\\)"))
          (while (re-search-forward re nil t)
            (replace-match (match-string 1)))))))
  (let (result)
    (while (search-forward-regexp
            "^COMPLETION: \\(.+\\) : \\(.+\\)$"
            nil t)
      (let ((symb (match-string 1)))
        (put-text-property 0 1
                           'sig
                           (match-string 2)
                           symb)
        (push symb result)))
    result))

(defun clang-capf--completions (&rest _ignore)
  "Call clang to collect suggestions at point."
  (let ((temp (generate-new-buffer " *clang*")))
    (prog2
        (apply
         #'call-process-region
         (append (list (point-min) (point-max)
                       clang-capf-clang nil temp nil
                       "-cc1" "-fsyntax-only"
                       "-code-completion-macros")
                 (mapcar (apply-partially #'concat "-I")
                         clang-capf-include-paths)
                 clang-capf-extra-flags
                 (list (format
                        "-code-completion-at=-:%d:%d"
                        (line-number-at-pos)
                        (1+ (length (encode-coding-region
                                     (line-beginning-position)
                                     (point) 'utf-8 t))))
                       "-")))
        (with-current-buffer temp
          (goto-char (point-min))
          (clang-capf--parse-output))
      (kill-buffer temp))))

(defun clang-capf--annotate (str)
  "Extract type of completed symbol from STR as annotation."
  (let ((sig (get-text-property 0 'sig str)))
    (when sig (concat " : " sig))))

(defun clang-capf--exit (str finished)
  "Add parentheses if applicable based on STR.
FINISHED contains the final state of the completion."
  (let ((sig (get-text-property 0 'sig str)))
    (when (and (memq finished '(sole finished)) sig)
      (cond ((string-match-p "\\`\\(?:[[:word:]]\\|_\\)*(" sig)
             (insert "()"))
            ((string-match-p "\\`\\(?:[[:word:]]\\|_\\)*\\[" sig)
             (insert "[]"))
            ((string-match-p "\\`\\(?:[[:word:]]\\|_\\)*{" sig)
             (insert "{}")))
      (forward-char -1))))

;;;###autoload
(defun clang-capf ()
  "Function used for `completion-at-point-functions' using clang."
  (unless clang-capf-clang
    (error "Company either not installed or not in path"))
  (list (save-excursion
          (unless (memq (char-before) clang-capf-special-chars)
            (backward-sexp))
          (point))
        (save-excursion
          (unless (memq (char-after) clang-capf-special-chars)
            (forward-sexp))
          (point))
        (completion-table-with-cache #'clang-capf--completions
                                     clang-capf-ignore-case)
        :annotation-function (and clang-capf-show-type
                                  #'clang-capf--annotate)
        :exit-function #'clang-capf--exit
        :exclusive 'no))

(provide 'clang-capf)

;;; clang-capf.el ends here
