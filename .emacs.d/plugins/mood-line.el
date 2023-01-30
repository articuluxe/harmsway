;;; mood-line.el --- A minimal mode line inspired by doom-modeline -*- lexical-binding: t; -*-

;; Author: Jessie Hildebrandt <jessieh.net>
;; Homepage: https://gitlab.com/jessieh/mood-line
;; Keywords: mode-line faces
;; Version: 2.1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; mood-line is a minimal mode line configuration that aims to replicate
;; some of the features of the more advanced doom-modeline package.
;;
;; Features offered:
;; * Clean, minimal design
;; * Customizable glyph sets
;; * Anzu and multiple-cursors counter
;; * Version control status indicator
;; * Custom Flycheck/Flymake indicator
;; * Lightweight with no dependencies
;;
;; To activate mood-line:
;; (mood-line-mode)

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

;; -------------------------------------------------------------------------- ;;
;;
;; Byte-compiler declarations
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; Compile time requirements
;; ---------------------------------- ;;

(eval-when-compile
  (require 'flymake))

;; ---------------------------------- ;;
;; External variable defs
;; ---------------------------------- ;;

(defvar anzu--cached-count)
(defvar anzu--current-position)
(defvar anzu--overflow-p)
(defvar anzu--total-matched)

(defvar flycheck-current-errors)

;; ---------------------------------- ;;
;; External function decls
;; ---------------------------------- ;;

(declare-function cl-struct-slot-value "cl-macs" (struct-type slot-name inst))

(declare-function flycheck-count-errors "flycheck" (errors))

(declare-function flymake-running-backends "flymake" ())
(declare-function flymake-reporting-backends "flymake" ())

(declare-function mood-line-segment-indentation--segment "mood-line-segment-indentation" ())

(declare-function mood-line-segment-modal--evil "mood-line-segment-modal" ())
(declare-function mood-line-segment-modal--meow "mood-line-segment-modal" ())
(declare-function mood-line-segment-modal--god "mood-line-segment-modal" ())

(declare-function mc/num-cursors "multiple-cursors" ())

(declare-function string-blank-p "subr-x" (string))

(declare-function warning-numeric-level "warnings" (level))

;; -------------------------------------------------------------------------- ;;
;;
;; Constants
;;
;; -------------------------------------------------------------------------- ;;

(defconst mood-line-glyphs-ascii
  '((:checker-info . ?i)
    (:checker-issues . ?+)
    (:checker-good . ?-)
    (:checker-checking . ?~)
    (:checker-errored . ?x)
    (:checker-interrupted . ?=)

    (:vc-added . ?+)
    (:vc-needs-merge . ?>)
    (:vc-needs-update . ?v)
    (:vc-conflict . ?x)
    (:vc-good . ?-)

    (:buffer-narrowed . ?v)
    (:buffer-modified . ?*)
    (:buffer-read-only . ?#)

    (:count-separator . ?*))
  "Set of ASCII glyphs for use with mood-line.")

(defconst mood-line-glyphs-fira-code
  '((:checker-info . ?↳)
    (:checker-issues . ?→)
    (:checker-good . ?✓)
    (:checker-checking . ?⟳)
    (:checker-errored . ?x)
    (:checker-interrupted . ?=)

    (:vc-added . ?+)
    (:vc-needs-merge . ?⟷)
    (:vc-needs-update . ?↓)
    (:vc-conflict . ?x)
    (:vc-good . ?✓)

    (:buffer-narrowed . ?◢)
    (:buffer-modified . ?●)
    (:buffer-read-only . ?■)

    (:count-separator . ?×))
  "Set of Fira Code-compatible glyphs for use with mood-line.")

(defconst mood-line-glyphs-unicode
  '((:checker-info . ?🛈)
    (:checker-issues . ?⚑)
    (:checker-good . ?✔)
    (:checker-checking . ?🗘)
    (:checker-errored . ?✖)
    (:checker-interrupted . ?⏸)

    (:vc-added . ?🞤)
    (:vc-needs-merge . ?⟷)
    (:vc-needs-update . ?↓)
    (:vc-conflict . ?✖)
    (:vc-good . ?✔)

    (:buffer-narrowed . ?▼)
    (:buffer-modified . ?●)
    (:buffer-read-only . ?■)

    (:count-separator . ?✕))
  "Set of Unicode glyphs for use with mood-line.")

;; -------------------------------------------------------------------------- ;;
;;
;; Custom definitions
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; Group definitions
;; ---------------------------------- ;;

(defgroup mood-line nil
  "A minimal mode line configuration."
  :group 'mode-line)

(defgroup mood-line-faces nil
  "Faces used by mood-line."
  :group 'mood-line
  :group 'faces)

;; ---------------------------------- ;;
;; Variable definitions
;; ---------------------------------- ;;

(defcustom mood-line-show-indentation-style nil
  "When non-nil, show the indentation style of the current buffer."
  :group 'mood-line
  :type 'boolean)

(defcustom mood-line-show-eol-style nil
  "When non-nil, show the EOL style of the current buffer."
  :group 'mood-line
  :type 'boolean)

(defcustom mood-line-show-encoding-information nil
  "When non-nil, show the encoding format of the current buffer."
  :group 'mood-line
  :type 'boolean)

(defcustom mood-line-show-cursor-point nil
  "When non-nil, show the `point' value as an integer."
  :group 'mood-line
  :type 'boolean)

(defcustom mood-line-show-major-mode t
  "When non-nil, show the name of the major mode of the current buffer."
  :group 'mood-line
  :type 'boolean)

(defcustom mood-line-glyph-alist mood-line-glyphs-ascii
  "Alist mapping glyph names to characters used to draw some mode line segments.

mood-line includes several sets of glyphs by default:

`mood-line-glyphs-ascii'     | Basic ASCII character glyphs
`mood-line-glyphs-fira-code' | Fira Code-compatible glyphs
`mood-line-glyphs-unicode'   | Fancy unicode glyphs

Note that if a character provided by a glyph set is not included in your default
font, the editor will render it with a fallback font.  If your fallback font is
not the same height as your default font, the mode line may unexpectedly grow
or shrink.

Keys are names for different mode line glyphs, values are characters for that
glyph.  Glyphs used by mood-line include:

`:checker-info'        | Syntax checker reports notes
`:checker-issues'      | Syntax checker reports issues
`:checker-good'        | Syntax checker reports no issues
`:checker-checking'    | Syntax checker is running
`:checker-errored'     | Syntax checker is stopped due to an error
`:checker-interrupted' | Syntax checker is paused

`:vc-added'            | VC backend reports additions/changes
`:vc-needs-merge'      | VC backend reports required merge
`:vc-needs-update'     | VC backend reports upstream is ahead of local
`:vc-conflict'         | VC backend reports conflict
`:vc-good'             | VC backend has nothing to report

`:buffer-narrowed'     | File-backed buffer is narrowed
`:buffer-modified'     | File-backed buffer is modified
`:buffer-read-only'    | File-backed buffer is read-only

`:count-separator'     | Separates some indicator names from numerical counts

`mood-line-glyphs-ascii' will be used as a fallback wherever the a glyph may be
found to be missing in `mood-line-glyph-alist'."
  :group 'mood-line
  :type `(alist :tag "Character map alist"
                :key-type (symbol :tag "Glyph name")
                :value-type (character :tag "Character to use")))

;; ---------------------------------- ;;
;; Face definitions
;; ---------------------------------- ;;

(defface mood-line-buffer-name
  '((t (:inherit mode-line-buffer-id)))
  "Face used for displaying the value of `buffer-name'."
  :group 'mood-line-faces)

(defface mood-line-buffer-status-modified
  '((t (:inherit error :weight normal)))
  "Face used for the ':buffer-modified' buffer status indicator."
  :group 'mood-line-faces)

(defface mood-line-buffer-status-read-only
  '((t (:inherit shadow :weight normal)))
  "Face used for the ':buffer-read-only' buffer status indicator."
  :group 'mood-line-faces)

(defface mood-line-buffer-status-narrowed
  '((t (:inherit font-lock-doc-face :weight normal)))
  "Face used for the ':buffer-narrowed' buffer status indicator."
  :group 'mood-line-faces)

(defface mood-line-major-mode
  '((t (:inherit bold)))
  "Face used for the major mode indicator."
  :group 'mood-line-faces)

(defface mood-line-status-neutral
  '((t (:inherit mood-line-unimportant)))
  "Face used for neutral or inactive status indicators."
  :group 'mood-line-faces)

(defface mood-line-status-info
  '((t (:inherit font-lock-keyword-face :weight normal)))
  "Face used for generic status indicators."
  :group 'mood-line-faces)

(defface mood-line-status-success
  '((t (:inherit success :weight normal)))
  "Face used for success status indicators."
  :group 'mood-line-faces)

(defface mood-line-status-warning
  '((t (:inherit warning :weight normal)))
  "Face for warning status indicators."
  :group 'mood-line-faces)

(defface mood-line-status-error
  '((t (:inherit error :weight normal)))
  "Face for error status indicators."
  :group 'mood-line-faces)

(defface mood-line-encoding
  '((t (:inherit mood-line-unimportant)))
  "Face used for buffer/file encoding information."
  :group 'mood-line-faces)

(defface mood-line-unimportant
  '((t (:inherit shadow :weight normal)))
  "Face used for less important mode line elements."
  :group 'mood-line-faces)

;; ---------------------------------- ;;
;; Obsolete faces
;; ---------------------------------- ;;

(define-obsolete-face-alias 'mood-line-modified 'mood-line-buffer-status-modified "2.1.0")

;; -------------------------------------------------------------------------- ;;
;;
;; Helper functions
;;
;; -------------------------------------------------------------------------- ;;

(defun mood-line--get-glyph (glyph)
  "Return character from `mood-line-glyph-alist' for GLYPH.

If a character could not be found for the requested glyph, a fallback will be
returned from `mood-line-glyphs-ascii'."
  (char-to-string (or (alist-get glyph
                                 mood-line-glyph-alist)
                      (alist-get glyph
                                 mood-line-glyphs-ascii))))

(defun mood-line--format (left right)
  "Format a mode line with a `LEFT' and `RIGHT' justified list of elements.
The mode line should fit the `window-width' with space between the lists."
  (let ((reserve (length right)))
    (concat left
            " "
            (propertize " "
                        'display `((space :align-to (- right
                                                       (- 0 right-margin)
                                                       ,reserve))))
            right)))

;; -------------------------------------------------------------------------- ;;
;;
;; Optional/lazy loaded segments
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; Indentation style
;; ---------------------------------- ;;

(defun mood-line-segment-indentation ()
  "Display the indentation style of the current buffer (if enabled)."
  (when mood-line-show-indentation-style
    (require 'mood-line-segment-indentation)
    (mood-line-segment-indentation--segment)))

;; ---------------------------------- ;;
;; Modal editing
;; ---------------------------------- ;;

(defun mood-line-segment-modal ()
  "Return the correct mode line segment for the first active modal mode found.

Modal modes checked, in order: `evil-mode', `meow-mode', `god-mode'."
  (cond
   ((bound-and-true-p evil-mode)
    (require 'mood-line-segment-modal)
    (mood-line-segment-modal--evil))
   ((bound-and-true-p meow-mode)
    (require 'mood-line-segment-modal)
    (mood-line-segment-modal--meow))
   ((featurep 'god-mode)
    (require 'mood-line-segment-modal)
    (mood-line-segment-modal--god))))

;; -------------------------------------------------------------------------- ;;
;;
;; Anzu segment
;;
;; -------------------------------------------------------------------------- ;;

(defun mood-line-segment-anzu ()
  "Display color-coded anzu status information."
  (when (bound-and-true-p anzu--state)
    (cond
     ((eq anzu--state 'replace-query)
      (format #("Replace%s%d  "
                7 10 (face mood-line-status-info))
              (mood-line--get-glyph :count-separator)
              anzu--cached-count))
     (anzu--overflow-p
      (format #("%d/%d+  "
                0 2 (face mood-line-status-info)
                3 6 (face mood-line-status-error))
              anzu--current-position anzu--total-matched))
     (t
      (format #("%d/%d  "
                0 2 (face mood-line-status-info))
              anzu--current-position anzu--total-matched)))))

;; -------------------------------------------------------------------------- ;;
;;
;; multiple-cursors segment
;;
;; -------------------------------------------------------------------------- ;;

(defun mood-line-segment-multiple-cursors ()
  "Display the number of active multiple-cursors."
  (when (bound-and-true-p multiple-cursors-mode)
    (format #("MC%s%d  "
              2 5 (face mood-line-status-info))
            (mood-line--get-glyph :count-separator)
            (mc/num-cursors))))

;; -------------------------------------------------------------------------- ;;
;;
;; VC segment
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; Update function
;; ---------------------------------- ;;

(defvar-local mood-line--vc-text nil)

(defun mood-line--vc-update-segment (&rest _)
  "Update `mood-line--vc-text' against the current VCS state."
  (setq mood-line--vc-text
        (when (and vc-mode
                   buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (branch (substring-no-properties vc-mode
                                                  (+ (if (eq backend 'Hg) 2 3)
                                                     2)))
                 (state (vc-state buffer-file-name
                                  (vc-backend buffer-file-name))))
            (cond
             ((memq state '(edited added))
              (format #("%s %s  "
                        0 2 (face mood-line-status-info))
                      (mood-line--get-glyph :vc-added)
                      branch))
             ((eq state 'needs-merge)
              (format #("%s %s  "
                        0 2 (face mood-line-status-warning))
                      (mood-line--get-glyph :vc-needs-merge)
                      branch))
             ((eq state 'needs-update)
              (format #("%s %s  "
                        0 2 (face mood-line-status-warning))
                      (mood-line--get-glyph :vc-needs-update)
                      branch))
             ((memq state '(removed conflict unregistered))
              (format #("%s %s  "
                        0 2 (face mood-line-status-error))
                      (mood-line--get-glyph :vc-conflict)
                      branch))
             (t
              (format #("%s %s  "
                        0 5 (face mood-line-status-neutral))
                      (mood-line--get-glyph :vc-good)
                      branch)))))))

;; ---------------------------------- ;;
;; Segment function
;; ---------------------------------- ;;

(defun mood-line-segment-vc ()
  "Display color-coded version control information."
  mood-line--vc-text)

;; -------------------------------------------------------------------------- ;;
;;
;; Checker segment
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; Flycheck update function
;; ---------------------------------- ;;

(defvar-local mood-line--checker-flycheck-text nil)

(defun mood-line--checker-flycheck-count-errors ()
  "Return alist with count of all error types in `flycheck-current-errors'.

Counts will be returned in an alist as the `cdr' of the following keys:
`'note-count'    | All notes reported by checker
`'error-count'   | All errors reported by checker
`'warning-count' | All warnings reported by checker
`'issue-count'   | All errors and warnings reported by checker
`'all-count'     | Everything reported by checker"
  (let-alist (flycheck-count-errors flycheck-current-errors)
    (let ((note-count (+ (or .info 0)))
          (error-count (+ (or .error 0)))
          (warning-count (+ (or .warning 0))))
      `((note-count . ,note-count)
        (error-count . ,error-count)
        (warning-count . ,warning-count)
        (issue-count . ,(+ warning-count
                           error-count))
        (all-count . ,(+ note-count
                         warning-count
                         error-count))))))

(defun mood-line--checker-flycheck-update-segment (&optional status)
  "Update `mood-line--checker-flycheck-text' against provided flycheck STATUS."
  (setq mood-line--checker-flycheck-text
        (pcase status
          ('finished
           (let-alist (mood-line--checker-flycheck-count-errors)
             (cond
              ((> .error-count 0)
               (format #("%s %s Issue%s  "
                         0 2 (face mood-line-status-error))
                       (mood-line--get-glyph :checker-issues)
                       .issue-count
                       (if (> .issue-count 1) "s" "")))
              ((> .warning-count 0)
               (format #("%s %s Issue%s  "
                         0 2 (face mood-line-status-warning))
                       (mood-line--get-glyph :checker-issues)
                       .issue-count
                       (if (> .issue-count 1) "s" "")))
              ((> .note-count 0)
               (format #("%s %s Note%s  "
                         0 2 (face mood-line-status-info))
                       (mood-line--get-glyph :checker-info)
                       .note-count
                       (if (> .note-count 1) "s" "")))
              ((zerop .all-count)
               (format #("%s No Issues  "
                         0 12 (face mood-line-status-neutral))
                       (mood-line--get-glyph :checker-good))))))
          ('running
           (format #("%s Checking  "
                     0 12 (face mood-line-status-neutral))
                   (mood-line--get-glyph :checker-checking)))
          ('errored
           (propertize (concat (mood-line--get-glyph :checker-errored)
                               " Error  ")
                       'face 'mood-line-status-error))
          ('interrupted
           (propertize (concat (mood-line--get-glyph :checker-interrupted)
                               " Paused  ")
                       'face 'mood-line-status-neutral))
          ('no-checker ""))))

;; ---------------------------------- ;;
;; Flycheck segment function
;; ---------------------------------- ;;

(defun mood-line-segment-checker-flycheck ()
  "Display the current status of flycheck."
  mood-line--checker-flycheck-text)

;; ---------------------------------- ;;
;; Flymake update function
;; ---------------------------------- ;;

(defvar-local mood-line--checker-flymake-text nil)

(defun mood-line--checker-flymake-count-report-type (type)
  "Return count of current flymake reports of TYPE."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (eq (cl-struct-slot-value 'flymake--diag 'type d) type)
        (cl-incf count)))
    count))

(defun mood-line--checker-flymake-count-errors ()
  "Return alist with count of all current flymake diagnostic reports.

Counts will be returned in an alist as the cdr of the following keys:
`'note-count'    | All notes reported by checker
`'error-count'   | All errors reported by checker
`'warning-count' | All warnings reported by checkero
`'issue-count'   | All errors and warnings reported by checker
`'all-count'     | Everything reported by checker"
  (let ((note-count (mood-line--checker-flymake-count-report-type :note))
        (error-count (mood-line--checker-flymake-count-report-type :error))
        (warning-count (mood-line--checker-flymake-count-report-type :warning)))
    `((note-count . ,note-count)
      (error-count . ,error-count)
      (warning-count . ,warning-count)
      (issue-count . ,(+ warning-count
                         error-count))
      (all-count . ,(+ note-count
                       warning-count
                       error-count)))))

(defun mood-line--checker-flymake-update-segment (&rest _)
  "Update `mood-line--checker-flymake-text' against the state of flymake."
  (setq mood-line--checker-flymake-text
        (when (and (fboundp 'flymake-is-running)
                   (flymake-is-running))
          (let-alist (mood-line--checker-flymake-count-errors)
            (cond
             ((seq-difference (flymake-running-backends)
                              (flymake-reporting-backends))
              (propertize (concat (mood-line--get-glyph :checker-checking)
                                  " Checking  ")
                          'face 'mood-line-status-info))
             ((> .error-count 0)
              (propertize (concat (mood-line--get-glyph :checker-issues)
                                  " Errors: "
                                  (number-to-string .all-count)
                                  "  ")
                          'face 'mood-line-status-error))
             ((> .warning-count 0)
              (propertize (concat (mood-line--get-glyph :checker-issues)
                                  " Issues: "
                                  (number-to-string .all-count)
                                  "  ")
                          'face 'mood-line-status-warning))
             ((> .note-count 0)
              (propertize (concat (mood-line--get-glyph :checker-info)
                                  " Info: "
                                  (number-to-string .all-count)
                                  "  ")
                          'face 'mood-line-status-info))
             (t
              (propertize (concat (mood-line--get-glyph :checker-good)
                                  " Good  ")
                          'face 'mood-line-status-success)))))))

;; ---------------------------------- ;;
;; Flymake segment function
;; ---------------------------------- ;;

(defun mood-line-segment-checker-flymake ()
  "Display the current status of flymake."
  mood-line--checker-flymake-text)

;; ---------------------------------- ;;
;; Checker segment function
;; ---------------------------------- ;;

(defun mood-line-segment-checker ()
  "Return the correct mode line segment for the first active checker found.

Checkers checked, in order: `flycheck', `flymake'."
  (cond
   ((bound-and-true-p flycheck-mode)
    (mood-line-segment-checker-flycheck))
   ((bound-and-true-p flymake-mode)
    (mood-line-segment-checker-flymake))))

;; -------------------------------------------------------------------------- ;;
;;
;; Buffer information segments
;;
;; -------------------------------------------------------------------------- ;;

;; ---------------------------------- ;;
;; Buffer status segment
;; ---------------------------------- ;;

(defun mood-line-segment-buffer-status ()
  "Return an indicator representing the status of the current buffer."
  (concat (if (buffer-file-name (buffer-base-buffer))
              (cond
               ((and (buffer-narrowed-p)
                     (buffer-modified-p))
                (propertize (mood-line--get-glyph :buffer-narrowed)
                            'face 'mood-line-buffer-status-modified))
               ((and (buffer-narrowed-p)
                     buffer-read-only)
                (propertize (mood-line--get-glyph :buffer-narrowed)
                            'face 'mood-line-buffer-status-read-only))
               ((buffer-narrowed-p)
                (propertize (mood-line--get-glyph :buffer-narrowed)
                            'face 'mood-line-buffer-status-narrowed))
               ((buffer-modified-p)
                (propertize (mood-line--get-glyph :buffer-modified)
                            'face 'mood-line-buffer-status-modified))
               (buffer-read-only
                (propertize (mood-line--get-glyph :buffer-read-only)
                            'face 'mood-line-buffer-status-read-only))
               (t " "))
            (if (buffer-narrowed-p)
                (propertize (mood-line--get-glyph :buffer-narrowed)
                            'face 'mood-line-buffer-status-narrowed)
              " "))
          " "))

;; ---------------------------------- ;;
;; Buffer name segment
;; ---------------------------------- ;;

(defun mood-line-segment-buffer-name ()
  "Display the name of the current buffer."
  (propertize "%b  "
              'face 'mood-line-buffer-name))

;; ---------------------------------- ;;
;; Cursor position segment
;; ---------------------------------- ;;

(defun mood-line-segment-cursor-position ()
  "Display the position of the cursor in the current buffer."
  (concat "%l:%c"
          (when mood-line-show-cursor-point
            (propertize (format ":%d" (point))
                        'face 'mood-line-unimportant))
          (propertize " %p%%  "
                      'face 'mood-line-unimportant)))

;; ---------------------------------- ;;
;; EOL segment
;; ---------------------------------- ;;

(defun mood-line-segment-eol ()
  "Display the EOL type for the coding system of the current buffer."
  (when (and mood-line-show-eol-style
             buffer-file-coding-system)
    (pcase (coding-system-eol-type buffer-file-coding-system)
      (0 "LF  ")
      (1 "CRLF  ")
      (2 "CR  "))))

;; ---------------------------------- ;;
;; Encoding segment
;; ---------------------------------- ;;

(defun mood-line-segment-encoding ()
  "Display the name of the coding system of the current buffer."
  (when (and mood-line-show-encoding-information
             buffer-file-coding-system)
    (concat (let ((coding-system (coding-system-plist buffer-file-coding-system)))
              (cond
               ((memq (plist-get coding-system :category)
                      '(coding-category-undecided coding-category-utf-8))
                "UTF-8")
               (t
                (upcase (symbol-name (plist-get coding-system :name))))))
            "  ")))

;; ---------------------------------- ;;
;; Major mode segment
;; ---------------------------------- ;;

(defun mood-line-segment-major-mode ()
  "Display the name of the major mode of the current buffer."
  (when mood-line-show-major-mode
    (concat (propertize (substring-no-properties (format-mode-line mode-name))
                        'face 'mood-line-major-mode)
            "  ")))

;; ---------------------------------- ;;
;; Misc. info segment
;; ---------------------------------- ;;

(defun mood-line-segment-misc-info ()
  "Display the current value of `mode-line-misc-info'."
  (let ((misc-info (format-mode-line mode-line-misc-info)))
    (unless (string-blank-p misc-info)
      (concat (propertize (string-trim misc-info)
                          'face 'mood-line-unimportant)
              "  "))))

;; ---------------------------------- ;;
;; Process segment
;; ---------------------------------- ;;

(defun mood-line-segment-process ()
  "Display the current value of `mode-line-process'."
  (let ((process-info (format-mode-line mode-line-process)))
    (unless (string-blank-p process-info)
      (concat (string-trim process-info)
              "  "))))

;; -------------------------------------------------------------------------- ;;
;;
;; mood-line-mode definition
;;
;; -------------------------------------------------------------------------- ;;

(defvar-local mood-line--default-mode-line mode-line-format)
(defvar-local mood-line--anzu-cons-mode-line-p nil)

;; ---------------------------------- ;;
;; Activation function
;; ---------------------------------- ;;

(defun mood-line--activate ()
  "Activate mood-line, installing hooks and setting `mode-line-format'."

  ;; Set up flycheck hooks
  (add-hook 'flycheck-status-changed-functions
            #'mood-line--checker-flycheck-update-segment)
  (add-hook 'flycheck-mode-hook
            #'mood-line--checker-flycheck-update-segment)

  ;; Set up flymake hooks
  (advice-add 'flymake-start :after
              #'mood-line--checker-flymake-update-segment)
  (advice-add 'flymake--handle-report :after
              #'mood-line--checker-flymake-update-segment)

  ;; Set up VC hooks
  (add-hook 'find-file-hook
            #'mood-line--vc-update-segment)
  (add-hook 'after-save-hook
            #'mood-line--vc-update-segment)
  (advice-add 'vc-refresh-state :after
              #'mood-line--vc-update-segment)

  ;; Disable anzu's mode line segment setting, saving the previous
  ;; setting to be restored later (if present)
  (when (boundp 'anzu-cons-mode-line-p)
    (setq mood-line--anzu-cons-mode-line-p anzu-cons-mode-line-p))
  (setq-default anzu-cons-mode-line-p nil)

  ;; Save previous value of `mode-line-format' to be restored later
  (setq mood-line--default-mode-line mode-line-format)

  ;; Set new value of `mode-line-format'
  (setq-default mode-line-format
                '((:eval
                   (mood-line--format
                    ;; Left
                    (format-mode-line
                     '(" "
                       (:eval (mood-line-segment-modal))
                       (:eval (mood-line-segment-buffer-status))
                       (:eval (mood-line-segment-buffer-name))
                       (:eval (mood-line-segment-anzu))
                       (:eval (mood-line-segment-multiple-cursors))
                       (:eval (mood-line-segment-cursor-position))))

                    ;; Right
                    (format-mode-line
                     '((:eval (mood-line-segment-indentation))
                       (:eval (mood-line-segment-eol))
                       (:eval (mood-line-segment-encoding))
                       (:eval (mood-line-segment-vc))
                       (:eval (mood-line-segment-major-mode))
                       (:eval (mood-line-segment-misc-info))
                       (:eval (mood-line-segment-checker))
                       (:eval (mood-line-segment-process))
                       " ")))))))

;; ---------------------------------- ;;
;; Deactivation function
;; ---------------------------------- ;;

(defun mood-line--deactivate ()
  "Deactivate mood-line, uninstalling hooks and restoring `mode-line-format'."

  ;; Remove flycheck hooks
  (remove-hook 'flycheck-status-changed-functions
               #'mood-line--checker-flycheck-update-segment)
  (remove-hook 'flycheck-mode-hook
               #'mood-line--checker-flycheck-update-segment)

  ;; Remove VC hooks
  (remove-hook 'file-find-hook
               #'mood-line--vc-update-segment)
  (remove-hook 'after-save-hook
               #'mood-line--vc-update-segment)
  (advice-remove #'vc-refresh-state
                 #'mood-line--vc-update-segment)

  ;; Restore anzu's mode line segment setting
  (setq-default anzu-cons-mode-line-p mood-line--anzu-cons-mode-line-p)

  ;; Restore the original value of `mode-line-format'
  (setq-default mode-line-format mood-line--default-mode-line))

;; ---------------------------------- ;;
;; Mode definition
;; ---------------------------------- ;;

;;;###autoload
(define-minor-mode mood-line-mode
  "Toggle mood-line on or off."
  :group 'mood-line
  :global t
  :lighter nil
  (if mood-line-mode
      (mood-line--activate)
    (mood-line--deactivate)))

;; -------------------------------------------------------------------------- ;;
;;
;; Provide package
;;
;; -------------------------------------------------------------------------- ;;

(provide 'mood-line)

;;; mood-line.el ends here
