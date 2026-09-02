;;; lambda-line.el --- A custom status line  -*- lexical-binding: t -*-

;; Author: Colin McLear
;; Maintainer: Colin McLear
;; Version: 0.6.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://codeberg.org/Lambda-Emacs/lambda-line
;; Keywords: mode-line faces

;; This file is NOT part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; lambda-line is a minimal, though opinionated, status-line (i.e. in Emacs the
;; information display either in the mode-line and/or header-line) for use as
;; either header or footer in a buffer. The structure of the status-line takes
;; the following form: [ status | name (primary) tertiary | secondary ]

;; Usage: M-x lambda-line-mode

;;; Code:

(require 'face-remap)
(require 'cl-lib)

;; Declare optional functions to suppress compiler warnings
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-toplevel "magit-repos")
(declare-function flycheck-count-errors "flycheck")
(declare-function lsp-workspaces "lsp-mode")
(declare-function eglot-managed-p "eglot")
(declare-function vc-git--run-command-string "vc-git")
(declare-function vc-responsible-backend "vc")
(declare-function mu4e-message-at-point "mu4e-view")
(declare-function mu4e-message-field "mu4e-message")
(declare-function mu4e-context-current "mu4e-context")
(declare-function mu4e-context-name "mu4e-context")
(declare-function elfeed-entry-title "elfeed-show")
(declare-function elfeed-entry-tags "elfeed-show")
(declare-function elfeed-db-last-update "elfeed-db")
(declare-function org-capture-get "org-capture")
(declare-function doc-view-current-page "doc-view")
(declare-function pdf-cache-number-of-pages "pdf-cache")
(declare-function Info-toc-nodes "info")

;;;; Group

(defgroup lambda-line nil
  "lambda-line group"
  :group 'mode-line
  :link '(url-link :tag "Homepage" "https://codeberg.org/Lambda-Emacs/lambda-line"))

;;;; Custom Variable Settings

(defcustom lambda-line-window-width-limit 0.25
  "The limit of the window width.
If `window-width' is smaller than the limit, some information won't be
displayed. It can be an integer or a float number. `nil' means no limit."
  :type '(choice integer
                 float
                 (const :tag "Disable" nil))
  :group 'lambda-line)

(defcustom lambda-line-position 'bottom
  "Default modeline position (top or bottom)"
  :type '(choice
          (const :tag "Nil" nil)
          (const :tag "Top"    top)
          (const :tag "Bottom" bottom))
  :group 'lambda-line)

(defcustom lambda-line-prefix t
  "Include a prefix icon to indicate buffer status in the status-line."
  :type 'boolean
  :group 'lambda-line)

(defcustom lambda-line-prefix-padding t
  "Include prefix padding."
  :type '(choice boolean string)
  :group 'lambda-line)

(defcustom lambda-line-prefix-padding-left nil
  "Include prefix padding to the left."
  :type '(choice boolean string)
  :group 'lambda-line)

(defcustom lambda-line-user-mode nil
  "User supplied mode to be evaluated for modeline."
  :type '(choice (const nil) function)
  :group 'lambda-line)

(defcustom lambda-line-abbrev nil
  "If t then show abbreviated mode symbol in modeline.
Default is nil. To change the values of the major-mode symbols
see the value of `lambda-line-abbrev-alist'"
  :group 'lambda-line
  :type 'boolean)

(defcustom lambda-line-git-diff-mode-line t
  "If t then show diff lines in modeline."
  :group 'lambda-line
  :type 'boolean)

(defcustom lambda-line-vc-refresh-on-repo-change t
  "If t then refresh version control state when the repository changes.
The branch name, the state indicator, and the diff counts all come
from `vc-mode', which Emacs recomputes only when a file is visited or
saved.  A commit, stage, or checkout made from anywhere else therefore
leaves the status-line showing the state as it was before.  When this
option is non-nil, lambda-line recomputes that state for the buffers
currently on display and leaves the remaining buffers of the
repository to refresh when a window next shows them.  Each refresh
runs a handful of Git subprocesses, so set this to nil to trade an
accurate status-line for fewer of them."
  :group 'lambda-line
  :type 'boolean)

(defcustom lambda-line-vc-symbol ""
  "Symbol to use in buffers visiting files under version control"
  :group 'lambda-line
  :type 'string)

;; Visual Bell
(defcustom lambda-line-visual-bell t
  "If t then use `lambda-line-visual-bell'."
  :group 'lambda-line
  :type 'boolean)

;; Evil state indicator
(defcustom lambda-line-evil-state t
  "If non-nil, show the current Evil state at the far left of the status-line.
The indicator renders only where Evil is loaded and active in the
buffer, so leaving this enabled is harmless without Evil and is robust
to Evil being loaded after lambda-line."
  :group 'lambda-line
  :type 'boolean)

(defcustom lambda-line-evil-state-alist
  '((normal       . " N ")
    (insert       . " I ")
    (visual       . " V ")
    (visual-line  . "VL ")
    (visual-block . "VB ")
    (replace      . " R ")
    (emacs        . " E ")
    (motion       . " M ")
    (operator     . " O "))
  "Alist mapping Evil state symbols to the tag shown in the status-line.
The `visual-line' and `visual-block' keys cover the visual sub-types
\(distinguished via `evil-visual-type').  Tags share a common width so
a colored-block face renders as an even field.  States absent from this
alist fall back to `lambda-line-evil-empty-tag' when
`lambda-line-evil-fixed-width' is non-nil, otherwise they show no tag."
  :group 'lambda-line
  :type '(alist :key-type symbol :value-type string))

(defcustom lambda-line-evil-fixed-width t
  "If non-nil, reserve a fixed-width field for the Evil state tag.
When the current state maps to no tag -- Evil inactive in the buffer,
or a state absent from `lambda-line-evil-state-alist' --
`lambda-line-evil-empty-tag' is shown instead of nothing, so toggling
Evil never shifts the rest of the status-line horizontally."
  :group 'lambda-line
  :type 'boolean)

(defcustom lambda-line-evil-empty-tag "   "
  "Placeholder shown in place of an Evil state tag.
Used only when `lambda-line-evil-fixed-width' is non-nil.  Its width
should match the tags in `lambda-line-evil-state-alist' so the
status-line keeps a constant width across states."
  :group 'lambda-line
  :type 'string)

;; Invert status faces
;; This make lambda-line look more like nano-modeline
(defcustom lambda-line-status-invert nil
  "If t then invert the colors to get a box effect for the corner of the status line."
  :group 'lambda-line
  :type 'boolean)

;; Mode line symbols
(defcustom lambda-line-gui-ro-symbol " ⨂"  ;;  ⬤◯⨂
  "Modeline gui read-only symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-gui-mod-symbol " ⬤" ;;  ⨀⬤
  "Modeline gui modified symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-gui-rw-symbol " ◯" ; λ ◉ ◎ ⬤◯
  "Modeline gui read-write symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-tty-ro-symbol " λ "
  "Modeline tty read-only symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-tty-mod-symbol " λ "
  "Modeline tty read-only symbol."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-tty-rw-symbol " λ "
  "Modeline tty read-write symbol."
  :group 'lambda-line
  :type 'string)

;;;; Nerd-icons prefix glyphs
;; -------------------------------------------------------------------
;; When `lambda-line-use-nerd-icons' is non-nil and the `nerd-icons'
;; package is available, prefix glyphs are resolved from
;; `lambda-line-nerd-icon-alist' instead of the default emoji/Unicode
;; symbols.  nerd-icons is an optional, lazily-checked dependency: it is
;; never `require'd, and any resolution failure falls back to the
;; default glyph.

(declare-function nerd-icons-codicon "nerd-icons")
(declare-function nerd-icons-octicon "nerd-icons")

(defvar lambda-line--nerd-glyph-cache (make-hash-table :test 'equal)
  "Cache of resolved nerd-icons glyph strings.
Keyed by (SEMANTIC-KEY . GRAPHIC-P) so graphical and terminal frames of
the same Emacs session resolve independently.")

(defun lambda-line--nerd-glyph-cache-clear ()
  "Clear the resolved nerd-icons glyph cache."
  (clrhash lambda-line--nerd-glyph-cache))

(defcustom lambda-line-use-nerd-icons nil
  "When non-nil, use nerd-icons glyphs for prefix symbols.
Requires the `nerd-icons' package and a Nerd Font.  When the package is
unavailable or a glyph cannot be resolved, lambda-line falls back to the
default symbol."
  :type 'boolean
  :group 'lambda-line
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'lambda-line--nerd-glyph-cache-clear)
           (lambda-line--nerd-glyph-cache-clear))))

(defcustom lambda-line-nerd-icon-alist
  '((read-only  . (nerd-icons-codicon "nf-cod-lock"))
    (read-write . (nerd-icons-octicon "nf-oct-pencil"))
    (modified   . (nerd-icons-octicon "nf-oct-dot_fill"))
    (terminal   . (nerd-icons-codicon "nf-cod-terminal"))
    (eshell     . (nerd-icons-codicon "nf-cod-terminal_bash"))
    (shell      . (nerd-icons-codicon "nf-cod-terminal_bash"))
    (debug      . (nerd-icons-codicon "nf-cod-debug"))
    (help       . (nerd-icons-octicon "nf-oct-question"))
    (info       . (nerd-icons-codicon "nf-cod-info"))
    (magit      . (nerd-icons-octicon "nf-oct-git_branch")))
  "Alist mapping semantic prefix keys to nerd-icons specifications.
Each value is a list (FUNCTION ICON-NAME) where FUNCTION is a nerd-icons
constructor such as `nerd-icons-codicon' and ICON-NAME is a glyph name it
accepts.  Used only when `lambda-line-use-nerd-icons' is non-nil.
Editing this alist has effect after the glyph cache is cleared, which
happens automatically when toggling `lambda-line-use-nerd-icons'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'lambda-line
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'lambda-line--nerd-glyph-cache-clear)
           (lambda-line--nerd-glyph-cache-clear))))

(defun lambda-line--nerd-font-available-p ()
  "Return non-nil when nerd-icons glyphs are likely to render.
On a graphical display this checks that the nerd-icons font family is
installed.  On a terminal Emacs cannot introspect the font, so this
trusts the user's configuration and returns t."
  (if (display-graphic-p)
      (find-font (font-spec :family (if (boundp 'nerd-icons-font-family)
                                        nerd-icons-font-family
                                      "Symbols Nerd Font Mono")))
    t))

(defun lambda-line--nerd-glyph (key)
  "Return the nerd-icons glyph string for semantic KEY, or nil.
Resolution is cached.  Return nil when nerd-icons is disabled, the
package is unavailable, the Nerd Font is not installed, KEY has no
mapping, or the glyph cannot be constructed -- in every such case the
caller falls back to the default symbol.  A leading space is prepended
to match the default symbols."
  (when (and lambda-line-use-nerd-icons
             (featurep 'nerd-icons))
    (let* ((cache-key (cons key (and (display-graphic-p) t)))
           (cached (gethash cache-key lambda-line--nerd-glyph-cache 'miss)))
      (if (not (eq cached 'miss))
          cached
        ;; Resolve once and cache, including the font-availability probe, so
        ;; `find-font' does not run on every modeline redraw.
        (puthash cache-key
                 (when (lambda-line--nerd-font-available-p)
                   (let ((spec (alist-get key lambda-line-nerd-icon-alist)))
                     (when (and spec (fboundp (car spec)))
                       (condition-case nil
                           (concat " " (apply (car spec) (cdr spec)))
                         (error nil)))))
                 lambda-line--nerd-glyph-cache)))))

(defun lambda-line--status-symbol (key gui-symbol tty-symbol)
  "Return the prefix symbol for status KEY.
Prefer a nerd-icons glyph, else GUI-SYMBOL or TTY-SYMBOL depending on the
display type."
  (or (lambda-line--nerd-glyph key)
      (if (display-graphic-p) gui-symbol tty-symbol)))

(defcustom lambda-line-truncate-value 30
  "Value of modeline truncate-length function."
  :group 'lambda-line
  :type 'integer)

(defcustom lambda-line-hspace " "
  "Space adjustment for right end of modeline."
  :type 'string
  :group 'lambda-line)

(defcustom lambda-line-space-top +.35
  "Space adjustment for top of status-line.
Positive is upwards"
  :type 'float
  :group 'lambda-line)

(defcustom lambda-line-space-bottom -.5
  "Space adjustment for bottom of status-line.
Negative is downwards."
  :type 'float
  :group 'lambda-line)

(defcustom lambda-line-symbol-position .067
  "Space adjustment for symbol in status-line.
Negative is downwards."
  :type 'float
  :group 'lambda-line)

(defcustom lambda-line-syntax t
  "Show flycheck/flymake report in status-line."
  :type 'boolean
  :group 'lambda-line)

(defcustom lambda-line-which-func nil
  "Show `which-function-mode' display in status-line."
  :type 'boolean
  :group 'lambda-line)

(defcustom lambda-line-flycheck-label "Issues: "
  "Show with flycheck/flymake issues count."
  :type 'string
  :group 'lambda-line)

(defcustom lambda-line-icon-time nil
  "When set to non-nil show the time as an icon clock.
Time info is only shown `display-time-mode' is non-nil"
  :type 'boolean
  :group 'lambda-line)

(defcustom lambda-line-position-format "%l:%c:%o"
  "`format-mode-line'."
  :type 'string
  :group 'lambda-line)

(defcustom lambda-line-time-day-and-date-format "  %H:%M %Y-%m-%e "
  "`format-time-string'."
  :type 'string
  :group 'lambda-line)

(defcustom lambda-line-time-format "  %H:%M "
  "`format-time-string'."
  :type 'string
  :group 'lambda-line)

(defcustom lambda-line-time-icon-format " %s"
  "`format-time-string'."
  :type 'string
  :group 'lambda-line)

(defcustom lambda-line-display-group-start "("
  "Modeline display group start indicator."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-display-group-end ")"
  "Modeline display group end indicator."
  :group 'lambda-line
  :type 'string)

(defcustom lambda-line-mode-formats
  '(;; with :mode-p first
    (imenu-list-mode        :mode-p lambda-line-imenu-list-mode-p
                            :format lambda-line-imenu-list-mode)
    (org-capture-mode       :mode-p lambda-line-org-capture-mode-p
                            :format lambda-line-org-capture-mode
                            :on-activate lambda-line-org-capture-activate
                            :on-deactivate lambda-line-org-capture-deactivate)
    (prog-mode              :mode-p lambda-line-prog-mode-p
                            :format lambda-line-prog-mode
                            :on-activate lambda-line-prog-activate
                            :on-deactivate lambda-line-prog-deactivate
                            :abbrev "PR")
    (mu4e-dashboard-mode    :mode-p lambda-line-mu4e-dashboard-mode-p
                            :format lambda-line-mu4e-dashboard-mode)
    (messages-mode          :mode-p lambda-line-messages-mode-p
                            :format lambda-line-messages-mode)
    (message-mode           :mode-p lambda-line-message-mode-p
                            :format lambda-line-message-mode)
    (term-mode              :mode-p lambda-line-term-mode-p
                            :format lambda-line-term-mode
                            :prefix-symbol " >_"
                            :prefix-key terminal
                            :face-prefix-active lambda-line-active-status-MD
                            :face-prefix-inactive lambda-line-inactive-status-RW
                            :always-modifiable t)
    (vterm-mode             :mode-p lambda-line-vterm-mode-p
                            :format lambda-line-term-mode
                            :prefix-symbol " >_"
                            :prefix-key terminal
                            :face-prefix-active lambda-line-active-status-MD
                            :face-prefix-inactive lambda-line-inactive-status-RW
                            :always-modifiable t)
    (eshell-mode            :mode-p lambda-line-eshell-mode-p
                            :format lambda-line-eshell-mode
                            :prefix-symbol " λ:"
                            :prefix-key eshell
                            :face-prefix-active lambda-line-active-status-MD
                            :face-prefix-inactive lambda-line-inactive-status-RW
                            :always-modifiable t)
    (shell-mode             :mode-p lambda-line-shell-mode-p
                            :format lambda-line-shell-mode
                            :prefix-symbol " >"
                            :prefix-key shell
                            :face-prefix-active lambda-line-active-status-MD
                            :face-prefix-inactive lambda-line-inactive-status-RW
                            :always-modifiable t)
    (buffer-menu-mode       :mode-p lambda-line-buffer-menu-mode-p
                            :format lambda-line-buffer-menu-mode
                            :on-activate lambda-line-buffer-menu-activate
                            :on-deactivate lambda-line-buffer-menu-deactivate)
    (calendar-mode          :mode-p lambda-line-calendar-mode-p
                            :format lambda-line-calendar-mode
                            :on-activate lambda-line-calendar-activate
                            :on-deactivate lambda-line-calendar-deactivate)
    (completion-list-mode   :mode-p lambda-line-completion-list-mode-p
                            :format lambda-line-completion-list-mode)
    (deft-mode              :mode-p lambda-line-deft-mode-p
                            :format lambda-line-deft-mode)
    (Dired-mode             :abbrev "Dir")
    (doc-view-mode          :mode-p lambda-line-doc-view-mode-p
                            :format lambda-line-doc-view-mode)
    (elfeed-search-mode     :mode-p lambda-line-elfeed-search-mode-p
                            :format lambda-line-elfeed-search-mode
                            :on-activate lambda-line-elfeed-search-activate
                            :on-deactivate lambda-line-elfeed-search-deactivate
                            :always-modifiable t)
    (elfeed-show-mode       :mode-p lambda-line-elfeed-show-mode-p
                            :format lambda-line-elfeed-show-mode
                            :always-modifiable t)
    (elpher-mode            :mode-p lambda-line-elpher-mode-p
                            :format lambda-line-elpher-mode
                            :on-activate lambda-line-elpher-activate)
    (emacs-lisp-mode        :abbrev "λ")
    (gud-mode               :prefix-symbol " 🐞"
                            :prefix-key debug
                            :face-prefix-active lambda-line-active-status-MD
                            :face-prefix-inactive lambda-line-inactive-status-RW
                            :always-modifiable t)
    (help-mode              :mode-p lambda-line-help-mode-p
                            :format lambda-line-help-mode
                            :abbrev "?"
                            :prefix-symbol " ?"
                            :prefix-key help
                            :face-prefix-active lambda-line-active-status-RO
                            :face-prefix-inactive lambda-line-inactive-status-RW
                            :always-modifiable t)
    (helpful-mode           :mode-p lambda-line-helpful-mode-p
                            :format lambda-line-help-mode
                            :abbrev "?"
                            :prefix-symbol " ?"
                            :prefix-key help
                            :face-prefix-active lambda-line-active-status-RO
                            :face-prefix-inactive lambda-line-inactive-status-RW
                            :always-modifiable t)
    (Info-mode              :mode-p lambda-line-info-mode-p
                            :format lambda-line-info-mode
                            :on-activate lambda-line-info-activate
                            :on-deactivate lambda-line-info-deactivate
                            :prefix-symbol " ℹ"
                            :prefix-key info
                            :face-prefix-active lambda-line-active-status-RO
                            :face-prefix-inactive lambda-line-inactive-status-RW
                            :always-modifiable t)
    (lisp-interaction-mode  :abbrev "λΙ"
                            :always-modifiable t)
    (magit-mode             :mode-p lambda-line-magit-mode-p
                            :format lambda-line-magit-mode
                            :abbrev "MG"
                            :prefix-symbol " ✨"
                            :prefix-key magit)
    (org-mode               :mode-p lambda-line-org-mode-p
                            :format lambda-line-org-mode)
    (markdown-mode          :mode-p lambda-line-markdown-mode-p
                            :format lambda-line-markdown-mode
                            :abbrev "MD")
    (mu4e-compose-mode      :mode-p lambda-line-mu4e-compose-mode-p
                            :format lambda-line-mu4e-compose-mode)
    (mu4e-headers-mode      :mode-p lambda-line-mu4e-headers-mode-p
                            :format lambda-line-mu4e-headers-mode)
    (mu4e-loading-mode      :mode-p lambda-line-mu4e-loading-mode-p
                            :format lambda-line-mu4e-loading-mode)
    (mu4e-main-mode         :mode-p lambda-line-mu4e-main-mode-p
                            :format lambda-line-mu4e-main-mode)
    (mu4e-view-mode         :mode-p lambda-line-mu4e-view-mode-p
                            :format lambda-line-mu4e-view-mode)
    (nxhtml-mode            :abbrev "NX")
    (org-agenda-mode        :mode-p lambda-line-org-agenda-mode-p
                            :format lambda-line-org-agenda-mode)
    (org-clock-mode         :mode-p lambda-line-org-clock-mode-p
                            :format lambda-line-org-clock-mode
                            :on-activate lambda-line-org-clock-activate
                            :on-deactivate lambda-line-org-clock-deactivate)
    (pdf-view-mode          :mode-p lambda-line-pdf-view-mode-p
                            :format lambda-line-pdf-view-mode)
    (python-mode            :abbrev "PY")
    (fundamental-mode       :mode-p lambda-line-fundamental-mode-p
                            :format lambda-line-fundamental-mode
                            :abbrev "F")
    (text-mode              :mode-p lambda-line-text-mode-p
                            :format lambda-line-text-mode
                            :abbrev "TX")

    ;; hooks only go last
    (ein-notebook-mode      :on-activate lambda-line-ein-notebook-activate
                            :on-deactivate lambda-line-ein-notebook-deactivate)
    (esh-mode               :on-activate lambda-line-esh-activate
                            :on-deactivate lambda-line-esh-deactivate)
    (ispell-mode            :on-activate lambda-line-ispell-activate
                            :on-deactivate lambda-line-ispell-deactivate)
    (mu4e-mode              :on-activate lambda-line-mu4e-activate
                            :on-deactivate lambda-line-mu4e-deactivate))

  "Modes to be evalued for modeline.
KEY mode name, for reference only. Easier to do lookups and/or replacements.
:MODE-P the function to check if :FORMAT needs to be used, first one wins.
:ON-ACTIVATE and :ON-DEACTIVATE do hook magic on enabling/disabling the mode.
:ABBREV substitutes an abbreviation if given the correct minor/major mode symbol
and a string you want to use in the modeline *as substitute for* the original.
:PREFIX-SYMBOL the default glyph shown as the buffer-status prefix.
:PREFIX-KEY a semantic key looked up in `lambda-line-nerd-icon-alist' to
substitute a nerd-icons glyph when `lambda-line-use-nerd-icons' is non-nil;
falls back to :PREFIX-SYMBOL otherwise.
"
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type (plist :key-type (choice (const :mode-p)
                                                     (const :format)
                                                     (const :on-activate)
                                                     (const :on-deactivate)
                                                     (const :abbrev)
                                                     (const :prefix-symbol)  ;; custom prefix
                                                     (const :prefix-key)     ;; nerd-icons lookup key
                                                     (const :face-prefix-active)
                                                     (const :face-prefix-inactive)
                                                     (const :always-modifiable))  ;; never read-only?
                                   :value-type (choice (function) (string) (symbol) (boolean))
                                   :tag "Mode formats"))
  :group 'lambda-line)

(defcustom lambda-line-mode-format-activate-hook nil
  "Add hooks on activation of the mode.
This is for those modes that define their own status-line."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'lambda-line)

(defcustom lambda-line-mode-format-deactivate-hook nil
  "Remove hooks on de-activation of the mode.
This is for those modes that define their own status-line."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'lambda-line)

(defcustom lambda-line-default-mode-format 'lambda-line-default-mode
  "Default mode to evaluate.
This is if no match could be found in `lambda-lines-mode-formats'"
  :type 'function
  :group 'lambda-line)

(defcustom lambda-line-default-tertiary-function nil
  "Default tertiary format format function to evaluate."
  :type '(choice (const nil) function)
  :group 'lambda-line)

(defcustom lambda-line-default-vc-mode-function 'lambda-line-vc-project-branch
  "Default version control format function to evaluate."
  :type '(choice (const :tag "None" nil)
                 (function :tag "Function"))
  :group 'lambda-line)

(defcustom lambda-line-prog-mode-info-function nil
  "Default prog-mode info format function to evaluate."
  :type '(choice (const :tag "None" nil)
                 (function :tag "Function"))
  :group 'lambda-line)

(defcustom lambda-line-lsp-indicator t
  "Show LSP/Eglot server status in modeline."
  :type 'boolean
  :group 'lambda-line)

(defcustom lambda-line-word-count-enabled nil
  "When non-nil, show word count in status-line for applicable modes."
  :type 'boolean
  :group 'lambda-line)

(defcustom lambda-line-word-count-modes '(org-mode markdown-mode text-mode)
  "List of major modes where word count should be displayed."
  :type '(repeat symbol)
  :group 'lambda-line)

(defcustom lambda-line-word-count-format " %d "
  "Format string for word count display. %d is replaced with count."
  :type 'string
  :group 'lambda-line)

(defcustom lambda-line-word-count-symbol "Ⓦ "
  "Symbol to display before word count. Unicode options: w ⓦ ω § ¶ # ∑"
  :type 'string
  :group 'lambda-line)

(defcustom lambda-line-word-count-separator " ∙ "
  "Separator to display after word count. Unicode options: ∙ • · | - ‖ │"
  :type 'string
  :group 'lambda-line)

;;;; Faces
;;;;; Line Faces

(defface lambda-line
  '((t (:inherit mode-line)))
  "Modeline face for modeline.")

(defface lambda-line-active
  '((t (:inherit lambda-line)))
  "Modeline face for active modeline."
  :group 'lambda-line-active)

(defface lambda-line-inactive
  '((default
      :inherit lambda-line)
    (((class color) (min-colors 88) (background light))
     :weight light
     :box (:line-width -1 :color "grey75" :style nil)
     :foreground "grey20" :background "grey90")
    (((class color) (min-colors 88) (background dark) )
     :weight light
     :box (:line-width -1 :color "grey40" :style nil)
     :foreground "grey80" :background "grey30"))
  "Modeline face for inactive line."
  :group 'lambda-line-inactive)

(defface lambda-line-hspace-active
  '((t (:family "Monospace" :inherit (lambda-line))))
  "Face for vertical spacer in active line.")

(defface lambda-line-hspace-inactive
  '((t (:family "Monospace" :inherit (lambda-line-inactive))))
  "Face for vertical spacer in inactive line.")

(defface lambda-line-active-name
  '((t (:inherit (lambda-line))))
  "Modeline face for active name element."
  :group 'lambda-line-active)

(defface lambda-line-inactive-name
  '((t (:inherit (lambda-line-inactive))))
  "Modeline face for inactive name element."
  :group 'lambda-line-inactive)

(defface lambda-line-active-primary
  '((t (:weight light :inherit (lambda-line))))
  "Modeline face for active primary element."
  :group 'lambda-line-active)

(defface lambda-line-inactive-primary
  '((t (:inherit (lambda-line-inactive))))
  "Modeline face for inactive primary element."
  :group 'lambda-line-inactive)

(defface lambda-line-active-secondary
  '((t (:inherit lambda-line)))
  "Modeline face for active secondary element."
  :group 'lambda-line-active)

(defface lambda-line-inactive-secondary
  '((t (:inherit (lambda-line-inactive))))
  "Modeline face for inactive secondary element."
  :group 'lambda-line-inactive)

(defface lambda-line-active-tertiary
  '((t (:inherit lambda-line)))
  "Modeline face for active tertiary element."
  :group 'lambda-line-active)

(defface lambda-line-inactive-tertiary
  '((t (:inherit (lambda-line-inactive))))
  "Modeline face for inactive tertiary element."
  :group 'lambda-line-inactive)

;;;;; Git Diff Faces

;; Faces for the +added/-removed line counts appended to the VC segment
;; when `lambda-line-git-diff-mode-line' is non-nil.  They inherit from
;; the theme-aware `success'/`error' faces so the colors follow the
;; active theme; customize them to override.

(defface lambda-line-git-diff-added
  '((t (:inherit success)))
  "Modeline face for the +N added-lines count in the git diff segment."
  :group 'lambda-line)

(defface lambda-line-git-diff-removed
  '((t (:inherit error)))
  "Modeline face for the -N removed-lines count in the git diff segment."
  :group 'lambda-line)

;;;;; Version Control Segment Faces

;; Named faces for the pieces of the VC segment that carry their own
;; color.  They inherit sensible, theme-aware defaults; set them
;; explicitly to restyle the divider or the LSP indicator.

(defface lambda-line-vc-divider
  '((t (:inherit shadow)))
  "Modeline face for the divider between the project name and branch."
  :group 'lambda-line)

(defface lambda-line-lsp-active
  '((t (:inherit success)))
  "Modeline face for the LSP/Eglot indicator when a server is active."
  :group 'lambda-line)

(defface lambda-line-lsp-available
  '((t (:inherit warning)))
  "Modeline face for the LSP indicator when a server is available but inactive."
  :group 'lambda-line)


;;;;; Status Bar Faces

;; lambda-line uses a colored symbol to indicate the status of the buffer

(defface lambda-line-active-status-RO
  '((t (:inherit lambda-line :foreground "yellow")))
  "Modeline face for active READ-ONLY element."
  :group 'lambda-line-active)

(defface lambda-line-inactive-status-RO
  '((t (:inherit lambda-line-inactive :foreground "light gray")))
  "Modeline face for inactive READ-ONLY element."
  :group 'lambda-line-inactive)

(defface lambda-line-active-status-RW
  '((t (:inherit lambda-line :foreground "green")))
  "Modeline face for active READ-WRITE element."
  :group 'lambda-line-active)

(defface lambda-line-inactive-status-RW
  '((t (:inherit lambda-line-inactive :foreground "light gray")))
  "Modeline face for inactive READ-WRITE element."
  :group 'lambda-line-inactive)

(defface lambda-line-active-status-MD
  '((t (:inherit lambda-line :foreground "red")))
  "Modeline face for active MODIFIED element."
  :group 'lambda-line-active)

(defface lambda-line-inactive-status-MD
  '((t (:inherit lambda-line-inactive :foreground "light gray")))
  "Modeline face for inactive MODIFIED element."
  :group 'lambda-line-inactive)

;; Evil state faces. Defaults inherit the status faces and set only a
;; foreground, so they blend with any theme; customize or restyle these
;; -- add a :background (and a same-color :box to sit flush inside a
;; boxed header-line) -- to get a colored-block modal indicator. A theme
;; such as lambda-themes can remap these to its own palette.
(defface lambda-line-evil-normal
  '((t (:inherit lambda-line-active-status-RW :foreground "green")))
  "Face for the Evil normal-state tag in the status-line."
  :group 'lambda-line-active)

(defface lambda-line-evil-insert
  '((t (:inherit lambda-line-active-status-MD :foreground "red")))
  "Face for the Evil insert-state tag in the status-line."
  :group 'lambda-line-active)

(defface lambda-line-evil-visual
  '((t (:inherit lambda-line-active-status-RW :foreground "orange")))
  "Face for the Evil visual-state tag in the status-line."
  :group 'lambda-line-active)

(defface lambda-line-evil-visual-line
  '((t (:inherit lambda-line-evil-visual)))
  "Face for the Evil visual-line-state tag in the status-line."
  :group 'lambda-line-active)

(defface lambda-line-evil-visual-block
  '((t (:inherit lambda-line-evil-visual)))
  "Face for the Evil visual-block-state tag in the status-line."
  :group 'lambda-line-active)

(defface lambda-line-evil-replace
  '((t (:inherit lambda-line-active-status-MD :foreground "orange")))
  "Face for the Evil replace-state tag in the status-line."
  :group 'lambda-line-active)

(defface lambda-line-evil-emacs
  '((t (:inherit lambda-line-active-status-RW :foreground "deep sky blue")))
  "Face for the Evil emacs-state tag in the status-line."
  :group 'lambda-line-active)

(defface lambda-line-evil-motion
  '((t (:inherit lambda-line-active-status-RW :foreground "yellow")))
  "Face for the Evil motion-state tag in the status-line."
  :group 'lambda-line-active)

(defface lambda-line-evil-operator
  '((t (:inherit lambda-line-active-status-RW :foreground "yellow")))
  "Face for the Evil operator-state tag in the status-line."
  :group 'lambda-line-active)

(defun lambda-line--apply-status-face (face)
  "Apply FACE with optional inverse video based on lambda-line-status-invert."
  (if lambda-line-status-invert
      `(,face :inverse-video t)
    face))


;;;;; Bell Faces

(defface lambda-line-visual-bell '((t (:background "red3")))
  "Face to use for the mode-line when `lambda-line-visual-bell-config' is used."
  :group 'lambda-line)

;;;; Setup Functions

;;;;; Visual bell for mode line

;; See https://github.com/hlissner/emacs-doom-themes for the basic idea

(defun lambda-line-visual-bell-fn ()
  "Blink the status-line red briefly. Set `ring-bell-function' to this to use it."
  (let ((lambda-line--bell-cookie (if (eq lambda-line-position 'bottom)
                                      (face-remap-add-relative 'lambda-line 'lambda-line-visual-bell)
                                    (face-remap-add-relative 'header-line 'lambda-line-visual-bell))))
    (force-mode-line-update t)
    (run-with-timer 0.15 nil
                    (lambda (cookie buf)
                      (with-current-buffer buf
                        (face-remap-remove-relative cookie)
                        (force-mode-line-update t)))
                    lambda-line--bell-cookie
                    (current-buffer))))

(defvar lambda-line--saved-ring-bell-function nil
  "Value of `ring-bell-function' before `lambda-line-visual-bell-config'.")
(defvar lambda-line--saved-visible-bell nil
  "Value of `visible-bell' before `lambda-line-visual-bell-config'.")

(defun lambda-line-visual-bell-config ()
  "Enable flashing the status-line on error."
  (unless (eq ring-bell-function #'lambda-line-visual-bell-fn)
    (setq lambda-line--saved-ring-bell-function ring-bell-function
          lambda-line--saved-visible-bell visible-bell))
  (setq ring-bell-function #'lambda-line-visual-bell-fn
        visible-bell t))

;;;;; Abbreviate Major-Mode
;; Source: https://www.masteringemacs.org/article/hiding-replacing-modeline-strings

(defun lambda-line--abbrev ()
  (cl-loop for elt in lambda-line-mode-formats
           do (let* ((mode (car elt))
                     (config (cdr elt))
                     (mode-str (plist-get config :abbrev))
                     (old-mode-str (when mode-str (cdr (assq mode minor-mode-alist)))))
                (when mode-str
                  (when old-mode-str
                    (setcar old-mode-str mode-str))
                  ;; major mode
                  (when (eq mode major-mode)
                    (setq mode-name mode-str))))))

;; Set abbrev (default is nil)
(when lambda-line-abbrev
  (add-hook 'after-change-major-mode-hook #'lambda-line--abbrev))

;;;;; Mode Name
(defun lambda-line-user-mode-p ()
  "Should the user supplied mode be called for modeline?"
  lambda-line-user-mode)

(defun lambda-line-mode-name ()
  "Return current major mode name."
  (format-mode-line mode-name))

;;;;; String Truncate
(defun lambda-line-truncate (str size &optional ellipsis)
  "If STR is longer than SIZE, truncate it and add ELLIPSIS."

  (let ((ellipsis (or ellipsis "…")))
    (if (> (length str) size)
        (format "%s%s" (substring str 0 (- size (length ellipsis))) ellipsis)
      str)))

(defun lambda-line--padding (pref)
  "Return padding for the prefix."
  (cond ((stringp pref) pref)
        (pref " ")
        (t "")))

;;;;; Get mode-formats config
(defun lambda-line--mode-format-config (cfg &optional exact)
  "Retrieve a config from the mode's mode-format; derived mode unless EXACT."
  (let ((found
          (cl-find-if
            (lambda (elt)
              (let ((mode (car elt))
                     (config (cdr elt)))
                (and (if exact (eq mode major-mode) (derived-mode-p mode)) (plist-member config cfg))))
            lambda-line-mode-formats)))
    (when found
      (plist-get (cdr found) cfg))))

;;;;; Performance Caching
;; -------------------------------------------------------------------
(defvar-local lambda-line--cache-project-name nil
  "Cached project name for current buffer.")
(defvar-local lambda-line--cache-vc-backend nil
  "Cached VC backend for current buffer.")
(defvar-local lambda-line--cache-vc-root 'unset
  "Cached repository root for current buffer.
The symbol `unset' marks a root that has not been looked up yet, so
that a buffer outside any repository caches its nil answer too.")
(defvar-local lambda-line--cache-git-diff nil
  "Cached git diff information for current buffer.")
(defvar-local lambda-line--cache-word-count nil
  "Cached word count for current buffer.")
(defvar-local lambda-line--cache-word-count-tick nil
  "Buffer modification tick when word count was cached.")
(defvar-local lambda-line--cache-timestamp nil
  "Timestamp of last cache update.")

(defcustom lambda-line-cache-duration 2.0
  "Duration in seconds to cache expensive operations."
  :type 'float
  :group 'lambda-line)

(defun lambda-line--cache-expired-p ()
  "Return t if cache has expired."
  (or (null lambda-line--cache-timestamp)
      (> (float-time (time-since lambda-line--cache-timestamp))
         lambda-line-cache-duration)))

(defun lambda-line--invalidate-cache ()
  "Invalidate all cached values."
  (setq lambda-line--cache-project-name nil
        lambda-line--cache-vc-backend nil
        lambda-line--cache-vc-root 'unset
        lambda-line--cache-git-diff nil
        lambda-line--cache-word-count nil
        lambda-line--cache-word-count-tick nil
        lambda-line--cache-timestamp nil))

(defun lambda-line--update-cache-timestamp ()
  "Update cache timestamp."
  (setq lambda-line--cache-timestamp (current-time)))

;; Cache invalidation hooks
(add-hook 'after-save-hook #'lambda-line--invalidate-cache)
(add-hook 'after-revert-hook #'lambda-line--invalidate-cache)
(add-hook 'find-file-hook #'lambda-line--invalidate-cache)

;;;;; Version Control
;; -------------------------------------------------------------------

;;;;; Refreshing version control state
;; The branch, the state indicator, and the diff counts the status-line
;; displays all live in `vc-mode', which Emacs recomputes only from
;; `find-file-hook' and from explicit `vc-refresh-state' calls.  Nothing
;; recomputes it after a commit, a stage, or a checkout made elsewhere, and
;; Magit does not do so either, so the status-line goes on reporting the
;; state as it was.  lambda-line counts the changes it is told about per
;; repository, refreshes the buffers that are on display at once, and lets
;; the rest catch up when a window next shows them.

(defvar lambda-line--vc-changes (make-hash-table :test 'equal)
  "Map each repository root to a count of the changes seen there.")

(defvar lambda-line--vc-revisions (make-hash-table :test 'equal)
  "Map each repository root to the revision last observed there.")

(defvar-local lambda-line--vc-change-seen nil
  "Cons of (ROOT . COUNT) recorded at this buffer's last VC refresh.")

(defun lambda-line--vc-root (&optional directory)
  "Return the repository root of DIRECTORY, or nil.
DIRECTORY defaults to `default-directory'.  Unlike `vc-root-dir' this
also answers in buffers that are not themselves under version control,
such as a Magit status buffer.  The result is cached per buffer, the
nil answer of a buffer outside any repository along with the rest."
  (when (eq lambda-line--cache-vc-root 'unset)
    (setq lambda-line--cache-vc-root
          (with-demoted-errors "lambda-line VC root error: %S"
            (let* ((directory (or directory default-directory))
                   (backend (vc-responsible-backend directory t))
                   (root (and backend
                              (vc-call-backend backend 'root directory))))
              (and root (expand-file-name root))))))
  lambda-line--cache-vc-root)

(defun lambda-line--vc-note-refreshed ()
  "Record this buffer's repository as refreshed at its current count."
  (let ((root (lambda-line--vc-root)))
    (when root
      (setq lambda-line--vc-change-seen
            (cons root (gethash root lambda-line--vc-changes 0))))))

(defun lambda-line--vc-refresh-state ()
  "Recompute the version control state of the current buffer.
Drop the caches first so that the branch and the diff counts are both
rebuilt rather than served from the value that went stale with them."
  (lambda-line--invalidate-cache)
  (with-demoted-errors "lambda-line VC refresh error: %S"
    (vc-refresh-state))
  (lambda-line--vc-note-refreshed))

(defun lambda-line--vc-refresh-if-stale ()
  "Refresh the current buffer when its repository changed since last seen."
  (when (and lambda-line-vc-refresh-on-repo-change buffer-file-name)
    (let ((root (lambda-line--vc-root)))
      (when (and root
                 (not (equal lambda-line--vc-change-seen
                             (cons root (gethash root
                                                 lambda-line--vc-changes 0)))))
        (lambda-line--vc-refresh-state)))))

(defun lambda-line--vc-refresh-frame (frame)
  "Refresh any stale buffer displayed on FRAME.
Added to `window-buffer-change-functions', whose default value is
called with a frame once per redisplay in which one of its windows
changed buffers."
  (when (frame-live-p frame)
    (dolist (window (window-list frame 'no-minibuf))
      (with-current-buffer (window-buffer window)
        (lambda-line--vc-refresh-if-stale)))))

(defun lambda-line--vc-revision (root)
  "Return the revision checked out at ROOT, or nil.
Read from the backend rather than from `vc-working-revision', whose
answer is the cached one that has just gone stale."
  (with-demoted-errors "lambda-line VC revision error: %S"
    (let* ((default-directory root)
           (backend (vc-responsible-backend root t))
           (process-file-side-effects nil))
      (and backend (vc-call-backend backend 'working-revision root)))))

(defun lambda-line--vc-repo-changed (&rest _)
  "Note a change to the current repository and refresh what is on display.
Buffers of the repository that no window shows are refreshed when one
next does, so that the cost stays proportional to what is visible."
  (when lambda-line-vc-refresh-on-repo-change
    (let ((root (lambda-line--vc-root)))
      (when root
        (puthash root (lambda-line--vc-revision root)
                 lambda-line--vc-revisions)
        (puthash root (1+ (gethash root lambda-line--vc-changes 0))
                 lambda-line--vc-changes)
        (mapc #'lambda-line--vc-refresh-frame (frame-list))))))

(defun lambda-line--vc-repo-maybe-changed (&rest _)
  "Refresh the current repository only when its revision has moved.
Magit refreshes after every command it runs, most of which leave the
checked-out revision alone.  One call to Git to compare revisions is
cheaper than recomputing the state of every buffer on display."
  (when lambda-line-vc-refresh-on-repo-change
    (let ((root (lambda-line--vc-root)))
      (when root
        (let ((revision (lambda-line--vc-revision root)))
          (unless (equal revision
                         (gethash root lambda-line--vc-revisions 'unknown))
            (lambda-line--vc-repo-changed)))))))

(defvar lambda-line--vc-change-hooks
  ;; Each entry is (HOOK FUNCTION FEATURE), where FEATURE is the library
  ;; that defines HOOK.
  '(;; Staging and unstaging change what `git diff' reports without
    ;; moving the revision, so these refresh unconditionally.
    (magit-post-stage-hook lambda-line--vc-repo-changed magit-apply)
    (magit-post-unstage-hook lambda-line--vc-repo-changed magit-apply)
    (vc-checkin-hook lambda-line--vc-repo-maybe-changed vc)
    ;; Magit runs this after an ordinary commit, one whose message was
    ;; written in a buffer.  `magit-post-commit-hook' covers only the
    ;; commands that need no message, such as `magit-commit-extend' and
    ;; `magit-commit-fixup'.
    (git-commit-post-finish-hook lambda-line--vc-repo-maybe-changed git-commit)
    (magit-post-commit-hook lambda-line--vc-repo-maybe-changed magit-commit)
    ;; The catch-all for everything else Magit does that moves the
    ;; revision: checkout, reset, pull, rebase, and stash.
    (magit-post-refresh-hook lambda-line--vc-repo-maybe-changed magit-mode))
  "Hooks that tell lambda-line a repository has changed.
Each entry is (HOOK FUNCTION FEATURE), where FEATURE is the library
that defines HOOK.")

(defun lambda-line--vc-setup-hooks (enable)
  "Add or remove the hooks that keep version control state current.
With ENABLE non-nil add them, otherwise remove them."
  (pcase-dolist (`(,hook ,fn ,feature) lambda-line--vc-change-hooks)
    ;; Wait for the library that owns the hook.  These hooks are
    ;; `defcustom's, and a `defcustom' keeps whatever value its variable
    ;; already holds, so touching one before its library loads leaves
    ;; that library's own default value unset for good.  Magit puts its
    ;; auto-revert in the default value of `magit-post-refresh-hook'.
    ;; `remove-hook' needs the same guard as `add-hook': it binds an
    ;; unbound hook to nil rather than letting it be.
    (let ((hook hook) (fn fn))
      (cond
       ((not enable) (when (boundp hook) (remove-hook hook fn)))
       ((boundp hook) (add-hook hook fn))
       (t (with-eval-after-load feature
            (when (and (bound-and-true-p lambda-line-mode) (boundp hook))
              (add-hook hook fn)))))))
  (if enable
      (progn
        (add-hook 'find-file-hook #'lambda-line--vc-note-refreshed)
        (add-hook 'window-buffer-change-functions
                  #'lambda-line--vc-refresh-frame))
    (remove-hook 'find-file-hook #'lambda-line--vc-note-refreshed)
    (remove-hook 'window-buffer-change-functions
                 #'lambda-line--vc-refresh-frame))
  ;; Saving changes the diff counts, and only a refresh recomputes them.
  ;; `lambda-line--vc-refresh-state' drops the caches first, so that the
  ;; counts do not come back from the value saving has just invalidated.
  (if (and enable lambda-line-git-diff-mode-line)
      (add-hook 'after-save-hook #'lambda-line--vc-refresh-state)
    (remove-hook 'after-save-hook #'lambda-line--vc-refresh-state)))

(defun lambda-line--vc-info ()
  "Return the version control information."
  (if lambda-line-default-vc-mode-function
    (funcall lambda-line-default-vc-mode-function)
    ""))

(defun lambda-line--prog-mode-info ()
  "Return the prog-mode information."
  (if lambda-line-prog-mode-info-function
    (funcall lambda-line-prog-mode-info-function)
    ""))

(defun lambda-line--lsp-status ()
  "Return LSP server status indicator."
  (when lambda-line-lsp-indicator
    (cond
     ;; LSP mode
     ((and (featurep 'lsp-mode) (bound-and-true-p lsp-mode))
      (when (lsp-workspaces)
        (propertize " LSP" 'face 'lambda-line-lsp-active)))
     ;; Eglot
     ((and (featurep 'eglot) (eglot-managed-p))
      (propertize " Eglot" 'face 'lambda-line-lsp-active))
     ;; LSP not active but available
     ((or (featurep 'lsp-mode) (featurep 'eglot))
      (propertize " LSP?" 'face 'lambda-line-lsp-available))
     (t ""))))

;;;;; Word Count
;; -------------------------------------------------------------------
(defun lambda-line--calculate-word-count ()
  "Calculate word count for current buffer using built-in count-words."
  (save-excursion
    (save-restriction
      (widen)
      (count-words (point-min) (point-max)))))

(defun lambda-line-word-count ()
  "Return formatted word count string with caching."
  (when (and lambda-line-word-count-enabled
             (memq major-mode lambda-line-word-count-modes))
    (let ((current-tick (buffer-chars-modified-tick)))
      ;; Check if cache is valid
      (unless (and lambda-line--cache-word-count
                   lambda-line--cache-word-count-tick
                   (= current-tick lambda-line--cache-word-count-tick))
        ;; Update cache
        (setq lambda-line--cache-word-count (lambda-line--calculate-word-count)
              lambda-line--cache-word-count-tick current-tick))
      ;; Return formatted string
      (when lambda-line--cache-word-count
        (concat
         " "  ; Leading space for separation
         (propertize lambda-line-word-count-symbol 
                     'face 'lambda-line-active-tertiary)
         (propertize (format "%d" lambda-line--cache-word-count)
                     'face 'lambda-line-active-tertiary)
         (propertize lambda-line-word-count-separator
                     'face 'lambda-line-active-tertiary))))))

;;;;; Branch display
;; -------------------------------------------------------------------
(defun lambda-line-project-name ()
  "Return name of project without path."
  (if (and lambda-line--cache-project-name
           (not (lambda-line--cache-expired-p)))
      lambda-line--cache-project-name
    (progn
      (lambda-line--update-cache-timestamp)
      (setq lambda-line--cache-project-name
            (file-name-nondirectory 
             (directory-file-name 
              (if (vc-root-dir) (vc-root-dir) "-")))))))

(defun lambda-line--colorize-vc-diff-counts (str)
  "Re-apply the diff faces to the +N/-N counts in STR.
The branch display strips text properties from `vc-mode' to drop VC's
own face, which also drops the faces `lambda-line--get-git-diff' put on
the added/removed line counts.  Match the trailing +N-N pattern and
restore `lambda-line-git-diff-added'/`lambda-line-git-diff-removed'.
The pattern requires the leading space that `lambda-line--get-git-diff'
emits so a branch name ending in a +N-N run is not mistaken for counts
\(git forbids spaces in ref names, so the space is unambiguous)."
  (if (and lambda-line-git-diff-mode-line
           (string-match " \\(\\+[0-9]+\\)\\(-[0-9]+\\)[ \t]*\\'" str))
      (let ((str (copy-sequence str)))
        (put-text-property (match-beginning 1) (match-end 1)
                           'face 'lambda-line-git-diff-added str)
        (put-text-property (match-beginning 2) (match-end 2)
                           'face 'lambda-line-git-diff-removed str)
        str)
    str))

(defun lambda-line-vc-project-branch ()
  "Show project and branch name for file.
Otherwise show '-'."
  (let ((backend (if (and lambda-line--cache-vc-backend
                          (not (lambda-line--cache-expired-p)))
                     lambda-line--cache-vc-backend
                   (progn
                     (lambda-line--update-cache-timestamp)
                     (setq lambda-line--cache-vc-backend 
                           (vc-backend buffer-file-name))))))
    (concat
     (if buffer-file-name
         (if vc-mode
             (let ((project-name (lambda-line-project-name)))
               ;; Project name
               (unless (string= "-" project-name)
                 (concat
                  ;; Divider
                  (propertize " •" 'face 'lambda-line-vc-divider)
                  (format " %s" project-name))))))

     ;; Show branch
     (if vc-mode
         (concat
          lambda-line-vc-symbol
          (lambda-line--colorize-vc-diff-counts
           (substring-no-properties vc-mode
                                    (+ (if (eq backend 'Hg) 2 3) 2))))
       nil))))

;;;;; Dir display
;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun lambda-line-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;;;;; Git diff in modeline
;; https://cocktailmake.github.io/posts/emacs-modeline-enhancement-for-git-diff/
(defun lambda-line--get-git-diff (file)
  "Get cached git diff information for FILE."
  (when (and lambda-line-git-diff-mode-line file)
    (if (and lambda-line--cache-git-diff
             (not (lambda-line--cache-expired-p)))
        lambda-line--cache-git-diff
      (progn
        (lambda-line--update-cache-timestamp)
        (setq lambda-line--cache-git-diff
              (let ((plus-minus (vc-git--run-command-string
                                 file "diff" "--numstat" "--")))
                (if (and plus-minus
                         (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
                    (concat
                     " "
                     (propertize (format "+%s" (match-string 1 plus-minus))
                                 'face 'lambda-line-git-diff-added)
                     (propertize (format "-%s" (match-string 2 plus-minus))
                                 'face 'lambda-line-git-diff-removed))
                  "")))))))

(define-advice vc-git-mode-line-string (:around (orig-fun file) lambda-line-git-diff)
  "Add git diff information to mode-line."
  (condition-case err
      (let ((result (funcall orig-fun file)))
        (if (and lambda-line-git-diff-mode-line
                 file
                 (stringp result))
            (concat result (lambda-line--get-git-diff file))
          result))
    (error
     ;; If there's an error, just return empty string to avoid breaking mode-line
     (message "lambda-line git diff error: %s" err)
     "")))

;;;;; Git Parse Repo Status
;; See https://kitchingroup.cheme.cmu.edu/blog/2014/09/19/A-git-status-Emacs-modeline/
(defun lambda-line-git-parse-status ()
  "Display the status of the repo."
  (interactive)
  (let ((U 0)   ; untracked files
        (M 0)   ; modified files
        (O 0)   ; other files
        (U-files "")
        (M-files "")
        (O-files ""))
    (dolist (line (split-string
                   (shell-command-to-string "git status --porcelain")
                   "\n"))
      (cond

       ;; ignore empty line at end
       ((string= "" line) nil)

       ((string-match "^\\?\\?" line)
        (setq U (+ 1 U))
        (setq U-files (concat U-files "\n" line)))

       ((string-match "^ M" line)
        (setq M (+ 1 M))
        (setq M-files (concat M-files "\n" line))
         )

       ((string-match "^M " line)
        (setq M (+ 1 M))
        (setq M-files (concat M-files "\n" line))
         )

       (t
        ;;(message "detected other in %s" line)
        (setq O (+ 1 O))
        (setq O-files (concat O-files "\n" line)))))
      
    ;; construct propertized string
    (concat
     (propertize
      (format "M%d" M)
      'face (if (> M 0)
                'error
              'success)
      'help-echo M-files)
     (propertize "|" 'face 'magit-dimmed)
     (propertize
      (format "U%d" U)
      'face (if (> U 0)
                'error
              'success)
      'help-echo U-files)
     (propertize "|" 'face 'magit-dimmed)
     (propertize
      (format "O%d" O)
      'face (if (> O 0)
                'warning
              'success)
       'help-echo O-files)
      " ")))

;;;;; Flycheck/Flymake Segment
(defvar-local lambda-line--flycheck-text nil)
(defun lambda-line--update-flycheck-segment (&optional status)
  "Update `lambda-line--flycheck-text' against the reported flycheck STATUS."
  (setq lambda-line--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat lambda-line-flycheck-label
                                                 (number-to-string sum)
                                                 " ")
                                         'face (if .error
                                                   'error
                                                 'warning))))
                       (propertize "Good " 'face 'success)))
          ('running (propertize "Checking " 'face 'flycheck-info))
          ('errored (propertize "Error " 'face 'error))
          ('interrupted (propertize "Paused " 'face 'fringe))
          ('no-checker ""))))

(defun lambda-line-check-syntax ()
  "Display syntax-checking information from flymake/flycheck in the mode-line (if available)."
  (if (and (>= emacs-major-version 28)
           (boundp 'flymake-mode)
           flymake-mode)
      (concat (format-mode-line flymake-mode-line-format) " ")
    lambda-line--flycheck-text))

(defun lambda-line-show-func ()
  "Display `which-function-mode' output in mode-line."
  (if (and (boundp 'which-function-mode)
       (default-value 'which-function-mode))
      (concat (format-mode-line which-func-format) " ")
    ""))

;;;;; Display-time-mode
(defun lambda-line-install-clockface-fonts ()
  "Install ClockFace fonts on the local system.

Thanks to the Doom Emacs project, for the basis of this
cross-platform font dowload/install code."
  (interactive)
  (let ((on-mac     (eq system-type 'darwin))
        (on-linux   (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
        (on-windows (memq system-type '(cygwin windows-nt ms-dos)))
        (name "ClockFace")
        (url-format "https://ocodo.github.io/ClockFace-font/%s")
        (fonts-list '("ClockFace-Regular.ttf"
                      "ClockFaceRect-Regular.ttf"
                      "ClockFaceSolid-Regular.ttf"
                      "ClockFaceRectSolid-Regular.ttf")))
    (unless (yes-or-no-p
             (format
              "Download%sthe ClockFace fonts, continue?"
              (if on-windows
                  " "
                " and install ")))
      (user-error "Aborted Download of ClockFace fonts"))
    (let* ((font-dest
            (cond (on-linux
                   (expand-file-name
                    "fonts/" (or (getenv "XDG_DATA_HOME")
                                 "~/.local/share")))
                  (on-mac
                   (expand-file-name "~/Library/Fonts/"))))
           (known-dest-p (stringp font-dest))
           (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))
      (unless (file-directory-p font-dest)
        (mkdir font-dest t))
      (dolist (font fonts-list)
        (url-copy-file (format url-format font)
                       (expand-file-name font font-dest)
                       t))
      (when known-dest-p
        (message "Font downloaded, updating font cache... Using <fc-cache -f -v> ")
        (shell-command-to-string "fc-cache -f -v"))
      (if on-windows
          (when (y-or-n-p "The %S font was downloaded, Windows users must install manually.\n\nOpen windows explorer?")
            (call-process "explorer.exe" nil nil nil font-dest))
        (message "Successfully %s %S fonts to %S!"
                 (if known-dest-p
                     "installed"
                   "downloaded")
                 name font-dest)))))

(defun lambda-line-clockface-select-font ()
  "Select clockface icon font."
  (interactive)
  (let ((font (completing-read
               "Select clockface icon font: "
               '("ClockFace"
                 "ClockFaceSolid"
                 "ClockFaceRect"
                 "ClockFaceRectSolid"))))
    (lambda-line-clockface-update-fontset font)))

(defun lambda-line-clockface-update-fontset (&optional font)
  "Use ClockFace font for unicode #xF0000..F008F.
Optionally use another clockface font."
  (set-fontset-font
   "fontset-default"
   (cons (decode-char 'ucs #xF0000)
         (decode-char 'ucs #xF008F))
   (or font "ClockFace")))

;; Usage example for testing
;; - exal each one after font installation to test.
;; (uses the complete font name now)
;;
;; [x] (lambda-line-clockface-update-fontset "ClockFace")
;; [x] (lambda-line-clockface-update-fontset "ClockFaceRect")
;; [x] (lambda-line-clockface-update-fontset "ClockFaceRectSolid")
;; [x] (lambda-line-clockface-update-fontset "ClockFaceSolid")

;; Need to add some note about Doom Emacs font-set modification for the user:
;;
;; E.g.
;;
;; Doom Emacs will reset fontset-default when fonts are resized
;; (e.g. after `doom/increase-font-size' or `doom/decrease-font-size')
;;
;; So it's necessary to use `lambda-line-clockface-update-fontset' after such events have
;; completed.
;;
;; (I haven't found a working solution, i.e. using the Doom hook `after-setting-font-hook' doesn't work.)

(defun lambda-line-clockface-icons-unicode (hours minutes)
  "Return ClockFace icon unicode for HOURS and MINUTES."
  (let* ((minute (- minutes (% minutes 5)))
         (offset (round (+ (* (% hours 12) 12) (* 12 (/ minute 60.0))))))
       (+ offset #xF0000)))

(defun lambda-line-time ()
  "Display the time when `display-time-mode' is non-nil.
When `lambda-line-icon-time' is non-nil, show a ClockFace icon;
otherwise show the time as text."
  (when display-time-mode
    (if lambda-line-icon-time
        (let ((time-unicode
               (cl-destructuring-bind (_ minute hour &rest n) (decode-time)
                 (lambda-line-clockface-icons-unicode hour minute))))
          (propertize
           (format lambda-line-time-icon-format (char-to-string time-unicode))
           'display '(raise 0)))
      (if display-time-day-and-date
          (propertize (format-time-string lambda-line-time-day-and-date-format))
        (propertize (format-time-string lambda-line-time-format)
                    'face '(:height 0.9))))))

;;;;; Status
(defun lambda-line-status ()
  "Return buffer status, one of 'read-only, 'modified or 'read-write."

  (let ((read-only  (when (not (lambda-line--mode-format-config :always-modifiable))
                      buffer-read-only))
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  'modified)
          (read-only 'read-only)
          (t         'read-write))))


;;;;; Evil State Indicator
(defconst lambda-line--evil-faces
  '((normal       . lambda-line-evil-normal)
    (insert       . lambda-line-evil-insert)
    (visual       . lambda-line-evil-visual)
    (visual-line  . lambda-line-evil-visual-line)
    (visual-block . lambda-line-evil-visual-block)
    (replace      . lambda-line-evil-replace)
    (emacs        . lambda-line-evil-emacs)
    (motion       . lambda-line-evil-motion)
    (operator     . lambda-line-evil-operator))
  "Alist mapping Evil state keys to their status-line faces.")

(defun lambda-line--evil-state-key ()
  "Return a key for the current Evil state, or nil.
Active only where `evil-local-mode' is on.  When the state is
`visual', the visual sub-type is read from `evil-visual-type' so
line-wise and block-wise selections resolve to `visual-line' and
`visual-block' respectively."
  (when (and (bound-and-true-p evil-local-mode)
             (bound-and-true-p evil-state))
    (if (and (eq evil-state 'visual)
             (bound-and-true-p evil-visual-type))
        (pcase evil-visual-type
          ('line  'visual-line)
          ('block 'visual-block)
          (_      'visual))
      evil-state)))

(defun lambda-line--evil-tag ()
  "Return the propertized Evil state tag, or nil.
Returns nil unless Evil is loaded, so a non-Evil session shows no tag
and no fixed-width placeholder.  Looks the resolved state key (see
`lambda-line--evil-state-key') up in `lambda-line-evil-state-alist' and
applies the matching face from `lambda-line--evil-faces'.  With no
matching tag, returns `lambda-line-evil-empty-tag' (unfaced) when
`lambda-line-evil-fixed-width' is non-nil, otherwise nil."
  (when (featurep 'evil)
    (let* ((key (lambda-line--evil-state-key))
           (tag (and key (cdr (assq key lambda-line-evil-state-alist)))))
      (cond
       (tag
        (propertize tag 'face (or (cdr (assq key lambda-line--evil-faces))
                                  'lambda-line-evil-normal)))
       (lambda-line-evil-fixed-width lambda-line-evil-empty-tag)))))

(defconst lambda-line--evil-state-entry-hooks
  '(evil-normal-state-entry-hook
    evil-insert-state-entry-hook
    evil-visual-state-entry-hook
    evil-replace-state-entry-hook
    evil-emacs-state-entry-hook
    evil-motion-state-entry-hook
    evil-operator-state-entry-hook)
  "Evil state-entry hooks that should trigger a status-line repaint.")

(defun lambda-line--evil-setup-hooks (enable)
  "Add or remove the Evil state-entry repaint hooks.
With ENABLE non-nil, add them when `lambda-line-evil-state' is enabled;
otherwise remove them unconditionally so they are never left behind
when the option is toggled off between activation and deactivation.  A
no-op when Evil is not loaded (the hooks do not yet exist)."
  (when (featurep 'evil)
    (dolist (hook lambda-line--evil-state-entry-hooks)
      (if (and enable lambda-line-evil-state)
          (add-hook hook #'force-mode-line-update)
        (remove-hook hook #'force-mode-line-update)))))

;;;;; Compose Status-Line
(defun lambda-line-compose (status name primary tertiary secondary &optional prefix)
  "Compose a string with provided information.
Each section is first defined, along with a measure of the width of the status-line.
STATUS, NAME, PRIMARY, and SECONDARY are always displayed. TERTIARY is displayed only in some modes."
  (let* ((window (get-buffer-window (current-buffer)))
         ;; Ensure all parameters are strings to prevent length calculation errors
         (name (or name ""))
         (primary (or primary ""))
         (tertiary (or tertiary ""))
         (secondary (or secondary ""))

         (name-max-width (max 12
                              (- (window-body-width)
                                 (round (* 0.8 (length primary)))
                                 (length tertiary)
                                 (length secondary))))

         (name (if (and (stringp name) (> (length name) name-max-width))
                   (format "…%s" (substring name (- (length name) name-max-width -1)))
                 name))

         (status (or status (lambda-line-status)))

         (active (eq window lambda-line--selected-window))

         ;; Is the current mode designated to have an explicit prefix symbol?
         (explicit-prefix (lambda-line--mode-format-config :prefix-symbol))
         ;; Semantic key for resolving a nerd-icons glyph, if any.
         (explicit-prefix-key (lambda-line--mode-format-config :prefix-key))

         (prefix (cond ((stringp prefix) prefix)
                       ((eq lambda-line-prefix nil) "")
                       ;; Prefer a nerd-icons glyph for the mode, else its
                       ;; default string prefix.
                       ((and explicit-prefix-key
                             (lambda-line--nerd-glyph explicit-prefix-key)))
                       ((stringp explicit-prefix) explicit-prefix)
                       (t
                        (cond ((eq status 'read-only)
                               (lambda-line--status-symbol 'read-only
                                                           lambda-line-gui-ro-symbol
                                                           lambda-line-tty-ro-symbol))
                              ((eq status 'read-write)
                               (lambda-line--status-symbol 'read-write
                                                           lambda-line-gui-rw-symbol
                                                           lambda-line-tty-rw-symbol))
                              ((eq status 'modified)
                               (lambda-line--status-symbol 'modified
                                                           lambda-line-gui-mod-symbol
                                                           lambda-line-tty-mod-symbol))
                              ((window-dedicated-p) (if (display-graphic-p) " ––" " --"))
                              ;; otherwise just use rw symbol
                              (t (lambda-line--status-symbol 'read-write
                                                             lambda-line-gui-rw-symbol
                                                             lambda-line-tty-rw-symbol))))))

         (face-modeline (if active
                            'lambda-line-active
                          'lambda-line-inactive))

         (explicit-face-prefix-active (lambda-line--mode-format-config :face-prefix-active))
         (explicit-face-prefix-inactive (lambda-line--mode-format-config :face-prefix-inactive))

         (face-prefix (if (not prefix) face-modeline
                        (if active
                            (cond ((eq status 'read-only)  (lambda-line--apply-status-face 'lambda-line-active-status-RO))
                                  ((eq status 'read-write) (lambda-line--apply-status-face 'lambda-line-active-status-RW))
                                  ((eq status 'modified)   (lambda-line--apply-status-face 'lambda-line-active-status-MD))
                                  (explicit-face-prefix-active explicit-face-prefix-active)
                                  (t                       'lambda-line-active))
                          (cond ((eq status 'read-only)  (lambda-line--apply-status-face 'lambda-line-inactive-status-RO))
                                ((eq status 'read-write) (lambda-line--apply-status-face 'lambda-line-inactive-status-RW))
                                ((eq status 'modified)   (lambda-line--apply-status-face 'lambda-line-inactive-status-MD))
                                (explicit-face-prefix-inactive explicit-face-prefix-inactive)
                                (t                       'lambda-line-inactive)))))
         (face-name (if active
                        'lambda-line-active-name
                      'lambda-line-inactive-name))
         (face-primary (if active
                           'lambda-line-active-primary
                         'lambda-line-inactive-primary))
         (face-secondary (if active
                             'lambda-line-active-secondary
                           'lambda-line-inactive-secondary))
         (face-tertiary (if active
                            'lambda-line-active-tertiary
                          'lambda-line-inactive-tertiary))
         ;; Optional Evil state segment, shown only when enabled and the
         ;; window is active.  The tag carries its own face and padding;
         ;; it is nil when absent, which `concat' below tolerates.
         (evil-tag (when (and lambda-line-evil-state active)
                     (lambda-line--evil-tag)))

         (left
          ;; special face for special mode prefixes
          (concat
           evil-tag
           (propertize (lambda-line--padding lambda-line-prefix-padding-left) 'face face-modeline)
           ;; Apply `face-prefix' as a *fallback* rather than a blanket
           ;; overwrite, so a nerd-icons glyph's own face (its :family, which
           ;; selects the Nerd Font) survives.  A plain `propertize' would
           ;; clobber it and the glyph would render as a missing-glyph box.
           ;; Plain string prefixes carry no face and simply receive
           ;; `face-prefix'.
           (let ((prefix (copy-sequence prefix)))
             (add-face-text-property 0 (length prefix) face-prefix t prefix)
             (put-text-property 0 (length prefix) 'display
                                `(raise ,lambda-line-symbol-position) prefix)
             prefix)
           ;; this matters for inverse-video!
           (propertize " " 'face face-prefix  'display `(raise ,lambda-line-space-top))

           (propertize (lambda-line--padding lambda-line-prefix-padding) 'face face-modeline)

           (propertize name 'face face-name)

           (propertize " "  'face (if active 'lambda-line-active
                                    'lambda-line-inactive)
                       'display `(raise ,lambda-line-space-bottom))

           ;; Apply the primary face as a *fallback* rather than a blanket
           ;; overwrite: `add-face-text-property' with APPEND leaves any face
           ;; already on individual characters (the git-diff counts, the
           ;; `•' divider, the LSP indicator) taking precedence for the
           ;; attributes they set, while unfaced characters still get
           ;; `face-primary'.  A plain `propertize' here would clobber them.
           (let ((primary (copy-sequence primary)))
             (add-face-text-property 0 (length primary) face-primary t primary)
             primary)))

          (tertiary (if (not (string-empty-p tertiary)) 
                       tertiary 
                     (if lambda-line-default-tertiary-function
                         (condition-case nil
                           (let ((result (funcall lambda-line-default-tertiary-function)))
                             (if (stringp result) result ""))
                           (error ""))
                       "")))

          (right (concat
                   (propertize tertiary 'face face-tertiary)
                   (propertize secondary 'face face-secondary)
                   (propertize lambda-line-hspace 'face face-modeline)))

          (right-len (length (format-mode-line right))))
    (concat
     left
     (propertize " " 'face face-modeline 'display `(space :align-to (- right ,right-len)))
     right)))

;;;; Mode Functions
;;;; Default display
(defun lambda-line-default-mode ()
  "Compose the default status line."
  (let ((buffer-name (format-mode-line (if buffer-file-name
                                           (file-name-nondirectory (buffer-file-name))
                                         "%b")))
        (mode-name   (lambda-line-mode-name))
        (vc-info     (lambda-line--vc-info))
        (position    (format-mode-line lambda-line-position-format)))
    (lambda-line-compose (lambda-line-status)
                         (lambda-line-truncate buffer-name lambda-line-truncate-value)
                         (concat lambda-line-display-group-start
                                 mode-name
                                 (when vc-info
                                   vc-info)
                                 lambda-line-display-group-end)
                         ""
                         ;; Narrowed buffer
                         (concat (if (buffer-narrowed-p)
                                     (concat
                                      (propertize "⇥ "  'face `(:inherit lambda-line-inactive-secondary))
                                      position " ")
                                   position)
                                 (lambda-line-time)))))

;;;;; Prog Mode
;; ---------------------------------------------------------------------
(defun lambda-line-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun lambda-line-prog-mode ()
  (let ((buffer-name (format-mode-line (if buffer-file-name (file-name-nondirectory (buffer-file-name)) "%b")))
        (mode-name   (lambda-line-mode-name))
        (vc-info     (lambda-line--vc-info))
        (prog-info   (lambda-line--prog-mode-info))
        (lsp-info    (lambda-line--lsp-status))
        (position    (format-mode-line lambda-line-position-format)))
    (lambda-line-compose (lambda-line-status)
                         (lambda-line-truncate buffer-name lambda-line-truncate-value)
                         (concat lambda-line-display-group-start mode-name
                                 (when vc-info vc-info)
                                 (when prog-info prog-info)
                                 (when lsp-info lsp-info)
                                 lambda-line-display-group-end)

                         (concat
                          (if lambda-line-which-func
                              (lambda-line-show-func) "")
                          (if lambda-line-syntax
                              (lambda-line-check-syntax) ""))

                         (concat
                          ;; Narrowed buffer
                          (when (buffer-narrowed-p)
                            (propertize "⇥ "  'face `(:inherit lambda-line-inactive-secondary)))
                          (if lambda-line-syntax
                              (if (or (boundp 'flycheck-mode)
                                      (boundp 'flymake-mode))
                                  (concat position lambda-line-hspace)
                                  position)
                            position)

                          (lambda-line-time)))))

(defun lambda-line-prog-activate ()
  "Setup flycheck hooks."
  (add-hook 'flycheck-status-changed-functions #'lambda-line--update-flycheck-segment)
  (add-hook 'flycheck-mode-hook #'lambda-line--update-flycheck-segment))

(defun lambda-line-prog-deactivate ()
  "Remove flycheck hooks."
  (remove-hook 'flycheck-status-changed-functions #'lambda-line--update-flycheck-segment)
  (remove-hook 'flycheck-mode-hook #'lambda-line--update-flycheck-segment))

;;;;; Fundamental Mode

(defun lambda-line-fundamental-mode-p ()
  (derived-mode-p 'fundamental-mode))

(defun lambda-line-fundamental-mode ()
  (lambda-line-default-mode))

;;;;; Text Mode

(defun lambda-line-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun lambda-line-text-mode ()
  (lambda-line-default-mode))

;;;;; Org Mode

(defun lambda-line-org-mode-p ()
  (derived-mode-p 'org-mode))

(defun lambda-line-org-mode ()
  (let ((buffer-name (format-mode-line (if buffer-file-name
                                           (file-name-nondirectory (buffer-file-name))
                                         "%b")))
        (mode-name   (lambda-line-mode-name))
        (vc-info     (lambda-line--vc-info))
        (word-count  (lambda-line-word-count))
        (position    (format-mode-line lambda-line-position-format)))
    (lambda-line-compose (lambda-line-status)
                         (lambda-line-truncate buffer-name lambda-line-truncate-value)
                         (concat lambda-line-display-group-start
                                 mode-name
                                 (when vc-info vc-info)
                                 lambda-line-display-group-end)
                         (or word-count "")
                         (concat (when (buffer-narrowed-p)
                                   (propertize "⇥ " 'face `(:inherit lambda-line-inactive-secondary)))
                                 position
                                 (lambda-line-time)))))

;;;;; Markdown Mode

(defun lambda-line-markdown-mode-p ()
  (derived-mode-p 'markdown-mode))

(defun lambda-line-markdown-mode ()
  ;; Same implementation as org-mode
  (lambda-line-org-mode))

;;;;; Help (& Helpful) Mode
(defun lambda-line-help-mode-p ()
  (derived-mode-p 'help-mode))

(defun lambda-line-helpful-mode-p ()
  (derived-mode-p 'helpful-mode))

(defun lambda-line-help-mode ()
  (lambda-line-compose "HELP"
                       (format-mode-line "%b")
                       ""
                       ""
                       (format-mode-line lambda-line-position-format)))


;;;;; Info Display
;; ---------------------------------------------------------------------
(defun lambda-line-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
        (node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
        line)
    (save-excursion
      (while  (> depth 0)
        (setq node (nth 1 (assoc node nodes)))
        (if node (push node crumbs))
        (setq depth (1- depth)))
      (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
                                   crumbs (cons nil crumbs))))
      (forward-line 1)
      (dolist (node crumbs)
        (let ((text
               (if (not (equal node "Top")) node
                 (format "%s"
                         (if (stringp Info-current-file)
                             (file-name-sans-extension
                              (file-name-nondirectory Info-current-file))
                           Info-current-file)))))
          (setq line (concat line (if (null line) "" " > ")
                             (if (null node) "..." text)))))
      (if (and cnode (not (equal cnode "Top")))
          (setq line (concat line (if (null line) "" " > ") cnode)))
      line)))

(defun lambda-line-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun lambda-line-info-mode ()
  (lambda-line-compose "INFO"
                       ""
                       (concat lambda-line-display-group-start
                               (lambda-line-info-breadcrumbs)
                               lambda-line-display-group-end)
                       ""
                       ""
                       ))

(defun lambda-line-info-activate ()
  (if (eq lambda-line-position 'top)
      (setq Info-use-header-line nil)))

(defun lambda-line-info-deactivate ()
  (custom-reevaluate-setting 'Info-use-header-line))

;;;; Term & Vterm
;; ---------------------------------------------------------------------
;; term
(defun lambda-line-term-mode-p ()
  (derived-mode-p 'term-mode))

;; vterm
(defun lambda-line-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun lambda-line-term-mode ()
  (lambda-line-compose " >_ "
                       "Terminal"
                       (concat lambda-line-display-group-start
                               (file-name-nondirectory shell-file-name)
                               lambda-line-display-group-end)
                       nil
                       (concat (lambda-line-shorten-directory default-directory 32)
                               (lambda-line-time))))


;; ---------------------------------------------------------------------

(defun lambda-line-get-ssh-host (_str)
  (let ((split-defdir (split-string default-directory)))
    (if (equal (length split-defdir) 1)
        (car (split-string (shell-command-to-string "hostname") "\n"))
      (cadr split-defdir))))

(defun lambda-line-ssh-mode ()
  (lambda-line-compose " >_ "
                       "Terminal"
                       (concat lambda-line-display-group-start
                               (lambda-line-get-ssh-host default-directory)
                               lambda-line-display-group-end)
                       nil
                       (concat (lambda-line-shorten-directory (car (last (split-string default-directory ":"))) 32)
                               (lambda-line-time))))

;;;; Eshell
;; ---------------------------------------------------------------------
(defun lambda-line-eshell-mode-p ()
  (derived-mode-p 'eshell-mode))

(defun lambda-line-eshell-mode ()
  (lambda-line-compose " >_ "
                       "Eshell"
                       (concat lambda-line-display-group-start
                               (buffer-name)
                               lambda-line-display-group-end)
                       ""
                       (concat (lambda-line-shorten-directory default-directory 32)
                               (lambda-line-time))))

(defun lambda-line-esh-activate ()
  (with-eval-after-load 'esh-mode
    (setq eshell-status-in-mode-line nil)))

(defun lambda-line-esh-deactivate ()
  (custom-reevaluate-setting 'eshell-status-in-mode-line))

;;;; Shell
;; ---------------------------------------------------------------------
(defun lambda-line-shell-mode-p ()
  (derived-mode-p 'shell-mode))

(defun lambda-line-shell-mode ()
  (lambda-line-compose " >_ "
                       "Shell"
                       (concat lambda-line-display-group-start
                               (buffer-name)
                               lambda-line-display-group-end)
                       ""
                       (concat (lambda-line-shorten-directory default-directory 32)
                               (lambda-line-time))))

;;;; Messages Buffer Mode
;; ---------------------------------------------------------------------
(defun lambda-line-messages-mode-p ()
  (derived-mode-p 'messages-buffer-mode))

(defun lambda-line-messages-mode ()
  (lambda-line-compose (lambda-line-status)
                       "*Messages*"
                       ""
                       ""
                       (concat "" (lambda-line-time))))

;;;; Message Mode
;; ---------------------------------------------------------------------
(defun lambda-line-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun lambda-line-message-mode ()
  (lambda-line-compose (lambda-line-status)
                       "Message" "(Draft)" nil (lambda-line-time)))

;;;; Docview Mode
;;---------------------------------------------------------------------
(defun lambda-line-doc-view-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun lambda-line-doc-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (lambda-line-mode-name))
        (vc-info     (lambda-line--vc-info))
        (page-number (concat
                          (number-to-string (doc-view-current-page)) "/"
                          (or (ignore-errors
                                    (number-to-string (doc-view-last-page-number)))
                              "???"))))
    (lambda-line-compose
     (lambda-line-status)
     buffer-name
     (concat lambda-line-display-group-start mode-name
             vc-info
             lambda-line-display-group-end)
     nil
     (concat page-number
             (lambda-line-time)))))

;;;; PDF View Mode
;; ---------------------------------------------------------------------
(defun lambda-line-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(with-eval-after-load 'pdf-tools
  (require 'pdf-view))

(defun lambda-line-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (lambda-line-mode-name))
        (page-number (concat
                      (number-to-string (eval `(pdf-view-current-page))) "/"
                      (or (ignore-errors
                            (number-to-string (pdf-cache-number-of-pages)))
                          "???"))))
    (lambda-line-compose (lambda-line-status)
                         buffer-name
                         (concat lambda-line-display-group-start mode-name
                                 lambda-line-display-group-end)
                         nil
                         (concat page-number " " (lambda-line-time)))))

;;;; MenuMode

(defun lambda-line-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun lambda-line-buffer-menu-mode ()
  (let ((buffer-name "Buffer list")
        (mode-name   (lambda-line-mode-name))
        (position    (format-mode-line lambda-line-position-format)))

    (lambda-line-compose (lambda-line-status)
                         buffer-name "" nil (concat position lambda-line-hspace (lambda-line-time)))))

;;;; Imenu-List
(defun lambda-line-imenu-list-mode-p ()
  (derived-mode-p 'imenu-list-major-mode))

(defun lambda-line-imenu-list-mode ()
  (let (
        ;; We take into account the case of narrowed buffers
        (buffer-name (buffer-name imenu-list--displayed-buffer))
        (vc-info     (lambda-line--vc-info))
        (position    (format-mode-line "%l:%c")))
    (lambda-line-compose (lambda-line-status)
                         buffer-name
                         "(imenu list)"
                         ""
                         "")))
;;;; Completion
;; ---------------------------------------------------------------------
(defun lambda-line-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun lambda-line-completion-list-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (lambda-line-mode-name))
        (position    (format-mode-line lambda-line-position-format)))

    (lambda-line-compose (lambda-line-status)
                         buffer-name "" nil (concat position lambda-line-hspace))))

;;;; Deft Mode

(with-eval-after-load 'deft
  (defun lambda-line--deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun lambda-line-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun lambda-line-deft-mode ()
  (let ((prefix " DEFT ")
        (primary "Search:")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (lambda-line-compose prefix primary filter nil matches)))

;;;; Calendar Mode
;; ---------------------------------------------------------------------
(defun lambda-line-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun lambda-line-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun lambda-line-calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default)))))

(defun lambda-line-calendar-activate ()
  (with-eval-after-load 'calendar
    (add-hook 'calendar-initial-window-hook
              #'lambda-line-calendar-setup-header)))

(defun lambda-line-calendar-deactivate ()
  (remove-hook 'calendar-initial-window-hook
               #'lambda-line-calendar-setup-header))

;;;; Org Capture
;; ---------------------------------------------------------------------
(defun lambda-line-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun lambda-line-org-capture-mode ()
  (lambda-line-compose (lambda-line-status)
                       "Capture"
                       (concat lambda-line-display-group-start
                               (org-capture-get :description)
                               lambda-line-display-group-end)
                       nil
                       "Finish: C-c C-c, refile: C-c C-w, cancel: C-c C-k "))


(defun lambda-line-org-capture-turn-off-header-line ()
  (setq-local header-line-format (default-value 'header-line-format))
  (message nil))

(defun lambda-line-org-capture-activate ()
  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook
              #'lambda-line-org-capture-turn-off-header-line)))

(defun lambda-line-org-capture-deactivate ()
  (remove-hook 'org-capture-mode-hook
               #'lambda-line-org-capture-turn-off-header-line))


;;;; Org Agenda
;; ---------------------------------------------------------------------
(defun lambda-line-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun lambda-line-org-agenda-mode ()
  (lambda-line-compose (lambda-line-status)
                       "Agenda"
                       (concat lambda-line-display-group-start (format "%S" org-agenda-current-span) lambda-line-display-group-end)
                       ""
                       (concat (format-time-string "%A, %d %B %Y")
                               ;; Clock icon only for users who opted in;
                               ;; the text time below is always shown.
                               (when lambda-line-icon-time (lambda-line-time))
                               (format-time-string " %H:%M"))))

;;;; Org Clock
;; ---------------------------------------------------------------------
(defun lambda-line-org-clock-mode-p ()
  (and (boundp 'org-mode-line-string)
       (stringp org-mode-line-string)))

(defun lambda-line-org-clock-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (lambda-line-mode-name))
        (vc-info     (lambda-line--vc-info))
        (position    (format-mode-line lambda-line-position-format)))
    (lambda-line-compose (lambda-line-status)
                         buffer-name
                         (concat lambda-line-display-group-start
                                 mode-name
                                 (when vc-info
                                   vc-info)
                                 lambda-line-display-group-end)
			 ""
                         (concat
                          ;; Narrowed buffer
                          (when (buffer-narrowed-p)
                            (propertize "⇥ "  'face `(:inherit lambda-line-inactive-secondary)))
                          org-mode-line-string
                          " "
                          position
                          lambda-line-hspace))))

(defun lambda-line-org-clock-out ()
  (setq org-mode-line-string nil)
  (force-mode-line-update))

(defun lambda-line-org-clock-activate ()
  (with-eval-after-load 'org-clock
    (add-hook 'org-clock-out-hook #'lambda-line-org-clock-out)))

(defun lambda-line-org-clock-deactivate ()
  (remove-hook 'org-clock-out-hook
               #'lambda-line-org-clock-out))

;;;; Elfeed
;; ---------------------------------------------------------------------
(defun lambda-line-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun lambda-line-elfeed-search-mode ()
  (let* ((status  "NEWS")
         (no-database (zerop (elfeed-db-last-update)))
         (update      (> (elfeed-queue-count-total) 0))

         (name  (cond (no-database "No database")
                      (update      "Update:")
                      (t           "Search:")))
         (primary (cond  (no-database "")
                         (update
                          (let ((total (elfeed-queue-count-total))
                                (in-process (elfeed-queue-count-active)))
                            (format "%d jobs pending, %d active"
                                    (- total in-process) in-process)))
                         (t  (let* ((db-time (seconds-to-time (elfeed-db-last-update)))
                                    (unread))
                               (cond (elfeed-search-filter-active "")
                                     ((string-match-p "[^ ]" elfeed-search-filter)
                                      elfeed-search-filter)
                                     (""))))))
         (secondary (concat
                     (cond
                      ((zerop (elfeed-db-last-update)) "")
                      ((> (elfeed-queue-count-total) 0) "")
                      (t (elfeed-search--count-unread)))
                     (lambda-line-time))))

    (lambda-line-compose status name primary nil secondary)))

;; Elfeed uses header-line, we need to tell it to use our own format
(defun lambda-line-elfeed-setup-header ()
  (setq header-line-format (default-value 'header-line-format)))

(defun lambda-line-elfeed-search-activate ()
  (with-eval-after-load 'elfeed
    (if (eq lambda-line-position 'top)
        (setq elfeed-search-header-function #'lambda-line-elfeed-setup-header))))

(defun lambda-line-elfeed-search-deactivate ()
  (if (boundp 'elfeed-search-header-function)
      (setq elfeed-search-header-function #'elfeed-search--header)))

;; ---------------------------------------------------------------------
(defun lambda-line-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun lambda-line-elfeed-show-mode ()
  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tags-str (mapconcat #'symbol-name tags ", "))
         (tag          (if tags
                           (concat lambda-line-display-group-start
                                   tags-str
                                   lambda-line-display-group-end)
                         " "))
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (entry-author (elfeed-meta elfeed-show-entry :author))
         (feed-title (if entry-author
                         (concat entry-author " (" (elfeed-feed-title feed) ")")
                       (elfeed-feed-title feed))))
    (lambda-line-compose
     ""
     (lambda-line-truncate title 65)
     tag
     ""
     (format-time-string "%Y-%m-%d %H:%M:%S" date))))


;;;; Mu4e

(defun lambda-line-mu4e-last-query ()
  "Get the most recent mu4e query or nil if there is none."
  (if (fboundp 'mu4e-last-query)
      (mu4e-last-query)
    mu4e~headers-last-query))

(defun lambda-line-mu4e-context ()
  "Return the current mu4e context as a non propertized string."
  (condition-case nil
    (let ((context (mu4e-context-current)))
      (if (and context (> (length (mu4e-context-name context)) 0))
          (concat
           lambda-line-display-group-start
           (substring-no-properties (mu4e-context-name context))
           lambda-line-display-group-end)
        "(none)"))
    (error "(none)")))

(defun lambda-line-mu4e-server-props ()
  "Encapsulates the call to the variable mu4e-/~server-props
depending on the version of mu4e."
  (if (version< mu4e-mu-version "1.6.0")
      mu4e~server-props
    mu4e--server-props))

(defun lambda-line-mu4e-activate ()
  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'lambda-line)))

(defun lambda-line-mu4e-deactivate ()
  (advice-remove #'mu4e~header-line-format #'lambda-line))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun lambda-line-mu4e-dashboard-mode ()
  (lambda-line-compose (lambda-line-status)
                       (condition-case nil
                         (format "%d messages"
                                 (or (plist-get (lambda-line-mu4e-server-props) :doccount) 0))
                         (error "0 messages"))
                       ""
                       ""
                       (lambda-line-time)))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-loading-mode-p ()
  (derived-mode-p 'mu4e-loading-mode))

(defun lambda-line-mu4e-loading-mode ()
  (lambda-line-compose (lambda-line-status)
                       (format-time-string "%A %d %B %Y, %H:%M ")
                       ""
                       "Loading..."
                       (lambda-line-mu4e-context)))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun lambda-line-mu4e-main-mode ()
  (lambda-line-compose (lambda-line-status)
                       (format-time-string "%A %d %B %Y, %H:%M ")
                       ""
                       ""
                       (lambda-line-mu4e-context)))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-compose-mode-p ()
  (derived-mode-p 'mu4e-compose-mode))

(defun lambda-line-mu4e-compose-mode ()
  (lambda-line-compose (lambda-line-status)
                       (or (ignore-errors (format-mode-line "%b")) 
                           (buffer-name) 
                           "Compose")
                       ""
                       ""
                       (condition-case nil
                         (let ((context (mu4e-context-current)))
                           (if context
                               (format "[%s] "
                                       (lambda-line-mu4e-quote
                                        (mu4e-context-name context)))
                             "[none] "))
                         (error "[none] "))))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-quote (str)
  (condition-case nil
    (if (version< mu4e-mu-version "1.8.0")
        (mu4e~quote-for-modeline str)
      (mu4e-quote-for-modeline str))
    (error (or str ""))))

(defun lambda-line-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun lambda-line-mu4e-headers-mode ()
  (let ((mu4e-modeline-max-width 80))
    (lambda-line-compose
     (lambda-line-status)
     "Search:"
     (or (lambda-line-mu4e-quote
          (lambda-line-mu4e-last-query)) "")
     ""
     (concat
      (condition-case nil
        (let ((context (mu4e-context-current)))
          (if context
              (format "[%s] "
                      (lambda-line-mu4e-quote
                       (mu4e-context-name context)))
            "[none] "))
        (error "[none] "))
      (or (lambda-line-time) "")))))

;; ---------------------------------------------------------------------
(defun lambda-line-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun lambda-line-mu4e-view-mode ()
  (condition-case nil
    (let* ((msg     (mu4e-message-at-point))
           (subject (and msg (mu4e-message-field msg :subject)))
           (from    (and msg (mu4e~headers-contact-str (mu4e-message-field msg :from))))
           (date    (and msg (mu4e-message-field msg :date))))
      (lambda-line-compose (lambda-line-status)
                           (or from "")
                           (concat lambda-line-display-group-start
                                   (lambda-line-truncate (or subject "") 50 "…")
                                   lambda-line-display-group-end)
                           ""
                           (concat (or (and date (format-time-string mu4e-headers-date-format date)) "") " ")))
    (error (lambda-line-compose (lambda-line-status) "Email" "" "" ""))))

;;;; Ein

(defun lambda-line-ein-notebook-mode ()
  (let ((buffer-name (format-mode-line "%b")))
    (lambda-line-compose (if (ein:notebook-modified-p) "MD" "RW")
                         buffer-name
                         ""
                         ""
                         (concat
                          (ein:header-line)
                          (lambda-line-time)))))

;; since the EIN library itself is constantly re-rendering the notebook, and thus
;; re-setting the header-line-format, we cannot use the lambda-line function to set
;; the header format in a notebook buffer. Fortunately, EIN exposes the
;; ein:header-line-format variable for just this purpose.

(defun lambda-line-ein-notebook-activate ()
  (with-eval-after-load 'ein
    (if (eq lambda-line-position 'top)
        (setq ein:header-line-format '((:eval (lambda-line-ein-notebook-mode)))))))

(defun lambda-line-ein-notebook-deactivate ()
  (if (boundp 'ein:header-line-format)
      (setq ein:header-line-format '(:eval (ein:header-line)))))


(defun lambda-line-buffer-menu-activate ()
  (if (eq lambda-line-position 'top)
      (setq Buffer-menu-use-header-line nil)))

(defun lambda-line-buffer-menu-deactivate ()
  (custom-reevaluate-setting 'Buffer-menu-use-header-line))

;;;; Elpher Mode
;; ---------------------------------------------------------------------
(defun lambda-line-elpher-mode-p ()
  (derived-mode-p 'elpher-mode))

(defun lambda-line-elpher-mode ()
  (let* ((display-string (elpher-page-display-string elpher-current-page))
         (sanitized-display-string (replace-regexp-in-string "%" "%%" display-string))
         (address (elpher-page-address elpher-current-page))
         (tls-string (if (and (not (elpher-address-about-p address))
                              (member (elpher-address-protocol address)
                                      '("gophers" "gemini")))
                         "(TLS encryption)"
                       "")))
    (lambda-line-compose nil
                         sanitized-display-string
                         tls-string
                         nil
                         (lambda-line-time))))

(defun lambda-line-elpher-activate ()
  (with-eval-after-load 'elpher
    (setq elpher-use-header nil)))

;;;; Ispell Mode
;; ---------------------------------------------------------------------
(defun lambda-line-enlarge-ispell-choices-buffer (buffer)
  (when (string= (buffer-name buffer) "*Choices*")
    (with-current-buffer buffer
      ;; (enlarge-window +2)
      (setq-local header-line-format nil)
      (setq-local mode-line-format nil))))

(defun lambda-line-ispell-activate ()
  (with-eval-after-load 'ispell
    (advice-add #'ispell-display-buffer :after
                #'lambda-line-enlarge-ispell-choices-buffer)))

(defun lambda-line-ispell-deactivate ()
  (advice-remove #'ispell-display-buffer
                 #'lambda-line-enlarge-ispell-choices-buffer))

;;;; Eldoc
;; ---------------------------------------------------------------------
;; `eldoc-minibuffer-message' changes `mode-line-format' but status-line when
;; `lambda-line-position' is `top' fails to display info. Solution is to move
;; eldoc messages to the minibuffer/echo area.
(when (eq lambda-line-position 'top)
  (setq eldoc-message-function #'message))

;;;; Magit
;; ---------------------------------------------------------------------
(defun lambda-line-magit-mode-p ()
  (derived-mode-p 'magit-mode))

;; Add functions to parse repo every N seconds
(defvar lambda-line-git-parse-last-update (float-time) "Last time we updated")
(defvar lambda-line-git-parse-update-interval 15 "Minimum time between update in seconds")
(defvar lambda-line-git-parse "" "Last value of the parse")

(defun lambda-line-magit-mode ()
  (let* ((buffer-name (format-mode-line
                       (if buffer-file-name
                           (file-name-nondirectory (buffer-file-name))
                         "%b")))
         (mode-name   (lambda-line-mode-name))
         (project     (file-name-nondirectory (directory-file-name (or (magit-toplevel) ""))))
         (branch      (or (magit-get-current-branch) ""))
         (status      (lambda-line-git-parse-status)))
    (lambda-line-compose (lambda-line-status)
                         mode-name
                         (concat lambda-line-display-group-start
                                 project
                                 lambda-line-vc-symbol
                                 branch
                                 lambda-line-display-group-end)
                         status
                         "")))


;;;; Setup Lambda-line
;; ---------------------------------------------------------------------
(defun lambda-line-face-clear (face)
  "Clear FACE"
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    'unspecified))

;; ---------------------------------------------------------------------
(defvar lambda-line--saved-mode-line-format nil)
(defvar lambda-line--saved-header-line-format nil)
(defvar lambda-line--selected-window nil)

(defun lambda-line--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq lambda-line--selected-window (selected-window)))

(defun lambda-line ()
  "Build and set the modeline."
  (let* ((format
          '((:eval
             (condition-case err
               (let* ((format-func (or (catch 'found
                                         (dolist (elt lambda-line-mode-formats)
                                           (let* ((config (cdr elt))
                                                  (mode-p (plist-get config :mode-p))
                                                  (format (plist-get config :format)))
                                             (when (and mode-p (functionp mode-p))
                                               (when (funcall mode-p)
                                                 (throw 'found format))))))
                                       lambda-line-default-mode-format))
                      (result (when (functionp format-func) (funcall format-func))))
                 (if (stringp result)
                     result
                   (format "lambda-line error: function %S returned %S (expected string)" format-func result)))
               (error (format "lambda-line error: %S" err)))))))
    (if (eq lambda-line-position 'top)
        (progn
          (setq header-line-format format)
          (setq-default header-line-format format))
      (progn
        (setq mode-line-format format)
        (setq-default mode-line-format format)))))

(defun lambda-line-update-windows ()
  "Hide the mode line depending on the presence of a window
below or a buffer local variable 'no-mode-line'."
  (dolist (window (window-list))
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        (if (or (not (boundp 'no-mode-line)) (not no-mode-line))
            (setq mode-line-format
                  (cond ((one-window-p t) (list ""))
                        ((eq (window-in-direction 'below) (minibuffer-window)) (list ""))
                        ((not (window-in-direction 'below)) (list ""))
                        (t nil))))))))

(defun lambda-line-mode--activate ()
  "Activate lambda-line."

  ;; Save current mode-line and header-line
  (unless lambda-line--saved-mode-line-format
    (setq lambda-line--saved-mode-line-format mode-line-format)
    (setq lambda-line--saved-header-line-format header-line-format))

  (dolist (elt lambda-line-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-activate)))
      (when fn (funcall fn))))

  (run-hooks 'lambda-line-mode-format-activate-hook)

  ;; Update selected window
  (lambda-line--update-selected-window)
  ;; (setq lambda-line--selected-window (selected-window))

  (setq         mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq         header-line-format nil)
  (setq-default header-line-format nil)

  (lambda-line)

  ;; Use lambda-line-visual-bell when var is set to t
  (when lambda-line-visual-bell
    (lambda-line-visual-bell-config))

  ;; This hooks is necessary to register selected window because when
  ;;  a modeline is evaluated, the corresponding window is always selected.
  (add-hook 'post-command-hook #'lambda-line--update-selected-window)

  ;; Repaint the status-line when the Evil state changes.  Evil may load
  ;; after lambda-line, so wire the hooks now when it is present and
  ;; defer until it loads otherwise.
  (if (featurep 'evil)
      (lambda-line--evil-setup-hooks t)
    (with-eval-after-load 'evil (lambda-line--evil-setup-hooks t)))

  ;; Keep the branch and the diff counts current when the repository
  ;; changes under a buffer that is already open.
  (lambda-line--vc-setup-hooks t)

  ;; This hooks hide the modeline for windows having a window below them
  ;; Disabled for the time being,
  ;;  -> see https://github.com/rougier/nano-modeline/issues/24
  ;; (add-hook 'window-configuration-change-hook #'lambda-line-update-windows)

  (force-mode-line-update t))

;; Deactivate status-line
(defun lambda-line-mode--deactivate ()
  "Deactivate lambda-line and restore default mode-line."

  (dolist (elt lambda-line-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-deactivate)))
      (when fn (funcall fn))))

  (run-hooks 'lambda-line-mode-format-deactivate-hook)

  (remove-hook 'post-command-hook
               #'lambda-line--update-selected-window)
  (remove-hook 'window-configuration-change-hook
               #'lambda-line-update-windows)

  (lambda-line--evil-setup-hooks nil)

  (lambda-line--vc-setup-hooks nil)

  ;; Restore the bell settings saved by `lambda-line-visual-bell-config',
  ;; leaving the `lambda-line-visual-bell' option itself untouched so the
  ;; bell comes back if the mode is re-enabled.
  (when (eq ring-bell-function #'lambda-line-visual-bell-fn)
    (setq ring-bell-function lambda-line--saved-ring-bell-function
          visible-bell lambda-line--saved-visible-bell))

  (setq         mode-line-format lambda-line--saved-mode-line-format)
  (setq-default mode-line-format lambda-line--saved-mode-line-format)
  (setq         header-line-format lambda-line--saved-header-line-format)
  (setq-default header-line-format lambda-line--saved-header-line-format))

;;;; Lambda-line minor mode

;; Store the default mode-line format
(defvar lambda-line--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode lambda-line-mode
  "Toggle lambda-line on or off."
  :group 'lambda-line
  :global t
  :lighter nil

  (if lambda-line-mode
      (lambda-line-mode--activate)
    (lambda-line-mode--deactivate))

  ;; Run any registered hooks
  (run-hooks 'lambda-line-mode-hook))

;;;; Interactive Commands

;;;###autoload
(defun lambda-line-toggle-word-count ()
  "Toggle word count display in lambda-line."
  (interactive)
  (setq lambda-line-word-count-enabled (not lambda-line-word-count-enabled))
  (lambda-line--invalidate-cache)
  (force-mode-line-update t)
  (message "Lambda-line word count %s" 
           (if lambda-line-word-count-enabled "enabled" "disabled")))

;;; Provide:
(provide 'lambda-line)

;;; lambda-line.el ends here
