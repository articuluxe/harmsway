;;; chyla-dark-theme.el --- Chyla.org - dark green color theme -*- lexical-binding: nil; -*-

;; Copyright (C) 2018-2024 Adam Chyła
;; Author: Adam Chyła <adam@chyla.org> https://chyla.org/
;; URL: https://github.com/chyla/ChylaDarkThemeForEmacs
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

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

;;; Commentary:

;; chyla.org - dark green color theme.

;;; Credits:

;; Philip Arvidsson created the GitHub theme file which this file is based on.
;; Bozhidar Batsov created the Zenburn theme file which GitHub theme file were based on.

;;; Code:

(deftheme chyla-dark "The chyla.org dark color theme.")

;;; Color Palette

(defvar chyla-dark-default-colors-alist
  '(("chyla-dark-border"                 . "#414141")
    ("chyla-dark-comment"                . "#858585")
    ("chyla-dark-constant"               . "#9cdcfe")
    ("chyla-dark-diff-added"             . "#507050")
    ("chyla-dark-diff-added-highlight"   . "#658b65")
    ("chyla-dark-diff-changed"           . "#77604d")
    ("chyla-dark-diff-changed-highlight" . "#9c836f")
    ("chyla-dark-diff-removed"           . "#521c1c")
    ("chyla-dark-diff-removed-highlight" . "#6a2323")
    ("chyla-dark-function"               . "#5ca1dc")
    ("chyla-dark-highlight"              . "#1e231e")
    ("chyla-dark-header-bg"              . "#407000")
    ("chyla-dark-header-fg"              . "#ffffff")
    ("chyla-dark-header-inactive-bg"     . "#051005")
    ("chyla-dark-html-tag"               . "#63a35c")
    ("chyla-dark-keyword"                . "#5d9b00")
    ("chyla-dark-selection"              . "#052905")
    ("chyla-dark-string"                 . "#5ca1dc")
    ("chyla-dark-text"                   . "#dddddd")
    ("chyla-dark-background"             . "#121212")
    ("chyla-dark-white"                  . "#fbfbfb")
    ("chyla-dark-red"                    . "#a32b2b")
    ("chyla-dark-green"                  . "#5d9b00")
    ("chyla-dark-yellow"                 . "#93a634")
    ("chyla-dark-blue"                   . "#9cdcfe")
    ("chyla-dark-magenta"                . "#7f3abc")
    ("chyla-dark-cyan"                   . "#25a8b3")
    )
  "List of chyla.org colors.
Each element has the form (NAME . HEX).")

(defvar chyla-dark-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar chyla-dark-colors-alist
  (append chyla-dark-default-colors-alist chyla-dark-override-colors-alist))

(defmacro chyla-dark-with-color-variables (&rest body)
  "`let' bind all colors defined in `chyla-dark-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   chyla-dark-colors-alist))
     ,@body))

;;; Theme Faces
(chyla-dark-with-color-variables
  (custom-theme-set-faces
   'chyla-dark
;;;; Built-in
;;;;; basic coloring
   '(fixed-pitch ((t nil)))
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,chyla-dark-keyword :underline t :weight bold))))
   `(link-visited ((t (:foreground ,chyla-dark-text :underline t :weight normal))))
   `(default ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background :family '("Ubuntu Mono" monospace)))))
   `(cursor ((t (:foreground ,chyla-dark-keyword :background ,chyla-dark-keyword))))
   `(escape-glyph ((t (:foreground ,chyla-dark-keyword :bold t))))
   `(fringe ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(header-line ((t (:foreground ,chyla-dark-keyword
                                  :background ,chyla-dark-selection
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,chyla-dark-highlight))))
   `(success ((t (:foreground ,chyla-dark-comment :weight bold))))
   `(warning ((t (:foreground ,chyla-dark-text :weight bold))))
   `(tooltip ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
;;;;; ansi-color
   `(ansi-color-black ((t (:foreground ,chyla-dark-text))))
   `(ansi-color-red ((t (:foreground ,chyla-dark-red))))
   `(ansi-color-green ((t (:foreground ,chyla-dark-green))))
   `(ansi-color-yellow ((t (:foreground ,chyla-dark-yellow))))
   `(ansi-color-blue ((t (:foreground ,chyla-dark-blue))))
   `(ansi-color-magenta ((t (:foreground ,chyla-dark-magenta))))
   `(ansi-color-cyan ((t (:foreground ,chyla-dark-cyan))))
   `(ansi-color-white ((t (:foreground ,chyla-dark-white))))
   '(ansi-default-fg-color ((t (:inherit ansi-color-white))))
   '(ansi-default-bg-color ((t (:inherit ansi-color-black))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,chyla-dark-keyword))))
   `(compilation-enter-directory-face ((t (:foreground ,chyla-dark-comment))))
   `(compilation-error-face ((t (:foreground ,chyla-dark-text :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,chyla-dark-text))))
   `(compilation-info-face ((t (:foreground ,chyla-dark-text))))
   `(compilation-info ((t (:foreground ,chyla-dark-constant :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,chyla-dark-comment))))
   `(compilation-line-face ((t (:foreground ,chyla-dark-keyword))))
   `(compilation-line-number ((t (:foreground ,chyla-dark-keyword))))
   `(compilation-message-face ((t (:foreground ,chyla-dark-text))))
   `(compilation-warning-face ((t (:foreground ,chyla-dark-text :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,chyla-dark-comment :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,chyla-dark-string :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,chyla-dark-keyword :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,chyla-dark-text))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,chyla-dark-text))))
   `(grep-error-face ((t (:foreground ,chyla-dark-text :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,chyla-dark-text))))
   `(grep-match-face ((t (:foreground ,chyla-dark-text :weight bold))))
   `(match ((t (:background ,chyla-dark-selection :foreground ,chyla-dark-text :weight bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,chyla-dark-white :weight bold :background ,chyla-dark-selection))))
   `(isearch-fail ((t (:foreground ,chyla-dark-border :background ,chyla-dark-background))))
   `(lazy-highlight ((t (:foreground ,chyla-dark-text :weight bold :background ,chyla-dark-highlight))))

   `(menu ((t (:foreground ,chyla-dark-white :background ,chyla-dark-header-bg))))
   `(minibuffer-prompt ((t (:foreground ,chyla-dark-keyword))))
   `(mode-line
     ((,class (:foreground ,chyla-dark-header-fg
                           :background ,chyla-dark-header-bg))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,chyla-dark-white :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,chyla-dark-comment
                      :background ,chyla-dark-header-inactive-bg
                      :box (:line-width -1 :color ,chyla-dark-header-inactive-bg)))))
   `(region ((,class (:background ,chyla-dark-selection))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,chyla-dark-background))))
   `(trailing-whitespace ((t (:background ,chyla-dark-diff-removed-highlight))))
   `(vertical-border ((t (:foreground ,chyla-dark-border))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,chyla-dark-keyword))))
   `(font-lock-comment-face ((t (:foreground ,chyla-dark-comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,chyla-dark-comment))))
   `(font-lock-constant-face ((t (:foreground ,chyla-dark-constant))))
   `(font-lock-doc-face ((t (:foreground ,chyla-dark-string))))
   `(font-lock-function-name-face ((t (:foreground ,chyla-dark-function))))
   `(font-lock-keyword-face ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,chyla-dark-keyword))))
   `(font-lock-preprocessor-face ((t (:foreground ,chyla-dark-keyword))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,chyla-dark-keyword))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,chyla-dark-comment))))
   `(font-lock-string-face ((t (:foreground ,chyla-dark-string))))
   `(font-lock-type-face ((t (:foreground ,chyla-dark-constant))))
   `(font-lock-variable-name-face ((t (:foreground ,chyla-dark-text))))
   `(font-lock-warning-face ((t (:foreground ,chyla-dark-text))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,chyla-dark-text))))
   `(newsticker-default-face ((t (:foreground ,chyla-dark-text))))
   `(newsticker-enclosure-face ((t (:foreground ,chyla-dark-html-tag))))
   `(newsticker-extra-face ((t (:foreground ,chyla-dark-white :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,chyla-dark-text))))
   `(newsticker-immortal-item-face ((t (:foreground ,chyla-dark-comment))))
   `(newsticker-new-item-face ((t (:foreground ,chyla-dark-text))))
   `(newsticker-obsolete-item-face ((t (:foreground ,chyla-dark-string))))
   `(newsticker-old-item-face ((t (:foreground ,chyla-dark-white))))
   `(newsticker-statistics-face ((t (:foreground ,chyla-dark-text))))
   `(newsticker-treeview-face ((t (:foreground ,chyla-dark-text))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,chyla-dark-comment))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,chyla-dark-text))))
   `(newsticker-treeview-new-face ((t (:foreground ,chyla-dark-text :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,chyla-dark-string))))
   `(newsticker-treeview-old-face ((t (:foreground ,chyla-dark-white))))
   `(newsticker-treeview-selection-face ((t (:background ,chyla-dark-selection :foreground ,chyla-dark-keyword))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background :inverse-video nil))))
   `(aw-leading-char-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-keyword :weight bold))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,chyla-dark-text))))
   `(android-mode-error-face ((t (:foreground ,chyla-dark-text :weight bold))))
   `(android-mode-info-face ((t (:foreground ,chyla-dark-text))))
   `(android-mode-verbose-face ((t (:foreground ,chyla-dark-comment))))
   `(android-mode-warning-face ((t (:foreground ,chyla-dark-keyword))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,chyla-dark-function :weight bold))))
   `(anzu-match-1 ((t (:foreground ,chyla-dark-white :background ,chyla-dark-comment))))
   `(anzu-match-2 ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(anzu-match-3 ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,chyla-dark-keyword))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,chyla-dark-string :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,chyla-dark-keyword))))
   `(font-latex-italic-face ((t (:foreground ,chyla-dark-function :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,chyla-dark-text))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,chyla-dark-string))))
   `(agda2-highlight-symbol-face ((t (:foreground ,chyla-dark-text))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,chyla-dark-constant))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,chyla-dark-text))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,chyla-dark-text))))
   `(agda2-highlight-datatype-face ((t (:foreground ,chyla-dark-text))))
   `(agda2-highlight-function-face ((t (:foreground ,chyla-dark-text))))
   `(agda2-highlight-module-face ((t (:foreground ,chyla-dark-constant))))
   `(agda2-highlight-error-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(agda2-highlight-typechecks-face ((t (:background ,chyla-dark-text))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,chyla-dark-background :foreground ,chyla-dark-text))))
   `(ac-completion-face ((t (:background ,chyla-dark-selection :foreground ,chyla-dark-text))))
   `(ac-selection-face ((t (:background ,chyla-dark-selection :foreground ,chyla-dark-text))))
   `(popup-tip-face ((t (:background ,chyla-dark-text :foreground ,chyla-dark-white))))
   `(popup-scroll-bar-foreground-face ((t (:background ,chyla-dark-text))))
   `(popup-scroll-bar-background-face ((t (:background ,chyla-dark-comment))))
   `(popup-isearch-match ((t (:background ,chyla-dark-background :foreground ,chyla-dark-text))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,chyla-dark-html-tag :background ,chyla-dark-background :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,chyla-dark-keyword :background ,chyla-dark-background :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,chyla-dark-function :background ,chyla-dark-background :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(company-tooltip-annotation ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(company-tooltip-annotation-selection ((t (:foreground ,chyla-dark-text :background ,chyla-dark-selection))))
   `(company-tooltip-selection ((t (:foreground ,chyla-dark-text :background ,chyla-dark-selection))))
   `(company-tooltip-mouse ((t (:background ,chyla-dark-selection))))
   `(company-tooltip-common ((t (:foreground ,chyla-dark-comment))))
   `(company-tooltip-common-selection ((t (:foreground ,chyla-dark-comment))))
   `(company-scrollbar-fg ((t (:background ,chyla-dark-text))))
   `(company-scrollbar-bg ((t (:background ,chyla-dark-background))))
   `(company-preview ((t (:background ,chyla-dark-comment))))
   `(company-preview-common ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-selection))))
;;;;; bm
   `(bm-face ((t (:background ,chyla-dark-text :foreground ,chyla-dark-white))))
   `(bm-fringe-face ((t (:background ,chyla-dark-text :foreground ,chyla-dark-white))))
   `(bm-fringe-persistent-face ((t (:background ,chyla-dark-comment :foreground ,chyla-dark-white))))
   `(bm-persistent-face ((t (:background ,chyla-dark-comment :foreground ,chyla-dark-white))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,chyla-dark-text :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,chyla-dark-text))))
   `(cider-deprecated-face ((t (:background ,chyla-dark-text))))
   `(cider-instrumented-face ((t (:box (:color ,chyla-dark-string :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,chyla-dark-function :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,chyla-dark-text))))
   `(cider-test-error-face ((t (:background ,chyla-dark-text))))
   `(cider-test-success-face ((t (:background ,chyla-dark-comment))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,chyla-dark-function))))
   `(circe-my-message-face ((t (:foreground ,chyla-dark-text))))
   `(circe-fool-face ((t (:foreground ,chyla-dark-text))))
   `(circe-topic-diff-removed-face ((t (:foreground ,chyla-dark-string :weight bold))))
   `(circe-originator-face ((t (:foreground ,chyla-dark-text))))
   `(circe-server-face ((t (:foreground ,chyla-dark-comment))))
   `(circe-topic-diff-new-face ((t (:foreground ,chyla-dark-text :weight bold))))
   `(circe-prompt-face ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,chyla-dark-text)))
   `(context-coloring-level-1-face ((t :foreground ,chyla-dark-function)))
   `(context-coloring-level-2-face ((t :foreground ,chyla-dark-constant)))
   `(context-coloring-level-3-face ((t :foreground ,chyla-dark-keyword)))
   `(context-coloring-level-4-face ((t :foreground ,chyla-dark-text)))
   `(context-coloring-level-5-face ((t :foreground ,chyla-dark-text)))
   `(context-coloring-level-6-face ((t :foreground ,chyla-dark-keyword)))
   `(context-coloring-level-7-face ((t :foreground ,chyla-dark-comment)))
   `(context-coloring-level-8-face ((t :foreground ,chyla-dark-text)))
   `(context-coloring-level-9-face ((t :foreground ,chyla-dark-text)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,chyla-dark-text :foreground ,chyla-dark-white))))
   `(ctbl:face-continue-bar ((t (:background ,chyla-dark-highlight :foreground ,chyla-dark-white))))
   `(ctbl:face-row-select ((t (:background ,chyla-dark-function :foreground ,chyla-dark-white))))
;;;;; diff
   `(diff-added          ((t (:background ,chyla-dark-diff-added :foreground ,chyla-dark-text))))
   `(diff-changed        ((t (:background ,chyla-dark-diff-changed :foreground ,chyla-dark-text))))
   `(diff-removed        ((t (:background ,chyla-dark-diff-removed :foreground ,chyla-dark-text))))
   `(diff-refine-added   ((t (:background ,chyla-dark-diff-added-highlight :foreground ,chyla-dark-text))))
   `(diff-refine-change  ((t (:background ,chyla-dark-diff-changed-highlight :foreground ,chyla-dark-text))))
   `(diff-refine-removed ((t (:background ,chyla-dark-diff-removed-highlight :foreground ,chyla-dark-text))))
   `(diff-header ((,class (:background ,chyla-dark-background))
                  (t (:background ,chyla-dark-text :foreground ,chyla-dark-white))))
   `(diff-file-header
     ((,class (:background ,chyla-dark-background :foreground ,chyla-dark-text :bold t))
      (t (:background ,chyla-dark-text :foreground ,chyla-dark-white :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,chyla-dark-text :background ,chyla-dark-diff-changed))))
   `(diff-hl-delete ((,class (:foreground ,chyla-dark-text :background ,chyla-dark-diff-removed))))
   `(diff-hl-insert ((,class (:foreground ,chyla-dark-text :background ,chyla-dark-diff-added))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,chyla-dark-white)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,chyla-dark-text))))
   `(diredp-compressed-file-suffix ((t (:foreground ,chyla-dark-text))))
   `(diredp-date-time ((t (:foreground ,chyla-dark-text))))
   `(diredp-deletion ((t (:foreground ,chyla-dark-keyword))))
   `(diredp-deletion-file-name ((t (:foreground ,chyla-dark-string))))
   `(diredp-dir-heading ((t (:foreground ,chyla-dark-text :background ,chyla-dark-selection))))
   `(diredp-dir-priv ((t (:foreground ,chyla-dark-function))))
   `(diredp-exec-priv ((t (:foreground ,chyla-dark-string))))
   `(diredp-executable-tag ((t (:foreground ,chyla-dark-text))))
   `(diredp-file-name ((t (:foreground ,chyla-dark-text))))
   `(diredp-file-suffix ((t (:foreground ,chyla-dark-comment))))
   `(diredp-flag-mark ((t (:foreground ,chyla-dark-keyword))))
   `(diredp-flag-mark-line ((t (:foreground ,chyla-dark-text))))
   `(diredp-ignored-file-name ((t (:foreground ,chyla-dark-string))))
   `(diredp-link-priv ((t (:foreground ,chyla-dark-keyword))))
   `(diredp-mode-line-flagged ((t (:foreground ,chyla-dark-keyword))))
   `(diredp-mode-line-marked ((t (:foreground ,chyla-dark-text))))
   `(diredp-no-priv ((t (:foreground ,chyla-dark-text))))
   `(diredp-number ((t (:foreground ,chyla-dark-text))))
   `(diredp-other-priv ((t (:foreground ,chyla-dark-text))))
   `(diredp-rare-priv ((t (:foreground ,chyla-dark-text))))
   `(diredp-read-priv ((t (:foreground ,chyla-dark-comment))))
   `(diredp-symlink ((t (:foreground ,chyla-dark-keyword))))
   `(diredp-write-priv ((t (:foreground ,chyla-dark-text))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,chyla-dark-string :weight bold))))
   `(dired-async-message ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,chyla-dark-keyword))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,chyla-dark-text :background ,chyla-dark-diff-removed))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,chyla-dark-text :background ,chyla-dark-text))))
   `(ediff-current-diff-B ((t (:foreground ,chyla-dark-text :background ,chyla-dark-diff-added))))
   `(ediff-current-diff-C ((t (:foreground ,chyla-dark-text :background ,chyla-dark-text))))
   `(ediff-even-diff-A ((t (:background ,chyla-dark-background))))
   `(ediff-even-diff-Ancestor ((t (:background ,chyla-dark-background))))
   `(ediff-even-diff-B ((t (:background ,chyla-dark-background))))
   `(ediff-even-diff-C ((t (:background ,chyla-dark-background))))
   `(ediff-fine-diff-A ((t (:foreground ,chyla-dark-text :background ,chyla-dark-diff-removed-highlight :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,chyla-dark-text :background ,chyla-dark-text weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,chyla-dark-text :background ,chyla-dark-diff-added-highlight :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,chyla-dark-text :background ,chyla-dark-text :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,chyla-dark-background))))
   `(ediff-odd-diff-Ancestor ((t (:background ,chyla-dark-background))))
   `(ediff-odd-diff-B ((t (:background ,chyla-dark-background))))
   `(ediff-odd-diff-C ((t (:background ,chyla-dark-background))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,chyla-dark-text))))
   `(egg-help-header-1 ((t (:foreground ,chyla-dark-keyword))))
   `(egg-help-header-2 ((t (:foreground ,chyla-dark-html-tag))))
   `(egg-branch ((t (:foreground ,chyla-dark-keyword))))
   `(egg-branch-mono ((t (:foreground ,chyla-dark-keyword))))
   `(egg-term ((t (:foreground ,chyla-dark-keyword))))
   `(egg-diff-add ((t (:foreground ,chyla-dark-constant))))
   `(egg-diff-del ((t (:foreground ,chyla-dark-text))))
   `(egg-diff-file-header ((t (:foreground ,chyla-dark-text))))
   `(egg-section-title ((t (:foreground ,chyla-dark-keyword))))
   `(egg-stash-mono ((t (:foreground ,chyla-dark-constant))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,chyla-dark-string))))
   `(elfeed-log-info-level-face ((t (:foreground ,chyla-dark-text))))
   `(elfeed-log-warn-level-face ((t (:foreground ,chyla-dark-keyword))))
   `(elfeed-search-date-face ((t (:foreground ,chyla-dark-text :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,chyla-dark-comment))))
   `(elfeed-search-feed-face ((t (:foreground ,chyla-dark-function))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,chyla-dark-keyword :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,chyla-dark-text
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,chyla-dark-text :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,chyla-dark-keyword
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background))))
   `(w3m-lnum-match ((t (:background ,chyla-dark-selection
                                     :foreground ,chyla-dark-text
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,chyla-dark-keyword))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,chyla-dark-text :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,chyla-dark-text))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,chyla-dark-keyword))))
   `(erc-keyword-face ((t (:foreground ,chyla-dark-text :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,chyla-dark-string :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,chyla-dark-comment))))
   `(erc-pal-face ((t (:foreground ,chyla-dark-text :weight bold))))
   `(erc-prompt-face ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,chyla-dark-constant))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,chyla-dark-constant :background ,chyla-dark-background))))
   `(ert-test-result-unexpected ((t (:foreground ,chyla-dark-string :background ,chyla-dark-background))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,chyla-dark-text :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,chyla-dark-text :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,chyla-dark-text))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,chyla-dark-function :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,chyla-dark-comment :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-text) :inherit unspecified))
      (t (:foreground ,chyla-dark-text :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-keyword) :inherit unspecified))
      (t (:foreground ,chyla-dark-keyword :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-function) :inherit unspecified))
      (t (:foreground ,chyla-dark-function :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,chyla-dark-text :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,chyla-dark-function :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-string)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,chyla-dark-text :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-text)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,chyla-dark-text :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-comment)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,chyla-dark-comment :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-text) :inherit unspecified))
      (t (:foreground ,chyla-dark-text :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-string) :inherit unspecified))
      (t (:foreground ,chyla-dark-text :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,chyla-dark-text))))
   `(ack-file ((t (:foreground ,chyla-dark-text))))
   `(ack-line ((t (:foreground ,chyla-dark-keyword))))
   `(ack-match ((t (:foreground ,chyla-dark-text :background ,chyla-dark-selection :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,chyla-dark-text :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,chyla-dark-keyword  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,chyla-dark-keyword  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,chyla-dark-constant :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(git-gutter:modified ((t (:foreground ,chyla-dark-string :weight bold))))
   `(git-gutter:unchanged ((t (:foreground ,chyla-dark-text :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,chyla-dark-comment  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,chyla-dark-string :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,chyla-dark-text :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, chyla-dark-text))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,chyla-dark-comment :weight bold))))
   `(gnus-server-denied ((t (:foreground ,chyla-dark-text :weight bold))))
   `(gnus-server-closed ((t (:foreground ,chyla-dark-text :slant italic))))
   `(gnus-server-offline ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(gnus-server-agent ((t (:foreground ,chyla-dark-text :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,chyla-dark-text))))
   `(gnus-summary-high-ancient ((t (:foreground ,chyla-dark-text))))
   `(gnus-summary-high-read ((t (:foreground ,chyla-dark-comment :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,chyla-dark-text :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,chyla-dark-text :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,chyla-dark-text))))
   `(gnus-summary-low-read ((t (:foreground ,chyla-dark-comment))))
   `(gnus-summary-low-ticked ((t (:foreground ,chyla-dark-text :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,chyla-dark-text))))
   `(gnus-summary-normal-ancient ((t (:foreground ,chyla-dark-text))))
   `(gnus-summary-normal-read ((t (:foreground ,chyla-dark-comment))))
   `(gnus-summary-normal-ticked ((t (:foreground ,chyla-dark-text :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,chyla-dark-text))))
   `(gnus-summary-selected ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,chyla-dark-text))))
   `(gnus-cite-10 ((t (:foreground ,chyla-dark-text))))
   `(gnus-cite-11 ((t (:foreground ,chyla-dark-keyword))))
   `(gnus-cite-2 ((t (:foreground ,chyla-dark-constant))))
   `(gnus-cite-3 ((t (:foreground ,chyla-dark-text))))
   `(gnus-cite-4 ((t (:foreground ,chyla-dark-comment))))
   `(gnus-cite-5 ((t (:foreground ,chyla-dark-text))))
   `(gnus-cite-6 ((t (:foreground ,chyla-dark-comment))))
   `(gnus-cite-7 ((t (:foreground ,chyla-dark-string))))
   `(gnus-cite-8 ((t (:foreground ,chyla-dark-text))))
   `(gnus-cite-9 ((t (:foreground ,chyla-dark-text))))
   `(gnus-group-news-1-empty ((t (:foreground ,chyla-dark-keyword))))
   `(gnus-group-news-2-empty ((t (:foreground ,chyla-dark-html-tag))))
   `(gnus-group-news-3-empty ((t (:foreground ,chyla-dark-text))))
   `(gnus-group-news-4-empty ((t (:foreground ,chyla-dark-text))))
   `(gnus-group-news-5-empty ((t (:foreground ,chyla-dark-text))))
   `(gnus-group-news-6-empty ((t (:foreground ,chyla-dark-white))))
   `(gnus-group-news-low-empty ((t (:foreground ,chyla-dark-white))))
   `(gnus-signature ((t (:foreground ,chyla-dark-keyword))))
   `(gnus-x ((t (:background ,chyla-dark-text :foreground ,chyla-dark-white))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,chyla-dark-text))))
   `(guide-key/key-face ((t (:foreground ,chyla-dark-comment))))
   `(guide-key/prefix-command-face ((t (:foreground ,chyla-dark-text))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,chyla-dark-comment
                      :background ,chyla-dark-background
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,chyla-dark-keyword
                      :background ,chyla-dark-selection
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,chyla-dark-highlight :underline nil))))
   `(helm-selection-line ((t (:background ,chyla-dark-background))))
   `(helm-visible-mark ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(helm-candidate-number ((t (:foreground ,chyla-dark-constant :background ,chyla-dark-selection))))
   `(helm-separator ((t (:foreground ,chyla-dark-string :background ,chyla-dark-background))))
   `(helm-time-zone-current ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background))))
   `(helm-time-zone-home ((t (:foreground ,chyla-dark-string :background ,chyla-dark-background))))
   `(helm-bookmark-addressbook ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(helm-bookmark-info ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background))))
   `(helm-bookmark-man ((t (:foreground ,chyla-dark-keyword :background ,chyla-dark-background))))
   `(helm-bookmark-w3m ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(helm-buffer-not-saved ((t (:foreground ,chyla-dark-string :background ,chyla-dark-background))))
   `(helm-buffer-process ((t (:foreground ,chyla-dark-function :background ,chyla-dark-background))))
   `(helm-buffer-saved-out ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(helm-buffer-size ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(helm-ff-directory ((t (:foreground ,chyla-dark-function :background ,chyla-dark-background :weight bold))))
   `(helm-ff-file ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background :weight normal))))
   `(helm-ff-executable ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,chyla-dark-string :background ,chyla-dark-background :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,chyla-dark-keyword :background ,chyla-dark-background :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,chyla-dark-white :background ,chyla-dark-keyword :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,chyla-dark-function :background ,chyla-dark-background))))
   `(helm-grep-file ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(helm-grep-finish ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background))))
   `(helm-grep-lineno ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,chyla-dark-string :background ,chyla-dark-background))))
   `(helm-match ((t (:foreground ,chyla-dark-text :background ,chyla-dark-selection :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,chyla-dark-function :background ,chyla-dark-background))))
   `(helm-mu-contacts-address-face ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(helm-mu-contacts-name-face ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(helm-swoop-target-word-face ((t (:foreground ,chyla-dark-keyword :background ,chyla-dark-background :weight bold))))
;;;;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,chyla-dark-constant))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,chyla-dark-highlight))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,chyla-dark-highlight)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,chyla-dark-background))
                   (t :weight bold)))
;;;;; hlinum
   `(linum-highlight-face ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-highlight))))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(hydra-face-amaranth ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(hydra-face-blue ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(hydra-face-pink ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(hydra-face-teal ((t (:foreground ,chyla-dark-function :background ,chyla-dark-background))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background))))
   `(ivy-match-required-face ((t (:foreground ,chyla-dark-string :background ,chyla-dark-background))))
   `(ivy-remote ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(ivy-subdir ((t (:foreground ,chyla-dark-keyword :background ,chyla-dark-background))))
   `(ivy-current-match ((t (:foreground ,chyla-dark-keyword :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,chyla-dark-background))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,chyla-dark-comment))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,chyla-dark-comment))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,chyla-dark-text))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(ido-only-match ((t (:foreground ,chyla-dark-text :weight bold))))
   `(ido-subdir ((t (:foreground ,chyla-dark-keyword))))
   `(ido-indicator ((t (:foreground ,chyla-dark-keyword :background ,chyla-dark-text))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,chyla-dark-background :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,chyla-dark-comment))))
   `(jabber-roster-user-online ((t (:foreground ,chyla-dark-constant))))
   `(jabber-roster-user-dnd ((t (:foreground ,chyla-dark-text))))
   `(jabber-roster-user-xa ((t (:foreground ,chyla-dark-text))))
   `(jabber-roster-user-chatty ((t (:foreground ,chyla-dark-text))))
   `(jabber-roster-user-error ((t (:foreground ,chyla-dark-text))))
   `(jabber-rare-time-face ((t (:foreground ,chyla-dark-text))))
   `(jabber-chat-prompt-local ((t (:foreground ,chyla-dark-constant))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,chyla-dark-text))))
   `(jabber-chat-prompt-system ((t (:foreground ,chyla-dark-html-tag))))
   `(jabber-activity-face((t (:foreground ,chyla-dark-text))))
   `(jabber-activity-personal-face ((t (:foreground ,chyla-dark-keyword))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,chyla-dark-text))))
   `(js2-error ((t (:foreground ,chyla-dark-string :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,chyla-dark-comment))))
   `(js2-jsdoc-type ((t (:foreground ,chyla-dark-comment))))
   `(js2-jsdoc-value ((t (:foreground ,chyla-dark-html-tag))))
   `(js2-function-param ((t (:foreground, chyla-dark-text))))
   `(js2-external-variable ((t (:foreground ,chyla-dark-text))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,chyla-dark-comment))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,chyla-dark-text))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,chyla-dark-text))))
   `(js2-object-property ((t (:foreground ,chyla-dark-keyword))))
   `(js2-magic-paren ((t (:foreground ,chyla-dark-text))))
   `(js2-private-function-call ((t (:foreground ,chyla-dark-function))))
   `(js2-function-call ((t (:foreground ,chyla-dark-function))))
   `(js2-private-member ((t (:foreground ,chyla-dark-constant))))
   `(js2-keywords ((t (:foreground ,chyla-dark-text))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,chyla-dark-text :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,chyla-dark-text :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,chyla-dark-background))))
   `(ledger-font-pending-face ((t (:foreground ,chyla-dark-text weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,chyla-dark-text))))
   `(ledger-font-posting-account-face ((t (:foreground ,chyla-dark-constant))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,chyla-dark-text))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,chyla-dark-text))))
   `(ledger-font-posting-amount-face ((t (:foreground ,chyla-dark-text))))
   `(ledger-occur-narrowed-face ((t (:foreground ,chyla-dark-text :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,chyla-dark-background))))
   `(ledger-font-comment-face ((t (:foreground ,chyla-dark-comment))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,chyla-dark-text :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,chyla-dark-text :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,chyla-dark-text :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,chyla-dark-text :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,chyla-dark-highlight :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,chyla-dark-keyword))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,chyla-dark-text))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,chyla-dark-keyword))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,chyla-dark-keyword :box t))))
   `(ruler-mode-default ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,chyla-dark-constant))))
   `(lui-hilight-face ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-selection))))
   `(macrostep-gensym-2
     ((t (:foreground ,chyla-dark-text :background ,chyla-dark-selection))))
   `(macrostep-gensym-3
     ((t (:foreground ,chyla-dark-keyword :background ,chyla-dark-selection))))
   `(macrostep-gensym-4
     ((t (:foreground ,chyla-dark-text :background ,chyla-dark-selection))))
   `(macrostep-gensym-5
     ((t (:foreground ,chyla-dark-keyword :background ,chyla-dark-selection))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,chyla-dark-background))))
   `(magit-section-heading             ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,chyla-dark-text :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,chyla-dark-background  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,chyla-dark-background
                                                        :foreground ,chyla-dark-text :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,chyla-dark-background))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,chyla-dark-background))))
   `(magit-diff-hunk-heading-selection ((t (:background ,chyla-dark-background
                                                        :foreground ,chyla-dark-text))))
   `(magit-diff-lines-heading          ((t (:background ,chyla-dark-text
                                                        :foreground ,chyla-dark-white))))
   `(magit-diff-context-highlight      ((t (:background ,chyla-dark-background
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,chyla-dark-constant))))
   `(magit-diffstat-removed ((t (:foreground ,chyla-dark-string))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,chyla-dark-keyword  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,chyla-dark-comment :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,chyla-dark-comment   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,chyla-dark-text    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,chyla-dark-text  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,chyla-dark-comment  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,chyla-dark-string    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,chyla-dark-text))))
   `(magit-log-date      ((t (:foreground ,chyla-dark-text))))
   `(magit-log-graph     ((t (:foreground ,chyla-dark-text))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,chyla-dark-text))))
   `(magit-sequence-stop ((t (:foreground ,chyla-dark-comment))))
   `(magit-sequence-part ((t (:foreground ,chyla-dark-keyword))))
   `(magit-sequence-head ((t (:foreground ,chyla-dark-text))))
   `(magit-sequence-drop ((t (:foreground ,chyla-dark-string))))
   `(magit-sequence-done ((t (:foreground ,chyla-dark-text))))
   `(magit-sequence-onto ((t (:foreground ,chyla-dark-text))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,chyla-dark-comment))))
   `(magit-bisect-skip ((t (:foreground ,chyla-dark-keyword))))
   `(magit-bisect-bad  ((t (:foreground ,chyla-dark-string))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,chyla-dark-selection :foreground ,chyla-dark-text))))
   `(magit-blame-hash    ((t (:background ,chyla-dark-selection :foreground ,chyla-dark-text))))
   `(magit-blame-name    ((t (:background ,chyla-dark-selection :foreground ,chyla-dark-text))))
   `(magit-blame-date    ((t (:background ,chyla-dark-selection :foreground ,chyla-dark-text))))
   `(magit-blame-summary ((t (:background ,chyla-dark-selection :foreground ,chyla-dark-text
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,chyla-dark-text))))
   `(magit-hash           ((t (:foreground ,chyla-dark-text))))
   `(magit-tag            ((t (:foreground ,chyla-dark-text :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,chyla-dark-comment  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,chyla-dark-text   :weight bold))))
   `(magit-branch-current ((t (:foreground ,chyla-dark-text   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,chyla-dark-text   :weight bold))))
   `(magit-refname        ((t (:background ,chyla-dark-background :foreground ,chyla-dark-text :weight bold))))
   `(magit-refname-stash  ((t (:background ,chyla-dark-background :foreground ,chyla-dark-text :weight bold))))
   `(magit-refname-wip    ((t (:background ,chyla-dark-background :foreground ,chyla-dark-text :weight bold))))
   `(magit-signature-good      ((t (:foreground ,chyla-dark-comment))))
   `(magit-signature-bad       ((t (:foreground ,chyla-dark-string))))
   `(magit-signature-untrusted ((t (:foreground ,chyla-dark-keyword))))
   `(magit-cherry-unmatched    ((t (:foreground ,chyla-dark-function))))
   `(magit-cherry-equivalent   ((t (:foreground ,chyla-dark-text))))
   `(magit-reflog-commit       ((t (:foreground ,chyla-dark-comment))))
   `(magit-reflog-amend        ((t (:foreground ,chyla-dark-text))))
   `(magit-reflog-merge        ((t (:foreground ,chyla-dark-comment))))
   `(magit-reflog-checkout     ((t (:foreground ,chyla-dark-text))))
   `(magit-reflog-reset        ((t (:foreground ,chyla-dark-string))))
   `(magit-reflog-rebase       ((t (:foreground ,chyla-dark-text))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,chyla-dark-comment))))
   `(magit-reflog-remote       ((t (:foreground ,chyla-dark-function))))
   `(magit-reflog-other        ((t (:foreground ,chyla-dark-function))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,chyla-dark-text))))
   `(message-header-other ((t (:foreground ,chyla-dark-comment))))
   `(message-header-to ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(message-header-cc ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(message-header-subject ((t (:foreground ,chyla-dark-text :weight bold))))
   `(message-header-xheader ((t (:foreground ,chyla-dark-comment))))
   `(message-mml ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,chyla-dark-text))))
   `(mew-face-header-from ((t (:foreground ,chyla-dark-keyword))))
   `(mew-face-header-date ((t (:foreground ,chyla-dark-comment))))
   `(mew-face-header-to ((t (:foreground ,chyla-dark-string))))
   `(mew-face-header-key ((t (:foreground ,chyla-dark-comment))))
   `(mew-face-header-private ((t (:foreground ,chyla-dark-comment))))
   `(mew-face-header-important ((t (:foreground ,chyla-dark-text))))
   `(mew-face-header-marginal ((t (:foreground ,chyla-dark-text :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,chyla-dark-string))))
   `(mew-face-header-xmew ((t (:foreground ,chyla-dark-comment))))
   `(mew-face-header-xmew-bad ((t (:foreground ,chyla-dark-string))))
   `(mew-face-body-url ((t (:foreground ,chyla-dark-text))))
   `(mew-face-body-comment ((t (:foreground ,chyla-dark-text :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,chyla-dark-comment))))
   `(mew-face-body-cite2 ((t (:foreground ,chyla-dark-text))))
   `(mew-face-body-cite3 ((t (:foreground ,chyla-dark-text))))
   `(mew-face-body-cite4 ((t (:foreground ,chyla-dark-keyword))))
   `(mew-face-body-cite5 ((t (:foreground ,chyla-dark-string))))
   `(mew-face-mark-review ((t (:foreground ,chyla-dark-text))))
   `(mew-face-mark-escape ((t (:foreground ,chyla-dark-comment))))
   `(mew-face-mark-delete ((t (:foreground ,chyla-dark-string))))
   `(mew-face-mark-unlink ((t (:foreground ,chyla-dark-keyword))))
   `(mew-face-mark-refile ((t (:foreground ,chyla-dark-comment))))
   `(mew-face-mark-unread ((t (:foreground ,chyla-dark-text))))
   `(mew-face-eof-message ((t (:foreground ,chyla-dark-comment))))
   `(mew-face-eof-part ((t (:foreground ,chyla-dark-keyword))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,chyla-dark-function :background ,chyla-dark-background :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text :weight bold))))
   `(paren-face-no-match ((t (:foreground ,chyla-dark-white :background ,chyla-dark-string :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,chyla-dark-text))))
   `(mingus-pausing-face ((t (:foreground ,chyla-dark-text))))
   `(mingus-playing-face ((t (:foreground ,chyla-dark-function))))
   `(mingus-playlist-face ((t (:foreground ,chyla-dark-function ))))
   `(mingus-song-file-face ((t (:foreground ,chyla-dark-keyword))))
   `(mingus-stopped-face ((t (:foreground ,chyla-dark-string))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,chyla-dark-keyword))))
   `(nav-face-button-num ((t (:foreground ,chyla-dark-function))))
   `(nav-face-dir ((t (:foreground ,chyla-dark-comment))))
   `(nav-face-hdir ((t (:foreground ,chyla-dark-string))))
   `(nav-face-file ((t (:foreground ,chyla-dark-text))))
   `(nav-face-hfile ((t (:foreground ,chyla-dark-text))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,chyla-dark-text    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,chyla-dark-comment :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,chyla-dark-text  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,chyla-dark-comment   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,chyla-dark-text  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,chyla-dark-comment :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,chyla-dark-text    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,chyla-dark-text))))
   `(mu4e-trashed-face ((t (:foreground ,chyla-dark-text :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,chyla-dark-selection))))
   `(mumamo-background-chunk-submode2 ((t (:background ,chyla-dark-background))))
   `(mumamo-background-chunk-submode3 ((t (:background ,chyla-dark-background))))
   `(mumamo-background-chunk-submode4 ((t (:background ,chyla-dark-background))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(neo-header-face ((t (:foreground ,chyla-dark-text))))
   `(neo-root-dir-face ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,chyla-dark-text))))
   `(neo-file-link-face ((t (:foreground ,chyla-dark-text))))
   `(neo-expand-btn-face ((t (:foreground ,chyla-dark-text))))
   `(neo-vc-default-face ((t (:foreground ,chyla-dark-text))))
   `(neo-vc-user-face ((t (:foreground ,chyla-dark-string :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,chyla-dark-text))))
   `(neo-vc-edited-face ((t (:foreground ,chyla-dark-text))))
   `(neo-vc-needs-merge-face ((t (:foreground ,chyla-dark-text))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,chyla-dark-string :background ,chyla-dark-text))))
   `(neo-vc-added-face ((t (:foreground ,chyla-dark-text))))
   `(neo-vc-conflict-face ((t (:foreground ,chyla-dark-text))))
   `(neo-vc-missing-face ((t (:foreground ,chyla-dark-text))))
   `(neo-vc-ignored-face ((t (:foreground ,chyla-dark-text))))
;;;;; org-mode
   `(org-agenda-clocking
     ((t (:bold t :background ,chyla-dark-highlight))) t)
   `(org-agenda-date-today
     ((t (:foreground ,chyla-dark-text :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,chyla-dark-text :weight bold))))
   `(org-checkbox ((t (:background ,chyla-dark-background :foreground ,chyla-dark-text
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,chyla-dark-text :underline t))))
   `(org-deadline-announce ((t (:foreground ,chyla-dark-text))))
   `(org-done ((t (:bold t :weight bold :foreground ,chyla-dark-html-tag))))
   `(org-formula ((t (:foreground ,chyla-dark-text))))
   `(org-headline-done ((t (:foreground ,chyla-dark-html-tag))))
   `(org-hide ((t (:foreground ,chyla-dark-selection))))
   `(org-level-1 ((t (:foreground ,chyla-dark-text))))
   `(org-level-2 ((t (:foreground ,chyla-dark-constant))))
   `(org-level-3 ((t (:foreground ,chyla-dark-constant))))
   `(org-level-4 ((t (:foreground ,chyla-dark-text))))
   `(org-level-5 ((t (:foreground ,chyla-dark-function))))
   `(org-level-6 ((t (:foreground ,chyla-dark-comment))))
   `(org-level-7 ((t (:foreground ,chyla-dark-text))))
   `(org-level-8 ((t (:foreground ,chyla-dark-text))))
   `(org-link ((t (:foreground ,chyla-dark-text :underline t))))
   `(org-scheduled ((t (:foreground ,chyla-dark-constant))))
   `(org-scheduled-previously ((t (:foreground ,chyla-dark-string))))
   `(org-scheduled-today ((t (:foreground ,chyla-dark-keyword))))
   `(org-sexp-date ((t (:foreground ,chyla-dark-keyword :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,chyla-dark-comment))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,chyla-dark-text))))
   `(org-todo ((t (:bold t :foreground ,chyla-dark-string :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,chyla-dark-string :weight bold :underline nil))))
   `(org-column ((t (:background ,chyla-dark-selection))))
   `(org-column-title ((t (:background ,chyla-dark-selection :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,chyla-dark-text :background ,chyla-dark-selection))))
   `(org-mode-line-clock-overrun ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(org-ellipsis ((t (:foreground ,chyla-dark-text :underline t))))
   `(org-footnote ((t (:foreground ,chyla-dark-function :underline t))))
   `(org-document-title ((t (:foreground ,chyla-dark-text))))
   `(org-document-info ((t (:foreground ,chyla-dark-text))))
   `(org-habit-ready-face ((t :background ,chyla-dark-comment)))
   `(org-habit-alert-face ((t :background ,chyla-dark-text :foreground ,chyla-dark-white)))
   `(org-habit-clear-face ((t :background ,chyla-dark-text)))
   `(org-habit-overdue-face ((t :background ,chyla-dark-text)))
   `(org-habit-clear-future-face ((t :background ,chyla-dark-text)))
   `(org-habit-ready-future-face ((t :background ,chyla-dark-comment)))
   `(org-habit-alert-future-face ((t :background ,chyla-dark-text :foreground ,chyla-dark-white)))
   `(org-habit-overdue-future-face ((t :background ,chyla-dark-text)))
;;;;; outline
   `(outline-1 ((t (:foreground ,chyla-dark-text))))
   `(outline-2 ((t (:foreground ,chyla-dark-constant))))
   `(outline-3 ((t (:foreground ,chyla-dark-constant))))
   `(outline-4 ((t (:foreground ,chyla-dark-text))))
   `(outline-5 ((t (:foreground ,chyla-dark-function))))
   `(outline-6 ((t (:foreground ,chyla-dark-comment))))
   `(outline-7 ((t (:foreground ,chyla-dark-text))))
   `(outline-8 ((t (:foreground ,chyla-dark-text))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,chyla-dark-text :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,chyla-dark-string :inherit mode-line))))
   `(powerline-active2 ((t (:background ,chyla-dark-keyword :inherit mode-line))))
   `(powerline-inactive1 ((t (:inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,chyla-dark-text :background ,chyla-dark-background))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-text))))
   `(proof-error-face ((t (:foreground ,chyla-dark-text :background ,chyla-dark-text))))
   `(proof-highlight-dependency-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-comment))))
   `(proof-highlight-dependent-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-comment))))
   `(proof-locked-face ((t (:background ,chyla-dark-comment))))
   `(proof-mouse-highlight-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-comment))))
   `(proof-queue-face ((t (:background ,chyla-dark-comment))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,chyla-dark-comment))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,chyla-dark-background))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,chyla-dark-background))))
   `(proof-warning-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-comment))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,chyla-dark-comment))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,chyla-dark-constant))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,chyla-dark-comment))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,chyla-dark-function))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,chyla-dark-comment))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,chyla-dark-keyword))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,chyla-dark-comment))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,chyla-dark-comment))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,chyla-dark-comment))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,chyla-dark-comment))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,chyla-dark-comment))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,chyla-dark-comment))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,chyla-dark-comment))))
   `(rcirc-other-nick ((t (:foreground ,chyla-dark-comment))))
   `(rcirc-bright-nick ((t (:foreground ,chyla-dark-keyword))))
   `(rcirc-dim-nick ((t (:foreground ,chyla-dark-comment))))
   `(rcirc-server ((t (:foreground ,chyla-dark-comment))))
   `(rcirc-server-prefix ((t (:foreground ,chyla-dark-comment))))
   `(rcirc-timestamp ((t (:foreground ,chyla-dark-comment))))
   `(rcirc-nick-in-message ((t (:foreground ,chyla-dark-keyword))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,chyla-dark-keyword :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,chyla-dark-keyword :bold t))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground "#24292e"))))
   `(realgud-overlay-arrow2 ((t (:foreground "#63645c"))))
   `(realgud-overlay-arrow3 ((t (:foreground "#bcbdc0"))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,chyla-dark-comment))))
   `(rpm-spec-doc-face ((t (:foreground ,chyla-dark-comment))))
   `(rpm-spec-ghost-face ((t (:foreground ,chyla-dark-string))))
   `(rpm-spec-macro-face ((t (:foreground ,chyla-dark-keyword))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,chyla-dark-string))))
   `(rpm-spec-package-face ((t (:foreground ,chyla-dark-string))))
   `(rpm-spec-section-face ((t (:foreground ,chyla-dark-keyword))))
   `(rpm-spec-tag-face ((t (:foreground ,chyla-dark-comment))))
   `(rpm-spec-var-face ((t (:foreground ,chyla-dark-string))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,chyla-dark-comment))))
   `(rst-level-2-face ((t (:foreground ,chyla-dark-comment))))
   `(rst-level-3-face ((t (:foreground ,chyla-dark-constant))))
   `(rst-level-4-face ((t (:foreground ,chyla-dark-comment))))
   `(rst-level-5-face ((t (:foreground ,chyla-dark-function))))
   `(rst-level-6-face ((t (:foreground ,chyla-dark-comment))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,chyla-dark-keyword :bold t))))
   `(sh-quoted-exec ((t (:foreground ,chyla-dark-string))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background :weight bold))))
   `(show-paren-match ((t (:foreground ,chyla-dark-white :background ,chyla-dark-keyword :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable chyla.org for sml
   `(sml/global ((,class (:foreground ,chyla-dark-comment :weight bold))))
   `(sml/modes ((,class (:foreground ,chyla-dark-keyword :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,chyla-dark-comment :weight bold))))
   `(sml/filename ((,class (:foreground ,chyla-dark-keyword :weight bold))))
   `(sml/line-number ((,class (:foreground ,chyla-dark-comment :weight bold))))
   `(sml/col-number ((,class (:foreground ,chyla-dark-keyword :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,chyla-dark-constant :weight bold))))
   `(sml/prefix ((,class (:foreground ,chyla-dark-comment))))
   `(sml/git ((,class (:foreground ,chyla-dark-html-tag))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,chyla-dark-comment :weight bold))))
   `(sml/read-only ((,class (:foreground ,chyla-dark-comment))))
   `(sml/outside-modified ((,class (:foreground ,chyla-dark-comment))))
   `(sml/modified ((,class (:foreground ,chyla-dark-string))))
   `(sml/vc-edited ((,class (:foreground ,chyla-dark-comment))))
   `(sml/charging ((,class (:foreground ,chyla-dark-constant))))
   `(sml/discharging ((,class (:foreground ,chyla-dark-comment))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,chyla-dark-comment :background ,chyla-dark-background :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,chyla-dark-background :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,chyla-dark-string))))
   `(slime-repl-inputed-output-face ((t (:foreground ,chyla-dark-comment))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-string)))
      (t
       (:underline ,chyla-dark-string))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-comment)))
      (t
       (:underline ,chyla-dark-comment))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-keyword)))
      (t
       (:underline ,chyla-dark-keyword))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,chyla-dark-comment)))
      (t
       (:underline ,chyla-dark-comment))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,chyla-dark-comment))))
   `(speedbar-directory-face ((t (:foreground ,chyla-dark-function))))
   `(speedbar-file-face ((t (:foreground ,chyla-dark-comment))))
   `(speedbar-highlight-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-comment))))
   `(speedbar-selected-face ((t (:foreground ,chyla-dark-string))))
   `(speedbar-separator-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-constant))))
   `(speedbar-tag-face ((t (:foreground ,chyla-dark-keyword))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,chyla-dark-comment
                                    :background ,chyla-dark-background))))
   `(tabbar-selected ((t (:foreground ,chyla-dark-comment
                                      :background ,chyla-dark-background
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,chyla-dark-comment
                                        :background ,chyla-dark-background
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,chyla-dark-text))))
   `(term-color-red ((t (:foreground ,chyla-dark-red))))
   `(term-color-green ((t (:foreground ,chyla-dark-green))))
   `(term-color-yellow ((t (:foreground ,chyla-dark-yellow))))
   `(term-color-blue ((t (:foreground ,chyla-dark-blue))))
   `(term-color-magenta ((t (:foreground ,chyla-dark-magenta))))
   `(term-color-cyan ((t (:foreground ,chyla-dark-cyan))))
   `(term-color-white ((t (:foreground ,chyla-dark-white))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,chyla-dark-comment :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,chyla-dark-comment :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,chyla-dark-comment))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,chyla-dark-keyword))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,chyla-dark-function))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,chyla-dark-highlight))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,chyla-dark-comment ))))
   `(web-mode-css-prop-face ((t (:foreground ,chyla-dark-constant))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,chyla-dark-html-tag :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,chyla-dark-html-tag))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,chyla-dark-comment))))
   `(web-mode-html-attr-name-face ((t (:foreground ,chyla-dark-function))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,chyla-dark-html-tag))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,chyla-dark-background))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,chyla-dark-string))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,chyla-dark-background :foreground ,chyla-dark-white))))
   `(whitespace-hspace ((t (:background ,chyla-dark-background :foreground ,chyla-dark-white))))
   `(whitespace-tab ((t (:background ,chyla-dark-comment))))
   `(whitespace-newline ((t (:foreground ,chyla-dark-white))))
   `(whitespace-trailing ((t (:background ,chyla-dark-string))))
   `(whitespace-line ((t (:background ,chyla-dark-background :foreground ,chyla-dark-comment))))
   `(whitespace-space-before-tab ((t (:background ,chyla-dark-comment :foreground ,chyla-dark-comment))))
   `(whitespace-indentation ((t (:background ,chyla-dark-keyword :foreground ,chyla-dark-string))))
   `(whitespace-empty ((t (:background ,chyla-dark-keyword))))
   `(whitespace-space-after-tab ((t (:background ,chyla-dark-keyword :foreground ,chyla-dark-string))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-folder-many-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-folder-path-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-message-citation-header ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,chyla-dark-string))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,chyla-dark-keyword))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-message-headers-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-message-header-contents ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-message-signature ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,chyla-dark-comment
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,chyla-dark-keyword))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,chyla-dark-comment))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,chyla-dark-constant))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,chyla-dark-keyword :weight bold))))
   `(cscope-function-face ((t (:foreground ,chyla-dark-function :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,chyla-dark-string :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-keyword))))
   `(cscope-separator-face ((t (:foreground ,chyla-dark-string :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,chyla-dark-selection))))
   `(yascroll:thumb-fringe ((t (:background ,chyla-dark-selection :foreground ,chyla-dark-selection))))

;;;;; elscreen
  `(elscreen-tab-background-face ((t (:background ,chyla-dark-keyword))))
  `(elscreen-tab-control-face ((t (:foreground ,chyla-dark-white :background ,chyla-dark-keyword))))
  `(elscreen-tab-current-screen-face ((t (:foreground ,chyla-dark-text :background ,chyla-dark-selection))))
  `(elscreen-tab-other-screen-face ((t (:foreground ,chyla-dark-text :background ,chyla-dark-highlight))))))

;;; Theme Variables
(chyla-dark-with-color-variables
  (custom-theme-set-variables
   'chyla-dark
;;;;; fill-column-indicator
   `(fci-rule-color ,chyla-dark-comment)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,chyla-dark-string ,chyla-dark-comment ,chyla-dark-keyword ,chyla-dark-comment ,chyla-dark-constant
                    ,chyla-dark-function ,chyla-dark-keyword ,chyla-dark-comment))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,chyla-dark-comment . ,chyla-dark-highlight))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,chyla-dark-comment)
       ( 40. . ,chyla-dark-string)
       ( 60. . ,chyla-dark-comment)
       ( 80. . ,chyla-dark-comment)
       (100. . ,chyla-dark-comment)
       (120. . ,chyla-dark-keyword)
       (140. . ,chyla-dark-comment)
       (160. . ,chyla-dark-comment)
       (180. . ,chyla-dark-comment)
       (200. . ,chyla-dark-comment)
       (220. . ,chyla-dark-html-tag)
       (240. . ,chyla-dark-constant)
       (260. . ,chyla-dark-function)
       (280. . ,chyla-dark-comment)
       (300. . ,chyla-dark-constant)
       (320. . ,chyla-dark-comment)
       (340. . ,chyla-dark-keyword)
       (360. . ,chyla-dark-comment)))
   `(vc-annotate-very-old-color ,chyla-dark-comment)
   `(vc-annotate-background ,chyla-dark-selection)))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'chyla-dark)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; chyla-dark-theme.el ends here
