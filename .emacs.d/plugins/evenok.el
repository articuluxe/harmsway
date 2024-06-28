;;; evenok.el --- Themes with perceptively evenly distributed colors  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author:                  Mekeor Melire <mekeor@posteo.de>
;; Created:                 2024
;; Homepage:                https://codeberg.org/mekeor/evenok
;; Keywords:                faces, theme
;; Maintainer:              Mekeor Melire <mekeor@posteo.de>
;; Package-Requires:        ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version:                 0.11.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `evenok' is a package that provides themes using the OKLCH color
;; space in order to evenly distribute its eight base colors within
;; the color spectrum as it is perceived by humans.  This also allows
;; to choose colors with equally perceived lightness.
;;
;; - https://bottosson.github.io/posts/oklab/
;; - https://oklch.com

;;;; Installation:

;; Clone the Git repository to your local persistent memory and put
;; its file-path to `load-path'.

;;;; Usage:

;; These themes are meant to be used on top of the
;; `unspecified-theme': https://codeberg.org/mekeor/unspecified-theme
;;
;; You can load them together like this:
;;
;;   (mapcar #'load-theme
;;       '(unspecified
;;         evenok-dark
;;         ;; evenok-dark-extra ;; optional, very opinionated
;;         ))

;;;; Demo:

;; A video demonstration of appearance and usage of the
;; `evenok-dark-theme' can be found here:
;; https://iv.catgirl.cloud/watch?v=_18ICdAmlTg

;;;; Roadmap:

;; - Update screenshots; add one for light variant.
;; - Use common colors for `eshell-ls-*' and `dired-*'.

;;;; Design Choices:

;; Links, buttons, or more generally clickables are underlined.

;; Faces which denote differences use following colors:
;; - added:   green
;; - changed: purple
;; - removed: red

;;; Code:

(defgroup evenok nil
  "Theme with perceptively evenly distributed colors."
  :group 'faces
  :link '(emacs-commentary-link
          :tag "Commentary"
          "evenok.el")
  :link '(emacs-library-link
          :tag "Source Lisp File"
          "evenok.el")
  :link '(url-link
          :tag "Website"
          "https://codeberg.org/mekeor/evenok")
  :prefix "evenok-"
  :tag "Evenok Themes")

(defmacro evenok-with-palette (palette &rest body)
  "Use variables referring to colors from PALETTE in BODY."
  (declare (indent 1))
  `(seq-let
       ( black dark dim grey grey-red grey-orange grey-green
         grey-purple grey-magenta faded faded-red faded-orange
         faded-green faded-purple faded-magenta bright bright-red
         bright-orange bright-yellow bright-green bright-cyan
         bright-blue bright-purple bright-magenta white)
       ,palette
     ,@body))

(defconst evenok-palette-dark
  (list
   "#000000" ;; L=0%
   "#1b1b1b" ;; L=22%
   "#3a3a3a" ;; L=35%
   "#5d5d5d" ;; L=48%
   "#96365d"
   "#993d0d"
   "#3d6c01"
   "#3d57a9"
   "#784293"
   "#777777" ;; L=57%
   "#b45177"
   "#b7582e"
   "#56872a"
   "#5572c6"
   "#935daf"
   "#a9a9a9" ;; L=73.6%
   "#ed83a9" ;; oklch(73.6% 0.135 358)
   "#f08b61"
   "#cda430"
   "#87bc5f"
   "#04c5aa"
   "#11bbe7"
   "#85a5ff"
   "#c78fe6"
   "#ffffff")) ;; L=100%

(defconst evenok-palette-light
  (list
   "#ffffff" ;; L=100%
   "#dedede" ;; L=90%
   "#bebebe" ;; L=80%
   "#9e9e9e" ;; L=70%
   "#c5917c"
   "#b19c6c"
   "#6dad9e"
   "#ad92be"
   "#c38d9f"
   "#808080" ;; L=60%
   "#a4735f"
   "#927e4e"
   "#4e8e80"
   "#8f749f"
   "#a36f80"
   "#424242" ;; L=38%
   "#613523"
   "#52400d"
   "#344a21"
   "#014e42"
   "#014a5d" ;; oklch(38% 0.07 222.36)
   "#324168"
   "#4f375d"
   "#603143"
   "#000000")) ;; L=0%

(defun evenok-theme (name palette)
  "Set variables and faces of theme named NAME with colors from PALETTE."
  (evenok-with-palette palette
    (let*
        ((faces-as-faces
          (list
           `(ansi-color-black                                     ((t :foreground "black")))
           `(ansi-color-blue                                      ((t :foreground ,bright-blue)))
           `(ansi-color-bold                                      ((t :weight bold)))
           `(ansi-color-bright-black                              ((t :foreground "black" :weight bold)))
           `(ansi-color-bright-blue                               ((t :foreground ,bright-blue :weight bold)))
           `(ansi-color-bright-cyan                               ((t :foreground ,bright-cyan :weight bold)))
           `(ansi-color-bright-green                              ((t :foreground ,bright-green :weight bold)))
           `(ansi-color-bright-magenta                            ((t :foreground ,bright-magenta :weight bold)))
           `(ansi-color-bright-red                                ((t :foreground ,bright-red :weight bold)))
           `(ansi-color-bright-white                              ((t :foreground "white" :weight bold)))
           `(ansi-color-bright-yellow                             ((t :foreground ,bright-yellow :weight bold)))
           `(ansi-color-cyan                                      ((t :foreground ,bright-cyan)))
           `(ansi-color-faint                                     ((t :weight light)))
           `(ansi-color-green                                     ((t :foreground ,bright-green)))
           `(ansi-color-inverse                                   ((t :inverse-video t)))
           `(ansi-color-italic                                    ((t :slant italic)))
           `(ansi-color-magenta                                   ((t :foreground ,bright-magenta)))
           `(ansi-color-red                                       ((t :foreground ,bright-red)))
           `(ansi-color-underline                                 ((t :underline t)))
           `(ansi-color-white                                     ((t :foreground "white")))
           `(ansi-color-yellow                                    ((t :foreground ,bright-yellow)))
           `(avy-goto-char-timer-face                             ((t :background ,white :foreground ,black :weight bold)))
           `(avy-lead-face                                        ((t :background ,white :foreground ,black :weight bold)))
           `(avy-lead-face-0                                      ((t :background ,white :foreground ,black :weight bold)))
           `(avy-lead-face-1                                      ((t :background ,white :foreground ,black :weight bold)))
           `(avy-lead-face-2                                      ((t :background ,white :foreground ,black :weight bold)))
           `(bold                                                 ((t :weight bold)))
           `(bold-italic                                          ((t :slant italic :weight bold)))
           `(breadcrumb-face                                      ((t :foreground ,faded :slant italic :weight light)))
           `(breadcrumb-imenu-crumbs-face                         ((t :foreground ,faded :slant italic :weight light)))
           `(breadcrumb-imenu-leaf-face                           ((t :foreground ,faded :slant italic :weight light)))
           `(breadcrumb-project-base-face                         ((t :foreground ,faded :slant italic :weight light)))
           `(breadcrumb-project-crumbs-face                       ((t :foreground ,faded :slant italic :weight light)))
           `(breadcrumb-project-leaf-face                         ((t :foreground ,faded :slant italic :weight light)))
           `(browse-url-button                                    ((t :underline t)))
           `(button                                               ((t :underline t)))
           `(calendar-month-header                                ((t :foreground ,white :weight bold)))
           `(calendar-today                                       ((t :foreground ,bright-yellow)))
           `(calendar-weekday-header                              ((t :foreground ,white :weight bold)))
           `(calendar-weekend-header                              ((t :foreground ,white :weight bold)))
           `(change-log-date                                      ((t :foreground ,white :weight bold)))
           `(change-log-email                                     ((t :foreground ,white :weight bold)))
           `(change-log-file                                      ((t :foreground ,bright-green)))
           `(change-log-function                                  ((t :foreground ,bright-blue)))
           `(change-log-list                                      ((t :foreground ,bright-purple)))
           `(change-log-name                                      ((t :foreground ,white :weight bold)))
           `(comint-highlight-input                               ((t :slant italic)))
           `(comint-highlight-prompt                              ((t :foreground ,bright-blue)))
           `(completions-annotations                              ((t :foreground ,faded)))
           `(completions-common-part                              ((t :weight light)))
           `(completions-group-separator                          ((t :background ,grey)))
           `(completions-group-title                              ((t :background ,grey :foreground ,white :weight bold)))
           `(completions-highlight                                ((t :foreground ,bright-yellow)))
           `(consult-highlight-match                              ((t :inverse-video t :weight bold)))
           `(consult-preview-match                                ((t :inverse-video t :weight bold)))
           `(consult-separator                                    ((t :foreground ,bright-cyan)))
           `(corfu-annotations                                    ((t :foreground ,faded)))
           `(corfu-bar                                            ((t :background ,grey)))
           `(corfu-border                                         ((t :background ,grey)))
           `(corfu-current                                        ((t :foreground ,bright-yellow)))
           `(corfu-default                                        ((t :background ,dark)))
           `(cursor                                               ((t :background ,white)))
           `(custom-button                                        ((t :underline t)))
           `(custom-button-mouse                                  ((t :underline t)))
           `(custom-button-pressed                                ((t :underline t)))
           `(custom-button-pressed-unraised                       ((t :underline t)))
           `(custom-button-unraised                               ((t :underline t)))
           `(custom-changed                                       ((t :foreground ,bright-purple)))
           `(custom-comment                                       ((t :foreground ,faded :weight normal)))
           `(custom-comment-tag                                   ((t :foreground ,faded :weight normal)))
           `(custom-documentation                                 ((t :underline nil)))
           `(custom-face-tag                                      ((t :foreground ,bright-magenta)))
           `(custom-group-subtitle                                ((t :foreground ,bright-yellow)))
           `(custom-group-tag                                     ((t :foreground ,bright-yellow)))
           `(custom-group-tag-1                                   ((t :foreground ,bright-yellow)))
           `(custom-invalid                                       ((t :foreground ,bright-red)))
           `(custom-link                                          ((t :underline t)))
           `(custom-modified                                      ((t :foreground ,bright-magenta)))
           `(custom-rogue                                         ((t :foreground ,bright-red)))
           `(custom-saved                                         ((t :foreground ,bright-green)))
           `(custom-set                                           ((t :foreground ,bright-green)))
           `(custom-state                                         ((t :foreground ,bright-blue)))
           `(custom-themed                                        ((t :foreground ,bright-magenta)))
           `(custom-variable-button                               ((t :underline t)))
           `(custom-variable-obsolete                             ((t :foreground ,bright-red)))
           `(custom-variable-tag                                  ((t :foreground ,bright-yellow)))
           `(custom-visibility                                    ((t :underline t)))
           `(debbugs-gnu-title                                    ((t :foreground ,white)))
           `(default                                              ((t :background ,black :foreground ,bright)))
           `(devdocs-code-block                                   ((t :background ,dark :extend t)))
           `(diary-button                                         ((t :underline t)))
           `(dictionary-button-face                               ((t :underline t)))
           `(dictionary-reference-face                            ((t :underline t)))
           `(dictionary-word-definition-face                      ((t :slant italic)))
           `(dictionary-word-entry-face                           ((t :foreground ,white)))
           `(diff-added                                           ((t :background ,grey-green)))
           `(diff-changed                                         ((t :background ,grey-purple)))
           `(diff-context                                         ((t :background ,dark)))
           `(diff-header                                          ((t :background ,grey)))
           `(diff-hl-change                                       ((t :background ,faded-purple :weight light)))
           `(diff-hl-delete                                       ((t :background ,faded-red :weight light)))
           `(diff-hl-dired-change                                 ((t :foreground ,faded-purple)))
           `(diff-hl-dired-delete                                 ((t :foreground ,faded-red)))
           `(diff-hl-dired-ignored                                ((t :foreground ,faded-orange)))
           `(diff-hl-dired-insert                                 ((t :foreground ,faded-green)))
           `(diff-hl-dired-unknown                                ((t :foreground ,faded)))
           `(diff-hl-insert                                       ((t :background ,faded-green)))
           `(diff-hl-margin-change                                ((t :foreground ,faded-purple)))
           `(diff-hl-margin-delete                                ((t :foreground ,faded-red)))
           `(diff-hl-margin-ignored                               ((t :foreground ,faded-orange)))
           `(diff-hl-margin-insert                                ((t :foreground ,faded-green)))
           `(diff-hl-margin-unknown                               ((t :foreground ,faded)))
           `(diff-indicator-added                                 ((t :foreground ,bright-green)))
           `(diff-indicator-changed                               ((t :foreground ,bright-purple)))
           `(diff-indicator-removed                               ((t :foreground ,bright-red)))
           `(diff-refine-added                                    ((t :background ,faded-green)))
           `(diff-refine-changed                                  ((t :background ,faded-purple)))
           `(diff-refine-removed                                  ((t :background ,faded-red)))
           `(diff-removed                                         ((t :background ,grey-red)))
           `(dired-broken-symlink                                 ((t :foreground ,bright-red)))
           `(dired-directory                                      ((t :foreground ,bright-blue)))
           `(dired-flagged                                        ((t :foreground ,bright-orange)))
           `(dired-header                                         ((t :foreground ,white :weight bold)))
           `(dired-mark                                           ((t :foreground ,bright-orange)))
           `(dired-marked                                         ((t :foreground ,bright-orange)))
           `(dired-symlink                                        ((t :foreground ,bright-cyan)))
           `(eat-shell-prompt-annotation-failure                  ((t :foreground ,bright-red)))
           `(eat-shell-prompt-annotation-running                  ((t :foreground ,bright-yellow)))
           `(eat-shell-prompt-annotation-success                  ((t :foreground ,bright-green)))
           `(eat-term-bold                                        ((t :weight bold)))
           `(eat-term-color-0                                     ((t :foreground ,black)))
           `(eat-term-color-1                                     ((t :foreground ,bright-red)))
           `(eat-term-color-10                                    ((t :foreground ,bright-green :weight bold)))
           `(eat-term-color-11                                    ((t :foreground ,bright-yellow :weight bold)))
           `(eat-term-color-12                                    ((t :foreground ,bright-blue :weight bold)))
           `(eat-term-color-13                                    ((t :foreground ,bright-magenta :weight bold)))
           `(eat-term-color-14                                    ((t :foreground ,bright-cyan :weight bold)))
           `(eat-term-color-15                                    ((t :foreground ,bright :weight bold)))
           `(eat-term-color-2                                     ((t :foreground ,bright-green)))
           `(eat-term-color-3                                     ((t :foreground ,bright-yellow)))
           `(eat-term-color-4                                     ((t :foreground ,bright-blue)))
           `(eat-term-color-5                                     ((t :foreground ,bright-magenta)))
           `(eat-term-color-6                                     ((t :foreground ,bright-cyan)))
           `(eat-term-color-7                                     ((t :foreground ,bright)))
           `(eat-term-color-8                                     ((t :foreground ,black :weight bold)))
           `(eat-term-color-9                                     ((t :foreground ,bright-red :weight bold)))
           `(eat-term-italic                                      ((t :slant italic)))
           `(eglot-highlight-symbol-face                          ((t :background ,dim)))
           `(eglot-inlay-hint-face                                ((t :foreground ,grey :weight light)))
           `(eglot-type-hint-face                                 ((t :foreground ,grey :weight light)))
           `(eldoc-highlight-function-argument                    ((t :inverse-video nil :weight bold)))
           `(elpaca-busy                                          ((t :foreground ,bright-purple)))
           `(elpaca-failed                                        ((t :foreground ,bright-red)))
           `(elpaca-blocked                                       ((t :foreground ,bright-orange)))
           `(elpaca-finished                                      ((t :foreground ,bright-green)))
           `(elpaca-log-info                                      ((t :foreground ,bright-yellow)))
           `(elpaca-log-error                                     ((t :foreground ,bright-red)))
           `(elpaca-info-package                                  ((t :foreground ,white)))
           `(elpaca-log-highlight                                 ((t :foreground ,bright-yellow)))
           `(elpaca-ui-conflicting                                ((t :foreground ,bright-red :weight bold)))
           `(elpaca-ui-marked-pull                                ((t :foreground ,bright-yellow :weight bold)))
           `(elpaca-ui-marked-fetch                               ((t :foreground ,bright-blue :weight bold)))
           `(elpaca-ui-marked-merge                               ((t :foreground ,bright-purple :weight bold)))
           `(elpaca-ui-marked-delete                              ((t :foreground ,bright-red :weight bold)))
           `(elpaca-ui-marked-install                             ((t :foreground ,bright-cyan :weight bold)))
           `(elpaca-ui-marked-rebuild                             ((t :foreground ,bright-magenta :weight bold)))
           `(embark-collect-group-separator                       ((t :foreground ,bright-yellow)))
           `(embark-collect-group-title                           ((t :foreground ,white)))
           `(embark-keybinding                                    ((t :foreground ,bright-yellow)))
           `(embark-selected                                      ((t :inverse-video t :weight bold)))
           `(embark-target                                        ((t :inverse-video t :weight bold)))
           `(embark-verbose-indicator-title                       ((t :foreground ,white)))
           `(erc-button                                           ((t :underline t)))
           `(erc-current-nick-face                                ((t :foreground ,bright-red)))
           `(erc-my-nick-face                                     ((t :foreground ,bright-green)))
           `(erc-nick-default-face                                ((t :foreground ,bright-yellow)))
           `(erc-notice-face                                      ((t :foreground ,faded)))
           `(erc-timestamp-face                                   ((t :foreground ,faded)))
           `(error                                                ((t :foreground ,bright-red)))
           `(ert-test-result-expected                             ((t :background ,grey-green :extend t :weight bold)))
           `(ert-test-result-unexpected                           ((t :background ,grey-red :extend t :weight bold)))
           `(escape-glyph                                         ((t :foreground ,faded-magenta :weight bold)))
           `(eshell-ls-directory                                  ((t :foreground ,bright-yellow)))
           `(eshell-ls-executable                                 ((t :foreground ,bright-green)))
           `(eshell-ls-missing                                    ((t :foreground ,bright-red)))
           `(eshell-ls-readonly                                   ((t :foreground ,bright-purple)))
           `(eshell-ls-special                                    ((t :foreground ,bright-magenta)))
           `(eshell-ls-symlink                                    ((t :foreground ,bright-cyan)))
           `(eshell-ls-unreadable                                 ((t :foreground ,bright-red)))
           `(eshell-prompt                                        ((t :foreground ,white :weight bold)))
           `(eshell-syntax-highlighting-alias-face                ((t :foreground ,bright-purple)))
           `(eshell-syntax-highlighting-command-substitution-face ((t :foreground ,bright-purple)))
           `(eshell-syntax-highlighting-comment-face              ((t :foreground ,faded)))
           `(eshell-syntax-highlighting-default-face              ((t :foreground ,bright)))
           `(eshell-syntax-highlighting-directory-face            ((t :foreground ,bright-green)))
           `(eshell-syntax-highlighting-envvar-face               ((t :foreground ,bright-orange)))
           `(eshell-syntax-highlighting-file-arg-face             ((t :foreground ,bright-green)))
           `(eshell-syntax-highlighting-invalid-face              ((t :foreground ,bright-red)))
           `(eshell-syntax-highlighting-lisp-function-face        ((t :foreground ,bright-cyan)))
           `(eshell-syntax-highlighting-option-face               ((t :foreground ,bright-magenta)))
           `(eshell-syntax-highlighting-shell-command-face        ((t :foreground ,bright-blue)))
           `(eshell-syntax-highlighting-string-face               ((t :foreground ,bright-green)))
           `(file-name-shadow                                     ((t :foreground ,faded)))
           `(fill-column-indicator                                ((t :foreground ,dim)))
           `(flycheck-verify-select-checker                       ((t :underline t)))
           `(flymake-end-of-line-diagnostics-face                 ((t :foreground ,grey-red :slant italic :weight light)))
           `(flymake-error                                        ((t :underline (:color ,bright-red :style wave :position nil))))
           `(flymake-error-echo                                   ((t :foreground ,bright-red)))
           `(flymake-error-echo-at-eol                            ((t :foreground ,grey-red :weight light)))
           `(flymake-note                                         ((t :foreground ,bright-purple)))
           `(flymake-note-echo                                    ((t :foreground ,bright-purple)))
           `(flymake-note-echo-at-eol                             ((t :foreground ,grey-purple :weight light)))
           `(flymake-popon                                        ((t :background ,dark)))
           `(flymake-popon-posframe-border                        ((t :foreground ,grey)))
           `(flymake-warning                                      ((t :underline (:color ,bright-orange :style wave :position nil))))
           `(flymake-warning-echo                                 ((t :foreground ,bright-orange)))
           `(flymake-warning-echo-at-eol                          ((t :foreground ,grey-orange :weight light)))
           `(font-latex-math-face                                 ((t :background ,black)))
           `(font-lock-builtin-face                               ((t :foreground ,bright-cyan)))
           `(font-lock-comment-delimiter-face                     ((t :foreground ,faded :weight normal)))
           `(font-lock-comment-face                               ((t :foreground ,faded :weight normal)))
           `(font-lock-constant-face                              ((t :foreground ,bright-orange)))
           `(font-lock-doc-face                                   ((t :foreground ,faded)))
           `(font-lock-function-name-face                         ((t :foreground ,bright-blue)))
           `(font-lock-keyword-face                               ((t :foreground ,bright-magenta)))
           `(font-lock-negation-char-face                         ((t :foreground ,bright-red)))
           `(font-lock-number-face                                ((t :foreground ,bright-purple)))
           `(font-lock-preprocessor-face                          ((t :foreground ,bright-blue)))
           `(font-lock-regexp-face                                ((t :foreground ,bright-blue)))
           `(font-lock-regexp-grouping-backslash                  ((t :foreground ,bright-yellow)))
           `(font-lock-regexp-grouping-construct                  ((t :foreground ,bright-magenta)))
           `(font-lock-string-face                                ((t :foreground ,bright-green)))
           `(font-lock-type-face                                  ((t :foreground ,bright-yellow)))
           `(font-lock-variable-name-face                         ((t :foreground ,bright-red)))
           `(font-lock-warning-face                               ((t :foreground ,bright-red)))
           `(fringe                                               ((t :foreground ,grey)))
           `(git-commit-summary                                   ((t :foreground ,white)))
           `(git-timemachine-commit                               ((t :slant italic)))
           `(git-timemachine-minibuffer-author-face               ((t :slant italic)))
           `(git-timemachine-minibuffer-detail-face               ((t :foreground ,white)))
           `(glyphless-char                                       ((t :background ,faded-magenta :foreground ,faded-magenta :weight bold)))
           `(gnus-button                                          ((t :underline t)))
           `(gnus-cite-1                                          ((t :foreground ,faded)))
           `(gnus-cite-10                                         ((t :foreground ,faded)))
           `(gnus-cite-11                                         ((t :foreground ,faded)))
           `(gnus-cite-2                                          ((t :foreground ,faded)))
           `(gnus-cite-3                                          ((t :foreground ,faded)))
           `(gnus-cite-4                                          ((t :foreground ,faded)))
           `(gnus-cite-5                                          ((t :foreground ,faded)))
           `(gnus-cite-6                                          ((t :foreground ,faded)))
           `(gnus-cite-7                                          ((t :foreground ,faded)))
           `(gnus-cite-8                                          ((t :foreground ,faded)))
           `(gnus-cite-9                                          ((t :foreground ,faded)))
           `(gnus-header-name                                     ((t :foreground ,bright-yellow)))
           `(gnus-header-subject                                  ((t :foreground ,white :weight bold)))
           `(gnus-server-agent                                    ((t :foreground ,bright-cyan)))
           `(gnus-server-closed                                   ((t :foreground ,bright-orange)))
           `(gnus-server-cloud                                    ((t :foreground ,bright-blue)))
           `(gnus-server-cloud-host                               ((t :foreground ,bright-purple)))
           `(gnus-server-denied                                   ((t :foreground ,bright-magenta)))
           `(gnus-server-offline                                  ((t :foreground ,bright-red)))
           `(gnus-server-opened                                   ((t :foreground ,bright-green)))
           `(gnus-summary-cancelled                               ((t :foreground ,bright-red)))
           `(gnus-summary-high-ancient                            ((t :foreground ,faded :weight bold)))
           `(gnus-summary-high-read                               ((t :weight bold)))
           `(gnus-summary-high-ticked                             ((t :weight bold)))
           `(gnus-summary-high-undownloaded                       ((t :weight bold)))
           `(gnus-summary-high-unread                             ((t :foreground ,bright-yellow :weight bold)))
           `(gnus-summary-low-ancient                             ((t :foreground ,faded :weight light)))
           `(gnus-summary-low-read                                ((t :weight light)))
           `(gnus-summary-low-ticked                              ((t :weight light)))
           `(gnus-summary-low-undownloaded                        ((t :weight light)))
           `(gnus-summary-low-unread                              ((t :foreground ,bright-yellow)))
           `(gnus-summary-normal-ancient                          ((t :foreground ,faded)))
           `(gnus-summary-normal-unread                           ((t :foreground ,bright-yellow)))
           `(grep-heading                                         ((t :foreground ,white :weight bold)))
           `(guix-build-log-phase-end                             ((t :foreground ,bright-blue)))
           `(guix-build-log-phase-fail                            ((t :foreground ,bright-red)))
           `(guix-build-log-phase-name                            ((t :foreground ,white)))
           `(guix-build-log-phase-seconds                         ((t :foreground ,faded)))
           `(guix-build-log-phase-start                           ((t :foreground ,bright-blue)))
           `(guix-build-log-phase-success                         ((t :foreground ,bright-green)))
           `(guix-build-log-title-end                             ((t :foreground ,bright-blue)))
           `(guix-build-log-title-fail                            ((t :foreground ,bright-red)))
           `(guix-build-log-title-head                            ((t :foreground ,bright-yellow)))
           `(guix-build-log-title-start                           ((t :foreground ,bright-blue)))
           `(guix-build-log-title-success                         ((t :foreground ,bright-green)))
           `(haskell-constructor-face                             ((t :slant italic)))
           `(haskell-definition-face                              ((t :weight bold)))
           `(haskell-error-face                                   ((t :foreground ,bright-red)))
           `(haskell-keyword-face                                 ((t :foreground ,bright-magenta)))
           `(haskell-pragma-face                                  ((t :foreground ,bright-yellow)))
           `(haskell-quasi-quote-face                             ((t :foreground ,bright-blue)))
           `(haskell-type-face                                    ((t :foreground ,bright-purple)))
           `(haskell-warning-face                                 ((t :foreground ,bright-orange)))
           `(header-line                                          ((t :background ,dark :foreground ,faded)))
           `(help-for-help-header                                 ((t :foreground ,white :weight bold)))
           `(help-key-binding                                     ((t :foreground ,bright-yellow)))
           `(helpful-heading                                      ((t :foreground ,white :weight bold)))
           `(highlight                                            ((t :background ,grey)))
           `(highlight-parentheses-highlight                      ((t :foreground ,white)))
           `(hl-line                                              ((t :background ,dark)))
           `(hydra-face-amaranth                                  ((t :foreground ,bright-magenta)))
           `(hydra-face-blue                                      ((t :foreground ,bright-blue)))
           `(hydra-face-pink                                      ((t :foreground ,bright-purple)))
           `(hydra-face-red                                       ((t :foreground ,bright-red)))
           `(info-header-node                                     ((t :foreground ,white)))
           `(info-header-xref                                     ((t :foreground ,bright-blue)))
           `(info-index-match                                     ((t :foreground ,bright-yellow)))
           `(info-menu-header                                     ((t :foreground ,white)))
           `(info-node                                            ((t :foreground ,bright-yellow)))
           `(Info-quoted                                          ((t :foreground ,faded)))
           `(info-title-1                                         ((t :foreground ,white :weight bold)))
           `(info-title-2                                         ((t :foreground ,white :weight bold)))
           `(info-title-3                                         ((t :foreground ,white :weight bold)))
           `(info-title-4                                         ((t :foreground ,white :weight bold)))
           `(info-xref                                            ((t :underline t)))
           `(info-xref-visited                                    ((t :underline t)))
           `(isearch                                              ((t :inverse-video t :weight bold)))
           `(isearch-fail                                         ((t :foreground ,bright-red)))
           `(italic                                               ((t :slant italic)))
           `(jinx-annotation                                      ((t :foreground ,bright-magenta)))
           `(jinx-highlight                                       ((t :inverse-video t :weight bold)))
           `(jinx-key                                             ((t :foreground ,bright-yellow)))
           `(jinx-misspelled                                      ((t :underline (:color ,faded-red :style wave :position nil))))
           `(jinx-save                                            ((t :foreground ,bright-blue)))
           `(lazy-highlight                                       ((t :background ,grey :foreground ,white :weight bold)))
           `(line-number                                          ((t :foreground ,dim)))
           `(line-number-current-line                             ((t :background ,dark :foreground ,dim :weight bold)))
           `(link                                                 ((t :underline t)))
           `(link-visited                                         ((t :underline t)))
           `(magit-blame-heading                                  ((t :background ,dark :foreground ,bright :slant italic)))
           `(magit-branch-current                                 ((t :foreground ,bright-purple)))
           `(magit-branch-local                                   ((t :foreground ,bright-purple)))
           `(magit-branch-remote                                  ((t :foreground ,bright-purple)))
           `(magit-branch-remote-head                             ((t :foreground ,bright-purple)))
           `(magit-branch-upstream                                ((t :foreground ,bright-purple)))
           `(magit-branch-warning                                 ((t :foreground ,bright-purple)))
           `(magit-diff-added                                     ((t :background ,grey-green)))
           `(magit-diff-added-highlight                           ((t :background ,grey-green)))
           `(magit-diff-conflict-heading                          ((t :background ,dark)))
           `(magit-diff-context-highlight                         ((t :background ,dark)))
           `(magit-diff-file-heading                              ((t :background ,dark)))
           `(magit-diff-file-heading-highlight                    ((t :background ,grey)))
           `(magit-diff-file-heading-selection                    ((t :background ,grey)))
           `(magit-diff-hunk-heading                              ((t :background ,dark)))
           `(magit-diff-hunk-heading-highlight                    ((t :background ,grey)))
           `(magit-diff-hunk-heading-selection                    ((t :background ,grey)))
           `(magit-diff-lines-heading                             ((t :background ,dark)))
           `(magit-diff-removed                                   ((t :background ,grey-red)))
           `(magit-diff-removed-highlight                         ((t :background ,grey-red)))
           `(magit-dimmed                                         ((t :foreground ,faded)))
           `(magit-hash                                           ((t :foreground ,grey)))
           `(magit-header-line                                    ((t :foreground ,bright-yellow)))
           `(magit-header-line-key                                ((t :foreground ,bright-yellow)))
           `(magit-header-line-log-select                         ((t :foreground ,bright-yellow)))
           `(magit-log-author                                     ((t :foreground ,faded)))
           `(magit-log-date                                       ((t :foreground ,faded)))
           `(magit-mode-line-process-error                        ((t :foreground ,bright-red)))
           `(magit-refname                                        ((t :foreground ,bright-magenta)))
           `(magit-section-heading                                ((t :foreground ,bright-yellow)))
           `(magit-section-heading-selection                      ((t :background ,grey :foreground ,bright-orange)))
           `(magit-section-highlight                              ((t :background ,dark)))
           `(magit-section-secondary-heading                      ((t :background ,dark)))
           `(magit-signature-bad                                  ((t :foreground ,bright-red)))
           `(magit-signature-error                                ((t :foreground ,bright-red)))
           `(magit-signature-expired                              ((t :foreground ,bright-red)))
           `(magit-signature-expired-key                          ((t :foreground ,bright-red)))
           `(magit-signature-good                                 ((t :foreground ,bright-green)))
           `(magit-signature-revoked                              ((t :foreground ,bright-red)))
           `(magit-signature-untrusted                            ((t :foreground ,bright-orange)))
           `(magit-tag                                            ((t :foreground ,bright-magenta)))
           `(marginalia-archive                                   ((t :foreground ,faded)))
           `(marginalia-char                                      ((t :foreground ,faded)))
           `(marginalia-date                                      ((t :foreground ,faded)))
           `(marginalia-documentation                             ((t :foreground ,faded)))
           `(marginalia-file-name                                 ((t :foreground ,faded)))
           `(marginalia-file-owner                                ((t :foreground ,faded)))
           `(marginalia-file-priv-dir                             ((t :foreground ,faded)))
           `(marginalia-file-priv-exec                            ((t :foreground ,faded)))
           `(marginalia-file-priv-link                            ((t :foreground ,faded)))
           `(marginalia-file-priv-no                              ((t :foreground ,faded)))
           `(marginalia-file-priv-other                           ((t :foreground ,faded)))
           `(marginalia-file-priv-rare                            ((t :foreground ,faded)))
           `(marginalia-file-priv-read                            ((t :foreground ,faded)))
           `(marginalia-file-priv-write                           ((t :foreground ,faded)))
           `(marginalia-function                                  ((t :foreground ,faded)))
           `(marginalia-installed                                 ((t :foreground ,faded)))
           `(marginalia-key                                       ((t :foreground ,faded)))
           `(marginalia-lighter                                   ((t :foreground ,faded)))
           `(marginalia-list                                      ((t :foreground ,faded)))
           `(marginalia-mode                                      ((t :foreground ,faded)))
           `(marginalia-modified                                  ((t :foreground ,faded)))
           `(marginalia-null                                      ((t :foreground ,faded)))
           `(marginalia-number                                    ((t :foreground ,faded)))
           `(marginalia-off                                       ((t :foreground ,faded)))
           `(marginalia-on                                        ((t :foreground ,faded)))
           `(marginalia-size                                      ((t :foreground ,faded)))
           `(marginalia-string                                    ((t :foreground ,faded)))
           `(marginalia-symbol                                    ((t :foreground ,faded)))
           `(marginalia-true                                      ((t :foreground ,faded)))
           `(marginalia-type                                      ((t :foreground ,faded)))
           `(marginalia-value                                     ((t :foreground ,faded)))
           `(marginalia-version                                   ((t :foreground ,faded)))
           `(markdown-blockquote-face                             ((t :foreground ,faded)))
           `(markdown-bold-face                                   ((t :weight bold)))
           `(markdown-code-face                                   ((t :foreground ,faded)))
           `(markdown-comment-face                                ((t :foreground ,faded)))
           `(markdown-header-face                                 ((t :foreground ,white)))
           `(markdown-header-face-1                               ((t :foreground ,white)))
           `(markdown-header-face-2                               ((t :foreground ,white)))
           `(markdown-header-face-3                               ((t :foreground ,white)))
           `(markdown-header-face-4                               ((t :foreground ,white)))
           `(markdown-header-face-5                               ((t :foreground ,white)))
           `(markdown-header-face-6                               ((t :foreground ,white)))
           `(markdown-highlight-face                              ((t :foreground ,bright-yellow)))
           `(markdown-highlighting-face                           ((t :foreground ,bright-yellow)))
           `(markdown-hr-face                                     ((t :foreground ,dark)))
           `(markdown-html-tag-name-face                          ((t :weight bold)))
           `(markdown-inline-code-face                            ((t :foreground ,faded)))
           `(markdown-italic-face                                 ((t :slant italic)))
           `(markdown-language-keyword-face                       ((t :foreground ,bright-magenta)))
           `(markdown-link-face                                   ((t :underline t)))
           `(markdown-metadata-key-face                           ((t :foreground ,bright-yellow)))
           `(markdown-plain-url-face                              ((t :underline t)))
           `(markdown-strike-through-face                         ((t :strike-through t)))
           `(markdown-url-face                                    ((t :underline t)))
           `(match                                                ((t :inverse-video t :weight bold)))
           `(mc/cursor-face                                       ((t :background ,white)))
           `(mc/region-face                                       ((t :background ,grey)))
           `(message-header-name                                  ((t :foreground ,bright-yellow)))
           `(message-header-subject                               ((t :foreground ,white :weight bold)))
           `(message-mml                                          ((t :foreground ,faded-magenta :slant italic)))
           `(message-separator                                    ((t :foreground ,faded)))
           `(minibuffer-prompt                                    ((t :foreground ,bright-blue)))
           `(mode-line                                            ((t :background ,dark)))
           `(mode-line-active                                     ((t :background ,dim)))
           `(mode-line-buffer-id                                  ((t :weight bold)))
           `(mode-line-emphasis                                   ((t :foreground ,bright-magenta)))
           `(mode-line-highlight                                  ((t :foreground ,bright-purple)))
           `(mode-line-inactive                                   ((t :background ,dark :foreground ,faded)))
           `(mu4e-flagged-face                                    ((t :foreground ,bright-red)))
           `(mu4e-header-highlight-face                           ((t :background ,dark)))
           `(mu4e-header-marks-face                               ((t :foreground ,bright-red)))
           `(mu4e-highlight-face                                  ((t :foreground ,bright-yellow)))
           `(mu4e-link-face                                       ((t :underline t)))
           `(mu4e-region-code                                     ((t :background ,dark)))
           `(mu4e-related-face                                    ((t :foreground ,bright-purple)))
           `(mu4e-replied-face                                    ((t :foreground ,bright-green)))
           `(mu4e-title-face                                      ((t :foreground ,white :weight bold)))
           `(mu4e-trashed-face                                    ((t :strike-through t)))
           `(mu4e-unread-face                                     ((t :foreground ,bright-yellow)))
           `(orderless-match-face-0                               ((t :inverse-video t :weight bold)))
           `(orderless-match-face-1                               ((t :inverse-video t :weight bold)))
           `(orderless-match-face-2                               ((t :inverse-video t :weight bold)))
           `(orderless-match-face-3                               ((t :inverse-video t :weight bold)))
           `(org-agenda-date                                      ((t :foreground ,bright-yellow :weight bold)))
           `(org-agenda-date-today                                ((t :foreground ,bright-orange :weight bold)))
           `(org-agenda-date-weekend                              ((t :foreground ,bright-yellow :weight bold)))
           `(org-agenda-date-weekend-today                        ((t :foreground ,bright-orange)))
           `(org-block                                            ((t :background ,dark)))
           `(org-block-begin-line                                 ((t :foreground ,faded)))
           `(org-block-end-line                                   ((t :foreground ,faded)))
           `(org-cite                                             ((t :foreground ,bright-blue)))
           `(org-code                                             ((t :foreground ,faded)))
           `(org-date                                             ((t :foreground ,bright-yellow)))
           `(org-dispatcher-highlight                             ((t :foreground ,bright-yellow)))
           `(org-document-title                                   ((t :foreground ,white :weight bold)))
           `(org-done                                             ((t :foreground ,bright-green)))
           `(org-drawer                                           ((t :foreground ,grey)))
           `(org-formula                                          ((t :foreground ,faded)))
           `(org-hide                                             ((t :foreground ,black)))
           `(org-level-1                                          ((t :foreground ,white)))
           `(org-level-2                                          ((t :foreground ,white)))
           `(org-level-3                                          ((t :foreground ,white)))
           `(org-level-4                                          ((t :foreground ,white)))
           `(org-level-5                                          ((t :foreground ,white)))
           `(org-level-6                                          ((t :foreground ,white)))
           `(org-level-7                                          ((t :foreground ,white)))
           `(org-level-8                                          ((t :foreground ,white)))
           `(org-link                                             ((t :underline t)))
           `(org-property-value                                   ((t :foreground ,grey)))
           `(org-quote                                            ((t :foreground ,faded)))
           `(org-special-keyword                                  ((t :foreground ,grey)))
           `(org-tag                                              ((t :foreground ,faded)))
           `(org-todo                                             ((t :foreground ,bright-red)))
           `(org-verbatim                                         ((t :foreground ,faded)))
           `(org-warning                                          ((t :foreground ,bright-orange)))
           `(outline-1                                            ((t :foreground ,white)))
           `(outline-2                                            ((t :foreground ,white)))
           `(outline-3                                            ((t :foreground ,white)))
           `(outline-4                                            ((t :foreground ,white)))
           `(outline-5                                            ((t :foreground ,white)))
           `(outline-6                                            ((t :foreground ,white)))
           `(outline-7                                            ((t :foreground ,white)))
           `(outline-8                                            ((t :foreground ,white)))
           `(outline-minor-0                                      ((t :foreground ,white)))
           `(outline-minor-1                                      ((t :foreground ,white)))
           `(outline-minor-2                                      ((t :foreground ,white)))
           `(outline-minor-3                                      ((t :foreground ,white)))
           `(outline-minor-4                                      ((t :foreground ,white)))
           `(outline-minor-5                                      ((t :foreground ,white)))
           `(outline-minor-6                                      ((t :foreground ,white)))
           `(outline-minor-7                                      ((t :foreground ,white)))
           `(outline-minor-8                                      ((t :foreground ,white)))
           `(package-name                                         ((t :foreground ,bright-orange)))
           `(pdf-view-region                                      ((t :background ,grey)))
           `(proced-cpu                                           ((t :weight bold)))
           `(proced-emacs-pid                                     ((t :foreground ,bright-blue)))
           `(proced-executable                                    ((t :foreground ,white)))
           `(proced-interruptible-sleep-status-code               ((t :foreground ,bright-red)))
           `(proced-mark                                          ((t :foreground ,bright-orange)))
           `(proced-marked                                        ((t :foreground ,bright-orange)))
           `(proced-mem                                           ((t :foreground ,bright-blue)))
           `(proced-memory-high-usage                             ((t :foreground ,bright-red)))
           `(proced-memory-low-usage                              ((t :foreground ,bright-green)))
           `(proced-memory-medium-usage                           ((t :foreground ,bright-yellow)))
           `(proced-pid                                           ((t :foreground ,faded)))
           `(proced-ppid                                          ((t :foreground ,faded)))
           `(proced-run-status-code                               ((t :foreground ,faded)))
           `(proced-sess                                          ((t :foreground ,faded)))
           `(proced-session-leader-pid                            ((t :foreground ,faded)))
           `(proced-sort-header                                   ((t :slant italic)))
           `(proced-time-colon                                    ((t :foreground ,faded)))
           `(proced-uninterruptible-sleep-status-code             ((t :foreground ,bright-red)))
           `(proced-user                                          ((t :foreground ,bright-magenta)))
           `(pulse-highlight-start-face                           ((t :background ,dim)))
           `(query-replace                                        ((t :inverse-video t :weight bold)))
           `(region                                               ((t :background ,grey)))
           `(restclient-header-name-face                          ((t :foreground ,bright-red)))
           `(restclient-header-value-face                         ((t :foreground ,bright-cyan)))
           `(restclient-method-face                               ((t :foreground ,bright-magenta)))
           `(restclient-request-hook-args-face                    ((t :foreground ,bright-green)))
           `(restclient-variable-name-face                        ((t :foreground ,bright-yellow)))
           `(shortdoc-heading                                     ((t :foreground ,white :weight bold)))
           `(show-paren-match                                     ((t :foreground ,white :weight heavy)))
           `(show-paren-mismatch                                  ((t :foreground ,bright-red :weight bold)))
           `(shr-h1                                               ((t :foreground ,white)))
           `(shr-h2                                               ((t :foreground ,white)))
           `(shr-h3                                               ((t :foreground ,white)))
           `(shr-h4                                               ((t :foreground ,white)))
           `(shr-h5                                               ((t :foreground ,white)))
           `(shr-h6                                               ((t :foreground ,white)))
           `(shr-link                                             ((t :underline t)))
           `(shr-selected-link                                    ((t :underline t)))
           `(shr-strike-through                                   ((t :strike-through t)))
           `(separator-line                                       ((t :background ,dark :height 1)))
           `(tab-bar                                              ((t :background ,dark :box (:line-width (2 . 2) :color ,dark :style nil))))
           `(tab-bar-tab                                          ((t :foreground ,white)))
           `(tab-bar-tab-inactive                                 ((t :foreground ,faded)))
           `(term-bold                                            ((t :weight bold)))
           `(term-color-black                                     ((t :foreground ,black)))
           `(term-color-blue                                      ((t :foreground ,bright-blue)))
           `(term-color-bright-black                              ((t :foreground ,black)))
           `(term-color-bright-blue                               ((t :foreground ,bright-blue)))
           `(term-color-bright-cyan                               ((t :foreground ,bright-cyan)))
           `(term-color-bright-green                              ((t :foreground ,bright-green)))
           `(term-color-bright-magenta                            ((t :foreground ,bright-magenta)))
           `(term-color-bright-red                                ((t :foreground ,bright-red)))
           `(term-color-bright-white                              ((t :foreground ,white)))
           `(term-color-bright-yellow                             ((t :foreground ,bright-yellow)))
           `(term-color-cyan                                      ((t :foreground ,bright-cyan)))
           `(term-color-green                                     ((t :foreground ,bright-green)))
           `(term-color-magenta                                   ((t :foreground ,bright-magenta)))
           `(term-color-red                                       ((t :foreground ,bright-red)))
           `(term-color-white                                     ((t :foreground ,white)))
           `(term-color-yellow                                    ((t :foreground ,bright-yellow)))
           `(tex-math                                             ((t :foreground ,bright-cyan)))
           `(tooltip                                              ((t :background ,dark :foreground ,bright)))
           `(trailing-whitespace                                  ((t :underline (:color ,grey-orange :style wave :position nil))))
           `(transient-argument                                   ((t :foreground ,bright-yellow)))
           `(transient-blue                                       ((t :foreground ,bright-blue)))
           `(transient-disabled-suffix                            ((t :foreground ,faded)))
           `(transient-heading                                    ((t :foreground ,white :weight bold)))
           `(transient-inactive-argument                          ((t :foreground ,faded)))
           `(transient-inactive-value                             ((t :foreground ,faded)))
           `(transient-key                                        ((t :foreground ,white)))
           `(transient-key-exit                                   ((t :foreground ,white)))
           `(transient-key-noop                                   ((t :foreground ,white)))
           `(transient-key-return                                 ((t :foreground ,white)))
           `(transient-key-stay                                   ((t :foreground ,white)))
           `(transient-mismatched-key                             ((t :foreground ,bright-red)))
           `(transient-pink                                       ((t :foreground ,bright-magenta)))
           `(transient-purple                                     ((t :foreground ,bright-purple)))
           `(transient-red                                        ((t :foreground ,bright-red)))
           `(transient-unreachable                                ((t :foreground ,faded)))
           `(transient-unreachable-key                            ((t :foreground ,faded)))
           `(typescript-ts-jsx-tag-face                           ((t :weight bold)))
           `(underline                                            ((t :underline t)))
           `(undo-hl-delete                                       ((t :background ,grey-red)))
           `(undo-hl-insert                                       ((t :background ,grey-green)))
           `(vertical-border                                      ((t :foreground ,dark)))
           `(vertico-current                                      ((t :background ,dark)))
           `(vertico-group-separator                              ((t :overline ,dark)))
           `(vertico-group-title                                  ((t :foreground ,white :overline ,dark :weight bold)))
           `(vertico-multiline                                    ((t :foreground ,bright-magenta)))
           `(warning                                              ((t :foreground ,bright-orange)))
           `(widget-button                                        ((t :underline t)))
           `(widget-button-pressed                                ((t :underline t)))
           `(widget-documentation                                 ((t :underline t)))
           `(widget-field                                         ((t :background ,dark)))
           `(widget-inactive                                      ((t :foreground ,faded)))
           `(window-divider                                       ((t :foreground ,dark)))
           `(window-divider-first-pixel                           ((t :foreground ,dark)))
           `(window-divider-last-pixel                            ((t :foreground ,dark)))
           `(woman-addition                                       ((t :foreground ,bright-yellow)))
           `(woman-bold                                           ((t :foreground ,white)))
           `(woman-italic                                         ((t :slant italic)))
           `(woman-unknown                                        ((t :underline (:color ,grey :style wave :position nil))))))

         (faces-as-variables
          (list
           `(gnus-face-0                    ((t :foreground ,white)))
           `(gnus-face-4                    ((t :foreground ,faded :slant italic)))
           '(goto-address-url-face          ((t :underline t)))
           `(grep-context-face              ((t :background ,dark)))
           `(grep-error-face                ((t :foreground ,bright-red)))
           `(grep-hit-face                  ((t :foreground ,bright-yellow)))
           `(grep-match-face                ((t :inverse-video t :weight bold)))
           `(ibuffer-deletion-face          ((t :foreground ,bright-red)))
           `(ibuffer-filter-group-name-face ((t :foreground ,white :weight bold)))
           `(ibuffer-marked-face            ((t :foreground ,bright-orange :slant italic)))
           `(ibuffer-title-face             ((t :foreground ,faded))))))

      ;; Set `faces-as-faces' as faces of theme.
      (apply #'custom-theme-set-faces name faces-as-faces)

      ;; For each face-as-variable, declare a face.  To do that,
      ;; prefix the symbol with name-.
      (pcase-dolist (`(,sy _) faces-as-variables)
        (custom-declare-face
         (intern (concat (symbol-name name) "-" (symbol-name sy)))
         nil nil :group 'evenok))

      ;; For each face-as-variable, set it to the previously declared
      ;; face.
      (apply #'custom-theme-set-variables name
             (mapcar
              (pcase-lambda (`(,sy _))
                `(,sy
                  (quote ,(intern (concat (symbol-name name) "-"
                                          (symbol-name sy))))
                  t))
              faces-as-variables))

      ;; For each face-as-variable, initialize the previously declared
      ;; face.
      (apply #'custom-theme-set-faces name
             (mapcar
              (pcase-lambda (`(,sy ,sp))
                (list
                 (intern (concat (symbol-name name) "-"
                                 (symbol-name sy)))
                 sp t))
              faces-as-variables))

      ;;
      (custom-theme-set-variables
       name
       '(highlight-parentheses-colors nil)))))

;;;###autoload
(when load-file-name
  (require 'custom)
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide 'evenok)

;;; evenok.el ends here
