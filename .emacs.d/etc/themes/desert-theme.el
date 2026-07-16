;;; desert-theme.el --- A warm earthy port of Vim's desert theme  -*- lexical-binding: t; -*-
;;
;; Author: Ben Carriel <notthefather@pm.me>
;; Maintainer: Ben Carriel <notthefather@pm.me>
;; Assisted-by: Claude:claude-opus-4-8
;; URL: https://github.com/bkc39/desert-theme
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces theme vim desert
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;; Public domain / CC0-1.0.  Do whatever you want.
;; SPDX-License-Identifier: CC0-1.0

;;; Commentary:
;;
;; This is an Emacs port of the classic "desert" colorscheme from Vim.
;; It tries to preserve:
;; - dark warm gray background
;; - golden/yellow comments
;; - soft greens for strings
;; - sandy/orange accents for variables and constants
;; - gentle blue for function names / types
;;
;; To use:
;;   (load-theme 'desert t)
;;
;; Or with straight.el + use-package:
;;   (use-package desert-theme
;;     :straight (desert-theme
;;                :type git
;;                :host github
;;                :repo "bkc39/desert-theme")
;;     :config
;;     (load-theme 'desert t))
;;

;;; Code:

(deftheme desert
  "A warm earthy port of Vim's desert theme.")

;; Color palette (hex values chosen to resemble Vim desert.vim)
(let* (
       ;; Base tones
       (desert-fg         "#ffffff")
       (desert-bg         "#333333")
       (desert-bg-alt     "#2a2a2a")
       (desert-selection  "#555555")
       (desert-highlight  "#444444")
       (desert-cursor     "#ffa500") ;; orange

       ;; Syntax-ish colors
       (desert-comment    "#cc9900") ;; warm gold/brown
       (desert-string     "#99cc99") ;; pale green
       (desert-keyword    "#ffcc66") ;; sandy yellow/orange
       (desert-func       "#99ccff") ;; light blue
       (desert-type       "#66ccff") ;; cyan-ish blue
       (desert-var        "#ff9966") ;; orange-peach
       (desert-const      "#ffcc99") ;; pale peach
       (desert-warning    "#ff6600") ;; stronger orange
       )

  (custom-theme-set-faces
   'desert

   ;; Basic UI
   `(default ((t (:background ,desert-bg :foreground ,desert-fg))))
   `(cursor  ((t (:background ,desert-cursor))))
   `(region  ((t (:background ,desert-selection))))
   `(highlight ((t (:background ,desert-highlight))))
   `(fringe  ((t (:background ,desert-bg))))
   `(vertical-border ((t (:foreground ,desert-highlight :background ,desert-bg))))
   `(link    ((t (:foreground ,desert-func :underline t))))
   `(minibuffer-prompt ((t (:foreground ,desert-keyword :weight bold))))

   ;; Line numbers
   `(line-number              ((t (:background ,desert-bg :foreground ,desert-comment))))
   `(line-number-current-line ((t (:background ,desert-bg-alt :foreground ,desert-keyword :weight bold))))

   ;; Font lock (core syntax highlighting)
   `(font-lock-builtin-face           ((t (:foreground ,desert-keyword))))
   `(font-lock-comment-face           ((t (:foreground ,desert-comment :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face          ((t (:foreground ,desert-const))))
   `(font-lock-doc-face               ((t (:foreground ,desert-comment :slant italic))))
   `(font-lock-function-name-face     ((t (:foreground ,desert-func :weight normal))))
   `(font-lock-keyword-face           ((t (:foreground ,desert-keyword :weight bold))))
   `(font-lock-string-face            ((t (:foreground ,desert-string))))
   `(font-lock-type-face              ((t (:foreground ,desert-type :weight normal))))
   `(font-lock-variable-name-face     ((t (:foreground ,desert-var))))
   `(font-lock-warning-face           ((t (:foreground ,desert-warning :weight bold))))

   ;; Mode line
   `(mode-line
     ((t (:background ,desert-highlight
          :foreground ,desert-fg
          :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive
     ((t (:background ,desert-bg-alt
          :foreground ,desert-comment
          :box (:line-width -1 :style released-button)))))

   ;; Success / warning / error (for flycheck, etc.)
   `(success ((t (:foreground ,desert-string :weight bold))))
   `(warning ((t (:foreground ,desert-warning :weight bold))))
   `(error   ((t (:foreground "red" :weight bold))))

   ;; Search
   `(isearch ((t (:background ,desert-keyword :foreground ,desert-bg :weight bold))))
   `(lazy-highlight ((t (:background ,desert-selection :foreground ,desert-keyword :weight bold))))

   ;; Show-paren
   `(show-paren-match    ((t (:background ,desert-keyword :foreground ,desert-bg :weight bold))))
   `(show-paren-mismatch ((t (:background "red" :foreground ,desert-bg :weight bold))))

   ;; Whitespace-mode
   `(whitespace-space ((t (:foreground ,desert-highlight))))
   `(whitespace-tab   ((t (:foreground ,desert-highlight))))
   `(whitespace-trailing ((t (:background ,desert-warning :foreground ,desert-bg :weight bold))))

   ;; which-key, helpful, etc. (nice but optional)
   `(which-key-key-face ((t (:foreground ,desert-keyword :weight bold))))
   `(which-key-command-description-face ((t (:foreground ,desert-fg))))
   `(which-key-separator-face ((t (:foreground ,desert-comment))))

   ;; org-mode basics
   `(org-level-1 ((t (:foreground ,desert-keyword :weight bold :height 1.2))))
   `(org-level-2 ((t (:foreground ,desert-func    :weight bold :height 1.1))))
   `(org-level-3 ((t (:foreground ,desert-var     :weight bold))))
   `(org-level-4 ((t (:foreground ,desert-type))))
   `(org-code    ((t (:background ,desert-bg-alt :foreground ,desert-string :inherit fixed-pitch))))
   `(org-block   ((t (:background ,desert-bg-alt :extend t))))
   `(org-block-begin-line ((t (:background ,desert-bg-alt :foreground ,desert-comment :extend t :height 0.9))))
   `(org-block-end-line   ((t (:background ,desert-bg-alt :foreground ,desert-comment :extend t :height 0.9))))

   ;; minibuffer completion UI (vertico/icomplete/etc. - generic-ish)
   `(completions-common-part ((t (:foreground ,desert-keyword :weight bold))))
   `(completions-first-difference ((t (:foreground ,desert-var :weight bold))))

   ;; diff / magit style faces
   `(diff-added   ((t (:background "#335533" :foreground ,desert-string :extend t))))
   `(diff-removed ((t (:background "#553333" :foreground ,desert-warning :extend t))))
   `(diff-changed ((t (:background "#444444" :foreground ,desert-type :extend t))))
   `(diff-refine-added   ((t (:background "#447744" :foreground ,desert-string :weight bold :extend t))))
   `(diff-refine-removed ((t (:background "#774444" :foreground ,desert-warning :weight bold :extend t))))
   `(diff-refine-changed ((t (:background "#555555" :foreground ,desert-type :weight bold :extend t))))

   ;; magit headings
   `(magit-section-heading        ((t (:foreground ,desert-keyword :weight bold))))
   `(magit-branch-local           ((t (:foreground ,desert-var :weight bold))))
   `(magit-branch-remote          ((t (:foreground ,desert-string :weight bold))))
   `(magit-diff-context-highlight ((t (:background ,desert-bg-alt :foreground ,desert-fg :extend t)))))

  ;; Variables (theme-level tweaks)
  (custom-theme-set-variables
   'desert
   ;; Make the region use our region face in all situations
   '(highlight-tail-colors nil)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  ;; Add the directory of this file to `custom-theme-load-path`
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'desert)
;;; desert-theme.el ends here
