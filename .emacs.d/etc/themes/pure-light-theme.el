;;; pure-light-theme.el --- A light colored theme for e-ink color monitors -*- lexical-binding: t; -*-
;;
;; Author: Mattias Nyrell <mattias@nyrell.se>
;; Maintainer: Mattias Nyrell <mattias@nyrell.se>
;; Version: 1.0.0
;; Keywords: faces, theme
;; URL: https://github.com/nyrell/pure-light-eink-color
;; Package-Requires: ((emacs "27.1"))
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; A light theme using only pure colors (#000000, #FF0000, #00FF00, #0000FF,
;; #FFFF00, #FF00FF, #00FFFF, #FFFFFF) designed for e-ink color monitors based
;; on the Kaleido 3 panel (e.g. Dasung Paperlike 13K color).  The palette avoids
;; dithering by staying within colors each RGB channel can render cleanly.

;;; Code:

(deftheme pure-light
  "A light theme using only pure colors (#000000, #FF0000, #00FF00, #0000FF, #FFFF00, #FF00FF, #00FFFF, #FFFFFF).")

(let ((pure-black "#000000")
      (pure-red "#FF0000")
      (pure-green "#00FF00")
      (pure-blue "#0000FF")
      (pure-yellow "#FFFF00")
      (pure-magenta "#FF00FF")
      (pure-cyan "#00FFFF")
      (pure-white "#FFFFFF"))
  
  (custom-theme-set-faces
   'pure-light

   ;; Basic faces
   `(default ((t (:foreground ,pure-black :background ,pure-white))))
   `(cursor ((t (:background ,pure-black))))
   `(region ((t (:background ,pure-yellow))))
   `(highlight ((t (:background ,pure-cyan))))
   `(fringe ((t (:background ,pure-white))))
   `(isearch ((t (:foreground ,pure-black :background ,pure-cyan))))
   `(isearch-fail ((t (:foreground ,pure-white :background ,pure-red))))
   `(lazy-highlight ((t (:foreground ,pure-black :background ,pure-yellow))))
   `(minibuffer-prompt ((t (:foreground ,pure-blue))))
   `(show-paren-match ((t (:foreground ,pure-black :background ,pure-cyan))))
   `(show-paren-mismatch ((t (:foreground ,pure-white :background ,pure-red))))
   `(line-number ((t (:foreground ,pure-black :background ,pure-white))))
   `(line-number-current-line ((t (:foreground ,pure-black :background ,pure-white :weight bold))))
   `(hl-line ((t (:background ,pure-cyan))))
   `(match ((t (:foreground ,pure-black :background ,pure-green))))
   `(header-line ((t (:foreground ,pure-black :background ,pure-cyan))))
   `(mode-line ((t (:foreground ,pure-black :background ,pure-cyan))))
   `(mode-line-inactive ((t (:foreground ,pure-white :weight bold :background ,pure-blue))))

   ;; Tab-bar faces (Emacs 27+ tab-bar-mode)
   `(tab-bar ((t (:foreground ,pure-black :background ,pure-white :underline ,pure-white))))
   `(tab-bar-tab ((t (:foreground ,pure-black :background ,pure-cyan))))
   `(tab-bar-tab-inactive ((t (:foreground ,pure-white :weight bold :background ,pure-blue))))

   ;; Tab-line faces (Emacs 27+ tab-line-mode)
   `(tab-line ((t (:foreground ,pure-black :background ,pure-white))))
   `(tab-line-tab ((t (:foreground ,pure-black :background ,pure-cyan))))
   `(tab-line-tab-current ((t (:foreground ,pure-black :background ,pure-cyan))))
   `(tab-line-tab-inactive ((t (:foreground ,pure-white :weight bold :background ,pure-blue))))
   `(tab-line-highlight ((t (:foreground ,pure-black :background ,pure-cyan))))

   ;; Font-lock faces
   `(font-lock-comment-face ((t (:foreground ,pure-red))))
   `(font-lock-doc-face ((t (:foreground ,pure-blue))))
   `(font-lock-string-face ((t (:foreground ,pure-blue))))
   `(font-lock-keyword-face ((t (:weight bold :foreground ,pure-black))))
   `(font-lock-function-name-face ((t (:foreground ,pure-magenta))))
   `(font-lock-variable-name-face ((t (:foreground ,pure-black))))
   `(font-lock-type-face ((t (:foreground ,pure-blue))))
   `(font-lock-constant-face ((t (:foreground ,pure-magenta))))
   `(font-lock-builtin-face ((t (:foreground ,pure-blue))))
   `(font-lock-warning-face ((t (:foreground ,pure-magenta))))

   ;; Compilation faces
   `(compilation-error ((t (:foreground ,pure-red :weight bold))))
   `(compilation-warning ((t (:foreground ,pure-magenta :weight bold))))
   `(compilation-info ((t (:foreground ,pure-green))))
   `(compilation-mode-line-exit ((t (:foreground ,pure-green :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,pure-red :weight bold))))

   ;; Flycheck faces
   `(flycheck-error ((t (:underline (:color ,pure-red :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,pure-magenta :style wave)))))
   `(flycheck-info ((t (:underline (:color ,pure-blue :style wave)))))

   ;; Flymake faces
   `(flymake-error ((t (:underline (:color ,pure-red :style wave)))))
   `(flymake-warning ((t (:underline (:color ,pure-magenta :style wave)))))
   `(flymake-note ((t (:underline (:color ,pure-blue :style wave)))))

   ;; Diff-hl faces (fringe git indicators)
   `(diff-hl-insert ((t (:foreground ,pure-green :background ,pure-green))))
   `(diff-hl-delete ((t (:foreground ,pure-red :background ,pure-red))))
   `(diff-hl-change ((t (:foreground ,pure-blue :background ,pure-blue))))

   ;; Ediff faces
   `(ediff-current-diff-A ((t (:background ,pure-red :foreground ,pure-white))))
   `(ediff-fine-diff-A    ((t (:background ,pure-magenta :foreground ,pure-white :weight bold))))
   `(ediff-current-diff-B ((t (:background ,pure-green :foreground ,pure-white))))
   `(ediff-fine-diff-B    ((t (:background ,pure-cyan :foreground ,pure-black :weight bold))))
   ;; Use the same color scheme for the inactive diffs. Yellow on Black clashes with regions, but I
   ;; think that is a lesser problem in ediff-mode
   `(ediff-odd-diff-A ((t (:background ,pure-yellow :foreground ,pure-black)))) 
   `(ediff-odd-diff-B ((t (:background ,pure-yellow :foreground ,pure-black))))
   `(ediff-even-diff-A ((t (:background ,pure-yellow :foreground ,pure-black))))
   `(ediff-even-diff-B ((t (:background ,pure-yellow :foreground ,pure-black))))

   ;; Org-mode faces
   `(org-level-1 ((t (:foreground ,pure-black :weight bold))))
   `(org-level-2 ((t (:foreground ,pure-blue :weight bold))))
   `(org-level-3 ((t (:foreground ,pure-green))))
   `(org-level-4 ((t (:foreground ,pure-red))))
   `(org-level-5 ((t (:foreground ,pure-black))))
   `(org-level-6 ((t (:foreground ,pure-black))))
   `(org-level-7 ((t (:foreground ,pure-black))))
   `(org-level-8 ((t (:foreground ,pure-black))))
   `(org-code ((t (:foreground ,pure-blue))))
   `(org-verbatim ((t (:foreground ,pure-blue))))
   `(org-link ((t (:foreground ,pure-blue :underline t))))
   `(org-block ((t (:foreground ,pure-blue))))
   `(org-block-begin-line ((t (:foreground ,pure-red))))
   `(org-block-end-line ((t (:foreground ,pure-red))))
   `(org-meta-line ((t (:foreground ,pure-red))))
   `(org-todo ((t (:foreground ,pure-red :weight bold))))
   `(org-done ((t (:foreground ,pure-green :weight bold))))
   `(org-tag ((t (:foreground ,pure-blue))))

   ;; Markdown faces (mirrors org-mode heading colors)
   `(markdown-header-face-1 ((t (:foreground ,pure-black :weight bold))))
   `(markdown-header-face-2 ((t (:foreground ,pure-blue :weight bold))))
   `(markdown-header-face-3 ((t (:foreground ,pure-green))))
   `(markdown-header-face-4 ((t (:foreground ,pure-red))))
   `(markdown-header-face-5 ((t (:foreground ,pure-black))))
   `(markdown-header-face-6 ((t (:foreground ,pure-black))))
   `(markdown-code-face ((t (:foreground ,pure-blue))))
   `(markdown-inline-code-face ((t (:foreground ,pure-blue))))
   `(markdown-link-face ((t (:foreground ,pure-blue :underline t))))
   `(markdown-url-face ((t (:foreground ,pure-blue :underline t))))
   `(markdown-bold-face ((t (:foreground ,pure-black :weight bold))))
   `(markdown-italic-face ((t (:foreground ,pure-black :slant italic))))
   `(markdown-comment-face ((t (:foreground ,pure-red))))

   ;; Dired faces
   `(dired-directory ((t (:foreground ,pure-blue :background ,pure-white))))
   `(dired-symlink ((t (:foreground ,pure-magenta))))
   `(dired-flagged ((t (:foreground ,pure-white :background ,pure-red))))

   ;; Magit diff faces (mirrors ediff color scheme)
   `(magit-diff-removed ((t (:foreground ,pure-white :background ,pure-red))))
   `(magit-diff-removed-highlight ((t (:foreground ,pure-white :background ,pure-red :weight bold))))
   `(magit-diff-added ((t (:foreground ,pure-white :background ,pure-green))))
   `(magit-diff-added-highlight ((t (:foreground ,pure-white :background ,pure-green :weight bold))))
   `(magit-diff-base ((t (:foreground ,pure-black :background ,pure-yellow))))
   `(magit-diff-base-highlight ((t (:foreground ,pure-black :background ,pure-yellow :weight bold))))
   `(magit-diff-context ((t (:foreground ,pure-black :background ,pure-white))))
   `(magit-diff-context-highlight ((t (:foreground ,pure-black :background ,pure-white))))
   `(magit-diff-hunk-heading ((t (:foreground ,pure-black :background ,pure-yellow))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,pure-black :background ,pure-yellow :weight bold))))
   `(magit-diff-file-heading ((t (:foreground ,pure-black :weight bold))))
   `(magit-diff-file-heading-highlight ((t (:foreground ,pure-black :weight bold :background ,pure-white))))
   `(magit-diffstat-added ((t (:foreground ,pure-green))))
   `(magit-diffstat-removed ((t (:foreground ,pure-red))))
   `(magit-section-heading ((t (:foreground ,pure-white :background ,pure-blue :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,pure-white :background ,pure-blue :weight bold))))
   `(magit-section-highlight ((t (:background unspecified))))
   `(magit-branch-local ((t (:foreground ,pure-green :weight bold))))
   `(magit-branch-remote ((t (:foreground ,pure-blue :weight bold))))
   `(magit-branch-current ((t (:foreground ,pure-black :background ,pure-cyan :weight bold))))
   `(magit-tag ((t (:foreground ,pure-red :weight bold))))
   `(magit-hash ((t (:foreground ,pure-blue))))
   `(magit-log-author ((t (:foreground ,pure-magenta))))
   `(magit-log-date ((t (:foreground ,pure-black))))
   `(magit-process-ok ((t (:foreground ,pure-green :weight bold))))
   `(magit-process-ng ((t (:foreground ,pure-red :weight bold))))
   `(magit-blame-highlight ((t (:foreground ,pure-black :background ,pure-yellow))))

   ;; Other faces
   `(link ((t (:foreground ,pure-blue :underline t))))
   `(error ((t (:foreground ,pure-red))))
   `(warning ((t (:foreground ,pure-magenta))))
   `(success ((t (:foreground ,pure-green)))) ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'pure-light)

;;; pure-light-theme.el ends here
