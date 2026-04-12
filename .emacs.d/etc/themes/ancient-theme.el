;;; ancient-theme.el --- A theme about ruins -*- lexical-binding: t -*-

;; Author: Thomas Bestvina <bestvinathomas@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: faces, theme
;; URL: https://github.com/thomasbestvina/ancient-theme
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; This theme inspired by kenshi/qud

;;; Code:

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(deftheme ancient "A theme about ruins.")

(let* (;; Backgrounds
       (bg-deep    "#0e0c09")
       (bg-base    "#1a1710")
       (bg-surface "#2d2820")
       (bg-overlay "#4a4234")

       ;; Foregrounds
       (fg-muted   "#665a48")
       (fg-subtle  "#8a7a64")
       (fg-dim     "#c8b89a")
       (fg-main    "#e8dcc8")
       (fg-bright  "#f0e8d4")

       ;; Green / verdigris
       (green-dim  "#2d6652")
       (green      "#3d8a6e")
       (green-hi   "#7ecfb4")

       ;; Rust / red
       (rust-dim   "#4c1c10")
       (rust       "#a84428")
       (rust-hi    "#e08c68")

       ;; Gold / yellow
       (gold-dim   "#5a4422")
       (gold       "#c8a05a")
       (gold-hi    "#e8cc90")

       ;; Blue / muted steel
       (blue-dim   "#2a4858")
       (blue       "#4a7a94")
       (blue-hi    "#7aacc0")

       ;; Magenta / dusty rose
       (magenta-dim "#4a2830")
       (magenta    "#8a4858")
       (magenta-hi "#c09080"))

  (custom-theme-set-faces
   'ancient

   ;; Core
   `(default                    ((t (:background ,bg-base :foreground ,fg-main))))
   `(cursor                     ((t (:background ,green))))
   `(region                     ((t (:background ,bg-overlay))))
   `(highlight                  ((t (:background ,bg-surface))))
   `(secondary-selection        ((t (:background ,bg-overlay))))
   `(fringe                     ((t (:background ,bg-base :foreground ,fg-muted))))
   `(vertical-border            ((t (:foreground ,bg-overlay))))
   `(window-divider             ((t (:foreground ,bg-overlay))))
   `(window-divider-first-pixel ((t (:foreground ,bg-surface))))
   `(window-divider-last-pixel  ((t (:foreground ,bg-deep))))
   `(minibuffer-prompt          ((t (:foreground ,green :weight normal))))
   `(link                       ((t (:foreground ,green-hi :underline t))))
   `(link-visited               ((t (:foreground ,gold :underline t))))
   `(button                     ((t (:foreground ,blue-hi :underline t))))
   `(warning                    ((t (:foreground ,gold))))
   `(error                      ((t (:foreground ,rust-hi))))
   `(success                    ((t (:foreground ,green-hi))))
   `(escape-glyph               ((t (:foreground ,rust-hi))))
   `(homoglyph                  ((t (:foreground ,gold-hi))))
   `(nobreak-space              ((t (:foreground ,rust :underline t))))
   `(shadow                     ((t (:foreground ,fg-muted))))

   ;; Line numbers
   `(line-number                ((t (:background ,bg-base :foreground ,bg-overlay))))
   `(line-number-current-line   ((t (:background ,bg-base :foreground ,fg-muted))))
   `(line-number-major-tick     ((t (:background ,bg-base :foreground ,fg-subtle))))
   `(line-number-minor-tick     ((t (:background ,bg-base :foreground ,bg-overlay))))

   ;; Mode line
   `(mode-line                  ((t (:background ,bg-surface :foreground ,fg-subtle
                                     :box (:line-width 1 :color ,bg-overlay)))))
   `(mode-line-inactive         ((t (:background ,bg-base :foreground ,fg-muted
                                     :box (:line-width 1 :color ,bg-surface)))))
   `(mode-line-buffer-id        ((t (:foreground ,gold :weight normal))))
   `(mode-line-emphasis         ((t (:foreground ,green))))
   `(mode-line-highlight        ((t (:foreground ,green-hi))))

   ;; Search / match
   `(isearch                    ((t (:background ,green-dim :foreground ,fg-bright))))
   `(isearch-fail               ((t (:background ,rust-dim :foreground ,rust-hi))))
   `(isearch-group-1            ((t (:background ,blue-dim :foreground ,blue-hi))))
   `(isearch-group-2            ((t (:background ,magenta-dim :foreground ,magenta-hi))))
   `(lazy-highlight             ((t (:background ,bg-overlay :foreground ,fg-subtle))))
   `(match                      ((t (:background ,green-dim :foreground ,fg-bright))))
   `(query-replace              ((t (:background ,gold-dim :foreground ,gold-hi))))

   ;; Font lock
   `(font-lock-comment-face          ((t (:foreground ,fg-muted :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg-muted))))
   `(font-lock-doc-face              ((t (:foreground ,gold-dim :slant italic))))
   `(font-lock-doc-markup-face       ((t (:foreground ,gold))))
   `(font-lock-string-face           ((t (:foreground ,gold))))
   `(font-lock-keyword-face          ((t (:foreground ,green))))
   `(font-lock-builtin-face          ((t (:foreground ,green-hi))))
   `(font-lock-function-name-face    ((t (:foreground ,fg-bright :weight normal))))
   `(font-lock-function-call-face    ((t (:foreground ,fg-main))))
   `(font-lock-variable-name-face    ((t (:foreground ,fg-main))))
   `(font-lock-variable-use-face     ((t (:foreground ,fg-dim))))
   `(font-lock-type-face             ((t (:foreground ,gold-hi))))
   `(font-lock-constant-face         ((t (:foreground ,rust-hi))))
   `(font-lock-preprocessor-face     ((t (:foreground ,rust))))
   `(font-lock-warning-face          ((t (:foreground ,rust :weight normal))))
   `(font-lock-negation-char-face    ((t (:foreground ,rust))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,gold-hi))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,gold))))
   `(font-lock-number-face           ((t (:foreground ,blue-hi))))
   `(font-lock-operator-face         ((t (:foreground ,fg-subtle))))
   `(font-lock-property-name-face    ((t (:foreground ,fg-main))))
   `(font-lock-property-use-face     ((t (:foreground ,fg-dim))))
   `(font-lock-delimiter-face        ((t (:foreground ,fg-subtle))))
   `(font-lock-bracket-face          ((t (:foreground ,fg-subtle))))
   `(font-lock-escape-face           ((t (:foreground ,rust-hi))))
   `(font-lock-misc-punctuation-face ((t (:foreground ,fg-subtle))))

   ;; Parens
   `(show-paren-match           ((t (:background ,green-dim :foreground ,fg-bright))))
   `(show-paren-mismatch        ((t (:background ,rust-dim :foreground ,rust-hi))))
   `(show-paren-match-expression ((t (:background ,bg-surface))))

   ;; Diffs
   `(diff-added                 ((t (:background "#122a20" :foreground ,green-hi))))
   `(diff-removed               ((t (:background ,rust-dim :foreground ,rust-hi))))
   `(diff-changed               ((t (:background "#2a2410" :foreground ,gold-hi))))
   `(diff-refine-added          ((t (:background "#1e4434" :foreground ,green-hi))))
   `(diff-refine-removed        ((t (:background "#7a2e18" :foreground ,rust-hi))))
   `(diff-refine-changed        ((t (:background "#4a3c10" :foreground ,gold-hi))))
   `(diff-header                ((t (:background ,bg-surface :foreground ,fg-subtle))))
   `(diff-file-header           ((t (:background ,bg-surface :foreground ,fg-main))))
   `(diff-hunk-header           ((t (:background ,bg-surface :foreground ,blue-hi))))
   `(diff-context               ((t (:foreground ,fg-muted))))

   ;; Completion - company
   `(company-tooltip            ((t (:background ,bg-surface :foreground ,fg-main))))
   `(company-tooltip-selection  ((t (:background ,bg-overlay :foreground ,fg-bright))))
   `(company-tooltip-common     ((t (:foreground ,green))))
   `(company-tooltip-annotation ((t (:foreground ,fg-muted))))
   `(company-tooltip-annotation-selection ((t (:foreground ,fg-subtle))))
   `(company-tooltip-search     ((t (:background ,green-dim :foreground ,fg-bright))))
   `(company-scrollbar-bg       ((t (:background ,bg-surface))))
   `(company-scrollbar-fg       ((t (:background ,bg-overlay))))
   `(company-preview-common     ((t (:foreground ,fg-muted))))
   `(company-preview            ((t (:background ,bg-surface))))

   ;; Completion - corfu
   `(corfu-default              ((t (:background ,bg-surface :foreground ,fg-main))))
   `(corfu-current              ((t (:background ,bg-overlay :foreground ,fg-bright))))
   `(corfu-bar                  ((t (:background ,green-dim))))
   `(corfu-border               ((t (:background ,bg-overlay))))
   `(corfu-annotations          ((t (:foreground ,fg-muted))))
   `(corfu-popupinfo            ((t (:background ,bg-deep :foreground ,fg-dim))))

   ;; Completion — vertico / marginalia
   `(vertico-current            ((t (:background ,bg-overlay :foreground ,fg-bright))))
   `(marginalia-documentation   ((t (:foreground ,fg-muted :slant italic))))
   `(marginalia-type            ((t (:foreground ,gold-hi))))
   `(marginalia-file-priv-dir   ((t (:foreground ,blue))))
   `(marginalia-file-priv-exec  ((t (:foreground ,green))))
   `(completions-common-part    ((t (:foreground ,green))))
   `(completions-first-difference ((t (:foreground ,fg-bright))))

   ;; Evil
   `(evil-ex-info               ((t (:foreground ,rust-hi))))
   `(evil-ex-substitute-matches ((t (:background ,rust-dim :foreground ,rust-hi))))
   `(evil-ex-substitute-replacement ((t (:background ,green-dim :foreground ,green-hi))))
   `(evil-ex-lazy-highlight     ((t (:background ,bg-overlay :foreground ,fg-subtle))))

   ;; Org
   `(org-level-1                ((t (:foreground ,gold-hi :weight normal :height 1.15))))
   `(org-level-2                ((t (:foreground ,gold :weight normal))))
   `(org-level-3                ((t (:foreground ,green-hi :weight normal))))
   `(org-level-4                ((t (:foreground ,green :weight normal))))
   `(org-level-5                ((t (:foreground ,blue-hi :weight normal))))
   `(org-level-6                ((t (:foreground ,blue :weight normal))))
   `(org-level-7                ((t (:foreground ,fg-main))))
   `(org-level-8                ((t (:foreground ,fg-dim))))
   `(org-todo                   ((t (:foreground ,rust :weight normal))))
   `(org-done                   ((t (:foreground ,fg-muted))))
   `(org-headline-done          ((t (:foreground ,fg-muted))))
   `(org-date                   ((t (:foreground ,gold))))
   `(org-date-selected          ((t (:background ,gold-dim :foreground ,gold-hi))))
   `(org-code                   ((t (:foreground ,green-hi :background ,bg-surface))))
   `(org-verbatim               ((t (:foreground ,gold-hi :background ,bg-surface))))
   `(org-block                  ((t (:background ,bg-deep :foreground ,fg-dim))))
   `(org-block-begin-line       ((t (:foreground ,fg-muted :background ,bg-deep))))
   `(org-block-end-line         ((t (:foreground ,fg-muted :background ,bg-deep))))
   `(org-meta-line              ((t (:foreground ,fg-muted))))
   `(org-document-title         ((t (:foreground ,gold-hi :weight normal :height 1.3))))
   `(org-document-info          ((t (:foreground ,fg-subtle))))
   `(org-document-info-keyword  ((t (:foreground ,fg-muted))))
   `(org-tag                    ((t (:foreground ,fg-muted :weight normal))))
   `(org-agenda-date            ((t (:foreground ,blue-hi))))
   `(org-agenda-date-today      ((t (:foreground ,gold-hi :weight normal))))
   `(org-agenda-date-weekend    ((t (:foreground ,blue))))
   `(org-agenda-structure       ((t (:foreground ,fg-subtle))))
   `(org-scheduled              ((t (:foreground ,green))))
   `(org-scheduled-today        ((t (:foreground ,green-hi))))
   `(org-scheduled-previously   ((t (:foreground ,rust))))
   `(org-upcoming-deadline      ((t (:foreground ,gold))))
   `(org-warning                ((t (:foreground ,rust-hi))))
   `(org-link                   ((t (:foreground ,green-hi :underline t))))
   `(org-footnote               ((t (:foreground ,fg-subtle))))
   `(org-table                  ((t (:foreground ,fg-dim))))
   `(org-formula                ((t (:foreground ,blue-hi))))
   `(org-checkbox               ((t (:foreground ,gold))))
   `(org-priority               ((t (:foreground ,rust))))
   `(org-special-keyword        ((t (:foreground ,fg-muted))))

   ;; Magit
   `(magit-section-heading      ((t (:foreground ,gold :weight normal))))
   `(magit-section-heading-selection ((t (:foreground ,gold-hi :weight normal))))
   `(magit-section-highlight    ((t (:background ,bg-surface))))
   `(magit-diff-added           ((t (:background "#122a20" :foreground ,green-hi))))
   `(magit-diff-added-highlight ((t (:background "#1e4434" :foreground ,green-hi))))
   `(magit-diff-removed         ((t (:background ,rust-dim :foreground ,rust-hi))))
   `(magit-diff-removed-highlight ((t (:background "#7a2e18" :foreground ,rust-hi))))
   `(magit-diff-context         ((t (:foreground ,fg-muted))))
   `(magit-diff-context-highlight ((t (:background ,bg-surface :foreground ,fg-subtle))))
   `(magit-diff-hunk-heading    ((t (:background ,bg-surface :foreground ,fg-muted))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,bg-overlay :foreground ,fg-subtle))))
   `(magit-diff-base            ((t (:background "#2a2410" :foreground ,gold-hi))))
   `(magit-diff-base-highlight  ((t (:background "#4a3c10" :foreground ,gold-hi))))
   `(magit-hash                 ((t (:foreground ,fg-muted))))
   `(magit-branch-local         ((t (:foreground ,green))))
   `(magit-branch-remote        ((t (:foreground ,blue))))
   `(magit-branch-current       ((t (:foreground ,green-hi :box (:line-width -1 :color ,green)))))
   `(magit-tag                  ((t (:foreground ,gold))))
   `(magit-dimmed               ((t (:foreground ,fg-muted))))
   `(magit-filename             ((t (:foreground ,fg-main))))
   `(magit-log-author           ((t (:foreground ,gold))))
   `(magit-log-date             ((t (:foreground ,fg-muted))))
   `(magit-log-graph            ((t (:foreground ,bg-overlay))))
   `(magit-process-ok           ((t (:foreground ,green))))
   `(magit-process-ng           ((t (:foreground ,rust-hi))))
   `(magit-signature-good       ((t (:foreground ,green-hi))))
   `(magit-signature-bad        ((t (:foreground ,rust-hi))))
   `(magit-signature-untrusted  ((t (:foreground ,gold))))
   `(magit-blame-heading        ((t (:background ,bg-surface :foreground ,fg-muted))))
   `(magit-blame-highlight      ((t (:background ,bg-overlay :foreground ,fg-subtle))))

   ;; Dired
   `(dired-directory            ((t (:foreground ,green))))
   `(dired-symlink              ((t (:foreground ,blue-hi))))
   `(dired-broken-symlink       ((t (:foreground ,rust-hi :strike-through t))))
   `(dired-special              ((t (:foreground ,magenta))))
   `(dired-flagged              ((t (:foreground ,rust-hi :strike-through t))))
   `(dired-marked               ((t (:foreground ,gold))))
   `(dired-mark                 ((t (:foreground ,gold))))
   `(dired-header               ((t (:foreground ,green-hi :weight normal))))
   `(dired-ignored              ((t (:foreground ,fg-muted))))
   `(dired-perm-write           ((t (:foreground ,rust))))

   ;; Which-key
   `(which-key-key-face                ((t (:foreground ,green-hi))))
   `(which-key-command-description-face ((t (:foreground ,fg-main))))
   `(which-key-group-description-face  ((t (:foreground ,gold))))
   `(which-key-separator-face          ((t (:foreground ,fg-muted))))
   `(which-key-note-face               ((t (:foreground ,fg-muted :slant italic))))
   `(which-key-highlighted-command-face ((t (:foreground ,gold-hi))))
   `(which-key-special-key-face        ((t (:foreground ,rust-hi))))

   ;; Treemacs / neotree
   `(treemacs-root-face         ((t (:foreground ,gold-hi :weight normal))))
   `(treemacs-directory-face   ((t (:foreground ,green))))
   `(treemacs-file-face         ((t (:foreground ,fg-main))))
   `(treemacs-tags-face         ((t (:foreground ,fg-subtle))))
   `(treemacs-git-modified-face ((t (:foreground ,gold))))
   `(treemacs-git-added-face    ((t (:foreground ,green-hi))))
   `(treemacs-git-deleted-face  ((t (:foreground ,rust-hi))))
   `(treemacs-git-ignored-face  ((t (:foreground ,fg-muted))))
   `(treemacs-git-untracked-face ((t (:foreground ,blue-hi))))

   ;; Flycheck / flymake
   `(flycheck-error             ((t (:underline (:style wave :color ,rust-hi)))))
   `(flycheck-warning           ((t (:underline (:style wave :color ,gold)))))
   `(flycheck-info              ((t (:underline (:style wave :color ,blue-hi)))))
   `(flycheck-fringe-error      ((t (:foreground ,rust-hi))))
   `(flycheck-fringe-warning    ((t (:foreground ,gold))))
   `(flycheck-fringe-info       ((t (:foreground ,blue-hi))))
   `(flymake-error              ((t (:underline (:style wave :color ,rust-hi)))))
   `(flymake-warning            ((t (:underline (:style wave :color ,gold)))))
   `(flymake-note               ((t (:underline (:style wave :color ,blue-hi)))))

   ;; LSP / eglot
   `(eglot-highlight-symbol-face ((t (:background ,bg-surface))))
   `(eglot-diagnostic-tag-unnecessary-face ((t (:foreground ,fg-muted :slant italic))))
   `(eglot-diagnostic-tag-deprecated-face  ((t (:foreground ,fg-muted :strike-through t))))

   ;; Hl-line
   `(hl-line                    ((t (:background ,bg-surface))))

   ;; Pulse / beacon
   `(pulse-highlight-start-face ((t (:background ,green-dim))))

   ;; Header line
   `(header-line                ((t (:background ,bg-deep :foreground ,fg-subtle
                                     :box (:line-width 1 :color ,bg-surface)))))
   `(header-line-highlight      ((t (:foreground ,fg-main))))

   ;; Tab bar
   `(tab-bar                    ((t (:background ,bg-deep :foreground ,fg-muted))))
   `(tab-bar-tab                ((t (:background ,bg-surface :foreground ,fg-main
                                     :box (:line-width 1 :color ,bg-overlay)))))
   `(tab-bar-tab-inactive       ((t (:background ,bg-deep :foreground ,fg-muted
                                     :box (:line-width 1 :color ,bg-surface)))))
   ;; Consult
   `(consult-preview-cursor     ((t (:background ,green-dim))))
   `(consult-preview-line       ((t (:background ,bg-surface))))
   `(consult-file               ((t (:foreground ,fg-main))))
   `(consult-grep-context       ((t (:foreground ,fg-muted))))
   `(consult-highlight-match    ((t (:background ,green-dim :foreground ,fg-bright))))

   ;; Orderless
   `(orderless-match-face-0     ((t (:foreground ,green-hi))))
   `(orderless-match-face-1     ((t (:foreground ,gold-hi))))
   `(orderless-match-face-2     ((t (:foreground ,blue-hi))))
   `(orderless-match-face-3     ((t (:foreground ,magenta-hi))))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,fg-subtle))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,gold))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,magenta))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,green-hi))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,blue-hi))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,gold-hi))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,magenta-hi))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,rust-hi :background ,rust-dim))))))

(provide-theme 'ancient)
;;; ancient-theme.el ends here
