;;; stimmung-themes-light-theme.el --- A light theme tuned to inner harmonies -*- lexical-binding: t -*-
;; Copyright © 2019

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/stimmung
;; Created: 2019-12-20
;; Version: 2021-03-20
;; Keywords: faces

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Stimmung (dark and light) is a pair of monochrome Emacs themes
;; with minimal syntax highlighting.  They are inspired by Tonsky's
;; Alabaster theme (https://github.com/tonsky/sublime-scheme-alabaster),
;; following the maxim that a theme that highlights everything
;; paradoxically highlights nothing.  Text backgrounds (comments,
;; strings and constants) and font variations (definitions) are used
;; as alternatives to text colors, ensuring a harmonious reading
;; experience.  Use `stimmung-themes-dark-highlight-color' and
;; `stimmung-themes-light-highlight-color' to customize the highlight.
;;
;; Screenshots are available at: https://github.com/motform/stimmung-themes

;;; Code:

(deftheme stimmung-themes-light
  "A light theme tuned to inner harmonies.")

(defgroup stimmung-themes nil
  "Stimmung settings.
You have to re-load the theme for these changes to take effect."
  :group 'faces
  :prefix "stimmung-theme-"
  :link   '(url-link "https://github.com/motform/stimmung-themes"))

(defcustom stimmung-themes-light-highlight-color "cornsilk1"
  "The primarily color for highlights, the only non-monochrome color in code."
  :type 'string
  :group 'stimmung-themes)

(let ((bg1 "white")
      (bg2 "gray95")
      (bg3 "gray90")
      (bg4 "gray85")
      (bg5 "gray98")

      (fg  "black")
      (fg2 "gray60")

      (str     "gray95")

      (search   "#e8e800")
      (search2  "#ffffb4")

      (warning "orange")
      (red     "darkred")
      (ok      "DarkGreen"))
  (custom-theme-set-faces
   'stimmung-themes-light

   `(default  ((t (:background ,bg1 :foreground ,fg))))
   `(shadow   ((t (:background ,bg2))))
   `(hl-line  ((t (:background ,bg2 :extend t))))

   `(region              ((t (:background ,bg4))))
   `(lazy-highlight      ((t (:foreground ,fg :background ,search2))))
   `(secondary-selection ((t (:foreground ,fg :background ,search2))))
   `(highlight           ((t (:foreground ,fg :background ,bg3 :bold t))))
   `(default             ((t (:foreground ,fg :background ,bg1))))
   `(fringe              ((t (:foreground ,fg :background ,bg1))))
   `(match               ((t (:foreground ,ok :bold t))))

   `(link                ((t (:underline t))))
   `(link-visited        ((t (:underline t :italic t))))
   `(button              ((t (:underline t))))
   `(header-line         ((t (:bold t))))
   `(tooltip             ((t (:foreground ,fg  :background ,bg3))))
   `(vertical-border     ((t (:foreground ,bg2 :background ,bg2))))
   `(info-string         ((t (:background ,stimmung-themes-light-highlight-color))))
   `(default-italic      ((t (:slant italic))))

   `(error                       ((t (:foreground ,red))))
   `(warning                     ((t (:foreground ,warning))))
   `(success                     ((t (:foreground ,ok))))
   `(cancel                      ((t (:foreground ,red :strike-through t))))
   
   `(minibuffer-noticable-prompt ((t (:foreground ,fg :bold t))))
   `(minibuffer-prompt           ((t (:foreground ,fg :bold t))))

   `(isearch                     ((t (:foreground ,fg :background ,search))))
   `(isearch-highlight           ((t (:foreground ,fg :background ,search2))))
   `(isearch-fail                ((t (:foreground ,fg :background ,search2))))

   `(paren-matched               ((t (:foreground ,ok  :background ,bg1))))
   `(paren-unmatched             ((t (:foreground ,red :background ,bg1))))
   `(escape-glyph                ((t (:foreground ,fg2)))) ; TODO move into an fg color?
   `(homoglyph                   ((t (:foreground ,red))))
   
   `(line-number              ((t (:foreground ,fg2 :background ,bg1))))
   `(line-number-current-line ((t (:foreground ,fg  :background ,bg1))))
   `(linum                    ((t (:inherit 'line-number))))
   
   ;; syntax
   `(font-lock-builtin-face              ((t (:background ,stimmung-themes-light-highlight-color :italic t))))
   `(font-lock-comment-delimiter-face    ((t (:background ,str :italic t))))
   `(font-lock-comment-face              ((t (:background ,str :italic t))))
   `(font-lock-doc-face                  ((t (:background ,str :italic t))))
   `(font-lock-constant-face             ((t (:background ,stimmung-themes-light-highlight-color :italic t))))
   `(font-lock-function-name-face        ((t (:foreground ,fg :bold t))))
   `(font-lock-keyword-face              ((t (:foreground ,fg))))
   `(font-lock-type-face                 ((t (:background ,stimmung-themes-light-highlight-color))))
   `(font-lock-variable-name-face        ((t (:foreground ,fg :bold t))))
   `(font-lock-negation-char-face        ((t (:foreground ,fg))))
   `(font-lock-preprocessor-face         ((t (:foreground ,fg))))
   `(font-lock-preprocessor-char-face    ((t (:foreground ,fg))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,fg :bold t))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,fg :bold t))))
   `(font-lock-string-face               ((t (:foreground ,fg :background ,str))))
   `(font-lock-warning-face              ((t (:foreground ,fg :underline (:style wave :color ,warning)))))
   
   ;; eshell
   `(eshell-ls-directory  ((t (:background ,stimmung-themes-light-highlight-color :bold t))))
   `(eshell-prompt        ((t (:foreground ,fg :bold t))))
   `(eshell-ls-executable ((t (:foreground ,fg :bold t))))
   `(eshell-ls-symlink    ((t (:foreground ,fg :italic t))))
   `(eshell-ls-special    ((t (:foreground ,ok :italic t))))
   `(eshell-ls-backup     ((t (:foreground ,fg :italic t))))
   `(eshell-ls-readonly   ((t (:foreground ,red))))
   `(eshell-ls-unreadable ((t (:foreground ,red))))
   `(eshell-ls-missing    ((t (:foreground ,red))))
   `(eshell-ls-product    ((t (:foreground ,fg))))
   `(eshell-ls-archive    ((t (:foreground ,fg))))
   `(eshell-ls-entries    ((t (:foreground ,fg))))
   
   ;; avy
   `(avy-lead-face   ((t (:background ,bg2 :foreground ,fg :distant-foreground ,fg :bold t))))
   `(avy-lead-face-0 ((t (:inherit 'avy-lead-face))))
   `(avy-lead-face-1 ((t (:inherit 'avy-lead-face))))
   `(avy-lead-face-2 ((t (:inherit 'avy-lead-face))))
   
   ;; flyspell
   `(flycheck-note            ((t (:underline (:style wave :color ,ok)))))
   `(flyspell-incorrect       ((t (:underline (:style wave :color ,red) ))))
   `(flycheck-error           ((t (:underline (:style wave :color ,red)))))
   `(flyspell-duplicate       ((t (:underline (:style wave :color ,warning)))))
   `(flysheck-warning         ((t (:underline (:style wave :color ,warning)))))
   `(flysheck-warning-overlay ((t (:underline (:style wave :color ,warning)))))
   
   ;; hydra
   `(hydra-face-red      ((t (:foreground ,fg :bold t))))
   `(hydra-face-blue     ((t (:foreground ,fg :bold t))))
   `(hydra-face-amaranth ((t (:foreground ,fg :bold t))))
   `(hydra-face-pink     ((t (:foreground ,fg :bold t))))
   `(hydra-face-teal     ((t (:foreground ,fg :bold t))))
   
   ;; cider
   `(cider-fringe-good-face      ((t (:foreground ,ok))))
   ;; `(cider-test-success-face     ((t (:background ,ok  :foreground ,bg1))))
   ;; `(cider-test-failure-face     ((t (:background ,red :foreground ,bg1))))
   `(cider-test-error-face       ((t (:background ,stimmung-themes-light-highlight-color))))
   `(cider-result-overlay-face   ((t (:background ,bg5 :box (:line-width -1 :color ,fg2)))))
   
   ;; company
   `(company-tooltip-mouse            ((t (:inherit highlight))))
   `(company-scrollbar-bg             ((t (:background ,fg))))
   `(company-scrollbar-fg             ((t (:foreground ,fg))))
   `(company-tooltip-common           ((t (:foreground ,fg))))
   `(company-echo-common              ((t (:background ,fg  :foreground ,bg1))))
   `(company-tooltip                  ((t (:background ,bg3 :foreground ,fg))))
   `(company-tooltip-selection        ((t (:background ,bg3 :foreground ,fg))))
   `(company-tooltip-selection-       ((t (:background ,bg3 :foreground ,fg))))
   `(company-tooltip-common-selection ((t (:background ,stimmung-themes-light-highlight-color))))
   `(company-tooltip-annotation       ((t (:foreground ,stimmung-themes-light-highlight-color))))
   `(company-preview                  ((t (:background ,stimmung-themes-light-highlight-color :foreground ,fg))))
   
   ;; compilation
   `(compilation-line-number    ((t (:bold t))))
   `(compilation-column-number  ((t (:inherit 'font-lock-comment-face))))
   `(compilation-error          ((t (:inherit 'error :bold t))))
   `(compilation-warning        ((t (:inherit 'warning))))
   `(compilation-info           ((t (:inherit ,success))))
   `(compilation-mode-line-exit ((t (:inherit 'compilation-info))))
   `(compilation-mode-line-fail ((t (:inherit 'compilation-error))))
   
   ;; TODO
   ;; custom
   `(custom-variable-tag    ((t (:bold t))))

   ;; modeline
   `(header-line         ((t (:inherit 'mode-line :distant-foreground ,bg1))))
   `(mode-line           ((t (:foreground ,fg  :background ,bg5 :box (:line-width 1 :color ,fg2 :style nil)))))
   `(mode-line-inactive  ((t (:foreground ,fg2 :background ,bg1 :box (:line-width 1 :color ,bg3 :style nil)))))
   `(mode-line-buffer-id ((t (:foreground ,fg :bold t :distant-foreground ,bg1))))
   `(mode-line-emphasis  ((t (:foreground ,fg :bold t))))
   `(mode-line-highlight ((t (:foreground ,bg3))))
   
   ;; completions
   `(completions-common-part ((t (:foreground ,fg :bold t))))
   
   ;; doom-modeline
   `(doom-modeline-buffer-path        ((t (:foreground ,fg))))
   `(doom-modeline-buffer-file        ((t (:foreground ,fg  :weight bold))))
   `(doom-modeline-buffer-modified    ((t (:foreground ,red :weight bold))))
   `(doom-modeline-project-dir        ((t (:foreground ,fg  :weight bold))))
   `(doom-modeline-project-root-dir   ((t (:foreground ,fg  :weight normal))))
   `(doom-modeline-project-parent-dir ((t (:foreground ,fg  :weight normal))))
   `(doom-modeline-bar-inactive       ((t (:foreground ,fg  :background ,bg1))))
   `(doom-modeline-bar                ((t (:background ,bg5)))) ; the leftmost bar
   `(doom-modeline-evil-insert-state  ((t (:foreground ,fg))))
   `(doom-modeline-evil-visual-state  ((t (:foreground ,fg))))
   `(doom-modeline-evil-normal-state  ((t (:foreground ,fg))))
   `(doom-modeline-evil-emacs-state   ((t (:foreground ,red :italic nil))))
   
   ;; dired
   `(dired-directory  ((t (:foreground ,fg :bold t))))
   `(dired-ignored    ((t (:foreground ,fg))))
   `(dired-flagged    ((t (:foreground ,ok))))
   `(dired-header     ((t (:foreground ,fg  :bold t))))
   `(dired-mark       ((t (:foreground ,red :bold t))))
   `(dired-marked     ((t (:foreground ,red :bold t))))
   `(dired-perm-write ((t (:foreground ,fg  :underline t))))
   `(dired-symlink    ((t (:foreground ,fg  :italic t))))
   `(dired-warning    ((t (:foreground ,fg :underline (:style wave :color ,warning)))))
   
   ;; evil
   `(evil-ex-info                   ((t (:foreground ,red :italic t))))
   `(evil-ex-search                 ((t (:background ,bg2 :foreground ,fg :bold t))))
   `(evil-search-highlight-persist-highlight-face ((t (:inherit 'lazy-highlight))))
   
   ;; evil-mc
   `(evil-mc-cursor-default-face ((t (:foreground ,bg1 :background ,fg))))
   `(evil-mc-region-face         ((t (:foreground ,bg1 :background ,fg))))
   `(evil-mc-cursor-bar-face     ((t (:foreground ,fg))))
   `(evil-mc-cursor-hbar-face    ((t (:foreground ,fg))))
   
   ;; info
   `(info-quoted    ((t (:inherit 'default :bold t))))
   `(info-menu-star ((t (:bold t))))

   ;; ivy
   `(ivy-current-match              ((t (:background ,bg1 :bold t))))
   `(ivy-minibuffer-match-highlight ((t (:foreground ,ok))))
   `(ivy-minibuffer-match-face-1    ((t (:foreground ,fg :bold t))))
   `(ivy-minibuffer-match-face-2    ((t (:foreground ,fg :bold t))))
   `(ivy-minibuffer-match-face-3    ((t (:foreground ,fg :bold t))))
   `(ivy-minibuffer-match-face-4    ((t (:foreground ,fg :bold t))))
   `(ivy-confirm-face               ((t (:foreground ,ok))))
   `(ivy-required-face              ((t (:foreground ,red))))
   `(ivy-subdir                     ((t (:foreground ,fg))))
   `(ivy-modified-buffer            ((t (:foreground ,red :bold t))))
   `(ivy-modified-outside-buffer    ((t (:foreground ,red))))
   `(ivy-remote                     ((t (:foreground ,fg))))
   `(ivy-virtual                    ((t (:foreground ,fg :italic t))))
   `(ivy-prompt                     ((t (:foreground ,red))))
   `(ivy-prompt-match               ((t (:foreground ,red))))
   `(ivy-separator                  ((t (:foreground ,stimmung-themes-light-highlight-color))))
   `(ivy-highlight-face             ((t (:foreground ,red))))
   `(ivy-grep-info                  ((t (:foreground ,red))))
   `(ivy-completions-annotations    ((t (:foreground ,red))))
   
   ;; magit
   `(magit-bisect-bad        ((t (:foreground ,red))))
   `(magit-bisect-good       ((t (:foreground ,ok))))
   `(magit-bisect-skip       ((t (:foreground ,fg))))
   `(magit-blame-date        ((t (:foreground ,red))))
   `(magit-branch            ((t (:foreground ,fg :bold t))))
   `(magit-branch-local      ((t (:foreground ,fg :bold t))))
   `(magit-branch-remote     ((t (:foreground ,fg :bold t))))
   `(magit-diff-file-heading ((t (:foreground ,fg :bold nil))))
   `(magit-diff-whitespace-warning ((t (:background ,red))))

   
   `(magit-diff-context-highlight ((t (:foreground ,fg :background ,bg3))))
   `(magit-diff-file-header       ((t (:foreground ,fg :background ,bg3))))
   `(magit-diffstat-added         ((t (:foreground ,ok))))
   `(magit-diffstat-removed       ((t (:foreground ,red))))
   `(magit-dimmed                 ((t (:foreground ,fg))))
   `(magit-hash                   ((t (:foreground ,fg))))
   `(magit-hunk-heading           ((t (:background ,bg3))))
   `(magit-hunk-heading-highlight ((t (:background ,bg3))))
   `(magit-item-highlight         ((t (:background ,bg3))))
   `(magit-log-author             ((t (:foreground ,fg))))
   `(magit-process-ng             ((t (:background ,stimmung-themes-light-highlight-color :bold t))))
   `(magit-process-ok             ((t (:foreground ,ok :bold t))))
   `(magit-section-heading        ((t (:foreground ,fg :bold t))))
   `(magit-section-highlight      ((t (:background ,bg3))))
   
   ;; diff-hl
   `(diff-hl-insert         ((t (:foreground ,ok  :background ,bg1 :bold nil :italic nil))))
   `(diff-hl-delete         ((t (:foreground ,red :background ,bg1 :bold nil :italic nil))))
   `(diff-hl-change         ((t (:foreground ,fg  :background ,bg1 :bold nil :italic nil))))
   `(diff-hl-ignore         ((t (:foreground ,fg  :background ,bg1 :bold nil :italic nil))))
   `(diff-hl-margin-ignore  ((t (:foreground ,fg  :background ,bg1 :bold nil :italic nil))))
   `(diff-hl-margin-unknown ((t (:foreground ,fg  :background ,bg1 :bold nil :italic nil))))
   
   ;; outline, extends org-outline
   `(outline-1 ((t (:foreground ,fg :bold t :extend t))))
   `(outline-2 ((t (:foreground ,fg :bold t :extend t))))
   `(outline-3 ((t (:foreground ,fg :bold t :extend t))))
   `(outline-4 ((t (:foreground ,fg :bold t :extend t))))
   `(outline-5 ((t (:foreground ,fg :bold t :extend t))))
   `(outline-6 ((t (:foreground ,fg :bold t :extend t))))
   `(outline-7 ((t (:foreground ,fg :bold t :extend t))))
   `(outline-8 ((t (:foreground ,fg :bold t :extend t))))

   ;; TODO org-agenda
   
   ;; org
   `(org-code                  ((t (:background ,str :distant-foreground ,bg1 :background ,stimmung-themes-light-highlight-color))))
   `(org-link                  ((t (:underline t))))
   `(org-block                 ((t (:foreground ,fg :background ,bg3 :extend t))))
   `(org-block-begin-line      ((t (:foreground ,fg :background ,bg3 :bold t :extend t))))
   `(org-block-end-line        ((t (:foreground ,fg :background ,bg3 :bold t :extend t))))
   `(org-drawer                ((t (:foreground ,fg :bold t))))
   `(org-document-info         ((t (:foreground ,fg :background ,bg1 :italic t))))
   `(org-document-info-keyword ((t (:foreground ,fg :background ,bg1))))
   `(org-document-title        ((t (:foreground ,fg :weight bold))))
   `(org-done                  ((t (:foreground ,ok :bold t :strike-through t))))
   `(org-ellipsis              ((t (:foreground ,fg))))
   `(org-meta-line             ((t (:background ,bg1))))
   `(org-formula               ((t (:foreground ,fg))))
   `(org-headline-done         ((t (:foreground ,fg :weight normal :strike-through t))))
   `(org-hide                  ((t (:foreground ,bg1 :background ,bg1))))
   `(org-list-dt               ((t (:foreground ,fg :bold t))))
   `(org-scheduled             ((t (:foreground ,red))))
   `(org-scheduled-today       ((t (:foreground ,ok))))
   `(org-table                 ((t (:foreground ,fg))))
   `(org-tag                   ((t (:foreground ,fg  :background ,bg1 :bold t))))
   `(org-todo                  ((t (:foreground ,red :bold t))))
   `(org-warning               ((t (:inherit 'warning))))
   `(org-upcoming-deadline     ((t (:foreground ,red))))
   `(org-priority              ((t (:background ,stimmung-themes-light-highlight-color))))
   `(org-footnote              ((t (:background ,stimmung-themes-light-highlight-color))))
   `(org-scheduled-previously  ((t (:background ,stimmung-themes-light-highlight-color))))
   `(org-sexp-date             ((t (:background ,stimmung-themes-light-highlight-color))))
   `(org-special-keyword       ((t (:background ,stimmung-themes-light-highlight-color))))
   `(org-date                  ((t (:background ,stimmung-themes-light-highlight-color :bold t))))
   
   ;; markdown mode
   `(markdown-header-face             ((t (:foreground ,fg :bold t))))
   `(markdown-list-face               ((t (:foreground ,fg :bold t))))
   `(markdown-bold-face               ((t (:foreground ,fg :bold t))))
   `(markdown-blockquote-face         ((t (:foreground ,fg :italic t))))
   `(markdown-italic-face             ((t (:foreground ,fg :italic t))))
   `(markdown-link-face               ((t (:foreground ,fg :underline t))))
   `(markdown-url-face                ((t (:foreground ,fg :underline t))))
   `(markdown-header-delimiter-face   ((t (:inherit 'markdown-header-face))))
   `(markdown-metadata-key-face       ((t (:foreground ,fg))))
   `(markdown-markup-face             ((t (:foreground ,fg))))
   `(markdown-pre-face                ((t (:foreground ,fg))))
   `(markdown-code-face               ((t (:background ,fg :extend t))))
   `(markdown-reference-face          ((t (:foreground ,fg))))
   `(markdown-html-attr-name-face     ((t (:inherit 'font-lock-variable-name-face))))
   `(markdown-html-attr-value-face    ((t (:inherit 'font-lock-string-face))))
   `(markdown-html-entity-face        ((t (:inherit 'font-lock-variable-name-face))))
   `(markdown-html-tag-delimiter-face ((t (:inherit 'markdown-markup-face))))
   `(markdown-html-tag-name-face      ((t (:inherit 'font-lock-keyword-face))))
   `(markdown-inline-code-face        ((t (:inherit 'markdown-code-face :extend nil))))
   
   ;; show-paren
   `(show-paren-match-face       ((t (:background ,search :bold t))))
   `(show-paren-match            ((t (:background ,search :bold t))))
   `(show-paren-match-expression ((t (:background ,search :bold t))))
   `(show-paren-mismatch         ((t (:background ,fg :foreground ,red :bold t))))
   
   ;; smartparens
   `(sp-show-pair-match-face    ((t (:inherit 'paren-matched))))
   `(sp-show-pair-mismatch-face ((t (:inherit 'paren-unmatched))))

   ;; treemacs
   `(treemacs-root-face                     ((t (:background ,bg1 :foreground ,fg  :bold t :underline t))))
   `(treemacs-root-unreadable-face          ((t (:background ,bg1 :foreground ,fg  :bold t :underline t :strike-through t))))
   `(treemacs-root-remote-face              ((t (:background ,bg1 :foreground ,fg  :bold t :underline t))))
   `(treemacs-root-remote-disconnected-face ((t (:background ,bg1 :foreground ,red :bold t :underline t))))
   `(treemacs-root-remote-unreadable-face   ((t (:background ,bg1 :foreground ,fg  :bold t :underline t :strike-through t))))
   `(treemacs-git-ignored-face              ((t (:background ,str :foreground ,fg :italic nil))))
   `(treemacs-git-renamed-face              ((t (:background ,bg1 :foreground ,fg :italic t))))
   `(treemacs-on-failure-pulse-face         ((t (:background ,bg1 :foreground ,red))))
   `(treemacs-on-success-pulse-face         ((t (:background ,bg1 :foreground ,ok))))
   `(treemacs-tags-face                     ((t (:background ,bg1 :foreground ,fg  :italic nil))))
   `(treemacs-term-node-face                ((t (:background ,bg1 :foreground ,fg))))

   ;; tab-bar-mode
   `(tab-bar              ((t (:background ,bg1 :foreground ,fg))))
   `(tab-bar-tab          ((t (:background ,bg1 :foreground ,fg :bold t))))
   `(tab-bar-tab-inactive ((t (:background ,bg1 :foreground ,fg))))

   ;; LaTeX
   `(font-latex-sectioning-0-face ((t (:bold t))))
   `(font-latex-sectioning-1-face ((t (:bold t))))
   `(font-latex-sectioning-2-face ((t (:bold t))))
   `(font-latex-sectioning-3-face ((t (:bold t))))
   `(font-latex-sectioning-4-face ((t (:italic t))))
   `(font-latex-sedate-face       ((t (:foreground ,fg2))))
   `(font-latex-italic-face       ((t (:foreground ,fg :italic t))))
   `(font-latex-bold-face         ((t (:foreground ,fg :bold t))))
   `(font-latex-verbatim-face     ((t (:background ,stimmung-themes-light-highlight-color :bold t))))
   `(font-latex-string-face       ((t (:foreground ,fg))))
   `(font-latex-warning-face      ((t (:foreground ,fg2))))
   `(font-latex-math-face         ((t (:foreground ,fg))))
   `(font-latex-script-char-face  ((t (:foregroudn ,fg))))

   ;; re-builder
   `(reb-match-0 ((t (:foreground ,fg :inverse-video t :bold t))))
   `(reb-match-1 ((t (:foreground ,fg :inverse-video t :bold t))))
   `(reb-match-2 ((t (:foreground ,fg :inverse-video t :bold t))))
   `(reb-match-3 ((t (:foreground ,fg :inverse-video t :bold t))))
   
   ;; undo-tree
   `(undo-tree-visualizer-default-face       ((t (:foreground ,fg))))
   `(undo-tree-visualizer-current-face       ((t (:foreground ,ok :bold t))))
   `(undo-tree-visualizer-unmodified-face    ((t (:foreground ,fg :italic t))))
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,fg))))
   `(undo-tree-visualizer-register-face      ((t (:foreground ,fg))))

   `(window-divider             ((t (:foreground ,bg1))))
   `(window-divider-first-pixel ((t (:foreground ,bg1))))
   `(window-divider-last-pixel  ((t (:foreground ,bg1))))
   
   ;; wo/man
   `(Man-overstrike ((t (:foreground ,fg :bold t))))
   `(Man-underline  ((t (:foreground ,fg :underline nil :italic t))))
   `(woman-bold     ((t (:inherit 'Man-overstrike))))
   `(woman-italic   ((t (:inherit 'Man-underline))))
   
   ;; web-mode
   `(web-mode-doctype-face           ((t (:foreground ,fg))))
   `(web-mode-html-tag-face          ((t (:foreground ,fg :italic t))))
   `(web-mode-html-tag-bracket-face  ((t (:foreground ,fg))))
   `(web-mode-html-attr-name-face    ((t (:foreground ,fg :bold t))))
   `(web-mode-html-entity-face       ((t (:foreground ,fg :italic t))))
   `(web-mode-block-control-face     ((t (:foreground ,fg))))
   `(web-mode-html-tag-bracket-face  ((t (:foreground ,fg :bold t))))

   ;; visual-regexp
   `(vr/match-0 ((t (:background ,bg3 :foreground ,red :bold t))))
   `(vr/match-1 ((t (:background ,bg2 :foreground ,red))))
   `(vr/group-0 ((t (:background ,bg3 :foreground ,red :bold t))))
   `(vr/group-1 ((t (:background ,bg2 :foreground ,red))))
   `(vr/group-2 ((t (:background ,bg2 :foreground ,fg))))

   ;; white-space
   `(whitespace-empty       ((t (:background ,bg3))))
   `(whitespace-space       ((t (:foreground ,fg))))
   `(whitespace-newline     ((t (:foreground ,fg))))
   `(whitespace-tab         ((t (:foreground ,fg   :background ,bg1))))
   `(whitespace-indentation ((t (:foreground ,red  :background ,fg))))
   `(whitespace-line        ((t (:foreground ,red  :background ,fg :weight bold))))
   `(nobreak-space          ((t (:inherit 'default :underline nil))))
   `(whitespace-trailing    ((t (:foreground ,red))))))

(custom-theme-set-variables
 'stimmung-themes-light
 '(ansi-color-names-vector ["black" "black" "black" "black"
                            "black" "black" "black" "white"]))

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'stimmung-themes-light)
(provide 'stimmung-themes-light-theme)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; stimmung-themes-light-theme.el ends here
