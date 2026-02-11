;;; edna-theme.el --- A dark, Edna-inspired theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026 golanv

;; Author: golanv <https://codeberg.org/golanv>
;; URL: https://codeberg.org/golanv/edna-theme
;; Version: 0.2.1
;; Keywords: faces, theme
;; Package-Requires: ((emacs "26.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; A dark, medium-contrast theme inspired by the Edna color scheme.
;; Designed for GUI and truecolor terminals.

;;; Code:

(deftheme edna
  "Edna-inspired dark theme with cool accents and medium contrast.")

(let ((bg              "#263238")
      (bg-alt          "#2A363E")
      (fg              "#B7BAC3")
      (fg-dim          "#9AA3AF")
      (fg-inactive     "#7F8C8D")

      ;; Cool accents (primary)
      (blue            "#3DAAE9")
      (cyan            "#5FB3B3")
      (teal            "#4DB6AC")

      ;; Warm accents (secondary / restrained)
      (orange          "#D08770")
      (red             "#BF616A")
      (green           "#A3BE8C")
      ;; (purple          "#B48EAD")  ;; cool, subdued violet
      ;; (purple          "#9A86B3")
      ;; (purple          "#8E79A6")
      (purple          "#C0A4E3")
      ;; (purple          "#B79AD6")
      ;; (purple          "#C4A7E7")

      ;; UI
      (cursor          "#ECEFF4")
      (border          "#37474F")
      (region          "#344955")

      ;; (line            "#2E3B44")
      (line            "#1F2A30")
      (modeline-bg     "#2A363E")
      (modeline-fg     "#D8DEE9")
      (paren-bg         "#31424A")  ;; slightly lighter than bg

      ;; Comments (cool, flat, no italics)
      (comment         "#8899A6"))

  (custom-theme-set-faces
   'edna

   ;; Core
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,cursor))))
   `(fringe ((t (:background ,bg))))
   `(region ((t (:background ,region))))
   `(highlight ((t (:background ,line))))
   `(vertical-border ((t (:foreground ,border))))
   `(minibuffer-prompt ((t (:foreground ,blue :weight bold))))
   `(link ((t (:foreground ,cyan :underline t))))
   `(show-paren-match ((t (:background ,paren-bg :foreground ,fg))))

   ;; Modeline (subtle)
   `(mode-line ((t (:background ,modeline-bg :foreground ,modeline-fg :box nil))))
   `(mode-line-inactive ((t (:background ,bg :foreground ,fg-inactive :box nil))))

   ;; Comments (fixed)
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-doc-face ((t (:foreground ,comment))))
   `(shadow ((t (:foreground ,fg-inactive))))

   ;; Syntax
   `(font-lock-keyword-face ((t (:foreground ,purple))))
   `(font-lock-function-name-face ((t (:foreground ,cyan))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:foreground ,teal))))
   `(font-lock-constant-face ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground "#A3BE8C")))) ;; soft nord-green
   `(font-lock-builtin-face ((t (:foreground ,purple))))
   `(font-lock-warning-face ((t (:foreground ,orange :weight bold))))

   ;; Line numbers
   `(line-number ((t (:foreground ,fg-inactive))))
   `(line-number-current-line ((t (:foreground ,fg))))

   ;; Diff-hl (VC gutter)
   `(diff-hl-insert ((t (:foreground ,green :background ,green))))
   `(diff-hl-delete ((t (:foreground ,red :background ,red))))
   `(diff-hl-change ((t (:foreground ,orange :background ,orange))))

   ;; Org
   `(org-document-title ((t (:foreground ,blue :weight bold :height 1.3))))
   `(org-level-1 ((t (:foreground ,blue :weight bold))))
   `(org-level-2 ((t (:foreground ,cyan :weight bold))))
   `(org-level-3 ((t (:foreground ,teal :weight bold))))
   `(org-meta-line ((t (:foreground ,comment))))
   `(org-block ((t (:background ,bg-alt))))
   `(org-block-begin-line ((t (:foreground ,comment :background ,bg-alt))))
   `(org-block-end-line ((t (:foreground ,comment :background ,bg-alt))))
   `(org-link ((t (:foreground ,blue :underline t))))
   `(org-todo ((t (:foreground ,orange :weight bold))))
   `(org-done ((t (:foreground ,green :weight bold))))

   ;; Magit
   `(magit-section-heading ((t (:foreground ,blue :weight bold))))
   `(magit-branch-local ((t (:foreground ,cyan))))
   `(magit-branch-remote ((t (:foreground ,green))))
   `(magit-diff-added ((t (:foreground ,green))))
   `(magit-diff-removed ((t (:foreground ,red))))
   `(magit-diff-context ((t (:foreground ,fg-dim))))

   ;; Completion (Ivy / Vertico / Helm)
   `(ivy-current-match ((t (:background ,line :weight bold))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,fg))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,cyan :weight bold))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,teal :weight bold))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,blue :weight bold))))

   `(vertico-current ((t (:background ,line))))

   ;; Company
   `(company-tooltip ((t (:background ,bg-alt :foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,line))))
   `(company-tooltip-common ((t (:foreground ,blue :weight bold))))
   `(company-scrollbar-bg ((t (:background ,bg-alt))))
   `(company-scrollbar-fg ((t (:background ,line))))

   ;; Flycheck
   `(flycheck-error ((t (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,orange)))))
   `(flycheck-info ((t (:underline (:style wave :color ,blue)))))
   `(flyspell-incorrect ((t (:underline (:style wave :color ,red)))))
   `(flyspell-duplicate ((t (:underline (:style wave :color ,orange)))))

   ;; LSP
   `(lsp-face-highlight-textual ((t (:background ,line))))
   `(lsp-face-highlight-read ((t (:background ,line))))
   `(lsp-face-highlight-write ((t (:background ,line))))

   ;; Dired / Treemacs (cooler filenames)
   `(dired-directory ((t (:foreground ,blue :weight bold))))
   `(dired-flagged ((t (:foreground ,red))))
   `(treemacs-root-face ((t (:foreground ,blue :weight bold))))
   `(treemacs-file-face ((t (:foreground ,fg))))
   `(treemacs-directory-face ((t (:foreground ,cyan))))

   ;; Search
   `(isearch ((t (:background ,blue :foreground ,bg :weight bold))))
   `(lazy-highlight ((t (:background ,teal :foreground ,bg))))))

;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory
                (file-name-directory load-file-name))))

(provide-theme 'edna)
;;; edna-theme.el ends here
