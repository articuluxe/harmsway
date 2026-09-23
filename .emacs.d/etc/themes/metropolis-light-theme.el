;;; metropolis-light-theme.el --- Light theme based on the Metropolis Beamer palette -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Vítor Santos

;; Author: Vítor Santos vhsoo@proton.me
;; URL: https://github.com/bitorhugo/metropolis-light-theme
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: faces, theme
;; SPDX-License-Identifier: MIT

;; This file is NOT part of GNU Emacs.

;;; License:

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; A light Emacs theme based on the color palette of the Metropolis
;; LaTeX Beamer theme (https://github.com/matze/mtheme).
;;
;; Metropolis itself only defines four colors:  mDarkTeal, mDarkBrown,
;; mLightBrown and mLightGreen, and derives everything else (block
;; backgrounds, progress bars, footnotes, ...) from them with simple
;; percentage color blends.  This theme follows the same approach: the
;; four base colors below are blended using the same ratios Metropolis
;; uses in its `.dtx' source to produce the rest of the palette, so the
;; result stays visually consistent with the original Beamer theme
;; while covering the much larger set of faces Emacs needs.
;;
;; A few extra hues (cyan, blue, red, yellow, magenta) are derived the
;; same way (by blending the base colors together) to cover roles
;; Metropolis has no opinion on, such as types, constants, errors and
;; terminal colors.

;;; Code:

(deftheme metropolis-light
  "A light theme based on the Metropolis Beamer color palette.")

(let* ((class '((class color) (min-colors 89)))

       ;;  Metropolis base colors (beamercolorthememetropolis.sty)
       ;;
       (mDarkBrown    "#604c38")
       (mDarkTeal     "#23373b")
       (mLightBrown   "#eb811b")
       (mLightGreen   "#14b03d")

       ;;  Backgrounds, derived the way Metropolis derives its own
       ;;  block/title backgrounds (percentage blends of fg into bg)
       ;;
       (bg-main       "#fafafa")
       (bg-alt        "#f1efee")
       (bg-hl         "#e4e6e7")
       (bg-hl-strong  "#d3d7d8")
       (bg-block      "#cfd3d4")
       (bg-mode-line-inactive "#dadddd")

       ;;  Foregrounds
       ;;
       (fg-main       mDarkTeal)
       (fg-dim        "#869193")
       (fg-faint      "#a3abad")

       ;;  Readable accents
       ;;
       (orange        "#9b6328")
       (green         "#1a803c")
       (brown         mDarkBrown)
       (cyan          "#0f6e6e")
       (blue          "#2e6b8e")
       (red           "#c0392b")
       (yellow        "#866a18")
       (magenta       "#77525c")

       ;;  Pure Metropolis accents, for chrome only (mode-line, links,
       ;;  banners) where contrast comes from an inverted background
       ;;  rather than the raw color against bg-main
       ;;
       (c-orange      mLightBrown)
       (c-green       mLightGreen)
       (progress      "#cf9a67"))

  (custom-theme-set-faces
   'metropolis-light

   ;; Basic UI
   ;;
   `(default ((,class (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((,class (:background ,fg-main))))
   `(fixed-pitch ((,class (:inherit default))))
   `(variable-pitch ((,class (:family "Sans Serif"))))
   `(escape-glyph ((,class (:foreground ,magenta))))
   `(homoglyph ((,class (:foreground ,magenta))))
   `(highlight ((,class (:background ,bg-hl :foreground ,fg-main))))
   `(hl-line ((,class (:background ,bg-hl))))
   `(region ((,class (:background ,bg-hl-strong :foreground ,fg-main))))
   `(secondary-selection ((,class (:background ,bg-block :foreground ,fg-main))))
   `(trailing-whitespace ((,class (:background ,red :foreground ,bg-main))))
   `(shadow ((,class (:foreground ,fg-dim))))
   `(vertical-border ((,class (:foreground ,bg-hl-strong))))
   `(window-divider ((,class (:foreground ,bg-hl-strong))))
   `(window-divider-first-pixel ((,class (:foreground ,bg-hl-strong))))
   `(window-divider-last-pixel ((,class (:foreground ,bg-hl-strong))))
   `(tooltip ((,class (:background ,bg-block :foreground ,fg-main))))

   ;; Font lock (syntax highlighting)
   ;;
   `(font-lock-builtin-face ((,class (:foreground ,brown))))
   `(font-lock-comment-face ((,class (:foreground ,fg-dim :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((,class (:foreground ,blue))))
   `(font-lock-doc-face ((,class (:foreground ,cyan :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,c-green))))
   `(font-lock-keyword-face ((,class (:foreground ,orange))))
   `(font-lock-negation-char-face ((,class (:foreground ,brown))))
   `(font-lock-preprocessor-face ((,class (:foreground ,brown))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,magenta))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,magenta))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,cyan))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-warning-face ((,class (:foreground ,red))))

   ;; Line numbers / fringe
   ;;
   `(fringe ((,class (:background ,bg-main))))
   `(line-number ((,class (:background ,bg-main :foreground ,fg-faint))))
   `(line-number-current-line ((,class (:background ,bg-hl :foreground ,orange :weight normal))))
   `(linum ((,class (:background ,bg-main :foreground ,fg-faint))))
   `(linum-highlight-face ((,class (:background ,bg-hl :foreground ,orange))))

   ;; Mode line / header line / tabs
   ;;
   `(mode-line ((,class (:background ,fg-main :foreground ,bg-main :box nil))))
   `(mode-line-active ((,class (:inherit mode-line))))
   `(mode-line-buffer-id ((,class (:foreground ,bg-main :weight bold))))
   `(mode-line-emphasis ((,class (:foreground ,c-orange :weight bold))))
   `(mode-line-highlight ((,class (:box (:line-width 1 :color ,bg-main)))))
   `(mode-line-inactive ((,class (:background ,bg-mode-line-inactive :foreground ,fg-dim :box nil))))
   `(header-line ((,class (:background ,bg-block :foreground ,fg-main
				       :underline (:color ,progress :style line)))))
   `(tab-bar ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(tab-bar-tab ((,class (:background ,bg-main :foreground ,fg-main :weight bold))))
   `(tab-bar-tab-inactive ((,class (:background ,bg-alt :foreground ,fg-dim))))
   `(tab-line ((,class (:background ,bg-alt :foreground ,fg-dim))))

   ;; Minibuffer / search / paren matching
   ;;
   `(minibuffer-prompt ((,class (:foreground ,orange))))
   `(isearch ((,class (:background ,c-orange :foreground ,bg-main))))
   `(isearch-fail ((,class (:background ,red :foreground ,bg-main))))
   `(lazy-highlight ((,class (:background ,bg-hl-strong :foreground ,fg-main))))
   `(match ((,class (:background ,c-green :foreground ,bg-main))))
   `(query-replace ((,class (:inherit isearch))))
   `(next-error ((,class (:inherit region))))
   `(show-paren-match ((,class (:background ,bg-hl-strong :foreground ,orange :weight bold))))
   `(show-paren-mismatch ((,class (:background ,red :foreground ,bg-main :weight bold))))

   ;; Whitespace mode
   ;;
   `(whitespace-trailing ((,class (:background ,red :foreground ,bg-main))))
   `(whitespace-tab ((,class (:foreground ,bg-hl-strong :underline t))))
   `(whitespace-space ((,class (:foreground ,bg-hl-strong))))
   `(whitespace-newline ((,class (:foreground ,bg-hl-strong))))
   `(whitespace-line ((,class (:background ,bg-block :foreground ,red))))
   `(whitespace-indentation ((,class (:foreground ,bg-hl-strong))))

   ;; Links / diagnostics
   ;;
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foreground ,magenta :underline t))))
   `(button ((,class (:inherit link))))
   `(error ((,class (:foreground ,red :weight bold))))
   `(warning ((,class (:foreground ,orange :weight bold))))
   `(success ((,class (:foreground ,green :weight bold))))

   ;; Flyspell / Flycheck / Flymake
   ;;
   `(flyspell-incorrect ((,class (:underline (:color ,red :style wave)))))
   `(flyspell-duplicate ((,class (:underline (:color ,orange :style wave)))))
   `(flycheck-error ((,class (:underline (:color ,red :style wave)))))
   `(flycheck-warning ((,class (:underline (:color ,orange :style wave)))))
   `(flycheck-info ((,class (:underline (:color ,blue :style wave)))))
   `(flymake-error ((,class (:underline (:color ,red :style wave)))))
   `(flymake-warning ((,class (:underline (:color ,orange :style wave)))))
   `(flymake-note ((,class (:underline (:color ,blue :style wave)))))

   ;; Completion UI: company / ido / ivy / vertico
   ;;
   `(company-tooltip ((,class (:background ,bg-block :foreground ,fg-main))))
   `(company-tooltip-selection ((,class (:background ,bg-hl-strong :foreground ,fg-main :weight bold))))
   `(company-tooltip-common ((,class (:foreground ,orange :weight bold))))
   `(company-tooltip-common-selection ((,class (:foreground ,orange :weight bold))))
   `(company-tooltip-annotation ((,class (:foreground ,fg-dim))))
   `(company-scrollbar-bg ((,class (:background ,bg-hl))))
   `(company-scrollbar-fg ((,class (:background ,fg-faint))))
   `(company-preview ((,class (:background ,bg-block :foreground ,fg-dim))))
   `(company-preview-common ((,class (:foreground ,orange))))

   `(ido-first-match ((,class (:foreground ,orange :weight bold))))
   `(ido-only-match ((,class (:foreground ,green :weight bold))))
   `(ido-subdir ((,class (:foreground ,blue))))

   `(ivy-current-match ((,class (:background ,bg-hl-strong :foreground ,fg-main :extend t))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,fg-dim))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,orange :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,green :weight bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,blue :weight bold))))

   `(vertico-current ((,class (:background ,bg-hl-strong :extend t))))
   `(orderless-match-face-0 ((,class (:foreground ,orange :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,green :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,blue :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,magenta :weight bold))))

   ;; Dired
   ;;
   `(dired-directory ((,class (:foreground ,blue :weight bold))))
   `(dired-symlink ((,class (:foreground ,cyan))))
   `(dired-marked ((,class (:background ,bg-block :foreground ,orange :weight bold))))
   `(dired-flagged ((,class (:foreground ,red :weight bold))))
   `(dired-header ((,class (:foreground ,fg-main :weight bold))))
   `(dired-ignored ((,class (:foreground ,fg-dim))))

   ;; Compilation
   ;;
   `(compilation-info ((,class (:foreground ,green :weight bold))))
   `(compilation-warning ((,class (:foreground ,orange :weight bold))))
   `(compilation-error ((,class (:foreground ,red :weight bold))))
   `(compilation-line-number ((,class (:foreground ,fg-dim))))
   `(compilation-mode-line-exit ((,class (:foreground ,green :weight bold))))
   `(compilation-mode-line-fail ((,class (:foreground ,red :weight bold))))

   ;; diff-mode / ediff
   ;;
   `(diff-added ((,class (:foreground ,green))))
   `(diff-removed ((,class (:foreground ,red))))
   `(diff-changed ((,class (:foreground ,blue))))
   `(diff-header ((,class (:background ,bg-block))))
   `(diff-file-header ((,class (:background ,bg-block :foreground ,fg-main :weight bold))))
   `(diff-hunk-header ((,class (:background ,bg-hl :foreground ,fg-dim))))
   `(diff-context ((,class (:foreground ,fg-dim))))
   `(diff-refine-added ((,class (:background ,bg-hl-strong :foreground ,green))))
   `(diff-refine-removed ((,class (:background ,bg-hl-strong :foreground ,red))))
   `(diff-refine-changed ((,class (:background ,bg-hl-strong :foreground ,blue))))
   `(ediff-current-diff-A ((,class (:background ,bg-hl-strong))))
   `(ediff-current-diff-B ((,class (:background ,bg-hl-strong))))
   `(ediff-current-diff-C ((,class (:background ,bg-hl-strong))))
   `(ediff-current-diff-Ancestor ((,class (:background ,bg-hl-strong))))
   `(ediff-even-diff-A ((,class (:background ,bg-hl))))
   `(ediff-even-diff-B ((,class (:background ,bg-hl))))
   `(ediff-even-diff-C ((,class (:background ,bg-hl))))
   `(ediff-even-diff-Ancestor ((,class (:background ,bg-hl))))
   `(ediff-odd-diff-A ((,class (:background ,bg-alt))))
   `(ediff-odd-diff-B ((,class (:background ,bg-alt))))
   `(ediff-odd-diff-C ((,class (:background ,bg-alt))))
   `(ediff-odd-diff-Ancestor ((,class (:background ,bg-alt))))
   `(ediff-fine-diff-A ((,class (:background ,bg-block))))
   `(ediff-fine-diff-B ((,class (:background ,bg-block))))
   `(ediff-fine-diff-C ((,class (:background ,bg-block))))
   `(ediff-fine-diff-Ancestor ((,class (:background ,bg-block))))

   ;; Magit
   ;;
   `(magit-section-heading ((,class (:foreground ,orange :weight bold))))
   `(magit-section-highlight ((,class (:background ,bg-hl))))
   `(magit-branch-local ((,class (:foreground ,blue :weight bold))))
   `(magit-branch-remote ((,class (:foreground ,green :weight bold))))
   `(magit-tag ((,class (:foreground ,brown :weight bold))))
   `(magit-hash ((,class (:foreground ,fg-faint))))
   `(magit-log-author ((,class (:foreground ,orange))))
   `(magit-log-date ((,class (:foreground ,fg-dim))))
   `(magit-diff-added ((,class (:foreground ,green :background ,bg-hl))))
   `(magit-diff-added-highlight ((,class (:foreground ,green :background ,bg-hl-strong))))
   `(magit-diff-removed ((,class (:foreground ,red :background ,bg-hl))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red :background ,bg-hl-strong))))
   `(magit-diff-context ((,class (:foreground ,fg-dim))))
   `(magit-diff-context-highlight ((,class (:background ,bg-hl :foreground ,fg-dim))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-block :foreground ,fg-dim))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg-block :foreground ,fg-main :weight bold))))
   `(magit-blame-heading ((,class (:background ,bg-block :foreground ,fg-dim))))

   ;; Org mode
   ;;
   `(org-level-1 ((,class (:foreground ,orange :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,green :height 1.15))))
   `(org-level-3 ((,class (:foreground ,blue :height 1.05))))
   `(org-level-4 ((,class (:foreground ,brown :height 1.0))))
   `(org-level-5 ((,class (:foreground ,cyan :height 1.0))))
   `(org-level-6 ((,class (:foreground ,magenta :height 1.0))))
   `(org-level-7 ((,class (:foreground ,orange :height 1.0))))
   `(org-level-8 ((,class (:foreground ,green :height 1.0))))
   `(org-document-title ((,class (:foreground ,fg-main :weight bold :height 1.4))))
   `(org-document-info ((,class (:foreground ,fg-dim :slant italic))))
   `(org-hide ((,class (:foreground ,bg-main))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-date ((,class (:foreground ,blue :underline t))))
   `(org-footnote ((,class (:foreground ,blue :underline t))))
   `(org-link ((,class (:foreground ,orange :underline t))))
   `(org-tag ((,class (:foreground ,fg-faint))))
   `(org-special-keyword ((,class (:foreground ,brown))))
   `(org-code ((,class (:foreground ,brown :background ,bg-alt))))
   `(org-verbatim ((,class (:foreground ,cyan))))
   `(org-block ((,class (:foreground ,fg-main :background ,bg-alt :extend t))))
   `(org-block-begin-line ((,class (:foreground ,fg-dim :background ,bg-block :extend t))))
   `(org-block-end-line ((,class (:foreground ,fg-dim :background ,bg-block :extend t))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-warning ((,class (:foreground ,red :weight bold))))
   `(org-agenda-structure ((,class (:foreground ,orange :weight bold))))
   `(org-agenda-date ((,class (:foreground ,blue))))
   `(org-agenda-date-weekend ((,class (:foreground ,fg-dim))))
   `(org-agenda-date-today ((,class (:foreground ,orange :weight bold))))
   `(org-scheduled ((,class (:foreground ,fg-dim))))
   `(org-scheduled-today ((,class (:foreground ,green))))
   `(org-scheduled-previously ((,class (:foreground ,red))))
   `(org-upcoming-deadline ((,class (:foreground ,red))))

   ;; Terminal / ANSI colors
   ;;
   `(term-color-black ((,class (:foreground ,fg-main :background ,fg-main))))
   `(term-color-red ((,class (:foreground ,red :background ,red))))
   `(term-color-green ((,class (:foreground ,green :background ,green))))
   `(term-color-yellow ((,class (:foreground ,yellow :background ,yellow))))
   `(term-color-blue ((,class (:foreground ,blue :background ,blue))))
   `(term-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
   `(term-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
   `(term-color-white ((,class (:foreground ,bg-main :background ,bg-main)))))

  (custom-theme-set-variables
   'metropolis-light
   `(ansi-color-names-vector
     [,fg-main ,red ,green ,yellow ,blue ,magenta ,cyan ,bg-main])))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'metropolis-light)

;;; metropolis-light-theme.el ends here
