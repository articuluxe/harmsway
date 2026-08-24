;;; gruber-darker-ayu-theme.el --- Gruber Darker + Ayu Dark color theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Hadi Alam

;; Author: Hadi Alam <hadialam493@gmail.com>
;; Assisted-by: opencode:deepseek-v4-flash-free
;; URL: https://github.com/Hadi493/gruber-darker-ayu-theme
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A blend of the Gruber Darker and Ayu Dark color themes.  Keeps
;; Gruber Darker's deep #181818 background while replacing its muted
;; syntax with the warm, high-contrast palette of Ayu Dark.
;;
;; Provides faces for Avy, Company, Compilation, Consult, Corfu,
;; Diff, Dired, Doom Modeline, Ediff, Eglot, Flycheck, Flymake,
;; Font Lock, Git Gutter, Magit, Marginalia, Markdown, Mode Line,
;; Multiple Cursors, Orderless, Org, Org Roam, PDF Tools, Rainbow
;; Delimiters, Tab Bar, Transient, Vertico, Which Key, Whitespace,
;; Yasnippet, and more.

;;; Code:

;;;###autoload
(deftheme gruber-darker-ayu
  "Gruber Darker × Ayu Dark color theme for Emacs 27+.")

;; Please, install rainbow-mode.
(let ((gruber-darker-ayu-fg         "#ffffff")
      (gruber-darker-ayu-bg         "#181818")
      (gruber-darker-ayu-bg-1       "#0d0d0d")
      (gruber-darker-ayu-bg+1       "#1c1c2a")
      (gruber-darker-ayu-bg+2       "#2a2a3e")
      (gruber-darker-ayu-bg+3       "#363652")
      (gruber-darker-ayu-gray       "#6B7B8E")
      (gruber-darker-ayu-gold       "#E6B450")
      (gruber-darker-ayu-orange     "#FF9944")
      (gruber-darker-ayu-func       "#FFD064")
      (gruber-darker-ayu-green      "#C5E665")
      (gruber-darker-ayu-blue       "#6DCFFF")
      (gruber-darker-ayu-purple     "#E0B8FF")
      (gruber-darker-ayu-red        "#FF7B85")
      (gruber-darker-ayu-peach      "#FFA375")
      (gruber-darker-ayu-cyan       "#4AC8E6")
      (gruber-darker-ayu-teal       "#A8F0D8")
      (gruber-darker-ayu-var        "#F5CD98")
      (gruber-darker-ayu-comment    "#95a99f")
      (gruber-darker-ayu-dim        "#3d4658"))
  (custom-theme-set-variables
   'gruber-darker-ayu
   '(frame-background-mode (quote dark))
   `(ansi-color-names-vector
     [,gruber-darker-ayu-bg-1
      ,gruber-darker-ayu-red
      ,gruber-darker-ayu-green
      ,gruber-darker-ayu-func
      ,gruber-darker-ayu-blue
      ,gruber-darker-ayu-purple
      ,gruber-darker-ayu-cyan
      ,gruber-darker-ayu-fg])
   `(ansi-color-faces-vector
     [default bold italic underline success warning error]))

  (custom-theme-set-faces
   'gruber-darker-ayu

   ;; Avy
   `(avy-lead-face ((t (:background ,gruber-darker-ayu-gold
                                    :foreground ,gruber-darker-ayu-bg))))
   `(avy-lead-face-0 ((t (:background ,gruber-darker-ayu-blue
                                      :foreground ,gruber-darker-ayu-bg))))
   `(avy-lead-face-1 ((t (:background ,gruber-darker-ayu-green
                                      :foreground ,gruber-darker-ayu-bg))))
   `(avy-lead-face-2 ((t (:background ,gruber-darker-ayu-purple
                                      :foreground ,gruber-darker-ayu-bg))))
   `(avy-background-face ((t (:foreground ,gruber-darker-ayu-bg+2))))
   `(avy-goto-char-timer-face ((t (:background ,gruber-darker-ayu-bg+1
                                               :foreground ,gruber-darker-ayu-fg))))

   ;; Basic Coloring (or Uncategorized)
   `(border ((t ,(list :background gruber-darker-ayu-bg-1
                       :foreground gruber-darker-ayu-bg+1))))
   `(cursor ((t (:background ,gruber-darker-ayu-gold))))
   `(default ((t ,(list :foreground gruber-darker-ayu-fg
                        :background gruber-darker-ayu-bg))))
   `(fringe ((t ,(list :background nil
                       :foreground gruber-darker-ayu-dim))))
   `(vertical-border ((t (:foreground ,gruber-darker-ayu-dim))))
   `(window-divider ((t (:foreground ,gruber-darker-ayu-dim))))
   `(link ((t (:foreground ,gruber-darker-ayu-blue :underline t))))
   `(link-visited ((t (:foreground ,gruber-darker-ayu-purple :underline t))))
   `(button ((t (:underline t :foreground ,gruber-darker-ayu-blue))))
   `(match ((t (:background ,gruber-darker-ayu-bg+2
                            :foreground ,gruber-darker-ayu-func))))
   `(shadow ((t (:foreground ,gruber-darker-ayu-dim))))
   `(highlight ((t (:background ,gruber-darker-ayu-bg+2
                                :foreground ,gruber-darker-ayu-fg))))
   `(minibuffer-prompt ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(region ((t (:background ,gruber-darker-ayu-bg+3 :foreground nil))))
   `(secondary-selection ((t ,(list :background gruber-darker-ayu-bg+3
                                    :foreground nil))))
   `(trailing-whitespace ((t ,(list :foreground gruber-darker-ayu-bg
                                    :background gruber-darker-ayu-red))))
   `(tooltip ((t ,(list :background gruber-darker-ayu-bg+1
                        :foreground gruber-darker-ayu-fg))))
   `(next-error ((t (:background ,gruber-darker-ayu-bg+2
                                 :foreground ,gruber-darker-ayu-fg :extend t))))
   `(escape-glyph ((t (:foreground ,gruber-darker-ayu-peach))))
   `(homoglyph ((t (:foreground ,gruber-darker-ayu-peach))))
   `(glyphless-char ((t (:height 0.6))))

   ;; Bookmarks
   `(bookmark-face ((t (:background ,gruber-darker-ayu-bg+2 :underline t))))

   ;; Child Frame
   `(child-frame-border ((t (:background ,gruber-darker-ayu-bg+1))))

   ;; Company
   `(company-tooltip ((t (:background ,gruber-darker-ayu-bg+1
                                      :foreground ,gruber-darker-ayu-fg))))
   `(company-tooltip-selection ((t (:background ,gruber-darker-ayu-bg+3
                                                :foreground ,gruber-darker-ayu-fg))))
   `(company-tooltip-common ((t (:foreground ,gruber-darker-ayu-func :bold t))))
   `(company-tooltip-common-selection ((t (:foreground ,gruber-darker-ayu-func :bold t))))
   `(company-tooltip-annotation ((t (:foreground ,gruber-darker-ayu-gray
                                                 :background ,gruber-darker-ayu-bg+1))))
   `(company-tooltip-mouse ((t (:background ,gruber-darker-ayu-bg+2))))
   `(company-scrollbar-fg ((t (:background ,gruber-darker-ayu-bg+3))))
   `(company-scrollbar-bg ((t (:background ,gruber-darker-ayu-bg+1))))
   `(company-preview-common ((t (:foreground ,gruber-darker-ayu-func
                                             :background ,gruber-darker-ayu-bg+2))))
   `(company-preview ((t (:background ,gruber-darker-ayu-bg+2
                                      :foreground ,gruber-darker-ayu-fg))))
   `(company-echo-common ((t (:foreground ,gruber-darker-ayu-func))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground gruber-darker-ayu-green
                                 :bold t
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground gruber-darker-ayu-func
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(compilation-mode-line-exit ((t ,(list :foreground gruber-darker-ayu-green
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-fail ((t ,(list :foreground gruber-darker-ayu-red
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-run ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(compilation-line-number ((t (:foreground ,gruber-darker-ayu-gray))))
   `(compilation-column-number ((t (:foreground ,gruber-darker-ayu-gray))))

   ;; Completions
   `(completions-common-part ((t (:foreground ,gruber-darker-ayu-func :bold t))))
   `(completions-first-difference ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(completions-highlight ((t (:background ,gruber-darker-ayu-bg+2
                                            :foreground ,gruber-darker-ayu-fg))))

   ;; Consult
   `(consult-preview-line ((t (:background ,gruber-darker-ayu-bg+1 :extend t))))
   `(consult-preview-cursor ((t (:foreground ,gruber-darker-ayu-gold
                                             :background ,gruber-darker-ayu-bg))))
   `(consult-preview-match ((t (:background ,gruber-darker-ayu-bg+2
                                            :foreground ,gruber-darker-ayu-func))))
   `(consult-narrow-indicator ((t (:foreground ,gruber-darker-ayu-gold))))
   `(consult-async-split-style-name ((t (:foreground ,gruber-darker-ayu-blue))))

   ;; Corfu
   `(corfu-default ((t (:background ,gruber-darker-ayu-bg+1
                                    :foreground ,gruber-darker-ayu-fg))))
   `(corfu-current ((t (:background ,gruber-darker-ayu-bg+3
                                    :foreground ,gruber-darker-ayu-fg
                                    :weight bold))))
   `(corfu-bar ((t (:background ,gruber-darker-ayu-gold))))
   `(corfu-border ((t (:background ,gruber-darker-ayu-bg+2))))
   `(corfu-popupinfo ((t (:background ,gruber-darker-ayu-bg+1
                                      :foreground ,gruber-darker-ayu-fg))))
   `(corfu-popupinfo-kind ((t (:foreground ,gruber-darker-ayu-gold))))
   `(corfu-indexed ((t (:foreground ,gruber-darker-ayu-gray :bold t))))

   ;; Diff
   `(diff-added ((t ,(list :foreground gruber-darker-ayu-green
                           :background "#1a2e1a"))))
   `(diff-removed ((t ,(list :foreground gruber-darker-ayu-red
                             :background "#2e1a1a"))))
   `(diff-refine-added ((t (:background "#243e24"))))
   `(diff-refine-removed ((t (:background "#3e2424"))))
   `(diff-header ((t (:background ,gruber-darker-ayu-bg+1))))
   `(diff-file-header ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(diff-hunk-header ((t (:foreground ,gruber-darker-ayu-gold
                                       :background ,gruber-darker-ayu-bg+2))))
   `(diff-index ((t (:foreground ,gruber-darker-ayu-gray))))
   `(diff-context ((t (:foreground ,gruber-darker-ayu-fg))))

   ;; diff-hl
   `(diff-hl-insert ((t (:foreground ,gruber-darker-ayu-green
                                     :background "#1a2e1a"))))
   `(diff-hl-change ((t (:foreground ,gruber-darker-ayu-func
                                     :background "#2a2a1a"))))
   `(diff-hl-delete ((t (:foreground ,gruber-darker-ayu-red
                                     :background "#2e1a1a"))))

   ;; Dired
   `(dired-directory ((t (:foreground ,gruber-darker-ayu-blue :bold t))))
   `(dired-symlink ((t (:foreground ,gruber-darker-ayu-orange))))
   `(dired-flagged ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(dired-marked ((t (:foreground ,gruber-darker-ayu-func :bold t))))
   `(dired-header ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(dired-ignored ((t ,(list :foreground gruber-darker-ayu-gray
                              :inherit 'unspecified))))
   `(dired-perm-write ((t (:foreground ,gruber-darker-ayu-fg))))
   `(dired-warning ((t (:foreground ,gruber-darker-ayu-func))))

   ;; Doom Modeline
   `(doom-modeline-bar ((t (:background ,gruber-darker-ayu-gold
                                        :foreground ,gruber-darker-ayu-bg))))
   `(doom-modeline-bar-inactive ((t (:background ,gruber-darker-ayu-bg+1))))

   ;; Ediff
   `(ediff-fine-diff-A ((t (:background "#3e2424"))))
   `(ediff-fine-diff-B ((t (:background "#1a3e1a"))))
   `(ediff-fine-diff-C ((t (:background "#1a2a3e"))))
   `(ediff-current-diff-A ((t (:background "#2e1a1a"))))
   `(ediff-current-diff-B ((t (:background "#1a2e1a"))))
   `(ediff-current-diff-C ((t (:background "#1a1a2e"))))
   `(ediff-even-diff-A ((t (:background ,gruber-darker-ayu-bg-1))))
   `(ediff-even-diff-B ((t (:background ,gruber-darker-ayu-bg-1))))
   `(ediff-even-diff-C ((t (:background ,gruber-darker-ayu-bg-1))))
   `(ediff-odd-diff-A ((t (:background ,gruber-darker-ayu-bg+1))))
   `(ediff-odd-diff-B ((t (:background ,gruber-darker-ayu-bg+1))))
   `(ediff-odd-diff-C ((t (:background ,gruber-darker-ayu-bg+1))))

   ;; Eglot
   `(eglot-diagnostic-tag-unnecessary-face ((t (:foreground ,gruber-darker-ayu-gray
                                                            :italic t))))
   `(eglot-diagnostic-tag-deprecated-face ((t (:strike-through t))))
   `(eglot-mode-line ((t (:foreground ,gruber-darker-ayu-gold))))

   ;; Error / Success / Warning
   `(error ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(success ((t (:foreground ,gruber-darker-ayu-green :bold t))))
   `(warning ((t (:foreground ,gruber-darker-ayu-func))))

   ;; Flycheck
   `(flycheck-fringe-error ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(flycheck-fringe-warning ((t (:foreground ,gruber-darker-ayu-func :bold t))))
   `(flycheck-fringe-info ((t (:foreground ,gruber-darker-ayu-green :bold t))))
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,gruber-darker-ayu-red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,gruber-darker-ayu-red :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,gruber-darker-ayu-func)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,gruber-darker-ayu-func :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,gruber-darker-ayu-green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,gruber-darker-ayu-green :weight bold :underline t))))

   ;; Flymake
   `(flymake-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,gruber-darker-ayu-red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,gruber-darker-ayu-red :weight bold :underline t))))
   `(flymake-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,gruber-darker-ayu-func)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,gruber-darker-ayu-func :weight bold :underline t))))
   `(flymake-note
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,gruber-darker-ayu-green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,gruber-darker-ayu-green :weight bold :underline t))))

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,gruber-darker-ayu-red))))
   `(font-lock-comment-face ((t (:foreground ,gruber-darker-ayu-comment :italic t))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,gruber-darker-ayu-comment
                                                       :italic t))))
   `(font-lock-constant-face ((t (:foreground ,gruber-darker-ayu-purple))))
   `(font-lock-doc-face ((t (:foreground ,gruber-darker-ayu-comment))))
   `(font-lock-doc-markup-face ((t (:foreground ,gruber-darker-ayu-func))))
   `(font-lock-function-name-face ((t (:foreground ,gruber-darker-ayu-func))))
   `(font-lock-keyword-face ((t (:foreground ,gruber-darker-ayu-orange :bold t))))
   `(font-lock-negation-char-face ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(font-lock-number-face ((t (:foreground ,gruber-darker-ayu-teal))))
   `(font-lock-operator-face ((t (:foreground ,gruber-darker-ayu-peach))))
   `(font-lock-preprocessor-face ((t (:foreground ,gruber-darker-ayu-red))))
   `(font-lock-property-name-face ((t (:foreground ,gruber-darker-ayu-var))))
   `(font-lock-punctuation-face ((t (:foreground ,gruber-darker-ayu-peach))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,gruber-darker-ayu-cyan :bold t))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,gruber-darker-ayu-purple :bold t))))
   `(font-lock-string-face ((t (:foreground ,gruber-darker-ayu-green))))
   `(font-lock-type-face ((t (:foreground ,gruber-darker-ayu-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,gruber-darker-ayu-var))))
   `(font-lock-bracket-face ((t (:foreground ,gruber-darker-ayu-peach))))
   `(font-lock-delimiter-face ((t (:foreground ,gruber-darker-ayu-peach))))

   ;; Git Gutter
   `(git-gutter:added ((t (:foreground ,gruber-darker-ayu-green :bold t))))
   `(git-gutter:modified ((t (:foreground ,gruber-darker-ayu-func :bold t))))
   `(git-gutter:deleted ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(git-gutter-fr:added ((t (:foreground ,gruber-darker-ayu-green :bold t))))
   `(git-gutter-fr:modified ((t (:foreground ,gruber-darker-ayu-func :bold t))))
   `(git-gutter-fr:deleted ((t (:foreground ,gruber-darker-ayu-red :bold t))))

   ;; Header
   `(header-line ((t (:background ,gruber-darker-ayu-bg+1
                                  :foreground ,gruber-darker-ayu-gray))))
   `(header-line-highlight ((t (:background ,gruber-darker-ayu-bg+2))))

   ;; Help
   `(help-argument-name ((t (:foreground ,gruber-darker-ayu-var))))

   ;; Hl-todo
   `(hl-todo ((t (:foreground ,gruber-darker-ayu-red :bold t))))

   ;; Info
   `(info-header-xref ((t (:foreground ,gruber-darker-ayu-blue :underline t))))
   `(info-menu-header ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(info-xref ((t (:foreground ,gruber-darker-ayu-blue :underline t))))

   ;; Line Numbers
   `(line-number ((t (:inherit default :foreground ,gruber-darker-ayu-dim))))
   `(line-number-current-line ((t (:inherit line-number
                                            :foreground ,gruber-darker-ayu-gray
                                            :background ,gruber-darker-ayu-bg+1))))
   `(line-number-major-tick ((t (:foreground ,gruber-darker-ayu-fg
                                             :background ,gruber-darker-ayu-bg+2))))
   `(line-number-minor-tick ((t (:foreground ,gruber-darker-ayu-gray
                                             :background ,gruber-darker-ayu-bg+2))))

   ;; Magit
   `(magit-branch-local ((t (:foreground ,gruber-darker-ayu-func))))
   `(magit-branch-remote ((t (:foreground ,gruber-darker-ayu-blue))))
   `(magit-branch-upstream ((t (:foreground ,gruber-darker-ayu-teal))))
   `(magit-branch-warning ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(magit-branch-current ((t (:foreground ,gruber-darker-ayu-gold :bold t
                                           :box (:line-width 1 :color ,gruber-darker-ayu-gold)))))
   `(magit-diff-added ((t (:foreground ,gruber-darker-ayu-green
                                       :background "#1a2e1a"))))
   `(magit-diff-removed ((t (:foreground ,gruber-darker-ayu-red
                                         :background "#2e1a1a"))))
   `(magit-diff-added-highlight ((t (:foreground ,gruber-darker-ayu-green
                                                 :background "#243e24"))))
   `(magit-diff-removed-highlight ((t (:foreground ,gruber-darker-ayu-red
                                                   :background "#3e2424"))))
   `(magit-diff-hunk-heading ((t (:foreground ,gruber-darker-ayu-gold
                                              :background ,gruber-darker-ayu-bg+2))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,gruber-darker-ayu-gold
                                                        :background ,gruber-darker-ayu-bg+3))))
   `(magit-diff-hunk-region ((t (:foreground ,gruber-darker-ayu-gold))))
   `(magit-diff-context ((t (:foreground ,gruber-darker-ayu-bg+2))))
   `(magit-diff-context-highlight ((t (:foreground ,gruber-darker-ayu-fg
                                                   :background ,gruber-darker-ayu-bg+1))))
   `(magit-diffstat-added ((t (:foreground ,gruber-darker-ayu-green))))
   `(magit-diffstat-removed ((t (:foreground ,gruber-darker-ayu-red))))
   `(magit-diff-file-heading ((t (:foreground ,gruber-darker-ayu-fg))))
   `(magit-diff-file-heading-highlight ((t (:foreground ,gruber-darker-ayu-fg
                                                        :background ,gruber-darker-ayu-bg+1))))
   `(magit-diff-file-heading-selection ((t (:foreground ,gruber-darker-ayu-gold
                                                        :background ,gruber-darker-ayu-bg+2))))
   `(magit-hash ((t (:foreground ,gruber-darker-ayu-gray))))
   `(magit-log-author ((t (:foreground ,gruber-darker-ayu-var))))
   `(magit-log-date ((t (:foreground ,gruber-darker-ayu-gray))))
   `(magit-log-graph ((t (:foreground ,gruber-darker-ayu-gray))))
   `(magit-log-message ((t (:foreground ,gruber-darker-ayu-fg))))
   `(magit-process-ok ((t (:foreground ,gruber-darker-ayu-green :bold t))))
   `(magit-process-ng ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(magit-refname ((t (:foreground ,gruber-darker-ayu-gray))))
   `(magit-refname-stash ((t (:foreground ,gruber-darker-ayu-gold))))
   `(magit-refname-wip ((t (:foreground ,gruber-darker-ayu-func))))
   `(magit-reflog-amend ((t (:foreground ,gruber-darker-ayu-orange))))
   `(magit-reflog-checkout ((t (:foreground ,gruber-darker-ayu-blue))))
   `(magit-reflog-cherry-pick ((t (:foreground ,gruber-darker-ayu-green))))
   `(magit-reflog-commit ((t (:foreground ,gruber-darker-ayu-gold))))
   `(magit-reflog-merge ((t (:foreground ,gruber-darker-ayu-green))))
   `(magit-reflog-other ((t (:foreground ,gruber-darker-ayu-gray))))
   `(magit-reflog-rebase ((t (:foreground ,gruber-darker-ayu-purple))))
   `(magit-reflog-remote ((t (:foreground ,gruber-darker-ayu-blue))))
   `(magit-reflog-reset ((t (:foreground ,gruber-darker-ayu-red))))
   `(magit-section-heading ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(magit-section-heading-selection ((t (:foreground ,gruber-darker-ayu-func))))
   `(magit-section-highlight ((t (:background ,gruber-darker-ayu-bg+1 :extend t))))
   `(magit-sequence-done ((t (:foreground ,gruber-darker-ayu-green))))
   `(magit-sequence-drop ((t (:foreground ,gruber-darker-ayu-red))))
   `(magit-sequence-head ((t (:foreground ,gruber-darker-ayu-gold))))
   `(magit-sequence-part ((t (:foreground ,gruber-darker-ayu-orange))))
   `(magit-sequence-stop ((t (:foreground ,gruber-darker-ayu-func))))
   `(magit-signature-bad ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(magit-signature-good ((t (:foreground ,gruber-darker-ayu-green :bold t))))
   `(magit-signature-untrusted ((t (:foreground ,gruber-darker-ayu-func :bold t))))
   `(magit-signature-error ((t (:foreground ,gruber-darker-ayu-red))))
   `(magit-signature-expired ((t (:foreground ,gruber-darker-ayu-func))))
   `(magit-signature-expired-key ((t (:foreground ,gruber-darker-ayu-red))))
   `(magit-tag ((t (:foreground ,gruber-darker-ayu-func))))

   ;; Marginalia
   `(marginalia-dir ((t (:foreground ,gruber-darker-ayu-blue :bold t))))
   `(marginalia-symlink ((t (:foreground ,gruber-darker-ayu-orange))))
   `(marginalia-date ((t (:foreground ,gruber-darker-ayu-teal))))
   `(marginalia-documentation ((t (:foreground ,gruber-darker-ayu-gray))))
   `(marginalia-key ((t (:foreground ,gruber-darker-ayu-orange))))
   `(marginalia-size ((t (:foreground ,gruber-darker-ayu-teal))))
   `(marginalia-type ((t (:foreground ,gruber-darker-ayu-blue))))
   `(marginalia-value ((t (:foreground ,gruber-darker-ayu-var))))
   `(marginalia-modified ((t (:foreground ,gruber-darker-ayu-func))))
   `(marginalia-char ((t (:foreground ,gruber-darker-ayu-peach))))
   `(marginalia-null ((t (:foreground ,gruber-darker-ayu-gray))))
   `(marginalia-string ((t (:foreground ,gruber-darker-ayu-green))))
   `(marginalia-number ((t (:foreground ,gruber-darker-ayu-teal))))
   `(marginalia-function ((t (:foreground ,gruber-darker-ayu-func))))
   `(marginalia-variable ((t (:foreground ,gruber-darker-ayu-var))))
   `(marginalia-face ((t (:foreground ,gruber-darker-ayu-purple))))
   `(marginalia-mode ((t (:foreground ,gruber-darker-ayu-blue))))
   `(marginalia-true ((t (:foreground ,gruber-darker-ayu-green))))
   `(marginalia-false ((t (:foreground ,gruber-darker-ayu-red))))
   `(marginalia-on ((t (:foreground ,gruber-darker-ayu-green))))
   `(marginalia-off ((t (:foreground ,gruber-darker-ayu-red))))
   `(marginalia-file ((t (:foreground ,gruber-darker-ayu-fg))))

   ;; Markdown
   `(markdown-header-face-1 ((t (:foreground ,gruber-darker-ayu-red :bold t :height 1.3))))
   `(markdown-header-face-2 ((t (:foreground ,gruber-darker-ayu-gold :bold t :height 1.2))))
   `(markdown-header-face-3 ((t (:foreground ,gruber-darker-ayu-blue :bold t :height 1.1))))
   `(markdown-header-face-4 ((t (:foreground ,gruber-darker-ayu-green :bold t))))
   `(markdown-header-face-5 ((t (:foreground ,gruber-darker-ayu-orange :bold t))))
   `(markdown-header-face-6 ((t (:foreground ,gruber-darker-ayu-purple :bold t))))
   `(markdown-header-delimiter-face ((t (:foreground ,gruber-darker-ayu-gray))))
   `(markdown-header-rule-face ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(markdown-markup-face ((t (:foreground ,gruber-darker-ayu-gray))))
   `(markdown-link-face ((t (:foreground ,gruber-darker-ayu-blue :underline t))))
   `(markdown-url-face ((t (:foreground ,gruber-darker-ayu-green))))
   `(markdown-link-title-face ((t (:foreground ,gruber-darker-ayu-gray))))
   `(markdown-reference-face ((t (:foreground ,gruber-darker-ayu-gray))))
   `(markdown-footnote-face ((t (:foreground ,gruber-darker-ayu-purple))))
   `(markdown-list-face ((t (:foreground ,gruber-darker-ayu-peach))))
   `(markdown-blockquote-face ((t (:foreground ,gruber-darker-ayu-gray :italic t))))
   `(markdown-pre-face ((t (:background ,gruber-darker-ayu-bg-1 :extend t))))
   `(markdown-inline-code-face ((t (:foreground ,gruber-darker-ayu-green))))
   `(markdown-code-face ((t (:background ,gruber-darker-ayu-bg-1 :extend t))))
   `(markdown-highlight-face ((t (:background ,gruber-darker-ayu-bg+2))))
   `(markdown-metadata-key-face ((t (:foreground ,gruber-darker-ayu-orange))))
   `(markdown-metadata-value-face ((t (:foreground ,gruber-darker-ayu-gray))))
   `(markdown-language-keyword-face ((t (:foreground ,gruber-darker-ayu-gray))))
   `(markdown-table-face ((t (:foreground ,gruber-darker-ayu-blue))))
   `(markdown-italic-face ((t (:italic t))))
   `(markdown-bold-face ((t (:bold t))))
   `(markdown-strike-through-face ((t (:strike-through t))))
   `(markdown-html-tag-name-face ((t (:foreground ,gruber-darker-ayu-orange))))
   `(markdown-html-attr-name-face ((t (:foreground ,gruber-darker-ayu-var))))
   `(markdown-html-attr-value-face ((t (:foreground ,gruber-darker-ayu-green))))

   ;; Mode Line
   `(mode-line ((t ,(list :background gruber-darker-ayu-bg+1
                          :foreground gruber-darker-ayu-fg
                          :box nil))))
   `(mode-line-inactive ((t ,(list :background gruber-darker-ayu-bg
                                   :foreground gruber-darker-ayu-gray
                                   :box nil))))
   `(mode-line-highlight ((t (:background ,gruber-darker-ayu-bg+2))))
   `(mode-line-emphasis ((t (:bold t))))
   `(mode-line-buffer-id ((t (:bold t :foreground ,gruber-darker-ayu-func))))

   ;; Multiple Cursors
   `(mc/cursor-face ((t (:background ,gruber-darker-ayu-gold
                                     :foreground ,gruber-darker-ayu-bg
                                     :inverse-video nil))))
   `(mc/region-face ((t (:background ,gruber-darker-ayu-bg+2 :extend t))))

   ;; Orderless
   `(orderless-match-face-0 ((t (:foreground ,gruber-darker-ayu-func :bold t))))
   `(orderless-match-face-1 ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(orderless-match-face-2 ((t (:foreground ,gruber-darker-ayu-blue :bold t))))
   `(orderless-match-face-3 ((t (:foreground ,gruber-darker-ayu-green :bold t))))

   ;; Org Agenda
   `(org-agenda-structure ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(org-agenda-structure-filter ((t (:foreground ,gruber-darker-ayu-orange))))
   `(org-agenda-structure-secondary ((t (:foreground ,gruber-darker-ayu-blue))))
   `(org-agenda-date ((t (:foreground ,gruber-darker-ayu-blue))))
   `(org-agenda-date-today ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(org-agenda-date-weekend ((t (:foreground ,gruber-darker-ayu-purple))))
   `(org-agenda-done ((t (:foreground ,gruber-darker-ayu-green))))
   `(org-agenda-dimmed-todo-face ((t (:foreground ,gruber-darker-ayu-gray))))
   `(org-scheduled ((t (:foreground ,gruber-darker-ayu-green))))
   `(org-scheduled-today ((t (:foreground ,gruber-darker-ayu-gold))))
   `(org-scheduled-previously ((t (:foreground ,gruber-darker-ayu-func))))
   `(org-upcoming-deadline ((t (:foreground ,gruber-darker-ayu-orange))))
   `(org-upcoming-distant-deadline ((t (:foreground ,gruber-darker-ayu-gray))))
   `(org-warning ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(org-deadline-announce ((t (:foreground ,gruber-darker-ayu-red))))
   `(org-imminent-deadline ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(org-sexp-date ((t (:foreground ,gruber-darker-ayu-gray))))
   `(org-diagonal ((t (:strike-through t))))

   ;; Org Clock
   `(org-clocking ((t (:background ,gruber-darker-ayu-bg+2 :extend t))))
   `(org-clock-overlay ((t (:background ,gruber-darker-ayu-bg+2
                                        :foreground ,gruber-darker-ayu-fg))))
   `(org-mode-line-clock ((t (:foreground ,gruber-darker-ayu-gold))))
   `(org-mode-line-clock-overrun ((t (:foreground ,gruber-darker-ayu-red :bold t))))

   ;; Org Columns
   `(org-column ((t (:background ,gruber-darker-ayu-bg+1))))
   `(org-column-title ((t (:background ,gruber-darker-ayu-bg+2
                                       :underline t :bold t))))

   ;; Org Content
   `(org-date ((t (:foreground ,gruber-darker-ayu-blue :underline t))))
   `(org-time-grid ((t (:foreground ,gruber-darker-ayu-gray))))
   `(org-today ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(org-done ((t (:foreground ,gruber-darker-ayu-green :bold t))))
   `(org-todo ((t (:foreground ,gruber-darker-ayu-orange :bold t))))
   `(org-link ((t (:foreground ,gruber-darker-ayu-blue :underline t))))
   `(org-code ((t (:foreground ,gruber-darker-ayu-green))))
   `(org-table ((t (:foreground ,gruber-darker-ayu-blue))))
   `(org-formula ((t (:foreground ,gruber-darker-ayu-red))))
   `(org-checkbox ((t (:foreground ,gruber-darker-ayu-green :bold t))))
   `(org-checkbox-statistics-done ((t (:foreground ,gruber-darker-ayu-green :bold t))))
   `(org-checkbox-statistics-todo ((t (:foreground ,gruber-darker-ayu-orange :bold t))))
   `(org-hide ((t (:foreground ,gruber-darker-ayu-bg))))
   `(org-block ((t (:background ,gruber-darker-ayu-bg-1 :extend t))))
   `(org-block-begin-line ((t (:foreground ,gruber-darker-ayu-gray
                                           :background ,gruber-darker-ayu-bg+1
                                           :extend t :italic t))))
   `(org-block-end-line ((t (:foreground ,gruber-darker-ayu-gray
                                         :background ,gruber-darker-ayu-bg+1
                                         :extend t :italic t))))
   `(org-drawer ((t (:foreground ,gruber-darker-ayu-gray))))
   `(org-property-value ((t (:foreground ,gruber-darker-ayu-gray))))
   `(org-special-keyword ((t (:foreground ,gruber-darker-ayu-gray))))
   `(org-tag ((t (:foreground ,gruber-darker-ayu-bg+3 :bold t))))
   `(org-ellipsis ((t (:foreground ,gruber-darker-ayu-bg+3))))
   `(org-priority ((t (:foreground ,gruber-darker-ayu-red :bold t))))
   `(org-macro ((t (:foreground ,gruber-darker-ayu-purple))))
   `(org-latex-and-related ((t (:foreground ,gruber-darker-ayu-purple))))
   `(org-tag-group ((t (:foreground ,gruber-darker-ayu-gray))))
   `(org-target ((t (:underline t))))
   `(org-list-dt ((t (:bold t))))
   `(org-quote ((t (:background ,gruber-darker-ayu-bg+1 :italic t :extend t))))
   `(org-verse ((t (:background ,gruber-darker-ayu-bg+1 :italic t :extend t))))

   ;; Org Headings
   `(org-level-1 ((t (:foreground ,gruber-darker-ayu-red :bold t :height 1.3))))
   `(org-level-2 ((t (:foreground ,gruber-darker-ayu-gold :bold t :height 1.2))))
   `(org-level-3 ((t (:foreground ,gruber-darker-ayu-blue :height 1.1))))
   `(org-level-4 ((t (:foreground ,gruber-darker-ayu-green))))
   `(org-level-5 ((t (:foreground ,gruber-darker-ayu-orange))))
   `(org-level-6 ((t (:foreground ,gruber-darker-ayu-purple))))
   `(org-level-7 ((t (:foreground ,gruber-darker-ayu-gray))))
   `(org-level-8 ((t (:foreground ,gruber-darker-ayu-gold))))
   `(org-headline-done ((t (:foreground ,gruber-darker-ayu-gray :strike-through t))))
   `(org-document-title ((t (:foreground ,gruber-darker-ayu-red :bold t :height 1.5))))
   `(org-document-info ((t (:foreground ,gruber-darker-ayu-blue))))
   `(org-document-info-keyword ((t (:foreground ,gruber-darker-ayu-gray))))

   ;; Org Roam
   `(org-roam-title ((t (:foreground ,gruber-darker-ayu-red :bold t :height 1.3))))
   `(org-roam-heading ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(org-roam-link ((t (:foreground ,gruber-darker-ayu-blue :underline t))))
   `(org-roam-link-current ((t (:foreground ,gruber-darker-ayu-blue :underline t))))
   `(org-roam-tag ((t (:foreground ,gruber-darker-ayu-bg+3 :bold t))))
   `(org-roam-dimmed ((t (:foreground ,gruber-darker-ayu-gray))))
   `(org-roam-dailies-calendar-note ((t (:background ,gruber-darker-ayu-bg+2
                                                     :foreground ,gruber-darker-ayu-fg))))
   `(org-roam-highlight ((t (:foreground ,gruber-darker-ayu-func
                                         :background ,gruber-darker-ayu-bg+2))))

   ;; Paren
   `(show-paren-match ((t (:foreground ,gruber-darker-ayu-bg
                                       :background ,gruber-darker-ayu-gold))))
   `(show-paren-match-expression ((t (:background ,gruber-darker-ayu-bg+2))))
   `(show-paren-mismatch ((t (:foreground ,gruber-darker-ayu-bg
                                          :background ,gruber-darker-ayu-red))))

   ;; PDF Tools
   `(pdf-view-midnight-colors ((t (:foreground ,gruber-darker-ayu-fg
                                               :background ,gruber-darker-ayu-bg))))
   `(pdf-links-link ((t (:foreground ,gruber-darker-ayu-blue :underline t))))
   `(pdf-links-read-link ((t (:background ,gruber-darker-ayu-bg+2
                                          :foreground ,gruber-darker-ayu-blue))))

   ;; Rainbow Delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,gruber-darker-ayu-red))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,gruber-darker-ayu-gold))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,gruber-darker-ayu-blue))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,gruber-darker-ayu-green))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,gruber-darker-ayu-orange))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,gruber-darker-ayu-purple))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,gruber-darker-ayu-teal))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,gruber-darker-ayu-cyan))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,gruber-darker-ayu-peach))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,gruber-darker-ayu-bg
                                                        :background ,gruber-darker-ayu-red
                                                        :bold t))))
   `(rainbow-delimiters-mismatched-face ((t (:foreground ,gruber-darker-ayu-bg
                                                         :background ,gruber-darker-ayu-red
                                                         :bold t))))

   ;; Scroll Bar
   `(scroll-bar ((t (:background ,gruber-darker-ayu-bg+1
                                 :foreground ,gruber-darker-ayu-bg+3))))

   ;; Search
   `(isearch ((t ,(list :foreground gruber-darker-ayu-bg
                        :background gruber-darker-ayu-gold))))
   `(isearch-fail ((t ,(list :foreground gruber-darker-ayu-bg
                             :background gruber-darker-ayu-red))))
   `(lazy-highlight ((t (:foreground ,gruber-darker-ayu-bg
                                     :background ,gruber-darker-ayu-blue))))
   `(query-replace ((t (:foreground ,gruber-darker-ayu-bg
                                    :background ,gruber-darker-ayu-func))))

   ;; Tab Bar
   `(tab-bar ((t (:background ,gruber-darker-ayu-bg
                              :foreground ,gruber-darker-ayu-fg))))
   `(tab-bar-tab ((t (:background ,gruber-darker-ayu-bg+1
                                  :foreground ,gruber-darker-ayu-fg :box nil))))
   `(tab-bar-tab-inactive ((t (:background ,gruber-darker-ayu-bg
                                           :foreground ,gruber-darker-ayu-gray :box nil))))
   `(tab-bar-tab-ungrouped ((t (:background ,gruber-darker-ayu-bg+1
                                            :foreground ,gruber-darker-ayu-fg :box nil))))
   `(tab-line ((t (:background ,gruber-darker-ayu-bg
                               :foreground ,gruber-darker-ayu-fg :height 0.9))))
   `(tab-line-tab ((t (:background ,gruber-darker-ayu-bg+1
                                   :foreground ,gruber-darker-ayu-fg :box nil))))
   `(tab-line-tab-inactive ((t (:background ,gruber-darker-ayu-bg
                                            :foreground ,gruber-darker-ayu-gray :box nil))))
   `(tab-line-close-highlight ((t (:foreground ,gruber-darker-ayu-red))))

   ;; Text Formatting
   `(bold ((t (:bold t))))
   `(italic ((t (:italic t))))
   `(bold-italic ((t (:bold t :italic t))))

   ;; Tool Bar
   `(tool-bar ((t (:background ,gruber-darker-ayu-bg+1
                               :foreground ,gruber-darker-ayu-fg))))
   `(menu ((t (:background ,gruber-darker-ayu-bg+1
                           :foreground ,gruber-darker-ayu-fg))))

   ;; Transient
   `(transient-heading ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(transient-key ((t (:foreground ,gruber-darker-ayu-orange))))
   `(transient-key-exit ((t (:foreground ,gruber-darker-ayu-red))))
   `(transient-key-no-exit ((t (:foreground ,gruber-darker-ayu-orange))))
   `(transient-key-stay ((t (:foreground ,gruber-darker-ayu-green))))
   `(transient-unreachable ((t (:foreground ,gruber-darker-ayu-gray))))
   `(transient-value ((t (:foreground ,gruber-darker-ayu-var))))
   `(transient-inactive-argument ((t (:foreground ,gruber-darker-ayu-gray))))
   `(transient-inactive-value ((t (:foreground ,gruber-darker-ayu-gray))))
   `(transient-inapt-suffix ((t (:foreground ,gruber-darker-ayu-gray :italic t))))
   `(transient-mismatched-key ((t (:underline (:style wave :color ,gruber-darker-ayu-red)))))

   ;; TTY Menu
   `(tty-menu-enabled-face ((t (:background ,gruber-darker-ayu-bg+1
                                            :foreground ,gruber-darker-ayu-fg))))
   `(tty-menu-disabled-face ((t (:background ,gruber-darker-ayu-bg+1
                                             :foreground ,gruber-darker-ayu-gray))))
   `(tty-menu-selected-face ((t (:background ,gruber-darker-ayu-gold
                                             :foreground ,gruber-darker-ayu-bg))))

   ;; Vertico
   `(vertico-current ((t (:background ,gruber-darker-ayu-bg+3
                                      :foreground ,gruber-darker-ayu-fg
                                      :weight bold))))
   `(vertico-group-title ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(vertico-group-separator ((t (:foreground ,gruber-darker-ayu-bg+2
                                              :strike-through t))))
   `(vertico-multiline ((t (:background ,gruber-darker-ayu-bg+1))))
   `(vertico-sort-overlay ((t (:foreground ,gruber-darker-ayu-gray))))

   ;; Which Key
   `(which-key-key-face ((t (:foreground ,gruber-darker-ayu-orange))))
   `(which-key-group-description-face ((t (:foreground ,gruber-darker-ayu-blue))))
   `(which-key-command-description-face ((t (:foreground ,gruber-darker-ayu-fg))))
   `(which-key-local-map-description-face ((t (:foreground ,gruber-darker-ayu-green))))
   `(which-key-separator-face ((t (:foreground ,gruber-darker-ayu-bg+3))))
   `(which-key-note-face ((t (:foreground ,gruber-darker-ayu-gray))))
   `(which-key-highlighted-command-face ((t (:foreground ,gruber-darker-ayu-gold :bold t))))
   `(which-key-special-key-face ((t (:foreground ,gruber-darker-ayu-red :bold t))))

   ;; Whitespace
   `(whitespace-space ((t ,(list :background gruber-darker-ayu-bg
                                 :foreground gruber-darker-ayu-bg+2))))
   `(whitespace-hspace ((t ,(list :background gruber-darker-ayu-bg
                                  :foreground gruber-darker-ayu-bg+2))))
   `(whitespace-tab ((t ,(list :background gruber-darker-ayu-bg
                               :foreground gruber-darker-ayu-bg+2))))
   `(whitespace-newline ((t ,(list :background gruber-darker-ayu-bg
                                   :foreground gruber-darker-ayu-bg+2))))
   `(whitespace-trailing ((t ,(list :background "#3e1a1a"
                                    :foreground "#3e1a1a"))))
   `(whitespace-line ((t ,(list :background gruber-darker-ayu-bg+2
                                :foreground gruber-darker-ayu-func))))
   `(whitespace-indentation ((t ,(list :background gruber-darker-ayu-bg
                                       :foreground gruber-darker-ayu-bg+2))))
   `(whitespace-empty ((t (:foreground ,gruber-darker-ayu-bg+2
                                       :background ,gruber-darker-ayu-bg-1))))
   `(whitespace-space-after-tab ((t (:foreground ,gruber-darker-ayu-bg+2))))
   `(whitespace-space-before-tab ((t (:foreground ,gruber-darker-ayu-bg+2))))

   ;; Writegood
   `(writegood-duplicate-face ((t (:underline (:style wave :color ,gruber-darker-ayu-purple)))))
   `(writegood-weasel-face ((t (:underline (:style wave :color ,gruber-darker-ayu-func)))))
   `(writegood-passive-face ((t (:underline (:style wave :color ,gruber-darker-ayu-orange)))))

   ;; Yasnippet
   `(yas-field-highlight-face ((t (:background ,gruber-darker-ayu-bg+2 :extend t))))))

(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gruber-darker-ayu)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gruber-darker-ayu-theme.el ends here.
