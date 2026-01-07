;;; amaranth-dark-theme.el --- Amaranth Dark theme -*- lexical-binding: t; -*-

;; Copyright (C) 2025-today Emiliano Rizzonelli

;; Author: Emiliano Rizzonelli <emiliano.rizzonelli@proton.me>
;; URL: http://github.com/a9sk/amaranth-dark-theme
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Amaranth Dark is a high-contrast dark color theme for Emacs.
;; It focuses on readability, strong syntax contrast, and a deep
;; black background suitable for low-light environments.

;;; Code:

(deftheme amaranth-dark
  "Amaranth Dark color theme for Emacs.")

;; +x -> lighter, -x -> darker.
(let (
      (amaranth-dark-fg        "#e4e4ef") ; default text, speedbar files, magit blame
      (amaranth-dark-fg+1      "#f4f4ff") ; variable names, lazy highlights
      (amaranth-dark-fg+2      "#f5f5f5") ; isearch match foreground
      (amaranth-dark-white     "#ffffff") ; mode-line text, tooltips
      (amaranth-dark-black     "#000000") ; search fail fg, trailing whitespace fg
      (amaranth-dark-bg-1      "#080808") ; org columns, company selection, deep UI
      (amaranth-dark-bg        "#000000") ; main editor background
      (amaranth-dark-bg+1      "#101010") ; current line, highlights, mode-line, popup
      (amaranth-dark-bg+2      "#302d2d") ; fringe, borders, magit headers, whitespac
      (amaranth-dark-bg+3      "#4f4949") ; region / selection background
      (amaranth-dark-bg+4      "#7b7171") ; tooltips, lazy highlight, line numbers
      (amaranth-dark-red-1     "#c73c3f") ; org TODO, minor errors
      (amaranth-dark-red       "#a02e2e") ; errors, failures, whitespace-trailing
      (amaranth-dark-red+1     "#c81a1a") ; compilation errors, diff removed
      (amaranth-dark-green     "#598b43") ; success, strings, diffs added
      (amaranth-dark-yellow    "#ffd966") ; keywords, warnings, cursor
      (amaranth-dark-brown     "#7b7171") ; comments, warnings, muted emphasis
      (amaranth-dark-quartz    "#959da3") ; constants, types, ignored files
      (amaranth-dark-niagara-2 "#303540") ; proof-locked regions
      (amaranth-dark-niagara-1 "#616775") ; lazy isearch background
      (amaranth-dark-niagara   "#97a1b5") ; functions, directories, links
      (amaranth-dark-wisteria  "#a64d79") ; visited links, which-func
      )
  (custom-theme-set-variables
   'amaranth-dark
   '(frame-background-mode (quote dark)))

  (custom-theme-set-faces
   'amaranth-dark

   ;; normal coloring and uncategorized
   `(border ((t ,(list :background amaranth-dark-bg-1
                       :foreground amaranth-dark-bg+2))))
   `(cursor ((t (:background ,amaranth-dark-yellow))))
   `(default ((t ,(list :foreground amaranth-dark-fg
                        :background amaranth-dark-bg))))
   `(fringe ((t ,(list :background nil
                       :foreground amaranth-dark-bg+2))))
   `(vertical-border ((t ,(list :foreground amaranth-dark-bg+2))))
   `(link ((t (:foreground ,amaranth-dark-niagara :underline t))))
   `(link-visited ((t (:foreground ,amaranth-dark-wisteria :underline t))))
   `(match ((t (:background ,amaranth-dark-bg+4))))
   `(shadow ((t (:foreground ,amaranth-dark-bg+4))))
   `(minibuffer-prompt ((t (:foreground ,amaranth-dark-niagara))))
   `(region ((t (:background ,amaranth-dark-bg+3 :foreground nil))))
   `(secondary-selection ((t ,(list :background amaranth-dark-bg+3
                                    :foreground nil))))
   `(trailing-whitespace ((t ,(list :foreground amaranth-dark-black
                                    :background amaranth-dark-red))))
   `(tooltip ((t ,(list :background amaranth-dark-bg+4
                        :foreground amaranth-dark-white))))

   ;; calendar
   `(holiday-face ((t (:foreground ,amaranth-dark-red))))

   ;; compilation IMPORTANT
   `(compilation-info ((t ,(list :foreground amaranth-dark-green
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground amaranth-dark-brown
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,amaranth-dark-red+1))))
   `(compilation-mode-line-fail ((t ,(list :foreground amaranth-dark-red
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground amaranth-dark-green
                                           :weight 'bold
                                           :inherit 'unspecified))))

   ;; completion
   `(completions-annotations ((t (:inherit 'shadow))))

   ;; custom
   `(custom-state ((t (:foreground ,amaranth-dark-green))))

   ;; diff
   `(diff-removed ((t ,(list :foreground amaranth-dark-red+1
                             :background nil))))
   `(diff-added ((t ,(list :foreground amaranth-dark-green
                           :background nil))))

   ;; dired
   `(dired-directory ((t (:foreground ,amaranth-dark-niagara :weight bold))))
   `(dired-ignored ((t ,(list :foreground amaranth-dark-quartz
                              :inherit 'unspecified))))

   ;; ebrowse
   `(ebrowse-root-class ((t (:foreground ,amaranth-dark-niagara :weight bold))))
   `(ebrowse-progress ((t (:background ,amaranth-dark-niagara))))

   ;; font lock
   `(font-lock-builtin-face ((t (:foreground ,amaranth-dark-yellow))))
   `(font-lock-comment-face ((t (:foreground ,amaranth-dark-brown))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,amaranth-dark-brown))))
   `(font-lock-constant-face ((t (:foreground ,amaranth-dark-quartz))))
   `(font-lock-doc-face ((t (:foreground ,amaranth-dark-green))))
   `(font-lock-doc-string-face ((t (:foreground ,amaranth-dark-green))))
   `(font-lock-function-name-face ((t (:foreground ,amaranth-dark-niagara))))
   `(font-lock-keyword-face ((t (:foreground ,amaranth-dark-yellow :bold t))))
   `(font-lock-preprocessor-face ((t (:foreground ,amaranth-dark-quartz))))
   `(font-lock-reference-face ((t (:foreground ,amaranth-dark-quartz))))
   `(font-lock-string-face ((t (:foreground ,amaranth-dark-green))))
   `(font-lock-type-face ((t (:foreground ,amaranth-dark-quartz))))
   `(font-lock-variable-name-face ((t (:foreground ,amaranth-dark-fg+1))))
   `(font-lock-warning-face ((t (:foreground ,amaranth-dark-red))))

   ;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,amaranth-dark-red)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,amaranth-dark-red :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,amaranth-dark-yellow)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,amaranth-dark-yellow :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,amaranth-dark-green)
                   :foreground unspecified
                   :background unspecified
                   :inherit unspecified))
      (t (:foreground ,amaranth-dark-green :weight bold :underline t))))

   ;; flyspell
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,amaranth-dark-red) :inherit unspecified))
      (t (:foreground ,amaranth-dark-red :weight bold :underline t))))
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,amaranth-dark-yellow) :inherit unspecified))
      (t (:foreground ,amaranth-dark-yellow :weight bold :underline t))))

   ;; ido
   `(ido-first-match ((t (:foreground ,amaranth-dark-yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,amaranth-dark-brown :weight bold))))
   `(ido-subdir ((t (:foreground ,amaranth-dark-niagara :weight bold))))

   ;; info
   `(info-xref ((t (:foreground ,amaranth-dark-niagara))))
   `(info-visited ((t (:foreground ,amaranth-dark-wisteria))))

   ;; line highlighting
   `(highlight ((t (:background ,amaranth-dark-bg+1 :foreground nil))))
   `(highlight-current-line-face ((t ,(list :background amaranth-dark-bg+1
                                            :foreground nil))))

   ;; line numbers
   `(line-number ((t (:inherit default :foreground ,amaranth-dark-bg+4))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,amaranth-dark-yellow))))

   ;; magit
   `(magit-branch ((t (:foreground ,amaranth-dark-niagara))))
   `(magit-diff-hunk-header ((t (:background ,amaranth-dark-bg+2))))
   `(magit-diff-file-header ((t (:background ,amaranth-dark-bg+4))))
   `(magit-log-sha1 ((t (:foreground ,amaranth-dark-red+1))))
   `(magit-log-author ((t (:foreground ,amaranth-dark-brown))))
   `(magit-log-head-label-remote ((t ,(list :foreground amaranth-dark-green
                                            :background amaranth-dark-bg+1))))
   `(magit-log-head-label-local ((t ,(list :foreground amaranth-dark-niagara
                                           :background amaranth-dark-bg+1))))
   `(magit-log-head-label-tags ((t ,(list :foreground amaranth-dark-yellow
                                          :background amaranth-dark-bg+1))))
   `(magit-log-head-label-head ((t ,(list :foreground amaranth-dark-fg
                                          :background amaranth-dark-bg+1))))
   `(magit-item-highlight ((t (:background ,amaranth-dark-bg+1))))
   `(magit-tag ((t ,(list :foreground amaranth-dark-yellow
                          :background amaranth-dark-bg))))
   `(magit-blame-heading ((t ,(list :background amaranth-dark-bg+1
                                    :foreground amaranth-dark-fg))))

   ;; message
   `(message-header-name ((t (:foreground ,amaranth-dark-green))))

   ;; mode line
   `(mode-line ((t ,(list :background amaranth-dark-bg+1
                          :foreground amaranth-dark-white))))
   `(mode-line-buffer-id ((t ,(list :background amaranth-dark-bg+1
                                    :foreground amaranth-dark-white))))
   `(mode-line-inactive ((t ,(list :background amaranth-dark-bg+1
                                   :foreground amaranth-dark-quartz))))

   ;; neo dir
   `(neo-dir-link-face ((t (:foreground ,amaranth-dark-niagara))))

   ;; org
   `(org-agenda-structure ((t (:foreground ,amaranth-dark-niagara))))
   `(org-column ((t (:background ,amaranth-dark-bg-1))))
   `(org-column-title ((t (:background ,amaranth-dark-bg-1 :underline t :weight bold))))
   `(org-done ((t (:foreground ,amaranth-dark-green))))
   `(org-todo ((t (:foreground ,amaranth-dark-red-1))))
   `(org-upcoming-deadline ((t (:foreground ,amaranth-dark-yellow))))

   ;; search
   `(isearch ((t ,(list :foreground amaranth-dark-black
                        :background amaranth-dark-fg+2))))
   `(isearch-fail ((t ,(list :foreground amaranth-dark-black
                             :background amaranth-dark-red))))
   `(isearch-lazy-highlight-face ((t ,(list
                                       :foreground amaranth-dark-fg+1
                                       :background amaranth-dark-niagara-1))))

   ;; sh
   `(sh-quoted-exec ((t (:foreground ,amaranth-dark-red+1))))

   ;; show paren
   `(show-paren-match-face ((t (:background ,amaranth-dark-bg+4))))
   `(show-paren-mismatch-face ((t (:background ,amaranth-dark-red-1))))

   ;; speedbar
   `(speedbar-directory-face ((t ,(list :foreground amaranth-dark-niagara
                                        :weight 'bold))))
   `(speedbar-file-face ((t (:foreground ,amaranth-dark-fg))))
   `(speedbar-highlight-face ((t (:background ,amaranth-dark-bg+1))))
   `(speedbar-selected-face ((t (:foreground ,amaranth-dark-red))))
   `(speedbar-tag-face ((t (:foreground ,amaranth-dark-yellow))))

   ;; which function
   `(which-func ((t (:foreground ,amaranth-dark-wisteria))))

   ;; whitespace
   `(whitespace-space ((t ,(list :background amaranth-dark-bg
                                 :foreground amaranth-dark-bg+1))))
   `(whitespace-tab ((t ,(list :background amaranth-dark-bg
                               :foreground amaranth-dark-bg+1))))
   `(whitespace-hspace ((t ,(list :background amaranth-dark-bg
                                  :foreground amaranth-dark-bg+2))))
   `(whitespace-line ((t ,(list :background amaranth-dark-bg+2
                                :foreground amaranth-dark-red+1))))
   `(whitespace-newline ((t ,(list :background amaranth-dark-bg
                                   :foreground amaranth-dark-bg+2))))
   `(whitespace-trailing ((t ,(list :background amaranth-dark-red
                                    :foreground amaranth-dark-red))))
   `(whitespace-empty ((t ,(list :background amaranth-dark-yellow
                                 :foreground amaranth-dark-yellow))))
   `(whitespace-indentation ((t ,(list :background amaranth-dark-yellow
                                       :foreground amaranth-dark-red))))
   `(whitespace-space-after-tab ((t ,(list :background amaranth-dark-yellow
                                           :foreground amaranth-dark-yellow))))
   `(whitespace-space-before-tab ((t ,(list :background amaranth-dark-brown
                                            :foreground amaranth-dark-brown))))
   
   ;; tab-bar
   `(tab-bar ((t (:background ,amaranth-dark-bg+1 :foreground ,amaranth-dark-bg+4))))
   `(tab-bar-tab ((t (:background nil :foreground ,amaranth-dark-yellow :weight bold))))
   `(tab-bar-tab-inactive ((t (:background nil))))

   ;; vterm / ansi-term
   `(term-color-black ((t (:foreground ,amaranth-dark-bg+3 :background ,amaranth-dark-bg+4))))
   `(term-color-red ((t (:foreground ,amaranth-dark-red-1 :background ,amaranth-dark-red-1))))
   `(term-color-green ((t (:foreground ,amaranth-dark-green :background ,amaranth-dark-green))))
   `(term-color-blue ((t (:foreground ,amaranth-dark-niagara :background ,amaranth-dark-niagara))))
   `(term-color-yellow ((t (:foreground ,amaranth-dark-yellow :background ,amaranth-dark-yellow))))
   `(term-color-magenta ((t (:foreground ,amaranth-dark-wisteria :background ,amaranth-dark-wisteria))))
   `(term-color-cyan ((t (:foreground ,amaranth-dark-quartz :background ,amaranth-dark-quartz))))
   `(term-color-white ((t (:foreground ,amaranth-dark-fg :background ,amaranth-dark-white))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,amaranth-dark-fg :background ,amaranth-dark-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,amaranth-dark-brown :background ,amaranth-dark-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,amaranth-dark-brown :background ,amaranth-dark-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,amaranth-dark-fg :background ,amaranth-dark-bg-1))))
   `(company-tooltip-mouse ((t (:background ,amaranth-dark-bg-1))))
   `(company-tooltip-common ((t (:foreground ,amaranth-dark-green))))
   `(company-tooltip-common-selection ((t (:foreground ,amaranth-dark-green))))
   `(company-scrollbar-fg ((t (:background ,amaranth-dark-bg-1))))
   `(company-scrollbar-bg ((t (:background ,amaranth-dark-bg+2))))
   `(company-preview ((t (:background ,amaranth-dark-green))))
   `(company-preview-common ((t (:foreground ,amaranth-dark-green :background ,amaranth-dark-bg-1))))

   ;; proof general
   `(proof-locked-face ((t (:background ,amaranth-dark-niagara-2))))

   ;; orderless
   `(orderless-match-face-0 ((t (:foreground ,amaranth-dark-yellow))))
   `(orderless-match-face-1 ((t (:foreground ,amaranth-dark-green))))
   `(orderless-match-face-2 ((t (:foreground ,amaranth-dark-brown))))
   `(orderless-match-face-3 ((t (:foreground ,amaranth-dark-quartz))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'amaranth-dark)
;;; amaranth-dark-theme.el ends here.
