;;; lop-theme.el --- Atom Gloom theme inspired for Emacs.

;; Copyright (C) 2017 , Morteza Nourelahi Alamdari

;; Author: Morteza Nourelahi Alamdari
;; URL: https://github.com/mortezaipo/lop-theme
;; Version: 1.0
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;;
;; A port of Atom theme Gloom for emacs 24, built on top of the new
;; built-in theme support in Emacs 24. This theme has been created
;; by emacs-theme-generator.
;;
;;; Credits:
;;
;; Robin Grass created the original theme (Gloom) for Atom
;; - https://github.com/hejrobin/gloom
;;
;; Kelvin Smith created the Monokai theme for Emacs
;; - http://github.com/oneKelvinSmith/monokai-emacs
;;
;; Martin Haesler created emacs-theme-generator for Emacs
;; - https://github.com/mswift42/theme-creator
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The lop theme required Emacs 24 or later!"))

(deftheme lop)

(let* (
       ;; For terminal which support 256 colors
       (lop-class '((class color) (min-colors 257)))

       (fg1     "#E2E0EA")
       (fg2     "#ceccd5")
       (fg3     "#bab8c0")
       (fg4     "#a7a5ac")
       (bg1     "#2D2D46")
       (bg2     "#3c3b53")
       (bg3     "#4b4a61")
       (bg4     "#5b596f")
       (key2    "#83a7fc")
       (key3    "#638adc")
       (builtin "#709DFB")
       (keyword "#6F9DFB")
       (const   "#AAFF9B")
       (comment "#6764AC")
       (func    "#00E4A8")
       (str     "#45E5CB")
       (type    "#4AD9EE")
       (var     "#FF6099")
       (warning "#FF9800")
 
       ;; For terminal which don't support 256 colors
       (lop-256-class '((class color) (min-colors 89)))

       (fg1-256     "#E2E0EA")
       (fg2-256     "#ceccd5")
       (fg3-256     "#bab8c0")
       (fg4-256     "#a7a5ac")
       (bg1-256     "#383838")
       (bg2-256     "#333333")
       (bg3-256     "#4b4a61")
       (bg4-256     "#5b596f")
       (key2-256    "#83a7fc")
       (key3-256    "#638adc")
       (builtin-256 "#709DFB")
       (keyword-256 "#6F9DFB")
       (const-256   "#AAFF9B")
       (comment-256 "#6764AC")
       (func-256    "#00E4A8")
       (str-256     "#45E5CB")
       (type-256    "#4AD9EE")
       (var-256     "#FF6099")
       (warning-256 "#FF9800"))

  (custom-theme-set-faces
  'lop

  `(default
    ((,lop-class     (:background ,bg1 :foreground ,fg1))
     (,lop-256-class (:background ,bg1-256 :foreground ,fg1-256))))
  
  `(font-lock-builtin-face
    ((,lop-class     (:foreground ,builtin))
     (,lop-256-class (:foreground ,builtin-256))))

  `(font-lock-comment-face
    ((,lop-class     (:foreground ,comment))
     (,lop-256-class (:foreground ,comment-256))))

  `(font-lock-negation-char-face
    ((,lop-class     (:foreground ,const))
     (,lop-256-class (:foreground ,const-256))))

  `(font-lock-reference-face
    ((,lop-class     (:foreground ,const))
     (,lop-256-class (:foreground ,const-256))))
	   
  `(font-lock-constant-face
    ((,lop-class     (:foreground ,const))
     (,lop-256-class (:foreground ,const-256))))

  `(font-lock-doc-face
    ((,lop-class     (:foreground ,comment))
     (,lop-256-class (:foreground ,comment-256))))

  `(font-lock-function-name-face
    ((,lop-class     (:foreground ,func :bold t))
     (,lop-256-class (:foreground ,func-256 :bold t))))

  `(font-lock-keyword-face
    ((,lop-class     (:bold ,lop-class :foreground ,keyword))
     (,lop-256-class (:bold ,lop-256-class :foreground ,keyword-256))))

  `(font-lock-string-face
    ((,lop-class     (:foreground ,str))
     (,lop-256-class (:foreground ,str-256))))

  `(font-lock-type-face
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(font-lock-variable-name-face
    ((,lop-class     (:foreground ,var))
     (,lop-256-class (:foreground ,var-256))))

  `(font-lock-warning-face
    ((,lop-class     (:foreground ,warning :background ,bg2))
     (,lop-256-class (:foreground ,warning-256 :background ,bg2-256))))

  `(region
    ((,lop-class     (:background ,fg1 :foreground ,bg1))
     (,lop-256-class (:background ,fg1-256 :foreground ,bg1-256))))

  `(highlight
    ((,lop-class     (:foreground ,fg3 :background ,bg3))
     (,lop-256-class (:foreground ,fg3-256 :background ,bg3-256))))

  `(hl-line
    ((,lop-class     (:background ,bg2))
     (,lop-256-class (:background ,bg2-256))))

  `(fringe
    ((,lop-class     (:background ,bg2 :foreground ,fg4))
     (,lop-256-class (:background ,bg2-256 :foreground ,fg4-256))))

  `(linum
    ((,lop-class     (:background, bg2 :forground, fg1))
     (,lop-256-class (:background, bg2-256 :forground, fg1-256))))

  `(cursor
    ((,lop-class     (:background ,bg3))
     (,lop-256-class (:background ,bg3-256))))

  `(show-paren-match-face
    ((,lop-class     (:background ,warning))
     (,lop-256-class (:background ,warning-256))))

  `(isearch
    ((,lop-class     (:bold t :foreground ,warning :background ,bg3))
     (,lop-256-class (:bold t :foreground ,warning-256 :background ,bg3-256))))

  `(mode-line
    ((,lop-class     (:box (:line-width 1 :color nil) :bold t :foreground ,fg4 :background ,bg2))
     (,lop-256-class (:box (:line-width 1 :color nil) :bold t :foreground ,fg4-256 :background ,bg2-256))))

  `(mode-line-inactive
    ((,lop-class     (:box (:line-width 1 :color nil :style pressed-button) :foreground ,key3 :background ,bg1 :weight normal))
     (,lop-256-class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,key3-256 :background ,bg1-256 :weight normal))))

  `(mode-line-buffer-id
    ((,lop-class     (:bold t :foreground ,func :background nil))
     (,lop-256-class (:bold t :foreground ,func-256 :background nil))))

  `(mode-line-highlight
    ((,lop-class     (:foreground ,keyword :box nil :weight bold))
     (,lop-256-class (:foreground ,keyword-256 :box nil :weight bold))))

  `(mode-line-emphasis
    ((,lop-class (:foreground ,fg1))
     (,lop-256-class (:foreground ,fg1-256))))

  `(vertical-border
    ((,lop-class     (:foreground ,fg3))
     (,lop-256-class (:foreground ,fg3-256))))
	  
  `(minibuffer-prompt
    ((,lop-class     (:bold t :foreground ,keyword))
     (,lop-256-class (:bold t :foreground ,keyword-256))))

  `(default-italic
    ((,lop-class     (:italic t))
     (,lop-256-class (:italic t))))

  `(link
    ((,lop-class     (:foreground ,const :underline t))
     (,lop-256-class (:foreground ,const-256 :underline t))))

  `(org-code
    ((,lop-class     (:foreground ,fg2))
     (,lop-256-class (:foreground ,fg2-256))))

  `(org-hide
    ((,lop-class (:foreground ,fg4))
     (,lop-256-class (:foreground ,fg4-256))))

  `(org-level-1
    ((,lop-class     (:bold t :foreground ,fg2 :height 1.1))
     (,lop-256-class (:bold t :foreground ,fg2-256 :height 1.1))))

  `(org-level-2
    ((,lop-class     (:bold nil :foreground ,fg3))
     (,lop-256-class (:bold nil :foreground ,fg3-256))))

  `(org-level-3
    ((,lop-class     (:bold t :foreground ,fg4))
     (,lop-256-class (:bold t :foreground ,fg4-256))))

  `(org-level-4
    ((,lop-class (:bold nil :foreground ,bg4))
     (,lop-256-class (:bold nil :foreground ,bg4-256))))

  `(org-date
    ((,lop-class     (:underline t :foreground ,var))
     (,lop-256-class (:underline t :foreground ,var-256))))

  `(org-footnote
    ((,lop-class     (:underline t :foreground ,fg4))
     (,lop-256-class (:underline t :foreground ,fg4-256))))

  `(org-link
    ((,lop-class     (:underline t :foreground ,type))
     (,lop-256-class (:underline t :foreground ,type-256))))

  `(org-special-keyword
    ((,lop-class     (:foreground ,func))
     (,lop-256-class (:foreground ,func-256))))

  `(org-block
    ((,lop-class     (:foreground ,fg3))
     (,lop-256-class (:foreground ,fg3-256))))

  `(org-quote
    ((,lop-class     (:inherit org-block :slant italic))
     (,lop-256-class (:inherit org-block :slant italic))))

  `(org-verse
    ((,lop-class     (:inherit org-block :slant italic))
     (,lop-256-class (:inherit org-block :slant italic))))

  `(org-todo
    ((,lop-class     (:box (:line-width 1 :color ,fg3) :foreground ,keyword :bold t))
     (,lop-256-class (:box (:line-width 1 :color ,fg3-256) :foreground ,keyword-256 :bold t))))

  `(org-done
    ((,lop-class     (:box (:line-width 1 :color ,bg3) :bold t :foreground ,bg4))
     (,lop-256-class (:box (:line-width 1 :color ,bg3-256) :bold t :foreground ,bg4-256))))

  `(org-warning
    ((,lop-class     (:underline t :foreground ,warning))
     (,lop-256-class (:underline t :foreground ,warning-256))))

  `(org-agenda-structure
    ((,lop-class     (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))
     (,lop-256-class (:weight bold :foreground ,fg3-256 :box (:color ,fg4-256) :background ,bg3-256))))

  `(org-agenda-date
    ((,lop-class     (:foreground ,var :height 1.1))
     (,lop-256-class (:foreground ,var-256 :height 1.1))))

  `(org-agenda-date-weekend
    ((,lop-class     (:weight normal :foreground ,fg4))
     (,lop-256-class (:weight normal :foreground ,fg4-256))))

  `(org-agenda-date-today
    ((,lop-class     (:weight bold :foreground ,keyword :height 1.4))
     (,lop-256-class (:weight bold :foreground ,keyword-256 :height 1.4))))

  `(org-agenda-done
    ((,lop-class     (:foreground ,bg4))
     (,lop-256-class (:foreground ,bg4-256))))

	`(org-scheduled
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(org-scheduled-today
    ((,lop-class     (:foreground ,func :weight bold :height 1.2))
     (,lop-256-class (:foreground ,func-256 :weight bold :height 1.2))))

  `(org-ellipsis
    ((,lop-class     (:foreground ,builtin))
     (,lop-256-class (:foreground ,builtin-256))))

  `(org-verbatim
    ((,lop-class     (:foreground ,fg4))
     (,lop-256-class (:foreground ,fg4-256))))

  `(org-document-info-keyword
    ((,lop-class     (:foreground ,func))
     (,lop-256-class (:foreground ,func-256))))

  `(font-latex-bold-face
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(font-latex-italic-face
    ((,lop-class     (:foreground ,key3 :italic t))
     (,lop-256-class (:foreground ,key3-256 :italic t))))

  `(font-latex-string-face
    ((,lop-class     (:foreground ,str))
     (,lop-256-class (:foreground ,str-256))))

  `(font-latex-match-reference-keywords
    ((,lop-class     (:foreground ,const))
     (,lop-256-class (:foreground ,const-256))))

  `(font-latex-match-variable-keywords
    ((,lop-class     (:foreground ,var))
     (,lop-256-class (:foreground ,var-256))))

  `(ido-only-match
    ((,lop-class     (:foreground ,warning))
     (,lop-256-class (:foreground ,warning-256))))

  `(org-sexp-date
    ((,lop-class     (:foreground ,fg4))
     (,lop-256-class (:foreground ,fg4-256))))

  `(ido-first-match
    ((,lop-class     (:foreground ,keyword))
     (,lop-256-class (:foreground ,keyword-256 :bold t))))
   
  `(gnus-header-content
    ((,lop-class     (:foreground ,keyword))
	   (,lop-256-class (:foreground ,keyword-256))))

  `(gnus-header-from
    ((,lop-class     (:foreground ,var))
     (,lop-256-class (:foreground ,var-256))))

  `(gnus-header-name
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(gnus-header-subject
    ((,lop-class     (:foreground ,func :bold t))
     (,lop-256-class (:foreground ,func-256 :bold t))))

  `(mu4e-view-url-number-face
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(mu4e-cited-1-face
    ((,lop-class     (:foreground ,fg2))
     (,lop-256-class (:foreground ,fg2-256))))

  `(mu4e-cited-7-face
    ((,lop-class     (:foreground ,fg3))
     (,lop-256-class (:foreground ,fg3-256))))

  `(mu4e-header-marks-face
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(ffap
    ((,lop-class     (:foreground ,fg4))
     (,lop-256-class (:foreground ,fg4-256))))

  `(js2-private-function-call
    ((,lop-class     (:foreground ,const))
     (,lop-256-class (:foreground ,const-256))))

  `(js2-jsdoc-html-tag-delimiter
    ((,lop-class     (:foreground ,str))
     (,lop-256-class (:foreground ,str-256))))

  `(js2-jsdoc-html-tag-name
    ((,lop-class     (:foreground ,key2))
     (,lop-256-class (:foreground ,key2-256))))

  `(js2-external-variable
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(js2-function-param
    ((,lop-class     (:foreground ,const))
     (,lop-256-class (:foreground ,const-256))))

  `(js2-jsdoc-value
    ((,lop-class     (:foreground ,str))
     (,lop-256-class (:foreground ,str-256))))

  `(js2-private-member
    ((,lop-class     (:foreground ,fg3))
     (,lop-256-class (:foreground ,fg3-256))))

  `(js3-warning-face
    ((,lop-class     (:underline ,keyword))
     (,lop-256-class (:underline ,keyword-256))))

  `(js3-error-face
    ((,lop-class     (:underline ,warning))
     (,lop-256-class (:underline ,warning-256))))

  `(js3-external-variable-face
    ((,lop-class     (:foreground ,var))
     (,lop-256-class (:foreground ,var-256))))

  `(js3-function-param-face
    ((,lop-class     (:foreground ,key3))
     (,lop-256-class (:foreground ,key3-256))))

  `(js3-jsdoc-tag-face
    ((,lop-class     (:foreground ,keyword))
     (,lop-256-class (:foreground ,keyword-256))))

  `(js3-instance-member-face
    ((,lop-class     (:foreground ,const))
     (,lop-256-class (:foreground ,const-256))))

  `(warning
    ((,lop-class     (:foreground ,warning))
     (,lop-256-class (:foreground ,warning-256))))

  `(ac-completion-face
    ((,lop-class     (:underline t :foreground ,keyword))
     (,lop-256-class (:underline t :foreground ,keyword-256))))

  `(info-quoted-name
    ((,lop-class     (:foreground ,builtin))
     (,lop-256-class (:foreground ,builtin-256))))

  `(info-string
    ((,lop-class     (:foreground ,str))
     (,lop-256-class (:foreground ,str-256))))

  `(icompletep-determined
    ((,lop-class     (:foreground ,builtin))
     (,lop-256-class (:foreground ,builtin-256))))

  `(undo-tree-visualizer-current-face
    ((,lop-class     (:foreground ,builtin))
     (,lop-256-class (:foreground ,builtin-256))))

  `(undo-tree-visualizer-default-face
    ((,lop-class     (:foreground ,fg2))
     (,lop-256-class (:foreground ,fg2-256))))

  `(undo-tree-visualizer-unmodified-face
    ((,lop-class     (:foreground ,var))
     (,lop-256-class (:foreground ,var-256))))

  `(undo-tree-visualizer-register-face
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(slime-repl-inputed-output-face
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(trailing-whitespace
    ((,lop-class     (:foreground nil :background ,warning))
     (,lop-256-class (:foreground nil :background ,warning-256))))

  `(rainbow-delimiters-depth-1-face
    ((,lop-class     (:foreground ,fg1))
     (,lop-256-class (:foreground ,fg1-256))))

  `(rainbow-delimiters-depth-2-face
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(rainbow-delimiters-depth-3-face
    ((,lop-class     (:foreground ,var))
     (,lop-256-class (:foreground ,var-256))))

  `(rainbow-delimiters-depth-4-face
    ((,lop-class     (:foreground ,const))
     (,lop-256-class (:foreground ,const-256))))

  `(rainbow-delimiters-depth-5-face
    ((,lop-class     (:foreground ,keyword))
     (,lop-256-class (:foreground ,keyword-256))))

  `(rainbow-delimiters-depth-6-face
    ((,lop-class     (:foreground ,fg1))
     (,lop-256-class (:foreground ,fg1-256))))

  `(rainbow-delimiters-depth-7-face
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(rainbow-delimiters-depth-8-face
    ((,lop-class     (:foreground ,var))
     (,lop-256-class (:foreground ,var-256))))

  `(magit-item-highlight
    ((,lop-class     (:background ,bg3))
     (,lop-256-class (:background ,bg3-256))))

  `(magit-section-heading
    ((,lop-class     (:foreground ,keyword :weight bold))
     (,lop-256-class (:foreground ,keyword-256 :weight bold))))

  `(magit-hunk-heading
    ((,lop-class     (:background ,bg3))
     (,lop-256-class (:background ,bg3-256))))

  `(magit-section-highlight
    ((,lop-class     (:background ,bg2))
     (,lop-256-class (:background ,bg2-256))))

  `(magit-hunk-heading-highlight
    ((,lop-class (:background ,bg3))
     (,lop-256-class (:background ,bg3-256))))

  `(magit-diff-context-highlight
    ((,lop-class     (:background ,bg3 :foreground ,fg3))
     (,lop-256-class (:background ,bg3-256 :foreground ,fg3-256))))

  `(magit-diffstat-added
    ((,lop-class     (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(magit-diffstat-removed
    ((,lop-class     (:foreground ,var))
     (,lop-256-class (:foreground ,var-256))))

  `(magit-process-ok
    ((,lop-class     (:foreground ,func :weight bold))
     (,lop-256-class (:foreground ,func-256 :weight bold))))

  `(magit-process-ng
    ((,lop-class     (:foreground ,warning :weight bold))
     (,lop-256-class (:foreground ,warning-256 :weight bold))))

  `(magit-branch
    ((,lop-class (:foreground ,const :weight bold))
     (,lop-256-class (:foreground ,const-256 :weight bold))))

  `(magit-log-author
    ((,lop-class     (:foreground ,fg3))
     (,lop-256-class (:foreground ,fg3-256))))

  `(magit-hash
    ((,lop-class     (:foreground ,fg2))
     (,lop-256-class (:foreground ,fg2-256))))

  `(magit-diff-file-header
    ((,lop-class (:foreground ,fg2 :background ,bg3))
     (,lop-256-class (:foreground ,fg2-256 :background ,bg3-256))))

  `(lazy-highlight
    ((,lop-class     (:foreground ,fg2 :background ,bg3))
     (,lop-256-class (:foreground ,fg2-256 :background ,bg3-256))))

  `(term
    ((,lop-class     (:foreground ,fg1 :background ,bg1))
     (,lop-256-class (:foreground ,fg1-256 :background ,bg1-256))))

  `(term-color-black
    ((,lop-class     (:foreground ,bg3 :background ,bg3))
     (,lop-256-class (:foreground ,bg3-256 :background ,bg3-256))))

  `(term-color-blue
    ((,lop-class     (:foreground ,func :background ,func))
     (,lop-256-class (:foreground ,func-256 :background ,func-256))))

  `(term-color-red
    ((,lop-class     (:foreground ,keyword :background ,bg3))
     (,lop-256-class (:foreground ,keyword-256 :background ,bg3-256))))

  `(term-color-green
    ((,lop-class     (:foreground ,type :background ,bg3))
     (,lop-256-class (:foreground ,type-256 :background ,bg3-256))))

  `(term-color-yellow
    ((,lop-class     (:foreground ,var :background ,var))
     (,lop-256-class (:foreground ,var-256 :background ,var-256))))

  `(term-color-magenta
    ((,lop-class     (:foreground ,builtin :background ,builtin))
     (,lop-256-class (:foreground ,builtin-256 :background ,builtin-256))))

  `(term-color-cyan
    ((,lop-class     (:foreground ,str :background ,str))
     (,lop-256-class (:foreground ,str-256 :background ,str-256))))

  `(term-color-white
    ((,lop-class     (:foreground ,fg2 :background ,fg2))
     (,lop-256-class (:foreground ,fg2-256 :background ,fg2-256))))

  `(rainbow-delimiters-unmatched-face
    ((,lop-class     (:foreground ,warning))
     (,lop-256-class (:foreground ,warning-256))))

  `(helm-header
    ((,lop-class     (:foreground ,fg2 :background ,bg1 :underline nil :box nil))
     (,lop-256-class (:foreground ,fg2-256 :background ,bg1-256 :underline nil :box nil))))

  `(helm-source-header
    ((,lop-class     (:foreground ,keyword :background ,bg1 :underline nil :weight bold))
     (,lop-256-class (:foreground ,keyword-256 :background ,bg1-256 :underline nil :weight bold))))

  `(helm-selection
    ((,lop-class     (:background ,bg2 :underline nil))
     (,lop-256-class (:background ,bg2-256 :underline nil))))

  `(helm-selection-line
    ((,lop-class     (:background ,bg2))
     (,lop-256-class (:background ,bg2-256))))

  `(helm-visible-mark
    ((,lop-class     (:foreground ,bg1 :background ,bg3))
     (,lop-256-class (:foreground ,bg1-256 :background ,bg3-256))))

  `(helm-candidate-number
    ((,lop-class     (:foreground ,bg1 :background ,fg1))
     (,lop-256-class (:foreground ,bg1-256 :background ,fg1-256))))

  `(helm-separator
    ((,lop-class     (:foreground ,type :background ,bg1))
     (,lop-256-class (:foreground ,type-256 :background ,bg1-256))))

  `(helm-time-zone-current
    ((,lop-class     (:foreground ,builtin :background ,bg1))
     (,lop-256-class (:foreground ,builtin-256 :background ,bg1-256))))

  `(helm-time-zone-home
    ((,lop-class     (:foreground ,type :background ,bg1))
     (,lop-256-class (:foreground ,type-256 :background ,bg1-256))))

  `(helm-buffer-not-saved
    ((,lop-class     (:foreground ,type :background ,bg1))
     (,lop-256-class (:foreground ,type-256 :background ,bg1-256))))

  `(helm-buffer-process
    ((,lop-class     (:foreground ,builtin :background ,bg1))
     (,lop-256-class (:foreground ,builtin-256 :background ,bg1-256))))

  `(helm-buffer-saved-out
    ((,lop-class     (:foreground ,fg1 :background ,bg1))
     (,lop-256-class (:foreground ,fg1-256 :background ,bg1-256))))

  `(helm-buffer-size
    ((,lop-class     (:foreground ,fg1 :background ,bg1))
     (,lop-256-class (:foreground ,fg1-256 :background ,bg1-256))))

  `(helm-ff-directory
    ((,lop-class     (:foreground ,func :background ,bg1 :weight bold))
     (,lop-256-class (:foreground ,func-256 :background ,bg1-256 :weight bold))))

  `(helm-ff-file
    ((,lop-class     (:foreground ,fg1 :background ,bg1 :weight normal))
     (,lop-256-class (:foreground ,fg1-256 :background ,bg1-256 :weight normal))))

  `(helm-ff-executable
    ((,lop-class     (:foreground ,key2 :background ,bg1 :weight normal))
     (,lop-256-class (:foreground ,key2-256 :background ,bg1-256 :weight normal))))

  `(helm-ff-invalid-symlink
    ((,lop-class     (:foreground ,key3 :background ,bg1 :weight bold))
     (,lop-256-class (:foreground ,key3-256 :background ,bg1-256 :weight bold))))

  `(helm-ff-symlink
    ((,lop-class     (:foreground ,keyword :background ,bg1 :weight bold))
     (,lop-256-class (:foreground ,keyword-256 :background ,bg1-256 :weight bold))))

  `(helm-ff-prefix
    ((,lop-class     (:foreground ,bg1 :background ,keyword :weight normal))
     (,lop-256-class (:foreground ,bg1-256 :background ,keyword-256 :weight normal))))

  `(helm-grep-cmd-line
    ((,lop-class     (:foreground ,fg1 :background ,bg1))
     (,lop-256-class (:foreground ,fg1-256 :background ,bg1-256))))

  `(helm-grep-file
    ((,lop-class     (:foreground ,fg1 :background ,bg1))
     (,lop-256-class (:foreground ,fg1-256 :background ,bg1-256))))

  `(helm-grep-finish
    ((,lop-class     (:foreground ,fg2 :background ,bg1))
     (,lop-256-class (:foreground ,fg2-256 :background ,bg1-256))))

  `(helm-grep-lineno
    ((,lop-class     (:foreground ,fg1 :background ,bg1))
     (,lop-256-class (:foreground ,fg1-256 :background ,bg1-256))))

  `(helm-grep-match
    ((,lop-class     (:foreground nil :background nil :inherit helm-match))
     (,lop-256-class (:foreground nil :background nil :inherit helm-match))))

  `(helm-grep-running
    ((,lop-class     (:foreground ,func :background ,bg1))
     (,lop-256-class (:foreground ,func-256 :background ,bg1-256))))

  `(helm-moccur-buffer
    ((,lop-class     (:foreground ,func :background ,bg1))
     (,lop-256-class (:foreground ,func-256 :background ,bg1-256))))

  `(helm-source-go-package-godoc-description
    ((,lop-class     (:foreground ,str))
     (,lop-256-class (:foreground ,str-256))))

  `(helm-bookmark-w3m
    ((,lop-class    (:foreground ,type))
     (,lop-256-class (:foreground ,type-256))))

  `(company-echo-common
    ((,lop-class    (:foreground ,bg1 :background ,fg1))
     (,lop-256-class (:foreground ,bg1-256 :background ,fg1-256))))

  `(company-preview
    ((,lop-class     (:background ,bg1 :foreground ,key2))
     (,lop-256-class (:background ,bg1-256 :foreground ,key2-256))))

  `(company-preview-common
    ((,lop-class     (:foreground ,bg2 :foreground ,fg3))
     (,lop-256-class (:foreground ,bg2-256 :foreground ,fg3-256))))

  `(company-preview-search
    ((,lop-class     (:foreground ,type :background ,bg1))
     (,lop-256-class (:foreground ,type-256 :background ,bg1-256))))

  `(company-scrollbar-bg
    ((,lop-class     (:background ,bg3))
     (,lop-256-class (:background ,bg3-256))))

  `(company-scrollbar-fg
    ((,lop-class     (:foreground ,keyword))
     (,lop-256-class (:foreground ,keyword-256))))

  `(company-tooltip
    ((,lop-class     (:foreground ,fg2 :background ,bg1 :bold t))
     (,lop-256-class (:foreground ,fg2-256 :background ,bg1-256 :bold t))))

  `(company-tooltop-annotation
    ((,lop-class     (:foreground ,const))
     (,lop-256-class (:foreground ,const-256))))

  `(company-tooltip-common
    ((,lop-class     (:foreground ,fg3))
     (,lop-256-class (:foreground ,fg3-256))))

  `(company-tooltip-common-selection
    ((,lop-class     (:foreground ,str))
     (,lop-256-class (:foreground ,str-256))))

  `(company-tooltip-mouse
    ((,lop-class     (:inherit highlight))
     (,lop-256-class (:inherit highlight))))

  `(company-tooltip-selection
    ((,lop-class     (:background ,bg3 :foreground ,fg3))
     (,lop-256-class (:background ,bg3-256 :foreground ,fg3-256))))

  `(company-template-field
    ((,lop-class     (:inherit region))
     (,lop-256-class (:inherit region))))

  `(web-mode-builtin-face
    ((,lop-class     (:inherit ,font-lock-builtin-face))
     (,lop-256-class (:inherit ,font-lock-builtin-face))))

  `(web-mode-comment-face
    ((,lop-class     (:inherit ,font-lock-comment-face))
     (,lop-256-class (:inherit ,font-lock-comment-face))))

  `(web-mode-constant-face
    ((,lop-class     (:inherit ,font-lock-constant-face))
     (,lop-256-class (:inherit ,font-lock-constant-face))))

  `(web-mode-keyword-face
    ((,lop-class     (:foreground ,keyword))
     (,lop-256-class (:foreground ,keyword-256))))

  `(web-mode-doctype-face
    ((,lop-class     (:inherit ,font-lock-comment-face))
     (,lop-256-class (:inherit ,font-lock-comment-face))))

  `(web-mode-function-name-face
    ((,lop-class     (:inherit ,font-lock-function-name-face))
     (,lop-256-class (:inherit ,font-lock-function-name-face))))

  `(web-mode-string-face
    ((,lop-class     (:foreground ,str))
     (,lop-256-class (:foreground ,str-256))))

  `(web-mode-type-face
    ((,lop-class     (:inherit ,font-lock-type-face))
     (,lop-256-class (:inherit ,font-lock-type-face))))

  `(web-mode-html-attr-name-face
    ((,lop-class     (:foreground ,func))
     (,lop-256-class (:foreground ,func-256))))

  `(web-mode-html-attr-value-face
    ((,lop-class     (:foreground ,keyword))
     (,lop-256-class (:foreground ,keyword-256))))

  `(web-mode-warning-face
    ((,lop-class     (:inherit ,font-lock-warning-face))
     (,lop-256-class (:inherit ,font-lock-warning-face))))

  `(web-mode-html-tag-face
    ((,lop-class     (:foreground ,builtin))
     (,lop-256-class (:foreground ,builtin-256))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'lop)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; lop-theme.el ends here
