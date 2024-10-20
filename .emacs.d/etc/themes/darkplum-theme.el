;;; darkplum-theme.el --- A Dark Plum Theme

;; Copyright 2021 Rebecca Skinner, All rights reserved
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Rebecca Skinner <rebecca@rebeccaskinner.dev>
;; Version: 1.0.0
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/rebeccaskinner/darkplum-theme
;; Keywords: theme, dark, purple, plum

;;; Commentary:
;; darkplum, A Dark Plum Theme.

;;; Code:
;;
;; Based on
;;
;; Rebecca: https://github.com/vic/rebecca-theme

(deftheme darkplum "A dark plum theme.")

(let*
  (
    (class
      '((class color) (min-colors 256)))

    ;; default colors
    (background "#2a2035") ; Dark plum color as the default background
    (foreground "#b0b0b0") ; Light grey color as the default foreground
    (cursor     "#ffc0cb") ; X11 pink

    ;; common programming colors
    (keyword "#9370DB")
    (comment "#645464")
    (identifier "#dda0dd")
    (operator "#eac6ea")
    (error-color "#FF1493")
    (warning-color "#FFE4B5")
    (info-color "#FFC0CB")
    (string-color "#ece0ec")
    (function-color "#da70d6")
    (type-color "#aa38f2")
    ;;


    ;; alternate defaults
    (dark-background  "#1d1625") ; slightly darker default background
    (half-light-background  "#362945") ; slightly darker default background
    (light-background "#433355")
    (dark-foreground  "#441177")
    (light-foreground "#ffbff2")

    ;; modeline colors
    (modeline-background light-background)
    (modeline-foreground light-foreground)
    (modeline-inactive-foreground light-foreground)
    (modeline-inactive-background half-light-background)
    (modeline-error-background modeline-background)
    (modeline-error-foreground error-color)

    ;; rainbow delimiters colors
    (rainbow-1 "#ffc0cb")
    (rainbow-2 "#ffa7b6")
    (rainbow-3 "#ff8da1")
    (rainbow-4 "#ff748c")
    (rainbow-5 "#fff35f")
    (rainbow-6 "#ffdae0")
    (rainbow-7 "#ffc0cb")
    (rainbow-8 "#ffa7b6")
    (rainbow-9 "#ff8da1")

    ;; ansi colors (for ansi-term)
    (ansi-white foreground)
    (ansi-black background)
    (ansi-blue  "#dda0dd")
    (ansi-cyan "#01b8b2")
    (ansi-green "#01a853")
    (ansi-magenta "#ffc0cb")
    (ansi-red "#a020f0")
    (ansi-yellow "#aaa853")
    )

  (custom-theme-set-faces
    'darkplum
    `(default
       (
         (((class color) (min-colors 4096))
           (:foreground ,foreground :background ,background))

         (((class color) (min-colors 256))
           (:foreground ,"#dddddd" :background ,"#000000"))

         (,class
           (:foreground ,"#dddddd" :background ,"#000000"))))

    ;; general editing faces

    `(cursor              ((,class (:background ,cursor))))
    `(header-line         ((,class (:foreground ,foreground))))
    `(fringe              ((,class (:background ,background))))
    `(highlight           ((,class (:foreground ,light-foreground :background ,light-background))))
    `(region              ((,class (:background ,light-background))))
    `(isearch             ((,class (:foreground ,light-foreground :background ,dark-background))))
    `(lazy-highlight      ((,class (:background ,light-background))))
    `(trailing-whitespace ((,class (:background ,error-color))))

    `(success ((,class (:foreground ,light-foreground :italic t))))
    `(warning ((,class (:foreground ,warning-color :italic t))))
    `(error ((,class (:foreground ,error-color :italic t))))
    `(compilation-warning ((,class (:foreground ,warning-color))))
    `(breakpoint-enabled ((,class (:foreground ,error-color))))

    ;; modeline faces
    `(mode-line           ((,class (:bold t :background ,modeline-background :foreground ,modeline-foreground))))
    `(mode-line-inactive  ((,class (:background ,modeline-inactive-background :foreground ,modeline-inactive-foreground))))
    `(compilation-mode-line-fail ((,class (:background ,modeline-error-background :foreground ,modeline-error-foreground))))

    ;; line numbers
    `(line-number ((,class (:background ,background :foreground ,comment))))

    ;; minibuffer faces
    `(minibuffer-prompt ((,class (:bold t :foreground ,light-foreground))))

    ;; font lock faces
    `(font-lock-builtin-face ((,class (:bold t :foreground ,keyword))))
    `(font-lock-comment-face ((,class (:foreground ,comment))))
    `(font-lock-doc-face ((,class (:bold t :foreground ,comment))))
    `(font-lock-constant-face ((,class (:bold t))))
    `(font-lock-function-name-face ((,class (:bold t :foreground ,function-color))))
    `(font-lock-variable-name-face ((,class (:foreground ,identifier))))
    `(font-lock-type-face ((,class (:bold t :foreground ,type-color))))
    `(font-lock-keyword-face ((,class (:bold t :foreground ,keyword))))
    `(font-lock-warning-face ((,class (:underline t))))
    `(font-lock-preprocessor-face ((,class (:foreground ,light-foreground))))
    `(font-lock-string-face ((,class (:foreground ,string-color))))

    ;; flycheck
    `(flycheck-error
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,error-color)))
       (,class (:foreground ,foreground :background ,error-color :inherit bold :underline t))))

    `(flycheck-warning
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,warning-color)))
       (,class (:foreground ,foreground :background ,warning-color :inherit bold :underline t))))

    `(flycheck-info
      ((,(append '((supports :underline (:style wave))) class)
        (:underline (:style wave :color ,info-color)))
       (,class (:foreground ,foreground :background ,info-color :inherit bold :underline t))))

    `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
    `(flycheck-error-list-info ((,class (:foreground ,info-color))))
    `(flycheck-error-list-warning ((,class (:foreground ,warning-color))))

    `(flycheck-fringe-error ((,class (:foreground ,error-color :inherit bold))))
    `(flycheck-fringe-info ((,class (:foreground ,info-color :inherit bold))))
    `(flycheck-fringe-warning ((,class (:foreground ,warning-color :inherit bold))))

    ;; lsp-ui
    `(lsp-ui-sideline-code-action ((,class (:foreground ,light-foreground :bold t))))
    `(lsp-ui-peek-file-name ((,class (:foreground ,light-foreground))))

    ;; lsp-treemacs
    `(lsp-treemacs-file-hint ((,class (:foreground ,info-color :italic t))))
    `(lsp-treemacs-file-info ((,class (:foreground ,info-color :bold t))))
    `(lsp-treemacs-file-warn ((,class (:foreground ,warning-color :bold t))))

    ;; lsp-headerline
    `(lsp-headerline-breadcrumb-path-hint-face
      ((,(append '((supports :underline (:style line))) class)
        (:underline (:style line :color ,info-color)))
       (,class (:foreground ,foreground :background ,info-color :inherit bold :underline t))))

    `(lsp-headerline-breadcrumb-path-info-face
      ((,(append '((supports :underline (:style line))) class)
        (:underline (:style line :color ,info-color)))
       (,class (:foreground ,foreground :background ,info-color :inherit bold :underline t))))

    `(lsp-headerline-breadcrumb-path-warning-face
      ((,(append '((supports :underline (:style line))) class)
        (:underline (:style line :color ,warning-color)))
       (,class (:foreground ,foreground :background ,warning-color :inherit bold :underline t))))

    `(lsp-headerline-breadcrumb-path-error-face
      ((,(append '((supports :underline (:style line))) class)
        (:underline (:style line :color ,error-color)))
       (,class (:foreground ,foreground :background ,error-color :inherit bold :underline t))))

    `(lsp-headerline-breadcrumb-symbols-hint-face
      ((,(append '((supports :underline (:style line))) class)
        (:underline (:style line :color ,info-color)))
       (,class (:foreground ,foreground :background ,info-color :italic t :underline t))))

    `(lsp-headerline-breadcrumb-symbols-info-face
      ((,(append '((supports :underline (:style line))) class)
        (:underline (:style line :color ,info-color)))
       (,class (:foreground ,light-foreground :background ,info-color :bold t :underline t))))

    `(lsp-headerline-breadcrumb-symbols-warning-face
      ((,(append '((supports :underline (:style line))) class)
        (:underline (:style line :color ,warning-color)))
       (,class (:foreground ,light-foreground :background ,warning-color :bold t :underline t))))

    `(lsp-headerline-breadcrumb-symbols-error-face
      ((,(append '((supports :underline (:style line))) class)
        (:underline (:style line :color ,error-color)))
       (,class (:foreground ,light-foreground :background ,error-color :bold t :underline t))))


    ;; links
    `(link ((,class (:underline t :foreground ,light-foreground))))
    `(link-visited ((,class (:underline t))))

    ;; line numbers
    `(line-number-major-tick ((,class (:bold t :background ,background :foreground ,foreground))))
    `(line-number-minor-tick ((,class (:background ,background :foreground ,foreground))))

    ;; rainbow delimiters
    `(rainbow-delimiters-depth-1-face ((,class (:background ,background :foreground ,rainbow-1))))
    `(rainbow-delimiters-depth-2-face ((,class (:background ,background :foreground ,rainbow-2))))
    `(rainbow-delimiters-depth-3-face ((,class (:background ,background :foreground ,rainbow-3))))
    `(rainbow-delimiters-depth-4-face ((,class (:background ,background :foreground ,rainbow-4))))
    `(rainbow-delimiters-depth-4-face ((,class (:background ,background :foreground ,rainbow-5))))
    `(rainbow-delimiters-depth-6-face ((,class (:background ,background :foreground ,rainbow-6))))
    `(rainbow-delimiters-depth-7-face ((,class (:bold t :background ,background :foreground ,rainbow-7))))
    `(rainbow-delimiters-depth-8-face ((,class (:bold t :background ,background :foreground ,rainbow-8))))
    `(rainbow-delimiters-depth-9-face ((,class (:bold t :background ,background :foreground ,rainbow-9))))

    ;; Headers
    `(info-title-1 ((,class (:height 2.0  :bold t :background ,background :foreground ,light-foreground))))
    `(info-title-2 ((,class (:height 1.5  :bold t :background ,background :foreground ,light-foreground))))
    `(info-title-3 ((,class (:height 1.25 :bold t :background ,background :foreground ,light-foreground))))
    `(info-title-4 ((,class (:height 1.0  :bold t :background ,background :foreground ,light-foreground))))

    ;; Mardown Code Blocks
    `(markdown-code-face ((,class (:foreground ,light-foreground))))
    `(markdown-markup-face ((,class (:bold t :foreground ,comment))))
    `(markdown-header-face-1 ((,class (:height 2.0  :bold t :background ,background :foreground ,keyword))))
    `(markdown-header-face-2 ((,class (:height 1.5  :bold t :background ,background :foreground ,keyword))))
    `(markdown-header-face-3 ((,class (:height 1.25 :bold t :background ,background :foreground ,keyword))))
    `(markdown-header-face-4 ((,class (:height 1.0  :bold t :background ,background :foreground ,keyword))))

    ;; Org-Mode
    `(org-level-1 ((,class (:height 1.5  :bold t :foreground ,light-foreground))))
    `(org-level-2 ((,class (:height 1.25 :bold t :foreground ,light-foreground))))
    `(org-level-3 ((,class (:height 1.15 :bold t :foreground ,light-foreground))))
    `(org-level-4 ((,class (:height 1.0  :bold t :foreground ,light-foreground))))
    `(org-table ((,class (:foreground ,light-foreground))))

    `(org-headline-done ((,class (:bold t :foreground ,comment))))
    `(org-todo ((,class (:bold t :foreground ,keyword))))
    `(org-done ((,class (:bold t :foreground ,comment))))

    ;; ansi-term colors
    `(term ((,class (:foreground ,foreground))))
    `(term-color-black ((,class (:foreground ,ansi-black))))
    `(term-color-blue ((,class (:foreground ,ansi-blue))))
    `(term-color-cyan ((,class (:foreground ,ansi-cyan))))
    `(term-color-green ((,class (:foreground ,ansi-green))))
    `(term-color-magenta ((,class (:foreground ,ansi-magenta))))
    `(term-color-red ((,class (:foreground ,ansi-red))))
    `(term-color-white ((,class (:foreground ,ansi-white))))
    `(term-color-yellow ((,class (:foreground ,ansi-yellow))))

    (custom-theme-set-variables
      'darkplum
      `(ansi-color-names-vector [])))
)


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'darkplum)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; darkplum-theme.el ends here
