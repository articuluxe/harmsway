;;; hippo-themesv-light-theme.el --- Hippo color theme


;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:


;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.


;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;; Author:Kimi MA
;; Keywords: faces local color theme
;; URL: http://github.com/kimim/emacs-hippo-theme
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1"))


;;; Commentary:
;; This theme assumes light background.


;;; Code:

(deftheme hippo-themes-light
  "Hippo Color theme for day light.")

(let ((class '((class color) (min-colors 88)))
      (hippo-theme-bg "#f8f8f8")
      (hippo-theme-fg "#000000")
      (hippo-theme-definition "blue")
      (hippo-theme-const "#110099")
      (hippo-theme-comment "#3F7F5F")
      (hippo-theme-error "#FF0000")
      (hippo-theme-builtin "#7F0055")
      (hippo-theme-string "#2A00FF")
      (hippo-theme-blue-3 "#758BC6")
      (hippo-theme-region "LightBlue")
      (hippo-theme-shadow "grey30")
      (hippo-theme-highlight "azure")
      (hippo-theme-modeline-bg "gainsboro")
      (hippo-theme-lowlight "SlateGray4"))
  (apply #'custom-theme-set-faces 'hippo-themes-light
         (mapcar
          (lambda (x) `(,(car x) ((,class ,(cdr x)))))
          `((default :foreground ,hippo-theme-fg :background ,hippo-theme-bg)
            (bold :inherit default :weight bold)
            (italic :inherit default :slant italic)
            (underline :inherit default :foreground "magenta4" :underline t)
            ;; make the code font in markdown look better
            (fixed-pitch :family Consolas)
            (fixed-pitch-serif :family Consolas)
            (cursor :background ,"orange")
            (fringe :background ,hippo-theme-bg)
            (mode-line :foreground "black" :background ,hippo-theme-modeline-bg :box nil :height 0.85)
            (mode-line-inactive :foreground "royal blue" :background ,hippo-theme-modeline-bg :box nil :height 0.85)
            (mode-line-buffer-id :box nil :weight bold)
            (header-line :foreground "black" :background ,hippo-theme-modeline-bg :box nil :height 0.85)
            (shadow :foreground ,hippo-theme-shadow)
            (success :foreground ,hippo-theme-error)
            (error :foreground ,hippo-theme-error :weight bold)
            (warning :foreground "DarkOrange" :weight bold)
            (compilation-warning :underline t :inherit warning)
            (compilation-error :underline t :inherit error)
            (compilation-info :underline t :foreground ,hippo-theme-const)
            (highlight :background "DarkSeaGreen1")
            (marginalia-documentation :background unspecified :slant italic) ;; remove background
            (region :background ,hippo-theme-region)
            (secondary-selection :background "PaleTurquoise" :foreground "orange")
            (whitespace-indentation :background "LightYellow" :foreground "LightGray")
            ;; (font-lock-negation-char-face :foreground "#e8e2b7")
            (font-lock-builtin-face :foreground ,hippo-theme-builtin :bold t)
            (font-lock-comment-face :foreground ,hippo-theme-comment :slant normal)
            (font-lock-comment-delimiter-face :foreground ,hippo-theme-comment :slant normal)
            (font-lock-constant-face :foreground ,hippo-theme-const)
            (font-lock-doc-face :foreground ,hippo-theme-string)
            (font-lock-doc-string-face :foreground ,hippo-theme-string)
            (font-lock-function-name-face :foreground ,hippo-theme-definition :bold t)
            (font-lock-keyword-face :foreground ,hippo-theme-builtin :weight bold)
            (font-lock-preprocessor-face :foreground ,hippo-theme-builtin :bold t)
            (font-lock-regexp-grouping-backslash :foreground ,hippo-theme-builtin)
            (font-lock-regexp-grouping-construct :foreground ,hippo-theme-builtin)
            (font-lock-string-face :foreground ,hippo-theme-string)
            (font-lock-type-face :foreground ,hippo-theme-fg :underline t :slant italic)
            (font-lock-variable-name-face :foreground ,hippo-theme-fg)
            (font-lock-warning-face :foreground ,hippo-theme-error)
            (font-lock-doxygen-face :foreground "SaddleBrown" :background "#f7f7f7")
            ;; avoid punctuation's background from default
            (tree-sitter-hl-face:punctuation :background unspecified)
            (org-level-1 :weight bold :foreground "#00008b") ;; dark blue
            (org-level-2 :weight bold :foreground "#6a5acd") ;; slate blue
            (org-level-3 :weight bold :foreground "#0078d7")
            (org-level-4 :weight bold :foreground "#c71585") ;; medium violet red
            (org-level-5 :slant normal :foreground "#00008b")
            (org-level-6 :slant normal :foreground "#6a5acd")
            (org-level-7 :slant normal :foreground "#a0522d")
            (org-level-8 :slant normal :foreground "#c71585")
            (markdown-header-face-1 :inherit org-level-1)
            (markdown-header-face-2 :inherit org-level-2)
            (markdown-header-face-3 :inherit org-level-3)
            (markdown-header-face-4 :inherit org-level-4)
            (markdown-header-face-5 :inherit org-level-5)
            (markdown-header-face-6 :inherit org-level-6)
            (markdown-header-face-7 :inherit org-level-7)
            (markdown-header-face-8 :inherit org-level-8)
            (org-code :foreground ,hippo-theme-builtin :weight bold)
            (org-verbatim :foreground ,hippo-theme-const)
            (org-hide :foreground ,hippo-theme-bg)
            (org-drawer :foreground ,hippo-theme-lowlight)
            (org-tag :slant italic :weight normal :foreground ,hippo-theme-lowlight)
            (org-beamer-tag :inherit org-tag)
            (org-block-begin-line :inherit fixed-pitch :foreground ,hippo-theme-const)
            (org-block-end-line :inherit fixed-pitch :foreground ,hippo-theme-const)
            (org-block :background "gray94")
            (org-scheduled-previously :foreground ,hippo-theme-comment)
            (org-todo :foreground "orange red" :weight bold)
            (org-checkbox :inherit org-todo)
            (org-warning :foreground "dark orchid" :weight bold)
            (org-table :foreground "gray10")
            (org-footnote :foreground "purple" :underline t)
            (org-ref-cite-face :inherit link)
            (org-agenda-clocking :inherit secondary-selection
                                 :foreground ,hippo-theme-fg)
            (gnus-summary-cancelled :foreground "lightblue")
            (gnus-header-subject :foreground "blue" :bold t)
            (ido-subdir :weight bold)
            (which-func :foreground ,hippo-theme-builtin)
            (minibuffer-prompt :foreground "medium blue")
            (hl-line :background ,hippo-theme-highlight)
            ;; defaults
            (show-paren-match :background "turquoise")
            (isearch :background "orange")
            (link :foreground "RoyalBlue3" :underline t)
            ;; other packages
            (helm-locate-finish :foreground ,hippo-theme-const)
            (aw-mode-line-face :foreground ,hippo-theme-string)
            (swiper-match-face-1 :background "white smoke")
            (swiper-match-face-2 :background "#FFCCCC") ;;selected match 1
            (swiper-match-face-3 :background "#CCFFFF") ;;selected match 2
            (swiper-match-face-4 :background "#FFFFCC") ;;selected match 3
            (swiper-background-match-face-1 :background "white smoke")
            (swiper-background-match-face-2 :background "#FFECEC") ;; unselected match 1
            (swiper-background-match-face-3 :background "#ECFFFF") ;; unselected match 2
            (swiper-background-match-face-4 :background "#FFFFEC") ;; unselected match 3
            (swiper-line-face :background "azure2")
            (swiper-line-face :background "#f3d3d3")
            (hydra-face-red :foreground "#cc0000" :bold t)
            (hydra-face-blue :foreground "RoyalBlue3" :bold t)
            (powerline-active1 :background "grey22" :foreground "white" :inherit mode-line)
            (powerline-active2 :background "grey40" :foreground "white" :inherit mode-line)
            (powerline-inactive1 :background "grey22" :foreground "white" :inherit mode-line-inactive)
            (powerline-inactive2 :background "grey40" :foreground "white" :inherit mode-line-inactive)
            ;; (magit-tag :background "LemonChiffon1" :foreground "goldenrod4")
            ;; (magit-section-heading :inherit header-line)
            (magit-section-highlight :weight bold :background ,hippo-theme-highlight)
            ;; (magit-diff-context :foreground "grey20")
            ;; (magit-diff-context-highlight :weight bold :foreground "grey20")
            ;; (magit-diff-added :inherit diff-added)
            ;; (magit-diff-added-highlight :inherit diff-added :weight bold)
            ;; (magit-diff-removed :inherit diff-removed)
            ;; (magit-diff-removed-highlight :inherit diff-removed :weight bold)
            ;; (magit-diff-file-heading)
            ;; (magit-diff-file-heading-highlight :weight bold)
            ;; (magit-diff-file-heading-selection :foreground "red")
            ;; (magit-diff-hunk-heading :inherit diff-hunk-header)
            ;; (magit-diff-hunk-heading-highlight :inherit diff-hunk-header :weight bold)
            ;; (magit-hash :foreground "firebrick")
            ;; (magit-branch-remote :background "Grey85" :foreground "OliveDrab4" :box t)
            ;; (magit-branch-local :background "Grey85" :foreground "LightSkyBlue4" :box t)
            (ivy-highlight-face)
            (ivy-posframe :background "#eeeeee" :foreground "#000000")
            (sdcv-tooltip-face :background "#eeeeee" :foreground "#000000")
            (wgrep-face :foreground ,hippo-theme-comment)
            (cider-instrumented-face)
            (mu4e-header-highlight-face :background "azure")
            (mu4e-replied-face :foreground "dark green")
            (mu4e-forwarded-face :foreground "dark orange")
            (mu4e-unread-face :foreground "blue" :bold t)
            (message-cited-text-1 :foreground "blue")
            (sr-active-path-face :background ,hippo-theme-bg :foreground "deep pink")
            (sr-passive-path-face :background ,hippo-theme-bg :foreground "blue")
            (hide-ifdef-shadow :inherit shadow :foreground "olive drab")
            (ggtags-global-line :inherit secondary-selection :foreground "black")
            (lsp-ui-doc-url :inherit link :height 0.8)
            (lsp-ui-doc-background :background ,hippo-theme-modeline-bg)
            (term-color-white :foreground "blue")
            (term-color-blue :foreground "blue")
            (next-error :inherit highlight)
            (lsp-ui-doc-highlight-hover :inherit highlight)
            (yas-field-highlight-face :inherit highlight)))))


(custom-theme-set-variables
 'hippo-themes-light
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
                            "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'hippo-themes-light)

;;; hippo-themes-light-theme.el ends here
