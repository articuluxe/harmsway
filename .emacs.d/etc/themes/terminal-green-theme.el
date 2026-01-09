;;; terminal-green-theme.el --- Color theme based on black terminal with a green foreground color

;; Copyright (C) 2025 Alexey Veretennikov

;; Author: Alexey Veretennikov <alexey dot veretennikov at protonmail dot com>
;; Keywords: themes
;; URL: http://codeberg.org/fourier/terminal-green-theme
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

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

;; To use it, put the following in your Emacs configuration file:
;;
;;   (load-theme 'terminal-green t)
;;
;; Requirements: Emacs 29.

;;; Code:

(deftheme terminal-green
  "Theme based on terminal green colors")

(custom-theme-set-faces
 'terminal-green
 '(default ((t (:underline nil
                           :overline nil
                           :strike-through nil
                           :box nil
                           :inverse-video nil
                           :foreground "#00BB00"
                           :background "#000000"
                           :stipple nil
                           :inherit nil))))
 '(cursor ((t (:background "#00bb00"))))
 '(region ((t (:background "#005500"))))
 ;; vertical line when spilt window with C-x 3 on text terminals
 '(vertical-border ((t (:foreground "#003300"))))
 ;; same on graphical displays
 '(fringe ((t (:foreground "#003300"))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#33FF33"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :inherit (link)))))
 ;; search
 '(isearch ((t (:background "#001100" :box (:line-width -1 :color "#00bb00")))))
 ;; other search matches
 '(lazy-highlight ((t (:background "#001100" :box (:line-width -1 :color "#007000")))))
 ;; tooltip colors doesn't work on OSX
 '(tooltip ((t (:inherit (variable-pitch) :foreground "#00bb00" :background "#005c00"))))
 '(mode-line ((t (:box (:line-width -1 :color "#464646") :weight light :foreground "#00AA00" :background "#003300"))))
 '(mode-line-inactive ((t (:style released-button :inherit (mode-line) :weight normal))))
 ;; '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 ;; IDEA supports something like hl-mode
 '(hl-line ((t (:background "#001100"))))
 ;; ecb customizations
 '(ecb-default-highlight-face ((t (:background "DarkSlateGray" :box (:line-width 1 :style released-button)))))
 '(ecb-default-general-face ((t (:foreground "white"))))
 ;; coding customizations
 '(font-lock-comment-face ((t (:foreground "#003300"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#003300"))))
 '(font-lock-doc-face ((t (:italic t :foreground "#003300"))))
 '(font-lock-keyword-face ((t (:bold t))))
 '(font-lock-preprocessor-face ((t (:foreground "#66FF66" :underline t))))
 '(font-lock-string-face ((t (:foreground "#33FF33"))))
 '(font-latex-string-face ((t (:bold t))))
 '(font-latex-math-face ((t (:bold t))))
 '(font-lock-type-face ((t (:bold t :italic t))))
 '(font-lock-builtin-face ((t (:bold t))))
 '(font-lock-function-name-face ((t (:italic t :underline t))))
 ;; (font-lock-function-name-face ((t (:foreground "selectedControlColor"))))
 '(font-lock-variable-name-face ((t (:foreground "green" :box t))))
 ;; (font-lock-negation-char-face ((t (:foreground "white"))))
 '(font-lock-number-face ((t (:foreground "#33FF33"))))
 '(font-lock-constant-face ((t (:bold t :italic t))))
 '(font-lock-warning-face ((t (:foreground "red"))))
 '(font-lock-operator-face ((t (:foreground "#33FF33"))))
 '(font-lock-end-statement ((t (:bold t))))
 ;; Java-like annotations 
 '(c-annotation-face ((t (:bold t))))
 ;; log4j customizations
 '(log4j-font-lock-warn-face ((t (:bold t))))
 ;; info-mode customization
 '(info-menu-header ((t (:foreground "green"))))
 '(info-title-1 ((t (:foreground "green"))))
 '(info-title-2 ((t (:foreground "green"))))
 '(info-title-3 ((t (:foreground "green"))))
 '(info-title-4 ((t (:foreground "green"))))
 ;; python customizations
 '(py-builtins-face ((t (:bold t))))
 ;; helm customizations
 '(helm-selection ((t (:background "#003300" :foreground "black"))))
 '(helm-ff-directory ((t (:foreground "#00ff00" :background "#003300"))))
 ;; dired customizations
 '(diredp-file-name ((t (:italic t))))
 '(diredp-file-suffix ((t (:italic t))))
 '(diredp-dir-heading ((t (:bold t :underline t ))))
 '(diredp-dir-priv ((t (:bold t))))
 '(diredp-dir-name ((t (:bold t))))
 ;; file attributes in the dired
 '(diredp-read-priv ((t (:bold nil))))
 '(diredp-write-priv ((t (:bold nil))))
 '(diredp-exec-priv ((t (:bold nil))))
 ;; no attribute set
 '(diredp-no-priv ((t (:bold nil))))
 '(diredp-symlink ((t (:foreground "#003300"))))
 ;; marked file color and mark sign
; '(diredp-flag-mark-line ((t (:background "#2B2B2B" :foreground "gold"))))
; '(diredp-flag-mark ((t (:foreground "gold" :background "#2B2B2B"))))
; '(diredp-inode+size ((t (:foreground "white"))))
; '(diredp-compressed-file-suffix ((t (:foreground "cyan1"))))
; '(diredp-ignored-file-name ((t (:foreground "cyan1"))))
 ;; nXML customizations
 ; '<' and '>' characters
 '(nxml-tag-delimiter ((t (:bold t))))
 ; '=' and '"' characters 
 '(nxml-attribute-value-delimiter ((t (:bold t))))
 ; tag name
 '(nxml-element-local-name ((t (:bold t))))
 ; attribute name
 ;'(nxml-attribute-local-name ((t (:foreground "#BABABA"))))
 '(nxml-attribute-local-name ((t (:bold t))))
 ; attribute value
 '(nxml-attribute-value ((t (:bold t))))
 '(nxml-text ((t (:bold t))))
 '(nxml-cdata-section-content ((t (:bold t))))
 ; attribute prefix like xlink:href - here it is "xlink"
 ;'(nxml-attribute-prefix ((t (:foreground "#DADADA"))))
 '(nxml-attribute-prefix ((t (:bold t))))
 ; tag prefix : <ui:Checkbox> - here it is "ui"
 '(nxml-element-prefix ((t (:bold t))))
 '(show-paren-match ((t (:background "#005500"))))
 '(echo-area-face ((t (:foreground "#00BB00"))))
 '(minibuffer-prompt ((t (:bold t :foreground "green"))))
 '(help-key-binding ((t (:box (:line-width -1 :color "#464646") :foreground "#00BB00" :background "black"))))
 ;; TODO: fix below
 ;; ERC customizations
 '(erc-nick-default-face ((t (:foreground "##9876AA" :bold t))))
 '(erc-action-face ((t (:foreground "#808080"))))
 '(erc-button ((t (:foreground "cyan" :underline t))))
 ;; GNUS customizations
 '(gnus-group-mail-3-empty ((t )))
 '(gnus-group-mail-3 ((t (:foreground "#BBEDFF" :weight bold))))
 '(gnus-group-mail-low-empty  ((t )))
 '(gnus-group-mail-low  ((t (:foreground "#BBEDFF" :weight bold))))
 '(gnus-group-news-3-empty  ((t )))
 '(gnus-group-news-3  ((t (:foreground "#BBEDFF" :weight bold))))
 ;; GNUS topic face. By default GNUS doesn't support
 ;; topic faces, see http://www.emacswiki.org/emacs/GnusFormatting#toc6
 ;; how to add one
 '(gnus-topic-face ((t (:foreground "#FFC66D" :weight bold :underline t))))
 '(gnus-topic-empty-face ((t (:foreground "#FFC66D" :underline t))))
 ;; GNUS summary (list of messages) faces
 '(gnus-summary-normal-ancient ((t )))
 '(gnus-summary-normal ((t )))
 ;; unread message face
 '(gnus-summary-normal-unread ((t (:foreground "#BBEDFF" :weight bold))))
 ;; to use these faces, set them in the config file as
 ;; (setq gnus-face-1 'darkula-gnus-face-1) etc.
 ;; they are set here, see below
 ;; author in the list of messages
 '(darkula-gnus-face-1 ((t (:foreground "#9876AA" :italic t))) t)
 ;; message header in the list of messages
 '(darkula-gnus-face-2 ((t (:foreground "#A5C261"))))

 ;; Company-mode faces based on IntelliJ IDEA colors
 '(company-tooltip ((t (:foreground "#bbbbbb" :background "#3c3f41"))))
 '(company-tooltip-selection ((t (:background "#0052a4"))))

 '(company-tooltip-search ((t (:inherit isearch))))
 '(company-tooltip-mouse ((t (:background "#4a4e4f"))))
 '(company-tooltip-common ((t (:foreground "#d17ad6"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip-common))))
 '(company-scrollbar-fg ((t (:background "#5b5d5e"))))
 '(company-scrollbar-bg ((t (:background "#3b3f40"))))
 ;;'(company-preview ((t (:background "blue4" :foreground "wheat"))))
 '(company-preview ((t (:inherit region))))
 '(company-preview-common ((t (:inherit company-preview :foreground "#d17ad6"))))
 '(company-preview-search ((t (:inherit isearch))))
 '(company-template-field ((t (:inherit isearch))))
 ;; some custom additions to the Common Lisp code
 '(lisp-font-lock-annotation-face ((t (:foreground "#BBB529"))))
 '(slime-repl-inputed-output-face ((t (:inherit 'default))))
 '(slime-repl-input-face ((t (:inherit 'default :weight bold))))
 '(slime-repl-output-face ((t (:foreground "cyan1"))))
 '(slime-repl-prompt-face ((t (:foreground "#007f00"))))
 ;; updated
 '(sly-mrepl-output-face ((t (:foreground "#007f00"))))
 )


 ;; '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 ;; '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 ;; '(header-line ((t (:underline (:color foreground-color :style line) :inverse-video nil :foreground "#e7f6da" :background "#303030" :inherit (mode-line)))))

;; (setq gnus-face-1 'darkula-gnus-face-1)
;; (setq gnus-face-2 'darkula-gnus-face-2)


;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'terminal-green)
;;; terminal-green-theme.el ends here
