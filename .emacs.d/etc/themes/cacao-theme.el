;;; cacao-theme.el --- Based on a image reversal
;; Package-Requires: ((emacs "24.1"))
;; Author: Panaman Creel 
;; Modeline correction and color class by RickMMA@github.com
;; URL: https://github.com/Michael-Garibaldi/cacao-theme
;; Version: 1.2
;; UPdated 05/13/2026
;; Package-Requires ((emacs "24.1"))


;;; Commentary:
;; Colors provided from https://imagecolorpicker.com
;; An excellent site that allows you to upload a pic and it turns the colors into
;; css color representations!

;; To use it, put it  the following in your Emacs configuration file:
;; (load-theme 'cacao-theme t)
;; Requirements: Emacs 24

;;; Code
(require 'org)
(deftheme cacao
  "This  theme is based on an accidentally inversed image")

;; Custom variables to make changing colors easier.
(let ((class '((class color)))
      (colorbg1 "#e6dac2")
      (colorbg2 "#e65a43")
      (colorfg1 "#693c28")
      (colorfg2 "#e6dac2") ;; Used in highligt face.
      )

(custom-theme-set-faces
 'cacao

 ;; default setting
 `(default ((t ( :background ,colorbg1 :foreground ,colorfg1
                 :slant normal :weight medium :height 112  :width normal
                 :foundry "outline" :family "Times New Roman Bold 20"))))

`(fixed-pitch ((t (:height 100 :family "DejaVu Sans Mono Bold" :foreground "#7e4c37" :weight bold))))

;; Basic  Settings
`(cursor ((t (:background ,colorbg1 :family "Times New Roman Bold 20"))))
`(header-line ((t (:background ,colorbg2 :foreground ,colorfg1 :box (:line-width -1 :color "chartreuse" :style released-button) :height 0.9 :family "Times New Roman Bold 20"))))
;;`(highlight ((t (:background "#e65a43" :foreground "#a65a4f" :family "Times New Roman Bold 20"))))
`(highlight ((t (:background "#a65a4f" :foreground "#f7efee" :family "Times New Roman Bold 20"))))
;;`(fixed-pitch ((t (:background ,colorbg1 :foreground ,colorfg1 :family "Times New Roman Bold 20"))))
`(minibuffer-prompt ((t (:foreground "#7e4c37" :weight bold))))
`(mode-line  ((t (:background  "#693c28" :foreground "#f2e4c1" :box 2 :height 1.0 :weight bold :family "Times New Roman  Book 20"))))
`(mode-line-buffer-id ((t (:foreground "#a65a4f" :background "#f2e4c1" :weight bold :height 0.9 :family "Times New Roman Book 20"))))
`(mode-line-emphasis ((t (:weight bold :family "Times New Roman Book 35"))))
`(mode-line-highlight ((t (:box (:line-width 2 :color ,colorbg2 :style released-button) :family "Times New Roman Book 20 Bold"))))

;;  mode-line inactive is a bit different, a box was added and the background color has been changed to be a bit different than the
;; foreground.

`(mode-line-inactive ((t (:background "#dccca3" :foreground "#7f5e42" :box (:line-width 8 :color "#693c28") :overline nil :underline nil :height 105 :weight bold  :family "Times New Roman Book 20 Book"))))



`(tab-bar ((t (:background "#693c28" :foreground "#f2e4c1"))))
`(tooltip ((t (:background "#f9f0dd" :foreground "#460a04" :family "Times New Roman Bold 20"))))
`(isearch ((t (:background "#e7ddc9" :foreground "#7f5e42" :weight bold)))) ;; for isearch queries
`(isearch-fail ((t (:foreground "#8c111b" :weight bold)))) ;; isearch has failed


;; Elements within code
;; built-in-face is for built-in functions

`(font-lock-builtin-face ((t (:foreground "#4f4846" :weight bold))))
`(font-lock-comment-delimiter-face ((t (:foreground "#7e4c37" :slant italic :weight bold)))) ;for /* in most terminals
`(font-lock-comment-face ((t (:foreground "#ab5e51" :slant italic :family "Times New Roman 20" )))) ;for comments themselves
`(font-lock-constant-face ((t (:foreground "#d1914a" :weight bold)))) ;; For constants, like "NULL" in C
`(font-lock-doc-face ((t (:foreground "#d6af7d" :slant italic :family "Times New Roman 20")))) ;; For documentation embedded in code.
`(font-lock-function-name-face ((t (:foreground "#db6f34")))) ;; name of a function being defined
`(font-lock-keyword-face ((t (:foreground "#ee7060")))) ;; for a keyword with special syntactic signifigance (i.e "for", "if" in C)
;;`(font-lock-keyword-face ((t (:foreground "#a6776c")))) ;; for easily overlooked negation characters
`(font-lock-preprocessor-face ((t (:foreground "#ade086")))) ;; for preprocessor commands.
`(font-lock-regexp-grouping-backslash ((t (:foreground "#926150")))) ;;highlights slashes that are part of grouping constructs in Elisp code 
`(font-lock-regexp-grouping-construct ((t (:foreground "#b1e491")))) ;;for parenthesised exp[ression within a regexp to define capture groups.
`(font-lock-string-face ((t (:foreground "#962e3c")))) ;;for string constants.
`(font-lock-type-face ((t (:foreground "#430d0d")))) ;;for names of user-defined data types.
`(button ((t (:background "#4e0d0d"  :foreground "#e7ddc9" :weight bold)))) ;; for buttons.
;; font-lock-variable-name-face and font-lock-warning-face don't work with this theme. Commenting out.

`(lazy-highlight ((t (:background "#eee9d8" :foreground "#4f4846"))))
`(link ((t (:foreground "#64873f" :weight bold)))) ;;weblinks
`(link-visited ((t (:foreground "#5c633d")))) ;;links we've already visited
`(match ((t (:foreground "#77812b"))))
`(next-error ((t (:foreground "#ec7d6d"))))
`(query-replace ((t (:background "#e7ddc9" :foreground "#7f5e42" :weight bold))))


;;elfeed-search faces

`(elfeed-search-date-face ((t (:foreground "#4f4846"))))
`(elfeed-search-tag-face ((t (:foreground "#df735e"))))
`(elfeed-search-feed-face ((t (:foreground "#7e4c37"))))
`(elfeed-search-title-face ((t (:foreground "#8a5243"))))
`(elfeed-search-filter-face ((t (:foreground "#552f14"))))
`(elfeed-search-unread-count-face  ((t (:foreground "#708221"))))
`(elfeed-search-last-update-face ((t (:foreground "#d1914a"))))
`(elfeed-search-unread-title-face ((t (:foreground "#673c27"))))
`(hl-line ((t (:background "#663e29" :foreground "#f3e7c9"))))
`(header-line ((t (:background "#663e29" :foreground "#f3e7c9"))))
;; eshell prompts

`(eshell-ls-archive ((t (:foreground "#59301a"))))
`(eshell-ls-backup ((t (:foreground "#f27668"))))
`(eshell-ls-clutter ((t (:foreground "#df715d" :weight bold))))
`(eshell-ls-directory ((t (:foreground "#a37369" :weight bold))))
`(eshell-ls-executable ((t (:foreground "#728424" :weight bold))))
`(eshell-ls-missing ((t (:foreground "#f57a6c" :weight bold))))
`(eshell-ls-product ((t (:foreground "#c9705a" :weight bold))))
`(eshell-prompt ((t (:foreground "#4f4846" :weight bold ))))
`(eshell-ls-readonly ((t (:foreground "#f27668" :weight bold))))
`(eshell-ls-special ((t (:foreground "#f7a53c" :weight bold))))
`(eshell-ls-symlink ((t (:foreground "#f4a180"))))
`(eshell-ls-unreadable ((t (:foreground "#fd8c81"))))


;; Frame-tabs
;; If you currently use frame-tabs, (frame-tabs-mode t) this will color the frame tabs to match this theme.
;; The selected frame will have a box around it to distinguish it from all the other frame-tabs.
;; If you don't , just comment out this section

`(frame-tabs-buffer-tab ((t (:background "#693c28" :foreground "#c17b59" ))))
`(frame-tabs-higlight-tab ((t (:background "#dccca3" :foreground "#693c28"))))

;; a box is available for frame-tabs-selected-tab
`(frame-tabs-selected-tab ((t (:box  (:line-width 2  :color "#693c28"  (:background "#f2e4c1" :foreground "#693c28" ))))))



					  

;; toolbar  (where the icons are)

`(tool-bar ((t (:background "#d6af7d"))))


;; menu bar (where the "File, Edit, Options"... are can't be done in themeing. Leaving them alone

;; Space between the window divider and the buffer.
`(fringe ((t (:background "#d6af7d"))))))


;; Org-Mode Settings
;; Settings specific for org-mode

`(org-agenda-calendar-event ((t (:foreground "#4f4846"))))
`(org-agenda-calendar-sexp ((t: (:foreground "#a75c50"))))
`(org-agenda-date-today-face ((t (:foreground "#4c260e"))))
`(org-agenda-date-weekend ((t (:foreground "#ace186"))))
`(org-agenda-diary ((t (:foreground "#693c28"))))
`(org-emphasis-alist (tt (:foreground "#683e2a" )))
`(org-agenda-filter-tags ((t (:foreground "#f18070"))))
`(org-agenda-restriction-lock ((t (:foreground "#a75c50"))))
`(org-agenda-structure-face ((t (:foreground "#7e4c37"))))
`(org-archived-face ((t (:foreground "#d1914a"))))
`(org-checkbox ((t (:foreground "#4c260e"))))
`(org-checkbox-face ((t (:foreground "#4c260e"))))
`(org-checkbox-statistics-done ((t (:foreground "#fd7769"))))
`(org-date ((t (:foreground "#7f99a6"))))
`(org-date-face ((t (:foreground "#7f99a6"))))
`(org-date-selected ((t (:foreground "#f49c7e"))))
`(org-drawer ((t (:foreground "#a6584f" :weight bold))))
`(org-ellipsis ((t (:foreground "#72931d"))))
`(org-formula-face ((t (:foreground "#7e4c37"))))
`(org-headline-done-face ((t (:foreground "#4c260e"))))
`(org-hide-face ((t (:foreground "#eee9d8"))))
`(org-latex-and-related ((t (:foreground "#a6584f" :weight bold))))
`(org-level-1-face ((t (:foreground "#3d1e0b"))))
`(org-level-2-face ((t (:foreground "#7e4c37"))))
`(org-level-3-face ((t (:foreground "#7f99a6"))))
`(org-level-4-face ((t (:foreground "#d1914a"))))
`(org-level-5-face ((t (:foreground "#d6af7d"))))
`(org-level-6-face ((t (:foreground "#a9bac1"))))
`(org-level-7-face ((t (:foreground "#75b5e2"))))
`(org-level-8-face ((t (:foreground "#F68375"))))
`(org-link-face ((t (:foreground "#81a026" :background "#693c28"))))
`(org-mode-line-clock-overrun ((t (:foreground "#693c28" :background "#eee9d8" :weight bold))))
`(org-priority ((t (:forerground "#d8948a"))))
`(org-sexp-date ((t (:foreground "#a75c50" :background "#eee9d8" :weight bold))))
`(org-schedule-face ((t (:foreground "#f0ab8a"))))
`(org-scheduled-previously-face ((t (:foreground "#e09a88"))))
`(org-scheduled-today-face ((t (:foreground "#693c28" :weight bold))))
`(org-special-keyword-face ((t (:foreground "#a45943" :weight bold))))
`(org-table ((t :foreground "7e4c37" :background "#e6dac2" :weight bold))) 
`(org-table-face ((t (:foreground "#7e4c37" :background "#693c28"))))
`(org-tag-face ((t (:foreground "#ec7562" :background "#693c28"))))
`(org-time-grid ((t (:foreground "#ec7562" :background "#693c28"))))
`(org-time-grid-face ((t (:foreground "#1e7fbb"))))
`(org-todo-face ((t (:foreground "#4c260e"))))
`(org-upcoming-deadline-face ((t (:foreground "#fb7362"))))
`(org-warning-face ((t (:foreground "#c16d54"))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'cacao)

;; Local Variables
;; End
;;; cacao-theme.el ends here
