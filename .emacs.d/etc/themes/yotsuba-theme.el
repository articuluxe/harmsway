;;; yotsuba-theme.el --- A color theme inspired by the 4chan aesthetic

;;; If you don't like something, feel free to modify it.
;;; I hope the way I structured the file makes modification easy.

;;; Code

(deftheme yotsuba
  "Theme based on the classic 4chan (Yotsuba) color palette.")

(let ((yb-bg      "#ffffee") ; Page background
      (yb-post    "#f0e0d6") ; Post background
      (yb-subject "#800000") ; Subject/Maroon titles
      (yb-text    "#000000") ; Standard black text
      (yb-green   "#789922") ; Greentext / Quotes
      (yb-link    "#0000ee") ; Blue links
      (yb-name    "#117743") ; Username / Tripcode green
      (yb-border  "#d6bad0") ; Border / Reply highlight
      (yb-red     "#af0a0f") ; Error / Admin red
      (yb-grey    "#444444"))

  (custom-theme-set-faces
   'yotsuba

   ;; Basic Interface
   `(default ((t (:background ,yb-bg :foreground ,yb-text))))
   `(cursor ((t (:background ,yb-subject))))
   `(region ((t (:background ,yb-border))))
   `(fringe ((t (:background ,yb-bg))))
   `(mode-line ((t (:background ,yb-post :foreground ,yb-subject :box (:line-width 1 :color ,yb-border)))))
   `(mode-line-inactive ((t (:background ,yb-bg :foreground ,yb-grey :box (:line-width 1 :color ,yb-post)))))
   `(minibuffer-prompt ((t (:foreground ,yb-subject :weight bold))))

   ;; Font-Lock (Syntax Highlighting)
   `(font-lock-comment-face ((t (:foreground ,yb-green)))) ; Comments as Greentext
   `(font-lock-doc-face ((t (:foreground ,yb-green :slant italic))))
   `(font-lock-string-face ((t (:foreground ,yb-name))))    ; Strings as Usernames
   `(font-lock-keyword-face ((t (:foreground ,yb-subject :weight bold)))) ; Keywords as Subject
   `(font-lock-function-name-face ((t (:foreground ,yb-link)))) ; Functions as Links
   `(font-lock-variable-name-face ((t (:foreground ,yb-text))))
   `(font-lock-type-face ((t (:foreground ,yb-red))))
   `(font-lock-constant-face ((t (:foreground ,yb-link :italic t))))
   `(font-lock-warning-face ((t (:foreground ,yb-red :weight bold))))
   `(font-lock-builtin-face ((t (:foreground ,yb-subject))))

   ;; Org-Mode / Markdown
   `(org-level-1 ((t (:foreground ,yb-subject :weight bold :height 1.2))))
   `(org-level-2 ((t (:foreground ,yb-name :weight bold :height 1.1))))
   `(org-link ((t (:foreground ,yb-link :underline t))))
   `(org-block ((t (:background ,yb-post))))
   `(org-quote ((t (:foreground ,yb-green))))
   `(org-document-title ((t (:foreground ,yb-subject :weight bold :height 1.5))))

   ;; Line numbers
   `(line-number ((t (:background ,yb-bg :foreground ,yb-border))))
   `(line-number-current-line ((t (:background ,yb-bg :foreground ,yb-subject :weight bold))))

   ;; tab-bar-mode
   `(tab-bar ((t (:inherit default :background ,yb-post :foreground ,yb-grey))))
   `(tab-bar-tab ((t (:background ,yb-bg :foreground ,yb-subject))))
   `(tab-bar-tab-inactive ((t (:background ,yb-post :foreground ,yb-grey))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'yotsuba)
