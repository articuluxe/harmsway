;;; yotsuba-b-theme.el --- A color theme inspired by the 4chan Yotsuba B aesthetic

;;; If you don't like something, feel free to modify it.
;;; I hope the way I structured the file makes modification easy.

;;; Code:

(deftheme yotsuba-b
  "Theme based on the classic 4chan (Yotsuba B) blue color palette.")

(let ((yb-bg      "#d1d5ee") ; Page background (Pale Blue/Grey)
      (yb-post    "#d6daf0") ; Post background
      (yb-subject "#0f0c5d") ; Subject/Dark blue-purple titles
      (yb-text    "#000000") ; Standard black text
      (yb-green   "#789922") ; Greentext / Quotes
      (yb-link    "#34345c") ; Blue links (Deep navy)
      (yb-name    "#117743") ; Username / Tripcode green
      (yb-border  "#b7c5d9") ; Border / Reply highlight
      (yb-red     "#af0a0f") ; Error / Admin red
      (yb-grey    "#444444"))

  (custom-theme-set-faces
   'yotsuba-b

   ;; Basic Interface
   `(default ((t (:background ,yb-bg :foreground ,yb-text))))
   `(cursor ((t (:background ,yb-subject))))
   `(region ((t (:background ,yb-border))))
   `(fringe ((t (:background ,yb-bg))))
   `(mode-line ((t (:background ,yb-post :foreground ,yb-subject :box (:line-width 1 :color ,yb-border)))))
   `(mode-line-inactive ((t (:background ,yb-bg :foreground ,yb-grey :box (:line-width 1 :color ,yb-post)))))
   `(minibuffer-prompt ((t (:foreground ,yb-subject :weight bold))))
   
   ;; Font-Lock (Syntax Highlighting)
   `(font-lock-comment-face ((t (:foreground ,yb-green)))) 
   `(font-lock-doc-face ((t (:foreground ,yb-green :slant italic))))
   `(font-lock-string-face ((t (:foreground ,yb-name))))   
   `(font-lock-keyword-face ((t (:foreground ,yb-subject :weight bold)))) 
   `(font-lock-function-name-face ((t (:foreground ,yb-link)))) 
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

   ;; Line numbers
   `(line-number ((t (:foreground ,yb-border :background ,yb-bg))))
   `(line-number-current-line ((t (:foreground ,yb-subject :background ,yb-post))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'yotsuba-b)

;;; yotsuba-b-theme.el ends here
