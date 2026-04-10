;;; yotsuba-theme.el --- A color theme inspired by the 4chan aesthetic

;;; If you don't like something, feel free to modify it.
;;; I hope the way I structured the file makes modification easy.

;;; Code

(deftheme yotsuba
  "Theme based on the classic 4chan (Yotsuba) color palette.")

(let ((yotsuba-bg      "#ffffee") ; Page background
      (yotsuba-post    "#f0e0d6") ; Post background
      (yotsuba-subject "#800000") ; Subject/Maroon titles
      (yotsuba-text    "#000000") ; Standard black text
      (yotsuba-green   "#789922") ; Greentext / Quotes
      (yotsuba-link    "#0000ee") ; Blue links
      (yotsuba-name    "#117743") ; Username / Tripcode green
      (yotsuba-border  "#d6bad0") ; Border / Reply highlight
      (yotsuba-red     "#af0a0f") ; Error / Admin red
      (yotsuba-grey    "#444444"))

  (custom-theme-set-faces
   'yotsuba

   ;; Basic Interface
   `(default ((t (:background ,yotsuba-bg :foreground ,yotsuba-text))))
   `(cursor ((t (:background ,yotsuba-subject))))
   `(region ((t (:background ,yotsuba-border))))
   `(fringe ((t (:background ,yotsuba-bg))))
   `(mode-line ((t (:background ,yotsuba-post :foreground ,yotsuba-subject :box (:line-width 1 :color ,yotsuba-border)))))
   `(mode-line-inactive ((t (:background ,yotsuba-bg :foreground ,yotsuba-grey :box (:line-width 1 :color ,yotsuba-post)))))
   `(minibuffer-prompt ((t (:foreground ,yotsuba-subject :weight bold))))
   
   ;; Font-Lock (Syntax Highlighting)
   `(font-lock-comment-face ((t (:foreground ,yotsuba-green)))) ; Comments as Greentext
   `(font-lock-doc-face ((t (:foreground ,yotsuba-green :slant italic))))
   `(font-lock-string-face ((t (:foreground ,yotsuba-name))))    ; Strings as Usernames
   `(font-lock-keyword-face ((t (:foreground ,yotsuba-subject :weight bold)))) ; Keywords as Subject
   `(font-lock-function-name-face ((t (:foreground ,yotsuba-link)))) ; Functions as Links
   `(font-lock-variable-name-face ((t (:foreground ,yotsuba-text))))
   `(font-lock-type-face ((t (:foreground ,yotsuba-red))))
   `(font-lock-constant-face ((t (:foreground ,yotsuba-link :italic t))))
   `(font-lock-warning-face ((t (:foreground ,yotsuba-red :weight bold))))
   `(font-lock-builtin-face ((t (:foreground ,yotsuba-subject))))

   ;; Org-Mode / Markdown
   `(org-level-1 ((t (:foreground ,yotsuba-subject :weight bold :height 1.2))))
   `(org-level-2 ((t (:foreground ,yotsuba-name :weight bold :height 1.1))))
   `(org-link ((t (:foreground ,yotsuba-link :underline t))))
   `(org-block ((t (:background ,yotsuba-post))))
   `(org-quote ((t (:foreground ,yotsuba-green))))
   `(org-document-title ((t (:foreground ,yotsuba-subject :weight bold :height 1.5))))

   ;; Line numbers
   `(line-number ((t (:foreground ,yotsuba-border :background ,yotsuba-bg))))
   `(line-number-current-line ((t (:foreground ,yotsuba-subject :background ,yotsuba-post))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'yotsuba)
