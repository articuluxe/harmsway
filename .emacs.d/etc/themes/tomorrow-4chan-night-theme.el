;;; tomorrow-4chan-night-theme.el --- Dark aesthetic based on 4chan's dark mode

;;; Commentary:
;; A dark theme inspired by the Tomorrow Night palette used on 4chan.

;;; Code:

(deftheme tomorrow-4chan-night
  "Dark theme based on the Tomorrow Night 4chan aesthetic.")

(let ((bg      "#1d1f21") ; Main background
      (bg-alt  "#282a2e") ; Post background
      (fg      "#c5c8c6") ; Main text
      (gray    "#969896") ; UI and comments
      (red     "#cc6666") ; Constants and IDs
      (green   "#b5bd68") ; Greentext
      (blue    "#81a2be") ; Headers and links
      (border  "#373b41")) ; Selection and borders

  (custom-theme-set-faces
   'tomorrow-4chan-night

   ;; Basic UI
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,fg))))
   `(fringe ((t (:background ,bg))))
   `(mode-line ((t (:background ,blue :foreground ,bg :box nil))))
   `(mode-line-inactive ((t (:background ,bg-alt :foreground ,gray :box nil))))
   `(header-line ((t (:background ,bg-alt :foreground ,blue :weight bold))))
   `(vertical-border ((t (:foreground ,border))))

   ;; Syntax Highlighting
   `(font-lock-builtin-face ((t (:foreground ,blue))))
   `(font-lock-comment-face ((t (:foreground ,green :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,red))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,blue :weight bold))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,blue))))
   `(font-lock-variable-name-face ((t (:foreground ,red))))
   `(font-lock-warning-face ((t (:foreground ,red :weight bold))))

   ;; Search and Selection
   `(region ((t (:background ,border))))
   `(isearch ((t (:background ,red :foreground ,bg))))
   `(lazy-highlight ((t (:background ,border :foreground ,fg))))

   ;; Markdown / Org Mode
   `(org-level-1 ((t (:foreground ,blue :weight bold :height 1.2))))
   `(org-link ((t (:foreground ,blue :underline t))))
   `(org-quote ((t (:foreground ,green))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'tomorrow-4chan-night)
;;; tomorrow-4chan-night-theme.el ends here
