;;; purp-common.el --- simple themes with few colors

;; Author: Vincent Foley <vfoley@gmail.com>
;; URL: https://github.com/gnuvince/purp
;; Version: 1.0
;; Keywords: faces

;;; Commentary:
;;; - mktheme: a function that abstracts away the actual colors

;;; Code:

(defun purp-common-mktheme (theme-name &rest colors)
  "Create a purp theme.
THEME-NAME is the of the variant, COLORS is a plist of the colors to use."
  (let* (
         ;; The four main text colors (excluding special handling)
         (default-fg  (plist-get colors :default-fg))
         (string-fg   (plist-get colors :string-fg))
         (function-fg (plist-get colors :function-fg))
         (comment-fg  (plist-get colors :comment-fg))

         (default-bg  (plist-get colors :default-bg))
         (bright-bg   (plist-get colors :bright-bg))
         (inactive-bg (plist-get colors :inactive-bg))

         (error-fg    (plist-get colors :error-fg))
         (error-bg    (plist-get colors :error-bg))

         ;; special cases
         (hl-line-bg  (plist-get colors :hl-line-bg))
         )
    (custom-theme-set-faces
     theme-name
     `(default                             ((t (:background ,default-bg :foreground ,default-fg))))
     `(region                              ((t (:background ,bright-bg))))
     `(cursor                              ((t (:background ,default-fg))))

     `(font-lock-builtin-face              ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-comment-delimiter-face    ((t (:background ,default-bg :foreground ,comment-fg))))
     `(font-lock-comment-face              ((t (:background ,default-bg :foreground ,comment-fg))))
     `(font-lock-constant-face             ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-doc-face                  ((t (:background ,default-bg :foreground ,comment-fg))))
     `(font-lock-function-name-face        ((t (:background ,default-bg :foreground ,function-fg))))
     `(font-lock-keyword-face              ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-negation-char-face        ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-preprocessor-face         ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-regexp-grouping-backslash ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-regexp-grouping-construct ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-string-face               ((t (:background ,default-bg :foreground ,string-fg))))
     `(font-lock-type-face                 ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-variable-name-face        ((t (:background ,default-bg :foreground ,default-fg))))
     `(font-lock-warning-face              ((t (:weight bold :background ,error-bg :foreground ,error-fg))))

     `(mode-line                           ((t (:foreground ,default-fg :background ,bright-bg))))
     `(mode-line-inactive                  ((t (:foreground ,default-fg :background ,inactive-bg))))

     `(fringe                              ((t (:background ,default-bg))))

     ;; isearch
     `(lazy-highlight                      ((t (:background ,bright-bg))))

     `(hl-line                             ((t (:background ,hl-line-bg))))

     `(show-paren-mismatch                 ((t (:background ,error-bg))))
     `(show-paren-match                    ((t (:background ,bright-bg))))
     )))

(provide 'purp-common)

;;; purp-common.el ends here
