;;; royal-hemlock-theme.el --- Soothing royal-blue light-theme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 vs-123
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Author: vs-123 (GitHub)
;; Homepage: https://github.com/vs-123/royal-hemlock-theme
;; Url: https://github.com/vs-123/royal-hemlock-theme
;; Version: 1.0
;; Filename: royal-hemlock-theme.el
;; Keywords: color theme faces
;; Package-Requires: ((emacs "24"))
;; Package-License: AGPL-3.0-or-later

;;; Commentary:

;; A light-theme with the core colour being Royal Blue for Emacs 24+.
;; The theme is accompanied by a warm white background for a soothing
;; experience

;;; Code:

(deftheme royal-hemlock "Royal Hemlock Theme -- by vs-123.")

(let ((class '((class color) (min-colors 24)))
      (royal-hemlock/black                 "#000000")
      (royal-hemlock/grey                  "#AAAAAA")
      (royal-hemlock/white                 "#FFFFFF")
      
      (royal-hemlock/background            "#FFFFED")
      (royal-hemlock/hl-line-background    "#F2F2E1")
      (royal-hemlock/cursor                "#BFFFFF")
      (royal-hemlock/highlight-background  "#3FFFFF")
      (royal-hemlock/majestic-hemlock      "#0000BB")
      (royal-hemlock/string                "#007F00")
      (royal-hemlock/text-colour           "#3F3F3F")
      (royal-hemlock/pink-colour           "#FC55C2")
      )

  (custom-theme-set-faces
   'royal-hemlock

   `(cursor  ((,class (:foreground ,royal-hemlock/white       :background ,royal-hemlock/black))))
   `(default ((,class (:foreground ,royal-hemlock/text-colour :background ,royal-hemlock/background))))
   `(hl-line ((,class (:background ,royal-hemlock/hl-line-background))))   
   `(isearch ((,class (:foreground ,royal-hemlock/black       :background ,royal-hemlock/highlight-background :weight bold))))
   `(region  ((,class (:foreground ,royal-hemlock/black       :background ,royal-hemlock/cursor))))


   `(mode-line         ((,class (:foreground ,royal-hemlock/white            :background ,royal-hemlock/majestic-hemlock :weight bold))))
   `(minibuffer-prompt ((,class (:foreground ,royal-hemlock/majestic-hemlock :weight bold))))

   `(font-lock-builtin-face       ((,class (:foreground ,royal-hemlock/majestic-hemlock :weight bold))))
   `(font-lock-constant-face      ((,class (:foreground ,royal-hemlock/pink-colour    :weight bold))))      
   `(font-lock-keyword-face       ((,class (:foreground ,royal-hemlock/majestic-hemlock :weight bold))))
   `(font-lock-type-face          ((,class (:foreground ,royal-hemlock/majestic-hemlock :weight bold))))
   
   `(font-lock-comment-face       ((,class (:foreground ,royal-hemlock/grey))))
   `(font-lock-function-name-face ((,class (:foreground ,royal-hemlock/text-colour))))
   `(font-lock-string-face        ((,class (:foreground ,royal-hemlock/string))))
   `(font-lock-variable-name-face ((,class (:foreground ,royal-hemlock/black))))
   `(font-lock-number-face        ((,class (:foreground ,royal-hemlock/pink-colour    :weight bold))))
   `(font-lock-escape-face        ((,class (:foreground ,royal-hemlock/majestic-hemlock :weight bold))))   

   `(line-number              ((,class (:foreground ,royal-hemlock/grey             :background ,royal-hemlock/background :inherit 'default))))
   `(line-number-current-line ((,class (:foreground ,royal-hemlock/majestic-hemlock :background ,royal-hemlock/background :weight bold :inherit 'default))))



   `(link ((,class (:foreground ,royal-hemlock/majestic-hemlock :underline t :weight bold))))))

(provide-theme 'royal-hemlock)

;;;###autoload
(add-to-list
  'custom-theme-load-path
  (if load-file-name (file-name-directory load-file-name) default-directory))

;;; royal-hemlock-theme.el ends here
