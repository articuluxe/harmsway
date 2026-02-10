;;; royal-hemlock-theme-mono-theme.el --- Soothing monotone-ish light-theme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 vs-123
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Author: vs-123 (GitHub)
;; Homepage: https://github.com/vs-123/royal-hemlock-theme
;; Url: https://github.com/vs-123/royal-hemlock-theme
;; Version: 1.5
;; Filename: royal-hemlock-theme-mono-theme.el
;; Keywords: color theme faces
;; Package-License: AGPL-3.0-or-later

;;; Commentary:

;; A variant of Royal Hemlock Theme, using a monotone-ish colour-scheme.
;; The theme is accompanied by a warm white background for a soothing
;; experience

;;; Code:

(deftheme royal-hemlock-theme-mono "Mono Hemlock Theme -- by vs-123.")

(let ((class '((class color) (min-colors 24)))
      (mono-hemlock/black                 "#000000")
      (mono-hemlock/grey                  "#AAAAAA")
      (mono-hemlock/white                 "#FFFFFF")
      
      (mono-hemlock/background            "#FFFFEE")
      (mono-hemlock/hl-line-background    "#F2F2E1")
      (mono-hemlock/isearch-background    "#E1EAEA")
      (mono-hemlock/highlight-background  "#EFF9F9")
      (mono-hemlock/text-colour           "#3F3F3F")
      )

  (custom-theme-set-faces
   'royal-hemlock-theme-mono

   `(cursor  ((,class (:foreground ,mono-hemlock/white :background ,mono-hemlock/black))))
   `(default ((,class (:foreground ,mono-hemlock/text-colour :background ,mono-hemlock/background))))
   `(hl-line ((,class (:background ,mono-hemlock/hl-line-background))))
   `(isearch ((,class (:foreground ,mono-hemlock/isearch-background :background ,mono-hemlock/black))))
   `(region  ((,class (:foreground ,mono-hemlock/highlight-background :background ,mono-hemlock/black))))
   `(lazy-highlight  ((,class (:background ,mono-hemlock/isearch-background :foreground ,mono-hemlock/black :underline t))))
   `(match  ((,class (:foreground ,mono-hemlock/isearch-background :background ,mono-hemlock/black))))

   `(mode-line         ((,class (:foreground ,mono-hemlock/white :background ,mono-hemlock/text-colour :weight bold))))
   `(minibuffer-prompt ((,class (:foreground ,mono-hemlock/text-colour :weight bold))))

   `(font-lock-builtin-face ((,class (   :foreground ,mono-hemlock/text-colour :weight bold))))
   `(font-lock-constant-face ((,class (  :foreground ,mono-hemlock/text-colour :weight bold))))
   `(font-lock-keyword-face ((,class (   :foreground ,mono-hemlock/text-colour :weight bold))))
   `(font-lock-type-face ((,class (      :foreground ,mono-hemlock/text-colour :weight bold))))

   `(font-lock-function-name-face ((,class (  :foreground ,mono-hemlock/text-colour))))
   `(font-lock-string-face((,class (          :foreground ,mono-hemlock/text-colour))))
   `(font-lock-variable-name-face ((,class (  :foreground ,mono-hemlock/text-colour))))
   `(font-lock-comment-face((,class (         :foreground ,mono-hemlock/text-colour   :weight bold))))
   `(font-lock-number-face((,class (          :foreground ,mono-hemlock/text-colour   :weight bold))))
   `(font-lock-escape-face((,class (          :foreground ,mono-hemlock/text-colour   :weight bold))))   

   `(line-number((,class (:foreground ,mono-hemlock/grey :background ,mono-hemlock/background :inherit 'default))))
   `(line-number-current-line((,class (   :foreground ,mono-hemlock/text-colour :background ,mono-hemlock/background :weight bold :inherit 'default))))

   `(link ((,class (:foreground ,mono-hemlock/text-colour :underline t :weight bold))))))

;;;###autoload
(add-to-list
  'custom-theme-load-path
  (if load-file-name (file-name-directory load-file-name) default-directory))

(provide-theme 'royal-hemlock-theme-mono)
(provide 'royal-hemlock-theme-mono)
;;; royal-hemlock-theme-mono-theme.el ends here
