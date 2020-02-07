;;; qtcreator-theme.el --- A color theme that mimics Qt Creator IDE
;;
;; Copyright (C) 2020 Lesley Lai
;;
;; Author: Lesley Lai <lesley@lesleylai.info>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: theme light faces
;; URL: https://github.com/LesleyLai/emacs-qtcreator-theme
;;
;; This file is not part of GNU Emacs.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;;; Commentary:
;;
;; Qt Creator Theme is a color theme that mimics the default color
;; scheme of the Qt Creator IDE.
;;
;;; Credits:
;;
;; Lesley Lai came up with the color scheme.
;;
;;; Code:


(deftheme qtcreator
  "Mimics the Qt Creator's default color theme.")

(defgroup qtcreator-theme nil
  "Qtcreator theme options. Reload the theme after changing to see effect."
  :group 'faces)

(unless (>= emacs-major-version 24)
  (error "The qtcreator-theme requires Emacs 24 or later"))

(let ((class '((class color) (min-colors 89)))
      (white "#FFFFFF") (black "#000000")

      (bg-0 "#EFEFEF")
      (fg-0 "#ABABAB") (fg-1 "#888888")
      (red-0 "#FE0000")
      (orange-1 "#ce5c00") (orange-2 "#b35000") (orange-3 "#EFC846")
      (yellow-0 "#FEEE0B") (yellow-1 "#808000")
      (green-0 "#008000") (green-1 "#B4EDB3")
      (blue-0 "#2D83DE") (blue-1 "#0000FE") (blue-2 "#000080")
      (cyan-1 "#00677C")
      (purple-0 "#800080"))

  (custom-theme-set-faces
   'qtcreator

   ;; Built-in
   ;; basic coloring
   `(default ((t (:foreground , black :background , white))))
   `(fringe ((,class (:background ,bg-0))))
   `(region ((t (:foreground ,white :background ,blue-0))))
   `(isearch ((,class (:foreground ,white :background ,orange-1))))
   `(lazy-highlight ((,class (:background ,yellow-0))))
   `(show-paren-match ((,t (:background ,green-1 :foreground ,red-0))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,t (:foreground ,yellow-1))))
   `(font-lock-comment-face ((,t (:foreground ,green-0))))
   `(font-lock-comment-delimiter-face((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((,t (:foreground ,blue-2))))
   `(font-lock-doc-face ((,class (:foreground ,blue-2))))
   `(font-lock-function-name-face ((,class (:foreground ,cyan-1))))
   `(font-lock-keyword-face ((,class (:foreground ,yellow-1))))
   `(font-lock-string-face ((,class (:foreground ,green-0))))
   `(font-lock-type-face ((,class (:foreground ,purple-0))))
   `(font-lock-preprocessor-face ((,class (:foreground ,blue-2))))
   `(font-lock-variable-name-face ((,class (:foreground ,orange-2))))
   `(font-lock-warning-face ((,t (:foreground ,red-0 :weight bold))))

   ;; Link faces
   `(link ((,class (:underline t :foreground ,blue-1))))
   ;; `(link-visited ((,class (:underline t :foreground ,blue-2))))

   ;; Line-number-mode
   `(line-number ((,class (:inherit default :background ,bg-0 :foreground ,fg-0))))
   `(line-number-current-line ((t :inherit line-number :weight bold :foreground ,fg-1)))

   ;; merlin
   `(merlin-eldoc-occurrences-face((t (:inherit idle-highlight-face))))

   ;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,red-0) :inherit unspecified))
      (t (:foreground ,red-0 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,orange-3) :inherit unspecified))
      (t (:foreground ,orange-3 :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,green-0) :inherit unspecified))
      (t (:foreground ,green-0 :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,red-0 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,orange-3 :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,green-0 :weight bold))))

  ;; Highlight doxygen mode
  `(highlight-doxygen-comment ((t (:inherit font-lock-doc-face))))
  `(highlight-doxygen-command ((t (:foreground ,blue-1 :weight bold)))))


  (custom-theme-set-variables
   'qtcreator
   `(git-gutter:modified-sign "✱")))

;;----------------------------------------------------------------------------


;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list
   'custom-theme-load-path
   (file-name-directory load-file-name)))
(provide-theme 'qtcreator)

(provide 'qtcreator-theme)
;;; qtcreator-theme.el ends here
