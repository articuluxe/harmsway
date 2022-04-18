;;; tok-theme.el --- Minimal theme with dark and yellow color scheme  -*- lexical-binding: t; -*-

;; Copyright (C) 2022, Topi Kettunen <topi@topikettunen.com>

;; Author: Topi Kettunen <topi@topikettunen.com>
;; URL: https://github.com/topikettunen/tok-theme
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; Tok is a simple and minimal Emacs theme with dark and yellow color scheme.

;;; Code:

(deftheme tok
  "Minimal Emacs theme with yellow and green color scheme")

(let* ((class '((class color) (min-colors 89)))
       ;; Color palette
       (butter-1 "#ffeb8f") (butter-2 "#ccbc72") (butter-3 "#998d56")
       (butter-4 "#665e39") (butter-5 "#332f1d") (dark-grey "#282828"))
  (custom-theme-set-faces
   'tok
   ;; Basic faces
   `(default ((,class (:background ,dark-grey :foreground ,butter-1))))
   `(fringe ((,class (:background ,dark-grey :foreground ,butter-1))))
   `(cursor ((,class (:background ,butter-1))))
   `(region ((,class (:foreground ,butter-1 :background ,butter-4))))
   `(show-paren-match ((,class (:background ,butter-3))))
   `(hl-line ((,class (:foreground ,butter-1 :background ,butter-4))))
   `(isearch ((,class (:foreground ,butter-1 :background ,butter-2))))
   `(link ((,class (:underline t :foreground "Cyan"))))
   `(link-visited ((,class (:underline t :foreground "DarkCyan"))))
   `(mode-line ((,class (:foreground ,dark-grey :background ,butter-2))))
   `(mode-line-inactive ((,class (:foreground ,butter-3 :background ,butter-5))))
   `(line-number ((,class (:foreground ,butter-5))))
   `(line-number-current-line ((,class (:foreground ,butter-1 :background ,butter-4))))
   `(error ((,class (:foreground "Red1"))))
   `(warning ((,class (:foreground "DarkOrange2"))))
   `(success ((,class (:foreground "ForestGreen"))))
   ;; Font lock faces
   `(font-lock-comment-face ((t nil)))
   `(font-lock-doc-face ((t nil)))
   `(font-lock-string-face ((t nil)))
   `(font-lock-builtin-face ((t nil)))
   `(font-lock-keyword-face ((t nil)))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-reference-face ((t nil)))
   `(font-lock-constant-face ((t nil)))
   `(font-lock-function-name-face ((t nil)))
   `(font-lock-type-face ((t nil)))
   `(font-lock-variable-name-face ((t nil)))
   `(font-lock-warning-face ((t nil)))
   `(font-lock-preprocessor-face ((t nil)))
   ;; Shell script faces
   `(sh-heredoc ((t nil)))
   ;; Org faces
   `(org-block ((,class (:foreground ,butter-1 :extend t :inherit (fixed-pitch shadow)))))
   `(org-block-begin-line ((,class (:foreground ,butter-3))))
   `(org-block-end-line ((,class (:inherit org-block-begin-line))))
   `(org-code ((,class (:foreground ,butter-3))))
   `(org-headline-done ((,class (:foreground ,butter-3))))
   `(org-document-title ((,class (:foreground ,butter-1))))
   `(org-drawer ((,class (:foreground ,butter-3))))
   `(org-link ((,class (:foreground ,butter-1 :underline t))))
   `(org-date ((,class (:inherit (fixed-pitch link)))))
   ;; Terraform faces
   '(terraform--resource-name-face ((t nil)))
   '(terraform--resource-type-face ((t nil)))))

(provide-theme 'tok)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tok-theme.el ends here
