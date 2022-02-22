;;; tok-faces.el --- Face definitions for my theme -*- lexical-binding: t; -*-

;; Copyright (C) 2021, Topi Kettunen <topi@topikettunen.com>

;; Author: Topi Kettunen <topi@topikettunen.com>
;; URL: https://github.com/topikettunen/tok-theme
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

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

;; Face definition for my theme

;;; Code: 

(defvar tok-definition
  '((custom-theme-set-faces
     theme-name
     `(default ((,class (:background ,bg :foreground ,fg))))
     `(fringe ((,class (:background ,bg :foreground ,fg))))
     `(cursor ((,class (:background ,primary))))
     `(region ((,class (:foreground ,fg :background ,region))))
     `(vertical-border ((,class (:foreground ,primary))))
     `(show-paren-match ((,class (:foreground ,fg :background ,primary2))))
     `(hl-line ((,class (:foreground ,fg :background ,hl-line))))
     `(isearch ((,class (:foreground ,fg :background ,primary2))))
     `(link ((,class (:foreground ,primary :underline t))))
     `(mode-line ((,class (:foreground ,bg :background ,primary))))
     `(line-number ((,class (:foreground ,primary2))))
     (when (>= emacs-major-version 29)
       `(mode-line-active ((,class :inherit mode-line))))
     `(mode-line-inactive ((,class (:foreground ,primary4 :background ,primary2))))
     (when (>= emacs-major-version 26)
       `(line-number-current-line ((,class (:foreground ,primary :background ,primary4)))))
     `(org-block ((,class (:foreground ,fg :extend t :inherit (fixed-pitch shadow)))))
     `(org-block-begin-line ((,class (:inherit shadow))))
     `(org-block-end-line ((,class (:inherit shadow))))
     `(org-headline-done ((,class (:foreground ,comment))))
     `(org-document-title ((,class (:foreground ,fg))))
     `(org-drawer ((,class (:inherit shadow))))
     `(org-link ((,class (:foreground ,primary :underline t))))
     ;; disabled faces
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
     `(font-lock-doc-face ((t nil)))
     `(font-lock-comment-face ((t nil)))
     `(font-lock-string-face ((t nil)))
     `(sh-heredoc ((t nil)))
     '(terraform--resource-name-face ((t nil)))
     '(terraform--resource-type-face ((t nil))))))

(provide 'tok-faces)

;;; tok-faces.el ends here
