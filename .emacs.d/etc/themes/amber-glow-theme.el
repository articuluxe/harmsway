;;; amber-glow-theme.el --- A warm and inviting theme -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Omer Arif

;; Author: Omer Arif
;; Maintainer: Omer Arif
;; Version: 1.0
;; Package-Requires: ((emacs "24.1"))
;; Homepage: https://github.com/madara123pain/unique-emacs-theme-pack
;; Keywords: faces, theme, warm, amber, glow, dark

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Amber Glow is a warm and inviting Emacs theme designed to reduce eye strain
;; while maintaining high readability. It features a balanced contrast of deep
;; amber and dark tones, perfect for prolonged coding sessions.

;;; Code:

(deftheme amber-glow
  "A warm and inviting amber-themed Emacs theme.")

(custom-theme-set-faces
 'amber-glow

 ;; Basic colors
 '(default ((t (:background "#15130C" :foreground "#EDE6D6"))))
 '(cursor ((t (:background "#EDE6D6"))))
 '(fringe ((t (:background "#15130C"))))
 '(region ((t (:background "#362F21"))))
 '(highlight ((t (:background "#EDE6D6" :foreground "#15130C"))))
 '(vertical-border ((t (:background "#15130C" :foreground "#EDE6D6"))))

 ;; Font lock faces
 '(font-lock-builtin-face ((t (:foreground "#B28E63"))))
 '(font-lock-comment-face ((t (:foreground "#7D6C4B"))))
 '(font-lock-constant-face ((t (:foreground "#D19A66"))))
 '(font-lock-function-name-face ((t (:foreground "#C87850"))))
 '(font-lock-keyword-face ((t (:foreground "#5E3724"))))
 '(font-lock-string-face ((t (:foreground "#93655E"))))
 '(font-lock-type-face ((t (:foreground "#506948"))))
 '(font-lock-variable-name-face ((t (:foreground "#6AC24E"))))
 '(font-lock-warning-face ((t (:bold t :foreground "#EDE6D6"))))

 ;; UI elements
 '(mode-line ((t (:background "#362F21" :foreground "#EDE6D6"))))
 '(mode-line-inactive ((t (:background "#15130C" :foreground "#EDE6D6"))))
 '(minibuffer-prompt ((t (:foreground "#945738")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'amber-glow)

;;; amber-glow-theme.el ends here

