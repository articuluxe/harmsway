;;; custom-themes.el --- customize themes
;; Copyright (C) 2016-2017  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Wednesday, December 21, 2016
;; Version: 1.0
;; Modified Time-stamp: <2017-01-26 13:41:10 dan.harms>
;; Modified by: Dan Harms
;; Keywords: themes colors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setq custom-safe-themes t)
;; (set-face-attribute 'default nil :slant 'italic)
;; solarized
(setq solarized-use-more-italic t)
(setq solarized-distinct-fringe-background t)
(setq solarized-use-variable-pitch nil)
(setq solarized-height-minus-1 1)
(setq solarized-height-plus-1 1)
(setq solarized-height-plus-2 1)
(setq solarized-height-plus-3 1)
(setq solarized-height-plus-4 1)

(defun my/make-all-font-sizes-the-same-please ()
  "Make all faces use the default height."
  (mapc (lambda (face)
          (when (not (equal face 'default))
            (set-face-attribute face nil :height 1.0)))
        (face-list)))

(defun my/make-comment-delimiter-face-same-as-comment-face (sym)
  (custom-theme-set-faces
   sym `(font-lock-comment-delimiter-face
         ((t (:foreground ,(face-attribute
                            'font-lock-comment-face
                            :foreground nil 'default)))))))

(defvar my/sml-dark-themes
  '(jonadabian jonadabian-slate obsidian deeper-blue)
  "List of themes to which should be applied a dark sml theme.")
(defvar my/sml-light-themes
  '(dichromacy)
  "List of themes to which should be applied a light sml theme.")
(defvar my/sml-respectful-themes
  '()
  "List of themes to which should be applied a respectful sml theme.")

(defun my/after-load-theme (x)
  "Run custom code after a theme X is loaded."
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
  (let ((sym (intern x)))
    (when (eq (face-attribute 'diff-hl-insert :background nil t) 'unspecified)
      (custom-theme-set-faces
       sym `(diff-hl-insert ((t (:background "green4" :foreground "green3"))))))
    (when (eq (face-attribute 'diff-hl-delete :background nil t) 'unspecified)
      (custom-theme-set-faces
       sym `(diff-hl-delete ((t (:background "red4" :foreground "red3"))))))
    (cond ((eq sym 'adwaita)
           (setq wg-use-faces nil))
          ((eq sym 'brin)
           (my/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'granger)
           (my/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'ld-dark)
           (my/make-all-font-sizes-the-same-please))
          ((eq sym 'misterioso)
           (custom-theme-set-faces sym '(cursor ((t (:background "#cae682"))))))
          ((eq sym 'obsidian)
           (custom-theme-set-faces sym '(cursor ((t (:background "#e8e2b7")))))
           (custom-theme-set-faces sym '(show-paren-match-face ((t (:background "#678cb1"))))))
          ((eq sym 'wombat)
           (custom-theme-set-faces sym '(cursor ((t (:background "#8ac6f2"))))))
          ((eq sym 'xp)
           (my/make-all-font-sizes-the-same-please))
          )
    (cond ((memq sym my/sml-dark-themes)
           (load-theme 'smart-mode-line-dark t))
          ((memq sym my/sml-light-themes)
           (load-theme 'smart-mode-line-light t))
          ((memq sym my/sml-respectful-themes)
           (load-theme 'smart-mode-line-respectful t))
          )))
(advice-add 'counsel-load-theme-action :after #'my/after-load-theme)

(provide 'custom-themes)
;;; custom-themes.el ends here
