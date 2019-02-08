;;; custom-themes.el --- customize themes
;; Copyright (C) 2016-2019  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Wednesday, December 21, 2016
;; Modified Time-stamp: <2019-02-08 16:14:50 dan.harms>
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
;; Personal theme customizations.
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
;; kaolin customization
(setq kaolin-italic-comments t)

(defun harmsway/make-all-font-sizes-the-same-please ()
  "Make all faces use the default height."
  (mapc (lambda (face)
          (when (not (equal face 'default))
            (set-face-attribute face nil :height 1.0)))
        (face-list)))

(defun harmsway/make-comment-delimiter-face-same-as-comment-face (sym)
  (custom-theme-set-faces
   sym `(font-lock-comment-delimiter-face
         ((t (:foreground ,(face-attribute
                            'font-lock-comment-face
                            :foreground nil 'default)))))))

(defvar harmsway/sml-dark-themes
  '(jonadabian jonadabian-slate obsidian deeper-blue)
  "List of themes to which should be applied a dark sml theme.")
(defvar harmsway/sml-light-themes
  '(dichromacy nubox-light ample-light
               gruvbox-light-hard gruvbox-light-medium gruvbox-light-soft)
  "List of themes to which should be applied a light sml theme.")
(defvar harmsway/sml-respectful-themes
  '()
  "List of themes to which should be applied a respectful sml theme.")

(defun harmsway/after-load-theme (x)
  "Run custom code after a theme X is loaded."
  (setq zoom-window-mode-line-color (face-attribute 'mode-line :background nil 'default))
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
  (set-face-attribute 'comint-highlight-prompt nil :inherit nil) ;shell mode prompt
  (let ((sym (intern x)))
    (when (or (eq (face-attribute 'diff-hl-insert :background nil t) 'unspecified)
              (eq (frame-parameter nil 'background-mode) 'light))
      (custom-theme-set-faces
       sym `(diff-hl-insert ((t (:background "green4" :foreground "green3"))))))
    (when (or (eq (face-attribute 'diff-hl-delete :background nil t) 'unspecified)
              (eq (frame-parameter nil 'background-mode) 'light))
      (custom-theme-set-faces
       sym `(diff-hl-delete ((t (:background "red4" :foreground "red3"))))))
    (when (or (eq (face-attribute 'diff-hl-change :background nil t) 'unspecified)
              (eq (frame-parameter nil 'background-mode) 'light))
      (custom-theme-set-faces
       sym `(diff-hl-change ((t (:background "DodgerBlue2" :foreground "DodgerBlue1"))))))
    (cond ((eq sym 'adwaita)
           (setq wg-use-faces nil))
          ((eq sym 'Amelie)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((memq sym '(ample ample-light ample-flat))
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'brin)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'busybee)
           (custom-theme-set-faces sym '(cursor ((t (:background "LightYellow"))))))
          ((eq sym 'deep-blue)
           (custom-theme-set-faces sym '(show-paren-match-face ((t (:bold t :background "coral")))))
           (custom-theme-set-faces sym '(show-paren-mismatch-face ((t (:bold t :background "VioletRed4"))))))
          ((eq sym 'desert)
           (custom-theme-set-faces sym '(show-paren-match-face ((t (:bold t
                                                                          :foreground "khaki"
                                                                          :background "OliveDrab")))))
           (custom-theme-set-faces sym '(show-paren-mismatch-face ((t (:bold t
                                                                             :foreground "yellow2"
                                                                             :background "IndianRed3")))))
           (custom-theme-set-faces sym '(mode-line-inactive ((t (:inherit header-line
                                                                          :foreground "gray14"
                                                                          :background "gray6")))))
           (custom-theme-set-faces sym '(magit-section-highlight ((t (:background "grey23"))))))
          ((eq sym 'distinguished)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'eltbus)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'evenhold)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'faff)
           (custom-theme-set-faces sym '(region ((t (:background "LightGoldenrod2"))) t)))
          ((eq sym 'farmhouse-dark)
           (custom-theme-set-faces sym '(iedit-occurrence ((t (:inherit region))))))
          ((eq sym 'foggy-night)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow"))))))
          ((eq sym 'granger)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'green-is-the-new-black)
           (custom-theme-set-faces sym '(show-paren-match ((t (:bold t :background "#218c23")))))
           (custom-theme-set-faces sym '(show-paren-mismatch ((t (:bold t :background "#f47a47")))))
           (custom-theme-set-faces sym '(cursor ((t (:background "GreenYellow"))))))
          ((eq sym 'hamburg)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow"))))))
          ((eq sym 'hober2)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'idea-darkula)
           (custom-theme-set-faces sym '(mode-line ((t (:background "#4A6739")))))
           (custom-theme-set-faces sym '(mode-line-inactive ((t (:background "#3c3f41"))))))
          ((eq sym 'iodine)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((memq sym '(kaolin-dark kaolin-mono-dark kaolin-light kaolin-eclipse kaolin-galaxy
                                   kaolin-ocean kaolin-aurora kaolin-valley-dark kaolin-valley-light
                                   kaolin-bubblegum kaolin-fusion kaolin-breeze))
           (custom-theme-set-faces sym `(show-paren-match-face
                                         ((t (:bold t :background
                                                    ,(cadr (assoc 'teal1 kaolin-palette))))))))
          ((eq sym 'klere)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow3"))))))
          ((eq sym 'ld-dark)
           (harmsway/make-all-font-sizes-the-same-please))
          ((eq sym 'lop)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow2"))))))
          ((eq sym 'lush)
           (custom-theme-set-faces sym '(iedit-occurrence ((t (:inherit region))))))
          ((eq sym 'mandm)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'manoj-dark)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym)
           (harmsway/make-all-font-sizes-the-same-please))
          ((eq sym 'misterioso)
           (custom-theme-set-faces sym '(cursor ((t (:background "#cae682"))))))
          ((eq sym 'obsidian)
           (custom-theme-set-faces sym '(cursor ((t (:background "#e8e2b7")))))
           (custom-theme-set-faces sym '(show-paren-match-face ((t (:background "#678cb1"))))))
          ((eq sym 'phoenix-dark-pink)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'railscast)
           (custom-theme-set-faces sym '(cursor ((t (:background "#FFC66D"))))))
          ((eq sym 'soft-charcoal)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow"))))))
          ((eq sym 'soothe)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'sourcerer)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow3"))))))
          ((eq sym 'srcery)
           (custom-theme-set-faces sym '(show-paren-match ((t (:background "#FF8700" :foreground "#FED06E")))))
           (custom-theme-set-faces sym '(region ((t (:background "tomato4")))))
           )
          ((eq sym 'subatomic)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'tangotango)
           (custom-theme-set-faces sym '(magit-section-highlight ((t (:background "grey23"))))))
          ((eq sym 'tao-yang)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'tao-yin)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'underwater)
           (custom-theme-set-faces sym '(show-paren-match ((t (:background "royal blue"))))))
          ((eq sym 'warm-night)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow"))))))
          ((eq sym 'weyland-yutani)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow"))))))
          ((eq sym 'white-sand)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow"))))))
          ((eq sym 'wombat)
           (custom-theme-set-faces sym '(cursor ((t (:background "#8ac6f2")))))
           ;; switch isearch and lazy-highlight
           (custom-theme-set-faces sym '(isearch ((t (:background "#384048" :foreground "#a0a8b0")))))
           (custom-theme-set-faces sym '(lazy-highlight ((t (:background "#343434" :foreground "#857b6f"))))))
          ((eq sym 'xp)
           (harmsway/make-all-font-sizes-the-same-please)
           (custom-theme-set-faces sym '(fringe ((t :background "LightYellow2"))))
           (custom-theme-set-faces sym '(mode-line-inactive ((t :background "LightYellow1"
                                                                :foreground "cadetblue"
                                                                :box (:line-width -1 :color "grey75")
                                                                :slant oblique
                                                                :weight light))))
           (custom-theme-set-faces sym '(mode-line ((t (:background "LightBlue3"
                                                                    :foreground "black"
                                                                    :box (:line-width 1 :style released-button))))))
           (custom-theme-set-faces sym '(header-line ((t (:foreground "grey20"
                                                                      :background "LightGoldenrod")))))
           (custom-theme-set-faces sym '(magit-section-highlight ((t (:background "PaleGoldenrod"))))))
          )
    (cond ((memq sym harmsway/sml-dark-themes)
           (load-theme 'smart-mode-line-dark t))
          ((memq sym harmsway/sml-light-themes)
           (load-theme 'smart-mode-line-light t))
          ((memq sym harmsway/sml-respectful-themes)
           (load-theme 'smart-mode-line-respectful t))
          )))
(advice-add 'counsel-load-theme-action :after #'harmsway/after-load-theme)

(provide 'custom-themes)
;;; custom-themes.el ends here
