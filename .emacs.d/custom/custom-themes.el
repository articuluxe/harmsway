;;; custom-themes.el --- customize themes
;; Copyright (C) 2016-2020  Dan Harms (dan.harms)
;; Author: Dan Harms <dan.harms@xrtrading.com>
;; Created: Wednesday, December 21, 2016
;; Modified Time-stamp: <2020-02-19 10:40:19 dan.harms>
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
           (custom-theme-set-faces sym '(cursor ((t (:background "DarkOrange")))))
           (setq wg-use-faces nil))
          ((eq sym 'Amelie)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((memq sym '(ample ample-light ample-flat))
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'anti-zenburn)
           (custom-theme-set-faces sym '(lazy-highlight ((t (:background "#b0b0b0" :foreground "#2f4070")))))
           (custom-theme-set-faces sym '(region ((t (:background "#b6b6b6"))) t))
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((memq sym '(base16-horizon-dark base16-horizon-terminal-dark
                                           base16-ia-dark base16-material-palenight
                                           base16-woodland))
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
          ((memq sym '(eziam-light eziam-dark eziam-dusk))
           (custom-theme-set-faces sym '(show-paren-match ((t (:bold t :background "#AAAAAA")))))
           (custom-theme-set-faces sym '(show-paren-mismatch ((t (:bold t :background "#ff0000"))))))
          ((eq sym 'faff)
           (custom-theme-set-faces sym '(region ((t (:background "LightGoldenrod2"))) t)))
          ((eq sym 'farmhouse-dark)
           (custom-theme-set-faces sym '(iedit-occurrence ((t (:inherit region))))))
          ((eq sym 'foggy-night)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow"))))))
          ((eq sym 'github-modern)
           (custom-theme-set-faces sym '(show-paren-match ((t (:bold t :foreground "#d0d0d0" :background "#032f62")))))
           (custom-theme-set-faces sym '(show-paren-mismatch ((t (:bold t :foreground "#ffffff" :background "#d73a49"))))))
          ((eq sym 'granger)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'grayscale)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow2"))))))
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
                                   kaolin-bubblegum kaolin-fusion kaolin-breeze kaolin-temple))
           (custom-theme-set-faces sym `(show-paren-match-face
                                         ((t (:bold t :background
                                                    ,(cadr (assoc 'teal1 kaolin-palette))))))))
          ((eq sym 'klere)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow3"))))))
          ((eq sym 'ld-dark)
           (harmsway/make-all-font-sizes-the-same-please))
          ((eq sym 'light-soap)
           (custom-theme-set-faces sym '(cursor ((t (:background "HotPink1"))))))
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
          ((eq sym 'naquadah)
           (custom-theme-set-faces sym '(region ((t (:background "#555753"))))))
          ((eq sym 'obsidian)
           (custom-theme-set-faces sym '(cursor ((t (:background "#e8e2b7")))))
           (custom-theme-set-faces sym '(show-paren-match-face ((t (:background "#678cb1"))))))
          ((eq sym 'phoenix-dark-pink)
           (harmsway/make-comment-delimiter-face-same-as-comment-face sym))
          ((eq sym 'railscast)
           (custom-theme-set-faces sym '(cursor ((t (:background "#FFC66D"))))))
          ((eq sym 'silkworm)
           (custom-theme-set-faces sym '(cursor ((t :background "DeepPink")))))
          ((eq sym 'soft-charcoal)
           (custom-theme-set-faces sym '(cursor ((t (:background "yellow"))))))
          ((memq sym '(solarized-dark solarized-dark-high-contrast))
           (custom-theme-set-faces sym '(show-paren-match ((t (:foreground "#93a1a1" :background "#657b83")))))
           (custom-theme-set-faces sym '(cursor ((t (:background "#b58900"))))))
          ((memq sym '(solarized-light solarized-light-high-contrast))
           (custom-theme-set-faces sym '(show-paren-match ((t (:foreground "#586e75" :background "#93a1a1"))))))
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
           (custom-theme-set-faces sym '(cursor ((t (:background "gray20"))))))
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
          ((memq sym '(nordless lavenderless broceliande chocolateless seagreenless nofrils-darkless
                               hydrangealess einkless darkless))
                (custom-theme-set-faces
                 sym
                 `(show-paren-match ((t (:foreground ,(face-attribute 'cursor :foreground nil t)
                                                     :background ,(face-attribute 'cursor :background nil t))))))
                (custom-theme-set-faces
                 sym
                 `(show-paren-mismatch ((t (:background ,(face-attribute 'show-paren-mismatch :foreground nil t)
                                                        :foreground ,(face-attribute 'cursor :background nil t)))))))
          )
    ;; (cond ((memq sym harmsway/sml-dark-themes)
    ;;        (load-theme 'smart-mode-line-dark t))
    ;;       ((memq sym harmsway/sml-light-themes)
    ;;        (load-theme 'smart-mode-line-light t))
    ;;       ((memq sym harmsway/sml-respectful-themes)
    ;;        (load-theme 'smart-mode-line-respectful t))
    ;;       )
    ))
(advice-add 'counsel-load-theme-action :after #'harmsway/after-load-theme)

(provide 'custom-themes)
;;; custom-themes.el ends here
