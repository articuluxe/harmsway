;;; codex-theme.el --- Codex theme, a simple high contrast theme

;; Copyright 2024 Çağan Korkmaz

;; Author: Çağan Korkmaz <root@hsnovel.net>
;; URL: https://github.com/hsnovel/codex-theme
;; Version: 0.01

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; Visual studio dark inspired color scheme
;; modified for making it more readable

;;; Code:

(deftheme codex "Codex theme.")

(let
    ((codex-fg "#bdbdbd")
     (codex-fg-2 "#73778C")
     (codex-fg-3 "#84858f")
     (codex-fg-4 "#84858f")
     (codex-fg-5 "#FEFEFE")
     (codex-bg "#000000")
     (codex-bg-2 "#171717")
     (codex-bg-3 "#242424")
     (codex-bg-4 "#282828")
     (codex-bg-5 "#303030")
     (codex-bg-6 "#4d4d4d")
     (codex-keyword "#bdbdbd")
     (codex-type "#56c8ff")
     (codex-str "#00cd00")
     (codex-test "#bdbdbd")
     (codex-test2 "IndianRed")
     (codex-cursor "#40FF40")
     (codex-bg-inactive "#121212")
     (codex-comment "#7d7d7d")
     (codex-select "#3c3c3d")
     (codex-compilation-g "#40FF40")
     (codex-compilation-warn "#c98d40")
     (codex-compilation-r "#bb2626")
     (codex-whitespace-line "#9e3131"))


  (custom-theme-set-faces 'codex
                          `(default ((t (:foreground ,codex-fg :background ,codex-bg))))
                          `(cursor ((t (:background ,codex-cursor ))))
                          `(fringe ((t (:background ,codex-bg))))
                          `(mode-line ((t (:foreground ,codex-fg-5 :background ,codex-comment))))
                          `(mode-line-inactive ((t (:foreground ,codex-fg-5 :background ,codex-bg-inactive))))
                          `(region ((t (:foreground, codex-whitespace-line))))
                          '(secondary-selection ((t (:background "#3e3834" ))))
                          `(font-lock-builtin-face ((t (:foreground ,codex-keyword))))
                          `(font-lock-comment-face ((t (:italic t :foreground, codex-bg-5, :italic t))))
                          `(font-lock-function-name-face ((t (:foreground ,"YellowGreen"))))
                          `(font-lock-keyword-face ((t (:foreground ,codex-test))))
                          `(font-lock-string-face ((t (:foreground ,codex-str ))))
                          `(font-lock-type-face ((t (:foreground ,codex-type ))))
                          `(font-lock-constant-face ((t (:foreground ,codex-fg ))))
                          `(font-lock-variable-name-face ((t (:foreground ,codex-fg))))
                          `(highlight ((t (:background ,codex-bg-3 :foreground nil))))
                          `(highlight-current-line-face ((t ,(list :background codex-bg-3 :foreground nil))))
                          `(font-lock-preprocessor-face ((t (:foreground ,codex-test2))))
                          `(minibuffer-prompt ((t (:foreground ,codex-fg-3))))

                          ;; `(show-paren-match ((t (:background ,codex-select))))
                          '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white"))))

                          '(font-lock-warning-face ((t (:foreground "red" :bold t ))))
                          '(which-func ((t (:foreground "white"))))

                          `(shadow ((t (:foreground ,codex-fg-4))))

                          `(fringe ((t (:background nil :foreground ,codex-bg-4))))
                          `(vertical-border ((t (:foreground ,codex-bg-4))))

                          ;; Rainbow Delimeters Mode
                          `(rainbow-delimiters-depth-1-face ((t (:foreground ,codex-fg))))

                          `(ansi-color-black ((t (:foreground ,"#000000"
                                                              :background ,"#eeeeee"))))

                          `(ansi-color-red ((t (:foreground ,codex-compilation-g
                                                            :background ,codex-compilation-g))))

                          `(ansi-color-green ((t (:foreground "tan3"
                                                              :background "tan3"))))
                          `(ansi-color-yellow ((t (:foreground ,codex-type
                                                               :background ,codex-type))))
                          `(ansi-color-magenta ((t (:foreground ,codex-compilation-g
                                                                :background ,codex-compilation-g))))
                          `(ansi-color-cyan ((t (:foreground ,codex-compilation-warn
                                                             :background ,codex-compilation-warn))))
                          `(ansi-color-blue ((t (:foreground ,codex-fg
                                                             :background ,codex-fg))))
                          `(ansi-color-white ((t (:foreground ,codex-fg
                                                              :background ,codex-fg))))

                          ;; Compilation
                          `(compilation-info ((t (:foreground ,codex-compilation-g))))
                          `(compilation-warning ((t (:foreground ,codex-compilation-warn :bold))))
                          `(compilation-error ((t (:foreground ,codex-compilation-r))))
                          `(compilation-mode-line-fail ((t ,(list :foreground codex-compilation-r :weight 'bold))))
                          `(compilation-mode-line-exit ((t ,(list :foreground codex-compilation-g
                                                                  :weight 'bold :inherit 'unspecified))))

                          ;; Dired
                          `(dired-directory ((t (:foreground ,codex-type))))
                          `(dired-ignored ((t ,(list :foreground codex-compilation-g
                                                     :inherit 'unspecified))))

                          `(line-number ((t (:inherit default :foreground ,codex-fg-4))))
                          `(line-number-current-line ((t (:inherit line-number :foreground ,codex-str))))


                          ;; Tab bar
                          `(tab-bar ((t (:background ,codex-bg-4 :foreground , codex-fg-2))))
                          `(tab-bar-tab ((t (:background nil :foreground ,codex-str))))
                          `(tab-bar-tab-inactive ((t (:background nil))))

                          ;; Ido mode
                          `(ido-first-match ((t (:foreground ,codex-type :bold nil))))
                          `(ido-only-match ((t (:foreground ,codex-type :weight bold))))
                          `(ido-subdir ((t (:foreground ,codex-fg-3 :weight bold))))


                          ;; Which Function
                          `(which-func ((t (:foreground ,codex-fg-3))))
                          ;; Whitespace
                          `(whitespace-space ((t (:background ,codex-bg :foreground ,codex-bg-4))))
                          `(whitespace-tab ((t (:background ,codex-bg :foreground ,codex-bg-4))))
                          `(whitespace-hspace ((t (:background ,codex-bg-2 :foreground ,codex-bg))))
                          `(whitespace-line ((t  (:background ,codex-bg-2 :foreground ,codex-compilation-r :bold t))))

                          `(whitespace-newline ((t (:background ,codex-bg-2 :foreground ,codex-bg-3))))
                          `(whitespace-trailing ((t (:background ,codex-bg :foreground ,codex-str))))
                          `(whitespace-empty ((t (:background ,codex-bg-3
                                                              :foreground ,codex-fg-2))))
                          `(whitespace-indentation ((t (:background ,codex-bg-3 :foreground ,codex-fg-2))))
                          `(whitespace-space-after-tab ((t (:background ,codex-bg-3 :foreground ,codex-fg-2))))
                          `(whitespace-space-before-tab ((t (:background ,codex-bg-3 :foreground ,codex-fg-2))))))
;; autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'codex)

;;; codex-theme.el ends here
