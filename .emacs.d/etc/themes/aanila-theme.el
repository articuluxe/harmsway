;;; aanlia-theme.el --- A moderately dark theme

;; Copyright (C) 2015 by Santosh Sivaraj

;; Author: Santosh Sivaraj <santosh@fossix.org>
;; URL: https://github.com/santoshs/aanila
;; Version: 0.01

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

;;; Code:

(deftheme aanila
  "aanila theme")

(custom-theme-set-faces
 'aanila

 '(default ((t (:background "black" :foreground "lightgray"))))
 '(background-toolbar-color ((t (:foregroung "#000"))))
 '(border ((t (:foreground "black"))))
 '(bottom-toolbar-shadow-color ((t (:foreground "black"))))
 '(cursor ((t (:foreground "black"))))
 '(top-toolbar-shadow-color ((t (:foreground "#111"))))
 '(modeline ((t (:background "gray30" :foreground "goldenrod"))))
 '(modeline-inactive ((t (:background "gray5" :foreground "gray40"))))
 '(modeline-buffer-id ((t (:foreground "dark green"
                                       :box (:line-width 1 :style none)))))
 '(modeline-mousable ((t (:background "#000" :foreground "#555"))))
 '(modeline-mousable-minor-mode ((t (:background "#000" :foreground "#555"))))
 '(which-func ((t (:inherit mode-line))))
 '(fringe ((t (:background "#111" :foreground "#444"))))
 '(linum ((t (:background "gray10" :foreground "dim gray"))))
 '(region ((t (:foreground "cyan" :background "dark cyan"))))
 '(highlight ((t (:foreground "#f2f2f2" :background "#141414"))))
 '(show-paren-match ((t (:background "#1f1f1f"))))
 '(highlight-changes ((t (:foreground nil :background "midnight blue"))))
 '(highlight-changes-delete ((t (:foreground nil :background "chocolate4" :underline nil))))
 '(hl-line ((t (:background "#404040"))))
 '(secondary-selection ((t (:foreground "black" :background "white smoke"))))

 ;; All font locks
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-face ((t (:foreground "#6b5745" :italic t :background "#141414"))))
 '(font-lock-constant-face ((t (:foreground "indianred"))))
 '(font-lock-doc-string-face ((t (:foreground "SeaGreen2" :bold t))))
 '(font-lock-keyword-face ((t (:foreground "SkyBlue" :bold t))))
 '(font-lock-preprocessor-face ((t (:foreground "sky blue" :background "gray10"))))
 '(font-lock-reference-face ((t (:foreground "blue"))))
 '(font-lock-string-face ((t (:foreground "DarkSlateGray2"))))
 '(font-lock-function-name-face ((t (:foreground "steelblue"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-name-face ((t (:foreground "DarkOrange"))))
 '(isearch ((t (:foreground "red4" :background "CadetBlue4"))))
 '(underline ((t (:underline t))))
 '(italic ((t (:italic t))))
 '(bold-italic ((t (:bold t :italic t))))

 ;; auto-completion faces
 '(ac-candidate-face ((t (:inherit popup-face :background "lightgray"))))
 '(ac-completion-face ((t (:foreground "SkyBlue1" :underline t))))
 '(ac-selection-face ((t (:inherit popup-menu-selection-face :background "steelblue"))))

 ;; helm faces
 '(helm-ff-directory ((t (:foreground "light slate blue"))))
 '(helm-ff-dotted-directory ((t (:foreground "khaki4"))))
 '(helm-selection ((t (:background "gray25" :weight bold))))
 '(helm-selection-line ((t (:inherit helm-selection))))
 '(helm-source-header ((t (:inherit font-lock-comment-face))))

 ;; fixme mode
 '(fic-face ((t (:inherit font-lock-comment-face :foreground "#aa2222" :weight bold))))

 ;; org-mode
 '(org-agenda-date-today ((t (:foreground "white"
                                          :slant italic :weight bold))) t)
 '(org-agenda-structure ((t (:foreground "SlateGray"))))
 '(org-archived ((t (:foreground "#8f8f8f"))))
 '(org-checkbox ((t (:background "dimgray" :foreground "WhiteSmoke"
                                 :box (:line-width 1 :style released-button)))))

 '(org-todo ((t (:bold t :foreground "#cc9393" :weight bold))))
 '(org-scheduled ((t (:foreground "skyblue"))))
 '(org-scheduled-previously ((t (:foreground "RoyalBlue"))))
 '(org-scheduled-today ((t (:foreground "DeepSkyBlue"))))
 '(org-tag ((t (:bold t :weight bold))))
 '(org-time-grid ((t (:foreground "DimGray"))))
 '(org-deadline-announce ((t (:foreground "Salmon"))))
 '(org-upcoming-deadline ((t (:foreground "Salmon"))))
 '(org-upcoming-distant-deadline ((t (:foreground "#8c5353"))))
 '(org-warning ((t (:bold t :foreground "#ff3333" :weight bold))))

 ;; org heading levels
 '(org-level-1 ((t (:bold t :foreground "#4682b4" :weight bold))))
 '(org-level-2 ((t (:bold t :foreground "#778899" :weight bold))))
 '(org-level-3 ((t (:bold t :foreground "#5f6e70" :weight bold))))
 '(org-level-4 ((t (:bold t :foreground "#4f5e70" :weight bold))))
 '(org-level-5 ((t (:bold t :foreground "#99aabb" :weight bold))))
 '(org-level-6 ((t (:bold t :foreground "#bbccdd" :weight bold))))
 '(org-level-7 ((t (:bold t :foreground "#bbbbbb" :weight bold))))
 '(org-level-8 ((t (:bold t :foreground "#dddddd" :weight bold))))
 '(org-drawer ((t (:foreground "#252525"))))
 '(org-special-keyword ((t (:foreground "#777777" :background "#111111"))))
 '(org-property-value ((t (:foreground "#777777"))))
 '(org-link ((t (:foreground "#939393" :underline t))))
 '(org-checkbox-statistics-todo ((t (:foreground "#bebebe")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'aanila)
