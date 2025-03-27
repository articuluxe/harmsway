;;; thankful-eyes-theme.el --- Theme for color blindness and visual impairments  -*- lexical-binding: t; -*-

;; Author: Andros Fenollosa <hi@andros.dev>
;; URL: https://github.com/tanrax/thankful-eyes-theme.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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
;; This theme is designed to be high contrast and easy to read for people with
;; color blindness or visual impairments.

;;; Code:

(deftheme thankful-eyes "Thankful Eyes theme for Emacs.")

(custom-theme-set-faces
 'thankful-eyes
 '(default ((t (:foreground "#faf6e4" :background "#122b3b"))))
 '(cursor ((t (:background "#faf6e4"))))
 '(fringe ((t (:background "#122b3b"))))
 '(region ((t (:background "#4e5d62"))))
 '(highlight ((t (:background "#4e5d62"))))
 '(hl-line ((t (:background "#1c2f3b"))))
 '(mode-line ((t (:foreground "#a8e1fe" :background "#1c2f3b"))))
 '(mode-line-inactive ((t (:foreground "#6c8b9f" :background "#1c2f3b"))))
 '(font-lock-comment-face ((t (:foreground "#6c8b9f" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#6c8b9f" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "#f6dd62" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#a8e1fe" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#faf6e4" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#fff0a6" :weight bold))))
 '(font-lock-constant-face ((t (:foreground "#b2fd6d" :weight bold))))
 '(font-lock-type-face ((t (:foreground "#b2fd6d" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#b2fd6d" :weight bold))))
 '(font-lock-warning-face ((t (:foreground "#fefeec" :background "#cc0000" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "#ffb000" :weight bold))))
 '(link ((t (:foreground "#a8e1fe" :underline t))))
 '(link-visited ((t (:foreground "#f696db" :underline t))))
 '(isearch ((t (:foreground "#122b3b" :background "#f6dd62"))))
 '(lazy-highlight ((t (:foreground "#122b3b" :background "#fff0a6"))))
 '(error ((t (:foreground "#cc0000" :weight bold))))
 '(tab-bar ((t (:background "#122b3b" :foreground "#a8e1fe" :box (:line-width 2 :color "#122b3b")))))
 '(tab-bar-tab ((t (:background "#4e5d62" :foreground "#faf6e4" :box (:line-width 2 :color "#4e5d62")))))
 '(tab-bar-tab-inactive ((t (:background "#1c2f3b" :foreground "#6c8b9f" :box (:line-width 2 :color "#1c2f3b")))))
 '(tab-line ((t (:background "#122b3b" :foreground "#a8e1fe" :box (:line-width 2 :color "#122b3b")))))
 '(tab-line-tab ((t (:background "#4e5d62" :foreground "#faf6e4" :box (:line-width 2 :color "#4e5d62")))))
 '(tab-line-tab-inactive ((t (:background "#1c2f3b" :foreground "#6c8b9f" :box (:line-width 2 :color "#1c2f3b")))))
 '(tab-line-tab-current ((t (:background "#4e5d62" :foreground "#faf6e4" :box (:line-width 2 :color "#4e5d62"))))))

(provide-theme 'thankful-eyes)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;; thankful-eyes-theme.el ends here
