;; color-theme-pomgranate-explosion: a color theme for GNU Emacs

;; Copyright (C) 2012, 2015 Benoit Marcot - juz4m AT hotmail DOT com

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(defun color-theme-pomgranate-explosion ()
  "Color Theme Pomgranate Explosion - Benoit Marcot (juz4m@hotmail.com)."
  (interactive)
  (color-theme-install
   '(color-theme-pomgranate-explosion
     ((background-color . "#222222")
      (background-mode . dark)
      (border-color . "#222222")
      (cursor-color . "#dddddd")
      (foreground-color . "#dddddd")
      (mouse-color . "black"))
     ((blank-space-face . blank-space-face)
      (blank-tab-face . blank-tab-face)
      (list-matching-lines-face . bold)
      (view-highlight-face . highlight))
     (default ((t (nil))))
     (blank-space-face ((t (:background "LightGray"))))
     (blank-tab-face ((t (:background "green" :foreground "black"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))

     (font-lock-constant-face ((t (:foreground "#f2e1ac"))))
     (font-lock-comment-face ((t (:foreground "#cd2c24" :italic t))))
     (font-lock-comment-delimiter-face ((t (:foreground "#cd2c24" :italic t))))
     (font-lock-keyword-face ((t (:foreground "#f2594b"))))
     (font-lock-preprocessor-face ((t (:foreground "#63a69f"))))
     (font-lock-string-face ((t (:foreground "#f2e1ac"))))
     (font-lock-variable-name-face ((t (:bold t :foreground "#f2e1ac"))))
     (font-lock-function-name-face ((t (:bold t :foreground "#ffffff"))))
     (font-lock-type-face ((t (:foreground "#f2836b"))))
     (font-lock-warning-face ((t (:bold t :foreground "red"))))
     (font-lock-builtin-face ((t (:foreground "#63a69f"))))

     (highlight ((t (:background "#f2836b" :foreground "#dddddd"))))
     (isearch ((t (:background "#63a69f" :foreground "#dddddd"))))
     (minibuffer-prompt ((t (:foreground "#f2886b"))))
     (modeline ((t (:background "#cd2c24" :foreground "#dddddd"))))
     (region ((t (:background "#f2594b" :foreground "#dddddd"))))
     (show-paren-match-face ((t (:bold t :background "#63a69f" :foreground "#222222"))))
     (show-paren-mismatch-face ((t (:bold t :background "#cd2c24" :foreground "#dddddd"))))
 )))
