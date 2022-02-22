;;; package --- Summary: melancholy-theme.el --- A dark theme for dark minds -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Sod Oscarfono

;; Author: Sod Oscarfono <sod@oscarfono.com>
;; URL: http://github.com/techquila/melancholy-theme
;; Package-Version: 20220108.1001
;; Version: 2.8
;; Package-requires: emacs, ttf-ubuntu-font-family

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
;; ========================================
;; A dark theme for dark minds.  > Emacs 24
;;
;; Theme should be considered WIP and is likely to change dramatically, and frequently.
;; That will make you sad.  Now you know why the name?
;; The idea is to get it right by 2020. :-)
;;
;;
;; You must install the ubuntu fonts package to use this theme:
;;
;; sudo apt install ttf-ubuntu-font-family
;;
;; And the Milkshake font:
;;
;; https://www.fontmirror.com/app_public/files/t/1/family_zip/2020/02/Milkshake_Font_Family_(Fontmirror).zip



;;; Code:
;; ========================================

(deftheme melancholy
  "A dark theme for dark minds" )

(let ((font-family-default   "Ubuntu" )
     (font-family-monospace "Ubuntu Mono" )
;;   (font-family-condensed "Ubuntu Condensed" )
     (font-family-cursive   "Milkshake" )
     (my-active             "#F92672" )
     (my-visited            "#999999" )
     (my-info               "#FFB728" )
     (my-highlight          "#96BF33" )
     (my-contrast           "#666666" )
     (my-deepcontrast       "#444444" )
     (my-hicontrast         "#DEDEDE" )
     (my-shadow             "#333333" )
     (my-pop                "#00B7FF" )
     (my-warning            "#FF6969" )
     (my-btw                "#8B4538" )
     (my-white              "#FFFFFF" )
    )

;;;; Theme Faces
  (custom-theme-set-faces
    'melancholy

    ;;;; default
    ;; ========================================

    `(default ((t (:family ,font-family-monospace
                   :width normal
                   :height 99
                   :weight normal
                   :slant normal
                   :foreground ,my-hicontrast
                   :background ,my-shadow
                   :underline nil
                   :overline nil
                   :strike-through nil
                   :box nil
                   :inverse-video nil
                   :stipple nil
                   :inherit nil ))))

    ;;;; window and frame settings
    ;; ========================================
    `(fringe ((t (:inherit default ))))
    `(header-line ((t (:foreground ,my-hicontrast :background ,my-shadow ))))
    `(vertical-border ((t (:foreground ,my-contrast ))))
    `(scroll-bar ((t (:foreground ,my-shadow :background ,my-visited ))))
    `(hl-line ((t (:background ,my-contrast ))))

    ;; line numbers
    ;; ========================================
    `(linum ((t (:foreground ,my-deepcontrast ))))
    `(line-number ((t (:foreground ,my-deepcontrast ))))
    `(line-number-current-line ((t (:foreground ,my-highlight ))))

    ;; base settings
    ;; ========================================
    `(cursor ((t (:background ,my-hicontrast ))))
    `(region ((t (:background ,my-deepcontrast ))))
    `(query-replace ((t (:inherit isearch ))))
    `(match ((t (:background ,my-pop ))))
    `(highlight ((t (:foreground ,my-pop :background ,my-contrast ))))
    `(lazy-highlight ((t (:foreground ,my-shadow :background ,my-highlight ))))
    `(fixed-pitch ((t (:family ,font-family-monospace ))))
    `(variable-pitch ((t (:family ,font-family-default :height 99 :weight normal ))))
    `(bold ((t (:weight bold ))))
    `(italic ((t (:slant italic ))))
    `(bold-italic ((t (:weight bold :slant italic ))))
    `(shadow ((t (:background ,my-shadow ))))
    `(button ((t (:foreground ,my-active :underline (:color foreground-color :style line) ))))
    `(link ((t (:foreground ,my-active :underline t :weight bold ))))
    `(link-visited ((t ( :foreground ,my-visited ))))
    `(secondary-selection ((t (:background ,my-deepcontrast ))))
    `(font-lock-builtin-face ((t (:foreground ,my-highlight ))))
    `(font-lock-comment-delimiter-face ((t (:foreground ,my-visited ))))
    `(font-lock-comment-face ((t (:foreground ,my-visited ))))
    `(font-lock-constant-face ((t (:foreground ,my-info ))))
    `(font-lock-doc-face ((t (:foreground ,my-info ))))
    `(font-lock-function-name-face ((t (:foreground ,my-pop ))))
    `(font-lock-keyword-face ((t (:family ,font-family-cursive :height 1.4 :foreground ,my-active ))))
    `(font-lock-negation-char-face ((t (:foreground ,my-active ))))
    `(font-lock-preprocessor-face ((t (:foreground ,my-active ))))
    `(font-lock-regexp-grouping-backslash ((t (:foreground ,my-pop  ))))
    `(font-lock-regexp-grouping-construct ((t (:foreground ,my-pop  ))))
    `(font-lock-string-face ((t (:foreground ,my-visited :weight extra-light :slant italic ))))
    `(font-lock-type-face ((t (:foreground ,my-pop ))))
    `(font-lock-variable-name-face ((t (:foreground ,my-highlight ))))
    `(font-lock-warning-face ((t (:foreground ,my-warning ))))
    `(tooltip ((t (:foreground ,my-contrast :background ,my-info ))))
    `(trailing-whitespace ((t (:background ,my-warning ))))

    ;; parens / smart-parens
    ;; ========================================
    `(show-paren-match ((t (:background ,my-shadow :weight extra-bold :foreground ,my-pop  ))))
    `(show-paren-mismatch ((t (:background ,my-warning :weight extra-bold ))))
    `(sp-show-pair-match-face ((t (:background ,my-active :height 1.25 :weight extra-bold ))))
    `(sp-show-pair-mismatch-face ((t (:background ,my-warning :weight extra-bold ))))
    `(sp-pair-overlay-face ((t (:background ,my-contrast ))))

    ;; info/errors
    ;; ========================================
    `(success ((t (:foreground ,my-highlight ))))
    `(warning ((t (:foreground ,my-info ))))
    `(error ((t (:foreground ,my-warning :weight bold ))))
    `(next-error ((t (:inherit (region) ))))


    ;; all the icons
    ;; ========================================
    `(all-the-icons-dcyan ((t (:foreground ,my-pop ))))
    `(all-the-icons-dgreen ((t (:foreground ,my-highlight ))))
    `(all-the-icons-dpink ((t (:foreground ,my-active ))))

    ;; calendar
    ;; ========================================
    `(calendar-today ((t (:weight bold :foreground ,my-highlight ))))
    `(calendar-weekday-header ((t (:foreground ,my-info ))))
    `(calendar-weekend-header ((t (:foreground ,my-contrast ))))
    `(calendar-holiday-marker ((t (:foreground ,my-contrast ))))

    ;; dired
    ;; ========================================
    `(dired-header ((t (:family ,font-family-monospace :foreground ,my-pop ))))

    ;; flycheck
    ;; ========================================
    `(flycheck-error ((t  (:colour ,my-active :style wave ))))

    ;; gnus
    ;; ========================================
    `(gnus-group-mail-3 ((t (:foreground ,my-highlight ))))
    `(gnus-group-mail-3-empty ((t (:foreground ,my-contrast ))))
    `(gnus-header-name ((t (:foreground ,my-active ))))
    `(gnus-header-from ((t (:foreground ,my-hicontrast ))))
    `(gnus-header-subject ((t (:foreground ,my-highlight ))))
    `(gnus-header-content ((t (:foreground ,my-visited ))))
    `(gnus-summary-normal-unread ((t (:foreground ,my-highlight ))))
    `(gnus-summary-normal-read ((t (:foreground ,my-highlight ))))
    `(gnus-summary-selected ((t (:foreground ,my-pop ))))

    ;; helm
    ;; ========================================
    `(helm-buffer-file ((t ( :foreground ,my-pop ))))
    `(helm-buffer-directory ((t (:foreground ,my-shadow :background ,my-hicontrast ))))
    `(helm-grep-match ((t (:foreground ,my-highlight ))))
    `(helm-header ((t ( :foreground ,my-white ))))
    `(helm-source-header ((t (:family ,font-family-cursive :height 1.8 :foreground ,my-contrast :underline t ))))
    `(helm-selection ((t (:foreground ,my-deepcontrast :background ,my-pop ))))
    `(helm-separator ((t (:background ,my-deepcontrast ))))


    ;; isearch
    ;; ========================================
    `(isearch ((t (:foreground ,my-shadow :background ,my-highlight ))))
    `(isearch-fail ((t (:background ,my-pop ))))

    ;; magit
    ;; ========================================
     `(magit-section-highlight ((t (:foreground ,my-pop :background ,my-deepcontrast ))))


    ;; minibuffer
    ;; ========================================
    `(minibuffer-prompt ((t (:weight bold :foreground ,my-pop ))))

    ;; org-mode
    ;; ========================================
    `(org-agenda-day-view ((t (:weight bold :foreground ,my-visited ))))
    `(org-agenda-date ((t (:foreground ,my-contrast ))))
    `(org-agenda-date-today ((t (:weight extra-bold :foreground ,my-highlight ))))
    `(org-agenda-date-weekend ((t (:foreground ,my-deepcontrast ))))
    `(org-agenda-done ((t (:slant italic :foreground ,my-contrast :strike-through t ))))
    `(org-imminent-deadline ((t (:foreground ,my-pop ))))
    `(org-upcoming-deadline ((t (:slant italic :foreground ,my-visited ))))
    `(org-priority ((t (:family ,font-family-monospace :slant normal :foreground ,my-visited ))))
    `(org-block-begin-line ((t (:foreground ,my-shadow :background ,my-contrast ))))
    `(org-block ((t (:foreground ,my-pop :background ,my-deepcontrast :box nil ))))
    `(org-block-end-line ((t (:foreground ,my-shadow :background ,my-contrast ))))
    `(org-date ((t (:foreground ,my-visited ))))
    `(org-document-info ((t :(:height 1.25 foreground ,my-visited ))))
    `(org-document-info-keyword ((t (:foreground ,my-contrast ))))
    `(org-document-title ((t (:family ,font-family-cursive  :height 1.9 :foreground ,my-info ))))
    `(org-done ((t (:foreground ,my-contrast :strike-through t ))))
    `(org-headline-done ((t (:foreground ,my-contrast :strike-through t ))))
    `(org-level-1 ((t :family ,font-family-default :height 1.4 :weight bold )))
    `(org-level-2 ((t :family ,font-family-default :height 1.25 :weight regular )))
    `(org-level-3 ((t :family ,font-family-default :height 1.125 :weight normal )))
    `(org-level-4 ((t :family ,font-family-default :weight light )))
    `(org-level-5 ((t :family ,font-family-default :weight extra-light )))
    `(org-level-6 ((t :inherit (org-level-5) )))
    `(org-level-7 ((t :inherit (org-level-5) )))
    `(org-link ((t :foreground ,my-active :underline t )))
    `(org-src-block-faces ((t :family ,font-family-monospace )))
    `(org-special-keyword ((t :family ,font-family-default :foreground ,my-contrast )))
    `(org-table ((t :family ,font-family-monospace )))
    `(org-tag ((t (:foreground ,my-active ))))
    `(org-todo ((t (:foreground ,my-info ))))

    ;; Speedbar
    ;; =======================================
    `(speedbar-directory-face ((t :family ,font-family-monospace :foreground ,my-contrast t )))
    `(speedbar-file-face ((t (:family ,font-family-monospace :foreground ,my-contrast ))))
    `(speedbar-selected-face ((t (:weight extra-bold :foreground ,my-active ))))
    `(speedbar-highlight-face ((t (:foreground ,my-pop ))))
    `(speedbar- ((t (:foreground ,my-active ))))
    `(speedbar-button-face ((t (:foreground ,my-highlight ))))

    ;; The End
    ;; =========================================

    ) ;; custom-theme-set-faces ends here
) ;; let ends here

(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name) )))

(provide-theme 'melancholy)

;;; melancholy-theme.el ends here
