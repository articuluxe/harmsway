;;; carp-theme.el --- Carp theme -*- lexical-binding: t; -*-

;; Copyright (C) 2024 David Goudou

;; Author: David Goudou <david.goudou@gmail.com>
;; Version: 0.0.3
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://codeberg.org/seahorse/carp-theme

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Carp is a dark, multicoloured theme for GNU Emacs

;;; Code:

(deftheme carp "Carp theme.")

(let* ((fg "#d6deeb")
       (bg "#280c27")
       (shadow "#808898")
       (region "#792465")
       (cursor "#fdff28")
       (hl-line "#43192f")
       (blue-0 "#78b0d3")
       (blue-1 "#6172c6")
       (green-0 "#97ef9f")
       (green-1 "#85eac3")
       (green-2 "#1f7500")
       (grey-0 "#BDC3C7")
       (grey-1 "#8B8B8B")
       (grey-2 "#6C757D")
       (orange-0 "#f9c686")
       (orange-1 "#d8893a")
       (purple-0 "#aa89d6")
       (pink-0 "#db538e")
       (red-0 "#d14051")
       (red-1 "#f44649")
       (red-2 "#ad034d")
       (red-3 "#bf466c")
       (yellow-0 "#fcfa6c"))

  (custom-theme-set-faces
   'carp
   `(default ((t (:foreground ,fg :background ,bg))))
   `(display-time-date-and-time ((t (:foreground ,grey-2))))
   `(diredp-date-time ((t (:foreground ,fg))))
   `(dired-directory ((t (:foreground ,blue-0))))
   `(diredp-deletion ((t (:foreground ,red-0 :background ,bg))))
   `(diredp-dir-heading ((t (:foreground ,yellow-0 :background ,bg))))
   `(diredp-dir-name ((t (:foreground ,green-1 :background ,bg))))
   `(diredp-dir-priv ((t (:foreground ,green-1 :background ,bg))))
   `(diredp-exec-priv ((t (:foreground ,fg :background ,bg))))
   `(diredp-file-name ((t (:foreground ,fg))))
   `(diredp-file-suffix ((t (:foreground ,fg))))
   `(diredp-link-priv ((t (:foreground ,fg))))
   `(diredp-number ((t (:foreground ,fg))))
   `(diredp-no-priv ((t (:foreground ,fg :background ,bg))))
   `(diredp-rare-priv ((t (:foreground ,red-0 :background ,bg))))
   `(diredp-read-priv ((t (:foreground ,fg :background ,bg))))
   `(diredp-symlink ((t (:foreground ,red-3))))
   `(diredp-write-priv ((t (:foreground ,fg :background ,bg))))
   `(font-lock-builtin-face ((t (:foreground ,blue-1))))
   `(font-lock-comment-face ((t (:foreground ,grey-0 :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,orange-1))))
   `(font-lock-doc-face ((t (:foreground ,green-0))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-keyword-face ((t (:foreground ,yellow-0 :slant italic))))
   `(font-lock-preprocessor-face ((t (:foreground ,green-1))))
   `(font-lock-string-face ((t (:foreground ,pink-0))))
   `(font-lock-type-face ((t (:foreground ,orange-0))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-warning-face ((t (:foreground ,red-1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,yellow-0 :bold t))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,red-0 :bold t))))
   `(fringe ((t (:foreground ,fg :background ,bg))))
   `(parenthesis ((t (:foreground ,grey-2))))
   `(header-line ((t (:foreground ,fg))))
   `(highlight ((t (:foreground ,red-1))))
   `(highlight-indentation-face ((t (:background ,grey-1))))
   `(highlight-indentation-current-column-face ((t (:background ,grey-1))))
   `(hl-line ((t (:background ,hl-line))))
   `(isearch ((t (:foreground ,fg :background ,red-1))))
   `(isearch-fail ((t (:background ,red-1))))
   `(lazy-highlight ((t (:foreground ,red-1 :background unspecified))))
   `(match ((t (:background ,red-1))))
   `(minibuffer-prompt ((t (:foreground ,fg))))
   `(mode-line ((t (:background unspecified :box unspecified :overline ,shadow))))
   `(mode-line-inactive ((t (:foreground ,shadow :background unspecified :box unspecified :overline ,shadow))))
   `(org-ellipsis ((t (:foreground ,blue-1 :underline nil))))
   `(org-block-begin-line ((t (:slant italic))))
   `(org-checkbox ((t (:foreground ,green-1))))
   `(org-date ((t (:foreground ,grey-0))))
   `(org-document-info-keyword ((t (:foreground ,green-1))))
   `(org-document-title ((t (:foreground ,green-1))))
   `(org-verbatim ((t (:foreground ,blue-0))))
   `(org-code ((t (:foreground ,purple-0))))
   `(org-done ((t (:foreground ,green-2))))
   `(org-level-1 ((t (:foreground ,blue-0))))
   `(org-level-2 ((t (:foreground ,green-0))))
   `(org-level-3 ((t (:foreground ,orange-0))))
   `(org-level-4 ((t (:foreground ,purple-0))))
   `(org-level-5 ((t (:foreground ,red-0))))
   `(org-level-6 ((t (:foreground ,yellow-0))))
   `(org-link ((t (:foreground ,blue-0))))
   `(org-meta-line ((t (:foreground ,grey-0))))
   `(org-special-keyword ((t (:foreground ,purple-0))))
   `(org-todo ((t (:foreground ,red-3))))
   `(region ((t (:foreground unspecified :background ,region))))
   `(trailing-whitespace ((t (:background ,red-1))))
   `(vertical-border ((t (:foreground ,shadow :background ,shadow))))
   `(warning ((t (:foreground ,orange-0))))
   `(whitespace-trailing ((t (:background ,red-1))))
   `(cursor ((t (:foreground unspecified :background ,cursor))))
   `(show-paren-match ((t (:foreground unspecified :background unspecified :underline t :bold t))))
   `(rcirc-prompt ((t (:foreground ,fg))))
   `(rcirc-server ((t (:foreground ,grey-0 :slant italic))))
   `(rcirc-url ((t (:foreground ,blue-0))))
   `(rcirc-my-nick ((t (:foreground ,purple-0))))
   `(rcirc-nick-in-message ((t (:foreground ,purple-0))))
   `(rcirc-nick-in-message-full-line ((t (:foreground ,purple-0))))
   `(rcirc-other-nick ((t (:foreground ,blue-1))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory (or load-file-name buffer-file-name)))))

(provide-theme 'carp)

;;; carp-theme.el ends here
