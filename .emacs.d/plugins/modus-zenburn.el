;;; modus-zenburn.el --- Zenburn themes on Modus themes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kien Nguyen
;; Author: Kien Nguyen
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (modus-themes "5.0.0"))
;; Keywords: faces, theme

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides the `modus-zenburn' and
;; `modus-zenburn-light' themes.  They combine Zenburn-inspired
;; palettes with the face coverage and customization framework of
;; Modus themes.

;;; Code:

(require 'modus-themes)

(defgroup modus-zenburn ()
  "Zenburn themes on the Modus themes infrastructure."
  :group 'faces
  :group 'modus-themes
  :prefix "modus-zenburn-"
  :tag "Modus Zenburn")

(defconst modus-zenburn-palette-mappings
  '((builtin fg-main)
    (comment green)
    (constant green-intense)
    (docstring green-cooler)
    (fnname cyan)
    (fnname-call cyan)
    (keyword yellow)
    (number blue)
    (preprocessor blue-warmer)
    (property cyan)
    (rx-backslash green)
    (rx-construct yellow)
    (string red)
    (type blue-faint)
    (variable yellow-warmer)
    (variable-use yellow-warmer)

    (fg-link yellow)
    (underline-link yellow)
    (fg-link-symbolic yellow)
    (underline-link-symbolic yellow)
    (fg-link-visited yellow-faint)
    (underline-link-visited yellow-faint)
    (fg-prompt yellow)

    (fg-completion-match-0 blue)
    (fg-completion-match-1 green-cooler)
    (fg-completion-match-2 yellow)
    (fg-completion-match-3 magenta)

    (fg-mode-line-active green-warmer)
    (fg-mode-line-inactive green-faint)
    (fg-region fg-main)
    (fg-paren-match fg-main)
    (fg-search-current yellow-faint)
    (fg-search-lazy yellow-faint)
    (fg-search-static yellow-faint)
    (fg-search-replace fg-main))
  "Semantic palette mappings shared by the Modus Zenburn themes.")

(defconst modus-zenburn-palette
  (modus-themes-generate-palette
   '((bg-main "#3F3F3F")
     (bg-dim "#383838")
     (bg-active "#4F4F4F")
     (bg-inactive "#494949")
     (border "#5F5F5F")
     (fg-main "#DCDCCC")
     (fg-dim "#989890")
     (fg-alt "#656555")
     (fringe "#2B2B2B")

     (red "#ECB3B3")
     (red-warmer "#DCA3A3")
     (red-cooler "#ECB3B3")
     (red-faint "#F0BDBD")
     (red-intense "#F8CACA")
     (green "#A0C5A0")
     (green-warmer "#BFEBBF")
     (green-cooler "#AFD8AF")
     (green-faint "#AFD8AF")
     (green-intense "#BFEBBF")
     (yellow "#F0DFAF")
     (yellow-warmer "#F3CD98")
     (yellow-cooler "#E0CF9F")
     (yellow-faint "#E9D8A8")
     (blue "#ACE0E3")
     (blue-warmer "#BBD7F7")
     (blue-faint "#BDE0F3")
     (blue-intense "#BDE0F3")
     (magenta "#E5AAD2")
     (cyan "#93E0E3")

     (bg-popup "#4F4F4F")
     (bg-completion "#2B2B2B")
     (bg-hover "#2B2B2B")
     (bg-hover-secondary "#383838")
     (bg-hl-line "#2B2B2B")
     (bg-region "#2B2B2B")

     (bg-mode-line-active "#2B2B2B")
     (border-mode-line-active "#2B2B2B")
     (bg-mode-line-inactive "#383838")
     (border-mode-line-inactive "#383838")

     (bg-tab-bar "#2B2B2B")
     (bg-tab-current "#2B2B2B")
     (bg-tab-other "#2B2B2B")

     (fg-line-number-inactive "#6F6F6F")
     (fg-line-number-active "#D0BF8F")
     (bg-line-number-inactive "#383838")
     (bg-line-number-active "#383838")

     (bg-paren-match "#6F6F6F")

     (bg-search-current "#5F5F5F")
     (bg-search-lazy "#383838")
     (bg-search-static "#494949")
     (bg-search-replace "#8C5353")

     (cursor "#FFFFEF"))
   'cool
   modus-themes-vivendi-tinted-palette
   modus-zenburn-palette-mappings)
  "Palette for the `modus-zenburn' theme.")

(defcustom modus-zenburn-palette-overrides nil
  "Palette overrides for the `modus-zenburn' theme."
  :group 'modus-zenburn
  :package-version '(modus-zenburn . "0.1")
  :type '(repeat (list symbol (choice symbol string))))

(defconst modus-zenburn-light-palette
  (modus-themes-generate-palette
   '((bg-main "#F9F8F5")
     (bg-dim "#EFEEE8")
     (bg-active "#DCDCCC")
     (bg-inactive "#E8E6DF")
     (border "#C0C0C0")
     (fg-main "#3E3E3E")
     (fg-dim "#6F6F6F")
     (fg-alt "#656555")
     (fringe "#EFEEE8")

     (red "#7C4343")
     (red-faint "#653434")
     (red-intense "#402121")
     (green "#365536")
     (green-warmer "#3B4727")
     (green-cooler "#2C4F43")
     (green-faint "#3E4937")
     (green-intense "#274F2F")
     (yellow "#594600")
     (yellow-warmer "#74421F")
     (yellow-faint "#4C4223")
     (yellow-intense "#574300")
     (blue "#2F4B5E")
     (blue-warmer "#45506A")
     (blue-faint "#2C4454")
     (magenta "#663A57")
     (magenta-intense "#62314F")
     (cyan "#315858")
     (cyan-intense "#2A5050")

     (bg-popup "#F3F1EB")
     (bg-completion "#E4E2D8")
     (bg-hover "#DCDCCC")
     (bg-hover-secondary "#E8E6DF")
     (bg-hl-line "#E4E2D8")
     (bg-region "#E4E2D8")

     (bg-mode-line-active "#DCDCCC")
     (border-mode-line-active "#989890")
     (bg-mode-line-inactive "#E8E6DF")
     (border-mode-line-inactive "#C0C0C0")

     (bg-tab-bar "#E8E6DF")
     (bg-tab-current "#F9F8F5")
     (bg-tab-other "#DCDCCC")

     (fg-line-number-inactive "#989890")
     (fg-line-number-active "#6F6F6F")
     (bg-line-number-inactive "#F9F8F5")
     (bg-line-number-active "#EFEEE8")

     (bg-paren-match "#DCDCCC")

     (bg-search-current "#E5D9A8")
     (bg-search-lazy "#D7E4E2")
     (bg-search-static "#E7D8E1")
     (bg-search-replace "#E6CCCC")

     (cursor "#7C4343"))
   'warm
   modus-themes-operandi-tinted-palette
   modus-zenburn-palette-mappings)
  "Palette for the `modus-zenburn-light' theme.")

(defcustom modus-zenburn-light-palette-overrides nil
  "Palette overrides for the `modus-zenburn-light' theme."
  :group 'modus-zenburn
  :package-version '(modus-zenburn . "0.1")
  :type '(repeat (list symbol (choice symbol string))))

(modus-themes-declare
 'modus-zenburn
 'modus-zenburn
 "Zenburn colors and semantic mappings on Modus Vivendi Tinted."
 'dark
 'modus-themes-vivendi-tinted-palette
 'modus-zenburn-palette
 'modus-zenburn-palette-overrides)

(modus-themes-declare
 'modus-zenburn-light
 'modus-zenburn
 "Original light adaptation of Zenburn colors and semantic mappings."
 'light
 'modus-themes-operandi-tinted-palette
 'modus-zenburn-light-palette
 'modus-zenburn-light-palette-overrides)

(modus-themes-register 'modus-zenburn)
(modus-themes-register 'modus-zenburn-light)

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (file-equal-p dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'modus-zenburn)
;;; modus-zenburn.el ends here
