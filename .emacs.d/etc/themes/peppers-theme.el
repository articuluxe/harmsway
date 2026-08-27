;;; peppers-theme.el --- Custom dark theme based on modus framework -*- lexical-binding: t -*-

;; Copyright (C) 2026 Joar von Arndt
;; Author: Joar von Arndt <joarxpablo@vonarndt.se>
;; Maintainer: Joar von Arndt <joarxpablo@vonarndt.se>
;; URL: https://codeberg.org/joar/peppers-theme
;; Keywords: faces, theme, accessibility
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (modus-themes "4.0.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Custom Emacs theme built on the modus-themes framework.  A mostly
;; black-and white theme with stark cyan and magenta highlights.
;; Original theme by Akshay of ~oppi.li~, Emacs implementation by Joar
;; von Arndt.

;;; Code:

(require 'modus-themes)

(defcustom peppers-theme-palette-user nil
  "Like the `peppers-theme-palette' for user-defined entries.
This is meant to extend the palette with custom named colors and/or
semantic palette mappings.  Those may then be used in combination with
palette overrides (also see `modus-themes-common-palette-overrides' and
`peppers-theme-palette-overrides')."
  :group 'modus-themes
  :package-version '(peppers-theme . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Option to extend the palette for use with overrides"))

(defcustom peppers-theme-palette-overrides nil
  "Overrides for `peppers-theme-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Modus themes,
refer to `modus-themes-common-palette-overrides'.

Theme-specific overrides take precedence over shared overrides.
The idea of common overrides is to change semantic color
mappings, such as to make the cursor red.  Whereas theme-specific
overrides can also be used to change the value of a named color,
such as what hexadecimal RGB value the red-warmer symbol
represents."
  :group 'modus-themes
  :package-version '(peppers-theme . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(defconst peppers-theme-palette
  (append
   '((bg-main            "#000000")
     (bg-dim             "#101010")
     (fg-main            "#CFCFCF")
     (fg-dim             "#787878")
     (fg-alt             "#EEFFFF")
     (bg-active          "#3A3A3A")
     (bg-inactive        "#353535")
     (border             "#4A4A4A")

     (white              "#FFFFFF")

     ;; Custom accent colors
     (red                "#c8181d")
     (red-intense "#be161b")
     (red-faint "#dc1a20")
     (red-cooler "#d8193d")
     (red-warmer "#e9272c")
     (bg-red-nuanced "#280405")
     (bg-red-subtle "#50090b")
     (bg-red-intense "#780e11")

     (green              "#10A778")
     (green-intense      "#12c08a")
     (green-faint        "#14dfa0")
     (green-cooler       "#5FD7A7")
     (green-warmer       "#3bd29b")
     (bg-green-nuanced   "#03281d")
     (bg-green-subtle    "#07513a")
     (bg-green-intense   "#0b7957")

     (yellow             "#FFe000")
     (yellow-intense     "#FFF000")
     (yellow-faint       "#eee517")
     (yellow-cooler      "#ecf368")
     (yellow-warmer      "#ffd831")
     (bg-yellow-nuanced  "#524000")
     (bg-yellow-subtle   "#656100")
     (bg-yellow-intense  "#b9a000")

     (blue               "#79ffe1")
     (blue-intense       "#66ffdd")
     (blue-faint         "#9fffea")
     (blue-cooler        "#a7f0ef")
     (blue-warmer        "#c3f3e5")
     (bg-blue-nuanced    "#004b3a")
     (bg-blue-subtle     "#009675")
     (bg-blue-intense    "#00e2af")
     
     (magenta            "#ff3299")
     (magenta-intense    "#ff2291")
     (magenta-faint      "#ff50a8")
     (magenta-cooler     "#ec5eb9")
     (magenta-warmer     "#ff69ac")
     (bg-magenta-nuanced "#3d001e")
     (bg-magenta-subtle  "#7a003d")
     (bg-magenta-intense "#b7005c")
     
     (cyan               "#79ffe1")
     (cyan-intense       "#66ffdd")
     (cyan-faint         "#9fffea")
     (cyan-cooler        "#a7f0ef")
     (cyan-warmer        "#c3f3e5")
     (bg-cyan-nuanced    "#004b3a")
     (bg-cyan-subtle     "#009675")
     (bg-cyan-intense    "#00e2af")


     (fnname magenta)
     (docstring fg-dim)
     (number cyan)
     (string cyan)
     (variable fnname)
     (variable-use fg-main)
     (keyword white)
     (builtin magenta)

     (fringe bg-main)
     (bg-line-number-active bg-main)
     (fg-line-number-inactive bg-inactive)
     (bg-line-number-inactive bg-main)

     (bg-region bg-active)
     
     (rainbow-0 fg-alt)
     (rainbow-1 magenta)
     (rainbow-2 blue)
     (rainbow-3 green)
     (rainbow-4 fg-alt)
     (rainbow-5 cyan-faint)
     (rainbow-6 magenta-intense)
     (rainbow-7 green-cooler)
     (rainbow-8 yellow-warmer)

     (fg-mode-line-active fg-main)
     (bg-mode-line-active bg-main)
     
     (fg-mode-line-inactive border)
     (bg-mode-line-inactive bg-main)

     (bg-completion bg-dim)
     (fg-completion yellow)

     (fg-heading-1 magenta)
     (fg-heading-2 cyan)
     (fg-heading-3 green-faint)
     (fg-heading-4 yellow)
     (fg-heading-5 magenta-cooler)
     (fg-heading-6 red-intense)
     (fg-heading-7 blue-cooler)

     (bg-search-current bg-magenta-intense)
     (bg-search-lazy bg-magenta-nuanced))
   modus-themes-vivendi-palette
   peppers-theme-palette-user)
  "The `peppers' palette.
This is the full palette for the theme, combining custom color
definitions with the base modus-vivendi palette.")

(modus-themes-theme
 'peppers
 'modus-themes
 "Custom dark theme with cyan foreground and vibrant accents.
Conforms with the highest legibility standard for color contrast
between background and foreground in any given piece of text,
which corresponds to a minimum contrast in relative luminance of
7:1 (WCAG AAA standard)."
 'dark
 'peppers-theme-palette
 'peppers-theme-palette-user
 'peppers-theme-palette-overrides)

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (add-to-list 'custom-theme-load-path dir)))

(provide 'peppers-theme)

;;; peppers-theme.el ends here
