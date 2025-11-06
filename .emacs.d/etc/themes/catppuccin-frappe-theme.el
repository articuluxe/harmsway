;;; catpuccin-frappe-theme.el --- The Less Vibrant Catppuccin -*- lexical-binding:t -*-

;; Copyright (C) 2022-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/standard-themes
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

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
;; The `modus-catppuccin' is a collection of themes for GNU Emacs.

;;; Code:



(require 'modus-catppuccin)

(defconst catppuccin-frappe-palette
  (append
   '(
;;; Catppuccin values
     (rosewater "#f2d5cf")
     (flamingo  "#eebebe")
     (pink      "#f4b8e4")
     (mauve     "#ca9ee6")
     (red       "#e78284")
     (maroon    "#ea999c")
     (peach     "#ef9f76")
     (yellow    "#e5c890")
     (green     "#a6d189")
     (teal      "#81c8be")
     (sky       "#99d1db")
     (sapphire  "#85c1dc")
     (blue      "#8caaee")
     (lavender  "#babbf1")
     (text      "#c6d0f5")
     (subtext1  "#b5bfe2")
     (subtext0  "#a5adce")
     (overlay2  "#949cbb")
     (overlay1  "#838ba7")
     (overlay0  "#737994")
     (surface2  "#626880")
     (surface1  "#51576d")
     (surface0  "#414559")
     (base      "#303446")
     (mantle    "#292c3c")
     (crust     "#232634"))
   modus-catppuccin-common-palette-mappings)
  "The `frappe' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

(defcustom catppuccin-frappe-palette-overrides nil
  "Overrides for `frappe-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Catppuccin themes,
refer to `modues-catppuccin-common-palette-overrides'.

Theme-specific overrides take precedence over shared overrides.
The idea of common overrides is to change semantic color
mappings, such as to make the cursor red.  Wherea theme-specific
overrides can also be used to change the value of a named color,
such as what hexadecimal RGB value the red-warmer symbol
represents."
  :group 'modus-catppuccin
  :package-version '(modus-catppuccin . "0.0.1")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-catppuccin) Palette overrides"))

(modus-themes-theme
 'catppuccin-frappe
 'modus-catppuccin
 "The Original — Our darkest variant offering a cozy feeling with color-rich accents."
 'dark
 'modus-themes-operandi-palette
 'frappe-palette
 'frappe-palette-overrides)

;;; catpuccin-frappe-theme.el ends here
