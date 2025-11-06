;;; catpuccin-macchiato-theme.el --- The Medium Contrast Catppuccin -*- lexical-binding:t -*-

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

(defconst catppuccin-macchiato-palette
  (append
   '(
;;; Catppuccin values
     (rosewater "#f4dbd6")
     (flamingo  "#f0c6c6")
     (pink      "#f5bde6")
     (mauve     "#c6a0f6")
     (red       "#ed8796")
     (maroon    "#ee99a0")
     (peach     "#f5a97f")
     (yellow    "#eed49f")
     (green     "#a6da95")
     (teal      "#8bd5ca")
     (sky       "#91d7e3")
     (sapphire  "#7dc4e4")
     (blue      "#8aadf4")
     (lavender  "#b7bdf8")
     (text      "#cad3f5")
     (subtext1  "#b8c0e0")
     (subtext0  "#a5adcb")
     (overlay2  "#939ab7")
     (overlay1  "#8087a2")
     (overlay0  "#6e738d")
     (surface2  "#5b6078")
     (surface1  "#494d64")
     (surface0  "#363a4f")
     (base      "#24273a")
     (mantle    "#1e2030")
     (crust     "#181926"))
   modus-catppuccin-common-palette-mappings)
  "The `macchiato' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

(defcustom catppuccin-macchiato-palette-overrides nil
  "Overrides for `macchiato-palette'.

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
 'catppuccin-macchiato
 'modus-catppuccin
 "The Original — Our darkest variant offering a cozy feeling with color-rich accents."
 'dark
 'modus-themes-operandi-palette
 'macchiato-palette
 'macchiato-palette-overrides)

;;; catpuccin-macchiato-theme.el ends here
