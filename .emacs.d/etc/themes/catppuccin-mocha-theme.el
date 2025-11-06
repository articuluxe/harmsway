;;; catpuccin-mocha-theme.el --- The Original Catppuccin -*- lexical-binding:t -*-

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

(defconst catppuccin-mocha-palette
  (append
   '(
;;; Catppuccin values

     (rosewater "#f5e0dc")
     (flamingo  "#f2cdcd")
     (pink      "#f5c2e7")
     (mauve     "#cba6f7")
     (red       "#f38ba8")
     (maroon    "#eba0ac")
     (peach     "#fab387")
     (yellow    "#f9e2af")
     (green     "#a6e3a1")
     (teal      "#94e2d5")
     (sky       "#89dceb")
     (sapphire  "#74c7ec")
     (blue      "#89b4fa")
     (lavender  "#b4befe")
     (text      "#cdd6f4")
     (subtext0  "#a6adc8")
     (subtext1  "#bac2de")
     (overlay0  "#6c7086")
     (overlay1  "#7f849c")
     (overlay2  "#9399b2")
     (surface0  "#313244")
     (surface1  "#45475a")
     (surface2  "#585b70")
     (base      "#1e1e2e")
     (mantle    "#181825")
     (crust     "#11111b"))
   modus-catppuccin-common-palette-mappings)
  "The `mocha' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

(defcustom catppuccin-mocha-palette-overrides nil
  "Overrides for `mocha-palette'.

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
 'catppuccin-mocha
 'modus-catppuccin
 "The Original — Our darkest variant offering a cozy feeling with color-rich accents."
 'dark
 'modus-themes-operandi-palette
 'mocha-palette
 'mocha-palette-overrides)

;;; catpuccin-mocha-theme.el ends here
