;;; catpuccin-latte-theme.el --- The Lightest Catppuccin -*- lexical-binding:t -*-

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

(defconst catppuccin-latte-palette
  (append
   '(
;;; Catppuccin values
     (rosewater "#dc8a78")
     (flamingo  "#dd7878")
     (pink      "#ea76cb")
     (mauve     "#8839ef")
     (red       "#d20f39")
     (maroon    "#e64553")
     (peach     "#fe640b")
     (yellow    "#df8e1d")
     (green     "#40a02b")
     (teal      "#179299")
     (sky       "#04a5e5")
     (sapphire  "#209fb5")
     (blue      "#1e66f5")
     (lavender  "#7287fd")
     (text      "#4c4f69")
     (subtext1  "#5c5f77")
     (subtext0  "#6c6f85")
     (overlay2  "#7c7f93")
     (overlay1  "#8c8fa1")
     (overlay0  "#9ca0b0")
     (surface2  "#acb0be")
     (surface1  "#bcc0cc")
     (surface0  "#ccd0da")
     (base      "#eff1f5")
     (mantle    "#e6e9ef")
     (crust     "#dce0e8"))
   modus-catppuccin-common-palette-mappings)
  "The `latte' palette.
Color values have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a color that already exists
in the palette and is associated with a HEX-VALUE.")

(defcustom catppuccin-latte-palette-overrides nil
  "Overrides for `latte-palette'.

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
 'catppuccin-latte
 'modus-catppuccin
 "The Original — Our darkest variant offering a cozy feeling with color-rich accents."
 'dark
 'modus-themes-vivendi-palette
 'latte-palette
 'latte-palette-overrides)

;;; catpuccin-latte-theme.el ends here
