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
   `(
;;; Catppuccin values
     (rosewater ,(ctp-color 0.028571428571428612 0.5737704918032784 0.8803921568627451))
     (flamingo  ,(ctp-color 0.0 0.5853658536585367 0.8392156862745098))
     (pink      ,(ctp-color 0.8777777777777778 0.7317073170731713 0.8392156862745098))
     (mauve     ,(ctp-color 0.7685185185185184 0.5901639344262294 0.7607843137254902))
     (red       ,(ctp-color 0.9966996699669965 0.6778523489932885 0.7078431372549019))
     (maroon    ,(ctp-color 0.9938271604938271 0.6585365853658534 0.7588235294117647))
     (peach     ,(ctp-color 0.05647382920110192 0.7908496732026143 0.7))
     (yellow    ,(ctp-color 0.10980392156862744 0.6204379562043796 0.7313725490196079))
     (green     ,(ctp-color 0.26620370370370366 0.4390243902439024 0.6784313725490196))
     (teal      ,(ctp-color 0.4765258215962442 0.3922651933701657 0.6450980392156862))
     (sky       ,(ctp-color 0.5252525252525252 0.47826086956521735 0.7294117647058823))
     (sapphire  ,(ctp-color 0.5517241379310346 0.5541401273885351 0.692156862745098))
     (blue      ,(ctp-color 0.6156462585034014 0.7424242424242424 0.7411764705882353))
     (lavender  ,(ctp-color 0.6636363636363637 0.6626506024096385 0.8372549019607842))
     (text      ,(ctp-color 0.6312056737588653 0.7014925373134333 0.8686274509803922))
     (subtext1  ,(ctp-color 0.6296296296296297 0.43689320388349495 0.7980392156862746))
     (subtext0  ,(ctp-color 0.6341463414634146 0.2949640287769784 0.7274509803921569))
     (overlay2  ,(ctp-color 0.6324786324786325 0.22285714285714275 0.6568627450980392))
     (overlay1  ,(ctp-color 0.6296296296296297 0.16981132075471703 0.584313725490196))
     (overlay0  ,(ctp-color 0.6363636363636364 0.13360323886639683 0.515686274509804))
     (surface2  ,(ctp-color 0.6333333333333334 0.1327433628318584 0.44313725490196076))
     (surface1  ,(ctp-color 0.6309523809523809 0.14736842105263157 0.37254901960784315))
     (surface0  ,(ctp-color 0.638888888888889 0.15584415584415584 0.30196078431372547))
     (base      ,(ctp-color 0.6363636363636364 0.18644067796610175 0.23137254901960785))
     (mantle    ,(ctp-color 0.6403508771929824 0.18811881188118806 0.19803921568627453))
     (crust     ,(ctp-color 0.6372549019607844 0.19540229885057467 0.17058823529411765)))
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
 'modus-themes-vivendi-palette
 'catppuccin-frappe-palette
 'catppuccin-frappe-palette-overrides
 'modus-catppuccin-custom-faces)

;;; catpuccin-frappe-theme.el ends here
