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
   `(
;;; Catppuccin values
     (rosewater ,(ctp-color 0.026666666666666578 0.555555555555556 0.911764705882353))
     (flamingo  ,(ctp-color 0.0 0.587301587301587 0.8764705882352941))
     (pink      ,(ctp-color 0.8790849673202613 0.7183098591549301 0.8607843137254902))
     (mauve     ,(ctp-color 0.742798353909465 0.8350515463917528 0.8098039215686275))
     (red       ,(ctp-color 0.9535256410256411 0.8124999999999998 0.7490196078431373))
     (maroon    ,(ctp-color 0.9733333333333333 0.6521739130434779 0.7745098039215685))
     (peach     ,(ctp-color 0.06376811594202897 0.92 0.7549019607843137))
     (yellow    ,(ctp-color 0.11486486486486486 0.8604651162790699 0.8313725490196078))
     (green     ,(ctp-color 0.32070707070707066 0.5409836065573769 0.7607843137254902))
     (teal      ,(ctp-color 0.4722222222222223 0.5735294117647057 0.7333333333333334))
     (sky       ,(ctp-color 0.5255102040816326 0.7101449275362316 0.7294117647058823))
     (sapphire  ,(ctp-color 0.5513888888888889 0.759493670886076 0.6901960784313725))
     (blue      ,(ctp-color 0.603244837758112 0.9186991869918699 0.7588235294117647))
     (lavender  ,(ctp-color 0.6441441441441441 0.9736842105263159 0.8509803921568628))
     (text      ,(ctp-color 0.6282051282051282 0.6393442622950825 0.8803921568627451))
     (subtext1  ,(ctp-color 0.6296296296296297 0.35294117647058837 0.8))
     (subtext0  ,(ctp-color 0.6323529411764706 0.23611111111111102 0.7176470588235294))
     (overlay2  ,(ctp-color 0.6344086021505376 0.16756756756756758 0.6372549019607843))
     (overlay1  ,(ctp-color 0.6379310344827586 0.12775330396475776 0.5549019607843138))
     (overlay0  ,(ctp-color 0.6410256410256411 0.10743801652892565 0.4745098039215686))
     (surface2  ,(ctp-color 0.6458333333333334 0.12 0.39215686274509803))
     (surface1  ,(ctp-color 0.6507936507936508 0.13207547169811326 0.31176470588235294))
     (surface0  ,(ctp-color 0.6578947368421053 0.16239316239316234 0.22941176470588237))
     (base      ,(ctp-color 0.6666666666666666 0.21052631578947367 0.14901960784313725))
     (mantle    ,(ctp-color 0.6666666666666666 0.2131147540983607 0.11960784313725491))
     (crust     ,(ctp-color 0.6666666666666666 0.22727272727272727 0.08627450980392157)))
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
 'modus-themes-vivendi-palette
 'catppuccin-mocha-palette
 'catppuccin-mocha-palette-overrides
 'modus-catppuccin-custom-faces)

;;; catpuccin-mocha-theme.el ends here
