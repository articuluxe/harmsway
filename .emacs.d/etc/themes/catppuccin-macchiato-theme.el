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
   `(
;;; Catppuccin values
     (rosewater ,(ctp-color 0.027777777777777676 0.5769230769230775 0.8980392156862745))
     (flamingo  ,(ctp-color 0.0 0.5833333333333333 0.8588235294117648))
     (pink      ,(ctp-color 0.8779761904761906 0.7368421052631583 0.8509803921568628))
     (mauve     ,(ctp-color 0.7403100775193798 0.8269230769230772 0.7960784313725491))
     (red       ,(ctp-color 0.9754901960784313 0.7391304347826088 0.7294117647058824))
     (maroon    ,(ctp-color 0.9862745098039216 0.7142857142857143 0.7666666666666666))
     (peach     ,(ctp-color 0.05932203389830507 0.8550724637681162 0.7294117647058824))
     (yellow    ,(ctp-color 0.1118143459915612 0.6991150442477877 0.7784313725490196))
     (green     ,(ctp-color 0.2922705314009662 0.4825174825174825 0.7196078431372549))
     (teal      ,(ctp-color 0.4752252252252252 0.46835443037974706 0.6901960784313725))
     (sky       ,(ctp-color 0.5243902439024389 0.5942028985507245 0.7294117647058823))
     (sapphire  ,(ctp-color 0.5517799352750808 0.6560509554140128 0.692156862745098))
     (blue      ,(ctp-color 0.6116352201257861 0.8281250000000003 0.7490196078431373))
     (lavender  ,(ctp-color 0.6512820512820513 0.8227848101265824 0.8450980392156863))
     (text      ,(ctp-color 0.6317829457364341 0.6825396825396831 0.8764705882352941))
     (subtext1  ,(ctp-color 0.6333333333333333 0.39215686274509803 0.8))
     (subtext0  ,(ctp-color 0.631578947368421 0.2676056338028167 0.7215686274509804))
     (overlay2  ,(ctp-color 0.6342592592592592 0.2000000000000001 0.6470588235294117))
     (overlay1  ,(ctp-color 0.6323529411764706 0.1545454545454545 0.5686274509803921))
     (overlay0  ,(ctp-color 0.6397849462365591 0.12350597609561753 0.49215686274509807))
     (surface2  ,(ctp-color 0.6379310344827586 0.13744075829383887 0.4137254901960784))
     (surface1  ,(ctp-color 0.6419753086419754 0.15606936416184972 0.3392156862745098))
     (surface0  ,(ctp-color 0.64 0.1879699248120301 0.2607843137254902))
     (base      ,(ctp-color 0.6439393939393939 0.23404255319148934 0.1843137254901961))
     (mantle    ,(ctp-color 0.6481481481481481 0.23076923076923075 0.15294117647058825))
     (crust     ,(ctp-color 0.6547619047619048 0.22580645161290322 0.12156862745098039)))
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
 'modus-themes-vivendi-palette
 'catppuccin-macchiato-palette
 'catppuccin-macchiato-palette-overrides
 'modus-catppuccin-custom-faces)

;;; catpuccin-macchiato-theme.el ends here
