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
   `(
;;; Catppuccin values
     (rosewater ,(ctp-color 0.02999999999999999 0.5882352941176472 0.6666666666666667))
     (flamingo  ,(ctp-color 0.0 0.5976331360946746 0.6686274509803922))
     (pink      ,(ctp-color 0.8778735632183908 0.7341772151898731 0.6901960784313725))
     (mauve     ,(ctp-color 0.7390109890109892 0.8504672897196262 0.5803921568627451))
     (red       ,(ctp-color 0.9641025641025641 0.8666666666666666 0.4411764705882353))
     (maroon    ,(ctp-color 0.9855072463768114 0.76303317535545 0.5862745098039216))
     (peach     ,(ctp-color 0.061042524005486966 0.9918367346938776 0.5196078431372549))
     (yellow    ,(ctp-color 0.0970790378006873 0.7698412698412698 0.49411764705882355))
     (green     ,(ctp-color 0.3034188034188034 0.5763546798029556 0.39803921568627454))
     (teal      ,(ctp-color 0.5089743589743589 0.7386363636363636 0.34509803921568627))
     (sky       ,(ctp-color 0.5474074074074075 0.965665236051502 0.45686274509803926))
     (sapphire  ,(ctp-color 0.5246085011185683 0.6995305164319249 0.4176470588235294))
     (blue      ,(ctp-color 0.6108527131782946 0.9148936170212768 0.5392156862745098))
     (lavender  ,(ctp-color 0.6414868105515588 0.9720279720279721 0.7196078431372549))
     (text      ,(ctp-color 0.6494252873563219 0.16022099447513813 0.3549019607843137))
     (subtext1  ,(ctp-color 0.6481481481481481 0.1279620853080569 0.4137254901960784))
     (subtext0  ,(ctp-color 0.6466666666666666 0.10373443983402494 0.4725490196078431))
     (overlay2  ,(ctp-color 0.644927536231884 0.09623430962343092 0.5313725490196078))
     (overlay1  ,(ctp-color 0.6428571428571429 0.10047846889952144 0.5901960784313726))
     (overlay0  ,(ctp-color 0.6333333333333334 0.11235955056179768 0.6509803921568628))
     (surface2  ,(ctp-color 0.6296296296296298 0.12162162162162159 0.7098039215686275))
     (surface1  ,(ctp-color 0.6250000000000001 0.13559322033898308 0.7686274509803922))
     (surface0  ,(ctp-color 0.6190476190476192 0.1590909090909089 0.8274509803921568))
     (base      ,(ctp-color 0.6111111111111114 0.23076923076923136 0.9490196078431372))
     (mantle    ,(ctp-color 0.6111111111111113 0.21951219512195116 0.919607843137255))
     (crust     ,(ctp-color 0.6111111111111113 0.20689655172413762 0.8862745098039215)))
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
 'latte-palette-overrides
 'modus-catppuccin-custom-faces)

;;; catpuccin-latte-theme.el ends here
