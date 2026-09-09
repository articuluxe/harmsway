;;; modus-zenburn-theme.el --- Zenburn theme on Modus Vivendi Tinted -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kien Nguyen
;; Author: Kien Nguyen
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
;; The `modus-zenburn' theme applies Zenburn colors and semantic
;; mappings to Modus Vivendi Tinted.

;;; Code:

(require 'modus-zenburn)

;;;###theme-autoload
(modus-themes-theme
 'modus-zenburn
 'modus-zenburn
 "Zenburn colors and semantic mappings on Modus Vivendi Tinted."
 'dark
 'modus-themes-vivendi-tinted-palette
 'modus-zenburn-palette
 'modus-zenburn-palette-overrides)

(provide-theme 'modus-zenburn)
;;; modus-zenburn-theme.el ends here
