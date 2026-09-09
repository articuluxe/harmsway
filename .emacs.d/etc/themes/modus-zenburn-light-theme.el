;;; modus-zenburn-light-theme.el --- Light Zenburn adaptation on Modus Operandi -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kien Nguyen
;; Author: Kien Nguyen
;; URL: https://github.com/kiennq/modus-zenburn
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
;; The `modus-zenburn-light' theme provides a light adaptation of
;; Zenburn colors and semantic mappings on Modus Operandi Tinted.

;;; Code:

(require 'modus-zenburn)

;;;###theme-autoload
(modus-themes-theme
 'modus-zenburn-light
 'modus-zenburn
 "Original light adaptation of Zenburn colors and semantic mappings."
 'light
 'modus-themes-operandi-tinted-palette
 'modus-zenburn-light-palette
 'modus-zenburn-light-palette-overrides)

(provide-theme 'modus-zenburn-light)
;;; modus-zenburn-light-theme.el ends here
