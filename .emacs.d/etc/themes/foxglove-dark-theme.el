;;; foxglove-dark-theme.el --- Foxglove Dark theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Vegas Harmon

;; Author: Vegas Harmon <vegasharmon@protonmail.com>
;; URL: https://gitlab.com/vegasharmon/foxglove-themes

;; This file is not part of GNU Emacs.  It is free software under the GNU
;; General Public License, version 3 or later; see foxglove-themes.el.

;;; Commentary:

;; Foxglove Dark: the Foxglove inks at mirrored lightness on warm near-black.
;; Options and design notes are in foxglove-themes.el.  This file is
;; written by tools/palette.py.

;;; Code:

(require 'foxglove-themes)

;;;###theme-autoload
(foxglove-themes-define
 'foxglove-dark 'linen 'dark
 "Foxglove Dark: the Foxglove inks at mirrored lightness on warm near-black.")

(provide 'foxglove-dark-theme)

;;; foxglove-dark-theme.el ends here
