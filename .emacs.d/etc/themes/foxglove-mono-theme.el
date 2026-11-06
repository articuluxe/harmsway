;;; foxglove-mono-theme.el --- Foxglove Mono theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Vegas Harmon

;; Author: Vegas Harmon <vegasharmon@protonmail.com>
;; URL: https://gitlab.com/vegasharmon/foxglove-themes

;; This file is not part of GNU Emacs.  It is free software under the GNU
;; General Public License, version 3 or later; see foxglove-themes.el.

;;; Commentary:

;; Foxglove Mono: exact e-ink greys, no color, black on white.
;; Options and design notes are in foxglove-themes.el.  This file is
;; written by tools/palette.py.

;;; Code:

(require 'foxglove-themes)

;;;###theme-autoload
(foxglove-themes-define
 'foxglove-mono 'mono 'light
 "Foxglove Mono: exact e-ink greys, no color, black on white.")

(provide 'foxglove-mono-theme)

;;; foxglove-mono-theme.el ends here
