;;; pierre-dark-theme.el --- Pierre Dark theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Pierre Dark, ported from @pierre/theme as used by https://diffs.com/.

;;; Code:

(require 'pierre-themes)

;;;###theme-autoload
(deftheme pierre-dark
  "Pierre Dark theme, ported from @pierre/theme.")

(pierre-themes--apply 'pierre-dark pierre-themes--dark-palette t)

(provide-theme 'pierre-dark)

;;; pierre-dark-theme.el ends here
