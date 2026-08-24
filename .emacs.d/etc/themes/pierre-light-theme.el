;;; pierre-light-theme.el --- Pierre Light theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Pierre Light, ported from @pierre/theme as used by https://diffs.com/.

;;; Code:

(require 'pierre-themes)

;;;###theme-autoload
(deftheme pierre-light
  "Pierre Light theme, ported from @pierre/theme.")

(pierre-themes--apply 'pierre-light pierre-themes--light-palette nil)

(provide-theme 'pierre-light)

;;; pierre-light-theme.el ends here
