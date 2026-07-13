;;; modus-nordic-midnight-theme.el --- Pitch-black version of Nordic Night  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Ashton Wiersdorf

;; Author: Ashton Wiersdorf <mail@wiersdorf.dev>
;; Package-Requires: ((emacs "28.1") (modus-themes "5.2.0"))
;; Homepage: https://codeberg.org/ashton314/modus-nordic-night

;;; Commentary:
;; This provides an even darker, higher-contrast version of the Nordic
;; Night theme. The background is pure black and some of the colors
;; are slightly muted to match. This is what the author uses!

;;; Code:

(require 'modus-nordic-night-theme)

(defcustom modus-nordic-midnight-theme-palette-overrides nil
  "Overrides for nordic-midnight palette."
  :group 'modus-nordic-night-theme
  :type '(repeat (list symbol (choice symbol string))))

(defvar modus-nordic-midnight-theme-palette-partial
  `((bg-mode-line-inactive ,(color-darken-name "#5e81ac" 80))
    (fringe ,(color-darken-name "#434c5e" 80))

    ;; hl-line, line numbers
    (bg-hl-line ,(color-darken-name "#5e81ac" 75))
    (bg-line-number-active ,(color-darken-name "#5e81ac" 75))
    (bg-line-number-inactive ,(color-darken-name "#434c5e" 80))))

(defvar modus-nordic-midnight-theme-palette
  (modus-themes-generate-palette
   '(
     ;; Background colors
     (bg-07 "#3f4f4f")
     (bg-08 "#3d5056")
     (bg-09 "#3b4551")
     (bg-10 "#233949")
     (bg-11 "#512e31")
     (bg-12 "#573d35")
     (bg-13 "#61553d")
     (bg-14 "#46503e")
     (bg-15 "#4c3e4a")

     ;; Extended nord colors
     (nn-dark1 "#000000")
     (nn-dark2 "#122a19d4233d"); (color-darken-name "#5e81ac" 80)
     (nn-dark3 "#1b4026be34dc"); (color-darken-name "#5e81ac" 70)
     (nn-dark4 "#245533a8467b"); (color-darken-name "#5e81ac" 60)
     ;; (nn-dark2 "#121212")
     ;; (nn-dark3 "#1c1c1c")
     (nn-light1 "#6b7386")
     (nn-light2 "#8892a4")
     (nn-light3 "#c6c6cf")

     ;; Main theme colors
     (bg-main "#000000")                 ; nn-dark1
     (bg-dim  "#122a19d4233d")           ; nn-dark2
     (bg-alt  "#1b4026be34dc")
     (fg-main "#d8dee9")
     (fg-dim  "#8892a4")
     (fg-alt  "#b5bdcc")

     ;; Base theme colors
     (red "#bf616a")
     (green "#a3be8c")
     (yellow "#ebcb8b")
     (yellow-warmer "#d08770")
     (blue "#5e81ac")
     (blue-warmer "#81a1c1")
     (blue-faint "#8fbcbb")
     (cyan "#88c0d0")
     (magenta "#b48ead"))
   'cool))

;; Nordic-Midnight theme
(modus-themes-theme
 'modus-nordic-midnight
 'nordic-themes
 "Pitch-black version of the Nord theme."
 'dark
 'modus-nordic-midnight-theme-palette
 'modus-nordic-night-theme-palette-partial
 'modus-nordic-midnight-theme-palette-overrides
 'modus-nordic-night-theme-custom-faces)

(provide 'modus-nordic-midnight-theme)

;;; modus-nordic-midnight-theme.el ends here
