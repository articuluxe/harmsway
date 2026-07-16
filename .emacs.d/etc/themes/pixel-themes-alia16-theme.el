;;; pixel-themes-alia16-theme.el --- alia16 -*- lexical-binding:t -*-
;; ===============================================================
;;; Commentary:
;; https://lospec.com/palette-list/alia16
;;; Code:

(require-theme 'modus-themes)
(require 'pixel-themes)

(defvar pixel-themes-alia16-palette
  (modus-themes-generate-palette
   '((cursor "#a7a8dd")

     (bg-main     "#101112")
     (bg-dim      "#1a1a1c")
     (bg-alt      "#262829")
     (bg-active   "#3a2c3a")
     (bg-inactive "#1c1c1e")

     (fg-main "#ccd2c9")
     (fg-dim  "#7f7266")
     (fg-alt  "#a7a8dd")

     (border "#3d2d3d")

     (red      "#ca475a")
     (red-warmer "#d05060")
     (red-cooler "#c04060")
     (red-faint  "#b07080")

     (green      "#458855")
     (green-warmer "#5a9a60")
     (green-cooler "#3a8060")
     (green-faint  "#7a889a")

     (yellow      "#ab933e")
     (yellow-warmer "#eeb487")
     (yellow-cooler "#c0a060")
     (yellow-faint  "#9a8060")

     (blue      "#6070b8")
     (blue-warmer "#7a889a")
     (blue-cooler "#47559d")
     (blue-faint  "#6878a0")

     (magenta      "#e166bc")
     (magenta-warmer "#d060b0")
     (magenta-cooler "#a07ad0")
     (magenta-faint  "#a7a8dd")

     (cyan      "#60a0b8")
     (cyan-warmer "#7ab0c0")
     (cyan-cooler "#4a90a8")
     (cyan-faint  "#7a889a")

     (bg-red-intense     "#6a1a2a")
     (bg-green-intense   "#1e4a28")
     (bg-yellow-intense  "#5a4010")
     (bg-blue-intense    "#1e2a60")
     (bg-magenta-intense "#5a1a5a")
     (bg-cyan-intense    "#0a3848")

     (bg-red-subtle     "#3a0a1a")
     (bg-green-subtle   "#0a2a1a")
     (bg-yellow-subtle  "#3a2a00")
     (bg-blue-subtle    "#141840")
     (bg-magenta-subtle "#300a30")
     (bg-cyan-subtle    "#001c30")

     ;; diff
     (bg-added          "#102a18")
     (bg-added-faint    "#0a1e10")
     (bg-added-refine   "#1a4020")
     (fg-added          "#90d090")
     (bg-changed        "#3a2c00")
     (bg-changed-faint  "#281e00")
     (bg-changed-refine "#504000")
     (fg-changed        "#e0d080")
     (bg-removed        "#3e1018")
     (bg-removed-faint  "#2a0a10")
     (bg-removed-refine "#5a1820")
     (fg-removed        "#f0b0b0")

     ;; ui chrome
     (bg-mode-line-active "#262829")
     (fg-mode-line-active "#ccd2c9")
     (bg-completion       "#2a1a2a")
     (bg-popup            "#141416")
     (bg-hover            "#3a2040")
     (bg-hover-secondary  "#2a2840")
     (bg-hl-line          "#1e1420")
     (bg-paren-match      "#402850")
     (bg-err              "#3e0a18")
     (bg-warning          "#382800")
     (bg-info             "#0a2818")
     (bg-region           "#512450"))

   'dark
   nil

   (append
    '(;; theme-specific mappings
      (constant  red-faint)

      ;; rainbow
      (rainbow-0 magenta)
      (rainbow-1 blue-cooler)
      (rainbow-2 yellow-warmer)
      (rainbow-3 green)
      (rainbow-4 cyan)
      (rainbow-5 magenta-cooler)
      (rainbow-6 red-cooler)
      (rainbow-7 green-faint)
      (rainbow-8 yellow))
    pixel-themes-palette-common)))

(defcustom pixel-themes-alia16-palette-overrides nil
  "Overrides for `pixel-themes-alia16-palette'."
  :group 'pixel-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(modus-themes-theme
 'pixel-themes-alia16
 'pixel-themes
 "Dark theme: pink, indigo, warm gold and earthy greens."
 'dark
 'pixel-themes-alia16-palette
 nil
 'pixel-themes-alia16-palette-overrides)

(provide-theme 'pixel-themes-alia16)
;;; pixel-themes-alia16-theme.el ends here
