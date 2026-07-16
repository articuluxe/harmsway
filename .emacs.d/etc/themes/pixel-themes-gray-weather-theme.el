;;; pixel-themes-gray-weather-theme.el --- gray-weather -*- lexical-binding:t -*-
;; ===============================================================
;;; Commentary:
;; https://lospec.com/palette-list/gray-weather
;;; Code:

(require-theme 'modus-themes)
(require 'pixel-themes)

(defvar pixel-themes-gray-weather-palette
  (modus-themes-generate-palette
   '((cursor "#989ea1")

     (bg-main     "#201921") ; #120c12
     (bg-dim      "#1c141c")
     (bg-alt      "#3e3042")
     (bg-active   "#4a3c50")
     (bg-inactive "#181018")

     (fg-main "#bcafb6")
     (fg-dim  "#7d7688")
     (fg-alt  "#928d9a")

     (border "#503f58")

     (red      "#98585b")
     (red-warmer "#a86868")
     (red-cooler "#884858")
     (red-faint  "#906870")

     (green      "#758672")
     (green-warmer "#849670")
     (green-cooler "#688070")
     (green-faint  "#768c9c")

     (yellow      "#a09b72")
     (yellow-warmer "#b1a599")
     (yellow-cooler "#908c6a")
     (yellow-faint  "#8a8468")

     (blue      "#8aa0b0")
     (blue-warmer "#768c9c")
     (blue-cooler "#768c9c")
     (blue-faint  "#6a8090")

     (magenta      "#a67c6a")
     (magenta-warmer "#986860")
     (magenta-cooler "#887888")
     (magenta-faint  "#989ea1")

     (cyan      "#768c9c")
     (cyan-warmer "#8498a8")
     (cyan-cooler "#688090")
     (cyan-faint  "#708490")

     (bg-red-intense     "#481418")
     (bg-green-intense   "#183020")
     (bg-yellow-intense  "#383010")
     (bg-blue-intense    "#182040")
     (bg-magenta-intense "#341430")
     (bg-cyan-intense    "#082838")

     (bg-red-subtle     "#260808")
     (bg-green-subtle   "#081808")
     (bg-yellow-subtle  "#1e1c08")
     (bg-blue-subtle    "#0c1028")
     (bg-magenta-subtle "#1c0820")
     (bg-cyan-subtle    "#001420")

     ;; diff
     (bg-added          "#0c2014")
     (bg-added-faint    "#081610")
     (bg-added-refine   "#14381c")
     (fg-added          "#88c888")
     (bg-changed        "#282000")
     (bg-changed-faint  "#181400")
     (bg-changed-refine "#382c00")
     (fg-changed        "#d8c870")
     (bg-removed        "#2a0c10")
     (bg-removed-faint  "#1c0808")
     (bg-removed-refine "#3e1014")
     (fg-removed        "#e8a8a8")

     ;; ui chrome
     (bg-mode-line-active "#3e3042")
     (fg-mode-line-active "#bcafb6")
     (bg-completion       "#342840")
     (bg-popup            "#181018")
     (bg-hover            "#3a2c48")
     (bg-hover-secondary  "#2a2840")
     (bg-hl-line          "#1c1420")
     (bg-paren-match      "#3a2850")
     (bg-err              "#2e0c10")
     (bg-warning          "#281e00")
     (bg-info             "#081c10")
     (bg-region           "#503f58"))

   'dark
   nil

   (append
    '(;; theme-specific mappings
      (constant  magenta-faint)

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

(defcustom pixel-themes-gray-weather-palette-overrides nil
  "Overrides for `pixel-themes-gray-weather-palette'."
  :group 'pixel-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(modus-themes-theme
 'pixel-themes-gray-weather
 'pixel-themes
 "Dark theme: cold and sad, with a quiet beauty."
 'dark
 'pixel-themes-gray-weather-palette
 nil
 'pixel-themes-gray-weather-palette-overrides)

(provide-theme 'pixel-themes-gray-weather)
;;; pixel-themes-gray-weather-theme.el ends here
