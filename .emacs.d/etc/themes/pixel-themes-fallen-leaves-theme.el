;;; pixel-themes-fallen-leaves-theme.el --- fallen-leaves -*- lexical-binding:t -*-
;; ===============================================================
;;; Commentary:
;; https://lospec.com/palette-list/twelve-fallen-leaves
;;; Code:

(require-theme 'modus-themes)
(require 'pixel-themes)

(defvar pixel-themes-fallen-leaves-palette
  (modus-themes-generate-palette
   '((cursor "#a8a39d")

     (bg-main     "#171010")
     (bg-dim      "#201818")
     (bg-alt      "#382e2e")
     (bg-active   "#443838")
     (bg-inactive "#1e1616")

     (fg-main "#d4c9ba")
     (fg-dim  "#a8a39d")
     (fg-alt  "#b58b90")

     (border "#4b4b5e")

     (red      "#946663")
     (red-warmer "#a87570")
     (red-cooler "#885560")
     (red-faint  "#9a7878")

     (green      "#6d7a69")
     (green-warmer "#7a8a70")
     (green-cooler "#607870")
     (green-faint  "#9cb3b8")

     (yellow      "#797d91")
     (yellow-warmer "#bf9a84")
     (yellow-cooler "#8a8878")
     (yellow-faint  "#8a8878")

     (blue      "#aabfc4")
     (blue-warmer "#9cb3b8")
     (blue-cooler "#9cb3b8")
     (blue-faint  "#8aaab0")

     (magenta      "#b58b90")
     (magenta-warmer "#a87880")
     (magenta-cooler "#9880a0")
     (magenta-faint  "#a8a39d")

     (cyan      "#9cb3b8")
     (cyan-warmer "#aac0c4")
     (cyan-cooler "#8aaab0")
     (cyan-faint  "#90a8ac")

     (bg-red-intense      "#4a1818")
     (bg-green-intense    "#1a3020")
     (bg-yellow-intense   "#3a3418")
     (bg-blue-intense     "#1a2840")
     (bg-magenta-intense  "#3a1a2a")
     (bg-cyan-intense     "#0a2c34")

     (bg-red-subtle     "#280a0a")
     (bg-green-subtle   "#0a1c10")
     (bg-yellow-subtle  "#201e08")
     (bg-blue-subtle    "#0c1428")
     (bg-magenta-subtle "#200a18")
     (bg-cyan-subtle    "#001820")

     ;; diff
     (bg-added          "#0e2416")
     (bg-added-faint    "#081a0e")
     (bg-added-refine   "#163a1e")
     (fg-added          "#88c888")
     (bg-changed        "#2c2200")
     (bg-changed-faint  "#1c1600")
     (bg-changed-refine "#3c3000")
     (fg-changed        "#d8c870")
     (bg-removed        "#2e0e10")
     (bg-removed-faint  "#1e080a")
     (bg-removed-refine "#441418")
     (fg-removed        "#e8a8a8")

     ;; ui chrome
     (bg-mode-line-active "#382e2e")
     (fg-mode-line-active "#d4c9ba")
     (bg-completion       "#2e2430")
     (bg-popup            "#1c1414")
     (bg-hover            "#3a2c38")
     (bg-hover-secondary  "#2a2840")
     (bg-hl-line          "#201818")
     (bg-paren-match      "#3c2c48")
     (bg-err              "#320c10")
     (bg-warning          "#2c2000")
     (bg-info             "#081e10")
     (bg-region           "#4b4b5e"))

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

(defcustom pixel-themes-fallen-leaves-palette-overrides nil
  "Overrides for `pixel-themes-fallen-leaves-palette'."
  :group 'pixel-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(modus-themes-theme
 'pixel-themes-fallen-leaves
 'pixel-themes
 "Dark theme: muted autumn tones."
 'dark
 'pixel-themes-fallen-leaves-palette
 nil
 'pixel-themes-fallen-leaves-palette-overrides)

(provide-theme 'pixel-themes-fallen-leaves)
;;; pixel-themes-fallen-leaves-theme.el ends here
