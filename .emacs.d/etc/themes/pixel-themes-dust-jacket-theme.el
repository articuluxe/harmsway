;;; pixel-themes-dust-jacket-theme.el --- dust-jacket palette -*- lexical-binding: t -*-

;; Author: Lucas Oliveira <obxlucas@gmail.com>
;; URL: https://github.com/lucasobx/pixel-themes
;; Keywords: faces, theme
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; https://lospec.com/palette-list/dust-jacket

;;; Code:

(require 'pixel-themes)

(defconst pixel-themes-dust-jacket-palette
  (modus-themes-generate-palette
   '((cursor "#be7740")

     (bg-main     "#0f0812")
     (bg-dim      "#24161e")
     (bg-alt      "#2a1922")
     (bg-active   "#4c3c34")
     (bg-inactive "#1c101a")

     (fg-main "#c0a87c")
     (fg-dim  "#846f5f")
     (fg-alt  "#ab935e")

     (border "#544f36")

     (red        "#af624a")
     (red-warmer "#be7740")
     (red-cooler "#a06575")
     (red-faint  "#9d5843")

     (green        "#748368")
     (green-warmer "#877f57")
     (green-cooler "#6a8872")
     (green-faint  "#66745b")

     (yellow        "#ab935e")
     (yellow-warmer "#b4854f")
     (yellow-cooler "#7e7651")
     (yellow-faint  "#916f46")

     (blue        "#819d88")
     (blue-warmer "#8d719b")
     (blue-cooler "#6a8872")
     (blue-faint  "#5a6c62")

     (magenta        "#a87381")
     (magenta-warmer "#ae6f68")
     (magenta-cooler "#9378a1")
     (magenta-faint  "#935b6a")

     (cyan        "#74937c")
     (cyan-warmer "#7b8c6f")
     (cyan-cooler "#6a8872")
     (cyan-faint  "#5d7266")

     (red-intense    "#c88e7c")
     (yellow-intense "#bba77c")
     (green-intense  "#9fac96")

     (bg-red-intense     "#3d2220")
     (bg-green-intense   "#30312d")
     (bg-yellow-intense  "#433127")
     (bg-blue-intense    "#2c3131")
     (bg-magenta-intense "#342028")
     (bg-cyan-intense    "#282c2d")

     (bg-red-subtle     "#29161a")
     (bg-green-subtle   "#211f21")
     (bg-yellow-subtle  "#332521")
     (bg-blue-subtle    "#232427")
     (bg-magenta-subtle "#24161e")
     (bg-cyan-subtle    "#212225")

     (bg-red-nuanced     "#190e15")
     (bg-green-nuanced   "#161118")
     (bg-yellow-nuanced  "#1f1418")
     (bg-blue-nuanced    "#18151c")
     (bg-magenta-nuanced "#1a0f18")
     (bg-cyan-nuanced    "#16121a")

     ;; diff
     (bg-added          "#2a2e2f")
     (bg-added-faint    "#1d1b20")
     (bg-added-refine   "#353e3a")
     (fg-added          "#96ad9c")
     (bg-changed        "#362722")
     (bg-changed-faint  "#22171a")
     (bg-changed-refine "#463328")
     (fg-changed        "#b6a274")
     (bg-removed        "#331c1d")
     (bg-removed-faint  "#221218")
     (bg-removed-refine "#482824")
     (fg-removed        "#c68b78")

     ;; ui chrome
     (bg-mode-line-active "#442a31")
     (fg-mode-line-active "#c0a87c")
     (modeline-err        "#be7740")
     (modeline-warning    "#ab935e")
     (modeline-info       "#6a8872")
     (bg-completion       "#3b241e")
     (bg-popup            "#271720")
     (bg-hover            "#2a2e2f")
     (bg-hover-secondary  "#321e1b")
     (bg-hl-line          "#1d1619")
     (bg-paren-match      "#472c21")
     (bg-err              "#381f1f")
     (bg-warning          "#3c2c24")
     (bg-info             "#2a2e2f")
     (bg-region           "#383328"))

   'warm
   nil

   (append
    '((builtin      green)
      (constant     red)
      (fnname       yellow)
      (preprocessor red-cooler)
      (string       red-warmer)
      (type         magenta-cooler)
      (variable     fg-main)

      (fg-prominent-err     red-intense)
      (fg-prominent-warning yellow-intense)
      (fg-prominent-note    green-intense)
      (fg-mark-delete       red-intense)
      (fg-mark-other        yellow-intense)
      (fg-mark-select       green-intense)
      (fg-active-argument   yellow-intense)
      (fg-active-value      green-intense)

      (rainbow-0 red-warmer)
      (rainbow-1 blue-cooler)
      (rainbow-2 yellow)
      (rainbow-3 green)
      (rainbow-4 magenta)
      (rainbow-5 cyan)
      (rainbow-6 red)
      (rainbow-7 magenta-cooler)
      (rainbow-8 yellow-faint))
    pixel-themes-palette-common)))

(defcustom pixel-themes-dust-jacket-palette-overrides nil
  "Overrides for `pixel-themes-dust-jacket-palette'."
  :group 'pixel-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(apply #'modus-themes-theme
       (assq 'pixel-themes-dust-jacket pixel-themes-with-properties))

;;; pixel-themes-dust-jacket-theme.el ends here
