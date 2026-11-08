;;; pixel-themes-miri16-theme.el --- miri16 palette -*- lexical-binding: t -*-

;; Author: Lucas Oliveira <obxlucas@gmail.com>
;; URL: https://github.com/lucasobx/pixel-themes
;; Keywords: faces, theme
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; https://lospec.com/palette-list/miri16

;;; Code:

(require 'pixel-themes)

(defconst pixel-themes-miri16-palette
  (modus-themes-generate-palette
   '((cursor "#ac82b2")

     (bg-main     "#0d0d0a")
     (bg-dim      "#171714")
     (bg-alt      "#232320")
     (bg-active   "#342820")
     (bg-inactive "#191916")

     (fg-main "#ecd9d6")
     (fg-dim  "#86553b")
     (fg-alt  "#ad9688")

     (border "#3a2c20")

     (red      "#8e3a47")
     (red-warmer "#a04050")
     (red-cooler "#803045")
     (red-faint  "#a07080")

     (green        "#3f6050")
     (green-warmer "#508060")
     (green-cooler "#3a7060")
     (green-faint  "#5e8b9b")

     (yellow      "#9c7829")
     (yellow-warmer "#d8b47a")
     (yellow-cooler "#b09050")
     (yellow-faint  "#8a7050")

     (blue      "#7a7ac0")
     (blue-warmer "#5e8b9b")
     (blue-cooler "#6b6ab3")
     (blue-faint  "#7878a8")

     (magenta      "#ac82b2")
     (magenta-warmer "#9870a4")
     (magenta-cooler "#8878c0")
     (magenta-faint  "#ad9688")

     (cyan      "#5e8b9b")
     (cyan-warmer "#70a0b0")
     (cyan-cooler "#4a7888")
     (cyan-faint  "#7a9aaa")

     (bg-red-intense      "#4a0c1a")
     (bg-green-intense    "#1a3828")
     (bg-yellow-intense   "#4a3800")
     (bg-blue-intense     "#1a1a50")
     (bg-magenta-intense  "#3a1840")
     (bg-cyan-intense     "#003040")

     (bg-red-subtle     "#2a0010")
     (bg-green-subtle   "#081c10")
     (bg-yellow-subtle  "#2c2000")
     (bg-blue-subtle    "#0e0e2e")
     (bg-magenta-subtle "#1c0828")
     (bg-cyan-subtle    "#001828")

     ;; diff
     (bg-added          "#0e2818")
     (bg-added-faint    "#081c10")
     (bg-added-refine   "#183e20")
     (fg-added          "#88c888")
     (bg-changed        "#302400")
     (bg-changed-faint  "#201800")
     (bg-changed-refine "#403400")
     (fg-changed        "#d8c870")
     (bg-removed        "#300a14")
     (bg-removed-faint  "#200810")
     (bg-removed-refine "#48101c")
     (fg-removed        "#e8a8a8")

     ;; ui chrome
     (bg-mode-line-active "#232320")
     (fg-mode-line-active "#ecd9d6")
     (bg-completion       "#281e18")
     (bg-popup            "#111110")
     (bg-hover            "#3a2820")
     (bg-hover-secondary  "#281828")
     (bg-hl-line          "#1c1610")
     (bg-paren-match      "#382040")
     (bg-err              "#2e0810")
     (bg-warning          "#2a1e00")
     (bg-info             "#082010")
     (bg-region           "#433226"))

   'dark
   nil

   pixel-themes-palette-common))

(defcustom pixel-themes-miri16-palette-overrides nil
  "Overrides for `pixel-themes-miri16-palette'."
  :group 'pixel-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(apply #'modus-themes-theme
       (assq 'pixel-themes-miri16 pixel-themes-with-properties))

;;; pixel-themes-miri16-theme.el ends here
