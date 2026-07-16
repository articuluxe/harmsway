;;; pixel-themes-steam-lords-theme.el --- steam-lords -*- lexical-binding:t -*-
;; ===============================================================
;;; Commentary:
;; https://lospec.com/palette-list/steam-lords
;;; Code:

(require-theme 'modus-themes)
(require 'pixel-themes)

(defvar pixel-themes-steam-lords-palette
  (modus-themes-generate-palette
   '((cursor "#7c94a1")

     (bg-main     "#170e19") ;140c16
     (bg-dim      "#1c1420")
     (bg-alt      "#1f1422")
     (bg-active   "#2f213b")
     (bg-inactive "#181018")

     (fg-main "#c0d1cc")
     (fg-dim  "#7c94a1")
     (fg-alt  "#a0b9ba")

     (border "#2f213b")

     (red      "#603b3a")
     (red-warmer "#744848")
     (red-cooler "#583040")
     (red-faint  "#705858")

     (green      "#4f7754")
     (green-warmer "#5a8860")
     (green-cooler "#487060")
     (green-faint  "#65738c")

     (yellow      "#4f5277")
     (yellow-warmer "#a19f7c")
     (yellow-cooler "#686870")
     (yellow-faint  "#606070")

     (blue      "#7a9aac")
     (blue-warmer "#65738c")
     (blue-cooler "#65738c")
     (blue-faint  "#607088")

     (magenta      "#a19f7c")
     (magenta-warmer "#908e70")
     (magenta-cooler "#7878a0")
     (magenta-faint  "#989ea1")

     (cyan      "#65738c")
     (cyan-warmer "#748498")
     (cyan-cooler "#587080")
     (cyan-faint  "#607888")

     (bg-red-intense     "#380e10")
     (bg-green-intense   "#143820")
     (bg-yellow-intense  "#282a40")
     (bg-blue-intense    "#101e40")
     (bg-magenta-intense "#241440")
     (bg-cyan-intense    "#082030")

     (bg-red-subtle     "#1e0808")
     (bg-green-subtle   "#081808")
     (bg-yellow-subtle  "#141528")
     (bg-blue-subtle    "#080e28")
     (bg-magenta-subtle "#100828")
     (bg-cyan-subtle    "#041018")

     ;; diff
     (bg-added          "#0c2418")
     (bg-added-faint    "#081810")
     (bg-added-refine   "#103c20")
     (fg-added          "#88c888")
     (bg-changed        "#202240")
     (bg-changed-faint  "#141630")
     (bg-changed-refine "#2c3058")
     (fg-changed        "#d8c870")
     (bg-removed        "#280a10")
     (bg-removed-faint  "#180608")
     (bg-removed-refine "#3a0e18")
     (fg-removed        "#e8a8a8")

     ;; ui chrome
     (bg-mode-line-active "#1f1422")
     (fg-mode-line-active "#c0d1cc")
     (bg-completion       "#281838")
     (bg-popup            "#160e18")
     (bg-hover            "#2c1c3c")
     (bg-hover-secondary  "#201830")
     (bg-hl-line          "#1c1028")
     (bg-paren-match      "#302050")
     (bg-err              "#280810")
     (bg-warning          "#201c40")
     (bg-info             "#081c14")
     (bg-region           "#2f213b"))

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

(defcustom pixel-themes-steam-lords-palette-overrides nil
  "Overrides for `pixel-themes-steam-lords-palette'."
  :group 'pixel-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(modus-themes-theme
 'pixel-themes-steam-lords
 'pixel-themes
 "Dark theme: deep purples, cold blues and greens."
 'dark
 'pixel-themes-steam-lords-palette
 nil
 'pixel-themes-steam-lords-palette-overrides)

(provide-theme 'pixel-themes-steam-lords)
;;; pixel-themes-steam-lords-theme.el ends here
