;;; pixel-themes-psygnosia-theme.el --- psygnosia -*- lexical-binding:t -*-
;; ===============================================================
;;; Commentary:
;; https://lospec.com/palette-list/psygnosia
;;; Code:

(require-theme 'modus-themes)
(require 'pixel-themes)

(defvar pixel-themes-psygnosia-palette
  (modus-themes-generate-palette
   '((cursor "#516cbf")

     (bg-main     "#000000")
     (bg-dim      "#1b1e29")
     (bg-alt      "#362747")
     (bg-active   "#443f41")
     (bg-inactive "#0e1018")

     (fg-main "#cbe8f7")
     (fg-dim  "#9ea4a7")
     (fg-alt  "#64647c")

     (border "#52524c")

     (red      "#a2324e")
     (red-warmer "#e08b79")
     (red-cooler "#8a2040")
     (red-faint  "#7a4050")

     (green      "#546a00")
     (green-warmer "#607a10")
     (green-cooler "#084a3c")
     (green-faint  "#77785b")

     (yellow      "#77785b")
     (yellow-warmer "#9ea4a7")
     (yellow-cooler "#736150")
     (yellow-faint  "#52524c")

     (blue      "#516cbf")
     (blue-warmer "#64647c")
     (blue-cooler "#3a5aaa")
     (blue-faint  "#4a5888")

     (magenta      "#64647c")
     (magenta-warmer "#7a6878")
     (magenta-cooler "#505070")
     (magenta-faint  "#52524c")

     (cyan      "#084a3c")
     (cyan-warmer "#0a5a4a")
     (cyan-cooler "#003330")
     (cyan-faint  "#1b2e2a")

     (bg-red-intense     "#3a0818")
     (bg-green-intense   "#003308")
     (bg-yellow-intense  "#2e2a10")
     (bg-blue-intense    "#101838")
     (bg-magenta-intense "#221830")
     (bg-cyan-intense    "#002820")

     (bg-red-subtle     "#220410")
     (bg-green-subtle   "#001c04")
     (bg-yellow-subtle  "#1c1a08")
     (bg-blue-subtle    "#080e24")
     (bg-magenta-subtle "#14101e")
     (bg-cyan-subtle    "#001814")

     ;; diff
     (bg-added          "#0a2010")
     (bg-added-faint    "#041608")
     (bg-added-refine   "#103a18")
     (fg-added          "#77785b")
     (bg-changed        "#1e1c10")
     (bg-changed-faint  "#141208")
     (bg-changed-refine "#2c2a18")
     (fg-changed        "#9ea4a7")
     (bg-removed        "#280810")
     (bg-removed-faint  "#18040c")
     (bg-removed-refine "#3a0c18")
     (fg-removed        "#e08b79")

     ;; ui chrome
     (bg-mode-line-active "#1b1e29")
     (fg-mode-line-active "#cbe8f7")
     (bg-completion       "#1e1c2a")
     (bg-popup            "#0e0e18")
     (bg-hover            "#2a2838")
     (bg-hover-secondary  "#1e1c2e")
     (bg-hl-line          "#131220")
     (bg-paren-match      "#2a3860")
     (bg-err              "#280810")
     (bg-warning          "#1e1c10")
     (bg-info             "#001c08")
     (bg-region           "#362747"))

   'dark
   nil

   (append
    '(;; theme-specific mappings
      (builtin      blue-warmer) ; blue
      (keyword      blue-warmer) ; blue
      (fnname       fg-main)
      (type         green)
      (string       fg-dim)
      (comment      yellow-faint)
      (docstring    green-faint)
      (variable     fg-main)
      (constant     red-warmer)
      (preprocessor blue-cooler)
      (rx-construct red)
      (rx-backslash red-faint)

      (rainbow-0 blue)
      (rainbow-1 green)
      (rainbow-2 red-cooler)
      (rainbow-3 green-warmer)
      (rainbow-4 blue-cooler)
      (rainbow-5 red-faint)
      (rainbow-6 green-faint)
      (rainbow-7 blue-faint)
      (rainbow-8 red))
    pixel-themes-palette-common)))

(defcustom pixel-themes-psygnosia-palette-overrides nil
  "Overrides for `pixel-themes-psygnosia-palette'."
  :group 'pixel-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(modus-themes-theme
 'pixel-themes-psygnosia
 'pixel-themes
 "Dark theme: copper skies over a muddy range."
 'dark
 'pixel-themes-psygnosia-palette
 nil
 'pixel-themes-psygnosia-palette-overrides)

(provide-theme 'pixel-themes-psygnosia)
;;; pixel-themes-psygnosia-theme.el ends here
