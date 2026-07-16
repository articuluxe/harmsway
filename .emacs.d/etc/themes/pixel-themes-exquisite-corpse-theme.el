;;; pixel-themes-exquisite-corpse-theme.el --- exquisite-corpse -*- lexical-binding:t -*-
;; ===============================================================
;;; Commentary:
;; https://lospec.com/palette-list/exquisite-corpse
;;; Code:

(require-theme 'modus-themes)
(require 'pixel-themes)

(defvar pixel-themes-exquisite-corpse-palette
  (modus-themes-generate-palette
   '((cursor "#74a5bc")

     (bg-main     "#000000")
     (bg-dim      "#080608")
     (bg-alt      "#0e0c18")
     (bg-active   "#1c155f")
     (bg-inactive "#050408")

     (fg-main "#e8dbcb")
     (fg-dim  "#d2c19c")
     (fg-alt  "#a38152")

     (border "#232a73")

     (red        "#8a4235")
     (red-warmer "#a05040")
     (red-cooler "#6e3228")
     (red-faint  "#5e2a20")

     (green        "#a38152")
     (green-warmer "#b89060")
     (green-cooler "#8a6a40")
     (green-faint  "#6a5030")

     (yellow        "#d2c19c")
     (yellow-warmer "#e8dbcb")
     (yellow-cooler "#a38152")
     (yellow-faint  "#7a6040")

     (blue        "#74a5bc")
     (blue-warmer "#8abcd0")
     (blue-cooler "#5273a3")
     (blue-faint  "#354a8a")

     (magenta        "#5273a3")
     (magenta-warmer "#6a88b8")
     (magenta-cooler "#354a8a")
     (magenta-faint  "#232a73")

     (cyan        "#74a5bc")
     (cyan-warmer "#8abcd0")
     (cyan-cooler "#5a8aa0")
     (cyan-faint  "#3a6880")

     (bg-red-intense     "#3a1008")
     (bg-green-intense   "#56320e")
     (bg-yellow-intense  "#2e2200")
     (bg-blue-intense    "#0c1840")
     (bg-magenta-intense "#2a0033")
     (bg-cyan-intense    "#082030")

     (bg-red-subtle     "#1e0804")
     (bg-green-subtle   "#2e1a08")
     (bg-yellow-subtle  "#181200")
     (bg-blue-subtle    "#060e28")
     (bg-magenta-subtle "#160018")
     (bg-cyan-subtle    "#041018")

     (bg-added          "#0c1830")
     (bg-added-faint    "#060e18")
     (bg-added-refine   "#142440")
     (fg-added          "#74a5bc")
     (bg-changed        "#56320e")
     (bg-changed-faint  "#2e1a08")
     (bg-changed-refine "#6e4018")
     (fg-changed        "#e8dbcb")
     (bg-removed        "#2a0033")
     (bg-removed-faint  "#160018")
     (bg-removed-refine "#3c0048")
     (fg-removed        "#d2c19c")

     (bg-mode-line-active "#1c155f")
     (fg-mode-line-active "#e8dbcb")
     (bg-completion       "#0c0e28")
     (bg-popup            "#080a18")
     (bg-hover            "#141830")
     (bg-hover-secondary  "#2a0033")
     (bg-hl-line          "#0a0818")
     (bg-paren-match      "#082030")
     (bg-err              "#1e0804")
     (bg-warning          "#2e1a08")
     (bg-info             "#0c1830")
     (bg-region           "#1c155f"))

   'dark
   nil

   (append
    '((builtin      blue)
      (keyword      magenta)
      (fnname       blue-warmer)
      (fnname-call  blue-cooler)
      (type         yellow)
      (string       yellow-warmer)
      (comment      red)
      (docstring    green)
      (variable     fg-main)
      (constant     blue)
      (preprocessor magenta-cooler)
      (rx-construct blue)
      (rx-backslash blue-cooler)

      (accent-0 blue)
      (accent-1 magenta)
      (accent-2 yellow)
      (accent-3 red)

      (keybind        blue)
      (fg-prompt      blue)
      (fg-link        blue)
      (fg-link-visited magenta)

      (fg-prose-macro  magenta)
      (mail-subject    blue)
      (mail-recipient  blue-warmer)
      (mail-cite-0     blue-warmer)
      (mail-cite-1     yellow)
      (mail-cite-2     magenta)
      (mail-cite-3     blue-cooler)

      (rainbow-0 yellow-warmer)
      (rainbow-1 blue)
      (rainbow-2 yellow)
      (rainbow-3 magenta)
      (rainbow-4 blue-warmer)
      (rainbow-5 yellow-cooler)
      (rainbow-6 magenta-cooler)
      (rainbow-7 blue-cooler)
      (rainbow-8 red))
    pixel-themes-palette-common)))

(defcustom pixel-themes-exquisite-corpse-palette-overrides nil
  "Overrides for `pixel-themes-exquisite-corpse-palette'."
  :group 'pixel-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(modus-themes-theme
 'pixel-themes-exquisite-corpse
 'pixel-themes
 "Dark theme: faded parchment and dreamlike blues on black."
 'dark
 'pixel-themes-exquisite-corpse-palette
 nil
 'pixel-themes-exquisite-corpse-palette-overrides)

(provide-theme 'pixel-themes-exquisite-corpse)
;;; pixel-themes-exquisite-corpse-theme.el ends here
