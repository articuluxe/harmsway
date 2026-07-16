;;; pixel-themes-gothic-temple-theme.el --- gothic-temple -*- lexical-binding:t -*-
;; ===============================================================
;;; Commentary:
;; https://lospec.com/palette-list/gothic-temple
;;; Code:

(require-theme 'modus-themes)
(require 'pixel-themes)

(defvar pixel-themes-gothic-temple-palette
  (modus-themes-generate-palette
   '((cursor "#500d06")

     (bg-main     "#d6cdd4")
     (bg-dim      "#cdc4cc")
     (bg-alt      "#c0b8c2")
     (bg-active   "#b4a8b6")
     (bg-inactive "#d0c8ce")

     (fg-main "#000000")
     (fg-dim  "#494151")
     (fg-alt  "#676375")

     (border "#8e8496")

     (red      "#500d06")
     (red-warmer "#7c3b17")
     (red-cooler "#500d06")
     (red-faint  "#7c3b17")

     (green      "#676375")
     (green-warmer "#494151")
     (green-cooler "#676375")
     (green-faint  "#eeeeb4")

     (yellow      "#7c3b17")
     (yellow-warmer "#500d06")
     (yellow-cooler "#7c3b17")
     (yellow-faint  "#dcbf81")

     (blue      "#494151")
     (blue-warmer "#676375")
     (blue-cooler "#494151")
     (blue-faint  "#8e8496")

     (magenta      "#500d06")
     (magenta-warmer "#7c3b17")
     (magenta-cooler "#500d06")
     (magenta-faint  "#dda082")

     (cyan      "#676375")
     (cyan-warmer "#8e8496")
     (cyan-cooler "#494151")
     (cyan-faint  "#8e8496")

     (bg-red-intense     "#d88282")
     (bg-green-intense   "#b4a8b6")
     (bg-yellow-intense  "#eeeeb4")
     (bg-blue-intense    "#b4a8b6")
     (bg-magenta-intense "#dda082")
     (bg-cyan-intense    "#b4a8b6")

     (bg-red-subtle     "#f0d0d0")
     (bg-green-subtle   "#d4d0d8")
     (bg-yellow-subtle  "#f4f0cc")
     (bg-blue-subtle    "#d0cedd")
     (bg-magenta-subtle "#f0ddd4")
     (bg-cyan-subtle    "#d4d2dc")

     (bg-added          "#d0d8cc")
     (bg-added-faint    "#dce4d8")
     (bg-added-refine   "#beccb8")
     (fg-added          "#494151")
     (bg-changed        "#eeeeb4")
     (bg-changed-faint  "#f4f2cc")
     (bg-changed-refine "#dcbf81")
     (fg-changed        "#500d06")
     (bg-removed        "#d88282")
     (bg-removed-faint  "#f0d0d0")
     (bg-removed-refine "#c77b51")
     (fg-removed        "#500d06")

     (bg-mode-line-active "#8e8496")
     (fg-mode-line-active "#ffffff")
     (bg-completion       "#c0b8c2")
     (bg-popup            "#f0d0d0")
     (bg-hover            "#c4bcc4")
     (bg-hover-secondary  "#dda082")
     (bg-hl-line          "#cec6cc")
     (bg-paren-match      "#eeeeb4")
     (bg-err              "#d88282")
     (bg-warning          "#eeeeb4")
     (bg-info             "#d0d8cc")
     (bg-region           "#b4a8b6"))

   'light
   nil

   (append
    '(
      (builtin      blue)
      (keyword      red)
      (fnname       yellow)
      (fnname-call  green)
      (type         blue)
      (string       red-warmer)
      (comment      fg-alt)
      (docstring    green)
      (variable     fg-main)
      (constant     red)
      (preprocessor red)
      (rx-construct red-warmer)
      (rx-backslash blue)

      (accent-0 blue)
      (accent-1 red)
      (accent-2 yellow)
      (accent-3 magenta)

      (info       yellow)
      (bg-info    bg-yellow-subtle)

      (bg-line-number-active yellow-faint)
      (fg-line-number-active red)

      (keybind        red)
      (fg-prompt      red)
      (fg-link        red)
      (fg-link-visited yellow)

      (fg-prose-macro  red)

      (mail-subject    red)
      (mail-recipient  blue)
      (mail-cite-0     blue)
      (mail-cite-1     red-warmer)
      (mail-cite-2     green)
      (mail-cite-3     magenta-warmer)

      (rainbow-0 fg-main)
      (rainbow-1 red)
      (rainbow-2 blue)
      (rainbow-3 red-warmer)
      (rainbow-4 blue-warmer)
      (rainbow-5 magenta)
      (rainbow-6 yellow)
      (rainbow-7 green)
      (rainbow-8 red-cooler))
    pixel-themes-palette-common)))

(defcustom pixel-themes-gothic-temple-palette-overrides nil
  "Overrides for `pixel-themes-gothic-temple-palette'."
  :group 'pixel-themes
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(modus-themes-theme
 'pixel-themes-gothic-temple
 'pixel-themes
 "Light theme: weathered stone and faded gold of digital divinity."
 'light
 'pixel-themes-gothic-temple-palette
 nil
 'pixel-themes-gothic-temple-palette-overrides)

(provide-theme 'pixel-themes-gothic-temple)
;;; pixel-themes-gothic-temple-theme.el ends here
