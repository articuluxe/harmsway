;;; foxglove-themes.el --- Quiet, ink-on-paper themes that survive e-ink -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Vegas Harmon

;; Author: Vegas Harmon <vegasharmon@protonmail.com>
;; Maintainer: Vegas Harmon <vegasharmon@protonmail.com>
;; URL: https://gitlab.com/vegasharmon/foxglove-themes
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (modus-themes "5.0"))
;; Keywords: faces, theme

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Quiet, ink-on-paper themes, built on the Modus themes and designed
;; to stay legible on an e-ink panel.
;;
;; Prose is body ink and three greys.  Code gets just enough color to
;; be useful, after doom-homage-white: keywords, types and definitions in
;; bold body ink, function calls in a blue bold italic, strings in plum
;; and comments in an italic sage.  The plum and sage come from the cover
;; of Ted Hughes's Selected Poems 1957-1994.
;;
;; Every color sits on one of a few lightness tiers, spaced so that each
;; lands on its own grey level of a 16-level e-ink panel.  Where two roles
;; share a tier, weight or slant tells them apart.  The hierarchy thus
;; survives when the color is gone.
;;
;; The family consists of six themes, three papers in a light and a dark
;; variant (add -dark to the name):
;;
;;   `foxglove'          Linen, a near-neutral paper
;;   `foxglove-vellum'   Vellum, a warmer paper
;;   `foxglove-mono'     no color, only exact greys, for e-ink panels
;;
;; Being Modus derivatives, the themes style every package Modus
;; supports and honor the Modus user options, such as
;; `modus-themes-headings' and `modus-themes-mixed-fonts'.  Their own
;; user options are in the `foxglove-themes' group.  Changing one through
;; Customize or `setopt' reloads any enabled Foxglove theme.
;;
;;   (load-theme 'foxglove :no-confirm)

;;; Code:

(require 'modus-themes)

(defgroup foxglove-themes nil
  "Quiet, ink-on-paper themes built on the Modus themes."
  :group 'faces
  :prefix "foxglove-themes-"
  :link '(url-link "https://gitlab.com/vegasharmon/foxglove-themes"))

;;;; BEGIN GENERATED THEMES
;; Written by tools/palette.py.  Edit the tables there, not here.

(defconst foxglove-themes-items
  '(foxglove foxglove-dark
    foxglove-vellum foxglove-vellum-dark
    foxglove-mono foxglove-mono-dark)
  "Every theme in the Foxglove family.")

;;;###autoload
(with-eval-after-load 'modus-themes
  (dolist (theme '((foxglove foxglove-themes
                    "Foxglove: ink on near-neutral paper, with the cover's plum and sage."
                    light foxglove-palette nil foxglove-themes-palette-overrides)
                   (foxglove-dark foxglove-themes
                    "Foxglove Dark: the Foxglove inks at mirrored lightness on warm near-black."
                    dark foxglove-dark-palette nil foxglove-themes-palette-overrides)
                   (foxglove-vellum foxglove-themes
                    "Foxglove Vellum: the Foxglove inks on a warmer paper."
                    light foxglove-vellum-palette nil foxglove-themes-palette-overrides)
                   (foxglove-vellum-dark foxglove-themes
                    "Foxglove Vellum Dark: the Foxglove inks on lamplit brown."
                    dark foxglove-vellum-dark-palette nil foxglove-themes-palette-overrides)
                   (foxglove-mono foxglove-themes
                    "Foxglove Mono: exact e-ink greys, no color, black on white."
                    light foxglove-mono-palette nil foxglove-themes-palette-overrides)
                   (foxglove-mono-dark foxglove-themes
                    "Foxglove Mono Dark: exact e-ink greys, light on black."
                    dark foxglove-mono-dark-palette nil foxglove-themes-palette-overrides)))
    (apply #'modus-themes-declare theme)
    (modus-themes-register (car theme))))

;;;; END GENERATED THEMES

(defun foxglove-themes-reload ()
  "Reload each enabled Foxglove theme, so option changes take effect."
  (interactive)
  (dolist (theme (reverse custom-enabled-themes))
    (when (memq theme foxglove-themes-items)
      (load-theme theme :no-confirm))))

(defvar foxglove-themes--reloading nil
  "Non-nil while `foxglove-themes-reload' runs.")

(defun foxglove-themes--set (symbol value)
  "Set SYMBOL to VALUE and, if it changed, reload any enabled Foxglove theme.
Enabling a theme re-runs every customized option's setter with its
current value, so an unchanged value must not reload, or reloading
would recurse."
  (let ((changed (not (and (default-boundp symbol)
                           (equal (default-toplevel-value symbol) value)))))
    (set-default-toplevel-value symbol value)
    (when (and changed (not foxglove-themes--reloading))
      (let ((foxglove-themes--reloading t))
        (foxglove-themes-reload)))))

;;;; Options

(defcustom foxglove-themes-inks 'lapis
  "The inks of code and Org keywords.

- `lapis': a deep, greyish ultramarine for function calls and links,
  foxglove plum for strings and leaf sage for comments.
- `cobalt': the same with a brighter cobalt blue instead of lapis.
- `rubric': a darker blue, maroon strings, olive comments and a
  red-orange for TODO keywords.
- `two-inks': only a blue and a maroon, with grey comments.
- `classic': the pigments of a medieval manuscript: lapis, minium
  strings and sepia comments.

This applies to `foxglove', `foxglove-vellum' and their dark variants.
The Mono themes have no inks."
  :type '(choice (const :tag "Lapis: ultramarine, plum, sage" lapis)
                 (const :tag "Cobalt: cobalt, plum, sage" cobalt)
                 (const :tag "Rubric: blue, maroon, olive, red-orange" rubric)
                 (const :tag "Classic: lapis, minium, sepia" classic)
                 (const :tag "Two inks: blue and maroon only" two-inks))
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-tags 'footnote
  "The style of Org tags.

- `footnote': small, dim, regular-weight italic.
- `plates': a grey label for each tag.
- `foxglove': a label in plum ink on a pale petal tint.
- `plum-label': a label in the paper color on plum, the loudest.
- nil: the style of the Modus themes.

The three label styles draw each tag as a label of its own through
org-modern, and so need `org-modern-tag' to be non-nil.  Without it,
they fall back to plain dim text.  When top-level headings have the
`band' style, the labels sit inside the band."
  :type '(choice (const :tag "Footnote: small dim italic" footnote)
                 (const :tag "Plates: grey tab per tag" plates)
                 (const :tag "Foxglove: plum on a petal tint" foxglove)
                 (const :tag "Plum label: paper on plum" plum-label)
                 (const :tag "Modus default" nil))
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-heading-styles t
  "Whether headings get the styles of the options below.
When nil, no heading carries a rule, band, padding or serif face, and
the Foxglove themes leave headings to `modus-themes-headings', in body
ink.  The options `foxglove-themes-heading-1', `foxglove-themes-heading-2'
and `foxglove-themes-heading-rest' then have no effect."
  :type 'boolean
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-heading-1 'band
  "The style of top-level headings.

- `band': a rule above and a tint across the full width of the window.
- `padded': a rule and extra space above and below.
- `serif': a rule and the font of `foxglove-themes-serif-family', at
  the height of `foxglove-themes-serif-height'.
- `serif-padded': both of the above.
- `rule': a rule and nothing more.
- nil: the style of `foxglove-themes-heading-rest'.

Level 1's rule is darker than that of any other level.  The rules and
the band reach the window edge only when `org-fontify-whole-heading-line'
is non-nil."
  :type '(choice (const :tag "Band: full-width tint" band)
                 (const :tag "Padded: space above and below" padded)
                 (const :tag "Serif: level 1 in the serif face" serif)
                 (const :tag "Serif and padded" serif-padded)
                 (const :tag "Rule only" rule)
                 (const :tag "As the rest of the headings" nil))
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-heading-2 'rule
  "The style of second-level headings.
The values are those of `foxglove-themes-heading-1'.  With nil, level
2 takes the style of `foxglove-themes-heading-rest'."
  :type '(choice (const :tag "Band: full-width tint" band)
                 (const :tag "Padded: space above and below" padded)
                 (const :tag "Serif: level 2 in the serif face" serif)
                 (const :tag "Serif and padded" serif-padded)
                 (const :tag "Rule only" rule)
                 (const :tag "As the rest of the headings" nil))
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-heading-rest nil
  "The style of headings at level 3 and below.
The values are those of `foxglove-themes-heading-1', except that nil
means no style at all.  It also applies to levels 1 and 2 when their
own option is nil."
  :type '(choice (const :tag "Band: full-width tint" band)
                 (const :tag "Padded: space above and below" padded)
                 (const :tag "Serif: in the serif face" serif)
                 (const :tag "Serif and padded" serif-padded)
                 (const :tag "Rule only" rule)
                 (const :tag "No style" nil))
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-serif-family "EB Garamond"
  "The font family of headings in the serif styles.
It applies to each heading level whose style is `serif' or
`serif-padded'."
  :type 'string
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-serif-height 1.25
  "The height of headings in the serif styles, relative to the body.
A serif face has a lower x-height than a monospaced one, so at the same
size it reads smaller.  The default of 1.25 suits EB Garamond beside
IBM Plex Mono."
  :type 'number
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-blocks 'ruled-well
  "The style of Org blocks, alike for source, quote, verse and any other.

- `ruled-well': the body is tinted.  The begin and end lines stay on
  the paper, with a hairline rule under the begin line and over the end
  line.
- `well': the body is tinted, and the begin and end lines a step
  deeper, as in the Modus themes.
- `ruled': nothing is tinted; only the rules remain.

A colored accent down the left of each block is not part of this
option, as a face cannot draw one.  The org-block-wrap package draws
it, under its user option `org-block-wrap-bracket', and every Foxglove
theme colors it."
  :type '(choice (const :tag "Ruled well" ruled-well)
                 (const :tag "Well" well)
                 (const :tag "Ruled" ruled))
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-diffs 'standard
  "The colors of added and removed lines in diffs.

- `standard': sage for added lines and plum for removed ones.
- `cvd': blue and ochre, for color vision deficiency.

Sage and plum run together for people with deuteranopia, while blue
and ochre stay distinct for every type of color vision deficiency.
The Mono themes use greys either way."
  :type '(choice (const :tag "Sage and plum" standard)
                 (const :tag "Blue and ochre, for color blindness" cvd))
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-comments 'normal
  "The contrast of comments.

- `normal': about 4:1 against the paper, deliberately quiet.
- `darker': about 5:1, above the WCAG AA minimum for body text."
  :type '(choice (const :tag "Quiet (about 4:1)" normal)
                 (const :tag "Darker (about 5:1)" darker))
  :set #'foxglove-themes--set
  :initialize #'custom-initialize-default)

(defcustom foxglove-themes-palette-overrides nil
  "Palette entries that override those of every Foxglove theme.
Each entry is (NAME VALUE), as in `modus-themes-common-palette-overrides',
which also applies.  Reload the theme for changes to take effect."
  :type '(repeat (list symbol (choice symbol string))))

;;;; Palette

;;;; BEGIN GENERATED PALETTE
;; Written by tools/palette.py.  Edit the tables there, not here.

(defconst foxglove-themes--neutrals
  '(
    (linen-light
     (bg-main "#fdfdfa")
     (bg-dim "#f3f2ef")
     (bg-inactive "#e9e8e5")
     (bg-active "#dadad8")
     (border "#ccccca")
     (fox-rule "#b1b1ae")
     (fox-faint "#c1c1be")
     (fg-dim "#969693")
     (fg-alt "#61615e")
     (fg-main "#373735")
     (bg-mode-line-active "#d2d2cf")
     (border-mode-line-active "#646361")
     (bg-mode-line-inactive "#ebebe8")
     (border-mode-line-inactive "#b1b1ae")
     (fg-mode-line-inactive "#7f7e7c"))
    (vellum-light
     (bg-main "#fefcf8")
     (bg-dim "#f8f2e9")
     (bg-inactive "#eee7df")
     (bg-active "#e0d9d0")
     (border "#d1cbc2")
     (fox-rule "#b6afa7")
     (fox-faint "#c6c0b7")
     (fg-dim "#9c958d")
     (fg-alt "#656058")
     (fg-main "#3b3631")
     (bg-mode-line-active "#d7d1c8")
     (border-mode-line-active "#68635b")
     (bg-mode-line-inactive "#f0eae1")
     (border-mode-line-inactive "#b6afa7")
     (fg-mode-line-inactive "#847e76"))
    (linen-dark
     (bg-main "#22211f")
     (bg-dim "#2a2a28")
     (bg-inactive "#333330")
     (bg-active "#40403d")
     (border "#4e4d4b")
     (fox-rule "#5f5f5c")
     (fox-faint "#4c4c49")
     (fg-dim "#737270")
     (fg-alt "#a6a5a3")
     (fg-main "#d2d2cf")
     (bg-mode-line-active "#474744")
     (border-mode-line-active "#969693")
     (bg-mode-line-inactive "#2d2c2a")
     (border-mode-line-inactive "#555452")
     (fg-mode-line-inactive "#8c8c89"))
    (vellum-dark
     (bg-main "#25211a")
     (bg-dim "#2f2923")
     (bg-inactive "#37322b")
     (bg-active "#443f38")
     (border "#524d46")
     (fox-rule "#635e56")
     (fox-faint "#504a43")
     (fg-dim "#77726a")
     (fg-alt "#aba59d")
     (fg-main "#d6d1ca")
     (bg-mode-line-active "#4b463f")
     (border-mode-line-active "#9c958d")
     (bg-mode-line-inactive "#312b25")
     (border-mode-line-inactive "#59544c")
     (fg-mode-line-inactive "#918a82"))
    (mono-light
     (bg-main "#ffffff")
     (bg-dim "#eeeeee")
     (bg-inactive "#eeeeee")
     (bg-active "#cccccc")
     (border "#aaaaaa")
     (fox-rule "#999999")
     (fox-faint "#aaaaaa")
     (fg-dim "#888888")
     (fg-alt "#444444")
     (fg-main "#000000")
     (bg-mode-line-active "#cccccc")
     (border-mode-line-active "#000000")
     (bg-mode-line-inactive "#eeeeee")
     (border-mode-line-inactive "#999999")
     (fg-mode-line-inactive "#666666"))
    (mono-dark
     (bg-main "#111111")
     (bg-dim "#222222")
     (bg-inactive "#333333")
     (bg-active "#444444")
     (border "#555555")
     (fox-rule "#666666")
     (fox-faint "#555555")
     (fg-dim "#777777")
     (fg-alt "#bbbbbb")
     (fg-main "#eeeeee")
     (bg-mode-line-active "#444444")
     (border-mode-line-active "#cccccc")
     (bg-mode-line-inactive "#222222")
     (border-mode-line-inactive "#666666")
     (fg-mode-line-inactive "#999999")))
  "Paper colors, keyed by PAPER-MODE.")

(defconst foxglove-themes--hues
  '(
    (light
     (red "#9a4844")
     (red-warmer "#a6402b")
     (red-cooler "#8f4d59")
     (red-faint "#a27273")
     (red-intense "#ca4e3f")
     (green "#476933")
     (green-warmer "#556731")
     (green-cooler "#336b55")
     (green-faint "#7a8264")
     (green-intense "#468637")
     (yellow "#82590b")
     (yellow-warmer "#8e520c")
     (yellow-cooler "#745e26")
     (yellow-faint "#937b5f")
     (yellow-intense "#977105")
     (blue "#263fa7")
     (blue-warmer "#394190")
     (blue-cooler "#194983")
     (blue-faint "#4f6184")
     (blue-intense "#4470e2")
     (magenta "#894e70")
     (magenta-warmer "#904a67")
     (magenta-cooler "#6f558d")
     (magenta-faint "#96758a")
     (magenta-intense "#b45498")
     (cyan "#266977")
     (cyan-warmer "#476476")
     (cyan-cooler "#316966")
     (cyan-faint "#62848b")
     (cyan-intense "#08838e")
     (rust "#984b30")
     (gold "#795c19")
     (olive "#556731")
     (slate "#4a6474")
     (indigo "#5e5b8e")
     (maroon "#9a4844")
     (pink "#8f4d59")
     (bg-red-intense "#fab5b4")
     (bg-green-intense "#b4cf99")
     (bg-yellow-intense "#e2c28b")
     (bg-blue-intense "#adc7fc")
     (bg-magenta-intense "#eeb5da")
     (bg-cyan-intense "#83d3e1")
     (bg-red-subtle "#ffd2d1")
     (bg-green-subtle "#d0e2c0")
     (bg-yellow-subtle "#efdab9")
     (bg-blue-subtle "#cedefe")
     (bg-magenta-subtle "#f7d3e9")
     (bg-cyan-subtle "#b6e5ed")
     (bg-red-nuanced "#feedec")
     (bg-green-nuanced "#eaf4e2")
     (bg-yellow-nuanced "#faf0df")
     (bg-blue-nuanced "#ebf1fe")
     (bg-magenta-nuanced "#feecf7")
     (bg-cyan-nuanced "#ddf5f9")
     (bg-clay "#ffd3cc")
     (fg-clay "#612c24")
     (bg-ochre "#f1d9b9")
     (fg-ochre "#523608")
     (bg-lavender "#e3d8fb")
     (fg-lavender "#443261")
     (bg-sage "#c4e5ca")
     (fg-sage "#1a4325")
     (bg-search-current "#f1cf99")
     (bg-search-lazy "#dde5fa")
     (bg-search-static "#f6e0ed")
     (bg-search-replace "#fccbd1")
     (bg-search-rx-group-0 "#cedefe")
     (bg-search-rx-group-1 "#d0e2c0")
     (bg-search-rx-group-2 "#ffd2d1")
     (bg-search-rx-group-3 "#f7d3e9")
     (bg-paren-match "#c7d4f7")
     (bg-hover "#bee4e6")
     (bg-hover-secondary "#efdab9")
     (fox-tag-plum "#fee5e8")
     (bg-added "#e4efd5")
     (bg-added-faint "#f0f6e7")
     (bg-added-refine "#cdddb5")
     (bg-added-fringe "#9eb37b")
     (fg-added "#3d4f19")
     (fg-added-intense "#526822")
     (bg-removed "#fee5e8")
     (bg-removed-faint "#fff0f2")
     (bg-removed-refine "#fccbd1")
     (bg-removed-fringe "#dc9aa3")
     (fg-removed "#703741")
     (fg-removed-intense "#944a57")
     (bg-changed "#f8e9d2")
     (bg-changed-faint "#fbf3e5")
     (bg-changed-refine "#ebd4af")
     (bg-changed-fringe "#c6a771")
     (fg-changed "#614409")
     (fg-changed-intense "#805a07"))
    (dark
     (red "#e78d86")
     (red-warmer "#f5876f")
     (red-cooler "#db919d")
     (red-faint "#b07f80")
     (red-intense "#ee6f5d")
     (green "#8bb177")
     (green-warmer "#99ad75")
     (green-cooler "#79b298")
     (green-faint "#878f70")
     (green-intense "#65a556")
     (yellow "#cb9e5c")
     (yellow-warmer "#d9985c")
     (yellow-cooler "#bba46b")
     (yellow-faint "#a0886c")
     (yellow-intense "#bb8f1f")
     (blue "#a1bbfe")
     (blue-warmer "#a9b8fe")
     (blue-cooler "#8dbfff")
     (blue-faint "#91a6cc")
     (blue-intense "#6492fe")
     (magenta "#d392b6")
     (magenta-warmer "#dd8fad")
     (magenta-cooler "#b69ad7")
     (magenta-faint "#a38197")
     (magenta-intense "#d874b9")
     (cyan "#6fb0bf")
     (cyan-warmer "#8baabe")
     (cyan-cooler "#77b0ac")
     (cyan-faint "#6f9198")
     (cyan-intense "#14a6b3")
     (rust "#e49073")
     (gold "#c1a262")
     (olive "#99ad75")
     (slate "#8daabb")
     (indigo "#a1a0d8")
     (maroon "#e78d86")
     (pink "#db919d")
     (bg-red-intense "#7e4445")
     (bg-green-intense "#445b2a")
     (bg-yellow-intense "#6c5019")
     (bg-blue-intense "#3f5481")
     (bg-magenta-intense "#754566")
     (bg-cyan-intense "#005d6a")
     (bg-red-subtle "#5c3938")
     (bg-green-subtle "#38462a")
     (bg-yellow-subtle "#503f22")
     (bg-blue-subtle "#35425e")
     (bg-magenta-subtle "#563a4d")
     (bg-cyan-subtle "#1b4850")
     (bg-red-nuanced "#3d2d2c")
     (bg-green-nuanced "#2c3325")
     (bg-yellow-nuanced "#372f22")
     (bg-blue-nuanced "#2a313e")
     (bg-magenta-nuanced "#3a2c35")
     (bg-cyan-nuanced "#213437")
     (bg-clay "#5c3a34")
     (fg-clay "#fec7be")
     (bg-ochre "#523f23")
     (fg-ochre "#f1cfa2")
     (bg-lavender "#483d5b")
     (fg-lavender "#ddcdfe")
     (bg-sage "#2d4833")
     (fg-sage "#b2e0ba")
     (bg-search-current "#6c5019")
     (bg-search-lazy "#353b4c")
     (bg-search-static "#483741")
     (bg-search-replace "#633c42")
     (bg-search-rx-group-0 "#35425e")
     (bg-search-rx-group-1 "#38462a")
     (bg-search-rx-group-2 "#5c3938")
     (bg-search-rx-group-3 "#563a4d")
     (bg-paren-match "#444f6c")
     (bg-hover "#254849")
     (bg-hover-secondary "#503f22")
     (fox-tag-plum "#462f32")
     (bg-added "#303824")
     (bg-added-faint "#292d23")
     (bg-added-refine "#3f4b29")
     (bg-added-fringe "#5f713c")
     (fg-added "#aec38b")
     (fg-added-intense "#95af68")
     (bg-removed "#462f32")
     (bg-removed-faint "#36282a")
     (bg-removed-refine "#633c42")
     (bg-removed-fringe "#965963")
     (fg-removed "#eda9b3")
     (fg-removed-intense "#e08f9b")
     (bg-changed "#3e3320")
     (bg-changed-faint "#322b21")
     (bg-changed-refine "#564423")
     (bg-changed-fringe "#836531")
     (fg-changed "#d7b780")
     (fg-changed-intense "#c89f5a"))
    (mono-light
     (red "#444444")
     (red-warmer "#444444")
     (red-cooler "#444444")
     (red-faint "#666666")
     (red-intense "#222222")
     (green "#444444")
     (green-warmer "#444444")
     (green-cooler "#444444")
     (green-faint "#666666")
     (green-intense "#222222")
     (yellow "#444444")
     (yellow-warmer "#444444")
     (yellow-cooler "#444444")
     (yellow-faint "#666666")
     (yellow-intense "#222222")
     (blue "#222222")
     (blue-warmer "#222222")
     (blue-cooler "#222222")
     (blue-faint "#444444")
     (blue-intense "#222222")
     (magenta "#444444")
     (magenta-warmer "#444444")
     (magenta-cooler "#444444")
     (magenta-faint "#666666")
     (magenta-intense "#222222")
     (cyan "#444444")
     (cyan-warmer "#444444")
     (cyan-cooler "#444444")
     (cyan-faint "#666666")
     (cyan-intense "#222222")
     (rust "#444444")
     (gold "#444444")
     (olive "#444444")
     (slate "#444444")
     (indigo "#444444")
     (maroon "#444444")
     (pink "#444444")
     (bg-red-intense "#cccccc")
     (bg-green-intense "#cccccc")
     (bg-yellow-intense "#cccccc")
     (bg-blue-intense "#cccccc")
     (bg-magenta-intense "#cccccc")
     (bg-cyan-intense "#cccccc")
     (bg-red-subtle "#dddddd")
     (bg-green-subtle "#dddddd")
     (bg-yellow-subtle "#dddddd")
     (bg-blue-subtle "#dddddd")
     (bg-magenta-subtle "#dddddd")
     (bg-cyan-subtle "#dddddd")
     (bg-red-nuanced "#eeeeee")
     (bg-green-nuanced "#eeeeee")
     (bg-yellow-nuanced "#eeeeee")
     (bg-blue-nuanced "#eeeeee")
     (bg-magenta-nuanced "#eeeeee")
     (bg-cyan-nuanced "#eeeeee")
     (bg-clay "#dddddd")
     (fg-clay "#222222")
     (bg-ochre "#dddddd")
     (fg-ochre "#222222")
     (bg-lavender "#dddddd")
     (fg-lavender "#222222")
     (bg-sage "#dddddd")
     (fg-sage "#222222")
     (bg-search-current "#bbbbbb")
     (bg-search-lazy "#dddddd")
     (bg-search-static "#dddddd")
     (bg-search-replace "#dddddd")
     (bg-search-rx-group-0 "#dddddd")
     (bg-search-rx-group-1 "#dddddd")
     (bg-search-rx-group-2 "#dddddd")
     (bg-search-rx-group-3 "#dddddd")
     (bg-paren-match "#dddddd")
     (bg-hover "#dddddd")
     (bg-hover-secondary "#dddddd")
     (fox-tag-plum "#dddddd")
     (bg-added "#eeeeee")
     (bg-added-faint "#eeeeee")
     (bg-added-refine "#cccccc")
     (bg-added-fringe "#666666")
     (fg-added "#000000")
     (fg-added-intense "#000000")
     (bg-removed "#dddddd")
     (bg-removed-faint "#eeeeee")
     (bg-removed-refine "#bbbbbb")
     (bg-removed-fringe "#222222")
     (fg-removed "#000000")
     (fg-removed-intense "#000000")
     (bg-changed "#eeeeee")
     (bg-changed-faint "#eeeeee")
     (bg-changed-refine "#cccccc")
     (bg-changed-fringe "#222222")
     (fg-changed "#000000")
     (fg-changed-intense "#000000"))
    (mono-dark
     (red "#bbbbbb")
     (red-warmer "#bbbbbb")
     (red-cooler "#bbbbbb")
     (red-faint "#999999")
     (red-intense "#dddddd")
     (green "#bbbbbb")
     (green-warmer "#bbbbbb")
     (green-cooler "#bbbbbb")
     (green-faint "#999999")
     (green-intense "#dddddd")
     (yellow "#bbbbbb")
     (yellow-warmer "#bbbbbb")
     (yellow-cooler "#bbbbbb")
     (yellow-faint "#999999")
     (yellow-intense "#dddddd")
     (blue "#cccccc")
     (blue-warmer "#cccccc")
     (blue-cooler "#cccccc")
     (blue-faint "#bbbbbb")
     (blue-intense "#dddddd")
     (magenta "#bbbbbb")
     (magenta-warmer "#bbbbbb")
     (magenta-cooler "#bbbbbb")
     (magenta-faint "#999999")
     (magenta-intense "#dddddd")
     (cyan "#bbbbbb")
     (cyan-warmer "#bbbbbb")
     (cyan-cooler "#bbbbbb")
     (cyan-faint "#999999")
     (cyan-intense "#dddddd")
     (rust "#bbbbbb")
     (gold "#bbbbbb")
     (olive "#bbbbbb")
     (slate "#bbbbbb")
     (indigo "#bbbbbb")
     (maroon "#bbbbbb")
     (pink "#bbbbbb")
     (bg-red-intense "#555555")
     (bg-green-intense "#555555")
     (bg-yellow-intense "#555555")
     (bg-blue-intense "#555555")
     (bg-magenta-intense "#555555")
     (bg-cyan-intense "#555555")
     (bg-red-subtle "#444444")
     (bg-green-subtle "#444444")
     (bg-yellow-subtle "#444444")
     (bg-blue-subtle "#444444")
     (bg-magenta-subtle "#444444")
     (bg-cyan-subtle "#444444")
     (bg-red-nuanced "#333333")
     (bg-green-nuanced "#333333")
     (bg-yellow-nuanced "#333333")
     (bg-blue-nuanced "#333333")
     (bg-magenta-nuanced "#333333")
     (bg-cyan-nuanced "#333333")
     (bg-clay "#444444")
     (fg-clay "#cccccc")
     (bg-ochre "#444444")
     (fg-ochre "#cccccc")
     (bg-lavender "#444444")
     (fg-lavender "#cccccc")
     (bg-sage "#444444")
     (fg-sage "#cccccc")
     (bg-search-current "#666666")
     (bg-search-lazy "#444444")
     (bg-search-static "#444444")
     (bg-search-replace "#444444")
     (bg-search-rx-group-0 "#444444")
     (bg-search-rx-group-1 "#444444")
     (bg-search-rx-group-2 "#444444")
     (bg-search-rx-group-3 "#444444")
     (bg-paren-match "#444444")
     (bg-hover "#444444")
     (bg-hover-secondary "#444444")
     (fox-tag-plum "#444444")
     (bg-added "#333333")
     (bg-added-faint "#222222")
     (bg-added-refine "#555555")
     (bg-added-fringe "#999999")
     (fg-added "#eeeeee")
     (fg-added-intense "#eeeeee")
     (bg-removed "#444444")
     (bg-removed-faint "#222222")
     (bg-removed-refine "#666666")
     (bg-removed-fringe "#dddddd")
     (fg-removed "#eeeeee")
     (fg-removed-intense "#eeeeee")
     (bg-changed "#333333")
     (bg-changed-faint "#222222")
     (bg-changed-refine "#555555")
     (bg-changed-fringe "#dddddd")
     (fg-changed "#eeeeee")
     (fg-changed-intense "#eeeeee")))
  "Modus base hues and tints, keyed by MODE, or mono-MODE for Mono.")

(defconst foxglove-themes--inks
  '(
    (cobalt-light
     (fox-definition "#263fa7")
     (fox-string "#8f4d59")
     (fox-constant "#82590b")
     (fox-type "#4a6474")
     (fox-comment "#7a8264")
     (fox-comment-darker "#6b7255")
     (fox-todo "#8f4d59")
     (fox-link "#263fa7"))
    (lapis-light
     (fox-definition "#274885")
     (fox-string "#8f4d59")
     (fox-constant "#82590b")
     (fox-type "#4a6474")
     (fox-comment "#7a8264")
     (fox-comment-darker "#6b7255")
     (fox-todo "#8f4d59")
     (fox-link "#274885"))
    (rubric-light
     (fox-definition "#294591")
     (fox-string "#9a4844")
     (fox-constant "#82590b")
     (fox-type "#4a6474")
     (fox-comment "#73854f")
     (fox-comment-darker "#647640")
     (fox-todo "#a6402b")
     (fox-link "#294591"))
    (classic-light
     (fox-definition "#274885")
     (fox-string "#a6402b")
     (fox-constant "#805a07")
     (fox-type "#894e70")
     (fox-comment "#937a64")
     (fox-comment-darker "#846a55")
     (fox-todo "#a6402b")
     (fox-link "#274885"))
    (two-inks-light
     (fox-definition "#263fa7")
     (fox-string "#9a4844")
     (fox-constant fg-main)
     (fox-type fg-main)
     (fox-comment "#7c7f82")
     (fox-comment-darker "#6d7073")
     (fox-todo "#9a4844")
     (fox-link "#263fa7"))
    (cobalt-dark
     (fox-definition "#a1bbfe")
     (fox-string "#db919d")
     (fox-constant "#cb9e5c")
     (fox-type "#8daabb")
     (fox-comment "#878f70")
     (fox-comment-darker "#949c7d")
     (fox-todo "#db919d")
     (fox-link "#a1bbfe"))
    (lapis-dark
     (fox-definition "#98bdfe")
     (fox-string "#db919d")
     (fox-constant "#cb9e5c")
     (fox-type "#8daabb")
     (fox-comment "#878f70")
     (fox-comment-darker "#949c7d")
     (fox-todo "#db919d")
     (fox-link "#98bdfe"))
    (rubric-dark
     (fox-definition "#9dbbfe")
     (fox-string "#e78d86")
     (fox-constant "#cb9e5c")
     (fox-type "#8daabb")
     (fox-comment "#80935c")
     (fox-comment-darker "#8c9f68")
     (fox-todo "#f5876f")
     (fox-link "#9dbbfe"))
    (classic-dark
     (fox-definition "#98bdfe")
     (fox-string "#f5876f")
     (fox-constant "#c89f5a")
     (fox-type "#d392b6")
     (fox-comment "#a18670")
     (fox-comment-darker "#ae937d")
     (fox-todo "#f5876f")
     (fox-link "#98bdfe"))
    (two-inks-dark
     (fox-definition "#a1bbfe")
     (fox-string "#e78d86")
     (fox-constant fg-main)
     (fox-type fg-main)
     (fox-comment "#888c8f")
     (fox-comment-darker "#96999c")
     (fox-todo "#e78d86")
     (fox-link "#a1bbfe"))
    (mono-light
     (fox-string "#444444")
     (fox-comment "#666666")
     (fox-comment-darker "#555555")
     (fox-constant "#444444")
     (fox-definition fg-main)
     (fox-type fg-main)
     (fox-todo fg-main)
     (fox-link fg-main))
    (mono-dark
     (fox-string "#bbbbbb")
     (fox-comment "#999999")
     (fox-comment-darker "#aaaaaa")
     (fox-constant "#bbbbbb")
     (fox-definition fg-main)
     (fox-type fg-main)
     (fox-todo fg-main)
     (fox-link fg-main)))
  "Syntax inks, keyed by INK-MODE, or mono-MODE for Mono.")

(defconst foxglove-themes--cvd-diffs
  '(
    (light
     (bg-added "#ddedfe")
     (bg-added-faint "#ecf5fe")
     (bg-added-refine "#bcdafb")
     (bg-added-fringe "#84afdc")
     (fg-added "#244c73")
     (fg-added-intense "#2f6397")
     (bg-removed "#f8e9d2")
     (bg-removed-faint "#fbf3e5")
     (bg-removed-refine "#ebd4af")
     (bg-removed-fringe "#c6a771")
     (fg-removed "#614409")
     (fg-removed-intense "#805a07")
     (bg-changed "#f3e6fe")
     (bg-changed-faint "#f8f1fe")
     (bg-changed-refine "#e4d0f5")
     (bg-changed-fringe "#bda0d4")
     (fg-changed "#593e6c")
     (fg-changed-intense "#75528e"))
    (dark
     (bg-added "#273646")
     (bg-added-faint "#242d36")
     (bg-added-refine "#2f4863")
     (bg-added-fringe "#456e97")
     (fg-added "#94bfed")
     (fg-added-intense "#74abe2")
     (bg-removed "#3e3320")
     (bg-removed-faint "#322b21")
     (bg-removed-refine "#564423")
     (bg-removed-fringe "#836531")
     (fg-removed "#d7b780")
     (fg-removed-intense "#c89f5a")
     (bg-changed "#3b3143")
     (bg-changed-faint "#302a34")
     (bg-changed-refine "#51405e")
     (bg-changed-fringe "#7b6090")
     (fg-changed "#ceb0e5")
     (fg-changed-intense "#bd98d9")))
  "Blue and ochre diff colors, keyed by MODE.  Mono has no variant.")

;;;; END GENERATED PALETTE

(defconst foxglove-themes--mappings
  '((cursor fox-string)
    (fringe bg-main)
    (fg-region unspecified)
    (bg-region bg-active)
    (bg-hl-line bg-dim)
    (bg-completion bg-active)
    (bg-popup bg-dim)
    (bg-tab-bar bg-inactive)
    (bg-tab-current bg-main)
    (bg-tab-other bg-active)
    (modeline-err fox-todo)
    (modeline-warning yellow)
    (modeline-info blue)
    (keybind fox-definition)
    (name fg-alt)
    (identifier fg-alt)
    (err fox-todo)
    (warning yellow)
    (info blue)
    (underline-err fox-todo)
    (underline-warning yellow)
    (underline-note blue-faint)
    (bg-prominent-err bg-red-subtle)
    (fg-prominent-err fg-main)
    (bg-prominent-warning bg-yellow-subtle)
    (fg-prominent-warning fg-main)
    (bg-prominent-note bg-blue-subtle)
    (fg-prominent-note fg-main)
    (bg-active-argument bg-yellow-nuanced)
    (fg-active-argument yellow)
    (bg-active-value bg-blue-nuanced)
    (fg-active-value blue)
    ;; Syntax, after doom-homage-white: keywords, types, definitions and
    ;; builtins in bold body ink; calls in blue bold italic; strings and
    ;; numbers in plum; constants and variables in plain body ink.
    ;; Docstrings are strings, so plum too, but italic.
    (builtin fg-main)
    (comment fox-comment)
    (constant fg-main)
    (docstring fox-string)
    (docmarkup fox-constant)
    (fnname fg-main)
    (fnname-call fox-definition)
    (keyword fg-main)
    (preprocessor fg-main)
    (property fg-main)
    (rx-backslash fox-constant)
    (rx-construct fox-type)
    (string fox-string)
    (type fg-main)
    (variable fg-main)
    (variable-use fg-main)
    (number fox-string)
    (operator fg-main)
    (bracket fg-dim)
    (delimiter fg-dim)
    (punctuation fg-dim)
    (accent-0 fox-definition)
    (accent-1 fox-string)
    (accent-2 fox-constant)
    (accent-3 fox-comment)
    (fg-completion-match-0 fox-definition)
    (fg-completion-match-1 fox-string)
    (fg-completion-match-2 fox-constant)
    (fg-completion-match-3 green-cooler)
    ;; Dates: body ink, with plum only where something is due.
    (date-common fg-main)
    (date-deadline fox-todo)
    (date-deadline-subtle fg-alt)
    (date-event fg-alt)
    (date-holiday fox-string)
    (date-holiday-other fox-definition)
    (date-now fox-definition)
    (date-range fg-alt)
    (date-scheduled fg-main)
    (date-scheduled-subtle fg-dim)
    (date-weekday fg-main)
    (date-weekend fg-dim)
    ;; Links are body ink with a colored underline.
    (fg-link fg-main)
    (underline-link fox-link)
    (fg-link-symbolic fg-main)
    (underline-link-symbolic fg-dim)
    (fg-link-visited fg-main)
    (underline-link-visited fox-string)
    (mail-cite-0 fox-definition)
    (mail-cite-1 fox-string)
    (mail-cite-2 fox-comment)
    (mail-cite-3 fox-constant)
    (mail-part fg-alt)
    (mail-recipient fox-definition)
    (mail-subject fg-main)
    (mail-other fg-alt)
    (bg-mark-delete bg-removed)
    (fg-mark-delete fg-removed)
    (bg-mark-select bg-search-lazy)
    (fg-mark-select fox-definition)
    (bg-mark-other bg-yellow-nuanced)
    (fg-mark-other yellow)
    (fg-prompt fg-main)
    (fg-prose-code fox-constant)
    (fg-prose-macro fox-constant)
    (fg-prose-verbatim fg-alt)
    (prose-done fg-dim)
    (prose-todo fox-todo)
    (prose-metadata fg-dim)
    (prose-metadata-value fg-dim)
    (prose-table fg-main)
    (prose-table-formula fox-constant)
    (prose-tag fg-alt)
    (bg-prose-block-contents bg-dim)
    (bg-prose-block-delimiter unspecified)
    (fg-prose-block-delimiter fg-dim)
    (rainbow-0 fg-main)
    (rainbow-1 fox-definition)
    (rainbow-2 fox-string)
    (rainbow-3 fox-constant)
    (rainbow-4 fox-comment)
    (rainbow-5 fox-type)
    (rainbow-6 blue-faint)
    (rainbow-7 red-faint)
    (rainbow-8 yellow-faint)
    ;; Headings are body ink; their rules and bands are user options.
    (fg-heading-0 fg-main)
    (fg-heading-1 fg-main)
    (fg-heading-2 fg-main)
    (fg-heading-3 fg-main)
    (fg-heading-4 fg-main)
    (fg-heading-5 fg-main)
    (fg-heading-6 fg-main)
    (fg-heading-7 fg-main)
    (fg-heading-8 fg-main)
    (fg-line-number-inactive fox-faint)
    (fg-line-number-active fg-main)
    (bg-line-number-inactive unspecified)
    (bg-line-number-active unspecified)
    (fg-paren-match fg-main)
    (bg-paren-expression bg-dim)
    (fg-space border))
  "Semantic mappings shared by every Foxglove theme.
They point Modus's roles at the generated colors above.")

(defconst foxglove-themes--dots
  (condition-case nil
      (progn (face-spec-set 'foxglove-themes--probe
                            '((t :underline (:style dots))) 'face-defface-spec)
             'dots)
    (error 'line))
  "The underline style for diagnostic notes: `dots' where Emacs has it.")

(defvar foxglove-themes--paper nil
  "The paper of the theme being defined, while its faces are evaluated.")

(defun foxglove-themes--heading-style (level)
  "The style of headings at LEVEL, from the heading options.
Nil means no style."
  (when foxglove-themes-heading-styles
    (or (pcase level
          (1 foxglove-themes-heading-1)
          (2 foxglove-themes-heading-2))
        foxglove-themes-heading-rest)))

(defun foxglove-themes--option-entries (paper)
  "Palette entries that depend on the user options, for PAPER."
  (append
   (when (eq paper 'mono)
     ;; No hl-line on e-ink: moving it forces partial refreshes and ghosts.
     '((bg-hl-line bg-main) (cursor fg-main) (fg-prose-code fg-main)))
   (when (eq foxglove-themes-comments 'darker)
     '((comment fox-comment-darker) (accent-3 fox-comment-darker)))
   ;; Level 1's rule is the darker one.
   (mapcan (lambda (level)
             (let ((style (foxglove-themes--heading-style level)))
               (append
                (when style
                  (list (list (intern (format "overline-heading-%d" level))
                              (if (= level 1) 'fox-rule 'border))))
                (when (eq style 'band)
                  (list (list (intern (format "bg-heading-%d" level)) 'bg-dim))))))
           (number-sequence 1 8))
   (when (eq foxglove-themes-blocks 'well)
     '((bg-prose-block-delimiter bg-inactive)))))

(defun foxglove-themes-palette (paper mode)
  "Return the full palette for PAPER (linen, vellum or mono) in MODE.
MODE is `light' or `dark'.  The inks come from `foxglove-themes-inks'.
The first entry for a name wins, so the options come first and the core
Modus palette last."
  (let* ((mono (eq paper 'mono))
         (key (lambda (&rest parts) (intern (mapconcat #'symbol-name parts "-"))))
         (ink (if mono
                  (funcall key 'mono mode)
                (funcall key foxglove-themes-inks mode))))
    (append (foxglove-themes--option-entries paper)
            (alist-get ink foxglove-themes--inks)
            (alist-get (funcall key paper mode) foxglove-themes--neutrals)
            (and (eq foxglove-themes-diffs 'cvd) (not mono)
                 (alist-get mode foxglove-themes--cvd-diffs))
            (alist-get (if mono (funcall key 'mono mode) mode) foxglove-themes--hues)
            foxglove-themes--mappings
            (if (eq mode 'dark)
                modus-themes-vivendi-palette
              modus-themes-operandi-palette))))

;;;; Faces

(defun foxglove-themes--label-border ()
  "Height in pixels of org-modern's inset above and below a tag label.
The same computation as org-modern's, from `org-modern-label-border'."
  (let ((border (if (boundp 'org-modern-label-border) org-modern-label-border 0.1)))
    (cond ((integerp border) border)
          ((floatp border) (ceiling (* (frame-char-height) border)))
          (t 2))))

(defun foxglove-themes--tag (face fg fg-dim plate plate-fg petal plum paper band rule)
  "Attributes for tag FACE (`org-tag' or `org-modern-tag').
The rest are palette colors: FG and FG-DIM are inks, PLATE and
PLATE-FG the plate style, PETAL and PLUM the foxglove style, PAPER
the background, BAND the top-level heading band (unspecified when
there is none) and RULE a heading rule.

The label styles fill `org-modern-tag' only: org-modern leaves the
separators between tags in `org-tag', so filling that would draw empty
bars between and after the labels.  org-modern insets each label with
a box in the paper color, which cuts a notch out of a band; with the
band on, the inset takes the band's color instead, and a rule along
the label's top carries the heading rule across it.

Org draws tags in the heading's face too, so with serif top-level
headings `org-tag' names the body's family, and scales its height back
down, to keep tags in the body face."
  (let ((labels (memq foxglove-themes-tags '(plates foxglove plum-label)))
        (serif (memq (foxglove-themes--heading-style 1) '(serif serif-padded))))
    (append
     (when (eq face 'org-modern-tag)
       (append '(:inherit org-modern-label)
               (when (and labels (stringp band))
                 (list :box (list :line-width (cons -1 (- (foxglove-themes--label-border)))
                                  :color band)
                       :overline rule))))
     (pcase (if (and (eq face 'org-tag) labels) 'plain foxglove-themes-tags)
       ('plain (list :foreground fg-dim))
       ('plates (list :background plate :foreground plate-fg))
       ('foxglove (list :background petal :foreground plum))
       ('plum-label (list :background plum :foreground paper))
       ('footnote (list :foreground fg-dim :slant 'italic :weight 'regular))
       (_ (list :foreground fg)))
     (when (and (eq face 'org-tag) serif)
       (list :family (face-attribute 'default :family nil t)))
     ;; org-modern-label already sets a smaller height.  A serif heading's
     ;; height is a multiple of the body's, so divide it back out.
     (when (and (eq face 'org-tag) foxglove-themes-tags)
       (list :height (if serif (/ 0.86 foxglove-themes-serif-height) 0.86))))))

(defun foxglove-themes--heading (level fg band rule paper)
  "Attributes for headings at LEVEL.
FG, BAND and RULE are the heading's palette colors, the band and rule
unspecified when its style has none, and PAPER the background; padding
is drawn as an invisible box in whichever of the two sits behind the
heading.  They come after Modus's attributes: in a face spec the later
of two values wins, and Modus sets its own :height."
  (let ((style (foxglove-themes--heading-style level)))
    (append
     (modus-themes--heading level fg band rule)
     ;; A book face's x-height is lower than a monospace's, so at the
     ;; same size the serif heading reads smaller; scale it to match.
     (when (memq style '(serif serif-padded))
       (list :family foxglove-themes-serif-family
             :height foxglove-themes-serif-height))
     (when (memq style '(padded serif-padded))
       ;; Emacs rejects a zero width in (VWIDTH . HWIDTH); -1 draws the
       ;; vertical edges inside the glyphs, in the same invisible color.
       (list :box (list :line-width '(-1 . 6)
                        :color (if (stringp band) band paper))))
     '(:extend t))))

(defun foxglove-themes--block-rule (edge rule)
  "Rule attributes for the block delimiter at EDGE (`begin' or `end').
RULE is its color."
  (when (memq foxglove-themes-blocks '(ruled ruled-well))
    (if (eq edge 'begin)
        (list :underline (list :color rule :position t))
      (list :overline rule))))

(defconst foxglove-themes-custom-faces
  '(
;;;; Weight and slant carry the grammar, so they don't depend on
;;;; `modus-themes-bold-constructs' or `modus-themes-italic-constructs'.
    `(bold ((,c :weight semibold)))
    `(font-lock-builtin-face ((,c :inherit bold :foreground ,builtin)))
    `(font-lock-comment-face ((,c :inherit italic :foreground ,comment)))
    `(font-lock-doc-face ((,c :inherit italic :foreground ,docstring)))
    `(font-lock-function-name-face ((,c :inherit bold :foreground ,fnname)))
    `(font-lock-function-call-face ((,c :inherit bold-italic :foreground ,fnname-call)))
    `(font-lock-keyword-face ((,c :inherit bold :foreground ,keyword)))
    `(font-lock-preprocessor-face ((,c :inherit bold :foreground ,preprocessor)))
    `(font-lock-type-face ((,c :inherit bold :foreground ,type)))
;;;; Diagnostics: the underline style carries the severity on e-ink.
;;;; The dotted style for notes needs Emacs 30; before that, notes are a line.
    `(flycheck-error ((,c :underline (:style wave :color ,underline-err))))
    `(flycheck-warning ((,c :underline (:style line :color ,underline-warning))))
    `(flycheck-info ((,c :underline (:style ,foxglove-themes--dots :color ,underline-note))))
    `(flymake-error ((,c :underline (:style wave :color ,underline-err))))
    `(flymake-warning ((,c :underline (:style line :color ,underline-warning))))
    `(flymake-note ((,c :underline (:style ,foxglove-themes--dots :color ,underline-note))))
;;;; Completion: on Mono, the first match is underlined as well as bold.
    `(header-line-inactive
      ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-dim
           :foreground ,fg-mode-line-inactive)))
    `(orderless-match-face-0
      ((,c :inherit bold :foreground ,fg-completion-match-0
           :underline ,(eq foxglove-themes--paper 'mono))))
;;;; Org headings
    `(modus-themes-heading-1
      ((,c ,@(foxglove-themes--heading 1 fg-heading-1 bg-heading-1 overline-heading-1 bg-main))))
    `(modus-themes-heading-2
      ((,c ,@(foxglove-themes--heading 2 fg-heading-2 bg-heading-2 overline-heading-2 bg-main))))
    `(modus-themes-heading-3
      ((,c ,@(foxglove-themes--heading 3 fg-heading-3 bg-heading-3 overline-heading-3 bg-main))))
    `(modus-themes-heading-4
      ((,c ,@(foxglove-themes--heading 4 fg-heading-4 bg-heading-4 overline-heading-4 bg-main))))
    `(modus-themes-heading-5
      ((,c ,@(foxglove-themes--heading 5 fg-heading-5 bg-heading-5 overline-heading-5 bg-main))))
    `(modus-themes-heading-6
      ((,c ,@(foxglove-themes--heading 6 fg-heading-6 bg-heading-6 overline-heading-6 bg-main))))
    `(modus-themes-heading-7
      ((,c ,@(foxglove-themes--heading 7 fg-heading-7 bg-heading-7 overline-heading-7 bg-main))))
    `(modus-themes-heading-8
      ((,c ,@(foxglove-themes--heading 8 fg-heading-8 bg-heading-8 overline-heading-8 bg-main))))
    `(org-todo ((,c :inherit bold :foreground ,prose-todo)))
    `(org-done ((,c :inherit bold :foreground ,prose-done)))
    `(org-headline-done ((,c :foreground ,prose-done)))
;;;; Org tags
    `(org-tag ((,c ,@(foxglove-themes--tag 'org-tag prose-tag fg-dim bg-inactive fg-main
                                           fox-tag-plum fox-string bg-main
                                           bg-heading-1 border))))
    `(org-modern-tag ((,c ,@(foxglove-themes--tag 'org-modern-tag prose-tag fg-dim bg-inactive
                                                  fg-main fox-tag-plum fox-string bg-main
                                                  bg-heading-1 border))))
;;;; Org blocks: one delimiter face for every block type.
    `(org-block
      ((,c :inherit modus-themes-fixed-pitch :extend t
           :background ,(if (eq foxglove-themes-blocks 'ruled) 'unspecified bg-prose-block-contents))))
    `(org-block-begin-line
      ((,c :inherit modus-themes-fixed-pitch :slant italic :extend t
           :background ,bg-prose-block-delimiter :foreground ,fg-prose-block-delimiter
           ,@(foxglove-themes--block-rule 'begin fox-rule))))
    `(org-block-end-line
      ((,c :inherit modus-themes-fixed-pitch :slant italic :extend t
           :background ,bg-prose-block-delimiter :foreground ,fg-prose-block-delimiter
           ,@(foxglove-themes--block-rule 'end fox-rule))))
;;;; org-block-wrap's per-type bracket, when that package is installed.
    `(org-block-wrap-bracket-src ((,c :foreground ,fox-definition)))
    `(org-block-wrap-bracket-quote ((,c :foreground ,fox-string)))
    `(org-block-wrap-bracket-verse ((,c :foreground ,fox-string)))
    `(org-quote ((,c :inherit modus-themes-fixed-pitch :extend t
                     :background ,(if (eq foxglove-themes-blocks 'ruled) 'unspecified bg-prose-block-contents))))
    `(org-verse ((,c :inherit modus-themes-fixed-pitch :extend t
                     :background ,(if (eq foxglove-themes-blocks 'ruled) 'unspecified bg-prose-block-contents))))
;;;; auto-dim-other-buffers: one e-ink step, the same as hl-line and blocks.
;;;; Modus styles only the faces' pre-2.0 names.
    `(auto-dim-other-buffers ((,c :background ,bg-dim)))
    `(auto-dim-other-buffers-hide ((,c :foreground ,bg-dim :background ,bg-dim))))
  "Face definitions that override the Modus ones in every Foxglove theme.
Each is evaluated with the palette's names bound to their colors.")

;;;; Definition

(defun foxglove-themes-define (name paper mode description)
  "Define theme NAME on PAPER in MODE, documented by DESCRIPTION.
PAPER is `linen', `vellum' or `mono'; MODE is `light' or `dark'.  The
palette is stored in NAME-palette, where Modus looks it up."
  (let ((palette (intern (format "%s-palette" name)))
        (foxglove-themes--paper paper))
    (set palette (foxglove-themes-palette paper mode))
    (modus-themes-theme name 'foxglove-themes description mode
                        palette nil 'foxglove-themes-palette-overrides
                        'foxglove-themes-custom-faces)))

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (add-to-list 'custom-theme-load-path dir)))

(provide 'foxglove-themes)

;;; foxglove-themes.el ends here
