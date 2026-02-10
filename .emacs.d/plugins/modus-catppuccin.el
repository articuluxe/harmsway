;;; modus-catppuccin.el --- Like the default theme but more consistent -*- lexical-binding:t -*-

;; Author: Magnus Therning <magnus@therning.org>
;; Maintainer: Magnus Therning <magnus@therning.org>
;; URL: https://gitlab.com/magus/modus-catppuccin
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (modus-themes "5.2.0"))
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The `modus-catppuccin' is a collection of themes for GNU Emacs.

;;; Code:



(require 'color)
(require 'modus-themes)
(eval-when-compile (require 'subr-x))

;;;; Color functions
(defun ctp-color (h s l)
  (apply #'color-rgb-to-hex (color-hsl-to-rgb h s l)))

;;;; Basics for building on top of Modus

(defgroup modus-catppuccin ()
  "Beautiful themes using the catppuccin colour theme.
The `modus-catppuccin' are built on top of the `modus-themes'.  To make
all the Modus commands that operate on a theme only consider Standard
themes, enable the `modus-catppuccin-take-over-modus-themes-mode'.  Or,
if you prefer to blend Standard and Modus into a single group, enable
`modus-themes-include-derivatives-mode'."
  :group 'faces
  :group 'modus-themes
  :link '(info-link "(modus-catppuccin) Top")
  :link '(info-link "(modus-themes) Top")
  :prefix "modus-catppuccin-"
  :tag "Catppuccin Themes")

(defconst modus-catppuccin-light-themes
  '(catppuccin-latte)
  "List of symbols with the light Catppuccin themes.")

(defconst modus-catppuccin-dark-themes
  '(catppuccin-frappe catppuccin-macchiato catpuccin-mocha)
  "List of symbols with the dark Catppuccin themes.")

(defvaralias 'modus-catppuccin-collection 'modus-catppuccin-items
  "Alias of `modus-catppuccin-items'.")

(defconst modus-catppuccin-items
  (append modus-catppuccin-light-themes modus-catppuccin-dark-themes)
  "Symbols of the Catppuccin themes.")

(defconst modus-catppuccin-common-palette-mappings
  '(
    ;; Basic values

    (bg-main base)
    (bg-dim surface0)
    (fg-main text)
    (fg-dim subtext0)
    (fg-alt subtext1)
    (bg-active overlay0)
    (bg-inactive surface1)
    (border crust)

    ;; Special purpose

    (bg-completion surface1)
    (bg-hover surface0)
    (bg-hover-secondary surface2)
    (bg-hl-line surface0)
    (bg-region surface2)
    (fg-region text)

    (bg-mode-line-active mantle)
    (fg-mode-line-active subtext1)
    (border-mode-line-active mantle)
    (bg-mode-line-inactive mantle)
    (fg-mode-line-inactive surface2)
    (border-mode-line-inactive surface0)

    (modeline-err red)
    (modeline-warning yellow)
    (modeline-info teal)

    (bg-tab-bar base)
    (bg-tab-current bg-main)
    (bg-tab-other base)

    ;; Diffs

    (bg-added bg-main)
    (bg-added-faint bg-main)
    (bg-added-refine bg-main)
    (bg-added-fringe bg-main)
    (fg-added green)
    (fg-added-intense green)

    (bg-changed bg-main)
    (bg-changed-faint bg-main)
    (bg-changed-refine bg-main)
    (bg-changed-fringe bg-main)
    (fg-changed sky)
    (fg-changed-intense sky)

    (bg-removed bg-main)
    (bg-removed-faint bg-main)
    (bg-removed-refine bg-main)
    (bg-removed-fringe bg-main)
    (fg-removed maroon)
    (fg-removed-intense maroon)

    (bg-diff-context surface1)

    ;; Paren match

    (bg-paren-match surface0)
    (bg-paren-expression bg-paren-match)
    (underline-paren-match unspecified)

    ;; General mappings

    (cursor fg-main)
    (keybind blue)
    (name blue)
    (identifier mauve)

    (err red)
    (warning yellow)
    (info teal)

    (underline-err red)
    (underline-warning yellow)
    (underline-note green)

    (bg-prominent-err err)
    (fg-prominent-err fg-main)
    (bg-prominent-warning yellow)
    (fg-prominent-warning fg-main)
    (bg-prominent-note teal)
    (fg-prominent-note fg-main)

    (bg-active-argument rosewater)
    (fg-active-argument base)
    (bg-active-value teal)
    (fg-active-value base)

    ;; Code mappings

    (bracket overlay2)
    (builtin blue)
    (comment overlay2)
    (constant red)
    (delimiter overlay2)
    (docstring subtext0)
    (fnname blue)
    (fnname-call blue)
    (keyword mauve)
    (number peach)
    (operator sky)
    (preprocessor yellow)
    (property blue)
    (rx-backslash red)
    (rx-construct red)
    (string green)
    (type yellow)
    (variable text)
    (variable-use text)

    ;; Accent mappings

    (accent-0 blue)
    (accent-1 sky)
    ;; (accent-2 cyan)
    ;; (accent-3 red)

    ;; Completion mappings

    (fg-completion-match-0 blue)
    (fg-completion-match-1 red)
    (fg-completion-match-2 green)
    (fg-completion-match-3 peach)

    ;; Date mappings

    ;; (date-common cyan)
    ;; (date-deadline red-cooler)
    ;; (date-deadline-subtle red-faint)
    ;; (date-event fg-alt)
    ;; (date-holiday red)
    ;; (date-holiday-other blue)
    ;; (date-range fg-alt)
    ;; (date-scheduled yellow)
    ;; (date-scheduled-subtle yellow-faint)
    (date-weekday blue)
    (date-weekend peach)

    ;; Link mappings

    (fg-link blue)
    ;; (underline-link blue-warmer)
    ;; (fg-link-symbolic cyan)
    ;; (underline-link-symbolic cyan)
    ;; (fg-link-visited magenta)
    ;; (underline-link-visited magenta)

    ;; Mail mappings

    ;; (mail-cite-0 blue-faint)
    ;; (mail-cite-1 yellow-warmer)
    ;; (mail-cite-2 cyan-cooler)
    ;; (mail-cite-3 red-cooler)
    ;; (mail-part cyan)
    ;; (mail-recipient magenta-cooler)
    ;; (mail-subject magenta-warmer)
    ;; (mail-other magenta-faint)

    ;; Mark mappings

    ;; (bg-mark-delete bg-red-subtle) "#443245"
    (fg-mark-delete red)
    ;; (bg-mark-select bg-cyan-subtle) "#3e4b6c"
    (fg-mark-select blue)
    ;; (bg-mark-other bg-yellow-subtle)
    ;; (fg-mark-other yellow)

    ;; Prompt mappings

    (fg-prompt mauve)

    ;; Prose mappings

    ;; (fg-prose-code cyan-cooler)
    ;; (fg-prose-macro magenta-cooler)
    (fg-prose-verbatim green)
    ;; (prose-done green)
    ;; (prose-todo red)
    ;; (prose-metadata fg-dim)
    ;; (prose-metadata-value fg-alt)
    ;; (prose-table fg-alt)
    ;; (prose-table-formula magenta-warmer)
    ;; (prose-tag magenta-faint)

    ;; Rainbow mappings

    (rainbow-0 red)
    (rainbow-1 peach)
    (rainbow-2 yellow)
    (rainbow-3 green)
    (rainbow-4 sapphire)
    (rainbow-5 lavender)
    ;; (rainbow-6 green-intense)
    ;; (rainbow-7 blue-warmer)
    ;; (rainbow-8 magenta-warmer)

    ;; Search mappings

    ;; (bg-search-current bg-yellow-intense)
    ;; (bg-search-lazy bg-cyan-intense)
    ;; (bg-search-static bg-magenta-subtle)
    ;; (bg-search-replace bg-red-intense)

    ;; (bg-search-rx-group-0 bg-blue-intense)
    ;; (bg-search-rx-group-1 bg-green-intense)
    ;; (bg-search-rx-group-2 bg-red-subtle)
    ;; (bg-search-rx-group-3 bg-magenta-subtle)

    ;; Heading mappings

    (fg-heading-0 red)
    (fg-heading-1 peach)
    (fg-heading-2 yellow)
    (fg-heading-3 green)
    (fg-heading-4 sapphire)
    (fg-heading-5 lavender)
    ;; (fg-heading-6 red-faint)
    ;; (fg-heading-7 cyan-warmer)
    ;; (fg-heading-8 fg-dim)
    )
  "Common palette mappings for the Catppuccin themes.")

(defconst modus-catppuccin-custom-faces
  '(
    `(cursor ((,c :background ,rosewater)))
    `(tab-bar ((,c :foreground ,rosewater)))
    `(tab-bar-tab-inactive ((,c :foreground ,subtext0)))))

(defconst modus-catppuccin-with-properties
  '((catppuccin-latte modus-catppuccin "Our lightest theme harmoniously inverting the essence of Catppuccin's dark themes."
                      light modus-operandi-palette catppuccin-latte-palette catppuccin-latte-palette-overrides)
    (catppuccin-frappe modus-catppuccin "A less vibrant alternative using subdued colors for a muted aesthetic. "
                       dark modus-vivendi-palette catppuccin-frappe-palette catppuccin-frappe-palette-overrides)
    (catppuccin-macchiato modus-catppuccin "Medium contrast with gentle colors creating a soothing atmosphere."
                          dark modus-vivendi-palette catppuccin-macchiato-palette catppuccin-macchiato-palette-overrides)
    (catppuccin-mocha modus-catppuccin "The Original — Our darkest variant offering a cozy feeling with color-rich accents."
                      dark modus-vivendi-tinted-palette catppuccin-mocha-palette catppuccin-mocha-palette-overrides)))

(defvar modus-catppuccin--declared-p nil)

(defun modus-catppuccin-declare-themes ()
  "Declare the Catppuccin themes."
  (unless modus-catppuccin--declared-p
    (dolist (theme modus-catppuccin-with-properties)
      (apply #'modus-themes-declare theme)
      (modus-themes-register (car theme)))
    (setq modus-catppuccin--declared-p t)))

(modus-catppuccin-declare-themes)

;;;; Limit the Modus themes to only Catppuccin themes

;;;###autoload
(define-minor-mode modus-catppuccin-take-over-modus-themes-mode
  "When enabled, all Modus themes commands consider only Catppuccin themes.
Alternatively, use the commands `modus-catppuccin-rotate',
`modus-catppuccin-select', `modus-catppuccin-load-random',
`modus-catppuccin-load-random-dark', `modus-catppuccin-load-random-light',
`modus-catppuccin-list-colors', `modus-catppuccin-list-colors-current'.
They are all designed to only consider Catppuccin themes."
  :global t
  :init-value nil)

(cl-defmethod modus-themes-get-themes (&context (modus-catppuccin-take-over-modus-themes-mode (eql t)))
  "Return list of Standard themes, per `modus-catppuccin-take-over-modus-themes-mode'."
  (if-let* ((themes (modus-themes-get-all-known-themes 'modus-catppuccin))
            (sorted-a-z (sort themes #'string-lessp))
            (sorted-light-dark (modus-themes-sort sorted-a-z 'light)))
      sorted-light-dark
    modus-catppuccin-items))

;;;; Convenience commands

;;;###autoload (autoload 'modus-catppuccin-rotate "modus-catppuccin")
(modus-themes-define-derivative-command modus-catppuccin rotate)

;;;###autoload (autoload 'modus-catppuccin-select "modus-catppuccin")
(modus-themes-define-derivative-command modus-catppuccin select)

;;;###autoload (autoload 'modus-catppuccin-load-random "modus-catppuccin")
(modus-themes-define-derivative-command modus-catppuccin load-random)

;;;###autoload (autoload 'modus-catppuccin-load-random-dark "modus-catppuccin")
(modus-themes-define-derivative-command modus-catppuccin load-random-dark)

;;;###autoload (autoload 'modus-catppuccin-load-random-light "modus-catppuccin")
(modus-themes-define-derivative-command modus-catppuccin load-random-light)

;;;###autoload (autoload 'modus-catppuccin-list-colors "modus-catppuccin")
(modus-themes-define-derivative-command modus-catppuccin list-colors)

;;;###autoload (autoload 'modus-catppuccin-list-colors-current "modus-catppuccin")
(modus-themes-define-derivative-command modus-catppuccin list-colors-current)

;;;; Add themes from the package to the load path

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (file-equal-p dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'modus-catppuccin)
;;; modus-catppuccin.el ends here
