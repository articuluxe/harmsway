;;; pixel-themes.el --- pixel-themes -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;; Author: Lucas
;; URL: https://github.com/lucasobx/pixel-themes
;; Package-Requires: ((emacs "28.1") (modus-themes "5.0.0"))
;; Keywords: faces, theme

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
;; Emacs themes inspired by pixel art palettes.

;;; Code:

(require 'modus-themes)

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (file-equal-p dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

;;;; Group

(defgroup pixel-themes ()
  "Pixel art palette themes built on modus-themes."
  :group 'faces
  :group 'modus-themes
  :prefix "pixel-themes-"
  :tag "Pixel Themes")

;;;; Items

(defconst pixel-themes-dark-themes
  '(pixel-themes-exquisite-corpse
    pixel-themes-fallen-leaves
    pixel-themes-gray-weather
    pixel-themes-steam-lords
    pixel-themes-psygnosia
    pixel-themes-miri16
    pixel-themes-alia16)
  "List of dark Pixel themes.")

(defconst pixel-themes-light-themes
  '(pixel-themes-gothic-temple)
  "List of light Pixel themes.")

(defconst pixel-themes-items
  (append pixel-themes-dark-themes pixel-themes-light-themes)
  "List of all Pixel themes.")

;;;; Declaration

(defconst pixel-themes-with-properties
  '((pixel-themes-exquisite-corpse
     pixel-themes "Dark theme: faded parchment and dreamlike blues on black."
     dark pixel-themes-exquisite-corpse-palette nil pixel-themes-exquisite-corpse-palette-overrides)
    (pixel-themes-fallen-leaves
     pixel-themes "Dark theme: muted autumn tones."
     dark pixel-themes-fallen-leaves-palette nil pixel-themes-fallen-leaves-palette-overrides)
    (pixel-themes-gray-weather
     pixel-themes "Dark theme: cold and sad, with a quiet beauty."
     dark pixel-themes-gray-weather-palette nil pixel-themes-gray-weather-palette-overrides)
    (pixel-themes-steam-lords
     pixel-themes "Dark theme: deep purples, cold blues and greens."
     dark pixel-themes-steam-lords-palette nil pixel-themes-steam-lords-palette-overrides)
    (pixel-themes-psygnosia
     pixel-themes "Dark theme: copper skies over a muddy range."
     dark pixel-themes-psygnosia-palette nil pixel-themes-psygnosia-palette-overrides)
    (pixel-themes-miri16
     pixel-themes "Dark theme: indigo, olive, dusty mauve and harvest gold."
     dark pixel-themes-miri16-palette nil pixel-themes-miri16-palette-overrides)
    (pixel-themes-alia16
     pixel-themes "Dark theme: pink, indigo, warm gold and earthy greens."
     dark pixel-themes-alia16-palette nil pixel-themes-alia16-palette-overrides)
    (pixel-themes-gothic-temple
     pixel-themes "Light theme: weathered palette of stone, ash, and faded gold"
     light pixel-themes-gothic-temple-palette nil pixel-themes-gothic-temple-palette-overrides))
  "Properties for each Pixel theme.")

(defvar pixel-themes--declared-p nil
  "Non-nil if Pixel themes have been declared.")

(defun pixel-themes-declare-themes ()
  "Declare and register the Pixel themes with modus-themes."
  (unless pixel-themes--declared-p
    (dolist (theme pixel-themes-with-properties)
      (apply #'modus-themes-declare theme)
      (modus-themes-register (car theme)))
    (setq pixel-themes--declared-p t)))

(pixel-themes-declare-themes)

;;;; Minor mode

;;;###autoload
(define-minor-mode pixel-themes-mode
  "When enabled, all Modus theme commands consider only Pixel themes."
  :global t
  :init-value nil)

(cl-defmethod modus-themes-get-themes (&context (pixel-themes-mode (eql t)))
  "Return Pixel themes when `pixel-themes-mode' is enabled."
  (if-let* ((themes (modus-themes-get-all-known-themes 'pixel-themes))
            (sorted (modus-themes-sort themes 'light)))
      sorted
    pixel-themes-items))

;;;; Commands

;;;###autoload (autoload 'pixel-themes-select "pixel-themes")
(modus-themes-define-derivative-command pixel-themes select)

;;;###autoload (autoload 'pixel-themes-rotate "pixel-themes")
(modus-themes-define-derivative-command pixel-themes rotate)

;;;###autoload (autoload 'pixel-themes-load-random "pixel-themes")
(modus-themes-define-derivative-command pixel-themes load-random)

;;;###autoload (autoload 'pixel-themes-load-random-dark "pixel-themes")
(modus-themes-define-derivative-command pixel-themes load-random-dark)

;;;###autoload (autoload 'pixel-themes-load-random-light "pixel-themes")
(modus-themes-define-derivative-command pixel-themes load-random-light)

;;;###autoload (autoload 'pixel-themes-list-colors "pixel-themes")
(modus-themes-define-derivative-command pixel-themes list-colors)

;;;###autoload (autoload 'pixel-themes-list-colors-current "pixel-themes")
(modus-themes-define-derivative-command pixel-themes list-colors-current)

(defalias 'pixel-themes-load-theme #'modus-themes-load-theme)

;;;; Shared palette mappings

(defconst pixel-themes-palette-common
  '(;; semantic status
    (err     red)
    (warning yellow)
    (info    green)

    ;; links
    (fg-link         blue)
    (fg-link-visited magenta-faint)

    ;; text
    (name       fg-main)
    (keybind    blue-cooler)
    (identifier magenta-faint)
    (fg-prompt  blue-cooler)

    ;; syntax
    (builtin      fg-main)
    (comment      yellow-faint)
    (fnname       fg-main)
    (fnname-call  fg-dim)
    (keyword      blue-cooler)
    (preprocessor fg-main)
    (docstring    green-faint)
    (string       magenta)
    (type         green)
    (variable     yellow-warmer)
    (variable-use yellow-faint)
    (rx-backslash yellow-cooler)
    (rx-construct red)

    ;; accents
    (accent-0 blue-cooler)
    (accent-1 magenta)
    (accent-2 green)
    (accent-3 red)

    ;; dates
    (date-common           green-cooler)
    (date-deadline         red)
    (date-deadline-subtle  red-faint)
    (date-event            fg-alt)
    (date-holiday          magenta)
    (date-now              fg-main)
    (date-range            fg-alt)
    (date-scheduled        yellow)
    (date-scheduled-subtle yellow-faint)
    (date-weekday          cyan)
    (date-weekend          red-faint)

    ;; prose
    (fg-prose-code        magenta)
    (prose-done           green)
    (fg-prose-macro       blue-cooler)
    (prose-metadata       fg-dim)
    (prose-metadata-value fg-alt)
    (prose-table          fg-alt)
    (prose-table-formula  err)
    (prose-tag            yellow-faint)
    (prose-todo           red)
    (fg-prose-verbatim    cyan)

    ;; mail
    (mail-cite-0    blue)
    (mail-cite-1    magenta)
    (mail-cite-2    green-cooler)
    (mail-cite-3    yellow-cooler)
    (mail-part      magenta-faint)
    (mail-recipient blue-warmer)
    (mail-subject   blue-cooler)
    (mail-other     cyan)

    ;; search
    (bg-search-static  bg-warning)
    (bg-search-current bg-yellow-intense)
    (bg-search-lazy    bg-blue-intense)
    (bg-search-replace bg-red-intense)

    (bg-search-rx-group-0 bg-magenta-intense)
    (bg-search-rx-group-1 bg-green-intense)
    (bg-search-rx-group-2 bg-red-subtle)
    (bg-search-rx-group-3 bg-cyan-subtle)

    (bg-space-err bg-yellow-intense)

    ;; paren match
    (fg-paren-match fg-main)

    ;; diff fringe / intense
    (bg-added-fringe    green)
    (fg-added-intense   green-warmer)
    (bg-changed-fringe  yellow)
    (fg-changed-intense yellow-warmer)
    (bg-removed-fringe  red)
    (fg-removed-intense red-warmer)

    ;; infrastructure
    (fringe    unspecified)
    (property  variable)
    (fg-region unspecified)

    (bg-diff-context bg-dim)

    (bg-tab-bar     bg-alt)
    (bg-tab-current bg-main)
    (bg-tab-other   bg-active)

    (fg-link-symbolic        fg-alt)
    (underline-link          border)
    (underline-link-visited  border)
    (underline-link-symbolic border)

    (border-mode-line-active   border)
    (bg-mode-line-inactive     bg-alt)
    (fg-mode-line-inactive     fg-dim)
    (border-mode-line-inactive border)

    (bg-line-number-active   unspecified)
    (fg-line-number-active   accent-0)
    (bg-line-number-inactive unspecified)

    (bg-prominent-err     bg-err)
    (bg-prominent-warning bg-warning)
    (bg-prominent-note    bg-info)
    (fg-prominent-err     err)
    (fg-prominent-warning warning)
    (fg-prominent-note    info)

    (bg-space  unspecified)
    (fg-space  border)

    (bg-active-argument bg-warning)
    (fg-active-argument warning)
    (bg-active-value    bg-info)
    (fg-active-value    info)

    (bg-mark-delete bg-err)
    (fg-mark-delete err)
    (bg-mark-select bg-info)
    (fg-mark-select info)
    (bg-mark-other  bg-warning)
    (fg-mark-other  warning)

    (fg-search-current fg-main)
    (fg-search-lazy    fg-main)
    (fg-search-static  fg-main)
    (fg-search-replace fg-main)

    (fg-search-rx-group-0 fg-main)
    (fg-search-rx-group-1 fg-main)
    (fg-search-rx-group-2 fg-main)
    (fg-search-rx-group-3 fg-main)

    (fg-completion-match-0 accent-0)
    (fg-completion-match-1 accent-1)
    (fg-completion-match-2 accent-2)
    (fg-completion-match-3 accent-3)

    (fg-heading-0 rainbow-0)
    (fg-heading-1 rainbow-1)
    (fg-heading-2 rainbow-2)
    (fg-heading-3 rainbow-3)
    (fg-heading-4 rainbow-4)
    (fg-heading-5 rainbow-5)
    (fg-heading-6 rainbow-6)
    (fg-heading-7 rainbow-7)
    (fg-heading-8 rainbow-8))
  "Palette mappings shared by all pixel themes.")

(provide 'pixel-themes)
;;; pixel-themes.el ends here
