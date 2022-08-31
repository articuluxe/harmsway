;;; stimmung-themes.el --- Themes tuned to inner harmonies -*- lexical-binding: t -*-
;; Copyright © 2019

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/stimmung-themes
;; Package-Requires: ((emacs "25"))
;; Created: 2019-12-20
;; Version: 2021-03-20
;; Keywords: faces

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Stimmung (dark and light) is a pair of monochrome Emacs themes
;; with minimal syntax highlighting.  They are inspired by Tonsky's
;; Alabaster theme (https://github.com/tonsky/sublime-scheme-alabaster),
;; following the maxim that a theme that highlights everything
;; paradoxically highlights nothing.  Text backgrounds (comments,
;; strings and constants) and font variations (definitions) are used
;; as alternatives to text colors, ensuring a harmonious reading
;; experience.  Use `stimmung-themes-dark-highlight-color' and
;; `stimmung-themes-light-highlight-color' to customize the highlight.
;;
;; Screenshots are available at: https://github.com/motform/stimmung-themes

;;; Code:

;;; Theme loading/toggle inspired/sourced from the fantastic `protesilaos/modus-themes'

(defgroup stimmung-themes nil
  "Stimmung settings.
You have to re-load the theme for these changes to take effect."
  :group  'faces
  :prefix "stimmung-theme-"
  :link   '(url-link "https://github.com/motform/stimmung-themes"))


;;; Highlight colors

(defcustom stimmung-themes-dark-highlight-color "#40382b" ; I dub this shade "Japanese gravy"
  "The dark theme color for highlights, the only non-monochrome color in code."
  :type  'string
  :group 'stimmung-themes)

(defcustom stimmung-themes-dark-highlight-color-foreground "bisque1"
  "The dark theme color for highlights that are defined as 'foreground.
There are no 'foreground colors active by default."
  :type  'string
  :group 'stimmung-themes)

(defcustom stimmung-themes-light-highlight-color "cornsilk1"
  "The light theme color for highlights, the only non-monochrome color in code."
  :type  'string
  :group 'stimmung-themes)

(defcustom stimmung-themes-light-highlight-color-foreground "dark goldenrod"
  "The light theme color for highlights that are defined as 'foreground.
There are no 'foreground colors active by default."
  :type  'string
  :group 'stimmung-themes)


;;; font-lock faces

(defmacro stimmung-themes--font-lock-face (name default)
  "Register the custom font-lock-face for NAME with value DEFAULT."
  (let ((custom-name (intern (concat "stimmung-themes-" name))))
	`(defcustom ,custom-name ,default
	   ,(format "The type of highlighting used for %s." name)
	   :type    'symbol
	   :group   'stimmung-themes
	   :options '('background 'foreground 'none))))

(stimmung-themes--font-lock-face "builtin"  'background)
(stimmung-themes--font-lock-face "comment"  'background)
(stimmung-themes--font-lock-face "constant" 'background)
(stimmung-themes--font-lock-face "string"   'background)
(stimmung-themes--font-lock-face "markup"   'background)
(stimmung-themes--font-lock-face "type"	    'background)
(stimmung-themes--font-lock-face "function-name" 'none)
(stimmung-themes--font-lock-face "keyword"		 'none)
(stimmung-themes--font-lock-face "variable-name" 'none)
(stimmung-themes--font-lock-face "preprocessor"  'none)
(stimmung-themes--font-lock-face "regex"         'none)

;;;; Interactive functions

;;;###autoload
(defun stimmung-themes-load-dark ()
  "Load `stimmung-dark' and disable `stimmung-light'."
  (interactive)
  (disable-theme 'stimmung-themes-light)
  (load-theme 'stimmung-themes-dark t))

;;;###autoload
(defun stimmung-themes-load-light ()
  "Load `stimmung-light' and disable `stimmung-dark'."
  (interactive)
  (disable-theme 'stimmung-themes-dark)
  (load-theme 'stimmung-themes-light t))

;;;###autoload
(defun stimmung-themes--toggle-prompt ()
  "Helper for `stimmung-themes-toggle'."
  (let ((theme (intern (completing-read "Load Stimmung theme: "
										'(stimmung-themes-light stimmung-themes-dark) nil t))))
	(mapc #'disable-theme custom-enabled-themes) ; make sure to disable any non-stimmung themes to ignore accidental face-overlap
	(pcase theme
	  ('stimmung-themes-light (stimmung-themes-load-light))
	  ('stimmung-themes-dark  (stimmung-themes-load-dark)))))

;;;###autoload
(defun stimmung-themes-toggle ()
  "Toggle between the dark and light version of `stimmung-themes'.
Prompt the user for which to pick in case none is enabled.
Currently assumes the themes is loaded, which might be an issue.
Inspired by stimmung-themes."
  (interactive)
  (pcase (car custom-enabled-themes)
	('stimmung-themes-light (stimmung-themes-load-dark))
	('stimmung-themes-dark  (stimmung-themes-load-light))
	(_ (stimmung-themes--toggle-prompt))))

(provide 'stimmung-themes)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; stimmung-themes.el ends here
