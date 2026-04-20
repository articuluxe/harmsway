;;; guava-themes.el --- A pack of plant-inspired themes -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 07, 2025
;; Version: 0.14.0
;; Keywords: themes, faces, color
;; URL: http://github.com/bormoge/guava-themes
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A pack of plant-inspired themes for GNU Emacs.
;;
;;; Code:

(require 'face-remap)

(unless (>= emacs-major-version 24)
  (error "A version of Emacs equal or superior to 24 is required"))

(defgroup guava-themes nil
  "Options for guava-themes.
The theme has to be reloaded after changing anything in this group."
  :prefix "guava-themes-" :group 'faces)

(defface guava-themes-visible-bell
  '((t (:foreground "#FFFFFF" :background "#808080")))
  "Face used on `guava-themes-change-visible-bell' as a replacement for `visible-bell'."
  :group 'guava-themes)

(defcustom guava-themes-visible-bell-idle-delay 0.0
  "Number of seconds to wait before displaying `guava-themes-visible-bell'.

If this variable is set to 0.0, display `guava-themes-visible-bell' without any delay."
  :group 'guava-themes
  :type 'float)

(defcustom guava-themes-visible-bell-duration 0.15
  "Number of seconds used to display `guava-themes-visible-bell'.

If this variable is set to 0.0, the function `guava-themes-change-visible-bell'
is still called but does not display `guava-themes-visible-bell'."
  :group 'guava-themes
  :type 'float)

(defcustom guava-themes-visible-bell-faces-list (if (>= emacs-major-version 29) '(mode-line-active) '(mode-line))
  "List of faces modified when `guava-themes-change-visible-bell' is called.

By default, this variable contains either `mode-line-active' if
`emacs-major-version' is equal or above 29, or `mode-line' if it's below 29.

If this variable is set to nil, the function `guava-themes-change-visible-bell'
is still called but does not display `guava-themes-visible-bell'."
  :group 'guava-themes
  :type '(repeat symbol))

(defcustom guava-themes-before-change-visible-bell-hook nil
  "Hook that is run before displaying `guava-themes-visible-bell'."
  :group 'guava-themes
  :type 'hook)

(defcustom guava-themes-after-change-visible-bell-hook nil
  "Hook that is run after displaying `guava-themes-visible-bell'."
  :group 'guava-themes
  :type 'hook)



;; Henrik Lissner / Doom Emacs are the original authors of `doom-themes-visual-bell-fn'
;; As per the MIT license, here is the original copyright and permission notice of `doom-themes-ext-visual-bell.el'

;; Copyright (c) 2016-2024 Henrik Lissner.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

(defun guava-themes-change-visible-bell ()
  "Replace the `visible-bell' blink.
Set `ring-bell-function' with this function as its value to use it.

The value of `guava-themes-visible-bell-faces-list' will determine which faces
will blink when this function is called."
  (run-hooks 'guava-themes-before-change-visible-bell-hook)
  (sit-for guava-themes-visible-bell-idle-delay)
  (let* ((buf (current-buffer))
         (faces guava-themes-visible-bell-faces-list)
         (cookies (mapcar (lambda (face)
                            (when (facep face)
                              (face-remap-add-relative face 'guava-themes-visible-bell)))
                          faces)))
    (force-mode-line-update)
    (run-with-timer guava-themes-visible-bell-duration nil
                    (lambda ()
                      (with-current-buffer buf
                        (mapc #'face-remap-remove-relative cookies)
                        (force-mode-line-update)))))
  (run-hooks 'guava-themes-after-change-visible-bell-hook))



;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (let* ((base (file-name-directory load-file-name))
                      (dir (expand-file-name "themes/" base)))
                 (or (and (file-directory-p dir) dir)
                     base))))

(provide 'guava-themes)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (featurep 'package-lint-flymake) (package-lint-flymake-setup))
;; End:

;;; guava-themes.el ends here
