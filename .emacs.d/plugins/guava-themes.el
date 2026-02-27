;;; guava-themes.el --- A pack of plant-inspired themes -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026

;; Author: Geralld Borbón <eternalmangocean@gmail.com>
;; Created: Dec 07, 2025
;; Version: 0.11.1
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
  "Guava theme options.
The theme has to be reloaded after changing anything in this group."
  :prefix "guava-themes-" :group 'faces)

(defface guava-themes-visible-bell '()
  "Face to use as a replacement for `visible-bell'."
  :group 'guava-themes)

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
  "Change the blink of the minibuffer with a blink for the mode-line.
Set `ring-bell-function' with this function as its value to use it."
  (let* ((buf (current-buffer))
         (faces (if (facep 'mode-line-active)
                    '(mode-line-active)
                 '(mode-line)))
         (cookies (mapcar (lambda (face)
                            (when (facep face)
                              (face-remap-add-relative face 'guava-themes-visible-bell)))
                          faces)))
    (force-mode-line-update)
    (run-with-timer 0.15 nil
                    (lambda ()
                      (with-current-buffer buf
                        (mapc #'face-remap-remove-relative cookies)
                        (force-mode-line-update))))))

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
;; End:

;;; guava-themes.el ends here
