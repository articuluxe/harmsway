;;; mephistopheles-theme.el --- A monochromatic, purple-gray theme

;; Copyright (C) 2020 Brihadeesh S (@gitlab/peregrinator; @github/peregrinat0r)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Brihadeesh S <brihadeesh@protonmail.com>
;; URL: https://gitlab.com/peregrinator/mephistopheles.el
;; Version: 0.2
;; Package-Requires: ((colorless-themes "0.1"))
;; License: MIT
;; Keywords: faces theme

;;; Commentary:
;; This is a fork of beelzebub, a nearly colourless vim theme by Stanislav Karkavin
;; https://github.com/xdefrag/vim-beelzebub;
;; Made with the `colorless-themes` macro from Thomas Letan
;; https://github.com/peregrinat0r/colourless-themes.el

;;; Code:
(require 'colourless-themes)

(deftheme mephistopheles "A monochromatic purple-gray theme")

(colourless-themes-make mephistopheles
                       "#080808"    ; bg
                       "#080808"    ; bg+
                       "#444444"    ; current-line
                       "#282828"    ; fade
                       "#d7d7af"    ; fg
                       "#ffffbc"    ; fg+
                       "#87afaf"    ; primary
                       "#ff00ff"    ; red
                       "#800000"    ; orange
                       "#ffff00"    ; yellow
                       "#00ff00")   ; green

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mephistopheles)
(provide 'mephistopheles-theme)
;;; mephistopheles-theme.el ends here
