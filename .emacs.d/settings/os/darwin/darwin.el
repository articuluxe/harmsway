;; darwin.el --- os settings file
;; Copyright (C) 2015-2021, 2023  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Modified Time-stamp: <2023-05-17 09:58:27 dharms>
;; Modified by: Dan Harms
;; Keywords:

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

;;; Code:

(eval-when-compile
  (setq use-package-verbose t)
  (require 'use-package))

(set-register ?\C-c (cons 'file "~/Library/Mobile Documents/com~apple~CloudDocs"))

(when (executable-find "gls")
  (setq insert-directory-program "gls"))

(setq ns-pop-up-frames 'fresh)
(setq mac-system-move-file-to-trash-use-finder t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Process Viewer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vkill :bind ("C-c 0p" . vkill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; osx-plist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package osx-plist
  :commands (osx-plist-parse-file osx-plist-parse-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; homebrew ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'harmsway-homebrew-map)
(use-package homebrew
  :bind (("C-c 0h" . harmsway-homebrew-map)
         :map harmsway-homebrew-map
         ("i" . homebrew-install)
         ("u" . homebrew-upgrade)
         ("d" . homebrew-update)
         ("e" . homebrew-edit)
         ("I" . homebrew-info)
         ("p" . homebrew-package-info)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gif-screencast ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'gif-screencast
  (setq gif-screencast-args '("-x"))
  (setq gif-screencast-capture-format "ppm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; osx-dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package osx-dictionary)

(defun harmsway-finder-path()
  "Return path of foremost Finder window."
  (let* ((script "tell application \"Finder\" to if (count of Finder windows) > 0 then get POSIX path of (target of front Finder window as text)")
         (result (ns-do-applescript script)))
    (if result (string-trim result) "")))

(defun harmsway-dired-finder-path ()
  "Open Finder's foremost window in DIRED."
  (interactive)
  (let ((path (harmsway-finder-path)))
    (if (and path (not (string-empty-p path)))
        (dired path)
      (user-error "No finder window found")))
  )

(global-set-key "\C-c\M-E" #'harmsway-dired-finder-path)

;; mdfind extraneous output fix for counsel
(defun harmsway-counsel-locate-cmd-mdfind (input)
  "Find INPUT.  Hack for `mdfind' that prints out extra debug info."
  (counsel-require-program locate-command)
  (format "mdfind -name %s 2>/dev/null" (shell-quote-argument input)))

(setq counsel-locate-cmd #'harmsway-counsel-locate-cmd-mdfind)

;; darwin.el ends here
