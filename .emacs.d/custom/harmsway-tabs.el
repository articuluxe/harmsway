;;; harmsway-tabs.el --- tab customizations
;; Copyright (C) 2021  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, March 19, 2021
;; Version: 1.0
;; Modified Time-stamp: <2021-03-19 09:33:23 dharms>
;; Modified by: Dan Harms
;; Keywords: emacs gui

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
;; Tab customizations.
;;

;;; Code:

(setq tab-bar-show nil)
(tab-bar-mode 1)
(global-set-key "\C-xtu" #'tab-bar-undo-close-tab)
(global-set-key (kbd "C-5") #'tab-switcher)
(global-set-key (kbd "M-' 5") #'tab-switcher)
(global-set-key (kbd "C-7") #'tab-next)
(global-set-key (kbd "M-' 7") #'tab-next)
(global-set-key (kbd "C-6") (lambda () (interactive) (tab-next -1)))
(global-set-key (kbd "M-' 6") (lambda () (interactive) (tab-next -1)))

(defun harmsway-tab-name-fn ()
  "Return a suitable tab name, including project name."
  (or (proviso-current-project-name)
      (tab-bar-tab-name-current-with-count)))


(provide 'harmsway-tabs)
;;; harmsway-tabs.el ends here
