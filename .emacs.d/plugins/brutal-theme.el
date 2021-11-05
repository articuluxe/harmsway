;;; brutal-theme.el --- Brutalist theme -*- lexical-binding: t; -*-

;; Copyright (C) 2021, Topi Kettunen <topi@kettunen.io>

;; Author: Topi Kettunen <topi@kettunen.io>
;; URL: https://github.com/topikettunen/brutal-emacs
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; Brutalist theme for Emacs for all your minimalistic needs.

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'brutal-theme)

;;; brutal-theme.el ends here
