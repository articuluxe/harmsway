;; -*- Mode: Emacs-Lisp -*-
;; auto-complete-etags.el --- using etags with auto-complete
;; Copyright (C) 2015  Dan Harms (dharms)
;; Author: Dan Harms <danielrharms@gmail.com>
;; Created: Saturday, February 28, 2015
;; Version: 1.0
;; Modified Time-stamp: <2015-02-28 03:30:13 dharms>
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

;; Commentary:

;; This file is an update based on the original source by:

;; Copyright (C) 2014 Emanuele Tomasi <targzeta@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;; Author: Emanuele Tomasi <targzeta@gmail.com>
;; Version: 0.1
;; URL: https://gist.github.com/targzeta/10499354
;; Maintainer: Emanuele Tomasi <targzeta@gmail.com>
;; Keywords: auto-complete

;;; Commentary:

;; Source for Auto-Complete that uses etags.
;;
;; To use this program, copy this file in a directory which is in the Emacs
;; `load-path'. Then, execute the following code either directly or in your
;; .emacs file:
;;      (require 'auto-complete-etags)
;;      (setq-default ac-sources (add-to-list 'ac-sources 'ac-source-etags))

;;; Code:

(require 'auto-complete)

(defvar ac-etags-cache nil)
(defun ac-etags-init()
  (tags-completion-table)
  (mapatoms (lambda(item)
              (if (symbolp item) (push (symbol-name item) ac-etags-cache)))
            tags-completion-table))
;; Adding the source
(ac-define-source etags
  '( (init . ac-etags-init)
     (candidates . ac-etags-cache)
     (requires . 3)
     (symbol . "s")
     ))

(provide 'auto-complete-etags)

;;; auto-complete-etags.el ends here
