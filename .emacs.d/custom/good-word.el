;;; good-word.el --- utilities for word processing
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, October 18, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-10-20 18:11:06 dharms>
;; Modified by: Dan Harms
;; Keywords: text writing

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

(require 'hydra)

(defun good-word/init-word-processor()
  (setq indent-tabs-mode nil)
  (visual-line-mode 1)
  ;; uncomment to move by logical lines, not visual lines
  ;; (setq line-move-visual nil)
  ;; uncomment as an alternative to visual-line-mode that only word
  ;; wraps, without removing wrap indicators in the fringe, and without
  ;; altering movement commands to use visual lines rather than logical ones.
  ;; (setq truncate-lines nil)
  ;; (setq word-wrap t)
  )

(defhydra hydra-toggle-word-processor (:color teal)
  "word-processor"
  ("f" auto-fill-mode "auto-fill-mode")
  ("r" refill-mode "refill-mode")
  ("l" font-lock-mode "font-lock-mode")
  ("t" toggle-truncate-lines "truncate lines")
  ("j" hydra-set-justification/body "justify" :exit t)
  ("q" nil "cancel")
  )

(defhydra hydra-set-justification (:color blue)
  "justify"
  ("l" (lambda()(interactive)(setq default-justification 'left)) "left")
  ("r" (lambda()(interactive)(setq default-justification 'right)) "right")
  ("f" (lambda()(interactive)(setq default-justification 'full)) "full")
  ("c" (lambda()(interactive)(setq default-justification 'center)) "center")
  ("n" (lambda()(interactive)(setq default-justification 'none)) "none")
  ("q" nil "exit" :exit t)
  )

(provide 'good-word)

;;; good-word.el ends here
