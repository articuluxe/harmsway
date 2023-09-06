;;; good-word.el --- utilities for word processing
;; Copyright (C) 2016, 2018-2019, 2021, 2023  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, October 18, 2016
;; Version: 1.0
;; Modified Time-stamp: <2023-09-05 16:57:01 dharms>
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
(require 'use-package)

(use-package wc-goal-mode :commands wc-goal-mode)

(use-package adaptive-wrap
  :config
  (setq-default adaptive-wrap-extra-indent -2)
  )

(defun good-word/init-word-processor()
  (setq indent-tabs-mode nil)
  (add-hook 'visual-line-mode-hook
            (lambda()
              (adaptive-wrap-prefix-mode)))
  (visual-line-mode 1)
  (require 'flymake-collection-proselint)
  (flymake-mode 1)
  ;; (wc-goal-mode 1)
  ;; uncomment to move by logical lines, not visual lines
  ;; (setq line-move-visual nil)
  ;; uncomment as an alternative to visual-line-mode that only word
  ;; wraps, without removing wrap indicators in the fringe, and without
  ;; altering movement commands to use visual lines rather than logical ones.
  ;; (setq truncate-lines nil)
  ;; (setq word-wrap t)
  ;;
  ;; (set-face-attribute 'default nil :height 130)
  ;; (set-face-attribute 'fixed-pitch nil :family "Fira Code")
  ;; (set-face-attribute 'variable-pitch nil :family "Georgia")
  ;; (variable-pitch-mode 1)
  ;;
  ;; (olivetti-mode 1)    ;center text in buffer
  ;; (typo-mode 1)        ;handle dash symbols
  )

(defhydra hydra-toggle-word-processor (:color teal)
  "word-processor"
  ("f" auto-fill-mode "auto-fill-mode")
  ("a" adaptive-wrap-prefix-mode "adaptive-wrap")
  ("r" refill-mode "refill-mode")
  ("l" font-lock-mode "font-lock-mode")
  ("t" toggle-truncate-lines "truncate lines")
  ("j" hydra-set-justification/body "justify" :exit t)
  ("c" wc-goal-mode "word-count-goal-mode")
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
