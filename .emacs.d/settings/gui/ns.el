;; -*- Mode: Emacs-Lisp -*-
;;
;;

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; scroll one line at a time
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse nil)

;; toggle full-screen
(defun ns-toggle-full-screen() (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil 'fullboth)))
(global-set-key [f11] 'ns-toggle-full-screen)

(setq initial-frame-alist
      '(
        (top . 25) (left . 50) (height . 70) (width . 80)
        ))
(setq default-frame-alist
      '(
        ;; (cursor-color . "yellow")
        (cursor-type . (bar . 2))
        (height . 70) (width . 80)
        ))
