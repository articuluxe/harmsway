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

(setq initial-frame-alist
      '(
        (top . 5) (left . 5) (height . 70) (width . 95)
        ))
(setq default-frame-alist
      '(
        (cursor-type . (bar . 2))
        (height . 70) (width . 95)
        ))
