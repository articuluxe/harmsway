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

;; (setq frame-title-format "%b")

;; toggle full-screen
(defun w32-maximize-frame() "Maximize current frame on windows"
  (interactive)
  (w32-send-sys-command 61488))
(defun w32-minimize-frame() "Restore (un-maximize) current frame on windows"
  (interactive)
  (w32-send-sys-command 61728))
(defun w32-toggle-full-screen() "Toggle full screen" (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil 'fullscreen))
  (if (frame-parameter nil 'fullscreen)
      (w32-maximize-frame)
    (w32-minimize-frame)))
(global-set-key [f11] 'w32-toggle-full-screen)


(setq initial-frame-alist
      '(
        (top . 5) (left . 5) (height . 53) (width . 80)
        ))
(setq default-frame-alist
      '(
        (cursor-type . (bar . 2))
        (height . 55) (width . 80)
        ))

(set-face-font 'default "Consolas-11")

;;(pos-tip-w32-max-width-height t)          ;maximize frame temporarily
