;; -*- Mode: Emacs-Lisp -*-
;;
;;

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
