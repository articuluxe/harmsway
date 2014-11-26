;; -*- Mode: Emacs-Lisp -*-
;;
;;

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

(setq-default comint-process-echoes t)
(setq w32-get-true-file-attributes nil)
