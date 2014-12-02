;; -*- Mode: Emacs-Lisp -*-
;;
;;
;(setq frame-title-format "%b")

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


(global-unset-key [?\C-x ?\C-z])
(setq default-fill-column 74) ; this defaults to 70
                                        ; FONT LOCK CUSTOMIZATION
                                        ; comments
(set-face-foreground 'font-lock-comment-face "yellow")
(set-variable font-lock-comment-face 'font-lock-comment-face)
                                        ; builtin
(set-face-foreground 'font-lock-builtin-face "SlateBlue")
(set-variable font-lock-builtin-face 'font-lock-builtin-face)
                                        ; constant
(set-face-foreground 'font-lock-constant-face "chocolate")
(set-variable font-lock-constant-face 'font-lock-constant-face)
                                        ; function-name
(set-face-foreground 'font-lock-function-name-face "maroon")
(set-variable font-lock-function-name-face 'font-lock-function-name-face)
                                        ; keyword
(set-face-foreground 'font-lock-keyword-face "SteelBlue")
(set-variable font-lock-keyword-face 'font-lock-keyword-face)
                                        ; string
(set-face-foreground 'font-lock-string-face "LimeGreen")
(set-variable font-lock-string-face 'font-lock-string-face)
                                        ; type
(set-face-foreground 'font-lock-type-face "peru")
(set-variable font-lock-type-face 'font-lock-type-face)
                                        ; variable-name
(set-face-foreground 'font-lock-variable-name-face "salmon")
(set-variable font-lock-variable-name-face 'font-lock-variable-name-face)
                                        ; warning
(set-face-foreground 'font-lock-warning-face "gold")
(set-variable font-lock-warning-face 'font-lock-warning-face)

(set-background-color "pink")

                                        ;
(setq default-frame-alist
      '(
        (cursor-type . (bar . 2))
        (background-color . "black")
        (foreground-color . "white")
        )
      )
(setq initial-frame-alist
      '(
        (top . 5) (left . 5) (height . 55) (width . 80)
        )
      )
;;(pos-tip-w32-max-width-height t)          ;maximize frame temporarily
