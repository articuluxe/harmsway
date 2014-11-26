;; -*- Mode: Emacs-Lisp -*-
;;
;;
;(setq frame-title-format "%b")
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
                                        ;
(setq default-frame-alist
      '(
        (cursor-color . "yellow")
        (cursor-type . (bar . 2))
        (background-color . "black")
        (foreground-color . "white")
        )
      )
(setq initial-frame-alist
      '(
        (top . 25) (left . 50) (height . 70) (width . 80)
        )
      )
