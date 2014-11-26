;; -*- Mode: Emacs-Lisp -*-
;;
;;
                                        ; FONT LOCK CUSTOMIZATION
                                        ; comment
(set-face-foreground 'font-lock-comment-face "yellow")
(set-variable font-lock-comment-face 'font-lock-comment-face)
                                        ; comment-delimiter
(set-face-foreground 'font-lock-comment-delimiter-face "yellow")
(set-variable font-lock-comment-delimiter-face 'font-lock-comment-delimiter-face)
                                        ; builtin
(set-face-foreground 'font-lock-builtin-face "blue")
(set-variable font-lock-builtin-face 'font-lock-builtin-face)
                                        ; constant
(set-face-foreground 'font-lock-constant-face "cyan")
(set-variable font-lock-constant-face 'font-lock-constant-face)
                                        ; function-name
(set-face-foreground 'font-lock-function-name-face "red")
(set-variable font-lock-function-name-face 'font-lock-function-name-face)
                                        ; keyword
(set-face-foreground 'font-lock-keyword-face "magenta")
(set-variable font-lock-keyword-face 'font-lock-keyword-face)
                                        ; string
(set-face-foreground 'font-lock-string-face "green")
(set-variable font-lock-string-face 'font-lock-string-face)
                                        ; type
(set-face-foreground 'font-lock-type-face "magenta")
(set-variable font-lock-type-face 'font-lock-type-face)
                                        ; variable-name
(set-face-foreground 'font-lock-variable-name-face "white")
(set-variable font-lock-variable-name-face 'font-lock-variable-name-face)
                                        ; warning
(set-face-foreground 'font-lock-warning-face "red")
(set-variable font-lock-warning-face 'font-lock-warning-face)
                                        ;
(setq default-frame-alist
      '(
        (cursor-color . "red")
        (cursor-type . (bar . 2))
        )
      )
