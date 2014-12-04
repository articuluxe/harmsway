;; -*- Mode: Emacs-Lisp -*-
;;
;;


(setq-default comint-process-echoes t)
(setq w32-get-true-file-attributes nil)

(add-hook 'c-mode-common-hook
          (lambda() (setq my/compile-command "nmake debug")) t)

(add-to-list 'full-edit-reject-patterns "^moc")
(add-to-list 'full-edit-reject-patterns "^qrc")
(add-to-list 'full-edit-reject-patterns "^ui")
