;; -*- Mode: Emacs-Lisp -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;; Dan Harms compiling.el ;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defvar one-window-in-frame nil)
(defvar my/compile-command)
(defvar my/compile-sub-command "make")

;; automatically scroll compilation window
(setq compilation-scroll-output t)

(defun my/compilation-mode-hook()
  (setq truncate-lines nil) ; is buffer local
  (set (make-local-variable 'truncate-partial-width-windows) nil))
(add-hook 'compilation-mode-hook 'my/compilation-mode-hook)

(defun create-compile-command() "Initialize the compile command."
  (interactive)
  (if (and (find-project-root) my/build-sub-dir)
      (format "cd %s && %s"
              (concat my/project-root my/build-sub-dir)
              my/compile-sub-command)
    (message "Can't create compile command: unknown %s"
             (if (null my/project-root)
                 "project root" "build sub-dir"))
    nil))

(defun my/compile() (interactive)
  (setq one-window-in-frame (one-window-p t))
  (when (setq my/compile-command (create-compile-command))
    (setq compile-command my/compile-command)
    (call-interactively 'compile)))
(defun my/recompile() (interactive)
  (setq one-window-in-frame (one-window-p t))
  (call-interactively 'recompile))

(add-hook 'compilation-start-hook '(lambda (process)
                                        ; the compile is about to start
                                     ))

(defun check-compile-buffer-errors(buffer)
  "Check the current buffer for compile warnings or errors"
  (with-current-buffer buffer
    (catch 'found
      (goto-char 1)
      (while (search-forward-regexp "\\([Ww]arning\\|[Ee]rror\\)" nil t)
        (goto-char (match-beginning 1))
        (unless
            (or
             ;; ignore the compile line "-Werror" that cmake echoes
             (save-match-data
               (looking-back "-W" (- (point) 2)))
             ;; ignore boost test output "No errors detected"
             (and
              (save-match-data
                (looking-back "[Nn]o " (- (point) 3)))
              (save-match-data
                (looking-at "errors detected")))
             )
          (throw 'found t))
        (goto-char (match-end 1)))
      nil)))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if it succeeded without warnings or errors."
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not (check-compile-buffer-errors buffer)))
      (run-with-timer 2 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (if one-window-in-frame
                            (delete-window (get-buffer-window buf t))
                          (switch-to-prev-buffer (get-buffer-window buf t)
                                                 'kill))) buffer)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)
