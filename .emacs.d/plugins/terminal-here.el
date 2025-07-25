;;; terminal-here.el --- Run an external terminal in current directory -*- lexical-binding: t; -*-

;; Copyright © 2017-2020 David Shepherd

;; Author: David Shepherd <davidshepherd7@gmail.com>
;; Version: 2.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools, frames
;; URL: https://github.com/davidshepherd7/terminal-here


;;; Commentary:

;; Provides commands to help open external terminal emulators in the
;; directory of the current buffer.


;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup terminal-here nil
  "Open external terminal emulators in current buffer's directory."
  :group 'external
  :prefix "terminal-here-")

(defun terminal-here--pick-linux-default ()
  "Inspect environment variables to try to figure out what kind of
Linux this is and pick a sensible terminal."
  (let ((xdg-current-desktop (or (getenv "XDG_CURRENT_DESKTOP") ""))
        (desktop-session (or (getenv "DESKTOP_SESSION") "")))
    (cond
     ;; Try to guess from the wide range of "standard" environment variables!
     ;; Based on xdg_util.cc.
     ((or (equal xdg-current-desktop "Enlightenment")
          (equal desktop-session "enlightenment"))
      'terminology)
     ((or (equal xdg-current-desktop "GNOME")
          (equal desktop-session "gnome")
          (equal desktop-session "mate")
          (equal xdg-current-desktop "Unity"))
      ;; Gnome has three possible terminals these days
      (cond ((executable-find "kgx") 'gnome-console)
            ((executable-find "ptyxis") 'ptyxis)
            (t 'gnome-terminal)))
     ((or (equal xdg-current-desktop "KDE")
          (equal desktop-session "kde")
          (equal desktop-session "kde-plasma")
          (equal desktop-session "kde4"))
      'konsole)
     ((or (equal xdg-current-desktop "LXQt")
          (equal desktop-session "lxqt"))
      'qterminal)
     ((or (equal xdg-current-desktop "XFCE")
          (equal desktop-session "xubuntu")
          (string-match-p (regexp-quote "xfce") desktop-session))
      'xfce4-terminal)

     ;; We've failed, hopefully we're on a Debian-based OS so that we can use this
     ((executable-find "x-terminal-emulator") 'x-terminal-emulator))))

(defcustom terminal-here-linux-terminal-command
  (terminal-here--pick-linux-default)
  "Specification of the command to use to start a terminal on Linux.

If `terminal-here-terminal-command' is non-nil it overrides this
setting.

Common settings:
    gnome-terminal
    ptyxis
    konsole
    xfce4-terminal
    terminator
    terminology
    xterm
    sakura
    urxvt
    xst
    st
    alacritty
    kitty
    tilix
    foot
    ghostty
    qterminal
    rio

Usually this variable should be one of the symbols listed above.

Alternatively to use a terminal which is not yet supported this
should be a list of strings representing the command line to run,
which will be passed to `start-process'.

For advanced use cases it can be a function which accepts a
launch directory and returns a list of strings to pass to
`start-process'."
  :group 'terminal-here
  :type '(choice (symbol)
                 (repeat string)
                 (function)))

(defcustom terminal-here-mac-terminal-command
  'terminal-app
  "Specification of the command to use to start a terminal on Mac OS X.

If `terminal-here-terminal-command' is non-nil it overrides this
setting.

Common settings:
    terminal-app
    iterm2
    alacritty
    kitty

Usually this variable should be one of the symbols listed above.

Alternatively to use a terminal which is not yet supported this
should be a list of strings representing the command line to run,
which will be passed to `start-process'.

For advanced use cases it can be a function which accepts a
launch directory and returns a list of strings to pass to
`start-process'."
  :group 'terminal-here
  :type '(choice (symbol)
                 (repeat string)
                 (function)))

(defcustom terminal-here-windows-terminal-command
  'cmd
  "Specification of the command to use to start a terminal on Windows.

If `terminal-here-terminal-command' is non-nil it overrides this
setting.

Common settings:
    cmd

Usually this variable should be one of the symbols listed above.

Alternatively to use a terminal which is not yet supported this
should be a list of strings representing the command line to run,
which will be passed to `start-process'.

For advanced use cases it can be a function which accepts a
launch directory and returns a list of strings to pass to
`start-process'."
  :group 'terminal-here
  :type '(choice (symbol)
                 (repeat string)
                 (function)))

(defcustom terminal-here-terminal-command
  nil
  "Specification of the command to use to start a terminal on all platforms.

If you use Emacs on multiple platforms with the same
configuration files you should normally use
`terminal-here-linux-terminal-command',
`terminal-here-mac-terminal-command', or
`terminal-here-windows-terminal-command' to configure this
instead in a platform specific way.

If non-nil this value overrides the platform-specific settings.



Usually this variable should be the symbol for a terminal, the
options are the keys of `terminal-here-terminal-command-table'.

Alternatively to use a terminal which is not in the table this
should be a list of strings representing the command line to run,
which will be passed to `start-process'.

For advanced use cases it can be a function which accepts a
launch directory and returns a list of strings to pass to
`start-process'."
  :group 'terminal-here
  :type '(choice (symbol)
                 (repeat string)
                 (function)))

(defcustom terminal-here-command-flag
  nil
  "The flag to tell your terminal to treat the rest of the line as a
command to run.

You should not normally need to set this variable.

If this is nil then terminal-here will try to automatically look
up the flag for your terminal in
`terminal-here-command-flag-table'."
  :group 'terminal-here
  :type 'string)

(defcustom terminal-here-project-root-function
  nil
  "Function called to find the current project root directory.

If nil falls back to `projectile-project-root', (which requires
you install the `projectile' package), or `vc-root-dir' which is
available in Emacs >= 25.1.

The function should return nil or signal an error if the current
buffer is not in a project."
  :group 'terminal-here
  :type '(choice (const nil) function))


(defcustom terminal-here-terminal-command-table
  (list
   ;; Linux
   (cons 'urxvt               (list "urxvt"))
   (cons 'gnome-terminal      (list "gnome-terminal"))
   ;; A newish gnome terminal alternative, the strange name seems to be here to
   ;; stay https://gitlab.gnome.org/GNOME/console/-/issues/119
   (cons 'gnome-console       (list "kgx")) 
   (cons 'ptyxis              (list "ptyxis" "--new-window"))
   (cons 'alacritty           (list "alacritty"))
   (cons 'xst                 (list "xst"))
   (cons 'st                  #'terminal-here--find-and-run-st)
   (cons 'konsole             (list "konsole"))
   (cons 'qterminal           (list "qterminal"))
   (cons 'xterm               (list "xterm"))
   (cons 'sakura              (list "sakura"))
   (cons 'xfce4-terminal      (list "xfce4-terminal"))
   (cons 'terminator          (list "terminator"))
   (cons 'terminology         (list "terminology"))
   (cons 'tilix               (list "tilix"))
   (cons 'kitty               (list "kitty"))
   (cons 'foot                (list "foot"))
   (cons 'ghostty             (list "ghostty"))
   (cons 'rio                 (list "rio"))

   ;; A default which picks a terminal based on the system configuration on
   ;; Debian-based OSes (but doesn't work for other Linux OSes)
   (cons 'x-terminal-emulator (list "x-terminal-emulator"))

   ;; Mac OS
   (cons 'terminal-app        (list "Terminal.app"))
   (cons 'iterm2              (list "iTerm.app"))

   ;; Windows
   ;; From http://stackoverflow.com/a/13509208/874671
   (cons 'cmd                 (list "cmd.exe" "/C" "start" "cmd.exe")))
  "A table of terminal commands.

The keys should be symbols, the values should be either a list of
strings: (terminal-binary arg1 arg2 ...); or a function taking a
directory and returning such a list.

When you add a new entry here you should also add an entry to
`terminal-here-command-flag-table' if you want to use
terminal-here with tramp files to create ssh connections."
  :group 'terminal-here
  :type '(repeat (cons symbol
                       (choice (repeat string)
                               (function)))))

(defcustom terminal-here-command-flag-table
  (list
   (cons 'urxvt          "-e")
   (cons 'gnome-terminal "-x")
   (cons 'gnome-console  "-e")
   (cons 'ptyxis         "--")
   (cons 'alacritty      "-e")
   (cons 'xst            "-e") ; popular st fork
   (cons 'st             "-e")
   (cons 'konsole        "-e") ; ssh seems to immediately exit with konsole
   (cons 'qterminal      "-e")
   (cons 'xterm          "-e")
   (cons 'sakura         "-e")
   (cons 'xfce4-terminal "-x")
   (cons 'terminator     "-x")
   (cons 'terminology    "-e")
   (cons 'tilix          "-e")
   (cons 'kitty          "--") ; kitty and foot don't need a special
   (cons 'foot           "--") ; flag for this, but we have to specify something.
   (cons 'ghostty        "-e")
   (cons 'rio            "-e")
   (cons 'x-terminal-emulator "-e") ; Actually this could be anything, but -e is
                                        ; the most common option.

   ;; I don't know how to do this on any Mac or Windows terminals! PRs please!
   )
  "A table of flags to tell terminals to use the rest of the line as
a command to run.

If `terminal-here-command-flag' is set then it will be used
instead of this table."
  :group 'terminal-here
  :type '(repeat (cons symbol
                       (choice (repeat string)
                               (function)))))

(defcustom terminal-here-verbose nil
  "Print commands before they are run."
  :group 'terminal-here
  :type 'boolean)



;;; Terminal command configuration

(defun terminal-here--non-function-symbol-p (x)
  (and (symbolp x) (not (functionp x))))

(defun terminal-here--os-terminal-command ()
  (or
   terminal-here-terminal-command
   (when (equal system-type 'gnu/linux) terminal-here-linux-terminal-command)
   (when (equal system-type 'darwin) terminal-here-mac-terminal-command)
   (when (or (equal system-type 'ms-dos)
             (equal system-type 'windows-nt)
             (equal system-type 'cygwin))
     terminal-here-windows-terminal-command)
   (user-error "No terminal configuration found for OS %s" system-type)))

(defun terminal-here--maybe-lookup-in-terminal-command-table (term-spec)
  (if (not (terminal-here--non-function-symbol-p term-spec))
      term-spec
    (let ((terminal-command (alist-get term-spec terminal-here-terminal-command-table)))
      (unless terminal-command
        (user-error "No settings found for terminal %s in `terminal-here-terminal-command-table'" term-spec))
      terminal-command)))

(defun terminal-here--maybe-funcall (dir x)
  (if (not (functionp x))
      x
    (funcall x dir)))

(defun terminal-here--maybe-add-mac-os-open (terminal-command)
  "On Mac OS we use the open command to run the terminal in `default-directory'."
  (if (and (equal system-type 'darwin)
           (not (equal (car terminal-command) "open")))
      (append (list "open" "-a" (car terminal-command) "." "--args") (cdr terminal-command))
    terminal-command))

(defun terminal-here--get-terminal-command (dir)
  (thread-last (terminal-here--os-terminal-command)
    (terminal-here--maybe-lookup-in-terminal-command-table)
    (terminal-here--maybe-funcall dir)
    (terminal-here--maybe-add-mac-os-open)))

(defun terminal-here--get-command-flag ()
  (or
   terminal-here-command-flag
   (let ((term-spec (terminal-here--os-terminal-command)))
     (when (terminal-here--non-function-symbol-p term-spec)
       (let ((flag (alist-get term-spec terminal-here-command-flag-table)))
         (unless flag
           (user-error "No flag settings found for terminal %s in `terminal-here-command-flag-table'" term-spec))
         flag)))
   (user-error "Couldn't work out how to run an ssh command in your terminal, customize `terminal-here-command-flag' or set `terminal-here-terminal-command' to specify your terminal by symbol")))



;;; Individual terminals

(defun terminal-here--find-and-run-st (_)
  "Suckless term can either be called st or stterm depending on how
it was installed."
  (if (executable-find "stterm") '("stterm") '("st")))



;;; Tramp/ssh support

(defun terminal-here--parse-ssh-dir (dir)
  (when (equal (file-remote-p dir 'method) "ssh")
    (let ((user (file-remote-p dir 'user))
	  (host (file-remote-p dir 'host))
	  (localname (file-remote-p dir 'localname)))
      (list (if user (concat user "@" host) host) localname))))

(defun terminal-here--ssh-command (ssh-data)
  (let ((remote (car ssh-data))
        (dir (cadr ssh-data)))
    (append (terminal-here--get-terminal-command "")
            (list (terminal-here--get-command-flag) "ssh" "-t" remote
                  "cd" (shell-quote-argument dir) "&&" "exec" "$SHELL" "-"))))

(defun terminal-here--tramp-path-to-directory (dir)
  "Extract the local part of a local tramp path.

Given a tramp path returns the local part, otherwise returns
nil."
  (let ((method (file-remote-p dir 'method)))
    (cond
     ;; sudo: just strip the extra tramp stuff
     ((equal method "sudo") (file-remote-p dir 'localname))
     ;; ssh: run with a custom command handled later
     ((equal method "ssh") dir)
     (t (user-error "Terminal here cannot currently handle tramp files other than sudo and ssh")))))




;;; Launching

(defun terminal-here-launch-in-directory (dir &optional inner-command)
  "Launch a terminal in directory DIR.

Handles tramp paths sensibly."
  (let* ((local-dir (if (file-remote-p dir) (terminal-here--tramp-path-to-directory dir) dir))
         (ssh-data (terminal-here--parse-ssh-dir dir))
         (terminal-command (if ssh-data
                               (terminal-here--ssh-command ssh-data)
                             (terminal-here--get-terminal-command local-dir))))
    (when (and ssh-data inner-command)
      (user-error "Custom commands are not currently supported over ssh."))

    (terminal-here--run-command
     (append terminal-command
             (when inner-command (list (terminal-here--get-command-flag)))
             inner-command)
     local-dir)))

(defun terminal-here--run-command (command dir)
  (when terminal-here-verbose
    (message "Running `%s` with default-directory `%s`" (string-join command " ") dir))
  (let* ((default-directory dir)
         (process-name (car command))
         (proc (apply #'start-process process-name nil command)))
    (set-process-sentinel
     proc
     (lambda (proc _)
       (when (and (eq (process-status proc) 'exit)
                  (or (/= (process-exit-status proc) 0)
                      terminal-here-verbose))
         (message "In terminal here, command `%s` exited with error code %d"
                  (string-join command " ")
                  (process-exit-status proc)))))
    ;; Don't close when emacs closes, seems to only be necessary on Windows.
    (set-process-query-on-exit-flag proc nil)))

;;;###autoload
(defun terminal-here-launch (&optional inner-command)
  "Launch a terminal in the current working directory.

This is the directory of the current buffer unless you have
changed it by running `cd'."
  (interactive)
  (terminal-here-launch-in-directory default-directory inner-command))

;;;###autoload
(defalias 'terminal-here 'terminal-here-launch)

;;;###autoload
(defun terminal-here-project-launch (&optional inner-command)
  "Launch a terminal in the current project root.

Uses `terminal-here-project-root-function' to determine the
project root."
  (interactive)
  (let* ((real-project-root-function
          (or terminal-here-project-root-function
              (cl-find-if #'fboundp (list 'projectile-project-root 'vc-root-dir))
              (user-error "No `terminal-here-project-root-function' is set and no default could be picked")))
         (root (funcall real-project-root-function)))
    (when (not root)
      (user-error "Not in any project according to `terminal-here-project-root-function'"))
    (terminal-here-launch-in-directory root inner-command)))



(provide 'terminal-here)

;;; terminal-here.el ends here


;; Handy code for re-evaluating configuration after adding a new terminal

;; (validate-setq terminal-here-terminal-command nil)
;; (validate-setq terminal-here-linux-terminal-command 'kitty)

;; (defun ds/custom-reset-var (symbl)
;;   "Reset SYMBL to its standard value."
;;   (set symbl (eval (car (get symbl 'standard-value)))))
;; (ds/custom-reset-var 'terminal-here-terminal-command-table)
;; (ds/custom-reset-var 'terminal-here-command-flag-table)


;; ;; Handy code for manually QA-ing things

;; (terminal-here-launch (list "htop"))
;; (terminal-here-launch (list "less" (buffer-file-name)))
;; (terminal-here-launch (list (getenv "SHELL") "-c" "ls && exec $SHELL"))

;; ;; These only work if you have the right directories with venvs in them:
;; (let ((default-directory "~/code/monorepo")
;;       (buffer-file-name "~/code/monorepo/README.md"))
;;   (terminal-here-launch (list (getenv "SHELL") "-c" "source .root-venv/bin/activate && exec $SHELL")))

;; (let ((default-directory "~/code/monorepo")
;;       (buffer-file-name "~/code/monorepo/README.md"))
;;   (terminal-here-launch (list "bash" "-c" "source .root-venv/bin/activate && exec bash")))

;; (progn (find-file "/ssh:aws:/home/ec2-user/test") (terminal-here-launch))
