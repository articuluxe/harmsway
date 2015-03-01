;; -*- Mode: Emacs-Lisp -*-
;;
;;
(profile-define-derived "3red" "default" "dharms" "danielrharms@gmail.com"
                        'project-name "engine"
                        'on-file-open 'my/add-c-headers
                        )
(setq profile-path-alist (cons (cons "src/projects/3red" "3red")
                               profile-path-alist))
