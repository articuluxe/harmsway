;; -*- Mode: Emacs-Lisp -*-
;;
;;
(profile-define-derived "punch" "default" "dharms" "danielrharms@gmail.com"
                        'project-name "punch"
                        'build-sub-dir "build/"
                        'src-sub-dir "punch/"
                        'funcall 'my/add-c-headers
                        )
(setq profile-path-alist (cons (cons "src/projects/punch" "punch")
                               profile-path-alist))
