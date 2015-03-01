;; -*- Mode: Emacs-Lisp -*-
;;
;;

(defun add-punch-tags (project-root)
  (add-to-list 'etags-table-alist (list project-root
                                    (concat project-root "TAGS"))))

(profile-define-derived "punch" "default" "dharms" "danielrharms@gmail.com"
                        'project-name "punch"
                        'build-sub-dir "build/"
                        'src-sub-dir "punch/"
                        'on-file-open 'my/add-c-headers
                        'on-profile-init 'add-punch-tags
                        )
(setq profile-path-alist (cons (cons "src/projects/punch" "punch")
                               profile-path-alist))
