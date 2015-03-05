;; -*- Mode: Emacs-Lisp -*-
;;
;;

(defun add-punch-tags (project-root)
  (setq etags-table-alist
        (cons (list
               (concat "^\\(" project-root "\\).*$") ;this needs to capture
                                        ;the entire line
; unneeded               (concat "\\1" (profile-current-get 'src-sub-dir) "TAGS")
               "/Users/dharms/src/clib-tags" ;these need to be absolute
               "/Users/dharms/src/boost-tags"
               ) etags-table-alist)))

(profile-define-derived "punch" "default" "dharms" "danielrharms@gmail.com"
                        'project-name "punch"
                        'build-sub-dir "build/"
                        'src-sub-dir "punch/"
                        'on-file-open 'my/add-c-headers
                        'on-profile-init 'add-punch-tags
                        )
(setq profile-path-alist (cons (cons "src/projects/punch" "punch")
                               profile-path-alist))
(add-to-list 'sml/replacer-regexp-list '("^~/src/projects/punch/" ":PUNCH:") t)
