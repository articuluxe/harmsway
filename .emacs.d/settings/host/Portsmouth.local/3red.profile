;; -*- Mode: Emacs-Lisp -*-
;;
;;

(defun add-3red-tags (project-root)
  (setq etags-table-alist
        (cons (list
               (concat "^\\(" project-root "\\).*$") ;capture entire line
               "/Users/dharms/src/clib-tags" ;these need to be absolute
               "/Users/dharms/src/boost-tags"
               ) etags-table-alist)))

(profile-define-derived "3red" "default" "dharms" "danielrharms@gmail.com"
                        'project-name "engine"
                        'on-file-open 'my/add-c-headers
                        'on-profile-init 'add-3red-tags
                        )
(setq profile-path-alist (cons (cons "src/projects/3red" "3red")
                               profile-path-alist))
(add-to-list 'sml/replacer-regexp-list '("^~/src/projects/3red/" ":3RED:") t)
