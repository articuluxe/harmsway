(require 'recentf)

;; Implement functionality similar to uniquify to make recentf results bearable
;; Requires s.el and dash.el - awesome libraries from Magnar Sveen
;; Hat-tip : Baishampayan Ghose for the clojure implementation at
;; https://gist.github.com/ghoseb/8432086
(require 's)
(require 'dash)


(defun explode (d)
  "Explode a directory name to its subcomponents."
  (s-split "/" d))


(defun tails* (coll acc)
  "Return successive tails of a collection."
  (if (cdr coll)
      (tails* (cdr coll) (cons coll acc))
    (cons coll acc)))


(defun tails (coll)
  "Return successive tails of a collection."
  (tails* coll '()))


(defun paths (d)
  "Given a single directory, return all the possible sub-paths / name
  representations for it."
  (mapcar (lambda (xs) (s-join "/" xs)) (tails (explode d))))


(defun index-coll (tab coll)
  "Given a table and a collection, add each entry of the
  collection into the table. If the key already exists, inc it's
  value by 1"
  (mapcar (lambda (x) (puthash x (+ 1 (gethash x tab 0)) tab)) coll)
  tab)


(defun vm-uniquify (filenames)
  "Given a bunch of filenames (as returned by `recentf-list'),
  simplify the names to make them more easily readable."
  (let* ((expanded-paths (mapcar 'paths filenames))
         (tab (make-hash-table :test 'equal))
         (freqs (mapcar (apply-partially 'index-coll tab) expanded-paths)))
    (mapcar (apply-partially '-first (lambda (x) (= 1 (gethash x tab 0))))
            expanded-paths)))


;; Mastering Emacs + some of my own elisp
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (let* ((unique-filenames (vm-uniquify recentf-list))
         (filename-map (-partition 2 (-interleave unique-filenames
                                                  recentf-list)))
         (short-filename (ido-completing-read "Choose recent file: "
                                              unique-filenames
                                              nil
                                              t)))
    (if short-filename
        (find-file (cadr (assoc short-filename filename-map)))
      (message "Aborting"))))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
