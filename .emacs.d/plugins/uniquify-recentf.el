;; -*- Mode: Emacs-Lisp -*-
;; uniquify-recentf.el --- Uniquify recentf results
;; Implement functionality similar to uniquify to make recentf results bearable
;; Requires s.el and dash.el - awesome libraries from Magnar Sveen
;; Hat-tip : Baishampayan Ghose for the clojure implementation at
;; https://gist.github.com/ghoseb/8432086
;; Forked by Dan Harms Tuesday, August  4, 2015 08/04/15 11:14 AM
;; Version: 1.0
;; Modified Time-stamp: <2015-06-18 13:44:36 dan.harms>
;; Modified by: Dan Harms
;; Keywords: uniquify recentf ido

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:

;;

;; Code:
(require 'recentf)

(require 's)
(require 'dash)


(defun uniquify-recentf-explode (d)
  "Explode a directory name to its subcomponents."
  (s-split "/" d))


(defun uniquify-recentf-tails* (coll acc)
  "Return successive tails of a collection."
  (if (cdr coll)
      (uniquify-recentf-tails* (cdr coll) (cons coll acc))
    (cons coll acc)))


(defun uniquify-recentf-tails (coll)
  "Return successive tails of a collection."
  (uniquify-recentf-tails* coll '()))


(defun uniquify-recentf-paths (d)
  "Given a single directory, return all the possible sub-paths / name
  representations for it."
  (mapcar (lambda (xs) (s-join "/" xs))
          (uniquify-recentf-tails (uniquify-recentf-explode d))))


(defun uniquify-recentf-index-coll (tab coll)
  "Given a table and a collection, add each entry of the
  collection into the table. If the key already exists, inc it's
  value by 1"
  (mapc (lambda (x) (puthash x (+ 1 (gethash x tab 0)) tab)) coll)
  tab)


(defun uniquify-recentf-vm-uniquify (filenames)
  "Given a bunch of filenames (as returned by `recentf-list'),
  simplify the names to make them more easily readable."
  (let* ((expanded-paths (mapcar 'uniquify-recentf-paths filenames))
         (tab (make-hash-table :test 'equal))
         (freqs (mapcar (apply-partially 'uniquify-recentf-index-coll tab)
                        expanded-paths)))
    (mapcar (apply-partially '-first (lambda (x) (= 1 (gethash x tab 0))))
            expanded-paths)))


;; Mastering Emacs + some of my own elisp
(defun uniquify-recentf-ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (let* ((unique-filenames (uniquify-recentf-vm-uniquify recentf-list))
         (filename-map (-partition 2 (-interleave unique-filenames
                                                  recentf-list)))
         (short-filename (ido-completing-read "Choose recent file: "
                                              unique-filenames
                                              nil
                                              t)))
    (if short-filename
        (find-file (cadr (assoc short-filename filename-map)))
      (message "Aborting"))))

(provide 'uniquify-recentf)

;; uniquify-recentf.el ends here
