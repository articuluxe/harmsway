;;; profile-tests.el --- test profiles
;; Copyright (C) 2016-2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, December  9, 2016
;; Version: 1.0
;; Modified Time-stamp: <2017-01-05 08:45:14 dharms>
;; Modified by: Dan Harms
;; Keywords: profiles test

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

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'profile)

(defun prof-test-reset-all ()
  "Reset all profile-related data structures to nil."
  (setq prof-obarray (make-vector 7 0))
  (intern "default" prof-obarray)
  (setq prof-path-alist '())
  (setq prof-current nil)
  (setq prof-local (default-value 'prof-local))
  )

(ert-deftest prof-compile-test()
  (let ((byte-compile-error-on-warn t))
    (should (byte-compile-file "profile.el")) ;path?
    (delete-file "profile.elc" nil)))

(ert-deftest prof-manipulate-properties-test ()
  (prof-test-reset-all)
  (prof-define "test")
  (let ((p (intern-soft "test" prof-obarray)))
    (should (prof-p p))
    (should-not (prof-get p :a))
    (prof-put p :a "avalue")
    (should (string= "avalue" (prof-get p :a)))
    (prof-put p :a nil)
    (should-not (prof-get p :a))
    ))

(ert-deftest prof-manipulate-properties-derived-test ()
  (prof-test-reset-all)
  (prof-define "parent" :p 'value)
  (prof-define-derived "child" "parent")
  (let ((p (intern "child" prof-obarray)))
    (should (prof-p p))
    (should (eq (prof-get p :p) 'value))
    (should-not (prof-get p :p t))
    ))

(ert-deftest prof-compute-basename-test ()
  (should (string-equal (prof--compute-basename "example.prof")
                        "example"))
  (should (string-equal (prof--compute-basename ".this.prof")
                        "this"))
  (should (not (string-equal (prof--compute-basename
                              "unknown") "")))
  )

(ert-deftest prof-compute-stem-test ()
  (let ((prof (intern "temp" prof-obarray)) str)
    ;; absolute path
    (setq str "/home/me/temp/")
    (prof-put prof :root-dir str)
    (should (string= (prof--compute-stem prof) str))
    ;; absolute, without trailing slash
    (setq str "/home/me/temp")
    (prof-put prof :root-dir str)
    (should (string= (prof--compute-stem prof) str))
    (prof-put prof :root-dir "~/me")
    (should (string= (prof--compute-stem prof) "me"))
    ))

(ert-deftest prof-find-root-test ()
  (let ((base default-directory) dir)
    (setq dir (concat base "tests/a/b/c/d"))
    (should (equal (prof--find-root dir t)
                   (cons (concat base "tests/a/b/c/c.eprof")
                         (concat base "tests/a/b/c/"))))
    ))

(ert-deftest prof-open-profile-test ()
  (prof-test-reset-all)
  (let ((base (concat default-directory "tests/")))
    (find-file (concat base "a/b/c/d/dfile"))
    (should (equal prof-path-alist
                   (list (cons (concat base "a/b/c/") "c"))))
    (kill-buffer "dfile")
  ))

;;; profile-tests.el ends here
