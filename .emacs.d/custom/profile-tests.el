;;; profile-tests.el --- test profiles
;; Copyright (C) 2016  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, December  9, 2016
;; Version: 1.0
;; Modified Time-stamp: <2016-12-30 07:17:43 dharms>
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

(ert-deftest profile-compile-test()
  (let ((byte-compile-error-on-warn t))
    (should (byte-compile-file "profile.el")) ;path?
    (delete-file "profile.elc" nil)))

(ert-deftest profile-find-basename-test ()
  (should (string-equal (prof--find-prof-basename "example.prof")
                        "example"))
  (should (string-equal (prof--find-prof-basename ".this.prof")
                        "this"))
  (should (not (string-equal (prof--find-prof-basename
                              "unknown") "")))
  )


;;; profile-tests.el ends here
