;;; luwak-org.el --- org integration for luwak. -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.
;; 
;; This file is part of luwak.
;; 
;; luwak is free software: you can redistribute it and/or modify it under
;; the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; luwak is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General
;; Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with luwak.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'org)
(require 'luwak)

(defun luwak-org-store-link ()
  (when (derived-mode-p 'luwak-mode)
    (org-link-store-props
     :type "luwak"
     :link (plist-get luwak-data :url)
     :description (luwak-guess-title))))

(org-link-set-parameters "luwak"
                         :follow #'luwak-open
                         :store #'luwak-org-store-link)

(provide 'luwak-org)
