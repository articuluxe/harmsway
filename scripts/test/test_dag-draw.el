;;; test_dag-draw.el --- test dag-draw.el
;; Copyright (C) 2026  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, February 19, 2026
;; Version: 1.0
;; Modified Time-stamp: <2026-02-19 14:58:41 dharms>
;; Modified by: Dan Harms
;; Keywords:

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
(require 'dag-draw)

(let (graph (dag-draw-create-graph))
      (dag-draw-add-node graph 'start "Start Here")
      (dag-draw-add-node graph 'middle "Do Work")
      (dag-draw-add-node graph 'done "Finish")
      (dag-draw-add-edge graph 'start 'middle)
      (dag-draw-add-edge graph 'middle 'finish)
      (dag-draw-layout-graph graph)
      (dag-draw-render-graph graph 'ascii)
      )



;;; test_dag-draw.el ends here
)
