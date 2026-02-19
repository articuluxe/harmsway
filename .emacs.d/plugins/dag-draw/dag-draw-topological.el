;;; dag-draw-topological.el --- Topological rank assignment fallback -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;; Author: Claude Code
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; GKNV Baseline Compliance:
;;
;; This module implements topological sort fallback for rank assignment.
;; Used when network simplex fails or for simple graphs where optimization
;; is not required.
;;
;; GKNV Reference: Section 2.3 (init_rank function, Figure 2-2 line 2)
;; Decision: D1.2 - Queue-based topological ranking (GKNV init_rank)
;; Algorithm: Kahn's algorithm (queue-based topological sort)
;;
;; Key Requirements:
;; - Processes nodes in topological order
;; - Respects minimum edge lengths δ(e)
;; - Assigns least rank that satisfies in-edges
;; - Used as fallback when network simplex fails
;;
;; Baseline Status: ✅ Compliant (Fallback implementation)
;;
;; GKNV Figure 2-2, line 2 states: "Our version keeps nodes in a queue.  Nodes are
;; placed in the queue when they have no unscanned in-edges."
;;
;; See doc/implementation-decisions.md (D1.2) for decision rationale.
;;
;; Functions:
;; - dag-draw--assign-ranks-topological: Main topological rank assignment

;;; Code:

(require 'ht)
(require 'cl-lib)
(require 'dag-draw-core)

;;; Topological Rank Assignment

(defun dag-draw--assign-ranks-topological (graph)
  "Assign ranks to nodes using topological sorting.

GRAPH is a `dag-draw-graph' structure to process.

This is a fallback algorithm when network simplex optimization fails.
Implements Kahn's algorithm with level-by-level processing (GKNV init_rank).
Processes nodes in topological order, respecting minimum edge lengths δ(e).
Assigns the least rank that satisfies all incoming edges.

Modifies GRAPH in place by setting node ranks and max-rank.
Returns the modified GRAPH."
  (let ((in-degree (ht-create))
        (queue '())
        (current-rank 0))

    ;; Step 1: Calculate in-degrees for all nodes
    (dolist (node-id (dag-draw-get-node-ids graph))
      (ht-set! in-degree node-id 0))

    (dolist (edge (dag-draw-graph-edges graph))
      (let ((from-node (dag-draw-edge-from-node edge))
            (to-node (dag-draw-edge-to-node edge)))
        ;; GKNV Section 2, line 361: "loops are ignored" in rank assignment
        (unless (eq from-node to-node)
          (ht-set! in-degree to-node (1+ (ht-get in-degree to-node 0))))))

    ;; Step 2: Find initial nodes with in-degree 0 (source nodes)
    (dolist (node-id (dag-draw-get-node-ids graph))
      (when (zerop (ht-get in-degree node-id))
        (push node-id queue)))

    ;; Step 3: Process nodes level by level using Kahn's algorithm
    (while queue
      (let ((current-level queue))
        (setq queue '())

        ;; Assign current rank to all nodes in this level
        (dolist (node-id current-level)
          (let ((node (dag-draw-get-node graph node-id)))
            (setf (dag-draw-node-rank node) current-rank)))

        ;; Update in-degrees and find next level
        (dolist (node-id current-level)
          (dolist (successor (dag-draw-get-successors graph node-id))
            ;; GKNV Section 2, line 361: "loops are ignored" in rank assignment
            (unless (eq node-id successor)
              (ht-set! in-degree successor (1- (ht-get in-degree successor)))
              (when (zerop (ht-get in-degree successor))
                (push successor queue)))))

        (setq current-rank (1+ current-rank))))

    ;; Step 4: Set maximum rank in graph for layout algorithms
    (setf (dag-draw-graph-max-rank graph) (1- current-rank))

    graph))

(provide 'dag-draw-topological)

;;; dag-draw-topological.el ends here