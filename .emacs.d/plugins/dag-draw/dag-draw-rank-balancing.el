;;; dag-draw-rank-balancing.el --- Rank balancing for GKNV layout algorithm -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;; Author: Claude Code
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This module implements GKNV rank balancing algorithms for improving
;; layout quality after initial rank assignment.
;;
;; Based on GKNV Figure 2-1 step 8: balance() function.
;; "Nodes having equal in- and out-edge weights and multiple feasible ranks
;; are moved to a feasible rank with the fewest nodes.  The purpose is to
;; reduce crowding and improve the aspect ratio of the drawing."
;;
;; Functions:
;; - dag-draw-normalize-ranks: Normalize ranks so minimum rank is 0
;; - dag-draw-balance-ranks: Apply GKNV balancing algorithm
;; - dag-draw--node-eligible-for-balancing-p: Check GKNV balancing criteria
;; - dag-draw--find-feasible-ranks: Find valid ranks for node placement

;;; Code:

(require 'ht)
(require 'cl-lib)
(require 'dag-draw-core)

;;; Rank Normalization

(defun dag-draw-normalize-ranks (graph)
  "Normalize ranks so the minimum rank is 0.

GRAPH is a `dag-draw-graph' structure with assigned ranks.

Finds the minimum rank across all nodes and adjusts all ranks by
subtracting this minimum, ensuring ranks start from 0.

Modifies GRAPH in place by updating node ranks and max-rank.
Returns the modified GRAPH."
  (let ((min-rank most-positive-fixnum))

    ;; Find minimum rank
    (ht-each (lambda (_node-id node)
               (when (dag-draw-node-rank node)
                 (setq min-rank (min min-rank (dag-draw-node-rank node)))))
             (dag-draw-graph-nodes graph))

    ;; Adjust all ranks
    (when (< min-rank most-positive-fixnum)
      (ht-each (lambda (_node-id node)
                 (when (dag-draw-node-rank node)
                   (setf (dag-draw-node-rank node)
                         (- (dag-draw-node-rank node) min-rank))))
               (dag-draw-graph-nodes graph))

      ;; Update max rank
      (when (dag-draw-graph-max-rank graph)
        (setf (dag-draw-graph-max-rank graph)
              (- (dag-draw-graph-max-rank graph) min-rank))))

    graph))

;;; GKNV Rank Balancing Implementation

(defun dag-draw-balance-ranks (graph)
  "Balance rank assignment to improve layout quality.

GRAPH is a `dag-draw-graph' structure with assigned ranks.

Implements GKNV Figure 2-1 step 8: balance() function.
Moves nodes with equal in/out weights to less crowded feasible ranks.
Reduces rank crowding and improves drawing aspect ratio.

Modifies GRAPH in place by updating node ranks.
Returns the modified GRAPH."
  (when (dag-draw-graph-max-rank graph)
    ;; Build rank node counts
    (let ((rank-counts (make-vector (1+ (dag-draw-graph-max-rank graph)) 0)))

      ;; Count nodes per rank
      (ht-each (lambda (_node-id node)
                 (when (dag-draw-node-rank node)
                   (let ((rank (dag-draw-node-rank node)))
                     (aset rank-counts rank (1+ (aref rank-counts rank))))))
               (dag-draw-graph-nodes graph))

      ;; Apply GKNV balancing: process nodes with equal in/out weights
      (ht-each (lambda (node-id node)
                 (when (and (dag-draw-node-rank node)
                           (dag-draw--node-eligible-for-balancing-p graph node-id))
                   (dag-draw--gknv-balance-node graph node-id rank-counts)))
               (dag-draw-graph-nodes graph))))

  graph)

(defun dag-draw--rank-move-valid-p (graph node-id new-rank)
  "Check if moving NODE-ID to NEW-RANK preserves edge constraints.

GRAPH is a `dag-draw-graph' structure.
NODE-ID is a symbol representing the node to check.
NEW-RANK is an integer representing the proposed new rank.

Validates GKNV constraints: λ(predecessor) < λ(node) < λ(successor).

Returns t if move is valid, nil otherwise."
  (let ((valid t))

    ;; Check all incoming edges: predecessor must have strictly lower rank
    ;; GKNV constraint: λ(predecessor) < λ(node)
    (dolist (predecessor (dag-draw-get-predecessors graph node-id))
      (let ((pred-node (dag-draw-get-node graph predecessor)))
        (when (and (dag-draw-node-rank pred-node)
                   (>= (dag-draw-node-rank pred-node) new-rank))
          (setq valid nil))))

    ;; Check all outgoing edges: successor must have strictly higher rank
    ;; GKNV constraint: λ(node) < λ(successor)
    (dolist (successor (dag-draw-get-successors graph node-id))
      (let ((succ-node (dag-draw-get-node graph successor)))
        (when (and (dag-draw-node-rank succ-node)
                   (<= (dag-draw-node-rank succ-node) new-rank))
          (setq valid nil))))

    valid))

(defun dag-draw--node-eligible-for-balancing-p (graph node-id)
  "Check if NODE-ID is eligible for GKNV balancing.

GRAPH is a `dag-draw-graph' structure.
NODE-ID is a symbol representing the node to check.

GKNV criteria: Nodes having equal in- and out-edge weights.
Source and sink nodes (no in/out edges) are not eligible.

Returns t if node is eligible for balancing, nil otherwise."
  (let ((in-weight (dag-draw--calculate-node-in-weight graph node-id))
        (out-weight (dag-draw--calculate-node-out-weight graph node-id)))

    ;; Node is eligible if it has equal in and out weights AND has edges
    ;; (source and sink nodes are not eligible for balancing)
    (and (> in-weight 0)      ; Has incoming edges
         (> out-weight 0)     ; Has outgoing edges
         (= in-weight out-weight))))  ; Equal weights

(defun dag-draw--calculate-node-in-weight (graph node-id)
  "Calculate total weight of incoming edges to NODE-ID.

GRAPH is a `dag-draw-graph' structure.
NODE-ID is a symbol representing the node.

Sums weights of all edges entering NODE-ID.

Returns total weight as a number."
  (let ((total-weight 0))
    (dolist (predecessor (dag-draw-get-predecessors graph node-id))
      (dolist (edge (dag-draw-get-edges-from graph predecessor))
        (when (eq (dag-draw-edge-to-node edge) node-id)
          (setq total-weight (+ total-weight (dag-draw-edge-weight edge))))))
    total-weight))

(defun dag-draw--calculate-node-out-weight (graph node-id)
  "Calculate total weight of outgoing edges from NODE-ID.

GRAPH is a `dag-draw-graph' structure.
NODE-ID is a symbol representing the node.

Sums weights of all edges leaving NODE-ID.

Returns total weight as a number."
  (let ((total-weight 0))
    (dolist (edge (dag-draw-get-edges-from graph node-id))
      (setq total-weight (+ total-weight (dag-draw-edge-weight edge))))
    total-weight))

(defun dag-draw--find-feasible-ranks (graph node-id)
  "Find all feasible ranks for NODE-ID preserving edge constraints.

GRAPH is a `dag-draw-graph' structure.
NODE-ID is a symbol representing the node.

Calculates range of valid ranks based on predecessor and successor ranks.
Ensures GKNV constraint: λ(pred) < λ(node) < λ(succ).

Returns list of integers representing valid ranks for NODE-ID."
  (let ((min-feasible 0)
        (max-feasible (or (dag-draw-graph-max-rank graph) 0))
        (predecessors (dag-draw-get-predecessors graph node-id))
        (successors (dag-draw-get-successors graph node-id)))

    ;; Find minimum feasible rank: max(predecessor_ranks) + 1
    (dolist (pred predecessors)
      (let ((pred-node (dag-draw-get-node graph pred)))
        (when (dag-draw-node-rank pred-node)
          (setq min-feasible (max min-feasible (1+ (dag-draw-node-rank pred-node)))))))

    ;; Find maximum feasible rank: min(successor_ranks) - 1
    (dolist (succ successors)
      (let ((succ-node (dag-draw-get-node graph succ)))
        (when (dag-draw-node-rank succ-node)
          (setq max-feasible (min max-feasible (1- (dag-draw-node-rank succ-node)))))))

    ;; Generate list of feasible ranks
    (let ((feasible-ranks '()))
      (cl-loop for rank from min-feasible to max-feasible do
        (push rank feasible-ranks))
      (nreverse feasible-ranks))))

(defun dag-draw--gknv-balance-node (graph node-id rank-counts)
  "Apply GKNV balancing to a single eligible node.

GRAPH is a `dag-draw-graph' structure.
NODE-ID is a symbol representing the node to balance.
RANK-COUNTS is a vector mapping rank numbers to node counts.

Moves node to the feasible rank with the fewest nodes to reduce crowding.

Modifies GRAPH and RANK-COUNTS in place.
Returns nil."
  (let* ((feasible-ranks (dag-draw--find-feasible-ranks graph node-id))
         (current-rank (dag-draw-node-rank (dag-draw-get-node graph node-id)))
         (best-rank current-rank)
         (min-count (aref rank-counts current-rank)))

    ;; Find feasible rank with fewest nodes
    (dolist (rank feasible-ranks)
      (when (< (aref rank-counts rank) min-count)
        (setq min-count (aref rank-counts rank))
        (setq best-rank rank)))

    ;; Move node if we found a better rank
    (when (not (= best-rank current-rank))
      ;; Update rank counts
      (aset rank-counts current-rank (1- (aref rank-counts current-rank)))
      (aset rank-counts best-rank (1+ (aref rank-counts best-rank)))
      ;; Move the node
      (setf (dag-draw-node-rank (dag-draw-get-node graph node-id)) best-rank))))

(provide 'dag-draw-rank-balancing)

;;; dag-draw-rank-balancing.el ends here
