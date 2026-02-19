;;; dag-draw-aesthetic-principles.el --- GKNV Aesthetic Principles A1-A4 -*- lexical-binding: t -*-

;; Copyright (C) 2024, 2025

;; Author: Claude Code
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Implementation of GKNV aesthetic principles A1-A4 per Section 1.1.
;; Provides validation and measurement functions for:
;; - A1: Expose hierarchical structure
;; - A2: Avoid visual anomalies
;; - A3: Keep edges short
;; - A4: Favor symmetry and balance
;;
;; GKNV Reference: Section 1.1, lines 43-54
;; Ubiquitous Language: Aesthetic Principles - proper A1-A4 integration

;;; Code:

(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)

;; Forward declarations for functions from modules that require us (avoid circular deps)
(declare-function dag-draw--count-total-crossings "dag-draw-pass2-ordering")
(declare-function dag-draw--organize-by-ranks "dag-draw-pass2-ordering")
(declare-function dag-draw-balance-ranks "dag-draw-rank-balancing")

;;; A1: Expose hierarchical structure (Section 1.1, line 43)

(defun dag-draw--validate-hierarchical-structure (graph)
  "Validate A1 compliance: hierarchical structure exposure.

GRAPH is a `dag-draw-graph' structure with assigned node ranks.

Returns a score from 0.0 to 1.0 indicating hierarchical clarity,
where 1.0 means all edges follow hierarchical flow from lower to
higher ranks."
  (let* ((_nodes (dag-draw-get-node-ids graph))
         (total-edges (length (dag-draw-graph-edges graph)))
         (forward-edges 0))

    (if (zerop total-edges)
        1.0  ; Perfect hierarchy with no edges
      ;; Count edges that follow hierarchical flow (rank increase)
      (progn
        (dolist (edge (dag-draw-graph-edges graph))
          (let* ((nodes (dag-draw--edge-nodes graph edge))
                 (from-node (car nodes))
                 (to-node (cdr nodes)))
            (when (and from-node to-node
                       (dag-draw-node-rank from-node)
                       (dag-draw-node-rank to-node)
                       (< (dag-draw-node-rank from-node)
                          (dag-draw-node-rank to-node)))
              (setq forward-edges (1+ forward-edges)))))

        ;; A1 score: proportion of edges following hierarchical flow
        (/ (float forward-edges) total-edges)))))

(defun dag-draw--measure-directional-consistency (graph)
  "Measure A1 directional consistency: edges in same general direction.

GRAPH is a `dag-draw-graph' structure with assigned node ranks.

Returns a score from 0.0 to 1.0 indicating consistency, where 1.0
means all edges flow in the primary direction (top-down)."
  (let* ((edges (dag-draw-graph-edges graph))
         (total-edges (length edges))
         (consistent-edges 0))

    (if (zerop total-edges)
        1.0
      ;; Determine primary direction (top-down in this case)
      (progn
        (dolist (edge edges)
          (let* ((nodes (dag-draw--edge-nodes graph edge))
                 (from-node (car nodes))
                 (to-node (cdr nodes)))
            (when (and from-node to-node
                       (dag-draw-node-rank from-node)
                       (dag-draw-node-rank to-node))
              ;; Edge is consistent if it goes from lower to higher rank (top-down)
              (when (<= (dag-draw-node-rank from-node)
                        (dag-draw-node-rank to-node))
                (setq consistent-edges (1+ consistent-edges))))))

        (/ (float consistent-edges) total-edges)))))

(defun dag-draw--identify-source-sink-prominence (graph)
  "Identify source and sink nodes for A1 hierarchical prominence.

GRAPH is a `dag-draw-graph' structure to analyze.

Returns a hash table with keys:
  - source-count: number of nodes with no incoming edges
  - sink-count: number of nodes with no outgoing edges
  - sources: list of source node IDs
  - sinks: list of sink node IDs"
  (let ((result (ht-create))
        (sources '())
        (sinks '())
        (has-incoming (ht-create))
        (has-outgoing (ht-create)))

    ;; Mark nodes with incoming/outgoing edges
    (dolist (edge (dag-draw-graph-edges graph))
      (ht-set! has-incoming (dag-draw-edge-to-node edge) t)
      (ht-set! has-outgoing (dag-draw-edge-from-node edge) t))

    ;; Identify sources (no incoming) and sinks (no outgoing)
    (dolist (node-id (dag-draw-get-node-ids graph))
      (unless (ht-get has-incoming node-id)
        (push node-id sources))
      (unless (ht-get has-outgoing node-id)
        (push node-id sinks)))

    (ht-set! result 'source-count (length sources))
    (ht-set! result 'sink-count (length sinks))
    (ht-set! result 'sources sources)
    (ht-set! result 'sinks sinks)
    result))

;;; A2: Avoid visual anomalies (Section 1.1, line 47)

(defun dag-draw--count-edge-crossings (graph)
  "Count edge crossings for A2 visual anomaly minimization.

GRAPH is a `dag-draw-graph' structure with ranks and ordering assigned.

Reuses existing crossing detection from Pass 2 ordering.
Returns the total number of edge crossings in the graph."
  (require 'dag-draw-pass2-ordering)

  ;; Use existing crossing count function if nodes have ranks/orders
  (let ((ranks (dag-draw--organize-by-ranks graph)))
    (if ranks
        (dag-draw--count-total-crossings graph ranks)
      ;; Fallback for graphs without ranking
      0)))


(defun dag-draw--analyze-edge-bends (graph)
  "Analyze edge bends for A2 sharp bend avoidance.

GRAPH is a `dag-draw-graph' structure with positioned nodes.

Returns a hash table with bend metrics:
  - max-bend-angle: maximum bend angle found (in degrees)
  - total-bends: count of edges with bends exceeding 30 degrees"
  (let ((result (ht-create))
        (max-bend-angle 0)
        (total-bends 0))

    ;; For each edge, analyze bending characteristics
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((nodes (dag-draw--edge-nodes graph edge))
             (from-node (car nodes))
             (to-node (cdr nodes))
             (bend-angle (dag-draw--calculate-edge-bend graph from-node to-node)))

        (when bend-angle
          (setq max-bend-angle (max max-bend-angle bend-angle))
          (when (> bend-angle 30)  ; Consider angles > 30 degrees as bends
            (setq total-bends (1+ total-bends))))))

    (ht-set! result 'max-bend-angle max-bend-angle)
    (ht-set! result 'total-bends total-bends)
    result))

(defun dag-draw--calculate-edge-bend (_graph from-node to-node)
  "Calculate bend angle for an edge between two nodes.

GRAPH is the `dag-draw-graph' structure (currently unused).
FROM-NODE is a `dag-draw-node' structure representing the source.
TO-NODE is a `dag-draw-node' structure representing the destination.

Returns the absolute deviation from straight vertical in degrees,
or nil if nodes lack coordinate information."
  (when (and from-node to-node
             (dag-draw-node-x-coord from-node) (dag-draw-node-y-coord from-node)
             (dag-draw-node-x-coord to-node) (dag-draw-node-y-coord to-node))

    (let* ((dx (- (dag-draw-node-x-coord to-node) (dag-draw-node-x-coord from-node)))
           (dy (- (dag-draw-node-y-coord to-node) (dag-draw-node-y-coord from-node)))
           (angle (atan dy dx))
           (degrees (* angle (/ 180.0 float-pi))))

      ;; Return absolute deviation from straight vertical (0 degrees)
      (abs degrees))))

;;; A3: Keep edges short (Section 1.1, line 50)

(defun dag-draw--measure-edge-lengths (graph)
  "Measure edge lengths for A3 short edge optimization.

GRAPH is a `dag-draw-graph' structure with positioned nodes.

Returns a hash table with length metrics:
  - average-length: mean Euclidean distance of all edges
  - total-length: sum of all edge lengths
  - edge-count: number of edges measured
  - max-length: longest edge in the graph"
  (let ((result (ht-create))
        (total-length 0)
        (edge-count 0)
        (lengths '()))

    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((nodes (dag-draw--edge-nodes graph edge))
             (from-node (car nodes))
             (to-node (cdr nodes))
             (length (dag-draw--calculate-edge-length from-node to-node)))

        (when length
          (push length lengths)
          (setq total-length (+ total-length length))
          (setq edge-count (1+ edge-count)))))

    (ht-set! result 'average-length (if (zerop edge-count) 0 (/ total-length edge-count)))
    (ht-set! result 'total-length total-length)
    (ht-set! result 'edge-count edge-count)
    (ht-set! result 'max-length (if lengths (apply #'max lengths) 0))
    result))

(defun dag-draw--calculate-edge-length (from-node to-node)
  "Calculate Euclidean distance between two nodes.

FROM-NODE is a `dag-draw-node' structure representing the source.
TO-NODE is a `dag-draw-node' structure representing the destination.

Returns the Euclidean distance between node centers, or nil if
nodes lack coordinate information."
  (when (and from-node to-node
             (dag-draw-node-x-coord from-node) (dag-draw-node-y-coord from-node)
             (dag-draw-node-x-coord to-node) (dag-draw-node-y-coord to-node))

    (let ((dx (- (dag-draw-node-x-coord to-node) (dag-draw-node-x-coord from-node)))
          (dy (- (dag-draw-node-y-coord to-node) (dag-draw-node-y-coord from-node))))
      (sqrt (+ (* dx dx) (* dy dy))))))

(defun dag-draw--measure-node-clustering (graph)
  "Measure node clustering quality for A3 related node proximity.

GRAPH is a `dag-draw-graph' structure with positioned nodes.

Returns a clustering score from 0.0 to 1.0, where 1.0 indicates
all connected nodes are within the proximity threshold (100 units)."
  (let* ((_nodes (dag-draw-get-node-ids graph))
         (total-pairs 0)
         (close-pairs 0))

    ;; For each pair of connected nodes, check if they're close
    (dolist (edge (dag-draw-graph-edges graph))
      (let* ((nodes (dag-draw--edge-nodes graph edge))
             (from-node (car nodes))
             (to-node (cdr nodes))
             (edge-distance (dag-draw--calculate-edge-length from-node to-node)))

        (when edge-distance
          (setq total-pairs (1+ total-pairs))
          ;; Consider "close" if edge-distance is below threshold
          (when (< edge-distance 100)  ; Arbitrary threshold
            (setq close-pairs (1+ close-pairs))))))

    (if (zerop total-pairs) 1.0
      (/ (float close-pairs) total-pairs))))

;;; A4: Favor symmetry and balance (Section 1.1, line 53)

(defun dag-draw--measure-layout-symmetry (graph)
  "Measure layout symmetry for A4 aesthetic balance.

GRAPH is a `dag-draw-graph' structure with positioned nodes.

Returns a symmetry score from 0.0 to 1.0, where 1.0 indicates
perfect mirror symmetry around the layout center."
  (let* ((nodes (dag-draw-get-node-ids graph))
         (x-positions '())
         (symmetry-score 0))

    ;; Collect X positions
    (dolist (node-id nodes)
      (let ((node (dag-draw-get-node graph node-id)))
        (when (and node (dag-draw-node-x-coord node))
          (push (dag-draw-node-x-coord node) x-positions))))

    (when x-positions
      (let* ((min-x (apply #'min x-positions))
             (max-x (apply #'max x-positions))
             (center-x (/ (+ min-x max-x) 2.0))
             (balanced-pairs 0)
             (total-checks 0))

        ;; Check for symmetric node placement around center
        (dolist (pos x-positions)
          (let ((mirror-pos (+ center-x (- center-x pos))))
            (setq total-checks (1+ total-checks))
            ;; Check if there's a node near the mirror position
            (when (cl-some (lambda (other-pos)
                             (< (abs (- other-pos mirror-pos)) 10))  ; 10 unit tolerance
                           x-positions)
              (setq balanced-pairs (1+ balanced-pairs)))))

        (setq symmetry-score (if (zerop total-checks) 1.0
                               (/ (float balanced-pairs) total-checks)))))

    symmetry-score))

(defun dag-draw--measure-layout-balance (graph)
  "Measure layout balance for A4 distributed node arrangement.

GRAPH is a `dag-draw-graph' structure with positioned nodes.

Returns a hash table with balance metrics:
  - horizontal-balance: uniformity of x-coordinate distribution (0.0-1.0)
  - vertical-balance: uniformity of y-coordinate distribution (0.0-1.0)
  - overall-balance: average of horizontal and vertical balance"
  (let ((result (ht-create))
        (x-positions '())
        (y-positions '()))

    ;; Collect node positions
    (dolist (node-id (dag-draw-get-node-ids graph))
      (let ((node (dag-draw-get-node graph node-id)))
        (when node
          (when (dag-draw-node-x-coord node)
            (push (dag-draw-node-x-coord node) x-positions))
          (when (dag-draw-node-y-coord node)
            (push (dag-draw-node-y-coord node) y-positions)))))

    ;; Calculate horizontal balance (standard deviation of X positions)
    (let ((h-balance (if (< (length x-positions) 2) 1.0
                       (dag-draw--calculate-position-balance x-positions)))
          (v-balance (if (< (length y-positions) 2) 1.0
                       (dag-draw--calculate-position-balance y-positions))))

      (ht-set! result 'horizontal-balance h-balance)
      (ht-set! result 'vertical-balance v-balance)
      (ht-set! result 'overall-balance (/ (+ h-balance v-balance) 2.0)))

    result))

(defun dag-draw--calculate-position-balance (positions)
  "Calculate balance score from position distribution.

POSITIONS is a list of numbers representing coordinate values.

Lower standard deviation indicates better balance.
Returns a score from 0.0 to 1.0, where 1.0 indicates perfect
uniform distribution."
  (when (> (length positions) 1)
    (let* ((mean (/ (apply #'+ positions) (length positions)))
           (variance (/ (apply #'+ (mapcar (lambda (pos)
                                             (expt (- pos mean) 2))
                                           positions))
                        (length positions)))
           (std-dev (sqrt variance)))

      ;; Convert std-dev to 0-1 score (lower std-dev = higher score)
      ;; Using arbitrary scaling factor
      (max 0.0 (min 1.0 (- 1.0 (/ std-dev 100.0)))))))

;;; Algorithm Integration Functions

(defun dag-draw--evaluate-ranking-aesthetics (graph)
  "Evaluate aesthetic principles in ranking decisions (Pass 1).

GRAPH is a `dag-draw-graph' structure with assigned ranks.

Focuses on A1 (hierarchy) and A3 (short edges).
Returns a property list with scores:
  - :hierarchical-score (0.0-1.0)
  - :edge-length-score (0.0-1.0)
  - :overall-score weighted combination (A1: 70%, A3: 30%)"
  (let ((a1-score (dag-draw--validate-hierarchical-structure graph))
        (a3-score (ht-get (dag-draw--measure-edge-lengths graph) 'average-length)))

    ;; Return plist with individual scores
    (list :hierarchical-score a1-score
          :edge-length-score (if a3-score (max 0 (min 1 (- 1 (/ a3-score 100)))) 0)
          :overall-score (+ (* 0.7 a1-score)  ; A1 is primary for ranking
                           (* 0.3 (if a3-score (max 0 (min 1 (- 1 (/ a3-score 100)))) 0))))))

(defun dag-draw--evaluate-ordering-aesthetics (graph)
  "Evaluate aesthetic principles in ordering decisions (Pass 2).

GRAPH is a `dag-draw-graph' structure with ranks and ordering assigned.

Focuses on A2 (crossing minimization).
Returns a property list with metrics:
  - :crossing-count total number of crossings
  - :total-edges number of edges in graph
  - :crossing-score quality score (0.0-1.0, higher is better)"
  (let ((crossings (or (dag-draw--count-edge-crossings graph) 0))
        (total-edges (length (dag-draw-graph-edges graph))))

    ;; Return plist with individual scores
    (list :crossing-count crossings
          :total-edges total-edges
          :crossing-score (if (zerop total-edges) 1.0
                           (max 0.0 (- 1.0 (/ crossings (* total-edges total-edges))))))))

(defun dag-draw--evaluate-positioning-aesthetics (graph)
  "Evaluate aesthetic principles in positioning decisions (Pass 3).

GRAPH is a `dag-draw-graph' structure with positioned nodes.

Focuses on A3 (edge length) and A4 (balance).
Returns a property list with metrics:
  - :average-edge-length mean edge length
  - :symmetry-score layout symmetry (0.0-1.0)
  - :edge-length-score A3 quality score (0.0-1.0)
  - :balance-score A4 quality score (0.0-1.0)
  - :overall-score weighted combination (A3: 60%, A4: 40%)"
  (let* ((a3-metrics (dag-draw--measure-edge-lengths graph))
         (a4-metrics (dag-draw--measure-layout-balance graph))
         (avg-edge-length (or (ht-get a3-metrics 'average-length) 0))
         (symmetry-score (or (ht-get a4-metrics 'symmetry-score) 1.0))
         (a3-score (if (> avg-edge-length 0)
                       (max 0 (min 1 (- 1 (/ avg-edge-length 100))))
                     1.0))
         (a4-score (or (ht-get a4-metrics 'overall-balance) 1.0)))

    ;; Return plist with individual scores
    (list :average-edge-length avg-edge-length
          :symmetry-score symmetry-score
          :edge-length-score a3-score
          :balance-score a4-score
          :overall-score (+ (* 0.6 a3-score)  ; A3 is primary for positioning
                           (* 0.4 a4-score)))))

(provide 'dag-draw-aesthetic-principles)

;;; dag-draw-aesthetic-principles.el ends here
