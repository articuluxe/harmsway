;;; dag-draw-cycle-breaking.el --- Cycle detection and breaking for DAG layout -*- lexical-binding: t -*-

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
;; This module implements Pass 1 (Cycle Breaking) of the GKNV graph drawing
;; algorithm as specified in "A Technique for Drawing Directed Graphs"
;; (Gansner, Koutsofios, North, Vo).
;;
;; GKNV Reference: Section 2.1 (lines 367-413)
;; Decision: D1.1 - DFS back-edge reversal for cycle breaking
;; Algorithm: Depth-first search with edge classification
;;
;; Key Requirements:
;; - Respects natural input order (stability)
;; - Uses DFS to classify edges as tree/forward/cross/back
;; - Reverses only back edges to break cycles
;; - Preserves semantic structure (sources stay at top)
;;
;; Baseline Status: ✅ Compliant
;;
;; GKNV Section 2.1 states: "From the standpoint of stability, the depth-first
;; cycle-breaking heuristic seems preferable.  It also makes more informative
;; drawings than would be obtained by collapsing all the nodes in a cycle."
;;
;; See doc/implementation-decisions.md (D1.1) for full decision rationale.
;;
;; Functions:
;; - dag-draw-simple-has-cycles: Detect if graph contains cycles
;; - dag-draw--break-cycles-using-gknv-classification: GKNV DFS edge
;;   classification cycle breaking
;; - dag-draw--simple-has-cycle: Helper for DFS cycle detection

;;; Code:

(require 'ht)
(require 'dag-draw)
(require 'dag-draw-core)

;; Forward declaration for function defined in pass1-ranking
(declare-function dag-draw--count-cycles-through-edge "dag-draw-pass1-ranking")

;;; GKNV DFS Edge Classification (Section 2.1)

(defun dag-draw--classify-edges-gknv (graph)
  "Classify edges using GKNV DFS taxonomy per Section 2.1.

GRAPH is a `dag-draw-graph' structure to analyze.

Performs depth-first search starting from source nodes, classifying edges as:
- tree-edges: edges to unvisited nodes
- forward-edges: edges to descendants already visited
- cross-edges: edges between non-ancestor nodes
- back-edges: edges creating cycles (point to ancestors)

Returns hash table with keys tree-edges, forward-edges, cross-edges, back-edges.
Each key maps to a list of `dag-draw-edge' structures."
  (let ((classification (ht-create))
        (visited (ht-create))
        (discovery-time (ht-create))
        (finish-time (ht-create))
        (time-counter 0)
        (parent (ht-create)))

    ;; Initialize classification categories
    (ht-set! classification 'tree-edges '())
    (ht-set! classification 'forward-edges '())
    (ht-set! classification 'cross-edges '())
    (ht-set! classification 'back-edges '())

    ;; Perform DFS following GKNV Section 2.1: start from source nodes first
    (let* ((source-sink-info (dag-draw--find-source-sink-nodes graph))
           (sources (ht-get source-sink-info 'sources))
           (all-nodes (ht-keys (dag-draw-graph-nodes graph)))
           ;; Sort sources first, then remaining nodes
           (sorted-sources (sort sources (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
           (remaining-nodes (sort (cl-set-difference all-nodes sources)
                                  (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
           (node-order (append sorted-sources remaining-nodes)))

      (dolist (node-id node-order)
        (unless (ht-get visited node-id)
          (setq time-counter (dag-draw--dfs-classify-edges
                              graph node-id visited discovery-time
                              finish-time time-counter parent classification)))))

    classification))

(defun dag-draw--dfs-classify-edges (graph node visited discovery-time finish-time time-counter parent classification)
  "Perform DFS traversal and classify edges per GKNV Section 2.1.

GRAPH is a `dag-draw-graph' structure.
NODE is a symbol representing the current node ID.
VISITED is a hash table tracking node visit states (gray/black).
DISCOVERY-TIME is a hash table mapping node IDs to discovery times.
FINISH-TIME is a hash table mapping node IDs to finish times.
TIME-COUNTER is an integer representing current time
\(incremented during traversal).
PARENT is a hash table mapping nodes to their DFS parent.
CLASSIFICATION is a hash table accumulating edge classifications.

Modifies VISITED, DISCOVERY-TIME, FINISH-TIME, and CLASSIFICATION in place.
Returns updated time counter as an integer."
  (ht-set! visited node t)
  (ht-set! discovery-time node time-counter)
  (setq time-counter (1+ time-counter))

  ;; Visit all adjacent nodes in natural order per GKNV Section 2.1, line 374
  (let ((edges (dag-draw-get-edges-from graph node)))
    ;; Reverse to get natural order (edges are pushed to front of list)
    (setq edges (reverse edges))
    (dolist (edge edges)
      (let ((neighbor (dag-draw-edge-to-node edge)))
        (cond
         ;; Tree edge: neighbor not yet visited
         ((not (ht-get visited neighbor))
          (ht-set! parent neighbor node)
          (ht-set! classification 'tree-edges
                   (cons edge (ht-get classification 'tree-edges)))
          (setq time-counter (dag-draw--dfs-classify-edges
                              graph neighbor visited discovery-time
                              finish-time time-counter parent classification)))

         ;; Back edge: neighbor is ancestor (in recursion stack)
         ((not (ht-get finish-time neighbor))  ; Still being processed
          (ht-set! classification 'back-edges
                   (cons edge (ht-get classification 'back-edges))))

         ;; Forward or Cross edge: neighbor already finished
         (t
          (if (< (ht-get discovery-time node) (ht-get discovery-time neighbor))
              ;; Forward edge: node was discovered before neighbor (ancestor relationship)
              (ht-set! classification 'forward-edges
                       (cons edge (ht-get classification 'forward-edges)))
            ;; Cross edge: neighbor was discovered before node
            (ht-set! classification 'cross-edges
                     (cons edge (ht-get classification 'cross-edges)))))))))

  ;; Mark node as finished
  (ht-set! finish-time node time-counter)
  (1+ time-counter))

(defun dag-draw--break-cycles-using-gknv-classification (graph)
  "Break cycles by reversing back edges per GKNV Section 2.1.

GRAPH is a `dag-draw-graph' structure containing cycles.

Uses DFS edge classification to identify back edges (edges creating cycles).
Reverses each back edge to eliminate cycles while preserving graph structure.

Modifies GRAPH in place by reversing back edges.
Returns the modified GRAPH."
  (let ((classification (dag-draw--classify-edges-gknv graph)))
    (dolist (back-edge (ht-get classification 'back-edges))
      ;; Reverse back edge to break cycle
      (dag-draw--reverse-edge graph back-edge)))
  graph)

(defun dag-draw--reverse-edge (graph edge)
  "Reverse an edge's internal direction while preserving visual direction.

GRAPH is a `dag-draw-graph' structure containing the edge.
EDGE is a `dag-draw-edge' to reverse.

Per GKNV Section 2.1: only internal direction is flipped for algorithmic
purposes.  Original direction preserved in edge attributes for rendering.

Modifies GRAPH in place by removing EDGE and adding reversed edge.
Returns nil."
  (let ((from-node (dag-draw-edge-from-node edge))
        (to-node (dag-draw-edge-to-node edge))
        (weight (dag-draw-edge-weight edge))
        (label (dag-draw-edge-label edge))
        (attributes (dag-draw-edge-attributes edge)))

    ;; Remove original edge
    (setf (dag-draw-graph-edges graph)
          (cl-remove edge (dag-draw-graph-edges graph)))

    ;; Add reversed edge with original direction preserved in attributes
    (let ((new-attrs (ht-copy (or attributes (ht-create)))))
      (ht-set! new-attrs 'original-direction (cons from-node to-node))
      (dag-draw-add-edge graph to-node from-node weight label new-attrs))))

(defun dag-draw--count-cycle-participation (graph)
  "Count how many cycles each edge participates in.

GRAPH is a `dag-draw-graph' structure to analyze.

Implements GKNV Section 2.1 heuristic for cycle-breaking.
Analyzes strongly connected components to count cycle membership.

Returns hash table mapping `dag-draw-edge' structures to integers
representing the number of cycles each edge participates in."
  (let ((participation (ht-create)))
    ;; Initialize all edges with zero participation
    (dolist (edge (dag-draw-graph-edges graph))
      (ht-set! participation edge 0))

    ;; For each strongly connected component, count cycles
    (let ((sccs (dag-draw--find-strongly-connected-components graph)))
      (dolist (scc sccs)
        (when (> (length scc) 1)  ; Non-trivial SCC
          (dag-draw--count-cycles-in-scc graph scc participation))))

    participation))

(defun dag-draw--find-strongly-connected-components (graph)
  "Find strongly connected components in GRAPH.

GRAPH is a `dag-draw-graph' structure to analyze.

Simplified implementation of Tarjan's algorithm for finding SCCs.

Returns list of components, where each component is a list of node IDs."
  ;; Simplified implementation - return list of node lists
  (let ((visited (ht-create))
        (components '()))

    (ht-each (lambda (node-id _node)
               (unless (ht-get visited node-id)
                 (let ((component (dag-draw--dfs-component graph node-id visited)))
                   (when component
                     (push component components)))))
             (dag-draw-graph-nodes graph))
    components))

(defun dag-draw--dfs-component (graph start visited)
  "Find connected component starting from START node using DFS.

GRAPH is a `dag-draw-graph' structure.
START is a symbol representing the starting node ID.
VISITED is a hash table tracking which nodes have been visited.

Modifies VISITED in place.
Returns list of node IDs in the connected component."
  (let ((component '())
        (stack (list start)))
    (while stack
      (let ((node (pop stack)))
        (unless (ht-get visited node)
          (ht-set! visited node t)
          (push node component)
          ;; Add neighbors to stack
          (dolist (edge (dag-draw-get-edges-from graph node))
            (let ((neighbor (dag-draw-edge-to-node edge)))
              (unless (ht-get visited neighbor)
                (push neighbor stack)))))))
    component))

(defun dag-draw--count-cycles-in-scc (graph scc participation)
  "Count cycles within a strongly connected component.

GRAPH is a `dag-draw-graph' structure.
SCC is a list of node IDs forming a strongly connected component.
PARTICIPATION is a hash table mapping edges to cycle participation counts.

For each edge within the SCC, counts how many cycles it participates in.

Modifies PARTICIPATION hash table in place.
Returns nil."
  ;; For each edge in the SCC, perform DFS to count cycles it participates in
  (dolist (edge (dag-draw-graph-edges graph))
    (let ((from (dag-draw-edge-from-node edge))
          (to (dag-draw-edge-to-node edge)))
      (when (and (member from scc) (member to scc))
        ;; Edge is within SCC, count cycles through it
        (let ((cycle-count (dag-draw--count-cycles-through-edge graph edge scc)))
          (ht-set! participation edge cycle-count))))))

(defun dag-draw--find-source-sink-nodes (graph)
  "Find source and sink nodes per GKNV Section 2.1.

GRAPH is a `dag-draw-graph' structure to analyze.

Source nodes: nodes with no incoming edges.
Sink nodes: nodes with no outgoing edges.

Returns hash table with keys `sources' and `sinks', each mapping to
a list of node IDs (symbols)."
  (let ((sources '())
        (sinks '())
        (has-incoming (ht-create))
        (has-outgoing (ht-create)))

    ;; Mark nodes with incoming/outgoing edges
    (dolist (edge (dag-draw-graph-edges graph))
      (ht-set! has-incoming (dag-draw-edge-to-node edge) t)
      (ht-set! has-outgoing (dag-draw-edge-from-node edge) t))

    ;; Identify sources and sinks
    (ht-each (lambda (node-id _node)
               (unless (ht-get has-incoming node-id)
                 (push node-id sources))
               (unless (ht-get has-outgoing node-id)
                 (push node-id sinks)))
             (dag-draw-graph-nodes graph))

    (let ((result (ht-create)))
      (ht-set! result 'sources sources)
      (ht-set! result 'sinks sinks)
      result)))

;;; Simple Cycle Breaking

(defun dag-draw--simple-has-cycle (graph visited rec-stack node)
  "Check if GRAPH has cycle starting from NODE using DFS.

GRAPH is a `dag-draw-graph' structure.
VISITED is a hash table tracking which nodes have been visited.
REC-STACK is a hash table tracking current recursion stack.
NODE is a symbol representing the starting node ID.

Uses recursion stack to detect back edges indicating cycles.

Modifies VISITED and REC-STACK in place.
Returns t if cycle found, nil otherwise."
  (ht-set! visited node t)
  (ht-set! rec-stack node t)

  (let ((has-cycle nil))
    (dolist (successor (dag-draw-get-successors graph node))
      (cond
       ;; Not visited yet - recurse
       ((not (ht-get visited successor))
        (when (dag-draw--simple-has-cycle graph visited rec-stack successor)
          (setq has-cycle t)))
       ;; In recursion stack - cycle found
       ((ht-get rec-stack successor)
        (setq has-cycle t))))

    (ht-set! rec-stack node nil)
    has-cycle))

(defun dag-draw-simple-has-cycles (graph)
  "Detect cycles in GRAPH using simple DFS approach.

GRAPH is a `dag-draw-graph' structure to check.

Performs depth-first search from each unvisited node, tracking recursion
stack to identify back edges that indicate cycles.

Returns t if any cycles found, nil otherwise."
  (let ((visited (ht-create))
        (rec-stack (ht-create))
        (has-cycle nil))

    ;; Initialize tracking tables
    (dolist (node-id (dag-draw-get-node-ids graph))
      (ht-set! visited node-id nil)
      (ht-set! rec-stack node-id nil))

    ;; Check from each unvisited node
    (dolist (node-id (dag-draw-get-node-ids graph))
      (when (and (not (ht-get visited node-id))
                 (not has-cycle))
        (setq has-cycle (dag-draw--simple-has-cycle graph visited rec-stack node-id))))

    has-cycle))


(provide 'dag-draw-cycle-breaking)

;;; dag-draw-cycle-breaking.el ends here
