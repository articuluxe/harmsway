;;; dag-draw.el --- Draw directed graphs using the GKNV algorithm -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Trevoke
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (dash "2.19.1") (ht "2.3"))
;; Keywords: tools, extensions
;; URL: https://codeberg.org/trevoke/dag-draw.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; dag-draw.el: Draw directed graphs that don't suck.
;;
;; You focus on the structure (what connects to what), and dag-draw handles
;; the layout (where everything goes).  No more fiddling with positions or
;; untangling crossed arrows.
;;
;; Quick Start:
;;
;;   (require 'dag-draw)
;;
;;   ;; Create a graph
;;   (setq my-graph (dag-draw-create-graph))
;;
;;   ;; Add nodes
;;   (dag-draw-add-node my-graph 'start "Start Here")
;;   (dag-draw-add-node my-graph 'middle "Do Work")
;;   (dag-draw-add-node my-graph 'done "Finish")
;;
;;   ;; Connect them
;;   (dag-draw-add-edge my-graph 'start 'middle)
;;   (dag-draw-add-edge my-graph 'middle 'done)
;;
;;   ;; Layout and render
;;   (dag-draw-layout-graph my-graph)
;;   (dag-draw-render-graph my-graph 'ascii)  ; or 'svg
;;
;; Or create from a data structure in one call:
;;
;;   (dag-draw-create-from-spec
;;     :nodes '((a :label "Task A") (b :label "Task B"))
;;     :edges '((a b)))
;;
;; Output formats: ASCII (box-drawing characters) and SVG (scalable graphics).
;; Both support node highlighting for "you are here" visual emphasis.
;;
;; See the README for full documentation, tutorials, and examples.
;;
;; Under the hood, this implements the GKNV algorithm from "A Technique for
;; Drawing Directed Graphs" (Gansner et al., 1993) - the same algorithm that
;; powers Graphviz.  See individual module files for technical details.

;;; Code:

(require 'cl-lib)

(require 'dash)
(require 'ht)

;; Forward declarations for functions in modules loaded later
(declare-function dag-draw--world-to-grid-size "dag-draw-ascii-grid")
(declare-function dag-draw--debug-spacing-calculation "dag-draw-quality")
(declare-function dag-draw--calculate-max-required-rank-separation "dag-draw-quality")

;;; Customization

(defgroup dag-draw nil
  "Draw directed graphs using the GKNV algorithm."
  :group 'graphics
  :prefix "dag-draw-")

(defcustom dag-draw-default-node-separation 20
  "Default minimum horizontal separation between nodes.
Optimized for compact, readable layouts following GKNV algorithm principles."
  :type 'integer
  :group 'dag-draw)

(defcustom dag-draw-default-rank-separation 25
  "Default minimum vertical separation between ranks.
Optimized for compact, readable layouts following GKNV algorithm principles."
  :type 'integer
  :group 'dag-draw)

(defcustom dag-draw-ascii-node-separation 6
  "Horizontal spacing between nodes in ASCII mode (characters).
Used when coordinate-mode is `ascii' for compact terminal output.
This provides readable spacing for 80-column terminals while
allowing edge routing."
  :type 'integer
  :group 'dag-draw)

(defcustom dag-draw-ascii-rank-separation 5
  "Vertical spacing between ranks in ASCII mode (rows).
Used when coordinate-mode is `ascii' for compact terminal output.
This provides clean vertical separation with enough space for
edge routing."
  :type 'integer
  :group 'dag-draw)

(defcustom dag-draw-ascii-coordinate-scale 0.15
  "Scale factor for converting GKNV algorithm coordinates to ASCII grid positions.

ASCII CHARACTER CONSTRAINTS:
- GKNV paper suggests '72 units per inch' for high-resolution graphics
- ASCII terminals have ~5 characters per inch in typical monospace fonts
- Scale factor needed: 72 ÷ 5 = ~14.4x compression from GKNV to ASCII
- Our 0.15 scale provides balanced compression avoiding coordinate collapse

VISUAL RESULT: Prevents negative coordinates while maintaining readable layout."
  :type 'float
  :group 'dag-draw)

(defcustom dag-draw-default-output-format 'svg
  "Default output format for rendered graphs."
  :type '(choice (const :tag "SVG" svg)
                 (const :tag "ASCII" ascii)
                 (const :tag "DOT" dot))
  :group 'dag-draw)

(defcustom dag-draw-debug-output nil
  "When non-nil, print debug messages during graph layout and rendering.
Useful for troubleshooting layout issues or understanding algorithm behavior."
  :type 'boolean
  :group 'dag-draw)

;;; Core Data Structures

(cl-defstruct (dag-draw-node
               (:constructor dag-draw-node-create)
               (:copier nil))
  "A node in a directed graph."
  id                    ; Unique identifier
  label                 ; Display label
  (x-size 80)          ; Width of node bounding box
  (y-size 40)          ; Height of node bounding box
  (x-coord nil)        ; X coordinate (set during layout)
  (y-coord nil)        ; Y coordinate (set during layout)
  (rank nil)           ; Assigned rank (set during rank assignment)
  (order nil)          ; Order within rank (set during ordering)
  (virtual-p nil)      ; Whether this is a virtual node (for long edge breaking)
  attributes)          ; Additional attributes hash table

(cl-defstruct (dag-draw-edge
               (:constructor dag-draw-edge-create)
               (:copier nil))
  "An edge in a directed graph."
  from-node            ; Source node ID
  to-node              ; Target node ID
  (weight 1)           ; Edge weight for optimization
  (min-length 1)       ; Minimum length constraint
  label                ; Optional edge label
  (spline-points nil)  ; Spline control points (set during spline generation)
  (label-position nil) ; Position for edge label
  attributes)          ; Additional attributes hash table

(cl-defstruct (dag-draw-graph
               (:constructor dag-draw-graph-create)
               (:copier nil))
  "A directed graph."
  (nodes (ht-create 'equal))           ; Hash table: id -> node
  (edges '())                          ; List of edges
  (node-separation dag-draw-default-node-separation)
  (rank-separation dag-draw-default-rank-separation)
  (max-rank nil)                       ; Maximum assigned rank
  (rank-sets nil)                      ; User-specified rank constraints
  (adjusted-positions nil)             ; Hash table: id -> (x y width height) adjusted coordinates
  (coordinate-mode 'ascii)             ; Coordinate system mode ('ascii is primary, 'high-res deprecated)
  attributes)                          ; Graph-level attributes

;;; Geometry Data Structures (used across multiple modules)

(cl-defstruct (dag-draw-point
               (:constructor dag-draw-point-create)
               (:copier nil))
  "A 2D point."
  x y)

(cl-defstruct (dag-draw-bezier-curve
               (:constructor dag-draw-bezier-curve-create)
               (:copier nil))
  "A cubic Bézier curve with 4 control points."
  p0 p1 p2 p3)

(cl-defstruct (dag-draw-box
               (:constructor dag-draw-box-create)
               (:copier nil))
  "A rectangular region."
  x-min y-min x-max y-max)

;;; Graph Count Functions (moved here to avoid circular deps)

(defun dag-draw-node-count (graph)
  "Get the number of nodes in GRAPH.

GRAPH is a `dag-draw-graph' structure.

Returns an integer count of nodes."
  (ht-size (dag-draw-graph-nodes graph)))

(defun dag-draw-edge-count (graph)
  "Get the number of edges in GRAPH.

GRAPH is a `dag-draw-graph' structure.

Returns an integer count of edges."
  (length (dag-draw-graph-edges graph)))

;; Provide early so submodules can require us during their load
;; This is the standard Emacs pattern for multi-file packages
(provide 'dag-draw)

;;; Public API

;;;###autoload
(defun dag-draw-create-graph (&optional attributes)
  "Create a new empty directed graph.

ATTRIBUTES is an optional hash table of graph-level attributes.

Returns a new `dag-draw-graph' structure ready for adding nodes
and edges."
  (dag-draw-graph-create :attributes (or attributes (ht-create))))

;;;###autoload
(defun dag-draw-add-node (graph node-id &optional label attributes)
  "Add a node with NODE-ID to GRAPH.

GRAPH is a `dag-draw-graph' structure to modify.
NODE-ID is a unique identifier (typically a symbol).
LABEL is an optional string for display (defaults to NODE-ID's name).
ATTRIBUTES is an optional hash table of node-specific attributes.

The node is auto-sized based on label length using text wrapping
constraints (up to 2 rows of 25 characters each).

Returns the created `dag-draw-node' structure."
  (let* ((node-label (or label (symbol-name node-id)))
         ;; Use the constrained text formatting (2 rows × 20 chars max)
         (text-lines (dag-draw--format-node-text-with-constraints node-label))
         ;; Calculate variable node size based on actual formatted text
         (size-info (dag-draw--calculate-constrained-node-size text-lines))
         (node-width (car size-info))
         (node-height (cdr size-info))
         ;; Create the node with formatted text as label (joined by newlines)
         (formatted-label (mapconcat #'identity text-lines "\n"))
         (node (dag-draw-node-create
                :id node-id
                :label formatted-label
                :x-size node-width
                :y-size node-height
                :attributes (or attributes (ht-create)))))
    (ht-set! (dag-draw-graph-nodes graph) node-id node)
    node))

;;;###autoload
(defun dag-draw-add-edge (graph from-node to-node &optional weight label attributes)
  "Add an edge from FROM-NODE to TO-NODE in GRAPH.

GRAPH is a `dag-draw-graph' structure to modify.
FROM-NODE is the node ID of the edge source.
TO-NODE is the node ID of the edge target.
WEIGHT is an optional edge weight for optimization (defaults to 1).
LABEL is an optional string label for the edge.
ATTRIBUTES is an optional hash table of edge-specific attributes.

The min-length constraint can be specified via ATTRIBUTES with key
`min-length' (defaults to 1).

Returns the created `dag-draw-edge' structure."
  (let* ((attrs (or attributes (ht-create)))
         (min-length (or (and attrs (ht-get attrs 'min-length)) 1))
         (edge (dag-draw-edge-create
                :from-node from-node
                :to-node to-node
                :weight (or weight 1)
                :min-length min-length
                :label label
                :attributes attrs)))
    (push edge (dag-draw-graph-edges graph))
    edge))

;;; Edge Label Processing (GKNV Section 5.3)

(defun dag-draw--process-edge-labels (graph)
  "Process edge labels per GKNV Section 5.3.

GRAPH is a `dag-draw-graph' structure that may contain labeled edges.

Creates virtual nodes for labels and adjusts edge lengths before
ranking.  This should be called before Pass 1 (ranking)."
  (when (dag-draw--graph-has-edge-labels-p graph)
    ;; GKNV Section 5.3: Create virtual nodes for edge labels
    (dag-draw--create-edge-label-virtual-nodes graph)

    ;; GKNV Section 5.3: Set minimum edge length to 2 for labeled edges
    (dag-draw--apply-label-edge-length-compensation graph)))

(defun dag-draw--graph-has-edge-labels-p (graph)
  "Check if any edges in GRAPH have labels.

GRAPH is a `dag-draw-graph' structure.

Returns non-nil if at least one edge has a label."
  (cl-some (lambda (edge) (dag-draw-edge-label edge))
           (dag-draw-graph-edges graph)))

;;;###autoload
(defun dag-draw-layout-graph (graph &rest args)
  "Apply the GKNV layout algorithm to GRAPH.

GRAPH is a `dag-draw-graph' structure to layout.

This performs the four GKNV passes:
1. Ranking - assigns vertical levels to nodes
2. Ordering - arranges nodes within ranks to minimize crossings
3. Positioning - assigns x,y coordinates to nodes
4. Spline generation - creates edge routing paths

Optional keyword arguments (passed as ARGS):
  :coordinate-mode MODE - Sets coordinate system mode
                         `ascii': ASCII-native coordinates (default)
                         `high-res': Legacy high-resolution mode

Returns the GRAPH with layout information assigned to nodes and edges."
  ;; Parse keyword arguments
  (let ((coordinate-mode 'ascii))     ; Default to ASCII-first mode
    (while args
      (let ((key (pop args))
            (value (pop args)))
        (cond
         ((eq key :coordinate-mode)
          (setq coordinate-mode value))
         (t
          (error "Unknown keyword argument: %s" key)))))

    ;; Store coordinate mode in graph for use by GKNV passes
    (setf (dag-draw-graph-coordinate-mode graph) coordinate-mode)

    ;; Apply ASCII-specific separations for compact terminal output
    ;; Only apply if user hasn't explicitly customized separations
    (when (eq coordinate-mode 'ascii)
      (when (= (dag-draw-graph-node-separation graph) dag-draw-default-node-separation)
        (setf (dag-draw-graph-node-separation graph) dag-draw-ascii-node-separation))
      (when (= (dag-draw-graph-rank-separation graph) dag-draw-default-rank-separation)
        (setf (dag-draw-graph-rank-separation graph) dag-draw-ascii-rank-separation))

      ;; Scale node sizes down to ASCII scale
      ;; Node sizes are calculated in "world" coordinates (60-170), but ASCII mode
      ;; needs them in character-grid scale (roughly 10-30 characters)
      (let ((scale dag-draw-ascii-coordinate-scale))
        (ht-each (lambda (_node-id node)
                   (let ((world-xsize (dag-draw-node-x-size node))
                         (world-ysize (dag-draw-node-y-size node)))
                     ;; Scale down to ASCII units
                     (setf (dag-draw-node-x-size node) (* world-xsize scale))
                     (setf (dag-draw-node-y-size node) (* world-ysize scale))))
                 (dag-draw-graph-nodes graph))))

    ;; GKNV Edge Label Processing (before Pass 1 per Section 5.3)
    (dag-draw--process-edge-labels graph)

    ;; GKNV Pass 1: Rank assignment
    (dag-draw-rank-graph graph)

    ;; ASCII resolution preprocessing (only for legacy high-res mode)
    ;; ASCII-first mode uses native ASCII spacing throughout - no preprocessing needed
    (unless (eq coordinate-mode 'ascii)
      (dag-draw--ensure-ascii-resolution graph))

    ;; GKNV Passes 2-4: ordering, positioning, and spline generation
    (dag-draw-order-vertices graph)
    (dag-draw-position-nodes graph)
    ;; GKNV Pass 4: Edge Drawing (Section 5) - generate splines for all coordinate modes
    (dag-draw-generate-splines graph)
    graph))

;;; ASCII Resolution Preprocessing

(defun dag-draw--estimate-ascii-scale (graph)
  "Estimate the scale factor for ASCII coordinate conversion.

GRAPH is a `dag-draw-graph' structure to analyze.

Returns a float representing the scale factor used for converting
world coordinates to ASCII grid positions.  The scale is dynamically
adjusted based on graph complexity."
  ;; GKNV AESTHETIC A3 "Keep edges short" with clarity maintenance
  ;; Dynamic scaling based on graph size and complexity for optimal ASCII output
  (if (and graph (> (dag-draw-node-count graph) 2))
      ;; Complex graphs need larger scale for visual clarity
      ;; Base scale * complexity factor, capped at reasonable limits
      (let* ((node-count (dag-draw-node-count graph))
             (_edge-count (dag-draw-edge-count graph))
             (complexity-factor (min 4.0 (+ 1.0 (* 0.5 (- node-count 2)))))
             (dynamic-scale (* dag-draw-ascii-coordinate-scale complexity-factor)))
        (min 0.4 (max 0.15 dynamic-scale)))
    ;; Simple graphs use base scale
    dag-draw-ascii-coordinate-scale))


(defun dag-draw--calculate-min-ascii-routing-space (&optional graph)
  "Calculate minimum ASCII characters needed for clean edge routing.

GRAPH is an optional `dag-draw-graph' structure for dynamic analysis.

If GRAPH is provided, uses dynamic analysis of edge patterns for
optimal spacing.  Otherwise uses safe defaults.

Returns a property list with:
  :min-horizontal - minimum character spacing between nodes
  :min-vertical - minimum row spacing between ranks
  :port-offset - space needed for port positioning"
  (let* ((min-vertical (if graph
                           ;; Dynamic calculation based on graph structure
                           (progn
                             (require 'dag-draw-quality)
                             (let ((dynamic-spacing (dag-draw--calculate-max-required-rank-separation graph)))
                               (when dag-draw-debug-output
                                 (message "DYNAMIC-SPACING: Calculated %d rows for graph (nodes: %d, edges: %d)"
                                          dynamic-spacing (dag-draw-node-count graph) (dag-draw-edge-count graph)))
                               (dag-draw--debug-spacing-calculation graph)
                               dynamic-spacing))
                         ;; Safe default for edge routing
                         2))
         ;; Calculate minimum horizontal separation based on actual node sizes
         (min-horizontal (if graph
                             ;; Find the maximum ASCII node width in the graph and add buffer
                             (let ((scale (dag-draw--estimate-ascii-scale graph))
                                   (max-ascii-width 0))
                               ;; Ensure ascii-grid module is loaded
                               (require 'dag-draw-ascii-grid)
                               (ht-each (lambda (_node-id node)
                                          (let ((ascii-width (dag-draw--world-to-grid-size
                                                              (dag-draw-node-x-size node) scale)))
                                            (setq max-ascii-width (max max-ascii-width ascii-width))))
                                        (dag-draw-graph-nodes graph))
                               ;; Use max node width plus minimum buffer for routing
                               (+ max-ascii-width 6))
                           ;; Safe default if no graph provided
                           12)))
    (list
     :min-horizontal min-horizontal  ; Dynamic horizontal spacing based on node sizes
     :min-vertical min-vertical      ; Dynamic rows between ranks based on edge analysis
     :port-offset 2)))               ; Space needed for port positioning variety

(defun dag-draw--adjust-separations-for-ascii (graph)
  "Adjust node and rank separations for ASCII resolution.

GRAPH is a `dag-draw-graph' structure to modify.

Called BEFORE GKNV passes to ensure clean edge routing space.
Increases separations if needed based on estimated ASCII scale."
  (let* ((scale (dag-draw--estimate-ascii-scale graph))
         (requirements (dag-draw--calculate-min-ascii-routing-space graph))
         (min-world-nodesep (/ (float (plist-get requirements :min-horizontal)) scale))
         (min-world-ranksep (/ (float (plist-get requirements :min-vertical)) scale)))

    ;; Increase separations if needed for ASCII resolution
    (when (< (dag-draw-graph-node-separation graph) min-world-nodesep)
      (when dag-draw-debug-output
        (message "ASCII-RESOLUTION: Increasing nodesep from %.1f to %.1f for scale %.3f"
                 (dag-draw-graph-node-separation graph) min-world-nodesep scale))
      (setf (dag-draw-graph-node-separation graph) min-world-nodesep))

    (when (< (dag-draw-graph-rank-separation graph) min-world-ranksep)
      (when dag-draw-debug-output
        (message "ASCII-RESOLUTION: Increasing ranksep from %.1f to %.1f for scale %.3f"
                 (dag-draw-graph-rank-separation graph) min-world-ranksep scale))
      (setf (dag-draw-graph-rank-separation graph) min-world-ranksep))))

(defun dag-draw--ensure-ascii-resolution (graph)
  "Ensure ASCII grid has sufficient resolution for edge routing.

GRAPH is a `dag-draw-graph' structure to prepare.

Must be called BEFORE the GKNV layout passes.  Adjusts graph
parameters to ensure clean ASCII rendering."
  (dag-draw--adjust-separations-for-ascii graph)
  (when dag-draw-debug-output
    (message "ASCII-RESOLUTION: Graph prepared for scale %.3f"
             (dag-draw--estimate-ascii-scale graph))))

;;;###autoload
(defun dag-draw-render-graph (graph &optional format selected)
  "Render GRAPH in the specified output format.

GRAPH is a `dag-draw-graph' structure that has been laid out.

FORMAT is an optional symbol specifying output format:
  `svg' - Scalable Vector Graphics
  `ascii' - ASCII art text representation
  `dot' - Graphviz DOT language
  Defaults to `dag-draw-default-output-format'.

SELECTED is an optional node ID (symbol) to render with selection highlighting.
Selected nodes are visually emphasized in the output:
  - ASCII: Double-line box characters (╔═╗╚╝║)
  - SVG: Glow filter effect
  - DOT: style=bold attribute

Returns a string representation of the rendered graph."
  (let ((output-format (or format dag-draw-default-output-format)))
    (cond
     ((eq output-format 'svg)
      (dag-draw-render-svg graph selected))
     ((eq output-format 'ascii)
      (dag-draw-render-ascii graph selected))
     ((eq output-format 'dot)
      (dag-draw-render-dot graph selected))
     (t (error "Unsupported output format: %s" output-format)))))

;;; Forward declarations for functions defined in submodules
;; These are needed for defalias and to silence byte-compiler warnings

(declare-function dag-draw-rank-graph "dag-draw-pass1-ranking")
(declare-function dag-draw-assign-ranks "dag-draw-pass1-ranking")
(declare-function dag-draw-order-vertices "dag-draw-pass2-ordering")
(declare-function dag-draw-position-nodes "dag-draw-pass3-positioning")
(declare-function dag-draw--calculate-separation "dag-draw-pass3-positioning")
(declare-function dag-draw-generate-splines "dag-draw-pass4-splines")
(declare-function dag-draw--create-inter-rank-spline "dag-draw-pass4-splines")
(declare-function dag-draw-render-svg "dag-draw-svg")
(declare-function dag-draw-render-ascii "dag-draw-render")
(declare-function dag-draw-render-dot "dag-draw-dot")

;;; Text Processing Utilities

(defun dag-draw--format-node-text-with-constraints (text)
  "Format TEXT for node display with wrapping constraints.

TEXT is a string to format for display in a node.

Wraps text to fit within node display constraints (up to 2 rows
of 25 display columns each).  Supports word wrapping and truncation
for overly long text.  Uses `string-width' for proper Unicode/CJK support.

Returns a list of strings representing the formatted rows."
  (let ((max-cols-per-line 25))  ; Maximum display columns per line
    (if (<= (string-width text) max-cols-per-line)
        (list text)
      ;; Check if text has no spaces (single long word)
      (if (not (string-match " " text))
          ;; Handle single long word - truncate with ellipsis only if extremely long
          (if (> (string-width text) 30)
              (list (concat (truncate-string-to-width text 27) "..."))
            (list text))  ; Allow single words up to 30 display columns
        ;; Word wrapping for text longer than max-cols-per-line
        (let ((words (split-string text " "))
              (line1 "")
              (line2 ""))
          ;; Fill first line up to max-cols-per-line
          (while (and words (<= (+ (string-width line1) (string-width (car words)) (if (string-empty-p line1) 0 1)) max-cols-per-line))
            (setq line1 (if (string-empty-p line1)
                            (car words)
                          (concat line1 " " (car words))))
            (setq words (cdr words)))
          ;; Fill second line up to max-cols-per-line
          (when words
            (let ((remaining-text (mapconcat #'identity words " ")))
              (if (<= (string-width remaining-text) max-cols-per-line)
                  ;; All remaining text fits in second line
                  (setq line2 remaining-text)
                ;; Need to truncate with ellipsis only if extremely long
                (while (and words (<= (+ (string-width line2) (string-width (car words)) (if (string-empty-p line2) 0 1)) (- max-cols-per-line 3)))
                  (setq line2 (if (string-empty-p line2)
                                  (car words)
                                (concat line2 " " (car words))))
                  (setq words (cdr words)))
                (when words  ; Only add ellipsis if there are still remaining words
                  (setq line2 (concat line2 "..."))))))
          (if (string-empty-p line2)
              (list line1)
            (list line1 line2)))))))

(defun dag-draw--calculate-constrained-node-size (text-lines)
  "Calculate appropriate node size for formatted text lines.

TEXT-LINES is a list of strings representing the node's text content.

Uses ASCII-first approach: measures actual ASCII grid requirements,
then converts to world coordinates per GKNV Section 1.2.

Returns a cons cell (WIDTH . HEIGHT) in world coordinate units,
where dimensions fit the actual text content with appropriate padding."
  (let* ((max-line-length (apply #'max (mapcar #'string-width text-lines)))
         (num-lines (length text-lines))
         ;; ASCII-FIRST CALCULATION: Direct measurement of actual requirements
         (ascii-width-needed (+ max-line-length 4))    ; Text + left/right borders + padding
         (ascii-height-needed (+ num-lines 2))         ; Text lines + top/bottom borders
         ;; MATHEMATICAL UNIFICATION: Use unified coordinate scale for conversion
         (ascii-box-scale (if (boundp 'dag-draw-ascii-coordinate-scale)
                              dag-draw-ascii-coordinate-scale
                            0.15))  ; Use unified scale for mathematical consistency
         ;; Convert ASCII requirements to world coordinates
         (calculated-width (/ ascii-width-needed ascii-box-scale))
         (calculated-height (/ ascii-height-needed ascii-box-scale))
         ;; GKNV-COMPLIANT SIZE CONSTRAINTS: Variable sizing with practical limits
         (min-width 60)     ; Minimum for very short text (prevents unusably tiny nodes)
         (max-width 170)    ; Maximum proven to work well (prevents layout issues)
         (node-width (max min-width (min max-width calculated-width)))
         (node-height (max 14 calculated-height)))  ; Minimum reasonable height

    (cons node-width node-height)))

(defun dag-draw--smart-wrap-text (text max-width)
  "Wrap TEXT intelligently to fit within MAX-WIDTH.

TEXT is a string to wrap.
MAX-WIDTH is the maximum number of characters per line (integer).

Breaks at whitespace nearest to the middle of the text for
balanced line lengths.

Returns a list of wrapped lines (strings)."
  (if (<= (length text) max-width)
      (list text)  ; No wrapping needed

    ;; Find the best place to break the text
    (let* ((target-pos (/ (length text) 2))  ; Ideal break position (middle)
           (best-pos nil)
           (best-distance most-positive-fixnum))

      ;; Find whitespace closest to the middle
      (dotimes (i (length text))
        (when (= (aref text i) ?\s)  ; Found a space
          (let ((distance (abs (- i target-pos))))
            (when (< distance best-distance)
              (setq best-distance distance)
              (setq best-pos i)))))

      (if best-pos
          ;; Split at the best whitespace position
          (let ((line1 (substring text 0 best-pos))
                (line2 (substring text (1+ best-pos))))  ; Skip the space
            (cons line1 (dag-draw--smart-wrap-text line2 max-width)))
        ;; No good break point found, force break at max-width
        (list (substring text 0 max-width)
              (substring text max-width))))))


;;; Graph utility functions

(defun dag-draw-get-graph-bounds (graph)
  "Get bounding box of the positioned graph.

GRAPH is a `dag-draw-graph' structure with positioned nodes.

Returns a list (MIN-X MIN-Y MAX-X MAX-Y) representing the
bounding rectangle that contains all nodes."
  (if (= (ht-size (dag-draw-graph-nodes graph)) 0)
      ;; Empty graph - return default bounds
      (list 0 0 100 100)
    (let ((min-x most-positive-fixnum)
          (min-y most-positive-fixnum)
          (max-x most-negative-fixnum)
          (max-y most-negative-fixnum))

      (ht-each (lambda (_node-id node)
                 (let* ((x (or (dag-draw-node-x-coord node) 0))
                        (y (or (dag-draw-node-y-coord node) 0))
                        (width (dag-draw-node-x-size node))
                        (height (dag-draw-node-y-size node))
                        (left (- x (/ width 2.0)))
                        (right (+ x (/ width 2.0)))
                        (top (- y (/ height 2.0)))
                        (bottom (+ y (/ height 2.0))))


                   (setq min-x (min min-x left))
                   (setq max-x (max max-x right))
                   (setq min-y (min min-y top))
                   (setq max-y (max max-y bottom))))
               (dag-draw-graph-nodes graph))

      (list min-x min-y max-x max-y))))

;;; GKNV Mathematical Notation Aliases
;; Section references from "A Technique for Drawing Directed Graphs"

(defalias 'dag-draw-edge-δ 'dag-draw-edge-min-length
  "GKNV δ(e) - minimum edge length constraint (Section 2, line 356).
Alias for dag-draw-edge-min-length using proper Greek mathematical notation.")

(defalias 'dag-draw-edge-ω 'dag-draw-edge-weight
  "GKNV ω(e) - edge weight for optimization (Section 1.2, line 83).
Alias for dag-draw-edge-weight using proper Greek mathematical notation.")

(defalias 'dag-draw-node-λ 'dag-draw-node-rank
  "GKNV λ(v) - rank assignment function (Section 2, line 352).
Alias for dag-draw-node-rank using proper Greek mathematical notation.")

;; GKNV mathematical function aliases (using declare-functions from above)
(defalias 'dag-draw-ρ 'dag-draw--calculate-separation
  "GKNV ρ(u,v) - minimum separation function between adjacent nodes.
ρ(u,v) = (xsize(u) + xsize(v))/2 + nodesep(G) per Section 4.")

;;; Standard GKNV Function Names from Figure 1-1
;; These are the canonical entry points referenced in the GKNV paper

(defalias 'dag-draw-rank 'dag-draw-rank-graph
  "GKNV rank(G) - main entry point for Pass 1 rank assignment (Figure 1-1).")

(defalias 'dag-draw-ordering 'dag-draw-order-vertices
  "GKNV ordering(G) - main entry point for Pass 2 ordering (Figure 1-1).")

(defalias 'dag-draw-position 'dag-draw-position-nodes
  "GKNV position(G) - Pass 3 coordinate assignment (Figure 1-1).")

(defalias 'dag-draw-make-splines 'dag-draw-generate-splines
  "GKNV make_splines(G) - Pass 4 spline generation (Figure 1-1).")

;; Additional canonical functions from Figure 2-2
(defalias 'dag-draw-init-rank 'dag-draw-assign-ranks
  "GKNV init_rank() - initial rank assignment from Figure 2-2.")

;; Section 5.2 spline generation
(defalias 'dag-draw-generate-spline 'dag-draw--create-inter-rank-spline
  "GKNV generate_spline() - individual spline generation from Section 5.2.")

;;; Load all submodules to ensure (require 'dag-draw) loads the entire library
;; Emacs handles recursive requires gracefully - when submodules require 'dag-draw,
;; the symbols defined above are already available since this file is in-progress.

(require 'dag-draw-core)
(require 'dag-draw-coord-transform)
(require 'dag-draw-quality)
(require 'dag-draw-topological)
(require 'dag-draw-cycle-breaking)
(require 'dag-draw-pass1-ranking)
(require 'dag-draw-rank-balancing)
(require 'dag-draw-pass2-ordering)
(require 'dag-draw-pass3-positioning)
(require 'dag-draw-ascii-junctions)
(require 'dag-draw-ascii-grid)
(require 'dag-draw-ports)
(require 'dag-draw-pass4-splines)
(require 'dag-draw-ascii-splines)
(require 'dag-draw-ascii-nodes)
(require 'dag-draw-ascii-edges)
(require 'dag-draw-render-gknv-compliant)
(require 'dag-draw-render)
(require 'dag-draw-svg)
(require 'dag-draw-dot)
(require 'dag-draw-algorithms)
(require 'dag-draw-aesthetic-principles)

;;; dag-draw.el ends here
