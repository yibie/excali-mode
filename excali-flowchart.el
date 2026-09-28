;;; excali-flowchart.el --- Flowchart creation and navigation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Port of upstream element/src/flowchart.ts and its App.tsx wiring
;; (docs/excalidraw-spec.md, createFlowchart and navigateFlowchart).
;;
;; With one flowchart node (rectangle, diamond, ellipse, sticky note)
;; selected, Mod+Arrow adds a copy of it one gap away in that direction,
;; joined by an elbow arrow.  Pressing it again in the same direction
;; grows a row of siblings; another direction starts over there.  The new
;; nodes are pending until Mod is released, when they join the history
;; and the first one is selected.
;;
;; Emacs sees no key releases, so "releasing Mod" is the first command
;; that is not a flowchart creation: it commits the pending nodes before
;; running.  Pending nodes live in the scene (so they render and route
;; like any element) but stay out of the history until then.
;;
;; Alt+Arrow walks the nodes linked to the selected one in that
;; direction; pressing it again cycles through them.  The walk ends with
;; the first other command, as upstream's ends when Alt is released.

;;; Code:

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-binding)
(require 'excali-elbow)
(require 'excali-frame)
(require 'excali-history)

(declare-function excali--zoom-to "excali-actions")
(declare-function excali--style-value "excali-style")
(defvar excali--canvas-size)
(defvar excali--pixel-scale)

(defconst excali--flowchart-vertical-offset 100 "VERTICAL_OFFSET.")
(defconst excali--flowchart-horizontal-offset 100 "HORIZONTAL_OFFSET.")
(defconst excali--flowchart-arrow-padding 6 "PADDING in `createBindingArrow'.")

(defvar-local excali--flowchart-pending nil
  "The flowchart being created (`FlowChartCreator'), or nil.
A plist with :start (the node), :direction, :count, :cross (the
cluster's cross-axis start) and :elements (the pending nodes and
arrows, in upstream's order: node, arrow, node, arrow, ...).")

(defvar-local excali--flowchart-navigator nil
  "The walk in progress (`FlowChartNavigator'), or nil.
A plist with :direction, :nodes (same-level nodes), :index and :visited
\(element ids).")

;;;; Queries

(defun excali--flowchart-node-p (element)
  "Return non-nil if ELEMENT is a flowchart node (`isFlowchartNodeElement')."
  (member (excali--get element 'type) '("rectangle" "stickynote" "ellipse" "diamond")))

(defun excali--flowchart-selected-node ()
  "Return the one selected element if it is a flowchart node, else nil.
Labels selected with their container do not count."
  (let ((selected (seq-remove #'excali--bound-text-p excali--selection)))
    (and selected (null (cdr selected))
         (excali--flowchart-node-p (car selected))
         (car selected))))

(defun excali--binding-element-id (arrow end)
  "Return the id of the element ARROW's END (`start' or `end') is bound to."
  (let ((binding (excali--get arrow (excali--binding-key end))))
    (and (consp binding) (alist-get 'elementId binding))))

(defun excali--flowchart-connected-nodes (node)
  "Return the bindable elements linked to NODE through elbow arrows.
The whole connected component (`getConnectedFlowchartNodes'), NODE
excluded."
  (let* ((arrows (seq-filter #'excali--elbow-p (excali--live-elements)))
         (visited (list (excali--get node 'id)))
         (queue (list (excali--get node 'id)))
         connected)
    (while queue
      (let ((current (pop queue)))
        (dolist (arrow arrows)
          (let* ((start (excali--binding-element-id arrow 'start))
                 (end (excali--binding-element-id arrow 'end))
                 (neighbor (cond ((equal start current) end)
                                 ((equal end current) start))))
            (when (and neighbor (not (member neighbor visited)))
              (push neighbor visited)
              (let ((element (excali--live-element-by-id neighbor)))
                (when (and element (excali--bindable-p element))
                  (push element connected)
                  (setq queue (append queue (list neighbor))))))))))
    (nreverse connected)))

;;;; Placement

(defun excali--merge-intervals (intervals)
  "Merge overlapping INTERVALS, a list of (START . END), sorted by start."
  (let (merged)
    (dolist (interval (sort (copy-sequence intervals) (lambda (a b) (< (car a) (car b)))))
      (if (and merged (<= (car interval) (cdar merged)))
          (setcdr (car merged) (max (cdar merged) (cdr interval)))
        (push (cons (car interval) (cdr interval)) merged)))
    (nreverse merged)))

(defun excali--interval-free-p (start size occupied)
  "Return non-nil if [START, START + SIZE] misses every OCCUPIED interval."
  (seq-every-p (lambda (o) (or (<= (+ start size) (car o)) (>= start (cdr o)))) occupied))

(defun excali--nearest-free-slot (ideal size occupied)
  "Return the start nearest IDEAL for a segment of SIZE avoiding OCCUPIED.
Both sides of IDEAL are searched; a tie goes to the positive side."
  (if (excali--interval-free-p ideal size occupied)
      ideal
    (let ((starts (cons -1.0e+INF (mapcar #'cdr occupied)))
          (ends (append (mapcar #'car occupied) (list 1.0e+INF)))
          (best ideal) (best-distance 1.0e+INF))
      (cl-mapc (lambda (gap-start gap-end)
                 (unless (< (- gap-end gap-start) size)
                   (let* ((start (min (max ideal gap-start) (- gap-end size)))
                          (distance (abs (- start ideal))))
                     (when (<= distance best-distance)
                       (setq best start best-distance distance)))))
               starts ends)
      best)))

(defun excali--flowchart-place (parent direction count obstacles sticky)
  "Place COUNT nodes the size of PARENT next to it in DIRECTION.
OBSTACLES are [X1 Y1 X2 Y2] boxes to keep one gap away from; STICKY is
the cross-axis start of the cluster already shown, or nil
\(`placeCluster').  Return (POSITIONS . CROSS-START), POSITIONS being
a list of (X . Y)."
  (let* ((horizontal (memq direction '(left right)))
         (w (excali--el-w parent)) (h (excali--el-h parent))
         (x (excali--el-x parent)) (y (excali--el-y parent))
         (primary-size (if horizontal w h))
         (cross-size (if horizontal h w))
         (primary-gap (if horizontal excali--flowchart-horizontal-offset
                        excali--flowchart-vertical-offset))
         (cross-gap (if horizontal excali--flowchart-vertical-offset
                      excali--flowchart-horizontal-offset))
         (parent-primary (if horizontal x y))
         (parent-center (if horizontal (+ y (/ h 2)) (+ x (/ w 2))))
         (primary-start (if (memq direction '(right down))
                            (+ parent-primary primary-size primary-gap)
                          (- parent-primary primary-gap primary-size)))
         (occupied
          (excali--merge-intervals
           (delq nil
                 (mapcar (lambda (b)
                           (let ((start (aref b (if horizontal 0 1)))
                                 (end (aref b (if horizontal 2 3))))
                             (and (< start (+ primary-start primary-size))
                                  (> end primary-start)
                                  (cons (- (aref b (if horizontal 1 0)) cross-gap)
                                        (+ (aref b (if horizontal 3 2)) cross-gap)))))
                         obstacles))))
         (step (+ cross-size cross-gap))
         (cluster (+ (* count cross-size) (* (1- count) cross-gap)))
         (anchored
          (and sticky
               (car (sort (seq-filter (lambda (start)
                                        (excali--interval-free-p start cluster occupied))
                                      (list sticky (- sticky step)))
                          (lambda (a b)
                            (< (abs (- (+ a (/ cluster 2)) parent-center))
                               (abs (- (+ b (/ cluster 2)) parent-center))))))))
         (cross-start (or anchored
                          (excali--nearest-free-slot (- parent-center (/ cluster 2))
                                                    cluster occupied))))
    (cons (cl-loop for i below count
                   for cross = (+ cross-start (* i step))
                   collect (if horizontal (cons primary-start cross) (cons cross primary-start)))
          cross-start)))

;;;; Creating

(defun excali--flowchart-clone (template x y)
  "Return a new node like TEMPLATE at X, Y (`cloneFlowchartNode')."
  (let ((props (mapcar (lambda (key) (cons key (copy-tree (excali--get template key))))
                       '(width height roundness roughness backgroundColor strokeColor
                               strokeWidth opacity fillStyle strokeStyle))))
    (when (equal (excali--get template 'type) "stickynote")
      (push (cons 'baseHeight (excali--get template 'baseHeight)) props))
    (apply #'excali--make-element (excali--get template 'type) x y props)))

(defun excali--flowchart-arrow (from to direction)
  "Return an elbow arrow from node FROM to node TO in DIRECTION, bound.
Port of `createBindingArrow'."
  (let* ((pad excali--flowchart-arrow-padding)
         (fx (excali--el-x from)) (fy (excali--el-y from))
         (fw (excali--el-w from)) (fh (excali--el-h from))
         (tx (excali--el-x to)) (ty (excali--el-y to))
         (tw (excali--el-w to)) (th (excali--el-h to))
         (start (pcase direction
                  ('up (cons (+ fx (/ fw 2)) (- fy pad)))
                  ('down (cons (+ fx (/ fw 2)) (+ fy fh pad)))
                  ('right (cons (+ fx fw pad) (+ fy (/ fh 2))))
                  (_ (cons (- fx pad) (+ fy (/ fh 2))))))
         (sx (car start)) (sy (cdr start))
         (end (pcase direction
                ('up (cons (- (+ tx (/ tw 2)) sx) (+ (- (+ ty th) sy) pad)))
                ('down (cons (- (+ tx (/ tw 2)) sx) (- ty sy pad)))
                ('right (cons (- tx sx pad) (+ (- ty sy) (/ th 2))))
                (_ (cons (+ (- (+ tx tw) sx) pad) (+ (- ty sy) (/ th 2))))))
         (head (excali--style-value 'endArrowhead))
         (arrow (excali--make-element
                 "arrow" sx sy
                 (cons 'points (vector (vector 0.0 0.0)
                                       (vector (float (car end)) (float (cdr end)))))
                 (cons 'width (abs (float (car end)))) (cons 'height (abs (float (cdr end))))
                 (cons 'startBinding :null) (cons 'endBinding :null)
                 (cons 'startArrowhead :null) (cons 'endArrowhead (or head :null))
                 (cons 'strokeColor (excali--get from 'strokeColor))
                 (cons 'strokeStyle (excali--get from 'strokeStyle))
                 (cons 'strokeWidth (excali--get from 'strokeWidth))
                 (cons 'opacity (excali--get from 'opacity))
                 (cons 'roughness (excali--get from 'roughness)))))
    (excali--elbow-make arrow)
    (excali--elbow-bind-end arrow 'start from)
    (excali--elbow-bind-end arrow 'end to)
    (excali--elbow-route-fresh arrow)
    arrow))

(defun excali--flowchart-add-nodes (start direction count sticky)
  "Add COUNT nodes linked from START in DIRECTION (`addNewNodes').
STICKY anchors the cluster, as in `excali--flowchart-place'.  Return
\(ELEMENTS . CROSS-START); ELEMENTS are already in the scene."
  (let* ((obstacles (mapcar #'excali--aabb-for-element
                            (excali--flowchart-connected-nodes start)))
         (placed (excali--flowchart-place start direction count obstacles sticky))
         elements)
    (dolist (position (car placed))
      (let ((node (excali--flowchart-clone start (car position) (cdr position))))
        ;; The arrow routes around the nodes in the scene.
        (setq excali--elements (append excali--elements (list node)))
        (let ((arrow (excali--flowchart-arrow start node direction)))
          (setq excali--elements (append excali--elements (list arrow)))
          (push node elements)
          (push arrow elements))))
    (cons (nreverse elements) (cdr placed))))

(defun excali--flowchart-drop-pending ()
  "Take the pending nodes and arrows out of the scene again."
  (when-let* ((pending excali--flowchart-pending))
    (let ((elements (plist-get pending :elements))
          (start (plist-get pending :start)))
      (dolist (e elements)
        (when (equal (excali--get e 'type) "arrow")
          (excali--remove-bound-element start e)))
      (setq excali--elements (seq-remove (lambda (e) (memq e elements)) excali--elements)))))

(defun excali--flowchart-file-in-frame (start elements)
  "Put ELEMENTS in START's frame if each at least overlaps it.
Like upstream's `insertElements', they then sit just below the frame."
  (when-let* ((frame (excali--live-element-by-id (excali--get start 'frameId))))
    (when (seq-every-p (lambda (e) (excali--overlaps-frame-p e frame)) elements)
      (dolist (e elements)
        (excali--put e 'frameId (excali--get frame 'id)))
      (excali--place-below elements frame))))

(defun excali--flowchart-create (direction)
  "Add pending nodes from the selected node in DIRECTION (`createNodes')."
  (let* ((pending excali--flowchart-pending)
         (start (or (plist-get pending :start) (excali--flowchart-selected-node))))
    (when start
      (let* ((same (and pending (eq direction (plist-get pending :direction))))
             (count (if same (1+ (plist-get pending :count)) 1))
             (sticky (and same (plist-get pending :cross))))
        (excali--flowchart-drop-pending)
        (pcase-let ((`(,elements . ,cross)
                     (excali--flowchart-add-nodes start direction count sticky)))
          (excali--flowchart-file-in-frame start elements)
          (setq excali--history-hold t
                excali--flowchart-pending
                (list :start start :direction direction :count count
                      :cross cross :elements elements))
          (unless (excali--box-in-view-p (excali--elements-bounds elements))
            (excali--zoom-to (excali--elements-bounds elements) 'scale-down))
          (excali--render))))))

(defun excali--flowchart-commit ()
  "Commit the pending nodes: keep them and select the first one.
The history records them as one step."
  (when-let* ((pending excali--flowchart-pending))
    (setq excali--flowchart-pending nil excali--history-hold nil)
    (let ((first (car (plist-get pending :elements))))
      (excali--deselect)
      (excali--select (list first))
      (excali--scroll-into-view (excali--element-box first))
      (excali--render))
    (excali--commit)))

(defconst excali--flowchart-create-commands
  '(excali-flowchart-up excali-flowchart-down excali-flowchart-left excali-flowchart-right)
  "Commands that grow the pending flowchart.")

(defconst excali--flowchart-navigate-commands
  '(excali-flowchart-navigate-up excali-flowchart-navigate-down
    excali-flowchart-navigate-left excali-flowchart-navigate-right)
  "Commands that walk the flowchart.")

(defun excali--flowchart-pre-command ()
  "Commit pending nodes before any command but a flowchart creation.
Pointer motion and focus changes do not count: Mod is still held."
  (when (and excali--flowchart-pending
             (not (memq this-command excali--flowchart-create-commands))
             (not (memq (car-safe last-input-event)
                        '(mouse-movement focus-in focus-out switch-frame
                                         select-window help-echo))))
    (excali--flowchart-commit))
  (unless (memq this-command excali--flowchart-navigate-commands)
    (setq excali--flowchart-navigator nil)))

;;;; Viewport

(defun excali--view-box ()
  "Return the scene box (X1 Y1 X2 Y2) the window shows, or nil."
  (when excali--canvas-size
    (let ((w (/ (car excali--canvas-size) excali--pixel-scale excali--zoom))
          (h (/ (cdr excali--canvas-size) excali--pixel-scale excali--zoom)))
      (list (- excali--scroll-x) (- excali--scroll-y)
            (+ (- excali--scroll-x) w) (+ (- excali--scroll-y) h)))))

(defun excali--box-in-view-p (box)
  "Return non-nil if BOX is completely visible (`isElementCompletelyInViewport').
Without a window everything counts as visible."
  (let ((view (excali--view-box)))
    (or (null view) (null box)
        (and (<= (nth 0 view) (nth 0 box)) (<= (nth 1 view) (nth 1 box))
             (>= (nth 2 view) (nth 2 box)) (>= (nth 3 view) (nth 3 box))))))

(defun excali--scroll-into-view (box)
  "Center BOX at the current zoom unless it is completely visible.
`scrollToContent' without fitting."
  (unless (excali--box-in-view-p box)
    (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
                 (`(,v1 ,w1 ,v2 ,w2) (excali--view-box)))
      (setq excali--scroll-x (- (/ (- v2 v1) 2) (/ (+ x1 x2) 2.0))
            excali--scroll-y (- (/ (- w2 w1) 2) (/ (+ y1 y2) 2.0))))))

;;;; Navigating

(defun excali--flowchart-relatives (node direction successors)
  "Return the nodes linked to NODE whose arrows leave it toward DIRECTION.
SUCCESSORS non-nil follows arrows starting at NODE, else arrows ending
there (`getNodeRelatives')."
  (let ((aabb (excali--aabb-for-element node))
        (id (excali--get node 'id))
        relatives)
    (dolist (arrow (excali--live-elements) (nreverse relatives))
      (when (excali--elbow-p arrow)
        (let ((other (excali--binding-element-id arrow (if successors 'end 'start)))
              (own (excali--binding-element-id arrow (if successors 'start 'end))))
          (when (and other (equal own id))
            (when-let* ((relative (excali--live-element-by-id other)))
              (let* ((points (excali--get arrow 'points))
                     (edge (if successors [0 0] (aref points (1- (length points)))))
                     (p (excali--ep (+ (excali--get arrow 'x) (aref edge 0))
                                   (+ (excali--get arrow 'y) (aref edge 1)))))
                (when (eq (excali--heading-from-element node aabb p) direction)
                  (push relative relatives))))))))))

(defun excali--flowchart-linked (node direction)
  "Return successors then predecessors of NODE in DIRECTION."
  (append (excali--flowchart-relatives node direction t)
          (excali--flowchart-relatives node direction nil)))

(defun excali--flowchart-explore (node direction)
  "Return the node to go to from NODE in DIRECTION, or nil.
Port of `FlowChartNavigator.exploreByDirection'."
  (unless (eq direction (plist-get excali--flowchart-navigator :direction))
    (setq excali--flowchart-navigator nil))
  (let* ((nav excali--flowchart-navigator)
         (exploring (plist-get nav :nodes-set))
         (visited (plist-get nav :visited))
         (id (excali--get node 'id)))
    (unless (member id visited) (push id visited))
    (setq nav (plist-put nav :visited visited))
    (setq excali--flowchart-navigator nav)
    (cond
     ;; Cycle through the nodes found at this level.
     ((and exploring (eq direction (plist-get nav :direction))
           (> (length (plist-get nav :nodes)) 1))
      (let ((index (mod (1+ (plist-get nav :index)) (length (plist-get nav :nodes)))))
        (setq excali--flowchart-navigator (plist-put nav :index index))
        (nth index (plist-get nav :nodes))))
     (t
      (let ((nodes (excali--flowchart-linked node direction)))
        (if nodes
            (progn
              (setq excali--flowchart-navigator
                    (list :direction direction :nodes nodes :index 0 :nodes-set t
                          :visited (cons (excali--get (car nodes) 'id) visited)))
              (car nodes))
          ;; Nothing that way: jump to some other unvisited linked node.
          (when (or (eq direction (plist-get nav :direction)) (not exploring))
            (let ((other (seq-find
                          (lambda (e) (not (member (excali--get e 'id) visited)))
                          (apply #'append
                                 (mapcar (lambda (d) (excali--flowchart-linked node d))
                                         (remq direction '(up right down left)))))))
              (when other
                (setq excali--flowchart-navigator
                      (plist-put (plist-put (plist-put nav :visited
                                                       (cons (excali--get other 'id) visited))
                                            :nodes-set t)
                                 :direction direction))
                other)))))))))

(defun excali--flowchart-navigate (direction)
  "Select the node linked to the selected one in DIRECTION."
  (when-let* ((node (excali--flowchart-selected-node))
              (next (excali--flowchart-explore node direction)))
    (excali--deselect)
    (excali--select (list next))
    (excali--scroll-into-view (excali--element-box next))
    (excali--render)))

;;;; Commands

(defmacro excali--define-flowchart-commands (direction where)
  "Define the create and navigate commands for DIRECTION.
WHERE words the direction for the docstrings, as \"above\"."
  `(progn
     (defun ,(intern (format "excali-flowchart-%s" direction)) ()
       ,(format "Add a linked copy of the selected node %s it.
Repeating it adds siblings; the nodes are kept once another command runs."
                where)
       (interactive)
       (excali--flowchart-create ',direction))
     (defun ,(intern (format "excali-flowchart-navigate-%s" direction)) ()
       ,(format "Select a node linked %s the selected one.
Repeating it cycles through the nodes at that level." where)
       (interactive)
       (excali--flowchart-navigate ',direction))))

(excali--define-flowchart-commands up "above")
(excali--define-flowchart-commands down "below")
(excali--define-flowchart-commands left "left of")
(excali--define-flowchart-commands right "right of")

(provide 'excali-flowchart)
;;; excali-flowchart.el ends here
