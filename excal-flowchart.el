;;; excal-flowchart.el --- Flowchart creation and navigation  -*- lexical-binding: t; -*-

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

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-binding)
(require 'excal-elbow)
(require 'excal-frame)
(require 'excal-history)

(declare-function excal--zoom-to "excal-actions")
(declare-function excal--style-value "excal-style")
(defvar excal--canvas-size)
(defvar excal--pixel-scale)

(defconst excal--flowchart-vertical-offset 100 "VERTICAL_OFFSET.")
(defconst excal--flowchart-horizontal-offset 100 "HORIZONTAL_OFFSET.")
(defconst excal--flowchart-arrow-padding 6 "PADDING in `createBindingArrow'.")

(defvar-local excal--flowchart-pending nil
  "The flowchart being created (`FlowChartCreator'), or nil.
A plist with :start (the node), :direction, :count, :cross (the
cluster's cross-axis start) and :elements (the pending nodes and
arrows, in upstream's order: node, arrow, node, arrow, ...).")

(defvar-local excal--flowchart-navigator nil
  "The walk in progress (`FlowChartNavigator'), or nil.
A plist with :direction, :nodes (same-level nodes), :index and :visited
\(element ids).")

;;;; Queries

(defun excal--flowchart-node-p (element)
  "Return non-nil if ELEMENT is a flowchart node (`isFlowchartNodeElement')."
  (member (excal--get element 'type) '("rectangle" "stickynote" "ellipse" "diamond")))

(defun excal--flowchart-selected-node ()
  "Return the one selected element if it is a flowchart node, else nil.
Labels selected with their container do not count."
  (let ((selected (seq-remove #'excal--bound-text-p excal--selection)))
    (and selected (null (cdr selected))
         (excal--flowchart-node-p (car selected))
         (car selected))))

(defun excal--binding-element-id (arrow end)
  "Return the id of the element ARROW's END (`start' or `end') is bound to."
  (let ((binding (excal--get arrow (excal--binding-key end))))
    (and (consp binding) (alist-get 'elementId binding))))

(defun excal--flowchart-connected-nodes (node)
  "Return the bindable elements linked to NODE through elbow arrows.
The whole connected component (`getConnectedFlowchartNodes'), NODE
excluded."
  (let* ((arrows (seq-filter #'excal--elbow-p (excal--live-elements)))
         (visited (list (excal--get node 'id)))
         (queue (list (excal--get node 'id)))
         connected)
    (while queue
      (let ((current (pop queue)))
        (dolist (arrow arrows)
          (let* ((start (excal--binding-element-id arrow 'start))
                 (end (excal--binding-element-id arrow 'end))
                 (neighbor (cond ((equal start current) end)
                                 ((equal end current) start))))
            (when (and neighbor (not (member neighbor visited)))
              (push neighbor visited)
              (let ((element (excal--live-element-by-id neighbor)))
                (when (and element (excal--bindable-p element))
                  (push element connected)
                  (setq queue (append queue (list neighbor))))))))))
    (nreverse connected)))

;;;; Placement

(defun excal--merge-intervals (intervals)
  "Merge overlapping INTERVALS, a list of (START . END), sorted by start."
  (let (merged)
    (dolist (interval (sort (copy-sequence intervals) (lambda (a b) (< (car a) (car b)))))
      (if (and merged (<= (car interval) (cdar merged)))
          (setcdr (car merged) (max (cdar merged) (cdr interval)))
        (push (cons (car interval) (cdr interval)) merged)))
    (nreverse merged)))

(defun excal--interval-free-p (start size occupied)
  "Return non-nil if [START, START + SIZE] misses every OCCUPIED interval."
  (seq-every-p (lambda (o) (or (<= (+ start size) (car o)) (>= start (cdr o)))) occupied))

(defun excal--nearest-free-slot (ideal size occupied)
  "Return the start nearest IDEAL for a segment of SIZE avoiding OCCUPIED.
Both sides of IDEAL are searched; a tie goes to the positive side."
  (if (excal--interval-free-p ideal size occupied)
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

(defun excal--flowchart-place (parent direction count obstacles sticky)
  "Place COUNT nodes the size of PARENT next to it in DIRECTION.
OBSTACLES are [X1 Y1 X2 Y2] boxes to keep one gap away from; STICKY is
the cross-axis start of the cluster already shown, or nil
\(`placeCluster').  Return (POSITIONS . CROSS-START), POSITIONS being
a list of (X . Y)."
  (let* ((horizontal (memq direction '(left right)))
         (w (excal--el-w parent)) (h (excal--el-h parent))
         (x (excal--el-x parent)) (y (excal--el-y parent))
         (primary-size (if horizontal w h))
         (cross-size (if horizontal h w))
         (primary-gap (if horizontal excal--flowchart-horizontal-offset
                        excal--flowchart-vertical-offset))
         (cross-gap (if horizontal excal--flowchart-vertical-offset
                      excal--flowchart-horizontal-offset))
         (parent-primary (if horizontal x y))
         (parent-center (if horizontal (+ y (/ h 2)) (+ x (/ w 2))))
         (primary-start (if (memq direction '(right down))
                            (+ parent-primary primary-size primary-gap)
                          (- parent-primary primary-gap primary-size)))
         (occupied
          (excal--merge-intervals
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
                                        (excal--interval-free-p start cluster occupied))
                                      (list sticky (- sticky step)))
                          (lambda (a b)
                            (< (abs (- (+ a (/ cluster 2)) parent-center))
                               (abs (- (+ b (/ cluster 2)) parent-center))))))))
         (cross-start (or anchored
                          (excal--nearest-free-slot (- parent-center (/ cluster 2))
                                                    cluster occupied))))
    (cons (cl-loop for i below count
                   for cross = (+ cross-start (* i step))
                   collect (if horizontal (cons primary-start cross) (cons cross primary-start)))
          cross-start)))

;;;; Creating

(defun excal--flowchart-clone (template x y)
  "Return a new node like TEMPLATE at X, Y (`cloneFlowchartNode')."
  (let ((props (mapcar (lambda (key) (cons key (copy-tree (excal--get template key))))
                       '(width height roundness roughness backgroundColor strokeColor
                               strokeWidth opacity fillStyle strokeStyle))))
    (when (equal (excal--get template 'type) "stickynote")
      (push (cons 'baseHeight (excal--get template 'baseHeight)) props))
    (apply #'excal--make-element (excal--get template 'type) x y props)))

(defun excal--flowchart-arrow (from to direction)
  "Return an elbow arrow from node FROM to node TO in DIRECTION, bound.
Port of `createBindingArrow'."
  (let* ((pad excal--flowchart-arrow-padding)
         (fx (excal--el-x from)) (fy (excal--el-y from))
         (fw (excal--el-w from)) (fh (excal--el-h from))
         (tx (excal--el-x to)) (ty (excal--el-y to))
         (tw (excal--el-w to)) (th (excal--el-h to))
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
         (head (excal--style-value 'endArrowhead))
         (arrow (excal--make-element
                 "arrow" sx sy
                 (cons 'points (vector (vector 0.0 0.0)
                                       (vector (float (car end)) (float (cdr end)))))
                 (cons 'width (abs (float (car end)))) (cons 'height (abs (float (cdr end))))
                 (cons 'startBinding :null) (cons 'endBinding :null)
                 (cons 'startArrowhead :null) (cons 'endArrowhead (or head :null))
                 (cons 'strokeColor (excal--get from 'strokeColor))
                 (cons 'strokeStyle (excal--get from 'strokeStyle))
                 (cons 'strokeWidth (excal--get from 'strokeWidth))
                 (cons 'opacity (excal--get from 'opacity))
                 (cons 'roughness (excal--get from 'roughness)))))
    (excal--elbow-make arrow)
    (excal--elbow-bind-end arrow 'start from)
    (excal--elbow-bind-end arrow 'end to)
    (excal--elbow-route-fresh arrow)
    arrow))

(defun excal--flowchart-add-nodes (start direction count sticky)
  "Add COUNT nodes linked from START in DIRECTION (`addNewNodes').
STICKY anchors the cluster, as in `excal--flowchart-place'.  Return
\(ELEMENTS . CROSS-START); ELEMENTS are already in the scene."
  (let* ((obstacles (mapcar #'excal--aabb-for-element
                            (excal--flowchart-connected-nodes start)))
         (placed (excal--flowchart-place start direction count obstacles sticky))
         elements)
    (dolist (position (car placed))
      (let ((node (excal--flowchart-clone start (car position) (cdr position))))
        ;; The arrow routes around the nodes in the scene.
        (setq excal--elements (append excal--elements (list node)))
        (let ((arrow (excal--flowchart-arrow start node direction)))
          (setq excal--elements (append excal--elements (list arrow)))
          (push node elements)
          (push arrow elements))))
    (cons (nreverse elements) (cdr placed))))

(defun excal--flowchart-drop-pending ()
  "Take the pending nodes and arrows out of the scene again."
  (when-let* ((pending excal--flowchart-pending))
    (let ((elements (plist-get pending :elements))
          (start (plist-get pending :start)))
      (dolist (e elements)
        (when (equal (excal--get e 'type) "arrow")
          (excal--remove-bound-element start e)))
      (setq excal--elements (seq-remove (lambda (e) (memq e elements)) excal--elements)))))

(defun excal--flowchart-file-in-frame (start elements)
  "Put ELEMENTS in START's frame if each at least overlaps it.
Like upstream's `insertElements', they then sit just below the frame."
  (when-let* ((frame (excal--live-element-by-id (excal--get start 'frameId))))
    (when (seq-every-p (lambda (e) (excal--overlaps-frame-p e frame)) elements)
      (dolist (e elements)
        (excal--put e 'frameId (excal--get frame 'id)))
      (excal--place-below elements frame))))

(defun excal--flowchart-create (direction)
  "Add pending nodes from the selected node in DIRECTION (`createNodes')."
  (let* ((pending excal--flowchart-pending)
         (start (or (plist-get pending :start) (excal--flowchart-selected-node))))
    (when start
      (let* ((same (and pending (eq direction (plist-get pending :direction))))
             (count (if same (1+ (plist-get pending :count)) 1))
             (sticky (and same (plist-get pending :cross))))
        (excal--flowchart-drop-pending)
        (pcase-let ((`(,elements . ,cross)
                     (excal--flowchart-add-nodes start direction count sticky)))
          (excal--flowchart-file-in-frame start elements)
          (setq excal--history-hold t
                excal--flowchart-pending
                (list :start start :direction direction :count count
                      :cross cross :elements elements))
          (unless (excal--box-in-view-p (excal--elements-bounds elements))
            (excal--zoom-to (excal--elements-bounds elements) 'scale-down))
          (excal--render))))))

(defun excal--flowchart-commit ()
  "Commit the pending nodes: keep them and select the first one.
The history records them as one step."
  (when-let* ((pending excal--flowchart-pending))
    (setq excal--flowchart-pending nil excal--history-hold nil)
    (let ((first (car (plist-get pending :elements))))
      (excal--deselect)
      (excal--select (list first))
      (excal--scroll-into-view (excal--element-box first))
      (excal--render))
    (excal--commit)))

(defconst excal--flowchart-create-commands
  '(excal-flowchart-up excal-flowchart-down excal-flowchart-left excal-flowchart-right)
  "Commands that grow the pending flowchart.")

(defconst excal--flowchart-navigate-commands
  '(excal-flowchart-navigate-up excal-flowchart-navigate-down
    excal-flowchart-navigate-left excal-flowchart-navigate-right)
  "Commands that walk the flowchart.")

(defun excal--flowchart-pre-command ()
  "Commit pending nodes before any command but a flowchart creation.
Pointer motion and focus changes do not count: Mod is still held."
  (when (and excal--flowchart-pending
             (not (memq this-command excal--flowchart-create-commands))
             (not (memq (car-safe last-input-event)
                        '(mouse-movement focus-in focus-out switch-frame
                                         select-window help-echo))))
    (excal--flowchart-commit))
  (unless (memq this-command excal--flowchart-navigate-commands)
    (setq excal--flowchart-navigator nil)))

;;;; Viewport

(defun excal--view-box ()
  "Return the scene box (X1 Y1 X2 Y2) the window shows, or nil."
  (when excal--canvas-size
    (let ((w (/ (car excal--canvas-size) excal--pixel-scale excal--zoom))
          (h (/ (cdr excal--canvas-size) excal--pixel-scale excal--zoom)))
      (list (- excal--scroll-x) (- excal--scroll-y)
            (+ (- excal--scroll-x) w) (+ (- excal--scroll-y) h)))))

(defun excal--box-in-view-p (box)
  "Return non-nil if BOX is completely visible (`isElementCompletelyInViewport').
Without a window everything counts as visible."
  (let ((view (excal--view-box)))
    (or (null view) (null box)
        (and (<= (nth 0 view) (nth 0 box)) (<= (nth 1 view) (nth 1 box))
             (>= (nth 2 view) (nth 2 box)) (>= (nth 3 view) (nth 3 box))))))

(defun excal--scroll-into-view (box)
  "Center BOX at the current zoom unless it is completely visible.
`scrollToContent' without fitting."
  (unless (excal--box-in-view-p box)
    (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
                 (`(,v1 ,w1 ,v2 ,w2) (excal--view-box)))
      (setq excal--scroll-x (- (/ (- v2 v1) 2) (/ (+ x1 x2) 2.0))
            excal--scroll-y (- (/ (- w2 w1) 2) (/ (+ y1 y2) 2.0))))))

;;;; Navigating

(defun excal--flowchart-relatives (node direction successors)
  "Return the nodes linked to NODE whose arrows leave it toward DIRECTION.
SUCCESSORS non-nil follows arrows starting at NODE, else arrows ending
there (`getNodeRelatives')."
  (let ((aabb (excal--aabb-for-element node))
        (id (excal--get node 'id))
        relatives)
    (dolist (arrow (excal--live-elements) (nreverse relatives))
      (when (excal--elbow-p arrow)
        (let ((other (excal--binding-element-id arrow (if successors 'end 'start)))
              (own (excal--binding-element-id arrow (if successors 'start 'end))))
          (when (and other (equal own id))
            (when-let* ((relative (excal--live-element-by-id other)))
              (let* ((points (excal--get arrow 'points))
                     (edge (if successors [0 0] (aref points (1- (length points)))))
                     (p (excal--ep (+ (excal--get arrow 'x) (aref edge 0))
                                   (+ (excal--get arrow 'y) (aref edge 1)))))
                (when (eq (excal--heading-from-element node aabb p) direction)
                  (push relative relatives))))))))))

(defun excal--flowchart-linked (node direction)
  "Return successors then predecessors of NODE in DIRECTION."
  (append (excal--flowchart-relatives node direction t)
          (excal--flowchart-relatives node direction nil)))

(defun excal--flowchart-explore (node direction)
  "Return the node to go to from NODE in DIRECTION, or nil.
Port of `FlowChartNavigator.exploreByDirection'."
  (unless (eq direction (plist-get excal--flowchart-navigator :direction))
    (setq excal--flowchart-navigator nil))
  (let* ((nav excal--flowchart-navigator)
         (exploring (plist-get nav :nodes-set))
         (visited (plist-get nav :visited))
         (id (excal--get node 'id)))
    (unless (member id visited) (push id visited))
    (setq nav (plist-put nav :visited visited))
    (setq excal--flowchart-navigator nav)
    (cond
     ;; Cycle through the nodes found at this level.
     ((and exploring (eq direction (plist-get nav :direction))
           (> (length (plist-get nav :nodes)) 1))
      (let ((index (mod (1+ (plist-get nav :index)) (length (plist-get nav :nodes)))))
        (setq excal--flowchart-navigator (plist-put nav :index index))
        (nth index (plist-get nav :nodes))))
     (t
      (let ((nodes (excal--flowchart-linked node direction)))
        (if nodes
            (progn
              (setq excal--flowchart-navigator
                    (list :direction direction :nodes nodes :index 0 :nodes-set t
                          :visited (cons (excal--get (car nodes) 'id) visited)))
              (car nodes))
          ;; Nothing that way: jump to some other unvisited linked node.
          (when (or (eq direction (plist-get nav :direction)) (not exploring))
            (let ((other (seq-find
                          (lambda (e) (not (member (excal--get e 'id) visited)))
                          (apply #'append
                                 (mapcar (lambda (d) (excal--flowchart-linked node d))
                                         (remq direction '(up right down left)))))))
              (when other
                (setq excal--flowchart-navigator
                      (plist-put (plist-put (plist-put nav :visited
                                                       (cons (excal--get other 'id) visited))
                                            :nodes-set t)
                                 :direction direction))
                other)))))))))

(defun excal--flowchart-navigate (direction)
  "Select the node linked to the selected one in DIRECTION."
  (when-let* ((node (excal--flowchart-selected-node))
              (next (excal--flowchart-explore node direction)))
    (excal--deselect)
    (excal--select (list next))
    (excal--scroll-into-view (excal--element-box next))
    (excal--render)))

;;;; Commands

(defmacro excal--define-flowchart-commands (direction where)
  "Define the create and navigate commands for DIRECTION.
WHERE words the direction for the docstrings, as \"above\"."
  `(progn
     (defun ,(intern (format "excal-flowchart-%s" direction)) ()
       ,(format "Add a linked copy of the selected node %s it.
Repeating it adds siblings; the nodes are kept once another command runs."
                where)
       (interactive)
       (excal--flowchart-create ',direction))
     (defun ,(intern (format "excal-flowchart-navigate-%s" direction)) ()
       ,(format "Select a node linked %s the selected one.
Repeating it cycles through the nodes at that level." where)
       (interactive)
       (excal--flowchart-navigate ',direction))))

(excal--define-flowchart-commands up "above")
(excal--define-flowchart-commands down "below")
(excal--define-flowchart-commands left "left of")
(excal--define-flowchart-commands right "right of")

(provide 'excal-flowchart)
;;; excal-flowchart.el ends here
