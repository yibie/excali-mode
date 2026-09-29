;;; excali-dsl-layout.el --- Layered graph layout for the diagram DSL  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A layered (Sugiyama) layout, the kind dagre does, for excali-dsl.el:
;;
;; 1. cycles are broken by reversing the back edges of a depth-first
;;    search;
;; 2. nodes get layers by longest path from the sources, and sources are
;;    pulled down next to their first successor;
;; 3. edges spanning several layers get a dummy node on each layer they
;;    cross, so they take part in ordering and can bend around nodes;
;; 4. each layer is ordered by barycenter sweeps, keeping the order with
;;    the fewest crossings;
;; 5. positions across the layers are the least-squares fit to the
;;    neighbours' positions under the spacing constraints, solved exactly
;;    by pool-adjacent-violators (isotonic regression), sweeping down and
;;    up a few times.
;;
;; Everything is computed along two axes, "cross" (within a layer) and
;; "main" (from layer to layer), and turned into x, y for the direction:
;; TB (the default), BT, LR or RL.  The functions here know nothing of
;; excali; they take sizes and edges and return positions.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defconst excali-dsl-layout-iterations 12
  "Barycenter sweeps when ordering layers.")

(defconst excali-dsl-layout-placement-passes 8
  "Down-and-up passes when placing nodes within layers.")

;;;; Cycle breaking and layering

(defun excali-dsl-layout--acyclic (n edges)
  "Return EDGES of a graph with N nodes made acyclic.
EDGES is a list of (U . V); the result is a list of (U V REVERSED)
where back edges of a depth-first search are turned around.  Self
loops are dropped."
  (let ((succ (make-vector n nil))
        (state (make-vector n 0))          ; 0 new, 1 on stack, 2 done
        (back (make-hash-table :test #'equal)))
    (dolist (e edges)
      (unless (= (car e) (cdr e))
        (push (cdr e) (aref succ (car e)))))
    (dotimes (i n) (aset succ i (nreverse (aref succ i))))
    (dotimes (root n)
      (when (= (aref state root) 0)
        ;; Iterative DFS: a stack of (NODE . REMAINING-SUCCESSORS).
        (let ((stack (list (cons root (aref succ root)))))
          (aset state root 1)
          (while stack
            (let ((top (car stack)))
              (if (null (cdr top))
                  (progn (aset state (car top) 2) (pop stack))
                (let ((v (pop (cdr top))))
                  (pcase (aref state v)
                    (0 (aset state v 1)
                       (push (cons v (aref succ v)) stack))
                    (1 (puthash (cons (car top) v) t back))))))))))
    (let (out)
      (dolist (e edges (nreverse out))
        (unless (= (car e) (cdr e))
          (if (gethash e back)
              (push (list (cdr e) (car e) t) out)
            (push (list (car e) (cdr e) nil) out)))))))

(defun excali-dsl-layout--layers (n dag)
  "Return a vector of layer numbers for N nodes of the acyclic DAG.
DAG is a list of (U V REVERSED).  Longest path from the sources, then
each source moves down to just above its nearest successor."
  (let ((indeg (make-vector n 0))
        (succ (make-vector n nil))
        (pred (make-vector n nil))
        (layer (make-vector n 0))
        (queue nil) (order nil))
    (dolist (e dag)
      (cl-incf (aref indeg (nth 1 e)))
      (push (nth 1 e) (aref succ (car e)))
      (push (car e) (aref pred (nth 1 e))))
    (dotimes (i n) (when (= (aref indeg i) 0) (push i queue)))
    (setq queue (nreverse queue))
    (while queue
      (let ((u (pop queue)))
        (push u order)
        (dolist (v (aref succ u))
          (aset layer v (max (aref layer v) (1+ (aref layer u))))
          (when (= (cl-decf (aref indeg v)) 0)
            (setq queue (append queue (list v)))))))
    ;; Pull sources down: a source feeding only later layers sits just
    ;; above the nearest of them.
    (dolist (u order)
      (when (and (null (aref pred u)) (aref succ u))
        (aset layer u (max (aref layer u)
                           (1- (apply #'min (mapcar (lambda (v) (aref layer v))
                                                    (aref succ u))))))))
    layer))

;;;; Ordering

(defun excali-dsl-layout--crossings (upper lower pos down)
  "Count crossings between layers UPPER and LOWER.
POS maps nodes to their index in their layer; DOWN maps a node to its
successors in the next layer."
  (let (pairs (count 0))
    (dolist (u upper)
      (dolist (v (gethash u down))
        (when (memq v lower)
          (push (cons (gethash u pos) (gethash v pos)) pairs))))
    (let ((v (vconcat pairs)))
      (dotimes (i (length v))
        (cl-loop for j from (1+ i) below (length v)
                 for a = (aref v i) for b = (aref v j)
                 when (< (* (- (car a) (car b)) (- (cdr a) (cdr b))) 0)
                 do (cl-incf count))))
    count))

(defun excali-dsl-layout--order (layers down up)
  "Order the nodes of LAYERS (a vector of lists) to reduce crossings.
DOWN and UP map each node to its neighbours in the next and previous
layer.  Return the best vector of ordered lists found."
  (let* ((count (length layers))
         (pos (make-hash-table))
         (index (lambda (layers)
                  (dotimes (l (length layers))
                    (cl-loop for v in (aref layers l) for i from 0
                             do (puthash v i pos)))))
         (total (lambda (layers)
                  (funcall index layers)
                  (cl-loop for l below (1- (length layers))
                           sum (excali-dsl-layout--crossings
                                (aref layers l) (aref layers (1+ l)) pos down))))
         (best (copy-sequence layers))
         (best-count (funcall total layers))
         (current (copy-sequence layers)))
    (dotimes (iter excali-dsl-layout-iterations)
      (let ((downward (cl-evenp iter)))
        (funcall index current)
        (dolist (l (if downward
                       (number-sequence 1 (1- count))
                     (number-sequence (- count 2) 0 -1)))
          (let* ((neighbours (if downward up down))
                 (keyed (mapcar
                         (lambda (v)
                           (let ((ns (gethash v neighbours)))
                             (cons v (if ns
                                         (/ (float (apply #'+ (mapcar (lambda (u) (gethash u pos)) ns)))
                                            (length ns))
                                       (float (gethash v pos))))))
                         (aref current l)))
                 (sorted (mapcar #'car (sort keyed (lambda (a b) (< (cdr a) (cdr b)))))))
            (aset current l sorted)
            (cl-loop for v in sorted for i from 0 do (puthash v i pos)))))
      (let ((c (funcall total current)))
        (when (< c best-count)
          (setq best (copy-sequence current) best-count c))))
    best))

;;;; Placement within layers

(defun excali-dsl-layout--isotonic (targets)
  "Return the non-decreasing sequence closest to TARGETS (least squares).
Pool adjacent violators; TARGETS is a list of numbers."
  (let (blocks)                         ; stack of (SUM . COUNT)
    (dolist (y targets)
      (push (cons (float y) 1) blocks)
      (while (and (cdr blocks)
                  (< (/ (caar blocks) (cdar blocks))
                     (/ (car (cadr blocks)) (cdr (cadr blocks)))))
        (let ((top (pop blocks)))
          (setcar (car blocks) (+ (caar blocks) (car top)))
          (setcdr (car blocks) (+ (cdar blocks) (cdr top))))))
    (let (out)
      (dolist (b blocks out)
        (let ((mean (/ (car b) (cdr b))))
          (dotimes (_ (cdr b)) (push mean out)))))))

(defun excali-dsl-layout--fit (nodes desired gaps)
  "Place NODES, in order, as close to DESIRED centers as GAPS allow.
GAPS holds the least center distance between each node and the next.
Return a list of centers."
  (let* ((offsets (let ((c 0.0) out)
                    (push c out)
                    (dolist (g gaps) (setq c (+ c g)) (push c out))
                    (nreverse out)))
         (fitted (excali-dsl-layout--isotonic
                  (cl-mapcar (lambda (d o) (- d o)) desired offsets))))
    (ignore nodes)
    (cl-mapcar #'+ fitted offsets)))

;;;; Entry point

(cl-defun excali-dsl-layout-graph (sizes edges &key (direction "TB")
                                         (node-spacing 60) (rank-spacing 90))
  "Lay out a graph and return (POSITIONS . WAYPOINTS).
SIZES is a vector of (WIDTH . HEIGHT), one per node; EDGES a list of
\(U . V) node indices.  DIRECTION is TB, BT, LR or RL.  NODE-SPACING
separates neighbours in a layer, RANK-SPACING the layers.

POSITIONS is a vector of the nodes' top-left (X . Y), the whole drawing
starting at 0, 0.  WAYPOINTS is a list parallel to EDGES: for an edge
spanning several layers, the centers (X . Y) it should pass through from
U to V, else nil."
  (let* ((n (length sizes))
         (horizontal (member direction '("LR" "RL")))
         (reverse-main (member direction '("BT" "RL")))
         (cross-size (lambda (i) (let ((s (aref sizes i))) (float (if horizontal (cdr s) (car s))))))
         (main-size (lambda (i) (let ((s (aref sizes i))) (float (if horizontal (car s) (cdr s))))))
         (dag (excali-dsl-layout--acyclic n edges))
         (layer (excali-dsl-layout--layers n dag))
         (total n)
         (dummy-size (make-hash-table))
         (down (make-hash-table)) (up (make-hash-table))
         (chains nil))                    ; edge -> nodes from U to V
    ;; Normalize: a dummy node on every layer a long edge crosses.
    (let ((extra-layers nil))
      (dolist (e dag)
        (pcase-let ((`(,u ,v ,reversed) e))
          (let ((prev u) (chain nil))
            (cl-loop for l from (1+ (aref layer u)) below (aref layer v)
                     do (let ((d total))
                          (cl-incf total)
                          (push (cons d l) extra-layers)
                          (puthash d 0.0 dummy-size)
                          (push d chain)
                          (push d (gethash prev down))
                          (push prev (gethash d up))
                          (setq prev d)))
            (push v (gethash prev down))
            (push prev (gethash v up))
            (setq chain (nreverse chain))
            (push (cons e (if reversed (reverse chain) chain)) chains))))
      (let ((all-layer (make-vector total 0)))
        (dotimes (i n) (aset all-layer i (aref layer i)))
        (dolist (d extra-layers) (aset all-layer (car d) (cdr d)))
        (setq layer all-layer)))
    (let* ((count (1+ (if (> total 0) (seq-max layer) 0)))
           (layers (make-vector count nil))
           (dummy-p (lambda (v) (>= v n)))
           (csize (lambda (v) (if (funcall dummy-p v) 0.0 (funcall cross-size v))))
           (msize (lambda (v) (if (funcall dummy-p v) 0.0 (funcall main-size v)))))
      (when (> total 0)
        ;; Initial order: depth-first discovery from the first layer.
        (let ((seen (make-hash-table)) (order nil))
          (cl-labels ((visit (v)
                        (unless (gethash v seen)
                          (puthash v t seen)
                          (push v order)
                          (dolist (w (reverse (gethash v down))) (visit w)))))
            (dotimes (v total)
              (when (= (aref layer v) 0) (visit v)))
            (dotimes (v total) (visit v)))
          (dolist (v (nreverse order))
            (push v (aref layers (aref layer v)))))
        (dotimes (l count) (aset layers l (nreverse (aref layers l))))
        (setq layers (excali-dsl-layout--order layers down up)))
      ;; Cross positions.
      (let* ((cross (make-hash-table))
             (gap (lambda (a b)
                    (+ (/ (funcall csize a) 2) (/ (funcall csize b) 2)
                       (cond ((and (funcall dummy-p a) (funcall dummy-p b)) (/ node-spacing 4.0))
                             ((or (funcall dummy-p a) (funcall dummy-p b)) (/ node-spacing 2.0))
                             (t (float node-spacing))))))
             (place (lambda (nodes desired)
                      (cl-mapc (lambda (v c) (puthash v c cross) )
                               nodes
                               (excali-dsl-layout--fit
                                nodes desired
                                (cl-loop for (a b) on nodes while b collect (funcall gap a b))))))
             (neighbour-mean (lambda (v tables)
                               (let ((ns (apply #'append (mapcar (lambda (tb) (gethash v tb)) tables))))
                                 (if ns
                                     (/ (apply #'+ (mapcar (lambda (u) (gethash u cross)) ns))
                                        (float (length ns)))
                                   (gethash v cross))))))
        ;; Start packed and centered.
        (dotimes (l count)
          (let ((nodes (aref layers l)) (c 0.0) (prev nil))
            (dolist (v nodes)
              (when prev (setq c (+ c (funcall gap prev v))))
              (puthash v c cross)
              (setq prev v))
            (dolist (v nodes) (puthash v (- (gethash v cross) (/ c 2)) cross))))
        (dotimes (_ excali-dsl-layout-placement-passes)
          (dolist (l (number-sequence 1 (1- count)))
            (let ((nodes (aref layers l)))
              (funcall place nodes (mapcar (lambda (v) (funcall neighbour-mean v (list up))) nodes))))
          (dolist (l (number-sequence (- count 2) 0 -1))
            (let ((nodes (aref layers l)))
              (funcall place nodes (mapcar (lambda (v) (funcall neighbour-mean v (list down))) nodes)))))
        (dotimes (l count)
          (let ((nodes (aref layers l)))
            (funcall place nodes (mapcar (lambda (v) (funcall neighbour-mean v (list up down))) nodes))))
        ;; Main positions: layer centers.
        (let ((main (make-vector count 0.0)) (at 0.0))
          (dotimes (l count)
            (let ((thick (apply #'max 0.0 (mapcar msize (aref layers l)))))
              (aset main l (+ at (/ thick 2)))
              (setq at (+ at thick rank-spacing))))
          (let* ((center (lambda (v)
                           (let ((c (gethash v cross))
                                 (m (* (if reverse-main -1 1) (aref main (aref layer v)))))
                             (if horizontal (cons m c) (cons c m)))))
                 (positions (make-vector n nil))
                 (min-x 1.0e+INF) (min-y 1.0e+INF))
            (dotimes (i n)
              (let* ((c (funcall center i)) (s (aref sizes i))
                     (x (- (car c) (/ (car s) 2.0))) (y (- (cdr c) (/ (cdr s) 2.0))))
                (aset positions i (cons x y))
                (setq min-x (min min-x x) min-y (min min-y y))))
            (when (= n 0) (setq min-x 0.0 min-y 0.0))
            (dotimes (i n)
              (let ((p (aref positions i)))
                (aset positions i (cons (- (car p) min-x) (- (cdr p) min-y)))))
            (cons positions
                  (mapcar (lambda (e)
                            (if (= (car e) (cdr e))
                                nil
                              (let* ((entry (seq-find (lambda (c) (and (eq (car (car c)) (if (nth 2 (car c)) (cdr e) (car e)))
                                                                       (eq (nth 1 (car c)) (if (nth 2 (car c)) (car e) (cdr e)))))
                                                      chains)))
                                (mapcar (lambda (d)
                                          (let ((c (funcall center d)))
                                            (cons (- (car c) min-x) (- (cdr c) min-y))))
                                        (cdr entry)))))
                          edges))))))))

(defun excali-dsl-layout-grid (sizes columns &optional spacing)
  "Place SIZES, a vector of (WIDTH . HEIGHT), in a grid of COLUMNS.
Rows are as tall and columns as wide as their largest cell; SPACING
separates cells.  Return a vector of top-left (X . Y)."
  (let* ((n (length sizes))
         (spacing (float (or spacing 40)))
         (columns (max 1 (min columns (max n 1))))
         (rows (ceiling n (float columns)))
         (col-w (make-vector columns 0.0))
         (row-h (make-vector (max rows 1) 0.0))
         (positions (make-vector n nil)))
    (dotimes (i n)
      (let ((s (aref sizes i)) (c (% i columns)) (r (/ i columns)))
        (aset col-w c (max (aref col-w c) (float (car s))))
        (aset row-h r (max (aref row-h r) (float (cdr s))))))
    (dotimes (i n)
      (let* ((s (aref sizes i)) (c (% i columns)) (r (/ i columns))
             (x (+ (cl-loop for k below c sum (+ (aref col-w k) spacing))
                   (/ (- (aref col-w c) (car s)) 2.0)))
             (y (+ (cl-loop for k below r sum (+ (aref row-h k) spacing))
                   (/ (- (aref row-h r) (cdr s)) 2.0))))
        (aset positions i (cons x y))))
    positions))

(provide 'excali-dsl-layout)
;;; excali-dsl-layout.el ends here
