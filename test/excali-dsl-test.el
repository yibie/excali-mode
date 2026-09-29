;;; excali-dsl-test.el --- The diagram DSL  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; test/fixtures/edsl/*.edsl are examples of excalidraw-dsl (MIT, see
;; THIRD-PARTY-NOTICES); every one must parse and draw.

(require 'ert)
(require 'excali)
(require 'excali-dsl)
(require 'excali-test)

(defconst excali-dsl-test--fixtures
  (expand-file-name "fixtures/edsl" (file-name-directory (or load-file-name buffer-file-name))))

(defun excali-dsl-test--source (name)
  "Return the text of fixture NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat name ".edsl") excali-dsl-test--fixtures))
    (buffer-string)))

(defun excali-dsl-test--fixture-names ()
  "Return the names of all fixtures."
  (mapcar #'file-name-base (directory-files excali-dsl-test--fixtures nil "\\.edsl\\'")))

(defun excali-dsl-test--statements (string kind)
  "Return the statements of KIND parsed from STRING."
  (seq-filter (lambda (s) (eq (car s) kind)) (cdr (excali-dsl-parse string))))

(defun excali-dsl-test--edges (model)
  "Return MODEL's edges as (FROM TO LABEL ARROW)."
  (mapcar (lambda (e) (list (excali-dsl--edge-from e) (excali-dsl--edge-to e)
                            (excali-dsl--edge-label e) (excali-dsl--edge-arrow e)))
          (plist-get model :edges)))

(defun excali-dsl-test--box (item)
  "Return ITEM's box (X1 Y1 X2 Y2)."
  (if (excali-dsl--node-p item)
      (list (excali-dsl--node-x item) (excali-dsl--node-y item)
            (+ (excali-dsl--node-x item) (excali-dsl--node-w item))
            (+ (excali-dsl--node-y item) (excali-dsl--node-h item)))
    (list (excali-dsl--cluster-x item) (excali-dsl--cluster-y item)
          (+ (excali-dsl--cluster-x item) (excali-dsl--cluster-w item))
          (+ (excali-dsl--cluster-y item) (excali-dsl--cluster-h item)))))

(defun excali-dsl-test--overlap-p (a b)
  "Return non-nil if boxes A and B overlap by more than a pixel."
  (and (< (+ (nth 0 a) 1) (nth 2 b)) (< (+ (nth 0 b) 1) (nth 2 a))
       (< (+ (nth 1 a) 1) (nth 3 b)) (< (+ (nth 1 b) 1) (nth 3 a))))

(defun excali-dsl-test--inside-p (inner outer)
  "Return non-nil if box INNER lies within box OUTER."
  (and (<= (nth 0 outer) (nth 0 inner)) (<= (nth 1 outer) (nth 1 inner))
       (>= (nth 2 outer) (nth 2 inner)) (>= (nth 3 outer) (nth 3 inner))))

;;;; Parsing

(ert-deftest excali-dsl-test-every-fixture-draws ()
  "Every upstream example parses, lays out and draws."
  (dolist (name (excali-dsl-test--fixture-names))
    (let ((elements (excali-dsl-elements (excali-dsl-test--source name))))
      (should (> (length elements) 0))
      (should (seq-some (lambda (e) (equal (alist-get 'type e) "arrow")) elements)))))

(ert-deftest excali-dsl-test-nodes ()
  "Node labels, types and attributes in both upstream syntaxes."
  (let ((nodes (excali-dsl-test--statements
                "a
b[Step one]
c \"Quoted\" @service
d[Styled] { backgroundColor: \"#ff6b6b\"; strokeWidth: 3 }
e[Typed] { type: database }
f [Spaced]"
                'node)))
    (should (equal (mapcar (lambda (n) (plist-get (cdr n) :id)) nodes) '("a" "b" "c" "d" "e" "f")))
    (should (null (plist-get (cdr (nth 0 nodes)) :label)))
    (should (equal (plist-get (cdr (nth 1 nodes)) :label) "Step one"))
    (should (equal (plist-get (cdr (nth 2 nodes)) :label) "Quoted"))
    (should (equal (plist-get (cdr (nth 2 nodes)) :type) "service"))
    (should (equal (plist-get (cdr (nth 3 nodes)) :attrs)
                   '(("backgroundColor" . "#ff6b6b") ("strokeWidth" . 3))))
    (should (equal (plist-get (cdr (nth 4 nodes)) :attrs) '(("type" . "database"))))
    (should (equal (plist-get (cdr (nth 5 nodes)) :label) "Spaced"))))

(ert-deftest excali-dsl-test-edges ()
  "Arrows, chains and the ways to label an edge."
  (let ((model (excali-dsl-model
                "a -> b
b <-> c: two way
c -- d {plain}
d ~> e : \"quoted: label\"
x -> y -> z: flow
p -> q \"first\" -> r
s[Start] -> t[End] { strokeStyle: dashed }
u -> v @orthogonal
w --- a")))
    (should (equal (excali-dsl-test--edges model)
                   '(("a" "b" nil "->") ("b" "c" "two way" "<->") ("c" "d" "plain" "--")
                     ("d" "e" "quoted: label" "~>")
                     ("x" "y" "flow" "->") ("y" "z" "flow" "->")
                     ("p" "q" "first" "->") ("q" "r" nil "->")
                     ("s" "t" nil "->") ("u" "v" nil "->") ("w" "a" nil "---"))))
    ;; Labels given in a chain define the nodes.
    (should (equal (excali-dsl--node-label (gethash "s" (plist-get model :nodes))) "Start"))
    (let ((edges (plist-get model :edges)))
      (should (equal (cdr (assoc "strokeStyle" (excali-dsl--edge-attrs (nth 8 edges)))) "dashed"))
      (should (equal (cdr (assoc "routing" (excali-dsl--edge-attrs (nth 9 edges)))) "orthogonal")))))

(ert-deftest excali-dsl-test-containers-and-groups ()
  "Containers and groups in upstream's and the older syntax, nested."
  (let* ((model (excali-dsl-model
                 "container \"Backend\" as backend {
  style: { backgroundColor: \"#eeeeee\" }
  api[API]
  container \"Data\" {
    db[DB]
  }
  api -> db
}
container front \"Frontend\" {
  strokeStyle: dashed
  ui
}
service \"Core\" { auth }
group team:people { alice }
flow \"Steps\" as steps { one -> two }
ui -> backend.api"))
         (clusters (plist-get model :clusters))
         (nodes (plist-get model :nodes))
         (parent (lambda (id) (excali-dsl--cluster-id (excali-dsl--node-parent (gethash id nodes))))))
    (should (gethash "backend" clusters))
    (should (equal (excali-dsl--cluster-label (gethash "backend" clusters)) "Backend"))
    (should (equal (cdr (assoc "backgroundColor" (excali-dsl--cluster-attrs (gethash "backend" clusters))))
                   "#eeeeee"))
    (should (equal (cdr (assoc "strokeStyle" (excali-dsl--cluster-attrs (gethash "front" clusters))))
                   "dashed"))
    (should (equal (funcall parent "api") "backend"))
    (should (equal (excali-dsl--cluster-id (excali-dsl--cluster-parent
                                            (excali-dsl--node-parent (gethash "db" nodes))))
                   "backend"))
    (should (equal (funcall parent "ui") "front"))
    (should (equal (excali-dsl--cluster-kind (excali-dsl--node-parent (gethash "auth" nodes))) "service"))
    (should (equal (excali-dsl--cluster-kind (gethash "team" clusters)) "people"))
    (should (equal (funcall parent "two") "steps"))
    ;; `backend.api' is the node `api' in `backend'.
    (should (member '("ui" "api" nil "->") (excali-dsl-test--edges model)))))

(ert-deftest excali-dsl-test-component-types ()
  "Component types from `componentType' and from the front matter."
  (let* ((model (excali-dsl-model
                 "---
component_types:
  database:
    backgroundColor: \"#fce4ec\"
    shape: ellipse
---
componentType service {
  shape: diamond;
  style { fill: \"#e8f5e9\"; strokeColor: \"#4caf50\"; }
}
a[A] { type: service }
b[B] @database
c[C] { type: service; strokeColor: \"#000000\" }"))
         (nodes (plist-get model :nodes))
         (elements (excali-dsl--draw model))
         (shape (lambda (id) (seq-find (lambda (e) (equal (alist-get 'edslId (alist-get 'customData e)) id))
                                       elements))))
    (should (equal (alist-get 'type (funcall shape "a")) "diamond"))
    (should (equal (alist-get 'backgroundColor (funcall shape "a")) "#e8f5e9"))
    (should (equal (alist-get 'type (funcall shape "b")) "ellipse"))
    (should (equal (alist-get 'backgroundColor (funcall shape "b")) "#fce4ec"))
    ;; The node's own attributes win.
    (should (equal (alist-get 'strokeColor (funcall shape "c")) "#000000"))
    (should (gethash "c" nodes))))

(ert-deftest excali-dsl-test-connection-blocks ()
  "`connection' and `connections' make styled edges."
  (let ((edges (plist-get (excali-dsl-model (excali-dsl-test--source "advanced-connections")) :edges)))
    (should (= (length edges) 9))
    (let ((cache (seq-find (lambda (e) (equal (excali-dsl--edge-to e) "cache")) edges)))
      (should (equal (excali-dsl--edge-label cache) "Cache"))
      (should (equal (cdr (assoc "strokeStyle" (excali-dsl--edge-attrs cache))) "dashed"))
      (should (equal (cdr (assoc "color" (excali-dsl--edge-attrs cache))) "#ff5722")))))

(ert-deftest excali-dsl-test-templates ()
  "Layer templates through `diagram', and front matter templates."
  (let ((model (excali-dsl-model (excali-dsl-test--source "microservices-template"))))
    (should (= (hash-table-count (plist-get model :clusters)) 4))
    (should (= (length (plist-get model :edges)) 19))
    (should (equal (plist-get model :title) "E-commerce Microservices")))
  (let* ((model (excali-dsl-model
                 "---
templates:
  microservice:
    api: \"$name API\"
    db: \"$name Database\"
    edges:
      - api -> db
---
microservice users {
  name: \"User\"
}
users.api -> users.db \"direct\""))
         (nodes (plist-get model :nodes)))
    (should (equal (excali-dsl--node-label (gethash "users.api" nodes)) "User API"))
    (should (equal (excali-dsl--node-label (gethash "users.db" nodes)) "User Database"))
    (should (member '("users.api" "users.db" nil "->") (excali-dsl-test--edges model)))
    (should (member '("users.api" "users.db" "direct" "->") (excali-dsl-test--edges model)))))

(ert-deftest excali-dsl-test-documented-examples ()
  "The examples of README.org and docs/dsl.md draw as described."
  (let* ((model (excali-dsl-model "---
direction: LR            # TB (default), BT, LR, RL
nodeSpacing: 60
rankSpacing: 90
---
componentType service {
  shape: rectangle;
  style { fill: \"#e3f2fd\"; strokeColor: \"#1976d2\"; }
}

client[Web Client]
container \"Backend\" as backend {
  api[API] { type: service }
  db[Database] { shape: ellipse; backgroundColor: \"#fce4ec\" }
  api -> db: query
}
client -> api: HTTPS
api -> cache -> db       # a chain; `cache' is made on first use
db <-> replica @orthogonal"))
         (nodes (plist-get model :nodes)))
    (should (equal (sort (hash-table-keys nodes) #'string<)
                   '("api" "cache" "client" "db" "replica")))
    (should (equal (excali-dsl--cluster-id (excali-dsl--node-parent (gethash "api" nodes))) "backend"))
    (should (= (length (plist-get model :edges)) 5))
    (excali-dsl-test--check-layout model))
  (let ((model (excali-dsl-model "template stack {
  layers {
    \"Clients\" { components: [\"Web\", \"Mobile\"]; layout: horizontal }
    \"Services\" { components: [\"API\"] }
  }
  connections { pattern: each-to-next-layer }
  layout { direction: top-to-bottom; spacing: { node_spacing: 50; layer_spacing: 120 } }
}
diagram \"My System\" { type: architecture; template: stack }
connection { from: \"web\"; to: \"api\"; style { type: dashed; label: \"HTTPS\"; color: \"#2196f3\"; width: 2 } }")))
    (should (= (hash-table-count (plist-get model :clusters)) 2))
    (should (= (length (plist-get model :edges)) 3))
    (should (equal (plist-get model :title) "My System"))))

(ert-deftest excali-dsl-test-yaml ()
  "The front matter subset: nesting, lists, scalars and comments."
  (should (equal (excali-dsl--parse-yaml
                  "layout: dagre  # the layout
title: \"A: B\"
layout_options:
  rankdir: LR
  nodesep: 50
flags: [a, b]
items:
  - one
  - \"two\"
dark: true")
                 '(("layout" . "dagre") ("title" . "A: B")
                   ("layout_options" . (("rankdir" . "LR") ("nodesep" . 50)))
                   ("flags" . ["a" "b"]) ("items" . ["one" "two"]) ("dark" . t)))))

(ert-deftest excali-dsl-test-errors-carry-lines ()
  "Errors name the line they happened on."
  (dolist (case '(("a\nb {\n  c: 1\n" . 4)
                  ("a\n}" . 2)
                  ("a\nb[B] { type: nope }" . 2)
                  ("---\nlayout: dagre\na -> b" . 1)
                  ("a -> \"b\"" . 1)))
    (let ((err (should-error (excali-dsl-model (car case)) :type 'excali-dsl-error)))
      (should (= (nth 1 err) (cdr case)))
      (should (stringp (nth 2 err))))))

;;;; Layout

(defun excali-dsl-test--check-layout (model)
  "Assert MODEL's layout invariants: no overlaps, clusters enclose members."
  (let (nodes clusters)
    (maphash (lambda (_ n) (push n nodes)) (plist-get model :nodes))
    (maphash (lambda (_ c) (push c clusters)) (plist-get model :clusters))
    ;; Nodes and sibling clusters do not overlap.
    (let ((children (lambda (cluster) (excali-dsl--cluster-children cluster))))
      (dolist (parent (cons (plist-get model :root) clusters))
        (let ((items (funcall children parent)))
          (cl-loop for (a . rest) on items
                   do (dolist (b rest)
                        (should-not (excali-dsl-test--overlap-p (excali-dsl-test--box a)
                                                                (excali-dsl-test--box b))))))))
    ;; Every node lies within each of its containers.
    (dolist (n nodes)
      (let ((c (excali-dsl--node-parent n)))
        (while (excali-dsl--cluster-parent c)
          (should (excali-dsl-test--inside-p (excali-dsl-test--box n) (excali-dsl-test--box c)))
          (setq c (excali-dsl--cluster-parent c)))))))

(ert-deftest excali-dsl-test-layout-invariants ()
  "No fixture overlaps nodes or lets them out of their containers."
  (dolist (name (excali-dsl-test--fixture-names))
    (excali-dsl-test--check-layout (excali-dsl-model (excali-dsl-test--source name)))))

(ert-deftest excali-dsl-test-layers-follow-edges ()
  "In an acyclic diagram each edge points down (TB) or right (LR)."
  (dolist (case '(("decision-tree" . cdr) ("complex-dag" . cdr) ("edge-chains" . cdr)
                  ("state-machine-simple" . car)))
    (let* ((model (excali-dsl-model (excali-dsl-test--source (car case))))
           (nodes (plist-get model :nodes))
           (center (lambda (id) (let ((n (gethash id nodes)))
                                  (cons (+ (excali-dsl--node-x n) (/ (excali-dsl--node-w n) 2))
                                        (+ (excali-dsl--node-y n) (/ (excali-dsl--node-h n) 2)))))))
      (dolist (e (plist-get model :edges))
        (let ((a (funcall (cdr case) (funcall center (excali-dsl--edge-from e))))
              (b (funcall (cdr case) (funcall center (excali-dsl--edge-to e)))))
          ;; state-machine-simple has one edge back to an earlier state.
          (unless (and (equal (car case) "state-machine-simple")
                       (equal (excali-dsl--edge-to e) "completed"))
            (should (< a b))))))))

(ert-deftest excali-dsl-test-direction ()
  "LR puts successive layers side by side, BT upward."
  (let* ((lr (plist-get (excali-dsl-model "---\ndirection: LR\n---\na -> b") :nodes))
         (bt (plist-get (excali-dsl-model "---\nlayout_options:\n  rankdir: BT\n---\na -> b") :nodes)))
    (should (< (excali-dsl--node-x (gethash "a" lr)) (excali-dsl--node-x (gethash "b" lr))))
    (should (= (excali-dsl--node-y (gethash "a" lr)) (excali-dsl--node-y (gethash "b" lr))))
    (should (> (excali-dsl--node-y (gethash "a" bt)) (excali-dsl--node-y (gethash "b" bt))))))

(ert-deftest excali-dsl-test-labels-fit ()
  "Shapes are at least as large as their labels, even if sized smaller."
  (let* ((model (excali-dsl-model "tiny[A rather long label] { width: 40; height: 40 }
big[Big] { width: 300; height: 100 }"))
         (nodes (plist-get model :nodes))
         (font (plist-get (excali-dsl--defaults nil) :font))
         (text-w (car (excali--measure-string "A rather long label" 20 font (excali--line-height font)))))
    (should (> (excali-dsl--node-w (gethash "tiny" nodes)) text-w))
    (should (= (excali-dsl--node-w (gethash "big" nodes)) 300))
    (should (= (excali-dsl--node-h (gethash "big" nodes)) 100))))

(ert-deftest excali-dsl-test-layout-graph ()
  "The layered layout keeps nodes apart and bends long edges."
  (pcase-let* ((sizes (vector '(100 . 50) '(100 . 50) '(100 . 50) '(100 . 50)))
               (`(,positions . ,waypoints)
                (excali-dsl-layout-graph sizes '((0 . 1) (1 . 2) (0 . 2) (2 . 0) (3 . 3)))))
    (should (= (length positions) 4))
    (should (< (cdr (aref positions 0)) (cdr (aref positions 1))))
    (should (< (cdr (aref positions 1)) (cdr (aref positions 2))))
    ;; 0 -> 2 skips a layer, and so does the back edge 2 -> 0.
    (should (= (length (nth 2 waypoints)) 1))
    (should (= (length (nth 3 waypoints)) 1))
    (should (null (nth 4 waypoints)))
    (should (equal (seq-min (mapcar #'car positions)) 0.0)))
  (should (equal (excali-dsl-layout--isotonic '(3 1 2 5 4)) '(2.0 2.0 2.0 4.5 4.5)))
  (should (equal (append (excali-dsl-layout-grid (vector '(10 . 10) '(20 . 10) '(10 . 30)) 2 5) nil)
                 '((0.0 . 0.0) (15.0 . 0.0) (0.0 . 15.0)))))

;;;; Drawing

(defun excali-dsl-test--by-dsl-id (elements id)
  "Return the element of ELEMENTS drawn for DSL ID."
  (seq-find (lambda (e) (equal (alist-get 'edslId (alist-get 'customData e)) id)) elements))

(ert-deftest excali-dsl-test-arrows-bind-and-labels-bind ()
  "Arrows bind to their nodes; labels bind to shapes and arrows."
  (let* ((elements (excali-dsl-elements "a[Alpha] -> b[Beta]: go
b <-> c
c -- a @orthogonal"))
         (by-id (lambda (id) (seq-find (lambda (e) (equal (alist-get 'id e) id)) elements)))
         (a (excali-dsl-test--by-dsl-id elements "a"))
         (b (excali-dsl-test--by-dsl-id elements "b"))
         (ab (excali-dsl-test--by-dsl-id elements "a->b"))
         (bc (excali-dsl-test--by-dsl-id elements "b->c"))
         (ca (excali-dsl-test--by-dsl-id elements "c->a")))
    (should (equal (alist-get 'elementId (alist-get 'startBinding ab)) (alist-get 'id a)))
    (should (equal (alist-get 'elementId (alist-get 'endBinding ab)) (alist-get 'id b)))
    (should (seq-some (lambda (x) (equal (alist-get 'id x) (alist-get 'id ab)))
                      (alist-get 'boundElements a)))
    ;; Arrowheads follow the operator.
    (should (equal (alist-get 'endArrowhead ab) "arrow"))
    (should (eq (alist-get 'startArrowhead ab) :null))
    (should (equal (alist-get 'startArrowhead bc) "arrow"))
    (should (eq (alist-get 'endArrowhead ca) :null))
    (should (eq (alist-get 'elbowed ca) t))
    ;; The node's label and the arrow's.
    (let ((label (funcall by-id (alist-get 'id (seq-find (lambda (x) (equal (alist-get 'type x) "text"))
                                                        (alist-get 'boundElements a))))))
      (should (equal (alist-get 'text label) "Alpha"))
      (should (equal (alist-get 'containerId label) (alist-get 'id a))))
    (let ((label (funcall by-id (alist-get 'id (seq-find (lambda (x) (equal (alist-get 'type x) "text"))
                                                        (alist-get 'boundElements ab))))))
      (should (equal (alist-get 'text label) "go")))))

(ert-deftest excali-dsl-test-edges-go-around-shapes ()
  "A straight edge bends around a shape in its way."
  (let* ((elements (excali-dsl-elements "container \"C\" { a -> b }
top -> b"))
         (b (excali-dsl-test--by-dsl-id elements "b"))
         (a (excali-dsl-test--by-dsl-id elements "a"))
         (arrow (excali-dsl-test--by-dsl-id elements "top->b"))
         (x (alist-get 'x arrow)) (y (alist-get 'y arrow))
         (points (mapcar (lambda (p) (cons (+ x (aref p 0)) (+ y (aref p 1))))
                         (alist-get 'points arrow)))
         (box (excali-dsl--box a)))
    (should (alist-get 'elementId (alist-get 'endBinding arrow)))
    (should (equal (alist-get 'elementId (alist-get 'endBinding arrow)) (alist-get 'id b)))
    (should (> (length points) 2))
    (cl-loop for (p q) on points while q
             do (should-not (excali-dsl--segment-hits-box-p p q box)))))

(ert-deftest excali-dsl-test-scene-round-trips ()
  "A drawn scene saves and loads back with its bindings."
  (let* ((doc (excali-dsl-scene (excali-dsl-test--source "nested-containers")))
         (file (make-temp-file "excali-dsl" nil ".excalidraw")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (excali--serialize-doc doc (append (alist-get 'elements doc) nil))))
          (let* ((back (excali--restore-doc (excali--read-file file)))
                 (before (alist-get 'elements doc))
                 (after (alist-get 'elements back)))
            (should (= (length before) (length after)))
            (should (equal (mapcar (lambda (e) (alist-get 'id e)) before)
                           (mapcar (lambda (e) (alist-get 'id e)) after)))
            (seq-doseq (e after)
              (should (stringp (alist-get 'index e)))
              (when (equal (alist-get 'type e) "arrow")
                (should (alist-get 'elementId (alist-get 'startBinding e)))
                (should (alist-get 'elementId (alist-get 'endBinding e)))))))
      (delete-file file))))

(ert-deftest excali-dsl-test-scene-app-state ()
  "Front matter theme and background go into the app state."
  (let ((doc (excali-dsl-scene "---\nbackground_color: \"#fdf8f6\"\n---\na")))
    (should (equal (alist-get 'viewBackgroundColor (alist-get 'appState doc)) "#fdf8f6"))))

(ert-deftest excali-dsl-test-insert-centers-and-selects ()
  "Inserting a diagram centers it in the view and selects it."
  (excali-test--in-window
   (setq excali--elements nil
         excali--canvas-size '(800 . 600) excali--pixel-scale 1.0)
   (let* ((inserted (excali-dsl-insert "a -> b"))
          (bounds (excali--elements-bounds inserted)))
     (should (= (length excali--elements) (length inserted)))
     (should (< (abs (- (/ (+ (nth 0 bounds) (nth 2 bounds)) 2) 400)) 1))
     (should (< (abs (- (/ (+ (nth 1 bounds) (nth 3 bounds)) 2) 300)) 1))
     (should excali--selection)
     (should (seq-every-p (lambda (e) (stringp (alist-get 'index e))) excali--elements)))))

;;;; The mode

(ert-deftest excali-dsl-test-mode-indents-and-fontifies ()
  "The mode indents by brace depth and fontifies without errors."
  (with-temp-buffer
    (insert "container \"A\" {\nx -> y: go\ncontainer \"B\" {\nz\n}\n}\n")
    (excali-dsl-mode)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "container \"A\" {\n  x -> y: go\n  container \"B\" {\n    z\n  }\n}\n"))
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))
    (should (eq (assoc-default "x.edsl" auto-mode-alist #'string-match) 'excali-dsl-mode))))

(provide 'excali-dsl-test)
;;; excali-dsl-test.el ends here
