;;; excali-dsl.el --- Draw scenes from a text DSL  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Excali DSL (.excalidsl) describes nodes, relative placement, bound arrows
;; and explicit style blocks.  See docs/excali-dsl.md for the language.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'excali-core)
(require 'excali-text)
(require 'excali-binding)
(require 'excali-elbow)
(require 'excali-restore)
(require 'excali-dsl-parser)
(require 'excali-dsl-constraints)

(declare-function excali--open "excali")
(declare-function excali--insert-elements "excali-clipboard")
(declare-function excali--view-center "excali-select")
(declare-function excali--elements-bounds "excali-select")
(declare-function excali--render "excali-view")
(declare-function excali-zoom-to-fit "excali-actions")
(declare-function excali--history-reset "excali-history")
(defvar excali--theme)
(defvar excali--file)
(defvar excali--doc)

(defgroup excali-dsl nil
  "Drawing excali scenes from text."
  :group 'excali)

(defcustom excali-dsl-render-on-save nil
  "Non-nil means saving an .excalidsl buffer draws it again, if drawn before."
  :type 'boolean)

(defcustom excali-dsl-indent-offset 2
  "Indentation step of `excali-dsl-mode'."
  :type 'integer)

;;;; The model

(cl-defstruct (excali-dsl--node (:constructor excali-dsl--make-node))
  id label type attrs parent line x y w h element)

(cl-defstruct (excali-dsl--cluster (:constructor excali-dsl--make-cluster))
  id label kind attrs parent children line x y w h element label-element)

(cl-defstruct (excali-dsl--edge (:constructor excali-dsl--make-edge))
  from to arrow label attrs parent line waypoints)

;;;; Styles

(defun excali-dsl--attr (attrs &rest keys)
  "Return the first of KEYS found in ATTRS."
  (cl-loop for k in keys for v = (cdr (assoc k attrs)) when v return v))

(defun excali-dsl--color (value)
  "Return VALUE as an Excalidraw color string, or nil."
  (when value
    (let ((s (format "%s" value)))
      (cond ((string-match "\\`#[[:xdigit:]]+\\'" s) (downcase s))
            ((string-equal s "transparent") s)
            ((string-match "\\`rgba?(\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\)" s)
             (format "#%02x%02x%02x" (string-to-number (match-string 1 s))
                     (string-to-number (match-string 2 s)) (string-to-number (match-string 3 s))))
            ((and (fboundp 'color-name-to-rgb) (ignore-errors (color-name-to-rgb s)))
             (apply #'format "#%02x%02x%02x"
                    (mapcar (lambda (c) (round (* 255 c))) (color-name-to-rgb s))))
            (t s)))))

(defun excali-dsl--font (value default)
  "Return the font family id VALUE names, or DEFAULT."
  (let ((name (and value (format "%s" value))))
    (cond ((null name) default)
          ((numberp value) value)
          ((cdr (assoc-string name excali-font-family-ids t)))
          ((member (downcase name) '("hand-drawn" "handdrawn")) 5)
          ((equal (downcase name) "normal") 6)
          ((equal (downcase name) "code") 8)
          (t default))))

(defun excali-dsl--arrowhead (value default)
  "Return the Excalidraw arrowhead VALUE names, or DEFAULT."
  (pcase (and value (downcase (format "%s" value)))
    ('nil default)
    ((or "none" "null" "false") :null)
    ((or "dot" "circle") "circle")
    ("triangle" "triangle") ("diamond" "diamond") ("bar" "bar") ("arrow" "arrow")
    (name name)))

(defun excali-dsl--defaults (_config)
  "Return stable language defaults, independent of interactive preferences."
  '(:roughness 1 :stroke-width 2 :font 5 :font-size 20 :routing "straight"))

(defun excali-dsl--shape (node)
  "Return the element type NODE is drawn as."
  (pcase (and (excali-dsl--attr (excali-dsl--node-attrs node) "shape")
              (downcase (format "%s" (excali-dsl--attr (excali-dsl--node-attrs node) "shape"))))
    ((or "ellipse" "circle" "cylinder" "oval") "ellipse")
    ((or "diamond" "rhombus" "decision") "diamond")
    ("text" "text")
    (_ "rectangle")))

;;;; Sizes

(defun excali-dsl--label (node)
  "Return NODE's label: its label or its id."
  (or (excali-dsl--node-label node) (excali-dsl--node-id node)))

(defun excali-dsl--node-size (node defaults)
  "Return (WIDTH . HEIGHT) for NODE, fitting its label, given DEFAULTS."
  (let* ((attrs (excali-dsl--node-attrs node))
         (size (or (excali-dsl--attr attrs "fontSize" "font_size") (plist-get defaults :font-size)))
         (family (excali-dsl--font (excali-dsl--attr attrs "font") (plist-get defaults :font)))
         (text (excali--normalize-text (excali-dsl--label node)))
         (measured (excali--measure-string text size family (excali--line-height family)))
         (shape (excali-dsl--shape node))
         (w (excali-dsl--attr attrs "width")) (h (excali-dsl--attr attrs "height")))
    (if (equal shape "text")
        (cons (float (or w (car measured))) (float (or h (cdr measured))))
      ;; An explicit size is kept unless the label would not fit.
      (let ((fit-w (excali--container-dimension-for-text (car measured) shape))
            (fit-h (excali--container-dimension-for-text (cdr measured) shape)))
        (cons (float (if (numberp w) (max w fit-w)
                       (max 120 (excali--container-dimension-for-text (+ (car measured) 30) shape))))
              (float (if (numberp h) (max h fit-h)
                       (max 60 (excali--container-dimension-for-text (+ (cdr measured) 20) shape)))))))))

(defun excali-dsl--item (id model)
  "Return the node or cluster ID names in MODEL."
  (or (gethash id (plist-get model :nodes)) (gethash id (plist-get model :clusters))))

(defun excali-dsl--item-size (item)
  "Return ITEM's (WIDTH . HEIGHT)."
  (if (excali-dsl--node-p item)
      (cons (excali-dsl--node-w item) (excali-dsl--node-h item))
    (cons (excali-dsl--cluster-w item) (excali-dsl--cluster-h item))))

(defun excali-dsl--set-item-position (item x y)
  "Place ITEM's top-left at X, Y relative to its parent's content."
  (if (excali-dsl--node-p item)
      (setf (excali-dsl--node-x item) x (excali-dsl--node-y item) y)
    (setf (excali-dsl--cluster-x item) x (excali-dsl--cluster-y item) y)))


;;;; Model and relative layout

(defconst excali-dsl--cluster-padding 30 "Container content padding.")
(defconst excali-dsl--cluster-label-size 20 "Default container label size.")
(defconst excali-dsl--gap 80 "Minimum gap in a directional placement.")

(defun excali-dsl--build (statements)
  "Validate STATEMENTS and build the native drawing model."
  (let ((styles (make-hash-table :test #'equal))
        (defaults (make-hash-table :test #'equal))
        (declarations (make-hash-table :test #'equal))
        (nodes (make-hash-table :test #'equal))
        (clusters (make-hash-table :test #'equal))
        (root (excali-dsl--make-cluster :kind "root"))
        (pairs (make-hash-table :test #'equal)) edges ordered)
    (dolist (s statements)
      (let ((kind (plist-get s :kind)) (id (plist-get s :id))
            (tok (plist-get s :token)) (props (plist-get s :props)))
        (pcase kind
          ((or "style" "default")
           (when (and (equal kind "style") (string-match-p "\\." id))
             (excali-dsl--fail tok "Style names must have one segment"))
           (when (and (equal kind "default") (not (member id '("node" "edge"))))
             (excali-dsl--fail tok "Default must target node or edge"))
           (let ((table (if (equal kind "style") styles defaults)))
             (when (gethash id table) (excali-dsl--fail tok "Duplicate %s %s" kind id))
             (excali-dsl--validate-props props (if (equal kind "style") 'both (intern id)))
             (puthash id s table)))
          ("node"
           (when (gethash id declarations) (excali-dsl--fail tok "Duplicate node %s" id))
           (when (string-match "\\`\\(.*\\)\\.[^.]+\\'" id)
             (unless (gethash (match-string 1 id) declarations)
               (excali-dsl--fail tok "Declare parent %s before its child" (match-string 1 id))))
           (puthash id s declarations) (push s ordered)))))
    (setq ordered (nreverse ordered))
    (cl-labels
        ((attributes (s)
           (let* ((kind (intern (plist-get s :kind))) (own (plist-get s :props))
                  (ref (assoc "style" own)) (named (and ref (gethash (cadr ref) styles)))
                  (base (plist-get (gethash (symbol-name kind) defaults) :props)))
             (when (and ref (not named))
               (excali-dsl--fail (nth 2 ref) "Unknown style %s" (cadr ref)))
             (excali-dsl--validate-props own kind t)
             (excali-dsl--validate-props (plist-get named :props) kind)
             ;; Earlier entries win: inline > named > document defaults.
             (append (excali-dsl--native-props own)
                     (excali-dsl--native-props (plist-get named :props))
                     (excali-dsl--native-props base)
                     '(("strokeColor" . "#1e1e1e") ("backgroundColor" . "transparent")
                       ("strokeWidth" . 2) ("opacity" . 100) ("font" . 5)
                       ("fontSize" . 20) ("textColor" . "#1e1e1e")))))
         (reference (token)
           (unless (gethash (cadr token) declarations)
             (excali-dsl--fail token "Unknown node %s" (cadr token)))))
      ;; Discover containers before constructing items, preserving source order.
      (dolist (s ordered)
        (let ((id (plist-get s :id)))
          (when (string-match "\\`\\(.*\\)\\.[^.]+\\'" id)
            (puthash (match-string 1 id) t clusters))))
      (dolist (s ordered)
        (let* ((id (plist-get s :id))
               (parent (if (string-match "\\`\\(.*\\)\\.[^.]+\\'" id)
                           (gethash (match-string 1 id) clusters) root))
               (attrs (attributes s))
               (label (if (plist-member s :label) (plist-get s :label) (car (last (split-string id "\\.")))))
               (container (gethash id clusters))
               (shape (or (cdr (assoc "shape" attrs)) "rectangle"))
               (item (if container
                         (excali-dsl--make-cluster :id id :label label :kind "container"
                                                  :parent parent :attrs attrs :line (plist-get s :line))
                       (excali-dsl--make-node :id id :label label :attrs attrs
                                             :parent parent :line (plist-get s :line)))))
          (when (and container (not (equal shape "rectangle")))
            (excali-dsl--fail (plist-get s :token) "Container %s must be rectangular" id))
          (when (and (equal shape "ellipse") (assoc "roundness" attrs))
            (excali-dsl--fail (plist-get s :token) "roundness is not valid on an ellipse"))
          (dolist (p (plist-get s :placements)) (reference (nth 1 p)))
          (puthash id item (if container clusters nodes))
          (setf (excali-dsl--cluster-children parent)
                (append (excali-dsl--cluster-children parent) (list item)))))
      (dolist (s statements)
        (when (equal (plist-get s :kind) "edge")
          (let* ((from (plist-get s :id)) (target (plist-get s :target))
                 (to (cadr target)) (tok (plist-get s :token)) (pair (cons from to)))
            (reference (list 'word from (nth 2 tok) (nth 3 tok))) (reference target)
            (when (equal from to) (excali-dsl--fail tok "Self-loop edges are not supported"))
            (when (gethash pair pairs) (excali-dsl--fail tok "Duplicate edge %s -> %s" from to))
            (puthash pair t pairs)
            (push (excali-dsl--make-edge
                   :from from :to to :arrow "->" :label (plist-get s :label)
                   :line (plist-get s :line) :parent root
                   :attrs (append (plist-get s :sides) (attributes s))) edges)))))
    (list :root root :nodes nodes :clusters clusters :edges (nreverse edges)
          :statements ordered)))

(defun excali-dsl--layout (model)
  "Jointly solve relative positions and container sizes in MODEL."
  (let* ((statements (plist-get model :statements)) (count (length statements))
         (n (* 4 count)) (indices (make-hash-table :test #'equal))
         (links (make-hash-table :test #'equal)) (previous (make-hash-table :test #'equal))
         (objective (make-vector n -1.0))
         (clearances (make-hash-table :test #'equal)) constraints)
    (cl-loop for s in statements for i from 0 do (puthash (plist-get s :id) (* i 4) indices))
    (dolist (edge (plist-get model :edges))
      (when-let* ((label (excali-dsl--edge-label edge)))
        (let* ((attrs (excali-dsl--edge-attrs edge))
               (font (cdr (assoc "font" attrs)))
               (size (excali--measure-string (excali--normalize-text label) (cdr (assoc "fontSize" attrs)) font
                                            (excali--line-height font)))
               (a (gethash (excali-dsl--edge-from edge) indices))
               (b (gethash (excali-dsl--edge-to edge) indices)))
          (puthash (cons a b) size clearances)
          (puthash (cons b a) size clearances))))
    (cl-labels
        ((link (a b) (push b (gethash a links)) (push a (gethash b links)))
         (bound (terms rhs)
           (let ((v (make-vector n 0.0)))
             (dolist (term terms) (cl-incf (aref v (car term)) (cdr term)))
             (push (cons v rhs) constraints)))
         (eqn (terms rhs)
           (bound terms rhs)
           (bound (mapcar (lambda (p) (cons (car p) (- (cdr p)))) terms) (- rhs)))
         (align (a b axis)
           (let ((offset (if (eq axis 'x) 0 2)))
             (eqn (list (cons (+ a offset) 1) (cons (+ a offset 1) 1)
                        (cons (+ b offset) -1) (cons (+ b offset 1) -1)) 0)))
         (place (a b op)
           (let* ((label (gethash (cons a b) clearances))
                  (gap (max excali-dsl--gap
                            (+ 24 (or (if (member op '("left" "right")) (car label) (cdr label)) 0)))))
             (pcase op
               ("right" (bound (list (cons (+ b 1) 1) (cons a -1)) (- gap)))
               ("left" (bound (list (cons (+ a 1) 1) (cons b -1)) (- gap)))
               ("below" (bound (list (cons (+ b 3) 1) (cons (+ a 2) -1)) (- gap)))
               ("above" (bound (list (cons (+ a 3) 1) (cons (+ b 2) -1)) (- gap)))
               ("level" (align a b 'y))))))
      (dolist (s statements)
        (let* ((id (plist-get s :id)) (a (gethash id indices)) (item (excali-dsl--item id model))
               (leaf (excali-dsl--node-p item))
               (parent (if leaf (excali-dsl--node-parent item) (excali-dsl--cluster-parent item)))
               (pid (excali-dsl--cluster-id parent))
               (p (and pid (gethash pid indices)))
               (placements (plist-get s :placements))
               (size (if leaf (excali-dsl--node-size item (excali-dsl--defaults nil))
                       (let* ((attrs (excali-dsl--cluster-attrs item))
                              (font (cdr (assoc "font" attrs))) (fs (cdr (assoc "fontSize" attrs)))
                              (extent (excali--measure-string (excali--normalize-text (excali-dsl--cluster-label item)) fs font
                                                              (excali--line-height font))))
                         (cons (+ 60 (car extent)) (+ 60 (cdr extent)))))))
          ;; Leaves have fixed text-derived sizes.  Containers grow as needed.
          (funcall (if leaf #'eqn #'bound) (list (cons a 1) (cons (1+ a) -1)) (- (car size)))
          (funcall (if leaf #'eqn #'bound) (list (cons (+ a 2) 1) (cons (+ a 3) -1)) (- (cdr size)))
          (when p
            (link id pid)
            (let* ((attrs (excali-dsl--cluster-attrs parent))
                   (fs (cdr (assoc "fontSize" attrs))) (font (cdr (assoc "font" attrs)))
                   (height (cdr (excali--measure-string (excali--normalize-text (excali-dsl--cluster-label parent)) fs font
                                                        (excali--line-height font)))))
              (bound (list (cons p 1) (cons a -1)) -30)
              (bound (list (cons (+ a 1) 1) (cons (+ p 1) -1)) -30)
              (bound (list (cons (+ p 2) 1) (cons (+ a 2) -1)) (- (+ 45 height)))
              (bound (list (cons (+ a 3) 1) (cons (+ p 3) -1)) -30)))
          (when (and pid (null placements) (gethash pid previous))
            (let* ((other (gethash pid previous)) (b (gethash other indices)))
              (place a b "below") (align a b 'x) (link id other)))
          (puthash pid id previous)
          (let ((horizontal (seq-filter (lambda (q) (member (car q) '("left" "right"))) placements))
                (vertical (seq-filter (lambda (q) (member (car q) '("above" "below" "level"))) placements)))
            (when (or (and (> (length horizontal) 1) (null vertical))
                      (and (> (length vertical) 1) (null horizontal)))
              (excali-dsl--fail (plist-get s :token) "Ambiguous alignment; specify both axes"))
            (dolist (q placements)
              (let* ((ref (cadr (nth 1 q))) (b (gethash ref indices)))
                (link id ref) (place a b (car q))))
            (when (and (= (length horizontal) 1) (null vertical))
              (align a (gethash (cadr (nth 1 (car horizontal))) indices) 'y))
            (when (and (= (length vertical) 1) (null horizontal)
                       (not (equal (caar vertical) "level")))
              (align a (gethash (cadr (nth 1 (car vertical))) indices) 'x)))))
      ;; Edges never provide layout connectivity.
      (when statements
        (let ((seen (make-hash-table :test #'equal)) (queue (list (plist-get (car statements) :id))))
          (while queue
            (let ((id (pop queue)))
              (unless (gethash id seen)
                (puthash id t seen) (setq queue (append (gethash id links) queue)))))
          (dolist (s statements)
            (unless (gethash (plist-get s :id) seen)
              (excali-dsl--fail (plist-get s :token) "Disconnected node %s; add a placement" (plist-get s :id))))))
      (let ((solution (excali-dsl--linear-solve (nreverse constraints) objective)))
        (unless solution
          (excali-dsl--fail (plist-get (car statements) :token)
                            "Conflicting or cyclic placement/containment constraints (nodes: %s)"
                            (mapconcat (lambda (s) (format "%s@%d" (plist-get s :id) (plist-get s :line))) statements ", ")))
        (dolist (s statements)
          (let* ((id (plist-get s :id)) (i (gethash id indices)) (item (excali-dsl--item id model))
                 (x (aref solution i)) (y (aref solution (+ i 2)))
                 (w (- (aref solution (+ i 1)) x)) (h (- (aref solution (+ i 3)) y)))
            (excali-dsl--set-item-position item x y)
            (if (excali-dsl--node-p item)
                (setf (excali-dsl--node-w item) w (excali-dsl--node-h item) h)
              (setf (excali-dsl--cluster-w item) w (excali-dsl--cluster-h item) h)))))))
  model)

(defconst excali-dsl--group-colors
  '(("group" "#6b7280" "#f3f4f6") ("flow" "#3b82f6" "#dbeafe")
    ("service" "#8b5cf6" "#f3e8ff") ("layer" "#f59e0b" "#fef3c7")
    ("component" "#10b981" "#d1fae5") ("subsystem" "#ef4444" "#fee2e2")
    ("zone" "#06b6d4" "#cffafe") ("cluster" "#ec4899" "#fce7f3"))
  "Default stroke and background colors of groups by kind, as upstream.")

(defun excali-dsl--custom-data (id)
  "Return the customData marking an element as drawn for DSL ID."
  (list (cons 'excaliDslId id)))

(defun excali-dsl--add (element)
  "Put ELEMENT on top of the scene being drawn and return it."
  (when-let* ((id (alist-get 'excaliDslId (excali--get element 'customData))))
    (excali--put element 'seed (1+ (mod (string-to-number (substring (secure-hash 'sha256 id) 0 8) 16) 2147483646))))
  (setq excali--elements (append excali--elements (list element)))
  element)

(defun excali-dsl--draw-cluster (cluster defaults)
  "Draw CLUSTER's box and label, then its children's clusters.
DEFAULTS is the default style."
  (when (excali-dsl--cluster-parent cluster)
    (let* ((attrs (excali-dsl--cluster-attrs cluster))
           (kind (excali-dsl--cluster-kind cluster))
           (colors (cdr (assoc kind excali-dsl--group-colors)))
           (container (equal kind "container"))
           (group-id (excali--new-id))
           (box (excali-dsl--add
                 (excali--make-element
                  "rectangle" (excali-dsl--cluster-x cluster) (excali-dsl--cluster-y cluster)
                  (cons 'width (float (excali-dsl--cluster-w cluster)))
                  (cons 'height (float (excali-dsl--cluster-h cluster)))
                  (cons 'strokeColor (or (excali-dsl--color (excali-dsl--attr attrs "strokeColor" "color"))
                                         (if container "#868e96" (or (car colors) "#6b7280"))))
                  (cons 'backgroundColor (or (excali-dsl--color (excali-dsl--attr attrs "backgroundColor" "fill"))
                                             (if container "#f8f9fa" (or (cadr colors) "#f3f4f6"))))
                  (cons 'fillStyle (format "%s" (or (excali-dsl--attr attrs "fillStyle") "solid")))
                  (cons 'strokeWidth (or (excali-dsl--attr attrs "strokeWidth") (if container 1 2)))
                  (cons 'strokeStyle (format "%s" (or (excali-dsl--attr attrs "strokeStyle")
                                                      (if (equal kind "flow") "dashed" "solid"))))
                  (cons 'roughness (or (excali-dsl--attr attrs "roughness") (plist-get defaults :roughness)))
                  (cons 'opacity (or (excali-dsl--attr attrs "opacity") (if container 50 30)))
                  (cons 'roundness (if (equal (excali-dsl--attr attrs "roundness") 0) :null '((type . 3))))
                  (cons 'groupIds (vector group-id))
                  (cons 'customData (excali-dsl--custom-data (excali-dsl--cluster-id cluster)))))))
      (setf (excali-dsl--cluster-element cluster) box)
      (when-let* ((label (excali-dsl--cluster-label cluster)))
        (let ((font (excali-dsl--font (excali-dsl--attr attrs "font") (plist-get defaults :font))))
          (setf (excali-dsl--cluster-label-element cluster)
                (excali-dsl--add
                 (excali--make-text-element
                  (+ (excali-dsl--cluster-x cluster) excali-dsl--cluster-padding
                     (* (pcase (excali-dsl--attr attrs "textAlign") ("center" 0.5) ("right" 1.0) (_ 0.0))
                        (car (excali--measure-string (excali--normalize-text label) (excali-dsl--attr attrs "fontSize") font
                                                    (excali--line-height font)))))
                  (+ (excali-dsl--cluster-y cluster) (/ excali-dsl--cluster-padding 2.0))
                  label
                  (cons 'fontSize (or (excali-dsl--attr attrs "fontSize") excali-dsl--cluster-label-size))
                  (cons 'fontFamily font)
                  (cons 'opacity (or (excali-dsl--attr attrs "opacity") 100))
                  (cons 'textAlign (or (excali-dsl--attr attrs "textAlign") "left"))
                  (cons 'strokeColor (or (excali-dsl--color (excali-dsl--attr attrs "textColor"))
                                         (if container "#495057" (or (car colors) "#495057"))))
                  (cons 'groupIds (vector group-id)))))))))
  (dolist (child (excali-dsl--cluster-children cluster))
    (unless (excali-dsl--node-p child)
      (excali-dsl--draw-cluster child defaults))))

(defun excali-dsl--draw-node (node defaults)
  "Draw NODE and its label with DEFAULTS; return the shape."
  (let* ((attrs (excali-dsl--node-attrs node))
         (shape (excali-dsl--shape node))
         (font (excali-dsl--font (excali-dsl--attr attrs "font") (plist-get defaults :font)))
         (size (or (excali-dsl--attr attrs "fontSize" "font_size") (plist-get defaults :font-size)))
         (text-color (excali-dsl--color (excali-dsl--attr attrs "textColor" "color")))
         (label (excali-dsl--label node))
         (element
          (if (equal shape "text")
              (excali-dsl--add
               (excali--make-text-element
                (+ (excali-dsl--node-x node) (/ (excali-dsl--node-w node) 2.0))
                (+ (excali-dsl--node-y node) (/ (excali-dsl--node-h node) 2.0))
                label
                (cons 'textAlign "center") (cons 'verticalAlign "middle")
                (cons 'fontSize size) (cons 'fontFamily font)
                (cons 'strokeColor (or text-color "#1e1e1e"))
                (cons 'customData (excali-dsl--custom-data (excali-dsl--node-id node)))))
            (let* ((background (excali-dsl--color (excali-dsl--attr attrs "backgroundColor" "fill")))
                   (fill (excali-dsl--attr attrs "fillStyle" "fill"))
                   (rounded (excali-dsl--attr attrs "roundness" "rounded"))
                   (element
                    (excali-dsl--add
                     (excali--make-element
                      shape (excali-dsl--node-x node) (excali-dsl--node-y node)
                      (cons 'width (float (excali-dsl--node-w node)))
                      (cons 'height (float (excali-dsl--node-h node)))
                      (cons 'strokeColor (or (excali-dsl--color (excali-dsl--attr attrs "strokeColor"))
                                             "#1e1e1e"))
                      (cons 'backgroundColor (if (and background (string-prefix-p "#" background))
                                                 background "transparent"))
                      (cons 'fillStyle (if (and (stringp fill)
                                                (member fill '("hachure" "cross-hatch" "solid" "zigzag")))
                                           fill "solid"))
                      (cons 'strokeWidth (or (excali-dsl--attr attrs "strokeWidth")
                                             (plist-get defaults :stroke-width)))
                      (cons 'strokeStyle (format "%s" (or (excali-dsl--attr attrs "strokeStyle") "solid")))
                      (cons 'roughness (let ((r (excali-dsl--attr attrs "roughness")))
                                         (if (numberp r) (min 2 (max 0 r)) (plist-get defaults :roughness))))
                      (cons 'opacity (or (excali-dsl--attr attrs "opacity") 100))
                      (cons 'roundness (if (or (equal shape "ellipse") (and (numberp rounded) (<= rounded 0)))
                                           :null
                                         (list (cons 'type (if (equal shape "rectangle") 3 2)))))
                      (cons 'customData (excali-dsl--custom-data (excali-dsl--node-id node)))))))
              (unless (string-empty-p label)
                (let ((text (excali--add-bound-text
                             element (cons 'fontSize size) (cons 'fontFamily font)
                             (cons 'lineHeight (excali--line-height font))
                             (cons 'opacity (or (excali-dsl--attr attrs "opacity") 100))
                             (cons 'textAlign (or (excali-dsl--attr attrs "textAlign") "center"))
                             (cons 'strokeColor (or text-color "#1e1e1e")))))
                  (excali--set-text text label)))
              element))))
    (setf (excali-dsl--node-element node) element)))

(defun excali-dsl--element-center (element)
  "Return the center of ELEMENT's box."
  (cons (+ (excali--get element 'x) (/ (excali--get element 'width) 2.0))
        (+ (excali--get element 'y) (/ (excali--get element 'height) 2.0))))

(defun excali-dsl--side-point (element toward)
  "Return the middle of ELEMENT's side facing the point TOWARD."
  (pcase-let* ((`(,cx . ,cy) (excali-dsl--element-center element))
               (dx (- (car toward) cx)) (dy (- (cdr toward) cy))
               (w (/ (excali--get element 'width) 2.0)) (h (/ (excali--get element 'height) 2.0)))
    (if (>= (* (abs dy) w) (* (abs dx) h))
        (cons cx (+ cy (if (> dy 0) h (- h))))
      (cons (+ cx (if (> dx 0) w (- w))) cy))))

(defun excali-dsl--port (element side toward)
  "Return ELEMENT's SIDE midpoint, or the side facing TOWARD."
  (let* ((center (excali-dsl--element-center element))
         (x (excali--get element 'x)) (y (excali--get element 'y))
         (w (excali--get element 'width)) (h (excali--get element 'height)))
    (pcase side
      ("top" (cons (car center) y)) ("bottom" (cons (car center) (+ y h)))
      ("left" (cons x (cdr center))) ("right" (cons (+ x w) (cdr center)))
      (_ (excali-dsl--side-point element toward)))))

(defun excali-dsl--draw-edge (edge model defaults)
  "Draw EDGE of MODEL as an arrow bound to its ends; DEFAULTS is the style."
  (let* ((from (excali-dsl--item (excali-dsl--edge-from edge) model))
         (to (excali-dsl--item (excali-dsl--edge-to edge) model))
         (a (if (excali-dsl--node-p from) (excali-dsl--node-element from)
              (excali-dsl--cluster-element from)))
         (b (if (excali-dsl--node-p to) (excali-dsl--node-element to)
              (excali-dsl--cluster-element to))))
    (when (and a b (not (eq a b)))
      (let* ((attrs (excali-dsl--edge-attrs edge))
             (arrow-op (excali-dsl--edge-arrow edge))
             (routing (downcase (format "%s" (or (excali-dsl--attr attrs "routing")
                                                 (if (equal arrow-op "~>") "curved"
                                                   (plist-get defaults :routing))))))
             (elbow (member routing '("orthogonal" "elbow")))
             (curved (member routing '("curved" "round")))
             (ca (excali-dsl--element-center a))
             (cb (excali-dsl--element-center b))
             (pa (excali-dsl--port a (excali-dsl--attr attrs "from") cb))
             (pb (excali-dsl--port b (excali-dsl--attr attrs "to") ca))
             (points (list pa pb))
             (origin (car points))
             (style (excali-dsl--attr attrs "strokeStyle" "style"))
             (arrow (excali-dsl--add
                     (excali--make-element
                      "arrow" (car origin) (cdr origin)
                      (cons 'points (vconcat (mapcar (lambda (p) (vector (float (- (car p) (car origin)))
                                                                         (float (- (cdr p) (cdr origin)))))
                                                     points)))
                      (cons 'strokeColor (or (excali-dsl--color (excali-dsl--attr attrs "strokeColor" "color"))
                                             "#1e1e1e"))
                      (cons 'strokeWidth (or (excali-dsl--attr attrs "strokeWidth" "width")
                                             (plist-get defaults :stroke-width)))
                      (cons 'strokeStyle (if (member style '("dashed" "dotted" "solid")) style "solid"))
                      (cons 'roughness (or (excali-dsl--attr attrs "roughness") (plist-get defaults :roughness)))
                      (cons 'roundness (if curved '((type . 2)) :null))
                      (cons 'startBinding :null) (cons 'endBinding :null)
                      (cons 'startArrowhead
                            (excali-dsl--arrowhead (excali-dsl--attr attrs "startArrowhead")
                                                   (if (equal arrow-op "<->") "arrow" :null)))
                      (cons 'endArrowhead
                            (excali-dsl--arrowhead (excali-dsl--attr attrs "endArrowhead")
                                                   (if (member arrow-op '("--" "---")) :null "arrow")))
                      (cons 'opacity (or (excali-dsl--attr attrs "opacity") 100))
                      (cons 'elbowed :false)
                      (cons 'customData (excali-dsl--custom-data
                                         (format "%s->%s" (excali-dsl--edge-from edge)
                                                 (excali-dsl--edge-to edge))))))))
        (excali--linear-extent arrow)
        (if elbow
            (progn
              (excali--elbow-make arrow)
              (excali--elbow-bind-end arrow 'start a)
              (excali--elbow-bind-end arrow 'end b)
              (excali--elbow-route-fresh arrow))
          (excali--bind-end arrow 'start a pa t)
          (excali--bind-end arrow 'end b pb t)
          ;; Outline points are not considered "inside" by hit testing.
          ;; Explicit ports nevertheless stay fixed to the requested side.
          (when (excali-dsl--attr attrs "from")
            (setcdr (assq 'mode (excali--get arrow 'startBinding)) "inside"))
          (when (excali-dsl--attr attrs "to")
            (setcdr (assq 'mode (excali--get arrow 'endBinding)) "inside"))
          (excali--update-arrow arrow))
        (when-let* ((label (excali-dsl--edge-label edge))
                    ((not (string-empty-p label))))
          (let* ((font (excali-dsl--font (excali-dsl--attr attrs "font") (plist-get defaults :font)))
                 (text (excali--add-bound-text
                        arrow (cons 'fontSize (or (excali-dsl--attr attrs "fontSize") 20))
                        (cons 'fontFamily font) (cons 'lineHeight (excali--line-height font))
                             (cons 'opacity (or (excali-dsl--attr attrs "opacity") 100))
                             (cons 'textAlign (or (excali-dsl--attr attrs "textAlign") "center"))
                        (cons 'strokeColor (or (excali-dsl--color (excali-dsl--attr attrs "textColor"))
                                               "#1e1e1e")))))
            (excali--set-text text label)))
        arrow))))

(defun excali-dsl--draw (model)
  "Draw the laid-out MODEL and return its elements, bottom to top."
  (with-temp-buffer
    (setq-local excali--elements nil)
    (setq-local excali--native-cache (make-hash-table :test #'eq))
    (let ((defaults (excali-dsl--defaults (plist-get model :config)))
          (root (plist-get model :root)))
      (when-let* ((title (plist-get model :title)))
        (excali-dsl--add
         (excali--make-text-element
          (/ (excali-dsl--cluster-w root) 2.0) -60 title
          (cons 'textAlign "center") (cons 'fontSize 28)
          (cons 'fontFamily (plist-get defaults :font)))))
      (excali-dsl--draw-cluster root defaults)
      (let (nodes)
        (maphash (lambda (_ node) (push node nodes)) (plist-get model :nodes))
        (dolist (node (sort nodes (lambda (a b) (< (or (excali-dsl--node-line a) 0)
                                                   (or (excali-dsl--node-line b) 0)))))
          (excali-dsl--draw-node node defaults)))
      (dolist (edge (plist-get model :edges))
        (excali-dsl--draw-edge edge model defaults))
      excali--elements)))

;;;; Programs

(defun excali-dsl--report-fonts (model)
  "Report missing primary fonts used by MODEL; measurement uses Pango fallback."
  (let ((seen (make-hash-table :test #'eql)))
    (cl-labels ((check (attrs)
                 (let* ((id (cdr (assoc "font" attrs)))
                        (preferred (car (split-string (excali-native-font-family id) ",")))
                        (resolved (excali-native-font-resolve "Hello" id)))
                   (unless (gethash id seen)
                     (puthash id t seen)
                     (unless (equal preferred resolved)
                       (message "Excali DSL: font %s uses %s (see M-x excali-font-report)" preferred resolved))))))
      (maphash (lambda (_ n) (check (excali-dsl--node-attrs n))) (plist-get model :nodes))
      (maphash (lambda (_ c) (check (excali-dsl--cluster-attrs c))) (plist-get model :clusters))
      (dolist (e (plist-get model :edges)) (check (excali-dsl--edge-attrs e))))))

(defun excali-dsl-model (string)
  "Parse, build and lay out diagram DSL STRING; return the model plist."
  (let ((model (excali-dsl--build (excali-dsl-parse string))))
    (excali-dsl--report-fonts model)
    (excali-dsl--layout model)))

(defun excali-dsl-elements (string)
  "Return the Excalidraw elements diagram DSL STRING draws.
Elements are alists as in .excalidraw files, bottom to top, with fresh
ids; each shape and arrow records its DSL id in `customData.excalidslId'.
Signal `excali-dsl-error' on bad input."
  (excali-dsl--draw (excali-dsl-model string)))

(defun excali-dsl-scene (string)
  "Return a restored native Excalidraw document for DSL STRING."
  (let ((doc (excali--empty-doc)))
    (setf (alist-get 'elements doc) (vconcat (excali-dsl-elements string)))
    (excali--restore-doc doc)))

(defun excali-dsl-write (string file)
  "Atomically write the scene for STRING to FILE.
Parsing and rendering finish before touching the destination."
  (let* ((doc (excali-dsl-scene string))
         (text (excali--serialize-doc doc (append (alist-get 'elements doc) nil)))
         (destination (expand-file-name file))
         (temp (make-temp-file (expand-file-name ".excali-dsl-" (file-name-directory destination)))))
    (unwind-protect
        (progn
          (with-temp-file temp
            (setq buffer-file-coding-system 'utf-8-unix)
            (insert text))
          (when (file-exists-p destination) (set-file-modes temp (file-modes destination)))
          (rename-file temp destination t))
      (when (file-exists-p temp) (delete-file temp)))
    file))

;;;; Commands

(defvar-local excali-dsl--scene-buffer nil
  "The excali buffer this .excalidsl buffer was last drawn in.")

(defun excali-dsl--signal-user (err &optional name)
  "Report the `excali-dsl-error' ERR as a user error, prefixed with NAME."
  (user-error "%s%s" (if name (concat name ":") "") (excali-dsl-error-message err)))

(defun excali-dsl-render ()
  "Draw the diagram in the current .excalidsl buffer in an excali buffer.
Drawing again replaces the scene in the same buffer and keeps its view."
  (interactive)
  ;; The DSL mode can be autoloaded before the canvas entry points.
  (require 'excali)
  (let* ((source (buffer-substring-no-properties (point-min) (point-max)))
         (name (format "*excali %s*" (if buffer-file-name
                                         (file-name-nondirectory buffer-file-name)
                                       (buffer-name))))
         (doc (condition-case err (excali-dsl-scene source)
                (excali-dsl-error (excali-dsl--signal-user err (buffer-name)))))
         (dark nil)
         (scene excali-dsl--scene-buffer))
    (if (buffer-live-p scene)
        (with-current-buffer scene
          (setq excali--doc doc
                excali--elements (append (alist-get 'elements doc) nil)
                excali--theme (if dark 'dark 'light))
          (when (hash-table-p excali--native-cache) (clrhash excali--native-cache))
          (excali--history-reset)
          (excali--render)
          (display-buffer scene))
      (let ((origin (selected-window)))
        (let ((display-buffer-overriding-action
               '((display-buffer-reuse-window display-buffer-pop-up-window)
                 (inhibit-same-window . t))))
          ;; Opening the canvas changes the current buffer.  Restore the DSL
          ;; buffer before storing its buffer-local preview association.
          (save-current-buffer
            (setq scene (excali--open doc nil name))))
        (with-current-buffer scene
          (when dark
            (setq excali--theme 'dark)
            (excali--render))
          (excali-zoom-to-fit))
        (setq excali-dsl--scene-buffer scene)
        (when (window-live-p origin) (select-window origin))))
    scene))

(defun excali-dsl-export (file)
  "Write the diagram in the current .excalidsl buffer to the .excalidraw FILE."
  (interactive
   (list (read-file-name "Write scene to: " nil nil nil
                         (concat (file-name-base (or buffer-file-name (buffer-name)))
                                 ".excalidraw"))))
  (condition-case err
      (excali-dsl-write (buffer-substring-no-properties (point-min) (point-max)) file)
    (excali-dsl-error (excali-dsl--signal-user err (buffer-name))))
  (message "Wrote %s" file))

(defun excali-dsl-insert (string)
  "Add the diagram DSL STRING draws to the current excali scene.
It is centered in the view and selected, one undo step."
  (require 'excali)
  (let* ((elements (condition-case err (excali-dsl-elements string)
                     (excali-dsl-error (excali-dsl--signal-user err))))
         (bounds (excali--elements-bounds elements))
         (center (excali--view-center))
         (dx (- (car center) (/ (+ (nth 0 bounds) (nth 2 bounds)) 2.0)))
         (dy (- (cdr center) (/ (+ (nth 1 bounds) (nth 3 bounds)) 2.0))))
    (dolist (e elements)
      (excali--put e 'x (float (+ (excali--get e 'x) dx)))
      (excali--put e 'y (float (+ (excali--get e 'y) dy))))
    (excali--insert-elements elements)
    (excali--render)
    elements))

(defun excali-dsl-yank ()
  "Add the diagram DSL at the front of the kill ring to the excali scene.
For pasting a diagram written elsewhere, for example by a program."
  (interactive)
  (excali-dsl-insert (current-kill 0)))

(defun excali-dsl-insert-file (file)
  "Add the diagram in the .excalidsl FILE to the current excali scene."
  (interactive "fDiagram file: ")
  (excali-dsl-insert (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))

(defun excali-dsl--after-save ()
  "Draw the diagram again after saving, per `excali-dsl-render-on-save'."
  (when (and excali-dsl-render-on-save (buffer-live-p excali-dsl--scene-buffer))
    (excali-dsl-render)))

;;;; The major mode

(defconst excali-dsl--keywords
  '("node" "edge" "style" "default" "above" "below" "left" "right" "of" "level" "with" "from" "to")
  "Keywords of Excali DSL.")

(defvar excali-dsl-font-lock-keywords
  `((,(concat "\\_<" (regexp-opt excali-dsl--keywords) "\\_>") . font-lock-keyword-face)
    ("->" . font-lock-builtin-face)
    ("\\_<\\([[:alnum:]_-]+\\)[ \t]*:" 1 font-lock-variable-name-face))
  "Highlighting rules for Excali DSL.")

(defvar excali-dsl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    table)
  "Syntax table for Excali DSL.")

(defun excali-dsl-indent-line ()
  "Indent the current line by its brace depth."
  (interactive)
  (let* ((depth (save-excursion
                  (beginning-of-line)
                  (car (syntax-ppss))))
         (closing (save-excursion (back-to-indentation) (looking-at "}")))
         (continuation (and (= depth 0) (not closing)
                            (save-excursion
                              (back-to-indentation)
                              (not (or (looking-at "\\(?:node\\|edge\\|style\\|default\\)\\_>")
                                       (looking-at "//") (eolp))))))
         (column (* excali-dsl-indent-offset (max (if continuation 1 0) (- depth (if closing 1 0)))))
         (offset (- (current-column) (current-indentation))))
    (indent-line-to column)
    (when (> offset 0) (forward-char offset))))

(defvar-keymap excali-dsl-mode-map
  "C-c C-c" #'excali-dsl-render
  "C-c C-e" #'excali-dsl-export)

;;;###autoload
(define-derived-mode excali-dsl-mode prog-mode "Excali DSL"
  "Major mode for diagrams in Excali DSL (.excalidsl).
\\<excali-dsl-mode-map>\\[excali-dsl-render] draws the buffer in an excali \
buffer, \\[excali-dsl-export] writes it to an .excalidraw file.

\\{excali-dsl-mode-map}"
  (setq-local comment-start "// "
              comment-start-skip "//+[ \t]*"
              font-lock-defaults '(excali-dsl-font-lock-keywords)
              indent-line-function #'excali-dsl-indent-line)
  (add-hook 'after-save-hook #'excali-dsl--after-save nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.excalidsl\\'" . excali-dsl-mode))

(provide 'excali-dsl)
;;; excali-dsl.el ends here
