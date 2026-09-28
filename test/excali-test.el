;;; excali-test.el --- Batch tests for excali.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)

(defconst excali-test--sample
  (expand-file-name "sample.excalidraw"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest excali-test-roundtrip-preserves-fields ()
  "Saving an unmodified scene keeps every element field."
  (let ((out (make-temp-file "excali" nil ".excalidraw")))
    (unwind-protect
        (with-temp-buffer
          (setq excali--native-cache (make-hash-table :test #'eq)
                excali--doc (excali--read-file excali-test--sample)
                excali--elements (append (alist-get 'elements excali--doc) nil)
                excali--file out)
          (excali-save)
          (should (equal (excali--read-file out)
                         (excali--read-file excali-test--sample))))
      (delete-file out))))

(ert-deftest excali-test-measure-text ()
  "Two-line text is taller than one line at the same size."
  (let ((one (excali-native-measure-text "你好" 20 5 1.25))
        (two (excali-native-measure-text "你好\n世界" 20 5 1.25)))
    (should (> (car one) 0))
    (should (= (cdr one) 25.0))
    (should (= (cdr two) 50.0))))

(ert-deftest excali-test-render-writes-pixels ()
  "Rendering the sample changes pixels away from the white background."
  (with-temp-buffer
    (setq excali--native-cache (make-hash-table :test #'eq)
          excali--elements (append (alist-get 'elements
                                             (excali--read-file excali-test--sample))
                                  nil))
    (let* ((w 400) (h 300)
           (canvas (list 'image :type 'canvas :id (gensym) :data-width w
                         :data-height h :data (make-vector (* w h) 0)))
           (png (make-temp-file "excali" nil ".png")))
      (unwind-protect
          (progn
            (should (excali-native-render canvas w h 1.0 1.0 0.0 0.0
                                         (excali--visible-elements)))
            (should (excali-native-write-png canvas w h png))
            (should (> (file-attribute-size (file-attributes png)) 1000)))
        (delete-file png)))))

;; Tiles

(defun excali-test--tiles (fb-width fb-height size)
  "Return a vector of tiles covering FB-WIDTH by FB-HEIGHT."
  (vconcat
   (cl-loop for y from 0 below fb-height by size
            nconc (cl-loop for x from 0 below fb-width by size
                           collect
                           (let ((w (min size (- fb-width x)))
                                 (h (min size (- fb-height y))))
                             (vector (list 'image :type 'canvas :id (gensym)
                                           :data-width w :data-height h
                                           :data (make-vector (* w h) 0))
                                     x y w h))))))

(ert-deftest excali-test-tiles-refresh-only-changes ()
  "Only tiles whose pixels changed are reported dirty."
  (with-temp-buffer
    (setq excali--native-cache (make-hash-table :test #'eq))
    (let* ((rect (excali--make-element "rectangle" 10 10
                                      (cons 'width 60.0) (cons 'height 40.0)))
           (fb (excali-native-fb-create 400 300))
           (tiles (excali-test--tiles 400 300 100)))
      (setq excali--elements (list rect))
      (excali-native-fb-render fb 1.0 1.0 0.0 0.0 (excali--visible-elements) nil)
      ;; First present fills every tile.
      (should (= (length (excali-native-fb-present-tiles fb tiles)) 12))
      ;; Nothing changed: nothing is dirty.
      (should (null (excali-native-fb-present-tiles fb tiles)))
      ;; Move the rectangle within the first tile, repainting only damage.
      (let ((damage (excali--with-damage rect
                      (excali--put rect 'x 20.0)
                      (excali--touch rect))))
        (excali-native-fb-render fb 1.0 1.0 0.0 0.0 (excali--visible-elements)
                                (excali--damage-vector damage)))
      ;; Damage spans tiles 0 1 4 5, but pixels change only in tile 0.
      (should (equal (excali-native-fb-present-tiles fb tiles) '(0))))))

(ert-deftest excali-test-damage-render-matches-full ()
  "A damage-limited render matches a full render up to antialiasing."
  (random "excali-test")
  (with-temp-buffer
    (setq excali--native-cache (make-hash-table :test #'eq))
    (let* ((rect (excali--make-element "ellipse" 50 50
                                      (cons 'width 80.0) (cons 'height 60.0)))
           (other (excali--make-element "rectangle" 200 150
                                       (cons 'width 80.0) (cons 'height 60.0)))
           (partial (excali-native-fb-create 400 300))
           (full (excali-native-fb-create 400 300)))
      (setq excali--elements (list rect other))
      (excali-native-fb-render partial 1.0 1.0 0.0 0.0
                              (excali--visible-elements) nil)
      (let ((damage (excali--with-damage rect
                      (excali--put rect 'x 90.0)
                      (excali--touch rect))))
        (excali-native-fb-render partial 1.0 1.0 0.0 0.0
                                (excali--visible-elements)
                                (excali--damage-vector damage)))
      (excali-native-fb-render full 1.0 1.0 0.0 0.0
                              (excali--visible-elements) nil)
      ;; Antialiased edges crossing the damage clip may differ slightly.
      (should (<= (excali-native-fb-diff partial full) 16)))))

(ert-deftest excali-test-culling-skips-offscreen ()
  "Elements outside the viewport are not drawn."
  (with-temp-buffer
    (setq excali--native-cache (make-hash-table :test #'eq))
    (setq excali--elements
          (list (excali--make-element "rectangle" 10 10 (cons 'width 50.0) (cons 'height 50.0))
                (excali--make-element "rectangle" 5000 5000 (cons 'width 50.0) (cons 'height 50.0))))
    (should (= 1 (excali-native-fb-render (excali-native-fb-create 200 200)
                                         1.0 1.0 0.0 0.0
                                         (excali--visible-elements) nil)))))

;; Resizing

(defmacro excali-test--with-scene (&rest body)
  "Run BODY in a temp buffer set up like an excali buffer at zoom 1."
  `(with-temp-buffer
     (setq excali--native-cache (make-hash-table :test #'eq)
           excali--zoom 1.0)
     ,@body))

;; Pointer shape

(ert-deftest excali-test-pointer-follows-scene ()
  "The pointer reflects handles, elements, empty space and the tool."
  (excali-test--with-scene
   (let ((rect (excali--make-element "rectangle" 10 20
                                    (cons 'width 100.0) (cons 'height 50.0)
                                    (cons 'backgroundColor "#a5d8ff"))))
     (setq excali--elements (list rect)
           excali--scroll-x 0.0 excali--scroll-y 0.0
           excali--tool 'select)
     (should (eq (excali--cursor-at '(60.0 . 45.0)) 'move))
     (should (eq (excali--cursor-at '(300.0 . 300.0)) 'default))
     ;; Handles only count once the element is selected.
     (should (eq (excali--cursor-at '(60.0 . 76.0)) 'move))
     (setq excali--selection (list rect))
     (should (eq (excali--cursor-at '(60.0 . 76.0)) 'ns-resize))
     (should (eq (excali--cursor-at '(116.0 . 45.0)) 'ew-resize))
     (should (eq (excali--cursor-at '(116.0 . 76.0)) 'nwse-resize))
     (setq excali--tool 'text)
     (should (eq (excali--cursor-at '(60.0 . 45.0)) 'crosshair))
     (setq excali--tool 'rectangle)
     (should (eq (excali--cursor-at '(60.0 . 45.0)) 'crosshair)))))

(ert-deftest excali-test-set-pointer-is-silent ()
  "Changing the pointer does not mark the buffer modified."
  (excali-test--with-scene
   (insert "  ")
   (set-buffer-modified-p nil)
   (excali--set-pointer 'pointer)
   (should (eq (get-text-property 1 'pointer) 'hand))
   (should-not (buffer-modified-p))))

;; Scroll reuse

(defmacro excali-test--with-view (width height &rest body)
  "Run BODY with a WIDTH by HEIGHT framebuffer and a random busy scene."
  (declare (indent 2))
  `(excali-test--with-scene
    (random "excali-scroll")
    (setq excali--pixel-scale 1.0 excali--scroll-x 0.0 excali--scroll-y 0.0
          excali--backend nil
          excali--canvas-size (cons ,width ,height)
          excali--fb (excali-native-fb-create ,width ,height)
          excali--elements (excali--stress-elements 60))
    ,@body))

(defun excali-test--full-render-diff ()
  "Return the largest difference between the framebuffer and a full render."
  (let ((full (excali-native-fb-create (car excali--canvas-size)
                                      (cdr excali--canvas-size))))
    (excali-native-fb-render full excali--pixel-scale excali--zoom
                            excali--scroll-x excali--scroll-y
                            (excali--visible-elements) nil)
    (excali-native-fb-diff excali--fb full)))

(ert-deftest excali-test-scroll-reuse-matches-full ()
  "Panning by whole pixels repaints only strips yet matches a full render."
  (excali-test--with-view 400 300
    (excali--render)
    (dolist (delta '((7 . 5) (-13 . 0) (0 . -9) (-3 . 11)))
      (excali--pan (car delta) (cdr delta))
      (should (< (plist-get excali--last-stats :drawn) 60)))
    (should (<= (excali-test--full-render-diff) 16))))

(ert-deftest excali-test-scroll-reuse-at-zoom ()
  "Reuse also holds at a non-integer zoom and 2x pixel scale."
  (excali-test--with-view 400 300
    (setq excali--zoom 1.37 excali--pixel-scale 2.0
          excali--scroll-x 3.3 excali--scroll-y -8.1)
    (excali--render)
    (dotimes (_ 5) (excali--pan 4 -3))
    (should (<= (excali-test--full-render-diff) 16))))

(ert-deftest excali-test-fractional-pan-accumulates ()
  "Sub-pixel wheel deltas wait until they add up to a whole pixel."
  (excali-test--with-view 200 100
    (excali--render)
    (let ((origin excali--rendered-origin))
      (excali--pan 0.4 0.0)
      (should (equal excali--rendered-origin origin))
      (excali--pan 0.7 0.0)
      (should (= excali--scroll-x 1.0))
      (should (< (abs (- (car excali--pan-remainder) 0.1)) 1e-9)))))

(ert-deftest excali-test-plan-repaint ()
  "Unchanged views skip rendering; zoom changes and odd shifts repaint all."
  (excali-test--with-view 200 100
    (excali--render)
    (should (eq (excali--plan-repaint 'scroll) 'none))
    (setq excali--scroll-x (+ excali--scroll-x 0.5))
    (should (null (excali--plan-repaint 'scroll)))
    (setq excali--zoom 2.0)
    (should (null (excali--plan-repaint 'scroll)))
    (setq excali--scroll-x (+ excali--scroll-x 2.0))
    (should (equal (excali--plan-repaint 'scroll) [[0 0 4 100]]))))

;; Selection, history, clipboard, arrangement

(defun excali-test--rect (x y &rest props)
  "Return a 10x10 rectangle at X, Y with extra PROPS alist."
  (apply #'excali--make-element "rectangle" x y
         (cons 'width 10.0) (cons 'height 10.0) props))

(defmacro excali-test--with-elements (bindings &rest body)
  "Bind BINDINGS to elements forming the scene, then run BODY."
  (declare (indent 1))
  `(excali-test--with-scene
    (let* ,bindings
      (setq excali--elements (list ,@(mapcar #'car bindings))
            excali--selection nil excali--editing-group nil)
      ,@body)))

(ert-deftest excali-test-group-units ()
  "Clicking a grouped element selects its outermost group until entered."
  (excali-test--with-elements
      ((a (excali-test--rect 0 0 (cons 'groupIds ["inner" "outer"])))
       (b (excali-test--rect 20 0 (cons 'groupIds ["inner" "outer"])))
       (c (excali-test--rect 40 0 (cons 'groupIds ["outer"])))
       (d (excali-test--rect 60 0)))
    (should (equal (excali--unit a) (list a b c)))
    (should (equal (excali--unit d) (list d)))
    ;; Entering the outer group exposes the inner group as the unit.
    (setq excali--editing-group "outer")
    (should (equal (excali--unit a) (list a b)))
    (should (equal (excali--unit c) (list c)))
    (setq excali--editing-group "inner")
    (should (equal (excali--unit a) (list a)))))

(ert-deftest excali-test-toggle-and-marquee ()
  "Shift-click toggles units; the marquee takes only units fully inside."
  (excali-test--with-elements
      ((a (excali-test--rect 0 0))
       (b (excali-test--rect 20 0))
       (c (excali-test--rect 40 0 (cons 'groupIds ["g"])))
       (d (excali-test--rect 80 0 (cons 'groupIds ["g"]))))
    (excali--toggle-unit b)
    (excali--toggle-unit a)
    (should (equal excali--selection (list a b)))
    (excali--toggle-unit b)
    (should (equal excali--selection (list a)))
    ;; The marquee covers c but only half of its group.
    (should (equal (excali--marquee-selection '(-1 -1 55 11)) (list a b)))
    (should (equal (excali--marquee-selection '(-1 -1 95 11)) (list a b c d)))))

(ert-deftest excali-test-undo-redo ()
  "Commands commit once per change; undo and redo restore scenes."
  (excali-test--with-elements ((a (excali-test--rect 0 0)))
    (excali--history-reset)
    (excali--commit)
    (should (= (length excali--undo-stack) 1))
    (excali--select (list a))
    (excali--nudge 5 0)
    (excali--commit)
    (let ((b (excali-test--rect 50 50)))
      (setq excali--elements (append excali--elements (list b))))
    (excali--commit)
    (should (= (length excali--undo-stack) 3))
    ;; The unchanged element shares one frozen copy across snapshots.
    (should (eq (car (plist-get (nth 0 excali--undo-stack) :elements))
                (car (plist-get (nth 1 excali--undo-stack) :elements))))
    (excali-undo)
    (should (= (length excali--elements) 1))
    (should (= (excali--get (car excali--elements) 'x) 5.0))
    (excali-undo)
    (should (= (excali--get (car excali--elements) 'x) 0.0))
    (excali-redo)
    (excali-redo)
    (should (= (length excali--elements) 2))
    ;; Undoing then editing drops the redo branch.
    (excali-undo)
    (excali--select (list (car excali--elements)))
    (excali--nudge 1 0)
    (excali--commit)
    (should (null excali--redo-stack))
    ;; Restored elements are fresh copies: editing them leaves history intact.
    (should (= (excali--get (car (plist-get (nth 1 excali--undo-stack) :elements)) 'x)
               5.0))))

(ert-deftest excali-test-clipboard-roundtrip ()
  "Copied elements paste with new ids and remapped internal references."
  (excali-test--with-elements
      ((box (excali-test--rect 0 0 (cons 'groupIds ["g"])
                              (cons 'boundElements [((id . "label") (type . "text"))])))
       (label (excali--make-text-element 2 2 "hi"))
       (arrow (excali--make-element
               "arrow" 20 5 (cons 'points [[0.0 0.0] [30.0 0.0]])
               (cons 'groupIds ["g"])
               (cons 'startBinding (list (cons 'elementId (excali--get box 'id))
                                         (cons 'focus 0) (cons 'gap 1)))
               (cons 'endBinding (list (cons 'elementId "elsewhere")
                                       (cons 'focus 0) (cons 'gap 1))))))
    (excali--put label 'id "label")
    (excali--put label 'containerId (excali--get box 'id))
    (let* ((json (excali--clipboard-json (list box label arrow)))
           (parsed (excali--parse-clipboard json))
           (clones (excali--clone-elements parsed)))
      (should (= (length parsed) 3))
      (pcase-let ((`(,box2 ,label2 ,arrow2) clones))
        (should-not (equal (excali--get box2 'id) (excali--get box 'id)))
        (should (equal (excali--get label2 'containerId) (excali--get box2 'id)))
        (should (equal (alist-get 'id (aref (excali--get box2 'boundElements) 0))
                       (excali--get label2 'id)))
        (should (equal (alist-get 'elementId (excali--get arrow2 'startBinding))
                       (excali--get box2 'id)))
        ;; References leaving the pasted set are dropped.
        (should (eq (alist-get 'endBinding arrow2) :null))
        ;; Both grouped clones share one new group.
        (should (equal (excali--get box2 'groupIds) (excali--get arrow2 'groupIds)))
        (should-not (equal (excali--get box2 'groupIds) ["g"]))))
    (should-not (excali--parse-clipboard "just text"))))

(ert-deftest excali-test-duplicate ()
  "Duplicating selects offset copies on top of the scene."
  (excali-test--with-elements ((a (excali-test--rect 0 0)))
    (setq excali--backend nil)
    (excali--select (list a))
    (excali-duplicate)
    (should (= (length excali--elements) 2))
    (should (eq (car excali--selection) (cadr excali--elements)))
    (should (= (excali--get (car excali--selection) 'x) 10.0))))

(ert-deftest excali-test-z-order ()
  "Z-order commands move selected runs past their neighbours."
  (excali-test--with-elements
      ((a (excali-test--rect 0 0)) (b (excali-test--rect 0 0))
       (c (excali-test--rect 0 0)) (d (excali-test--rect 0 0)))
    (setq excali--backend nil)
    (excali--select (list a b))
    (excali-bring-forward)
    (should (equal excali--elements (list c a b d)))
    (excali-bring-to-front)
    (should (equal excali--elements (list c d a b)))
    (excali-send-backward)
    (should (equal excali--elements (list c a b d)))
    (excali-send-to-back)
    (should (equal excali--elements (list a b c d)))))

(ert-deftest excali-test-group-ungroup ()
  "Grouping adds an outermost group; ungrouping removes it."
  (excali-test--with-elements
      ((a (excali-test--rect 0 0 (cons 'groupIds ["old"])))
       (b (excali-test--rect 20 0)))
    (setq excali--backend nil)
    (excali--select (list a b))
    (excali-group)
    (let ((group (aref (excali--get b 'groupIds) 0)))
      (should (equal (excali--get a 'groupIds) (vector "old" group)))
      (should (equal (excali--unit b) (list a b)))
      (excali-ungroup)
      (should (equal (excali--get a 'groupIds) ["old"]))
      (should (equal (excali--get b 'groupIds) [])))))
;; Zoom preview

(defun excali-test--full-render ()
  "Return a new framebuffer holding a full render of the current view."
  (let ((full (excali-native-fb-create (car excali--canvas-size)
                                      (cdr excali--canvas-size))))
    (excali-native-fb-render full excali--pixel-scale excali--zoom
                            excali--scroll-x excali--scroll-y
                            (excali--visible-elements) nil)
    full))

(defun excali-test--preview-timers ()
  "Return the pending timers that finish a zoom preview."
  (cl-remove-if-not (lambda (timer)
                      (eq (timer--function timer) #'excali--finish-preview))
                    timer-list))

(defmacro excali-test--with-preview (&rest body)
  "Run BODY in a 400x300 view zoomed out to show a busy scene.
Temporary buffers skip `kill-buffer-hook', so cancel the preview here."
  `(excali-test--with-view 400 300
     (let ((excali-zoom-preview-delay 0.1)
           (excali-zoom-preview-limit 4.0))
       (setq excali--zoom 0.5)
       (excali--render)
       (unwind-protect (progn ,@body)
         (excali--cancel-preview)))))

(ert-deftest excali-test-zoom-preview-approximates-full ()
  "A preview is much closer to a full render than the stale pixels are."
  (dolist (factor '(1.1 0.9 1.3 0.75))
    (excali-test--with-preview
     (let ((stale (excali-test--full-render)))
       (excali--zoom-at factor '(130 . 90))
       (should (plist-get excali--last-stats :preview))
       (should (= (plist-get excali--last-stats :drawn) 0))
       (let* ((full (excali-test--full-render))
              (preview (excali-native-fb-mean-diff excali--fb full))
              (unchanged (excali-native-fb-mean-diff stale full)))
         (should (< preview 3.0))
         (should (< preview (* 0.5 unchanged))))))))

(ert-deftest excali-test-zoom-preview-does-not-drift ()
  "Previews scale the last full render, so zooming back restores it."
  (excali-test--with-preview
   (let ((original (excali-test--full-render)))
     (dotimes (_ 4) (excali--zoom-at 1.1 '(130 . 90)))
     (dotimes (_ 4) (excali--zoom-at (/ 1 1.1) '(130 . 90)))
     (should (plist-get excali--last-stats :preview))
     (should (<= (excali-native-fb-diff excali--fb original) 2)))))

(ert-deftest excali-test-zoom-preview-timer-renders-full ()
  "The pending timer's function turns the preview into a full render."
  (excali-test--with-preview
   (excali--zoom-at 1.2 '(200 . 150))
   (excali--zoom-at 1.2 '(50 . 40))
   (should (timerp excali--preview-timer))
   (should (equal (excali-test--preview-timers) (list excali--preview-timer)))
   (should (equal (timer--args excali--preview-timer) (list (current-buffer))))
   (funcall (timer--function excali--preview-timer) (current-buffer))
   (should-not excali--preview-timer)
   (should-not excali--preview-origin)
   (should-not (plist-get excali--last-stats :preview))
   (should (equal excali--rendered-origin (excali--view-origin)))
   (should (<= (excali-test--full-render-diff) 16))))

(ert-deftest excali-test-zoom-preview-blocks-scroll-reuse ()
  "Preview pixels are never shifted as if they were exact."
  (excali-test--with-preview
   (excali--zoom-at 1.25 '(100 . 100))
   (should-not excali--rendered-origin)
   (should (null (excali--plan-repaint 'scroll)))
   ;; Damage during a preview repaints everything and ends it.
   (excali--zoom-at 1.25 '(100 . 100))
   (excali--render '(0 0 10 10))
   (should-not excali--preview-timer)
   (should-not (plist-get excali--last-stats :preview))
   (should (<= (excali-test--full-render-diff) 16))))

(ert-deftest excali-test-zoom-preview-pan ()
  "Pans during a preview keep previewing, then settle to a full render."
  (excali-test--with-preview
   (excali--zoom-at 1.2 '(100 . 100))
   (excali--pan 7 -5)
   (should (plist-get excali--last-stats :preview))
   (should-not excali--rendered-origin)
   (excali--finish-preview (current-buffer))
   (should-not (excali-test--preview-timers))
   (should (<= (excali-test--full-render-diff) 16))
   ;; With the preview gone, pans reuse pixels again.
   (excali--pan 7 -5)
   (should-not (plist-get excali--last-stats :preview))
   (should (<= (excali-test--full-render-diff) 16))))

(ert-deftest excali-test-zoom-preview-falls-back ()
  "Large factors, disabled previews and fresh framebuffers render fully."
  (excali-test--with-preview
   (excali--zoom-at 5.0 '(0 . 0))
   (should-not (plist-get excali--last-stats :preview))
   (should-not excali--preview-timer)
   (let ((excali-zoom-preview-delay nil))
     (excali--zoom-at 1.1 '(0 . 0))
     (should-not (plist-get excali--last-stats :preview)))
   (setq excali--rendered-origin nil)
   (excali--zoom-at 1.1 '(0 . 0))
   (should-not (plist-get excali--last-stats :preview))
   (should (<= (excali-test--full-render-diff) 16))))

(ert-deftest excali-test-zoom-preview-timer-cleanup ()
  "Steps share one timer, and killing the buffer cancels it."
  (let ((buffer (generate-new-buffer "excali-test-preview"))
        timer)
    (unwind-protect
        (with-current-buffer buffer
          (setq excali--native-cache (make-hash-table :test #'eq)
                excali--zoom 0.5 excali--pixel-scale 1.0
                excali--canvas-size (cons 200 100)
                excali--fb (excali-native-fb-create 200 100)
                excali--elements (excali--stress-elements 20))
          (excali--render)
          (dotimes (_ 5) (excali--zoom-at 1.05 '(10 . 10)))
          (setq timer excali--preview-timer)
          (should (timerp timer))
          (should (equal (excali-test--preview-timers) (list timer))))
      (kill-buffer buffer))
    (should-not (memq timer timer-list))
    (should-not (excali-test--preview-timers))
    ;; A timer outliving its buffer does nothing.
    (excali--finish-preview buffer)))

(ert-deftest excali-test-zoom-preview-fills-white ()
  "Pixels the scaled image does not cover are white."
  (excali-test--with-preview
   (let ((blank (excali-native-fb-create 400 300)))
     (excali-native-fb-render blank 1.0 1.0 0.0 0.0 [] nil)
     (should (> (excali-native-fb-mean-diff excali--fb blank) 0))
     (should (excali-native-fb-zoom-preview excali--fb excali--fb 0.5 400.0 0.0))
     (should (= (excali-native-fb-diff excali--fb blank) 0)))))

(ert-deftest excali-test-backend-resolution ()
  "`auto' picks `layer' on macOS frames with the module, else `tiles'."
  (let ((excali-backend 'auto))
    ;; Batch frames are not graphical.
    (should (eq (excali--resolve-backend) 'tiles))
    (cl-letf (((symbol-function 'excali--layer-frame-p) #'always))
      (should (eq (excali--resolve-backend)
                  (if (fboundp 'excali-native-layer-create) 'layer 'tiles)))))
  (cl-letf (((symbol-function 'excali--layer-frame-p) #'always))
    (dolist (choice '(canvas tiles))
      (let ((excali-backend choice))
        (should (eq (excali--resolve-backend) choice))))))

;; End-to-end mouse gestures with synthesized events

(defun excali-test--posn (x y)
  "Return a text-area mouse position at window pixel X, Y."
  (list (selected-window) 1 (cons x y) 0 nil 1 '(0 . 0) nil '(0 . 0) '(1 . 1)))

(defun excali-test--drag (x0 y0 x1 y1 &optional modifiers)
  "Run `excali-mouse-down' for a drag from X0,Y0 to X1,Y1 in window pixels.
MODIFIERS, such as (shift), are added to the press event."
  (let ((start (excali-test--posn x0 y0)))
    (setq unread-command-events
          (append (cl-loop for i from 1 to 4
                           collect (list 'mouse-movement
                                         (excali-test--posn
                                          (+ x0 (/ (* i (- x1 x0)) 4))
                                          (+ y0 (/ (* i (- y1 y0)) 4)))))
                  (list (list 'drag-mouse-1 start (excali-test--posn x1 y1)))))
    (excali-mouse-down
     (list (event-convert-list (append modifiers '(down-mouse-1))) start))))

(defmacro excali-test--in-window (&rest body)
  "Run BODY in an excali-like buffer shown in the selected window."
  `(let ((buffer (generate-new-buffer " *excali-test*")))
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer buffer)
           (setq excali--native-cache (make-hash-table :test #'eq)
                 excali--zoom 1.0 excali--pixel-scale 1.0
                 excali--scroll-x 0.0 excali--scroll-y 0.0
                 excali--backend nil excali--tool 'select
                 excali--selection nil excali--editing-group nil)
           ,@body)
       (kill-buffer buffer))))

(ert-deftest excali-test-gesture-box-select-then-move-from-gap ()
  "Box-select two shapes, then drag from the gap between them to move both."
  (excali-test--in-window
   (let ((a (excali-test--rect 10 10)) (b (excali-test--rect 50 10)))
     (setq excali--elements (list a b))
     ;; Box selection from empty space around both.
     (excali-test--drag 0 0 70 30)
     (should (equal excali--selection (list a b)))
     ;; Press in the gap at x=35, which hits neither shape, and drag.
     (should-not (excali--hit '(35.0 . 15.0)))
     (excali-test--drag 35 15 55 45)
     (should (equal excali--selection (list a b)))
     (should (equal (list (excali--get a 'x) (excali--get a 'y)) '(30.0 40.0)))
     (should (equal (list (excali--get b 'x) (excali--get b 'y)) '(70.0 40.0))))))

(ert-deftest excali-test-gesture-click-outside-box-deselects ()
  "Pressing outside the selection box starts a new box selection."
  (excali-test--in-window
   (let ((a (excali-test--rect 10 10)) (b (excali-test--rect 50 10)))
     (setq excali--elements (list a b))
     (excali--select (list a b))
     (excali-test--drag 200 200 210 210)
     (should (null excali--selection))
     (should (= (excali--get a 'x) 10.0)))))

(ert-deftest excali-test-gesture-drag-unselected-shape ()
  "Dragging an unselected shape selects and moves only it."
  (excali-test--in-window
   (let ((a (excali-test--rect 10 10)) (b (excali-test--rect 50 10)))
     (setq excali--elements (list a b))
     (excali--select (list a))
     (excali-test--drag 55 15 65 25)
     (should (equal excali--selection (list b)))
     (should (= (excali--get a 'x) 10.0))
     (should (= (excali--get b 'x) 60.0)))))

(ert-deftest excali-test-gesture-shift-click-adds ()
  "Shift-clicking a shape adds it to the selection."
  (excali-test--in-window
   (let ((a (excali-test--rect 10 10)) (b (excali-test--rect 50 10)))
     (setq excali--elements (list a b))
     (excali--select (list a))
     (excali-test--drag 55 15 55 15 '(shift))
     (should (equal excali--selection (list a b))))))

;; Style

(ert-deftest excali-test-style-app-state-roundtrip ()
  "currentItem* app state loads into the current style and saves back."
  (excali-test--with-scene
   (excali--load-current-style '((currentItemStrokeColor . "#e03131")
                                (currentItemEndArrowhead . :null)
                                (viewBackgroundColor . "#ffffff")))
   (should (equal (excali--style-value 'strokeColor) "#e03131"))
   (should (null (excali--style-value 'endArrowhead)))
   (should (equal (excali--style-value 'fontSize) 20))
   (excali-set-style 'fontSize 28)
   (let ((saved (excali--save-current-style '((currentItemStrokeColor . "#e03131")
                                             (currentItemEndArrowhead . :null)
                                             (viewBackgroundColor . "#ffffff")))))
     (should (equal (alist-get 'currentItemStrokeColor saved) "#e03131"))
     (should (eq (alist-get 'currentItemEndArrowhead saved) :null))
     (should (equal (alist-get 'currentItemFontSize saved) 28))
     ;; Untouched defaults are not added.
     (should-not (assq 'currentItemFillStyle saved))
     (should (equal (alist-get 'viewBackgroundColor saved) "#ffffff")))))

(ert-deftest excali-test-style-applies-to-matching-elements ()
  "A property changes only the selected elements it applies to."
  (excali-test--with-elements
      ((box (excali-test--rect 0 0))
       (label (excali--make-text-element 0 30 "Hello")))
    (setq excali--backend nil)
    (excali--select (list box label))
    (let ((old-width (excali--get label 'width)))
      (excali-set-style 'fontSize 40)
      (should (= (excali--get label 'fontSize) 40))
      (should (> (excali--get label 'width) old-width))
      (should-not (assq 'fontSize box)))
    (excali-set-style 'backgroundColor "#a5d8ff")
    (should (equal (excali--get box 'backgroundColor) "#a5d8ff"))
    (should (equal (excali--get label 'backgroundColor) "transparent"))
    (excali-set-style 'strokeColor "#1971c2")
    (should (equal (excali--get label 'strokeColor) "#1971c2"))
    (excali-set-style 'roundness "sharp")
    (should (eq (alist-get 'roundness box) :null))
    ;; The panel reports mixed values across the selection.
    (excali--set-element-style box 'opacity 50)
    (should (eq (excali--shown-style-value 'opacity) 'mixed))
    (should (string-match-p "mixed" (excali--style-description 'opacity)))))

(ert-deftest excali-test-new-elements-take-current-style ()
  "New elements get the current style, with the right roundness type."
  (excali-test--with-scene
   (excali--load-current-style nil)
   (excali-set-style 'strokeWidth "bold")
   (excali-set-style 'endArrowhead "triangle")
   (let ((rect (excali--apply-current-style (excali--make-element "rectangle" 0 0)))
         (arrow (excali--apply-current-style
                 (excali--make-element "arrow" 0 0 (cons 'points [[0.0 0.0] [9.0 0.0]]))))
         (line (excali--apply-current-style
                (excali--make-element "line" 0 0 (cons 'points [[0.0 0.0] [9.0 0.0]])))))
     (should (= (excali--get rect 'strokeWidth) 4))
     ;; Freedraw uses the thinner scale; diamonds round proportionally.
     (should (= (excali--get (excali--apply-current-style
                             (excali--make-element "freedraw" 0 0 (cons 'points [[0.0 0.0]])))
                            'strokeWidth)
                2))
     (should (equal (excali--get (excali--apply-current-style
                                 (excali--make-element "diamond" 0 0))
                                'roundness)
                    '((type . 2))))
     (should (equal (excali--element-stroke-width-key rect) "bold"))
     (should (equal (excali--get rect 'roundness) '((type . 3))))
     (should (equal (excali--get arrow 'roundness) '((type . 2))))
     (should (equal (excali--get arrow 'endArrowhead) "triangle"))
     (should-not (excali--get line 'endArrowhead))
     (excali-set-style 'roundness "sharp")
     (should (eq (alist-get 'roundness
                            (excali--apply-current-style
                             (excali--make-element "diamond" 0 0)))
                 :null)))))

(provide 'excali-test)
;;; excali-test.el ends here
