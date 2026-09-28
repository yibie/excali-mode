;;; excal-test.el --- Batch tests for excal.el  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)

(defconst excal-test--sample
  (expand-file-name "sample.excalidraw"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest excal-test-roundtrip-preserves-fields ()
  "Saving an unmodified scene keeps every element field."
  (let ((out (make-temp-file "excal" nil ".excalidraw")))
    (unwind-protect
        (with-temp-buffer
          (setq excal--native-cache (make-hash-table :test #'eq)
                excal--doc (excal--read-file excal-test--sample)
                excal--elements (append (alist-get 'elements excal--doc) nil)
                excal--file out)
          (excal-save)
          (should (equal (excal--read-file out)
                         (excal--read-file excal-test--sample))))
      (delete-file out))))

(ert-deftest excal-test-measure-text ()
  "Two-line text is taller than one line at the same size."
  (let ((one (excal-native-measure-text "你好" 20 5 1.25))
        (two (excal-native-measure-text "你好\n世界" 20 5 1.25)))
    (should (> (car one) 0))
    (should (= (cdr one) 25.0))
    (should (= (cdr two) 50.0))))

(ert-deftest excal-test-render-writes-pixels ()
  "Rendering the sample changes pixels away from the white background."
  (with-temp-buffer
    (setq excal--native-cache (make-hash-table :test #'eq)
          excal--elements (append (alist-get 'elements
                                             (excal--read-file excal-test--sample))
                                  nil))
    (let* ((w 400) (h 300)
           (canvas (list 'image :type 'canvas :id (gensym) :data-width w
                         :data-height h :data (make-vector (* w h) 0)))
           (png (make-temp-file "excal" nil ".png")))
      (unwind-protect
          (progn
            (should (excal-native-render canvas w h 1.0 1.0 0.0 0.0
                                         (excal--visible-elements)))
            (should (excal-native-write-png canvas w h png))
            (should (> (file-attribute-size (file-attributes png)) 1000)))
        (delete-file png)))))

;; Tiles

(defun excal-test--tiles (fb-width fb-height size)
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

(ert-deftest excal-test-tiles-refresh-only-changes ()
  "Only tiles whose pixels changed are reported dirty."
  (with-temp-buffer
    (setq excal--native-cache (make-hash-table :test #'eq))
    (let* ((rect (excal--make-element "rectangle" 10 10
                                      (cons 'width 60.0) (cons 'height 40.0)))
           (fb (excal-native-fb-create 400 300))
           (tiles (excal-test--tiles 400 300 100)))
      (setq excal--elements (list rect))
      (excal-native-fb-render fb 1.0 1.0 0.0 0.0 (excal--visible-elements) nil)
      ;; First present fills every tile.
      (should (= (length (excal-native-fb-present-tiles fb tiles)) 12))
      ;; Nothing changed: nothing is dirty.
      (should (null (excal-native-fb-present-tiles fb tiles)))
      ;; Move the rectangle within the first tile, repainting only damage.
      (let ((damage (excal--with-damage rect
                      (excal--put rect 'x 20.0)
                      (excal--touch rect))))
        (excal-native-fb-render fb 1.0 1.0 0.0 0.0 (excal--visible-elements)
                                (excal--damage-vector damage)))
      ;; Damage spans tiles 0 1 4 5, but pixels change only in tile 0.
      (should (equal (excal-native-fb-present-tiles fb tiles) '(0))))))

(ert-deftest excal-test-damage-render-matches-full ()
  "A damage-limited render matches a full render up to antialiasing."
  (random "excal-test")
  (with-temp-buffer
    (setq excal--native-cache (make-hash-table :test #'eq))
    (let* ((rect (excal--make-element "ellipse" 50 50
                                      (cons 'width 80.0) (cons 'height 60.0)))
           (other (excal--make-element "rectangle" 200 150
                                       (cons 'width 80.0) (cons 'height 60.0)))
           (partial (excal-native-fb-create 400 300))
           (full (excal-native-fb-create 400 300)))
      (setq excal--elements (list rect other))
      (excal-native-fb-render partial 1.0 1.0 0.0 0.0
                              (excal--visible-elements) nil)
      (let ((damage (excal--with-damage rect
                      (excal--put rect 'x 90.0)
                      (excal--touch rect))))
        (excal-native-fb-render partial 1.0 1.0 0.0 0.0
                                (excal--visible-elements)
                                (excal--damage-vector damage)))
      (excal-native-fb-render full 1.0 1.0 0.0 0.0
                              (excal--visible-elements) nil)
      ;; Antialiased edges crossing the damage clip may differ slightly.
      (should (<= (excal-native-fb-diff partial full) 16)))))

(ert-deftest excal-test-culling-skips-offscreen ()
  "Elements outside the viewport are not drawn."
  (with-temp-buffer
    (setq excal--native-cache (make-hash-table :test #'eq))
    (setq excal--elements
          (list (excal--make-element "rectangle" 10 10 (cons 'width 50.0) (cons 'height 50.0))
                (excal--make-element "rectangle" 5000 5000 (cons 'width 50.0) (cons 'height 50.0))))
    (should (= 1 (excal-native-fb-render (excal-native-fb-create 200 200)
                                         1.0 1.0 0.0 0.0
                                         (excal--visible-elements) nil)))))

;; Resizing

(defmacro excal-test--with-scene (&rest body)
  "Run BODY in a temp buffer set up like an excal buffer at zoom 1."
  `(with-temp-buffer
     (setq excal--native-cache (make-hash-table :test #'eq)
           excal--zoom 1.0)
     ,@body))

;; Pointer shape

(ert-deftest excal-test-pointer-follows-scene ()
  "The pointer reflects handles, elements, empty space and the tool."
  (excal-test--with-scene
   (let ((rect (excal--make-element "rectangle" 10 20
                                    (cons 'width 100.0) (cons 'height 50.0)
                                    (cons 'backgroundColor "#a5d8ff"))))
     (setq excal--elements (list rect)
           excal--scroll-x 0.0 excal--scroll-y 0.0
           excal--tool 'select)
     (should (eq (excal--cursor-at '(60.0 . 45.0)) 'move))
     (should (eq (excal--cursor-at '(300.0 . 300.0)) 'default))
     ;; Handles only count once the element is selected.
     (should (eq (excal--cursor-at '(60.0 . 76.0)) 'move))
     (setq excal--selection (list rect))
     (should (eq (excal--cursor-at '(60.0 . 76.0)) 'ns-resize))
     (should (eq (excal--cursor-at '(116.0 . 45.0)) 'ew-resize))
     (should (eq (excal--cursor-at '(116.0 . 76.0)) 'nwse-resize))
     (setq excal--tool 'text)
     (should (eq (excal--cursor-at '(60.0 . 45.0)) 'crosshair))
     (setq excal--tool 'rectangle)
     (should (eq (excal--cursor-at '(60.0 . 45.0)) 'crosshair)))))

(ert-deftest excal-test-set-pointer-is-silent ()
  "Changing the pointer does not mark the buffer modified."
  (excal-test--with-scene
   (insert "  ")
   (set-buffer-modified-p nil)
   (excal--set-pointer 'pointer)
   (should (eq (get-text-property 1 'pointer) 'hand))
   (should-not (buffer-modified-p))))

;; Scroll reuse

(defmacro excal-test--with-view (width height &rest body)
  "Run BODY with a WIDTH by HEIGHT framebuffer and a random busy scene."
  (declare (indent 2))
  `(excal-test--with-scene
    (random "excal-scroll")
    (setq excal--pixel-scale 1.0 excal--scroll-x 0.0 excal--scroll-y 0.0
          excal--backend nil
          excal--canvas-size (cons ,width ,height)
          excal--fb (excal-native-fb-create ,width ,height)
          excal--elements (excal--stress-elements 60))
    ,@body))

(defun excal-test--full-render-diff ()
  "Return the largest difference between the framebuffer and a full render."
  (let ((full (excal-native-fb-create (car excal--canvas-size)
                                      (cdr excal--canvas-size))))
    (excal-native-fb-render full excal--pixel-scale excal--zoom
                            excal--scroll-x excal--scroll-y
                            (excal--visible-elements) nil)
    (excal-native-fb-diff excal--fb full)))

(ert-deftest excal-test-scroll-reuse-matches-full ()
  "Panning by whole pixels repaints only strips yet matches a full render."
  (excal-test--with-view 400 300
    (excal--render)
    (dolist (delta '((7 . 5) (-13 . 0) (0 . -9) (-3 . 11)))
      (excal--pan (car delta) (cdr delta))
      (should (< (plist-get excal--last-stats :drawn) 60)))
    (should (<= (excal-test--full-render-diff) 16))))

(ert-deftest excal-test-scroll-reuse-at-zoom ()
  "Reuse also holds at a non-integer zoom and 2x pixel scale."
  (excal-test--with-view 400 300
    (setq excal--zoom 1.37 excal--pixel-scale 2.0
          excal--scroll-x 3.3 excal--scroll-y -8.1)
    (excal--render)
    (dotimes (_ 5) (excal--pan 4 -3))
    (should (<= (excal-test--full-render-diff) 16))))

(ert-deftest excal-test-fractional-pan-accumulates ()
  "Sub-pixel wheel deltas wait until they add up to a whole pixel."
  (excal-test--with-view 200 100
    (excal--render)
    (let ((origin excal--rendered-origin))
      (excal--pan 0.4 0.0)
      (should (equal excal--rendered-origin origin))
      (excal--pan 0.7 0.0)
      (should (= excal--scroll-x 1.0))
      (should (< (abs (- (car excal--pan-remainder) 0.1)) 1e-9)))))

(ert-deftest excal-test-plan-repaint ()
  "Unchanged views skip rendering; zoom changes and odd shifts repaint all."
  (excal-test--with-view 200 100
    (excal--render)
    (should (eq (excal--plan-repaint 'scroll) 'none))
    (setq excal--scroll-x (+ excal--scroll-x 0.5))
    (should (null (excal--plan-repaint 'scroll)))
    (setq excal--zoom 2.0)
    (should (null (excal--plan-repaint 'scroll)))
    (setq excal--scroll-x (+ excal--scroll-x 2.0))
    (should (equal (excal--plan-repaint 'scroll) [[0 0 4 100]]))))

;; Selection, history, clipboard, arrangement

(defun excal-test--rect (x y &rest props)
  "Return a 10x10 rectangle at X, Y with extra PROPS alist."
  (apply #'excal--make-element "rectangle" x y
         (cons 'width 10.0) (cons 'height 10.0) props))

(defmacro excal-test--with-elements (bindings &rest body)
  "Bind BINDINGS to elements forming the scene, then run BODY."
  (declare (indent 1))
  `(excal-test--with-scene
    (let* ,bindings
      (setq excal--elements (list ,@(mapcar #'car bindings))
            excal--selection nil excal--editing-group nil)
      ,@body)))

(ert-deftest excal-test-group-units ()
  "Clicking a grouped element selects its outermost group until entered."
  (excal-test--with-elements
      ((a (excal-test--rect 0 0 (cons 'groupIds ["inner" "outer"])))
       (b (excal-test--rect 20 0 (cons 'groupIds ["inner" "outer"])))
       (c (excal-test--rect 40 0 (cons 'groupIds ["outer"])))
       (d (excal-test--rect 60 0)))
    (should (equal (excal--unit a) (list a b c)))
    (should (equal (excal--unit d) (list d)))
    ;; Entering the outer group exposes the inner group as the unit.
    (setq excal--editing-group "outer")
    (should (equal (excal--unit a) (list a b)))
    (should (equal (excal--unit c) (list c)))
    (setq excal--editing-group "inner")
    (should (equal (excal--unit a) (list a)))))

(ert-deftest excal-test-toggle-and-marquee ()
  "Shift-click toggles units; the marquee takes only units fully inside."
  (excal-test--with-elements
      ((a (excal-test--rect 0 0))
       (b (excal-test--rect 20 0))
       (c (excal-test--rect 40 0 (cons 'groupIds ["g"])))
       (d (excal-test--rect 80 0 (cons 'groupIds ["g"]))))
    (excal--toggle-unit b)
    (excal--toggle-unit a)
    (should (equal excal--selection (list a b)))
    (excal--toggle-unit b)
    (should (equal excal--selection (list a)))
    ;; The marquee covers c but only half of its group.
    (should (equal (excal--marquee-selection '(-1 -1 55 11)) (list a b)))
    (should (equal (excal--marquee-selection '(-1 -1 95 11)) (list a b c d)))))

(ert-deftest excal-test-undo-redo ()
  "Commands commit once per change; undo and redo restore scenes."
  (excal-test--with-elements ((a (excal-test--rect 0 0)))
    (excal--history-reset)
    (excal--commit)
    (should (= (length excal--undo-stack) 1))
    (excal--select (list a))
    (excal--nudge 5 0)
    (excal--commit)
    (let ((b (excal-test--rect 50 50)))
      (setq excal--elements (append excal--elements (list b))))
    (excal--commit)
    (should (= (length excal--undo-stack) 3))
    ;; The unchanged element shares one frozen copy across snapshots.
    (should (eq (car (plist-get (nth 0 excal--undo-stack) :elements))
                (car (plist-get (nth 1 excal--undo-stack) :elements))))
    (excal-undo)
    (should (= (length excal--elements) 1))
    (should (= (excal--get (car excal--elements) 'x) 5.0))
    (excal-undo)
    (should (= (excal--get (car excal--elements) 'x) 0.0))
    (excal-redo)
    (excal-redo)
    (should (= (length excal--elements) 2))
    ;; Undoing then editing drops the redo branch.
    (excal-undo)
    (excal--select (list (car excal--elements)))
    (excal--nudge 1 0)
    (excal--commit)
    (should (null excal--redo-stack))
    ;; Restored elements are fresh copies: editing them leaves history intact.
    (should (= (excal--get (car (plist-get (nth 1 excal--undo-stack) :elements)) 'x)
               5.0))))

(ert-deftest excal-test-clipboard-roundtrip ()
  "Copied elements paste with new ids and remapped internal references."
  (excal-test--with-elements
      ((box (excal-test--rect 0 0 (cons 'groupIds ["g"])
                              (cons 'boundElements [((id . "label") (type . "text"))])))
       (label (excal--make-text-element 2 2 "hi"))
       (arrow (excal--make-element
               "arrow" 20 5 (cons 'points [[0.0 0.0] [30.0 0.0]])
               (cons 'groupIds ["g"])
               (cons 'startBinding (list (cons 'elementId (excal--get box 'id))
                                         (cons 'focus 0) (cons 'gap 1)))
               (cons 'endBinding (list (cons 'elementId "elsewhere")
                                       (cons 'focus 0) (cons 'gap 1))))))
    (excal--put label 'id "label")
    (excal--put label 'containerId (excal--get box 'id))
    (let* ((json (excal--clipboard-json (list box label arrow)))
           (parsed (excal--parse-clipboard json))
           (clones (excal--clone-elements parsed)))
      (should (= (length parsed) 3))
      (pcase-let ((`(,box2 ,label2 ,arrow2) clones))
        (should-not (equal (excal--get box2 'id) (excal--get box 'id)))
        (should (equal (excal--get label2 'containerId) (excal--get box2 'id)))
        (should (equal (alist-get 'id (aref (excal--get box2 'boundElements) 0))
                       (excal--get label2 'id)))
        (should (equal (alist-get 'elementId (excal--get arrow2 'startBinding))
                       (excal--get box2 'id)))
        ;; References leaving the pasted set are dropped.
        (should (eq (alist-get 'endBinding arrow2) :null))
        ;; Both grouped clones share one new group.
        (should (equal (excal--get box2 'groupIds) (excal--get arrow2 'groupIds)))
        (should-not (equal (excal--get box2 'groupIds) ["g"]))))
    (should-not (excal--parse-clipboard "just text"))))

(ert-deftest excal-test-duplicate ()
  "Duplicating selects offset copies on top of the scene."
  (excal-test--with-elements ((a (excal-test--rect 0 0)))
    (setq excal--backend nil)
    (excal--select (list a))
    (excal-duplicate)
    (should (= (length excal--elements) 2))
    (should (eq (car excal--selection) (cadr excal--elements)))
    (should (= (excal--get (car excal--selection) 'x) 10.0))))

(ert-deftest excal-test-z-order ()
  "Z-order commands move selected runs past their neighbours."
  (excal-test--with-elements
      ((a (excal-test--rect 0 0)) (b (excal-test--rect 0 0))
       (c (excal-test--rect 0 0)) (d (excal-test--rect 0 0)))
    (setq excal--backend nil)
    (excal--select (list a b))
    (excal-bring-forward)
    (should (equal excal--elements (list c a b d)))
    (excal-bring-to-front)
    (should (equal excal--elements (list c d a b)))
    (excal-send-backward)
    (should (equal excal--elements (list c a b d)))
    (excal-send-to-back)
    (should (equal excal--elements (list a b c d)))))

(ert-deftest excal-test-group-ungroup ()
  "Grouping adds an outermost group; ungrouping removes it."
  (excal-test--with-elements
      ((a (excal-test--rect 0 0 (cons 'groupIds ["old"])))
       (b (excal-test--rect 20 0)))
    (setq excal--backend nil)
    (excal--select (list a b))
    (excal-group)
    (let ((group (aref (excal--get b 'groupIds) 0)))
      (should (equal (excal--get a 'groupIds) (vector "old" group)))
      (should (equal (excal--unit b) (list a b)))
      (excal-ungroup)
      (should (equal (excal--get a 'groupIds) ["old"]))
      (should (equal (excal--get b 'groupIds) [])))))
;; Zoom preview

(defun excal-test--full-render ()
  "Return a new framebuffer holding a full render of the current view."
  (let ((full (excal-native-fb-create (car excal--canvas-size)
                                      (cdr excal--canvas-size))))
    (excal-native-fb-render full excal--pixel-scale excal--zoom
                            excal--scroll-x excal--scroll-y
                            (excal--visible-elements) nil)
    full))

(defun excal-test--preview-timers ()
  "Return the pending timers that finish a zoom preview."
  (cl-remove-if-not (lambda (timer)
                      (eq (timer--function timer) #'excal--finish-preview))
                    timer-list))

(defmacro excal-test--with-preview (&rest body)
  "Run BODY in a 400x300 view zoomed out to show a busy scene.
Temporary buffers skip `kill-buffer-hook', so cancel the preview here."
  `(excal-test--with-view 400 300
     (let ((excal-zoom-preview-delay 0.1)
           (excal-zoom-preview-limit 4.0))
       (setq excal--zoom 0.5)
       (excal--render)
       (unwind-protect (progn ,@body)
         (excal--cancel-preview)))))

(ert-deftest excal-test-zoom-preview-approximates-full ()
  "A preview is much closer to a full render than the stale pixels are."
  (dolist (factor '(1.1 0.9 1.3 0.75))
    (excal-test--with-preview
     (let ((stale (excal-test--full-render)))
       (excal--zoom-at factor '(130 . 90))
       (should (plist-get excal--last-stats :preview))
       (should (= (plist-get excal--last-stats :drawn) 0))
       (let* ((full (excal-test--full-render))
              (preview (excal-native-fb-mean-diff excal--fb full))
              (unchanged (excal-native-fb-mean-diff stale full)))
         (should (< preview 3.0))
         (should (< preview (* 0.5 unchanged))))))))

(ert-deftest excal-test-zoom-preview-does-not-drift ()
  "Previews scale the last full render, so zooming back restores it."
  (excal-test--with-preview
   (let ((original (excal-test--full-render)))
     (dotimes (_ 4) (excal--zoom-at 1.1 '(130 . 90)))
     (dotimes (_ 4) (excal--zoom-at (/ 1 1.1) '(130 . 90)))
     (should (plist-get excal--last-stats :preview))
     (should (<= (excal-native-fb-diff excal--fb original) 2)))))

(ert-deftest excal-test-zoom-preview-timer-renders-full ()
  "The pending timer's function turns the preview into a full render."
  (excal-test--with-preview
   (excal--zoom-at 1.2 '(200 . 150))
   (excal--zoom-at 1.2 '(50 . 40))
   (should (timerp excal--preview-timer))
   (should (equal (excal-test--preview-timers) (list excal--preview-timer)))
   (should (equal (timer--args excal--preview-timer) (list (current-buffer))))
   (funcall (timer--function excal--preview-timer) (current-buffer))
   (should-not excal--preview-timer)
   (should-not excal--preview-origin)
   (should-not (plist-get excal--last-stats :preview))
   (should (equal excal--rendered-origin (excal--view-origin)))
   (should (<= (excal-test--full-render-diff) 16))))

(ert-deftest excal-test-zoom-preview-blocks-scroll-reuse ()
  "Preview pixels are never shifted as if they were exact."
  (excal-test--with-preview
   (excal--zoom-at 1.25 '(100 . 100))
   (should-not excal--rendered-origin)
   (should (null (excal--plan-repaint 'scroll)))
   ;; Damage during a preview repaints everything and ends it.
   (excal--zoom-at 1.25 '(100 . 100))
   (excal--render '(0 0 10 10))
   (should-not excal--preview-timer)
   (should-not (plist-get excal--last-stats :preview))
   (should (<= (excal-test--full-render-diff) 16))))

(ert-deftest excal-test-zoom-preview-pan ()
  "Pans during a preview keep previewing, then settle to a full render."
  (excal-test--with-preview
   (excal--zoom-at 1.2 '(100 . 100))
   (excal--pan 7 -5)
   (should (plist-get excal--last-stats :preview))
   (should-not excal--rendered-origin)
   (excal--finish-preview (current-buffer))
   (should-not (excal-test--preview-timers))
   (should (<= (excal-test--full-render-diff) 16))
   ;; With the preview gone, pans reuse pixels again.
   (excal--pan 7 -5)
   (should-not (plist-get excal--last-stats :preview))
   (should (<= (excal-test--full-render-diff) 16))))

(ert-deftest excal-test-zoom-preview-falls-back ()
  "Large factors, disabled previews and fresh framebuffers render fully."
  (excal-test--with-preview
   (excal--zoom-at 5.0 '(0 . 0))
   (should-not (plist-get excal--last-stats :preview))
   (should-not excal--preview-timer)
   (let ((excal-zoom-preview-delay nil))
     (excal--zoom-at 1.1 '(0 . 0))
     (should-not (plist-get excal--last-stats :preview)))
   (setq excal--rendered-origin nil)
   (excal--zoom-at 1.1 '(0 . 0))
   (should-not (plist-get excal--last-stats :preview))
   (should (<= (excal-test--full-render-diff) 16))))

(ert-deftest excal-test-zoom-preview-timer-cleanup ()
  "Steps share one timer, and killing the buffer cancels it."
  (let ((buffer (generate-new-buffer "excal-test-preview"))
        timer)
    (unwind-protect
        (with-current-buffer buffer
          (setq excal--native-cache (make-hash-table :test #'eq)
                excal--zoom 0.5 excal--pixel-scale 1.0
                excal--canvas-size (cons 200 100)
                excal--fb (excal-native-fb-create 200 100)
                excal--elements (excal--stress-elements 20))
          (excal--render)
          (dotimes (_ 5) (excal--zoom-at 1.05 '(10 . 10)))
          (setq timer excal--preview-timer)
          (should (timerp timer))
          (should (equal (excal-test--preview-timers) (list timer))))
      (kill-buffer buffer))
    (should-not (memq timer timer-list))
    (should-not (excal-test--preview-timers))
    ;; A timer outliving its buffer does nothing.
    (excal--finish-preview buffer)))

(ert-deftest excal-test-zoom-preview-fills-white ()
  "Pixels the scaled image does not cover are white."
  (excal-test--with-preview
   (let ((blank (excal-native-fb-create 400 300)))
     (excal-native-fb-render blank 1.0 1.0 0.0 0.0 [] nil)
     (should (> (excal-native-fb-mean-diff excal--fb blank) 0))
     (should (excal-native-fb-zoom-preview excal--fb excal--fb 0.5 400.0 0.0))
     (should (= (excal-native-fb-diff excal--fb blank) 0)))))

(ert-deftest excal-test-backend-resolution ()
  "`auto' picks `layer' on macOS frames with the module, else `tiles'."
  (let ((excal-backend 'auto))
    ;; Batch frames are not graphical.
    (should (eq (excal--resolve-backend) 'tiles))
    (cl-letf (((symbol-function 'excal--layer-frame-p) #'always))
      (should (eq (excal--resolve-backend)
                  (if (fboundp 'excal-native-layer-create) 'layer 'tiles)))))
  (cl-letf (((symbol-function 'excal--layer-frame-p) #'always))
    (dolist (choice '(canvas tiles))
      (let ((excal-backend choice))
        (should (eq (excal--resolve-backend) choice))))))

;; End-to-end mouse gestures with synthesized events

(defun excal-test--posn (x y)
  "Return a text-area mouse position at window pixel X, Y."
  (list (selected-window) 1 (cons x y) 0 nil 1 '(0 . 0) nil '(0 . 0) '(1 . 1)))

(defun excal-test--drag (x0 y0 x1 y1 &optional modifiers)
  "Run `excal-mouse-down' for a drag from X0,Y0 to X1,Y1 in window pixels.
MODIFIERS, such as (shift), are added to the press event."
  (let ((start (excal-test--posn x0 y0)))
    (setq unread-command-events
          (append (cl-loop for i from 1 to 4
                           collect (list 'mouse-movement
                                         (excal-test--posn
                                          (+ x0 (/ (* i (- x1 x0)) 4))
                                          (+ y0 (/ (* i (- y1 y0)) 4)))))
                  (list (list 'drag-mouse-1 start (excal-test--posn x1 y1)))))
    (excal-mouse-down
     (list (event-convert-list (append modifiers '(down-mouse-1))) start))))

(defmacro excal-test--in-window (&rest body)
  "Run BODY in an excal-like buffer shown in the selected window."
  `(let ((buffer (generate-new-buffer " *excal-test*")))
     (unwind-protect
         (save-window-excursion
           (switch-to-buffer buffer)
           (setq excal--native-cache (make-hash-table :test #'eq)
                 excal--zoom 1.0 excal--pixel-scale 1.0
                 excal--scroll-x 0.0 excal--scroll-y 0.0
                 excal--backend nil excal--tool 'select
                 excal--selection nil excal--editing-group nil)
           ,@body)
       (kill-buffer buffer))))

(ert-deftest excal-test-gesture-box-select-then-move-from-gap ()
  "Box-select two shapes, then drag from the gap between them to move both."
  (excal-test--in-window
   (let ((a (excal-test--rect 10 10)) (b (excal-test--rect 50 10)))
     (setq excal--elements (list a b))
     ;; Box selection from empty space around both.
     (excal-test--drag 0 0 70 30)
     (should (equal excal--selection (list a b)))
     ;; Press in the gap at x=35, which hits neither shape, and drag.
     (should-not (excal--hit '(35.0 . 15.0)))
     (excal-test--drag 35 15 55 45)
     (should (equal excal--selection (list a b)))
     (should (equal (list (excal--get a 'x) (excal--get a 'y)) '(30.0 40.0)))
     (should (equal (list (excal--get b 'x) (excal--get b 'y)) '(70.0 40.0))))))

(ert-deftest excal-test-gesture-click-outside-box-deselects ()
  "Pressing outside the selection box starts a new box selection."
  (excal-test--in-window
   (let ((a (excal-test--rect 10 10)) (b (excal-test--rect 50 10)))
     (setq excal--elements (list a b))
     (excal--select (list a b))
     (excal-test--drag 200 200 210 210)
     (should (null excal--selection))
     (should (= (excal--get a 'x) 10.0)))))

(ert-deftest excal-test-gesture-drag-unselected-shape ()
  "Dragging an unselected shape selects and moves only it."
  (excal-test--in-window
   (let ((a (excal-test--rect 10 10)) (b (excal-test--rect 50 10)))
     (setq excal--elements (list a b))
     (excal--select (list a))
     (excal-test--drag 55 15 65 25)
     (should (equal excal--selection (list b)))
     (should (= (excal--get a 'x) 10.0))
     (should (= (excal--get b 'x) 60.0)))))

(ert-deftest excal-test-gesture-shift-click-adds ()
  "Shift-clicking a shape adds it to the selection."
  (excal-test--in-window
   (let ((a (excal-test--rect 10 10)) (b (excal-test--rect 50 10)))
     (setq excal--elements (list a b))
     (excal--select (list a))
     (excal-test--drag 55 15 55 15 '(shift))
     (should (equal excal--selection (list a b))))))

;; Style

(ert-deftest excal-test-style-app-state-roundtrip ()
  "currentItem* app state loads into the current style and saves back."
  (excal-test--with-scene
   (excal--load-current-style '((currentItemStrokeColor . "#e03131")
                                (currentItemEndArrowhead . :null)
                                (viewBackgroundColor . "#ffffff")))
   (should (equal (excal--style-value 'strokeColor) "#e03131"))
   (should (null (excal--style-value 'endArrowhead)))
   (should (equal (excal--style-value 'fontSize) 20))
   (excal-set-style 'fontSize 28)
   (let ((saved (excal--save-current-style '((currentItemStrokeColor . "#e03131")
                                             (currentItemEndArrowhead . :null)
                                             (viewBackgroundColor . "#ffffff")))))
     (should (equal (alist-get 'currentItemStrokeColor saved) "#e03131"))
     (should (eq (alist-get 'currentItemEndArrowhead saved) :null))
     (should (equal (alist-get 'currentItemFontSize saved) 28))
     ;; Untouched defaults are not added.
     (should-not (assq 'currentItemFillStyle saved))
     (should (equal (alist-get 'viewBackgroundColor saved) "#ffffff")))))

(ert-deftest excal-test-style-applies-to-matching-elements ()
  "A property changes only the selected elements it applies to."
  (excal-test--with-elements
      ((box (excal-test--rect 0 0))
       (label (excal--make-text-element 0 30 "Hello")))
    (setq excal--backend nil)
    (excal--select (list box label))
    (let ((old-width (excal--get label 'width)))
      (excal-set-style 'fontSize 40)
      (should (= (excal--get label 'fontSize) 40))
      (should (> (excal--get label 'width) old-width))
      (should-not (assq 'fontSize box)))
    (excal-set-style 'backgroundColor "#a5d8ff")
    (should (equal (excal--get box 'backgroundColor) "#a5d8ff"))
    (should (equal (excal--get label 'backgroundColor) "transparent"))
    (excal-set-style 'strokeColor "#1971c2")
    (should (equal (excal--get label 'strokeColor) "#1971c2"))
    (excal-set-style 'roundness "sharp")
    (should (eq (alist-get 'roundness box) :null))
    ;; The panel reports mixed values across the selection.
    (excal--set-element-style box 'opacity 50)
    (should (eq (excal--shown-style-value 'opacity) 'mixed))
    (should (string-match-p "mixed" (excal--style-description 'opacity)))))

(ert-deftest excal-test-new-elements-take-current-style ()
  "New elements get the current style, with the right roundness type."
  (excal-test--with-scene
   (excal--load-current-style nil)
   (excal-set-style 'strokeWidth "bold")
   (excal-set-style 'endArrowhead "triangle")
   (let ((rect (excal--apply-current-style (excal--make-element "rectangle" 0 0)))
         (arrow (excal--apply-current-style
                 (excal--make-element "arrow" 0 0 (cons 'points [[0.0 0.0] [9.0 0.0]]))))
         (line (excal--apply-current-style
                (excal--make-element "line" 0 0 (cons 'points [[0.0 0.0] [9.0 0.0]])))))
     (should (= (excal--get rect 'strokeWidth) 4))
     ;; Freedraw uses the thinner scale; diamonds round proportionally.
     (should (= (excal--get (excal--apply-current-style
                             (excal--make-element "freedraw" 0 0 (cons 'points [[0.0 0.0]])))
                            'strokeWidth)
                2))
     (should (equal (excal--get (excal--apply-current-style
                                 (excal--make-element "diamond" 0 0))
                                'roundness)
                    '((type . 2))))
     (should (equal (excal--element-stroke-width-key rect) "bold"))
     (should (equal (excal--get rect 'roundness) '((type . 3))))
     (should (equal (excal--get arrow 'roundness) '((type . 2))))
     (should (equal (excal--get arrow 'endArrowhead) "triangle"))
     (should-not (excal--get line 'endArrowhead))
     (excal-set-style 'roundness "sharp")
     (should (eq (alist-get 'roundness
                            (excal--apply-current-style
                             (excal--make-element "diamond" 0 0)))
                 :null)))))

(provide 'excal-test)
;;; excal-test.el ends here
