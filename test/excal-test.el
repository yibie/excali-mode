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

(ert-deftest excal-test-resize-rectangle ()
  "Dragging the south-east handle grows the box; the north-west can flip it."
  (excal-test--with-scene
   (let* ((rect (excal--make-element "rectangle" 10 20
                                     (cons 'width 100.0) (cons 'height 50.0)))
          (geometry (excal--geometry rect)))
     (should (= (length (excal--handles rect)) 8))
     ;; The se handle sits 6 px outside the corner.
     (should (eq (excal--hit-handle rect '(116.0 . 76.0)) 'se))
     (excal--resize rect 'se geometry 30 10)
     (should (equal (list (excal--get rect 'x) (excal--get rect 'y)
                          (excal--get rect 'width) (excal--get rect 'height))
                    '(10.0 20.0 130.0 60.0)))
     ;; Drag nw past the opposite corner: the box flips and stays positive.
     (excal--resize rect 'nw geometry 150 80)
     (should (equal (list (excal--get rect 'x) (excal--get rect 'y)
                          (excal--get rect 'width) (excal--get rect 'height))
                    '(110.0 70.0 50.0 30.0))))))

(ert-deftest excal-test-resize-arrow-scales-points ()
  "Resizing a multi-point arrow scales every point."
  (excal-test--with-scene
   (let* ((arrow (excal--make-element "arrow" 0 0
                                      (cons 'points (vector [0.0 0.0] [50.0 25.0]
                                                            [100.0 0.0]))))
          (geometry (excal--geometry arrow)))
     (excal--linear-extent arrow)
     (setq geometry (excal--geometry arrow))
     (excal--resize arrow 'e geometry 100 0)
     (should (equal (excal--get arrow 'points)
                    [[0.0 0.0] [100.0 25.0] [200.0 0.0]]))
     (should (= (excal--get arrow 'width) 200.0)))))

(ert-deftest excal-test-resize-text-scales-font ()
  "Corner-resizing text scales its font, anchored at the opposite corner."
  (excal-test--with-scene
   (let ((text (excal--make-element "text" 0 0 (cons 'fontSize 20)
                                    (cons 'fontFamily 5) (cons 'lineHeight 1.25))))
     (excal--set-text text "Hello")
     (should (= (length (excal--handles text)) 4))
     (let ((geometry (excal--geometry text)))
       (excal--resize text 'se geometry 0 25)
       (should (= (excal--get text 'fontSize) 40.0))
       (should (= (excal--get text 'height) 50.0))
       (should (= (excal--get text 'x) 0.0))))))

(ert-deftest excal-test-rotated-has-no-handles ()
  "Rotated elements are not resizable yet."
  (excal-test--with-scene
   (should-not (excal--handles
                (excal--make-element "rectangle" 0 0 (cons 'width 10.0)
                                     (cons 'height 10.0) (cons 'angle 0.3))))))

;; Pointer shape

(ert-deftest excal-test-pointer-follows-scene ()
  "The pointer reflects handles, elements, empty space and the tool."
  (excal-test--with-scene
   (let ((rect (excal--make-element "rectangle" 10 20
                                    (cons 'width 100.0) (cons 'height 50.0))))
     (setq excal--elements (list rect)
           excal--scroll-x 0.0 excal--scroll-y 0.0
           excal--tool 'select)
     (should (eq (excal--pointer-at '(60.0 . 45.0)) 'hand))
     (should (eq (excal--pointer-at '(300.0 . 300.0)) 'arrow))
     ;; Handles only count once the element is selected.
     (should (eq (excal--pointer-at '(60.0 . 76.0)) 'hand))
     (setq excal--selected rect)
     (should (eq (excal--pointer-at '(60.0 . 76.0)) 'nhdrag))
     (should (eq (excal--pointer-at '(116.0 . 45.0)) 'hdrag))
     (should (eq (excal--pointer-at '(116.0 . 76.0)) 'hdrag))
     (setq excal--tool 'text)
     (should (eq (excal--pointer-at '(60.0 . 45.0)) 'text))
     (setq excal--tool 'rectangle)
     (should (eq (excal--pointer-at '(60.0 . 45.0)) 'arrow)))))

(ert-deftest excal-test-set-pointer-is-silent ()
  "Changing the pointer does not mark the buffer modified."
  (excal-test--with-scene
   (insert "  ")
   (set-buffer-modified-p nil)
   (excal--set-pointer 'hand)
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

;;; excal-test.el ends here
