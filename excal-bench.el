;;; excal-bench.el --- Benchmarks for excal  -*- lexical-binding: t; -*-

;;; Commentary:

;; Frame-time measurements for the presentation backends.

;;; Code:

(require 'excal-core)
(require 'excal-view)

(defun excal--stress-elements (count)
  "Return COUNT random elements for benchmarking."
  (cl-loop for i below count
           collect
           (let ((type (nth (% i 5) '("rectangle" "ellipse" "diamond" "arrow" "text")))
                 (x (float (random 2000))) (y (float (random 1400))))
             (pcase type
               ("arrow" (excal--make-element
                         type x y (cons 'points (vector [0 0] [80 30] [160 -20]))
                         (cons 'roundness (list (cons 'type 2)))))
               ("text" (excal--make-element
                        type x y (cons 'text "手绘 Excalidraw")
                        (cons 'fontSize 20) (cons 'fontFamily 5)
                        (cons 'width 150.0) (cons 'height 25.0)))
               (_ (excal--make-element
                   type x y (cons 'width 120.0) (cons 'height 80.0)
                   (cons 'backgroundColor "#a5d8ff")
                   (cons 'fillStyle (if (cl-evenp i) "hachure" "solid"))))))))

(defun excal--bench-frame (damage)
  "Render one benchmark frame with DAMAGE and push it to the screen."
  (excal--render damage)
  (redisplay t)
  (when (eq excal--backend 'layer)
    (excal-native-layer-flush)))

(defun excal--bench-run (frames step)
  "Time FRAMES frames produced by calling STEP with the frame index.
STEP returns the frame's damage."
  (let ((render 0.0) (present 0.0) (refreshed 0)
        (start (float-time)))
    (dotimes (i frames)
      (excal--bench-frame (funcall step i))
      (cl-incf render (plist-get excal--last-stats :render-ms))
      (cl-incf present (plist-get excal--last-stats :present-ms))
      (cl-incf refreshed (plist-get excal--last-stats :refreshed)))
    (let ((total (- (float-time) start)))
      (list :render-ms (/ render frames)
            :present-ms (/ present frames)
            :frame-ms (/ (* 1000 total) frames)
            :fps (/ frames total)
            :surfaces (/ (float refreshed) frames)))))

(defun excal-bench (&optional frames)
  "Measure panning and dragging over FRAMES frames with the current backend."
  (interactive)
  (let* ((frames (or frames 60))
         (target (or excal--selected
                     (cl-find-if (lambda (e) (not (excal--get e 'isDeleted)))
                                 excal--elements)))
         (step (lambda (i) (if (< i (/ frames 2)) 1 -1)))
         (pan-full (excal--bench-run
                    frames
                    (lambda (i)
                      (cl-incf excal--scroll-x (* 3.0 (funcall step i)))
                      (cl-incf excal--scroll-y (* 2.0 (funcall step i)))
                      'full)))
         (pan (excal--bench-run
               frames
               (lambda (i)
                 (cl-incf excal--scroll-x (* 3.0 (funcall step i)))
                 (cl-incf excal--scroll-y (* 2.0 (funcall step i)))
                 'scroll)))
         (drag (and target
                    (excal--bench-run
                     frames
                     (lambda (i)
                       (excal--with-damage target
                         (excal--put target 'x (+ (excal--get target 'x)
                                                  (if (< i (/ frames 2)) 3.0 -3.0)))
                         (excal--touch target))))))
         (result (list :backend excal--backend
                       :elements (length excal--elements)
                       :canvas excal--canvas-size
                       :pixel-scale excal--pixel-scale
                       :pan-full pan-full :pan pan :drag drag)))
    (message "excal-bench: %S" result)
    result))

(defun excal-bench-backends (&optional frames show)
  "Benchmark every backend on this scene and on a 500-element stress scene.
Results go to the *excal-bench* buffer, which is displayed when SHOW is
non-nil (as it is interactively), and are returned as a list.  Showing it
from a script would split the window and change the measured canvas."
  (interactive (list nil t))
  (let* ((frames (or frames 60))
         (backends (if (fboundp 'excal-native-layer-create)
                       '(canvas tiles layer)
                     '(canvas tiles)))
         (original excal--backend)
         (elements excal--elements)
         (results nil))
    (unwind-protect
        (dolist (scene '(sample stress))
          (when (eq scene 'stress)
            (setq excal--elements (append elements (excal--stress-elements 500))))
          (dolist (backend backends)
            (excal--use-backend backend)
            (redisplay t)
            (push (cons scene (excal-bench frames)) results)))
      (setq excal--elements elements)
      (excal--use-backend original))
    (setq results (nreverse results))
    ;; Read these before leaving the excal buffer: they are buffer-local.
    (let ((size excal--canvas-size) (scale excal--pixel-scale))
     (with-current-buffer (get-buffer-create "*excal-bench*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "canvas %S, pixel scale %.1f, %d frames\n\n"
                        size scale frames))
        (insert (format "%-7s %-7s %-8s %9s %10s %9s %6s %9s\n"
                        "scene" "backend" "op" "render" "present" "frame" "fps"
                        "surfaces"))
        (dolist (r results)
          (dolist (op '(:pan-full :pan :drag))
            (when-let* ((m (plist-get (cdr r) op)))
              (insert (format "%-7s %-7s %-8s %7.2fms %8.2fms %7.2fms %6.1f %9.1f\n"
                              (car r) (plist-get (cdr r) :backend)
                              (substring (symbol-name op) 1)
                              (plist-get m :render-ms) (plist-get m :present-ms)
                              (plist-get m :frame-ms) (plist-get m :fps)
                              (plist-get m :surfaces)))))))
      (special-mode)
      (when show (display-buffer (current-buffer)))))
    results))

(defun excal-bench-elisp-fill ()
  "Measure filling the whole canvas pixel by pixel from Elisp."
  (let* ((w (car excal--canvas-size)) (h (cdr excal--canvas-size))
         (data (make-vector (* w h) #xFFFFFFFF))
         (canvas (list 'image :type 'canvas :id (gensym "excal-elisp-")
                       :data-width w :data-height h :data data))
         (start (float-time)))
    (dotimes (i (* w h)) (aset data i (logior #xFF000000 (logand i #xFFFFFF))))
    (let ((fill (- (float-time) start)))
      (setq start (float-time))
      (canvas-refresh canvas 'reload-data)
      (list :elisp-fill-ms (* 1000 fill)
            :reload-ms (* 1000 (- (float-time) start))))))

(provide 'excal-bench)
;;; excal-bench.el ends here
