;;; excali-bench.el --- Benchmarks for excali  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Frame-time measurements for the presentation backends.

;;; Code:

(require 'excali-core)
(require 'excali-view)

(defun excali--stress-elements (count)
  "Return COUNT random elements for benchmarking."
  (cl-loop for i below count
           collect
           (let ((type (nth (% i 5) '("rectangle" "ellipse" "diamond" "arrow" "text")))
                 (x (float (random 2000))) (y (float (random 1400))))
             (pcase type
               ("arrow" (excali--make-element
                         type x y (cons 'points (vector [0 0] [80 30] [160 -20]))
                         (cons 'roundness (list (cons 'type 2)))))
               ("text" (excali--make-element
                        type x y (cons 'text "手绘 Excalidraw")
                        (cons 'fontSize 20) (cons 'fontFamily 5)
                        (cons 'width 150.0) (cons 'height 25.0)))
               (_ (excali--make-element
                   type x y (cons 'width 120.0) (cons 'height 80.0)
                   (cons 'backgroundColor "#a5d8ff")
                   (cons 'fillStyle (if (cl-evenp i) "hachure" "solid"))))))))

(defun excali--bench-frame (damage)
  "Render one benchmark frame with DAMAGE and push it to the screen.
DAMAGE `preview' shows the frame as a zoom preview instead."
  (if (eq damage 'preview)
      (excali--render-preview)
    (excali--render damage))
  (redisplay t)
  (when (eq excali--backend 'layer)
    (excali-native-layer-flush)))

(defun excali--bench-run (frames step)
  "Time FRAMES frames produced by calling STEP with the frame index.
STEP returns the frame's damage; see `excali--bench-frame'."
  (let ((render 0.0) (present 0.0) (refreshed 0)
        (start (float-time)))
    (dotimes (i frames)
      (excali--bench-frame (funcall step i))
      (cl-incf render (plist-get excali--last-stats :render-ms))
      (cl-incf present (plist-get excali--last-stats :present-ms))
      (cl-incf refreshed (plist-get excali--last-stats :refreshed)))
    (let ((total (- (float-time) start)))
      (list :render-ms (/ render frames)
            :present-ms (/ present frames)
            :frame-ms (/ (* 1000 total) frames)
            :fps (/ frames total)
            :surfaces (/ (float refreshed) frames)))))

(defun excali--bench-zoom (frames preview)
  "Time FRAMES zoom steps about the view centre.
Zoom in by 3% per step for the first half and back out for the second,
as zoom previews when PREVIEW is non-nil and as full renders otherwise.
Afterwards restore the view and render it fully."
  (let ((zoom excali--zoom) (x excali--scroll-x) (y excali--scroll-y)
        (center (cons (/ (car excali--canvas-size) excali--pixel-scale 2)
                      (/ (cdr excali--canvas-size) excali--pixel-scale 2))))
    (unwind-protect
        (excali--bench-run
         frames
         (lambda (i)
           (excali--zoom-view (if (< i (/ frames 2)) 1.03 (/ 1 1.03)) center)
           (if preview 'preview 'full)))
      (setq excali--zoom zoom excali--scroll-x x excali--scroll-y y)
      (excali--render))))

(defun excali-bench (&optional frames)
  "Measure panning, dragging and zooming over FRAMES frames.
Use the current backend.  `zoom' steps are zoom previews, `zoom-full'
renders every step fully for comparison."
  (interactive)
  (let* ((frames (or frames 60))
         (target (or (car excali--selection)
                     (cl-find-if (lambda (e) (not (excali--get e 'isDeleted)))
                                 excali--elements)))
         (step (lambda (i) (if (< i (/ frames 2)) 1 -1)))
         (pan-full (excali--bench-run
                    frames
                    (lambda (i)
                      (cl-incf excali--scroll-x (* 3.0 (funcall step i)))
                      (cl-incf excali--scroll-y (* 2.0 (funcall step i)))
                      'full)))
         (pan (excali--bench-run
               frames
               (lambda (i)
                 (cl-incf excali--scroll-x (* 3.0 (funcall step i)))
                 (cl-incf excali--scroll-y (* 2.0 (funcall step i)))
                 'scroll)))
         (drag (and target
                    (excali--bench-run
                     frames
                     (lambda (i)
                       (excali--with-damage target
                         (excali--put target 'x (+ (excali--get target 'x)
                                                  (if (< i (/ frames 2)) 3.0 -3.0)))
                         (excali--touch target))))))
         (zoom (excali--bench-zoom frames t))
         (zoom-full (excali--bench-zoom frames nil))
         (result (list :backend excali--backend
                       :elements (length excali--elements)
                       :canvas excali--canvas-size
                       :pixel-scale excali--pixel-scale
                       :pan-full pan-full :pan pan :drag drag
                       :zoom zoom :zoom-full zoom-full)))
    (message "excali-bench: %S" result)
    result))

(defun excali-bench-backends (&optional frames show)
  "Benchmark every backend on this scene and on a 500-element stress scene.
Results go to the *excali-bench* buffer, which is displayed when SHOW is
non-nil (as it is interactively), and are returned as a list.  Showing it
from a script would split the window and change the measured canvas."
  (interactive (list nil t))
  (let* ((frames (or frames 60))
         (backends (if (fboundp 'excali-native-layer-create)
                       '(canvas tiles layer)
                     '(canvas tiles)))
         (original excali--backend)
         (elements excali--elements)
         (results nil))
    (unwind-protect
        (dolist (scene '(sample stress))
          (when (eq scene 'stress)
            (setq excali--elements (append elements (excali--stress-elements 500))))
          (dolist (backend backends)
            (excali--use-backend backend)
            (redisplay t)
            (push (cons scene (excali-bench frames)) results)))
      (setq excali--elements elements)
      (excali--use-backend original))
    (setq results (nreverse results))
    ;; Read these before leaving the excali buffer: they are buffer-local.
    (let ((size excali--canvas-size) (scale excali--pixel-scale))
     (with-current-buffer (get-buffer-create "*excali-bench*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "canvas %S, pixel scale %.1f, %d frames\n\n"
                        size scale frames))
        (insert (format "%-7s %-7s %-9s %9s %10s %9s %6s %9s\n"
                        "scene" "backend" "op" "render" "present" "frame" "fps"
                        "surfaces"))
        (dolist (r results)
          (dolist (op '(:pan-full :pan :drag :zoom :zoom-full))
            (when-let* ((m (plist-get (cdr r) op)))
              (insert (format "%-7s %-7s %-9s %7.2fms %8.2fms %7.2fms %6.1f %9.1f\n"
                              (car r) (plist-get (cdr r) :backend)
                              (substring (symbol-name op) 1)
                              (plist-get m :render-ms) (plist-get m :present-ms)
                              (plist-get m :frame-ms) (plist-get m :fps)
                              (plist-get m :surfaces)))))))
      (special-mode)
      (when show (display-buffer (current-buffer)))))
    results))

(defun excali-bench-elisp-fill ()
  "Measure filling the whole canvas pixel by pixel from Elisp."
  (let* ((w (car excali--canvas-size)) (h (cdr excali--canvas-size))
         (data (make-vector (* w h) #xFFFFFFFF))
         (canvas (list 'image :type 'canvas :id (gensym "excali-elisp-")
                       :data-width w :data-height h :data data))
         (start (float-time)))
    (dotimes (i (* w h)) (aset data i (logior #xFF000000 (logand i #xFFFFFF))))
    (let ((fill (- (float-time) start)))
      (setq start (float-time))
      (canvas-refresh canvas 'reload-data)
      (list :elisp-fill-ms (* 1000 fill)
            :reload-ms (* 1000 (- (float-time) start))))))

(provide 'excali-bench)
;;; excali-bench.el ends here
