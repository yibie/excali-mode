;;; excali-view-test.el --- One buffer, several windows  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Each window showing an excali buffer has its own view: zoom, scroll,
;; framebuffer, surfaces (an overlay of its own) and pointer map.

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-view-test--two-windows (w1 w2 rect &rest body)
  "Run BODY with a scene holding RECT shown in windows W1 and W2.
The tiles backend is used; both views are synced."
  (declare (indent 3))
  `(let ((buffer (generate-new-buffer " *excali-views*")))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           (switch-to-buffer buffer)
           (excali-mode)
           (let ((,rect (excali--make-element "rectangle" 10 2 (cons 'width 30.0)
                                              (cons 'height 6.0) (cons 'roughness 0)
                                              (cons 'backgroundColor "transparent"))))
             (setq excali--backend 'tiles excali--pixel-scale 1.0
                   excali--doc (excali--empty-doc) excali--elements (list ,rect))
             (let* ((,w1 (selected-window))
                    (,w2 (split-window ,w1)))
               (set-window-buffer ,w2 buffer)
               (excali--sync-canvas ,w1)
               (excali--sync-canvas ,w2)
               ,@body)))
       (kill-buffer buffer))))

(defun excali-view-test--pixel (window x y)
  "Return the pixel at X, Y of WINDOW's view's framebuffer."
  (excali--with-view window
    (excali-native-fb-pixel excali--fb x y)))

(ert-deftest excali-view-test-overlay-per-window ()
  "The buffer holds one placeholder; each window shows its own surfaces."
  (excali-view-test--two-windows w1 w2 _rect
    (should (equal (buffer-string) " "))
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (= (length overlays) 2))
      (should (equal (sort (mapcar (lambda (o) (overlay-get o 'window)) overlays)
                           (lambda (a b) (< (window-pixel-top a) (window-pixel-top b))))
                     (sort (list w1 w2)
                           (lambda (a b) (< (window-pixel-top a) (window-pixel-top b))))))
      (dolist (o overlays)
        (should (equal (overlay-get o 'display) ""))
        ;; The before-string carries the window's tile images.
        (let ((shown (overlay-get o 'before-string)))
          (should (eq (car (get-text-property 0 'display shown)) 'image)))))
    ;; The surfaces are distinct.
    (should-not (eq (excali--view-value w1 'excali--fb) (excali--view-value w2 'excali--fb)))
    (should-not (equal (excali--view-value w1 'excali--tiles)
                       (excali--view-value w2 'excali--tiles)))))

(ert-deftest excali-view-test-independent-zoom-and-scroll ()
  "Zooming and scrolling one window leaves the other's view alone."
  (excali-view-test--two-windows w1 w2 _rect
    (excali--with-view w1
      (excali--zoom-view 2.0 '(0 . 0))
      (cl-incf excali--scroll-x 5.0)
      (excali--render))
    (should (= (excali--view-value w1 'excali--zoom) 2.0))
    (should (= (excali--view-value w2 'excali--zoom) 1.0))
    (should (= (excali--view-value w2 'excali--scroll-x) 0.0))
    ;; The mode line shows each window's own zoom.
    (should (= (excali--view-value w2 'excali--zoom)
               (excali--with-view w2 excali--zoom)))))

(ert-deftest excali-view-test-edit-redraws-other-windows ()
  "A change made in one window shows in the other after the command."
  (excali-view-test--two-windows w1 w2 rect
    (let ((inside (lambda (w) (excali-view-test--pixel w 25 5))))
      (should (= (funcall inside w2) #xffffffff))
      ;; A command in W1 fills the rectangle.
      (excali--use-view w1)
      (excali--put rect 'backgroundColor "#ff0000")
      (excali--put rect 'fillStyle "solid")
      (excali--touch rect)
      (excali--render)
      (should (= (funcall inside w1) #xffff0000))
      (should (= (funcall inside w2) #xffffffff))
      (excali--sync-views)
      (should (= (funcall inside w2) #xffff0000))
      ;; Nothing changed: the other view is not drawn again.
      (let ((stats (excali--view-value w2 'excali--last-stats)))
        (excali--sync-views)
        (should (eq (excali--view-value w2 'excali--last-stats) stats))))))

(ert-deftest excali-view-test-commands-use-their-window ()
  "Mouse events act in their window's view, keys in the selected window's."
  (excali-view-test--two-windows w1 w2 _rect
    (select-window w1)
    (let ((last-input-event (list 'mouse-movement
                                  (list w2 1 '(5 . 5) 0 nil 1 '(0 . 0) nil '(5 . 5) '(1 . 1)))))
      (excali--select-view)
      (should (eq excali--view-window w2))
      (should (eq (selected-window) w1)))
    ;; A press there selects the window too.
    (let ((last-input-event (list 'down-mouse-1
                                  (list w2 1 '(5 . 5) 0 nil 1 '(0 . 0) nil '(5 . 5) '(1 . 1)))))
      (excali--select-view)
      (should (eq (selected-window) w2)))
    (select-window w1)
    (let ((last-input-event ?a))
      (excali--select-view)
      (should (eq excali--view-window w1)))))

(ert-deftest excali-view-test-pointer-map-per-window ()
  "Each view's surfaces carry a map built for that view."
  (excali-view-test--two-windows w1 w2 _rect
    (excali--with-view w1
      (excali--zoom-view 2.0 '(0 . 0))
      (excali--update-pointer t))
    (let ((map-of (lambda (w)
                    (plist-get (cdr (car (car (excali--view-value w 'excali--pointer-surfaces))))
                               :map))))
      (should (funcall map-of w1))
      (should (funcall map-of w2))
      (should-not (equal (funcall map-of w1) (funcall map-of w2))))))

(ert-deftest excali-view-test-deleted-window-releases-its-view ()
  "Deleting a window drops its overlay and its view."
  (excali-view-test--two-windows w1 w2 _rect
    (let ((overlay (excali--view-value w2 'excali--view-overlay)))
      (delete-window w2)
      (excali--window-size-change (selected-frame))
      (should-not (overlay-buffer overlay))
      (should (= (length (overlays-in (point-min) (point-max))) 1))
      (should-not (memq w2 (excali--other-views)))
      (should (eq excali--view-window w1))
      ;; The remaining view still renders.
      (excali--render)
      (should excali--fb))))

(ert-deftest excali-view-test-deleting-the-current-window ()
  "When the window of the current view goes, another view takes over."
  (excali-view-test--two-windows w1 w2 _rect
    (excali--with-view w2
      (excali--zoom-view 3.0 '(0 . 0)))
    (excali--use-view w1)
    (select-window w2)
    (delete-window w1)
    (excali--window-size-change (selected-frame))
    (should (eq excali--view-window w2))
    (should (= excali--zoom 3.0))
    (should (null (excali--other-views)))))

(ert-deftest excali-view-test-single-window-adopts-buffer-state ()
  "The first window's view is the buffer's state as it was."
  (excali-test--in-window
   (setq excali--zoom 1.5 excali--scroll-x 3.0)
   (excali--use-view (selected-window))
   (should (eq excali--view-window (selected-window)))
   (should (= excali--zoom 1.5))
   (should (= excali--scroll-x 3.0))
   (should (null (excali--other-views)))))

(provide 'excali-view-test)
;;; excali-view-test.el ends here
