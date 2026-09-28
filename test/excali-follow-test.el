;;; excali-follow-test.el --- Labels and arrows follow their shapes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defun excali-follow-test--center (e)
  "Return the center of element E's box."
  (excali--box-center (excali--element-box e)))

(defun excali-follow-test--near (a b &optional eps)
  "Return non-nil if conses A and B agree within EPS (default 0.5)."
  (let ((eps (or eps 0.5)))
    (and (< (abs (- (car a) (car b))) eps) (< (abs (- (cdr a) (cdr b))) eps))))

(defmacro excali-follow-test--with-label (&rest body)
  "Run BODY in a window-backed scene: BOX with bound text LABEL \"Hello\"."
  `(excali-test--in-window
    (excali--load-current-style nil)
    (let* ((box (excali--make-element "rectangle" 100 100 (cons 'width 120.0)
                                     (cons 'height 60.0) (cons 'strokeWidth 2)))
           (label (progn (setq excali--elements (list box))
                         (excali--add-bound-text box))))
      (excali--set-text label "Hello")
      (excali--refresh-bound-text box)
      ,@body)))

(ert-deftest excali-follow-test-label-moves-with-container ()
  "Dragging a container carries its label along."
  (excali-follow-test--with-label
   (should (excali-follow-test--near (excali-follow-test--center label)
                                    (excali-follow-test--center box)))
   (excali--select (list box))
   (excali-test--drag 150 130 190 160)
   (should (= (excali--get box 'x) 140.0))
   (should (excali-follow-test--near (excali-follow-test--center label)
                                    (excali-follow-test--center box)))
   ;; Nudging too.
   (excali-nudge-right-large)
   (should (excali-follow-test--near (excali-follow-test--center label)
                                    (excali-follow-test--center box)))))

(ert-deftest excali-follow-test-label-refits-on-resize ()
  "Resizing a container keeps its label centered inside."
  (excali-follow-test--with-label
   (excali--select (list box))
   ;; The se corner handle sits just outside (220, 160).
   (excali-test--drag 223 163 323 203)
   (should (> (excali--get box 'width) 200))
   (should (excali-follow-test--near (excali-follow-test--center label)
                                    (excali-follow-test--center box)))))

(ert-deftest excali-follow-test-delete-container-deletes-label ()
  "Deleting a container deletes its label."
  (excali-follow-test--with-label
   (excali--select (list box))
   (excali-delete-selected)
   (should (eq (excali--get label 'isDeleted) t))))

(ert-deftest excali-follow-test-arrow-label-follows-binding ()
  "An arrow's label moves when a bound shape moves the arrow."
  (excali-test--in-window
   (excali--load-current-style nil)
   (let* ((box (excali--make-element "rectangle" 300 100 (cons 'width 100.0)
                                    (cons 'height 60.0) (cons 'strokeWidth 2)))
          (arrow (excali--apply-current-style
                  (excali--make-element "arrow" 50 130 (cons 'points [[0.0 0.0] [240.0 0.0]])))))
     (excali--linear-extent arrow)
     (setq excali--elements (list box arrow))
     (excali--bind-end arrow 'end box '(295.0 . 130.0))
     (excali--update-arrow arrow)
     (let ((label (excali--add-bound-text arrow)))
       (excali--set-text label "go")
       (excali--refresh-bound-text arrow)
       (let ((before (excali-follow-test--center label)))
         (excali--select (list box))
         (excali--nudge 0 100)
         ;; The label sits on the re-routed arrow, so it moved down.
         (should (> (cdr (excali-follow-test--center label)) (+ (cdr before) 20))))))))

(ert-deftest excali-follow-test-text-side-resize-rewraps ()
  "Dragging a text element's side re-wraps it at a fixed width."
  (excali-test--in-window
   (excali--load-current-style nil)
   (let ((text (excali--make-text-element 10 10 "one two three four five")))
     (setq excali--elements (list text))
     (excali--select (list text))
     (let* ((g (excali--snapshot-geometry text))
            (w (excali--get text 'width))
            (h (excali--get text 'height)))
       (excali--resize-single text g 'e (cons (+ 10 (/ w 2)) 15.0))
       (should (eq (alist-get 'autoResize text) :false))
       (should (< (excali--get text 'width) w))
       (should (> (excali--get text 'height) h))
       (should (string-match-p "\n" (excali--get text 'text)))))))

(provide 'excali-follow-test)
;;; excali-follow-test.el ends here
