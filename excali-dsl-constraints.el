;;; excali-dsl-constraints.el --- Small deterministic layout solver -*- lexical-binding: t; -*-
;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Two-phase simplex for bounded diagram layout.  No external process or
;; optional layout engine is needed.  Variables are nonnegative box edges.
;;; Code:
(require 'cl-lib)

(defun excali-dsl--linear-solve (constraints objective)
  "Maximize OBJECTIVE under CONSTRAINTS and nonnegative variables.
Each constraint is (COEFFICIENT-VECTOR . UPPER-BOUND).  Return a solution
vector, or nil if infeasible.  Bland's index rule makes ties deterministic."
  (let* ((m (length constraints)) (n (length objective))
         (d (vconcat (cl-loop repeat (+ m 2) collect (make-vector (+ n 2) 0.0))))
         (basic (make-vector m 0)) (nonbasic (make-vector (1+ n) 0))
         (eps 1e-8))
    (cl-loop for (a . b) in constraints for i from 0 do
             (dotimes (j n) (aset (aref d i) j (float (aref a j))))
             (aset basic i (+ n i))
             (aset (aref d i) n -1.0)
             (aset (aref d i) (1+ n) (float b)))
    (dotimes (j n)
      (aset nonbasic j j)
      (aset (aref d m) j (- (aref objective j))))
    (aset nonbasic n -1)
    (aset (aref d (1+ m)) n 1.0)
    (cl-labels
        ((cell (i j) (aref (aref d i) j))
         (pivot (r s)
           (let ((inv (/ 1.0 (cell r s))))
             (dotimes (i (+ m 2))
               (unless (= i r)
                 (dotimes (j (+ n 2))
                   (unless (= j s)
                     (aset (aref d i) j (- (cell i j) (* (cell r j) (cell i s) inv)))))))
             (dotimes (j (+ n 2))
               (unless (= j s) (aset (aref d r) j (* (cell r j) inv))))
             (dotimes (i (+ m 2))
               (unless (= i r) (aset (aref d i) s (* (- (cell i s)) inv))))
             (aset (aref d r) s inv)
             (cl-rotatef (aref basic r) (aref nonbasic s))))
         (simplex (phase)
           (let ((row (if (= phase 1) (1+ m) m)))
             (catch 'finished
               (while t
                 (let (s r)
                   (dotimes (j (1+ n))
                     (when (and (not (and (= phase 2) (= (aref nonbasic j) -1)))
                                (< (cell row j) (- eps))
                                (or (null s) (< (aref nonbasic j) (aref nonbasic s))))
                       (setq s j)))
                   (unless s (throw 'finished t))
                   (dotimes (i m)
                     (when (> (cell i s) eps)
                       (when (or (null r)
                                 (< (/ (cell i (1+ n)) (cell i s))
                                    (- (/ (cell r (1+ n)) (cell r s)) eps))
                                 (and (< (abs (- (/ (cell i (1+ n)) (cell i s))
                                                   (/ (cell r (1+ n)) (cell r s)))) eps)
                                      (< (aref basic i) (aref basic r))))
                         (setq r i))))
                   (unless r (throw 'finished nil))
                   (pivot r s)))))))
      (catch 'infeasible
        (when (> m 0)
          (let ((r 0))
            (dotimes (i m) (when (< (cell i (1+ n)) (cell r (1+ n))) (setq r i)))
            (when (< (cell r (1+ n)) (- eps))
              (pivot r n)
              (unless (and (simplex 1) (< (abs (cell (1+ m) (1+ n))) eps))
                (throw 'infeasible nil))
              (dotimes (i m)
                (when (= (aref basic i) -1)
                  (let (s)
                    (dotimes (j (1+ n))
                      (when (and (> (abs (cell i j)) eps)
                                 (or (null s) (< (aref nonbasic j) (aref nonbasic s))))
                        (setq s j)))
                    (when s (pivot i s))))))))
        (unless (simplex 2) (error "Unbounded DSL layout objective"))
        (let ((x (make-vector n 0.0)))
          (dotimes (i m)
            (when (and (>= (aref basic i) 0) (< (aref basic i) n))
              (aset x (aref basic i) (max 0.0 (cell i (1+ n))))))
          x)))))
(provide 'excali-dsl-constraints)
;;; excali-dsl-constraints.el ends here
