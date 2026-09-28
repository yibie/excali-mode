;;; hero.el --- Render the README animation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Renders docs/media/hero.gif frame by frame with excal's own renderer
;; and commands, in batch: a shape is drawn and labelled, a flowchart
;; grows from it with Mod+Arrow, a node is dragged while its elbow
;; arrows re-route, a pen stroke circles the result, and the theme
;; turns dark.  Run `make hero' (needs ffmpeg); `make fonts' first for
;; Excalidraw's hand-drawn font.
;;
;; Frames are 1600x900 device pixels (800x450 scene px at 2x) and go to
;; the directory in $EXCAL_HERO_FRAMES.

;;; Code:

(require 'excal)

(defvar hero--dir (or (getenv "EXCAL_HERO_FRAMES") "build/hero"))
(defvar hero--frame 0)

(defun hero--shot (&optional count)
  "Render the scene and write it as COUNT frames (default 1)."
  (excal--render)
  (dotimes (_ (or count 1))
    (excal-native-fb-write-png
     excal--fb (expand-file-name (format "f%05d.png" hero--frame) hero--dir))
    (cl-incf hero--frame)))

(defun hero--ease (s)
  "Ease-out cubic of S in [0, 1]."
  (- 1 (expt (- 1 s) 3)))

(defun hero--type (element text &optional frames-per-char)
  "Type TEXT into ELEMENT's label one character at a time."
  (let ((label (or (excal--bound-text-of element)
                   (let ((l (excal--add-bound-text element)))
                     (excal--apply-current-style l)
                     (excal--put l 'textAlign "center")
                     (excal--put l 'verticalAlign "middle")
                     (excal--put l 'lineHeight (excal--line-height (excal--get l 'fontFamily)))
                     l))))
    (dotimes (i (length text))
      (excal--set-text label (substring text 0 (1+ i)))
      (excal--follow (list element))
      (hero--shot (or frames-per-char 2)))))

(defun hero--select (element)
  "Select only ELEMENT."
  (excal--deselect)
  (when element (excal--select (list element))))

(defun hero--run (command)
  "Run flowchart COMMAND as the command loop would."
  (let ((this-command command) (last-input-event ?x))
    (excal--flowchart-pre-command)
    (funcall command)))

(defun hero--move (element dy frames)
  "Drag ELEMENT by DY over FRAMES, arrows following."
  (let ((y0 (excal--get element 'y)))
    (dotimes (i frames)
      (let ((y (+ y0 (* dy (hero--ease (/ (1+ i) (float frames)))))))
        (excal--put element 'y y)
        (excal--touch element)
        (excal--follow (list element) (list element))
        (hero--shot)))))

(defun hero--circle (cx cy rx ry frames)
  "Draw a pen stroke around CX, CY with radii RX, RY over FRAMES."
  (let* ((start (cons (+ cx rx) cy))
         (stroke (excal--apply-current-style
                  (excal--make-element "freedraw" (car start) (cdr start)
                                       (cons 'points (vector [0.0 0.0]))
                                       (cons 'pressures []) (cons 'simulatePressure t))))
         (points (list [0.0 0.0]))
         (steps 64))
    ;; The current style would otherwise win over these.
    (excal--put stroke 'strokeColor "#e03131")
    (excal--put stroke 'strokeWidth 2)
    (excal--put stroke 'backgroundColor "transparent")
    (excal--add-new stroke)
    (dotimes (i steps)
      ;; A little more than a full turn, drifting outward like a hand.
      (let* ((a (* 2.15 float-pi (/ (1+ i) (float steps))))
             (grow (+ 1 (* 0.08 (/ (1+ i) (float steps)))
                      ;; A slow wobble, as a hand would draw it.
                      (* 0.03 (sin (* 3 a)))))
             (x (+ cx (* rx grow (cos a))))
             (y (- cy (* ry grow (sin a)))))
        (push (vector (- x (car start)) (- y (cdr start))) points)
        (excal--put stroke 'points (vconcat (reverse points)))
        (excal--touch stroke)
        (when (zerop (% i (max 1 (/ steps frames))))
          (hero--shot))))
    (excal--linear-extent stroke)
    (excal--touch stroke)
    (hero--shot)))

(defun hero-render ()
  "Render every frame of the animation into `hero--dir'."
  (make-directory hero--dir t)
  (dolist (f (directory-files hero--dir t "\\`f[0-9]+\\.png\\'")) (delete-file f))
  (with-temp-buffer
    (setq excal--native-cache (make-hash-table :test #'eq)
          excal--zoom 1.0 excal--pixel-scale 2.0
          excal--scroll-x 0.0 excal--scroll-y 0.0
          excal--backend nil excal--tool 'select excal--theme 'light
          excal--elements nil excal--selection nil
          excal--canvas-size (cons 1600 900)
          excal--fb (excal-native-fb-create 1600 900))
    (excal--load-current-style nil)
    (setf (alist-get 'fillStyle excal--current-style) "hachure"
          (alist-get 'roughness excal--current-style) 1
          (alist-get 'fontSize excal--current-style) 20)
    (excal--history-reset)
    (random "excal-hero")
    (hero--shot 12)
    ;; 1. Draw a rectangle and label it.
    (setf (alist-get 'backgroundColor excal--current-style) "#a5d8ff")
    (let ((root (excal--apply-current-style
                 (excal--make-element "rectangle" 40 80 (cons 'roundness '((type . 3)))))))
      (excal--add-new root)
      (dotimes (i 18)
        (let ((s (hero--ease (/ (1+ i) 18.0))))
          (excal--put root 'width (* 160.0 s))
          (excal--put root 'height (* 90.0 s))
          (excal--touch root)
          (hero--shot)))
      (hero--select root)
      (hero--shot 8)
      (hero--type root "Emacs 32" 3)
      (hero--shot 12)
      ;; 2. Mod+Right twice: a pair of linked siblings.
      (hero--run #'excal-flowchart-right)
      (hero--shot 14)
      (hero--run #'excal-flowchart-right)
      (hero--shot 16)
      (hero--run #'ignore)
      (let* ((nodes (seq-filter (lambda (e) (and (equal (excal--get e 'type) "rectangle")
                                                 (not (eq e root))))
                                excal--elements))
             (canvas (car nodes)) (module (cadr nodes)))
        (excal--put module 'backgroundColor "#b2f2bb")
        (excal--touch module)
        (hero--shot 6)
        (hero--type canvas "canvas API")
        (hero--select module)
        (hero--type module "C module")
        (hero--shot 8)
        ;; 3. And one more step from the first sibling.
        (hero--select canvas)
        (hero--run #'excal-flowchart-right)
        (hero--shot 12)
        (hero--run #'ignore)
        (let ((result (car (excal--flowchart-selected-node-list))))
          (excal--put result 'backgroundColor "#ffec99")
          (excal--touch result)
          (hero--type result "excal-mode")
          (hero--shot 10)
          ;; 4. Drag the root: the elbow arrows re-route.
          (hero--select root)
          (hero--shot 6)
          (hero--move root 150 28)
          (hero--shot 8)
          (hero--move root -150 28)
          (hero--shot 8)
          ;; 5. Circle the result with the pen.
          (hero--select nil)
          (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--element-box result)))
            (hero--circle (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0)
                          (+ 14 (/ (- x2 x1) 2.0)) (+ 16 (/ (- y2 y1) 2.0)) 26))
          (hero--shot 24)
          ;; 6. Dark mode.
          (setq excal--theme 'dark)
          (clrhash excal--native-cache)
          (hero--shot 50)))))
  (message "Wrote %d frames to %s" hero--frame hero--dir))

(defun excal--flowchart-selected-node-list ()
  "Return the selection as a list, for the script."
  excal--selection)

(provide 'hero)
;;; hero.el ends here
