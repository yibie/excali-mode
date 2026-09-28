;;; excal-core.el --- Native module, shared state and document model  -*- lexical-binding: t; -*-

;;; Commentary:

;; Loads the rendering module and defines the per-buffer state and the
;; .excalidraw document model shared by the other excal files.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst excal--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(unless (featurep 'excal-module)
  (module-load (expand-file-name (concat "excal-module" module-file-suffix)
                                 excal--directory)))

(declare-function excal-native-render "excal-module")
(declare-function excal-native-measure-text "excal-module")
(declare-function excal-native-write-png "excal-module")
(declare-function excal-native-fb-create "excal-module")
(declare-function excal-native-fb-render "excal-module")
(declare-function excal-native-fb-present-canvas "excal-module")
(declare-function excal-native-fb-present-tiles "excal-module")
(declare-function excal-native-fb-write-png "excal-module")
(declare-function excal-native-fb-diff "excal-module")
(declare-function excal-native-fb-scroll "excal-module")
(declare-function excal-native-layer-create "excal-module")
(declare-function excal-native-layer-set-geometry "excal-module")
(declare-function excal-native-layer-present "excal-module")
(declare-function excal-native-layer-flush "excal-module")

(defgroup excal nil
  "Excalidraw scenes on Emacs Canvas."
  :group 'multimedia)

(defcustom excal-pixel-scale nil
  "Device pixels per logical pixel, or nil to guess from the frame."
  :type '(choice (const :tag "Auto" nil) number))

(defvar-local excal--file nil "File the scene is saved to.")
(defvar-local excal--doc nil "Top-level .excalidraw alist.")
(defvar-local excal--elements nil "Element alists in z-order.")
(defvar-local excal--canvas nil "Canvas image spec.")
(defvar-local excal--canvas-size nil "(WIDTH . HEIGHT) in device pixels.")
(defvar-local excal--pixel-scale 1.0)
(defvar-local excal--zoom 1.0)
(defvar-local excal--scroll-x 0.0)
(defvar-local excal--scroll-y 0.0)
(defvar-local excal--tool 'select)
(defvar-local excal--selection nil "Selected element alists, in z-order.")
(defvar-local excal--editing-group nil
  "Group id entered by double-clicking, or nil; see `excal--unit'.")
(defvar-local excal--marquee nil
  "Box-selection rectangle (X1 Y1 X2 Y2) in scene units while dragging.")
(defvar-local excal--pointer nil "Pointer shape currently shown over the canvas.")
(defvar-local excal--rendered-origin nil
  "View origin of the framebuffer's contents; see `excal--view-origin'.")
(defvar-local excal--pan-remainder '(0.0 . 0.0)
  "Sub-pixel pan distance not yet applied; see `excal--pan'.")
(defvar-local excal--native-cache nil
  "Hash table mapping element alists to native vectors.")
(defvar-local excal--last-render-time nil
  "Seconds spent in the last native render.")

(defun excal--get (element key)
  "Return KEY of ELEMENT, mapping JSON null and false to nil."
  (let ((value (alist-get key element)))
    (if (memq value '(:null :false)) nil value)))

(defun excal--put (element key value)
  "Destructively set KEY of ELEMENT to VALUE."
  (if-let* ((cell (assq key element)))
      (setcdr cell value)
    (nconc element (list (cons key value))))
  value)

(defun excal--touch (element)
  "Invalidate ELEMENT's native cache and bump its version."
  (remhash element excal--native-cache)
  (excal--put element 'version (1+ (or (excal--get element 'version) 0)))
  (excal--put element 'versionNonce (random (ash 1 31)))
  (excal--put element 'updated (truncate (* 1000 (float-time)))))

(defun excal--read-file (file)
  "Parse .excalidraw FILE into an alist."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'alist :array-type 'array
                       :null-object :null :false-object :false)))

(defun excal--empty-doc ()
  "Return an empty .excalidraw document."
  (list (cons 'type "excalidraw")
        (cons 'version 2)
        (cons 'source "https://excalidraw.com")
        (cons 'elements [])
        (cons 'appState (list (cons 'viewBackgroundColor "#ffffff")
                              (cons 'gridSize :null)))
        (cons 'files (list))))

(defun excal-save ()
  "Write the scene back to its .excalidraw file."
  (interactive)
  (unless excal--file
    (setq excal--file (read-file-name "Save scene to: " nil nil nil
                                      "untitled.excalidraw")))
  (let ((doc (copy-alist excal--doc)))
    (setf (alist-get 'elements doc) (vconcat excal--elements))
    (unless (alist-get 'files doc) (setf (alist-get 'files doc) (list)))
    (with-temp-file excal--file
      (setq buffer-file-coding-system 'utf-8-unix)
      (json-insert doc :null-object :null :false-object :false)))
  (message "Saved %s" excal--file))

(defun excal--new-id ()
  "Return a random element id."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (apply #'string (cl-loop repeat 20 collect (aref chars (random (length chars)))))))

(defun excal--make-element (type x y &rest props)
  "Return a new element alist of TYPE at X, Y with extra PROPS alist."
  (let ((element
         (list (cons 'id (excal--new-id)) (cons 'type type)
               (cons 'x (float x)) (cons 'y (float y))
               (cons 'width 0.0) (cons 'height 0.0) (cons 'angle 0)
               (cons 'strokeColor "#1e1e1e") (cons 'backgroundColor "transparent")
               (cons 'fillStyle "solid") (cons 'strokeWidth 2)
               (cons 'strokeStyle "solid") (cons 'roughness 1)
               (cons 'opacity 100) (cons 'groupIds []) (cons 'frameId :null)
               (cons 'roundness :null) (cons 'seed (1+ (random (1- (ash 1 31)))))
               (cons 'version 1) (cons 'versionNonce (random (ash 1 31)))
               (cons 'isDeleted :false) (cons 'boundElements :null)
               (cons 'updated (truncate (* 1000 (float-time))))
               (cons 'link :null) (cons 'locked :false))))
    (dolist (prop props element)
      (excal--put element (car prop) (cdr prop)))))

(defun excal--bounds (element)
  "Return (X1 Y1 X2 Y2) of ELEMENT, ignoring rotation."
  (let ((x (excal--get element 'x)) (y (excal--get element 'y))
        (w (excal--get element 'width)) (h (excal--get element 'height)))
    (if-let* ((points (excal--get element 'points))
              ((> (length points) 0)))
        (let ((xs (mapcar (lambda (p) (+ x (aref p 0))) points))
              (ys (mapcar (lambda (p) (+ y (aref p 1))) points)))
          (list (apply #'min xs) (apply #'min ys)
                (apply #'max xs) (apply #'max ys)))
      (list (min x (+ x w)) (min y (+ y h)) (max x (+ x w)) (max y (+ y h))))))

(defun excal--normalize-box (element)
  "Make ELEMENT's width and height non-negative."
  (let ((w (excal--get element 'width)) (h (excal--get element 'height)))
    (when (< w 0)
      (excal--put element 'x (+ (excal--get element 'x) w))
      (excal--put element 'width (- w)))
    (when (< h 0)
      (excal--put element 'y (+ (excal--get element 'y) h))
      (excal--put element 'height (- h)))))

(defun excal--linear-extent (element)
  "Recompute ELEMENT's width and height from its points."
  (let ((points (excal--get element 'points)))
    (excal--put element 'width
                (float (- (seq-max (seq-map (lambda (p) (aref p 0)) points))
                          (seq-min (seq-map (lambda (p) (aref p 0)) points)))))
    (excal--put element 'height
                (float (- (seq-max (seq-map (lambda (p) (aref p 1)) points))
                          (seq-min (seq-map (lambda (p) (aref p 1)) points)))))))

(defun excal--measure-text (element)
  "Set ELEMENT's width and height from its text and font."
  (let ((size (excal-native-measure-text
               (or (excal--get element 'text) "")
               (or (excal--get element 'fontSize) 20)
               (or (excal--get element 'fontFamily) 5)
               (or (excal--get element 'lineHeight) 1.25))))
    (excal--put element 'width (car size))
    (excal--put element 'height (cdr size))))

(defun excal--set-text (element text)
  "Set ELEMENT's TEXT and resize it to fit."
  (excal--put element 'text text)
  (excal--put element 'originalText text)
  (excal--measure-text element)
  (excal--touch element))

(defun excal--make-text-element (x y text)
  "Return a new text element showing TEXT with its top-left corner at X, Y."
  (let ((element (excal--make-element
                  "text" x y
                  (cons 'text text) (cons 'originalText text)
                  (cons 'fontSize 20) (cons 'fontFamily 5)
                  (cons 'textAlign "left") (cons 'verticalAlign "top")
                  (cons 'containerId :null) (cons 'autoResize t)
                  (cons 'lineHeight 1.25))))
    (excal--measure-text element)
    element))

(provide 'excal-core)
;;; excal-core.el ends here
