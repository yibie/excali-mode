;;; excali-dsl-test.el --- Excali DSL acceptance tests -*- lexical-binding: t; -*-
;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'excali)
(require 'excali-test)

(defconst excali-dsl-test--fixtures
  (expand-file-name "../examples/excali-dsl" (file-name-directory (or load-file-name buffer-file-name))))
(defun excali-dsl-test--source (name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat name ".excalidsl") excali-dsl-test--fixtures))
    (buffer-string)))
(defun excali-dsl-test--by-id (elements id)
  (seq-find (lambda (e) (equal (alist-get 'excaliDslId (alist-get 'customData e)) id)) elements))
(defun excali-dsl-test--box (item)
  (if (excali-dsl--node-p item)
      (list (excali-dsl--node-x item) (excali-dsl--node-y item)
            (excali-dsl--node-w item) (excali-dsl--node-h item))
    (list (excali-dsl--cluster-x item) (excali-dsl--cluster-y item)
          (excali-dsl--cluster-w item) (excali-dsl--cluster-h item))))
(defun excali-dsl-test--near (a b) (< (abs (- a b)) 0.001))

(ert-deftest excali-dsl-examples ()
  (dolist (name '("application" "styled-application" "review-flow"))
    (let* ((doc (excali-dsl-scene (excali-dsl-test--source name))) (es (alist-get 'elements doc)))
      (should (> (length es) 10))
      (seq-doseq (e es)
        (when (equal (alist-get 'type e) "arrow")
          (should (alist-get 'elementId (alist-get 'startBinding e)))
          (should (alist-get 'elementId (alist-get 'endBinding e))))))))

(ert-deftest excali-dsl-parser-strings-and-continuations ()
  (let* ((s (excali-dsl-parse "// comment\nnode a \"汉字 // \\\"quote\\\"\\nline\\tend\\\\\"\nnode b\n  right of a {\n    fill: \"#abc\"; // color\n    opacity: 0\n  }\n"))
         (b (cadr s)))
    (should (equal (plist-get (car s) :label) "汉字 // \"quote\"\nline\tend\\"))
    (should (equal (caar (plist-get b :placements)) "right"))
    (should (= (length (plist-get b :props)) 2))))

(ert-deftest excali-dsl-invalid-syntax ()
  (dolist (source '("a[Old]" "node ->" "node a \"x\\q\"" "node a \"x\ny\""
                    "node a { fill: #abc }" "node a { fill: \"#abc\" opacity: 50 }"
                    "node a { fill: { color: red } }" "node a {} below b"
                    "node a\n\n  below b" "  node a" "node a { opacity: 1; opacity: 2 }"
                    "node a {" "style service" "node a\nedge a <-> a"))
    (should-error (excali-dsl-parse source) :type 'excali-dsl-error)))

(ert-deftest excali-dsl-semantic-errors ()
  (dolist (source '("node a\nnode a" "node a.b" "node a below missing"
                    "node a\nedge a -> missing" "node a\nedge a -> a"
                    "node a\nnode b right of a\nedge a -> b\nedge a -> b"
                    "style x {}\nstyle x {}" "default node {}\ndefault node {}"
                    "default text {}" "style x.y {}" "node a { style: missing }"
                    "style x { style: other }" "default node { style: x }"
                    "node a { routing: elbow }" "node a { fill: red }"
                    "node a { font: \"sans\" }" "node a { font-size: 0 }"
                    "node a { opacity: 101 }" "node a { roughness: 3 }"
                    "node a { mystery: 1 }" "node a { shape: ellipse; roundness: round }"
                    "node a { shape: ellipse }\nnode a.b"
                    "style x { fill: \"#abc\" }\nnode a\nnode b right of a\nedge a -> b { style: x }"
                    "node a\nnode b right of a\nedge a -> b from: north"
                    "node a\nnode b right of a\nedge a -> b from: top from: left"))
    (should-error (excali-dsl-model source) :type 'excali-dsl-error)))

(ert-deftest excali-dsl-diagnostics ()
  (condition-case err
      (excali-dsl-model "node a\nnode b below missing")
    (excali-dsl-error
     (should (= (nth 1 err) 2)) (should (= (nth 3 err) 14))
     (should (string-match-p "missing" (excali-dsl-error-message err))))))

(ert-deftest excali-dsl-style-precedence ()
  (let* ((es (excali-dsl-elements "node a { style: service; stroke: \"#abc\" }\nstyle service { fill: \"#def\"; stroke: \"#000\" }\ndefault node { stroke: \"#fff\"; font-size: 28; opacity: 40; text-align: right }"))
         (a (excali-dsl-test--by-id es "a"))
         (text (seq-find (lambda (e) (equal (alist-get 'type e) "text")) es)))
    (should (equal (alist-get 'strokeColor a) "#abc"))
    (should (equal (alist-get 'backgroundColor a) "#def"))
    (should (= (alist-get 'fontSize text) 28))
    (should (= (alist-get 'opacity text) 40))
    (should (equal (alist-get 'textAlign text) "right"))
    (should (equal (alist-get 'strokeColor text) "#1e1e1e"))))

(ert-deftest excali-dsl-baseline-geometry ()
  (let* ((m (excali-dsl-model (excali-dsl-test--source "application")))
         (box (lambda (id) (excali-dsl-test--box (excali-dsl--item id m))))
         (parent (funcall box "system")) (ui (funcall box "system.ui"))
         (api (funcall box "system.api")) (db (funcall box "database")))
    (should (>= (- (car db) (+ (car parent) (nth 2 parent))) 79.999))
    (should (>= (- (nth 1 api) (+ (nth 1 ui) (nth 3 ui))) 79.999))
    (should (excali-dsl-test--near (+ (nth 1 db) (/ (nth 3 db) 2))
                                 (+ (nth 1 api) (/ (nth 3 api) 2))))
    (dolist (b (list ui api))
      (should (>= (car b) (+ (car parent) 29.999)))
      (should (<= (+ (car b) (nth 2 b)) (- (+ (car parent) (nth 2 parent)) 29.999)))
      (should (<= (+ (nth 1 b) (nth 3 b)) (- (+ (nth 1 parent) (nth 3 parent)) 29.999))))))

(ert-deftest excali-dsl-containers-grow-and-cross-references ()
  (let (widths)
    (dolist (label '("API" "A substantially longer API label"))
      (let* ((src (format "node p\nnode p.child \"%s\"\nnode external right of p level with p.child\n" label))
             (m (excali-dsl-model src))
             (p (excali-dsl-test--box (excali-dsl--item "p" m)))
             (child (excali-dsl-test--box (excali-dsl--item "p.child" m)))
             (ext (excali-dsl-test--box (excali-dsl--item "external" m))))
        (push (nth 2 p) widths)
        (should (>= (- (car ext) (+ (car p) (nth 2 p))) 79.999))
        (should (excali-dsl-test--near (+ (nth 1 child) (/ (nth 3 child) 2))
                                     (+ (nth 1 ext) (/ (nth 3 ext) 2))))))
    (should (> (car widths) (cadr widths)))))

(ert-deftest excali-dsl-conflicting-layouts ()
  (dolist (source '("node a\nnode b" "node a\nnode b\nedge a -> b"
                    "node a right of b\nnode b right of a"
                    "node p\nnode p.c right of p"
                    "node a\nnode b below a level with a"
                    "node p\nnode p.child below external\nnode external right of p"))
    (should-error (excali-dsl-model source) :type 'excali-dsl-error)))

(ert-deftest excali-dsl-nested-implicit-and-forward ()
  (let* ((m (excali-dsl-model "node root\nnode root.inner\nnode root.inner.a\nnode root.inner.b\nnode later right of end\nnode end right of root"))
         (a (excali-dsl--item "root.inner.a" m)) (b (excali-dsl--item "root.inner.b" m)))
    (should (>= (- (excali-dsl--node-y b) (+ (excali-dsl--node-y a) (excali-dsl--node-h a))) 79.999))))

(ert-deftest excali-dsl-all-directions ()
  (dolist (op '("right of" "left of" "above" "below"))
    (let* ((m (excali-dsl-model (format "node a\nnode b %s a" op)))
           (a (excali-dsl-test--box (excali-dsl--item "a" m)))
           (b (excali-dsl-test--box (excali-dsl--item "b" m))))
      (pcase op
        ("right of" (should (>= (- (car b) (+ (car a) (nth 2 a))) 79.999)))
        ("left of" (should (>= (- (car a) (+ (car b) (nth 2 b))) 79.999)))
        ("above" (should (>= (- (nth 1 a) (+ (nth 1 b) (nth 3 b))) 79.999)))
        ("below" (should (>= (- (nth 1 b) (+ (nth 1 a) (nth 3 a))) 79.999)))))))

(ert-deftest excali-dsl-binding-sides-and-movement ()
  (dolist (routing '("straight" "elbow"))
    (with-temp-buffer
      (setq excali--native-cache (make-hash-table :test #'eq))
      (setq excali--elements (excali-dsl-elements
                             (format "node a\nnode b right of a\nedge a -> b \"go\" from: bottom to: left { routing: %s }" routing)))
      (let* ((a (excali-dsl-test--by-id excali--elements "a"))
             (arrow (excali-dsl-test--by-id excali--elements "a->b"))
             (binding (alist-get 'startBinding arrow))
             (before (excali--arrow-point arrow 0)))
        (should (equal (alist-get 'elementId binding) (alist-get 'id a)))
        (should (> (aref (alist-get 'fixedPoint binding) 1) 0.99))
        (should (< (aref (alist-get 'fixedPoint (alist-get 'endBinding arrow)) 0) 0.01))
        (when (equal routing "straight")
          (should (excali-dsl-test--near (cdr before) (+ (alist-get 'y a) (alist-get 'height a)))))
        (excali--put a 'y (+ 40 (alist-get 'y a)))
        (excali--update-bound-arrows (list a))
        (should-not (equal before (excali--arrow-point arrow 0)))
        (should (seq-some (lambda (e) (equal (alist-get 'containerId e) (alist-get 'id arrow))) excali--elements))))))

(ert-deftest excali-dsl-repeatable-and-independent-defaults ()
  (let* ((src (excali-dsl-test--source "styled-application"))
         (a (excali-dsl-elements src))
         (excali-default-font-size 36) (excali-default-font-family 8)
         (b (excali-dsl-elements src)))
    (should (equal (mapcar (lambda (e) (mapcar (lambda (k) (alist-get k e)) '(x y width height points seed)))
                          (seq-filter (lambda (e) (alist-get 'customData e)) a))
                   (mapcar (lambda (e) (mapcar (lambda (k) (alist-get k e)) '(x y width height points seed)))
                          (seq-filter (lambda (e) (alist-get 'customData e)) b))))))

(ert-deftest excali-dsl-atomic-export-and-roundtrip ()
  (let ((file (make-temp-file "excali-dsl-test" nil ".excalidraw")))
    (unwind-protect
        (progn
          (excali-dsl-write (excali-dsl-test--source "application") file)
          (let ((before (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (should-error (excali-dsl-write "node invalid below missing" file) :type 'excali-dsl-error)
            (should (equal before (with-temp-buffer (insert-file-contents file) (buffer-string)))))
          (let ((doc (excali--restore-doc (excali--read-file file))))
            (seq-doseq (e (alist-get 'elements doc))
              (should (stringp (alist-get 'index e)))
              (when (equal (alist-get 'type e) "arrow")
                (should (alist-get 'elementId (alist-get 'startBinding e)))))))
      (delete-file file))))

(ert-deftest excali-dsl-mode ()
  (with-temp-buffer
    (insert "node a {\nfill: \"#abc\"\n}\nnode b\n  right of a\n// comment\n")
    (excali-dsl-mode) (indent-region (point-min) (point-max)) (font-lock-ensure)
    (should (string-match-p "\n  fill:" (buffer-string)))
    (should (string-match-p "\n  right of a" (buffer-string)))
    (should (eq (get-text-property (point-min) 'face) 'font-lock-keyword-face))
    (should (equal comment-start "// "))
    (should (eq (assoc-default "a.excalidsl" auto-mode-alist #'string-match) 'excali-dsl-mode))
    (should-not (eq (assoc-default "a.edsl" auto-mode-alist #'string-match) 'excali-dsl-mode))))

(ert-deftest excali-dsl-linear-solver ()
  (let ((x (excali-dsl--linear-solve
            (list (cons [-1.0 0.0] -2) (cons [0.0 -1.0] -3)
                  (cons [1.0 1.0] 6)) [-1.0 -1.0])))
    (should (excali-dsl-test--near (aref x 0) 2))
    (should (excali-dsl-test--near (aref x 1) 3)))
  (should-not (excali-dsl--linear-solve (list (cons [-1.0] -2) (cons [1.0] 1)) [-1.0])))

(ert-deftest excali-dsl-edge-label-clearance ()
  (let* ((es (excali-dsl-elements "node a\nnode b right of a\nedge a->b \"A longer label\""))
         (a (excali-dsl-test--by-id es "a")) (b (excali-dsl-test--by-id es "b"))
         (arrow (excali-dsl-test--by-id es "a->b"))
         (label (seq-find (lambda (e) (equal (alist-get 'containerId e) (alist-get 'id arrow))) es)))
    (should (> (alist-get 'x label) (+ (alist-get 'x a) (alist-get 'width a))))
    (should (< (+ (alist-get 'x label) (alist-get 'width label)) (alist-get 'x b)))))

(ert-deftest excali-dsl-no-style-cascade-and-empty-label ()
  (let* ((es (excali-dsl-elements "node p { opacity: 30; fill: \"#abc\" }\nnode p.child \"\""))
         (p (excali-dsl-test--by-id es "p")) (child (excali-dsl-test--by-id es "p.child")))
    (should (= (alist-get 'opacity p) 30))
    (should (= (alist-get 'opacity child) 100))
    (should (equal (alist-get 'backgroundColor child) "transparent"))
    (should-not (seq-some (lambda (e) (equal (alist-get 'containerId e) (alist-get 'id child))) es))))

(ert-deftest excali-dsl-native-preview-export ()
  (with-temp-buffer
    (setq excali--native-cache (make-hash-table :test #'eq)
          excali--elements (excali-dsl-elements (excali-dsl-test--source "styled-application")))
    (let ((file (make-temp-file "excali-dsl-preview" nil ".png")))
      (unwind-protect
          (progn
            (should (excali-native-export-png file
                       (vconcat (mapcar #'excali--native-element excali--elements))
                       -20.0 -20.0 800.0 500.0 nil t nil 1.0 nil))
            (should (> (file-attribute-size (file-attributes file)) 1000)))
        (delete-file file)))))

(ert-deftest excali-dsl-insert-centers-and-selects ()
  (excali-test--in-window
   (setq excali--elements nil excali--canvas-size '(800 . 600) excali--pixel-scale 1.0)
   (let* ((inserted (excali-dsl-insert "node a\nnode b right of a\nedge a -> b"))
          (bounds (excali--elements-bounds inserted)))
     (should (= (length excali--elements) (length inserted)))
     (should (< (abs (- (/ (+ (nth 0 bounds) (nth 2 bounds)) 2) 400)) 1))
     (should (< (abs (- (/ (+ (nth 1 bounds) (nth 3 bounds)) 2) 300)) 1))
     (should excali--selection))))

(ert-deftest excali-dsl-preview-association-survives-buffer-switch ()
  "First preview must store its association in the source, not the canvas."
  (let ((source (generate-new-buffer " *dsl source*"))
        (scene (generate-new-buffer " *dsl scene*")) (opens 0))
    (unwind-protect
        (cl-letf (((symbol-function 'excali--open)
                   (lambda (&rest _)
                     (cl-incf opens)
                     (set-buffer scene)
                     scene))
                  ((symbol-function 'excali-zoom-to-fit) #'ignore)
                  ((symbol-function 'excali--history-reset) #'ignore)
                  ((symbol-function 'excali--render) #'ignore)
                  ((symbol-function 'display-buffer) #'ignore))
          (with-current-buffer source
            (insert "node a")
            (excali-dsl-mode))
          (dotimes (_ 2)
            (with-current-buffer source (should (eq (excali-dsl-render) scene)))
            (should (eq (buffer-local-value 'excali-dsl--scene-buffer source) scene)))
          (should (= opens 1))
          (should-not (buffer-local-value 'excali-dsl--scene-buffer scene)))
      (kill-buffer source)
      (kill-buffer scene))))

(ert-deftest excali-dsl-cold-start-commands ()
  "Exercise the documented autoload setup without preloading excali."
  (let* ((root (file-name-directory (directory-file-name excali-dsl-test--fixtures)))
         (root (file-name-directory (directory-file-name root)))
         (form
          '(progn
             (require 'cl-lib)
             (autoload 'excali-dsl-mode "excali-dsl" nil t)
             (add-to-list 'auto-mode-alist '("\\.excalidsl\\'" . excali-dsl-mode))
             (with-temp-buffer
               (setq buffer-file-name "/tmp/cold-start.excalidsl")
               (insert "node a\nnode b right of a\nedge a -> b")
               (normal-mode)
               (cl-assert (eq major-mode 'excali-dsl-mode))
               (cl-assert (not (featurep 'excali)))
               (cl-assert (eq (key-binding (kbd "C-c C-c")) 'excali-dsl-render))
               (cl-assert (eq (key-binding (kbd "C-c C-e")) 'excali-dsl-export))
               (let ((file (make-temp-file "excali-cold-export" nil ".excalidraw")))
                 (unwind-protect
                     (progn
                       (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file)))
                         (call-interactively (key-binding (kbd "C-c C-e"))))
                       (cl-assert (= 5 (length (alist-get 'elements (excali--read-file file))))))
                   (delete-file file)))
               ;; Batch Emacs cannot show a canvas, but the command must load
               ;; the application and reach that explicit capability check.
               (condition-case err
                   (call-interactively (key-binding (kbd "C-c C-c")))
                 (error
                  (cl-assert (featurep 'excali))
                  (cl-assert (string-match-p "graphical Emacs" (error-message-string err)))))))))
    (with-temp-buffer
      (let ((status (call-process (expand-file-name invocation-name invocation-directory)
                                  nil t nil "-Q" "--batch" "-L" root
                                  "--eval" (prin1-to-string form))))
        (ert-info ((buffer-string)) (should (equal status 0)))))))

(provide 'excali-dsl-test)
;;; excali-dsl-test.el ends here
