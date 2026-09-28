;;; excali-restore-test.el --- Tests for restore, save and fractional indices  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)

(defconst excali-restore-test--dir
  (expand-file-name "fixtures" (file-name-directory
                                (or load-file-name buffer-file-name))))

(defun excali-restore-test--fixture (name)
  "Return the path of fixture NAME."
  (expand-file-name name excali-restore-test--dir))

(defun excali-restore-test--load (name)
  "Read and restore fixture NAME."
  (excali--restore-doc (excali--read-file (excali-restore-test--fixture name))))

(defun excali-restore-test--save (doc)
  "Return the text excali saves for restored DOC."
  (excali--serialize-doc doc (append (alist-get 'elements doc) nil)))

(defun excali-restore-test--parse (text)
  "Parse JSON TEXT like `excali--read-file'."
  (json-parse-string text :object-type 'alist :array-type 'array
                     :null-object :null :false-object :false))

(defun excali-restore-test--normalize (value &optional drop)
  "Return VALUE with object keys sorted and keys in DROP removed."
  (cond
   ((vectorp value) (vconcat (mapcar (lambda (v) (excali-restore-test--normalize v drop))
                                     value)))
   ((and (consp value) (consp (car value)) (symbolp (caar value)))
    (sort (delq nil (mapcar (lambda (cell)
                              (unless (memq (car cell) drop)
                                (cons (car cell)
                                      (excali-restore-test--normalize (cdr cell) drop))))
                            value))
          (lambda (a b) (string< (car a) (car b)))))
   (t value)))

(defun excali-restore-test--json-equal (a b)
  "Return non-nil if normalized JSON values A and B are equal.
Numbers compare with a small tolerance, so 60 equals 60.0."
  (cond
   ((and (numberp a) (numberp b)) (< (abs (- a b)) 1e-9))
   ((and (vectorp a) (vectorp b))
    (and (= (length a) (length b))
         (cl-every #'excali-restore-test--json-equal a b)))
   ((and (consp a) (consp b))
    (and (excali-restore-test--json-equal (car a) (car b))
         (excali-restore-test--json-equal (cdr a) (cdr b))))
   (t (equal a b))))

(defconst excali-restore-test--volatile '(version versionNonce updated)
  "Element keys that restore may legitimately change.")

(defun excali-restore-test--element (doc id)
  "Return the element with ID in DOC."
  (seq-find (lambda (e) (equal (alist-get 'id e) id)) (alist-get 'elements doc)))

(defmacro excali-restore-test--should-json (actual expected)
  "Assert that JSON values ACTUAL and EXPECTED are equal, ignoring volatile keys."
  `(let ((a (excali-restore-test--normalize ,actual excali-restore-test--volatile))
         (e (excali-restore-test--normalize ,expected excali-restore-test--volatile)))
     (should (excali-restore-test--json-equal a e))))

;;;; Lossless round trip

(ert-deftest excali-restore-test-current-format-is-byte-identical ()
  "A current-format file with unknown types and fields saves byte for byte."
  (let* ((file (excali-restore-test--fixture "current.excalidraw"))
         (original (with-temp-buffer
                     (let ((coding-system-for-read 'utf-8))
                       (insert-file-contents file))
                     (buffer-string))))
    (should (equal (excali-restore-test--save (excali-restore-test--load "current.excalidraw"))
                   original))))

(ert-deftest excali-restore-test-unknown-elements-untouched ()
  "Unknown element types and fields, embeddables, iframes and magicframes survive."
  (let* ((raw (excali--read-file (excali-restore-test--fixture "current.excalidraw")))
         (doc (excali-restore-test--load "current.excalidraw")))
    (dolist (id '("chart1" "emb1" "ifr1" "mf1" "rect1"))
      (should (equal (excali-restore-test--element doc id)
                     (excali-restore-test--element raw id))))
    (should (equal (alist-get 'myPluginSetting (alist-get 'appState doc))
                   '((theme . "x"))))
    (should (equal (alist-get 'pluginExtra doc) "kept"))))

(ert-deftest excali-restore-test-save-through-buffer ()
  "`excali-save' writes the restored scene and current style."
  (let ((out (make-temp-file "excali" nil ".excalidraw")))
    (unwind-protect
        (with-temp-buffer
          (setq excali--native-cache (make-hash-table :test #'eq)
                excali--doc (excali-restore-test--load "current.excalidraw")
                excali--elements (append (alist-get 'elements excali--doc) nil)
                excali--file out)
          (excali--load-current-style (alist-get 'appState excali--doc))
          (excali-save)
          (should (equal (excali--read-file out)
                         (excali--read-file (excali-restore-test--fixture
                                            "current.excalidraw")))))
      (delete-file out))))

;;;; Legacy files

(ert-deftest excali-restore-test-legacy-v1 ()
  "A v1 file resaves in the current format, as upstream restores it."
  (let* ((saved (excali-restore-test--parse
                 (excali-restore-test--save (excali-restore-test--load "legacy-v1.excalidraw"))))
         (expected (excali--read-file (excali-restore-test--fixture "legacy-v1.expected.json"))))
    (should (equal (alist-get 'version saved) 2))
    (excali-restore-test--should-json (alist-get 'elements saved)
                                     (alist-get 'elements expected))
    (excali-restore-test--should-json (alist-get 'appState saved)
                                     (alist-get 'appState expected))
    (should (null (alist-get 'files saved)))))

(ert-deftest excali-restore-test-legacy-bindings ()
  "Focus/gap bindings become fixed-point bindings; references are repaired."
  (let* ((doc (excali-restore-test--load "legacy-bindings.excalidraw"))
         (el (lambda (id) (excali-restore-test--element doc id)))
         (binding (lambda (id key)
                    (excali-restore-test--normalize (alist-get key (funcall el id))))))
    ;; Endpoint inside the shape: "inside", fixed point at the endpoint.
    (excali-restore-test--should-json
     (funcall binding "in" 'startBinding)
     '((elementId . "R") (fixedPoint . [0.3 0.6]) (mode . "inside")))
    ;; Outside, no midpoint and no diagonal hit: the endpoint itself.
    (excali-restore-test--should-json
     (funcall binding "free" 'endBinding)
     '((elementId . "E") (fixedPoint . [-0.15 0.3]) (mode . "orbit")))
    ;; Outside: projected onto the rectangle's shrunk diagonal.
    (excali-restore-test--should-json
     (funcall binding "proj" 'endBinding)
     '((elementId . "R") (fixedPoint . [0.33513513513513515 0.6648648648648648])
       (mode . "orbit")))
    ;; Two-point arrow bound at both ends: rays start at the other end's
    ;; (0.5001, 0.5001) point.
    (excali-restore-test--should-json
     (funcall binding "both" 'startBinding)
     '((elementId . "R") (fixedPoint . [0.8157941826602575 0.18420581733974245])
       (mode . "orbit")))
    (excali-restore-test--should-json
     (funcall binding "both" 'endBinding)
     '((elementId . "D") (fixedPoint . [0.5001 0.833322839311839]) (mode . "orbit")))
    ;; Elbow arrows keep their binding keys; the fixed point is normalized.
    (should (equal (alist-get 'startBinding (funcall el "elbow"))
                   '((elementId . "R") (focus . 0) (gap . 1)
                     (fixedPoint . [1.5 0.5001]) (mode . "orbit"))))
    (should (equal (alist-get 'endBinding (funcall el "elbow"))
                   '((elementId . "E") (fixedPoint . [0.5001 0.5001]) (mode . "inside"))))
    (should (eq (alist-get 'fixedSegments (funcall el "elbow")) :null))
    ;; Lines never bind; a v2 binding without elementId is dropped; a v2
    ;; binding keeps only its three keys.
    (should (eq (alist-get 'startBinding (funcall el "line")) :null))
    (should (eq (alist-get 'startBinding (funcall el "noid")) :null))
    (should (equal (alist-get 'endBinding (funcall el "noid"))
                   '((elementId . "E") (mode . "orbit") (fixedPoint . [1 0.25]))))
    ;; Frame membership and container repair.
    (should (eq (alist-get 'frameId (funcall el "R")) :null))
    (should (equal (alist-get 'boundElements (funcall el "R")) []))
    (should (equal (alist-get 'boundElements (funcall el "B"))
                   [((type . "text") (id . "T1")) ((type . "text") (id . "T2"))]))
    (should (= (alist-get 'angle (funcall el "T1")) 0.5))
    (should (= (alist-get 'angle (funcall el "T2")) 0.5))
    (should (eq (alist-get 'containerId (funcall el "orphan")) :null))
    (should (= (alist-get 'angle (funcall el "orphan")) 0))
    (should (eq (alist-get 'baseFontSize (funcall el "orphan")) :null))
    ;; Bound text moves right after its container and gets new indices.
    (should (equal (mapcar (lambda (e) (alist-get 'id e)) (alist-get 'elements doc))
                   '("R" "E" "D" "in" "free" "proj" "both" "elbow" "line" "noid"
                     "B" "T1" "T2" "Tdel" "orphan")))
    (should (equal (mapcar (lambda (e) (alist-get 'index e))
                           (seq-drop (append (alist-get 'elements doc) nil) 10))
                   '("aB" "aBV" "aC" "aD" "aE")))))

(ert-deftest excali-restore-test-existing-elements-for-bindings ()
  "Legacy bindings to elements outside the payload use EXISTING."
  (let* ((box '((id . "X") (type . "rectangle") (x . 0) (y . 0)
                (width . 100) (height . 100) (strokeWidth . 2)))
         (arrow `((id . "arr") (type . "arrow") (x . 30) (y . 60)
                  (points . [[0 0] [100 0]])
                  (startBinding . ((elementId . "X") (focus . 0) (gap . 1)))))
         (restored (car (excali--restore-elements (list arrow) :existing (list box)))))
    (should (equal (alist-get 'startBinding restored)
                   '((mode . "inside") (elementId . "X") (fixedPoint . [0.3 0.6]))))
    (should (eq (alist-get 'startBinding
                           (car (excali--restore-elements (list arrow))))
                :null))))

(ert-deftest excali-restore-test-fonts-arrowheads-and-types ()
  "Line heights per font, fallbacks, arrowheads and per-type defaults."
  (let* ((doc (excali-restore-test--load "fonts-indices.excalidraw"))
         (el (lambda (id) (excali-restore-test--element doc id))))
    (pcase-dolist (`(,id . ,lh) '(("f1" . 1.25) ("f2" . 1.15) ("f3" . 1.2) ("f4" . 1.25)
                                  ("f7" . 1.15) ("f9" . 1.15) ("f42" . 1.25)
                                  ("f1000" . 1.25) ("own" . 1.3) ("legacy" . 1.25)))
      (should (= (alist-get 'lineHeight (funcall el id)) lh)))
    (should (= (alist-get 'fontSize (funcall el "f1000")) 20))
    (should (= (alist-get 'fontFamily (funcall el "f42")) 42))
    (should (= (alist-get 'fontSize (funcall el "legacy")) 28))
    (should (= (alist-get 'fontFamily (funcall el "legacy")) 3))
    (should (= (alist-get 'labelPosition (funcall el "own")) 1))
    (should (eq (alist-get 'baseFontSize (funcall el "own")) :null))
    (should (equal (alist-get 'originalText (funcall el "f1")) "a"))
    ;; Arrowheads and point normalisation.
    (let ((a (funcall el "arrows")))
      (should (equal (alist-get 'startArrowhead a) "cardinality_one"))
      (should (equal (alist-get 'endArrowhead a) "cardinality_one_or_many"))
      (should (equal (alist-get 'points a) [[0 0] [10 0] [10 20]]))
      (should (equal (list (alist-get 'x a) (alist-get 'y a)
                           (alist-get 'width a) (alist-get 'height a))
                     '(3 4 10 20))))
    (should (equal (alist-get 'startArrowhead (funcall el "arrows2")) "circle"))
    (should (eq (alist-get 'endArrowhead (funcall el "arrows2")) :null))
    ;; Freedraw points, pressures and stroke options.
    (let ((pen (funcall el "pen")))
      (should (equal (alist-get 'points pen) [[0 0] [2 2] [3 3]]))
      (should (equal (alist-get 'pressures pen) [0.1 0.5]))
      (should (equal (alist-get 'strokeOptions pen)
                     '((variability . "variable") (streamline . 0.5))))
      (should-not (assq 'simulatePressure pen)))
    ;; Images, sticky notes, frames, oversized lines.
    (let ((img (funcall el "img")))
      (should (equal (alist-get 'status img) "pending"))
      (should (equal (alist-get 'scale img) [1 1]))
      (should (eq (alist-get 'crop img) :null)))
    (let ((note (funcall el "note")))
      (should (equal (alist-get 'backgroundColor note) "#ffdf6b"))
      (should (equal (alist-get 'strokeColor note) "#1e1e1e"))
      (should (equal (alist-get 'fillStyle note) "solid"))
      (should (equal (list (alist-get 'width note) (alist-get 'height note)
                           (alist-get 'baseHeight note))
                     '(75 75 75))))
    (should (eq (alist-get 'name (funcall el "fr")) :null))
    (let ((big (funcall el "big")))
      (should (eq (alist-get 'isDeleted big) t))
      (should (equal (alist-get 'points big) [[0 0] [100 100]])))
    ;; Unknown types are kept as they were.
    (should (equal (funcall el "u1")
                   '((id . "u1") (type . "somethingnew") (index . "zz")
                     (payload . [1 ((deep . t))]))))
    ;; App state.
    (let ((state (alist-get 'appState doc)))
      (should (= (alist-get 'gridSize state) 100))
      (should (= (alist-get 'gridStep state) 3))
      (should (eq (alist-get 'viewBackgroundColor state) :null))
      (should (equal (alist-get 'currentItemStrokeWidthKey state) "medium"))
      (should (eq (alist-get 'gridModeEnabled state) :false)))
    ;; Only files used by live images are saved.
    (let ((saved (excali-restore-test--parse (excali-restore-test--save doc))))
      (should (equal (mapcar #'car (alist-get 'files saved)) '(f))))))

(ert-deftest excali-restore-test-invalid-indices ()
  "Missing, duplicate, malformed and out-of-order indices are regenerated."
  (let ((doc (excali-restore-test--load "fonts-indices.excalidraw")))
    (should (equal (mapcar (lambda (e) (alist-get 'index e)) (alist-get 'elements doc))
                   '("Zz" "a0" "a0V" "a1" "a3" "a4" "zz" "a5" "a6" "a7" "a8" "a9"
                     "aA" "aB" "aC" "aD" "aE" "aF")))))

(ert-deftest excali-restore-test-idempotent ()
  "Restoring a restored scene changes nothing but versions."
  (dolist (name '("legacy-v1.excalidraw" "legacy-bindings.excalidraw"
                  "fonts-indices.excalidraw" "current.excalidraw"))
    (let* ((once (excali-restore-test--load name))
           (text (excali-restore-test--save once))
           (twice (excali--restore-doc (excali-restore-test--parse text))))
      (excali-restore-test--should-json (alist-get 'elements twice)
                                       (alist-get 'elements once))
      (excali-restore-test--should-json (alist-get 'appState twice)
                                       (alist-get 'appState once))
      (excali-restore-test--should-json (excali-restore-test--parse
                                        (excali-restore-test--save twice))
                                       (excali-restore-test--parse text)))))

(ert-deftest excali-restore-test-clipboard-restore ()
  "Clipboard payloads restore without binding repair; selection is dropped."
  (let ((restored (excali--restore-elements
                   (vector '((id . "s") (type . "selection"))
                           '((id . "a") (type . "rectangle") (width . 10) (height . 10))
                           '((id . "a") (type . "ellipse") (width . 0) (height . 0)))
                   :delete-invisible t)))
    (should (= (length restored) 2))
    (should (equal (alist-get 'id (car restored)) "a"))
    ;; The duplicate id is replaced.
    (should-not (equal (alist-get 'id (cadr restored)) "a"))
    (should (eq (alist-get 'isDeleted (cadr restored)) t))
    (should (equal (mapcar (lambda (e) (alist-get 'index e)) restored) '("a0" "a1")))))

(ert-deftest excali-restore-test-rejects-other-files ()
  "Only Excalidraw scenes open."
  (should-error (excali--restore-doc '((type . "excalidrawlib") (version . 2)))
                :type 'user-error))

(ert-deftest excali-restore-test-empty-doc ()
  "A new scene has upstream's exported app state."
  (let ((doc (excali--restore-doc (excali--empty-doc))))
    (should (equal (alist-get 'appState doc) (alist-get 'appState (excali--empty-doc))))
    (should (equal (excali-restore-test--save doc)
                   (concat "{\n  \"type\": \"excalidraw\",\n  \"version\": 2,\n"
                           "  \"source\": \"https://excalidraw.com\",\n"
                           "  \"elements\": [],\n  \"appState\": {\n"
                           "    \"gridSize\": 20,\n    \"gridStep\": 5,\n"
                           "    \"gridModeEnabled\": false,\n"
                           "    \"viewBackgroundColor\": \"#ffffff\",\n"
                           "    \"lockedMultiSelections\": {}\n  },\n"
                           "  \"files\": {}\n}")))))

;;;; JSON output

(ert-deftest excali-restore-test-json-numbers ()
  "Numbers print like JavaScript's Number#toString."
  (pcase-dolist (`(,x . ,s) '((0 . "0") (-0.0 . "0") (60.0 . "60") (-3.0 . "-3")
                              (0.1 . "0.1") (123.456 . "123.456") (1e-7 . "1e-7")
                              (1e-6 . "0.000001") (1e-5 . "0.00001")
                              (1.5e20 . "150000000000000000000") (1e21 . "1e+21")
                              (0.30000000000000004 . "0.30000000000000004")
                              (12345678901234567890.0 . "12345678901234567000")
                              (5e-324 . "5e-324")))
    (should (equal (excali--json-number x) s)))
  (should (equal (excali--json-number 1.0e+NaN) "null")))

(ert-deftest excali-restore-test-json-strings ()
  "Strings escape like JSON.stringify and keep non-ASCII text."
  (should (equal (excali--json-encode "a\"b\\c\nd\te\^Afé中")
                 "\"a\\\"b\\\\c\\nd\\te\\u0001fé中\""))
  (should (equal (excali--json-encode '((a . []) (b) (c . [1 [2]])))
                 "{\n  \"a\": [],\n  \"b\": {},\n  \"c\": [\n    1,\n    [\n      2\n    ]\n  ]\n}")))

(ert-deftest excali-restore-test-json-matches-node ()
  "The saved text equals JSON.stringify(JSON.parse(text), null, 2)."
  (skip-unless (executable-find "node"))
  (dolist (name '("legacy-v1.excalidraw" "legacy-bindings.excalidraw"
                  "fonts-indices.excalidraw"))
    (let* ((text (excali-restore-test--save (excali-restore-test--load name)))
           (file (make-temp-file "excali" nil ".json" text)))
      (unwind-protect
          (should (equal (with-temp-buffer
                           (let ((coding-system-for-read 'utf-8))
                             (call-process "node" nil t nil "-e"
                                           "const fs=require('fs');process.stdout.write(JSON.stringify(JSON.parse(fs.readFileSync(process.argv[1],'utf8')),null,2))"
                                           file))
                           (buffer-string))
                         text))
        (delete-file file)))))

;;;; Fractional indices

(ert-deftest excali-restore-test-index-keys ()
  "Keys match the fractional-indexing test suite."
  (pcase-dolist (`(,a ,b ,expected)
                 '((nil nil "a0") (nil "a0" "Zz") (nil "Zz" "Zy") ("a0" nil "a1")
                   ("a1" nil "a2") ("a0" "a1" "a0V") ("a1" "a2" "a1V")
                   ("a0V" "a1" "a0l") ("Zz" "a0" "ZzV") ("Zz" "a1" "a0")
                   (nil "Y00" "Xzzz") ("bzz" nil "c000") ("a0" "a0V" "a0G")
                   ("a0" "a0G" "a08") ("b125" "b129" "b127") ("a0" "a1V" "a1")
                   ("Zz" "a01" "a0") (nil "a0V" "a0") (nil "b999" "b99")
                   (nil "A00000000000000000000000000" error)
                   (nil "A000000000000000000000000001" "A000000000000000000000000000V")
                   ("zzzzzzzzzzzzzzzzzzzzzzzzzzy" nil "zzzzzzzzzzzzzzzzzzzzzzzzzzz")
                   ("zzzzzzzzzzzzzzzzzzzzzzzzzzz" nil "zzzzzzzzzzzzzzzzzzzzzzzzzzzV")
                   ("a00" nil error) ("a00" "a1" error) ("0" "1" error)
                   ("a1" "a0" error)))
    (should (equal (condition-case nil (excali--index-between a b)
                     (excali-index-error 'error))
                   expected)))
  (should (equal (excali--index-n-between nil nil 5) '("a0" "a1" "a2" "a3" "a4")))
  (should (equal (excali--index-n-between "a4" nil 10)
                 '("a5" "a6" "a7" "a8" "a9" "aA" "aB" "aC" "aD" "aE")))
  (should (equal (excali--index-n-between nil "a0" 5) '("Zv" "Zw" "Zx" "Zy" "Zz")))
  (should (equal (excali--index-n-between "a0" "a2" 20)
                 (split-string "a04 a08 a0G a0K a0O a0V a0Z a0d a0l a0t a1 a14 a18 a1G a1O a1V a1Z a1d a1l a1t"))))

(defun excali-restore-test--indices ()
  "Return the scene's indices."
  (mapcar (lambda (e) (alist-get 'index e)) excali--elements))

(defun excali-restore-test--increasing-p ()
  "Return non-nil if the scene's indices are valid and strictly increasing."
  (let ((keys (excali-restore-test--indices)))
    (and (cl-every #'excali--valid-order-key-p keys)
         (cl-every #'string< keys (cdr keys)))))

(defmacro excali-restore-test--with-scene (n &rest body)
  "Run BODY in a scene of N indexed rectangles bound to `rects'."
  (declare (indent 1))
  `(with-temp-buffer
     (setq excali--native-cache (make-hash-table :test #'eq)
           excali--backend nil
           excali--elements (cl-loop repeat ,n
                                    collect (excali--make-element "rectangle" 0 0
                                                                 (cons 'width 10.0)
                                                                 (cons 'height 10.0))))
     (excali--sync-indices)
     (let ((rects (copy-sequence excali--elements)))
       (ignore rects)
       (cl-letf (((symbol-function 'excali--render) #'ignore))
         ,@body))))

(ert-deftest excali-restore-test-reorder-keeps-indices ()
  "Z-order commands give only the moved elements new, increasing keys."
  (excali-restore-test--with-scene 4
    (should (equal (excali-restore-test--indices) '("a0" "a1" "a2" "a3")))
    (pcase-let ((`(,a ,b ,c ,d) rects))
      (excali--select (list a))
      (excali-bring-to-front)
      (should (equal excali--elements (list b c d a)))
      (should (equal (excali-restore-test--indices) '("a1" "a2" "a3" "a4")))
      (excali-send-to-back)
      (should (equal (excali-restore-test--indices) '("a0" "a1" "a2" "a3")))
      (excali-bring-forward)
      (should (equal excali--elements (list b a c d)))
      (should (equal (excali-restore-test--indices) '("a1" "a1V" "a2" "a3")))
      (excali--select (list c d))
      (excali-send-backward)
      (should (excali-restore-test--increasing-p))
      (should (equal excali--elements (list b c d a))))))

(ert-deftest excali-restore-test-paste-and-duplicate-indices ()
  "Pasted and duplicated elements get keys above the top element."
  (excali-restore-test--with-scene 3
    (excali--select (list (car rects)))
    (excali-duplicate)
    (should (= (length excali--elements) 4))
    (should (equal (excali-restore-test--indices) '("a0" "a1" "a2" "a3")))
    (let ((kill-ring nil))
      (excali--select (list (car rects) (cadr rects)))
      (excali-copy)
      (cl-letf (((symbol-function 'excali--mouse-scene-xy) (lambda () '(0 . 0))))
        (excali-paste)))
    (should (= (length excali--elements) 6))
    (should (excali-restore-test--increasing-p))
    (should (equal (last (excali-restore-test--indices) 2) '("a4" "a5")))))

(ert-deftest excali-restore-test-post-command-sync ()
  "New elements appended without an index get one before history commits."
  (excali-restore-test--with-scene 2
    (let ((new (excali--make-element "rectangle" 5 5)))
      (setq excali--elements (append excali--elements (list new)))
      (should-not (excali--indices-in-order-p))
      (let ((version (alist-get 'version new)))
        (excali--sync-indices-maybe)
        (should (equal (alist-get 'index new) "a2"))
        (should (> (alist-get 'version new) version)))
      (should (excali--indices-in-order-p))
      ;; Nothing to do: no element changes.
      (let ((versions (mapcar (lambda (e) (alist-get 'version e)) excali--elements)))
        (excali--sync-indices-maybe)
        (should (equal versions (mapcar (lambda (e) (alist-get 'version e))
                                        excali--elements)))))))

(ert-deftest excali-restore-test-sync-moved-falls-back ()
  "An impossible move falls back to repairing the invalid indices."
  (excali-restore-test--with-scene 3
    (pcase-let ((`(,a ,b ,c) rects))
      ;; b's neighbours are out of order, so no key fits between them.
      (excali--put a 'index "a5")
      (excali--sync-moved-indices (list b))
      (should (excali-restore-test--increasing-p))
      (ignore c))))

(ert-deftest excali-restore-test-make-element-defaults ()
  "New elements carry every base field of upstream `_newElementBase'."
  (let ((e (excali--make-element "rectangle" 1 2)))
    (dolist (key '(id type x y width height angle strokeColor backgroundColor
                      fillStyle strokeWidth strokeStyle roughness opacity groupIds
                      frameId index roundness seed version versionNonce isDeleted
                      boundElements updated created link locked))
      (should (assq key e)))
    (should (eq (alist-get 'index e) :null))
    (should (integerp (alist-get 'created e)))))

;;; excali-restore-test.el ends here
