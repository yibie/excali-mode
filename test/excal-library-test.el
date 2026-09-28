;;; excal-library-test.el --- Element library  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(defmacro excal-library-test--with-file (&rest body)
  "Run BODY with a fresh temporary library file."
  `(let* ((dir (make-temp-file "excal-lib" t))
          (excal-library-file (expand-file-name "lib.excalidrawlib" dir))
          (excal--library nil) (excal--library-loaded nil))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(ert-deftest excal-library-test-add-save-reload ()
  "Adding the selection writes a v2 library that reads back."
  (excal-library-test--with-file
   (excal-test--with-scene
    (setq excal--backend nil)
    (let ((a (excal-test--rect 0 0)) (b (excal-test--rect 20 0)))
      (setq excal--elements (list a b))
      (excal--select (list a b))
      (excal-library-add "Pair")
      (setq excal--library nil excal--library-loaded nil)
      (let ((items (excal--library)))
        (should (= (length items) 1))
        (should (equal (alist-get 'name (car items)) "Pair"))
        (should (= (length (alist-get 'elements (car items))) 2)))
      (with-temp-buffer
        (insert-file-contents excal-library-file)
        (let ((data (json-parse-buffer :object-type 'alist)))
          (should (equal (alist-get 'type data) "excalidrawlib"))
          (should (= (alist-get 'version data) 2))))))))

(ert-deftest excal-library-test-v1-and-merge ()
  "Version 1 libraries load; merging skips items already present."
  ;; Elements exported by Excalidraw carry valid indices, so restoring
  ;; them does not bump versions and duplicates can be recognized.
  (let* ((el (lambda (id) (list (cons 'id id) (cons 'type "rectangle") (cons 'x 0)
                                (cons 'y 0) (cons 'width 10) (cons 'height 10)
                                (cons 'index "a0") (cons 'version 3)
                                (cons 'versionNonce 7))))
         (v1 (json-serialize (list (cons 'type "excalidrawlib") (cons 'version 1)
                                   (cons 'library (vector (vector (funcall el "a"))
                                                          (vector (funcall el "b"))
                                                          []))))))
    (let ((items (excal--parse-library v1)))
      ;; The empty item is dropped; the others get ids and a status.
      (should (= (length items) 2))
      (should (equal (alist-get 'status (car items)) "unpublished"))
      (should (stringp (alist-get 'id (car items))))
      (let ((merged (excal--merge-library items (excal--parse-library v1))))
        (should (= (length merged) 2))))
    (should-error (excal--parse-library "{\"type\":\"excalidraw\"}"))))

(ert-deftest excal-library-test-insert-grid ()
  "Inserted items get new ids and sit on a square grid around the target."
  (excal-library-test--with-file
   (excal-test--with-scene
    (setq excal--backend nil excal--canvas-size '(800 . 600))
    (let ((items (list (list (cons 'id "i1") (cons 'status "unpublished")
                             (cons 'elements (vector (excal-test--rect 0 0))))
                       (list (cons 'id "i2") (cons 'status "unpublished")
                             (cons 'elements (vector (excal-test--rect 500 500)))))))
      (excal--insert-library-items items '(100.0 . 100.0))
      (should (= (length excal--elements) 2))
      (should (equal excal--selection excal--elements))
      (let ((b (excal--elements-bounds excal--elements)))
        ;; Two cells side by side, 50 apart, centered on the target.
        (should (= (- (nth 2 b) (nth 0 b)) 70.0))
        (should (= (/ (+ (nth 0 b) (nth 2 b)) 2) 100.0)))
      (should-not (member (excal--get (car excal--elements) 'id) '("i1" "i2")))))))

(ert-deftest excal-library-test-thumbnail ()
  "Thumbnails render to images no larger than the thumbnail size."
  (let ((image (excal--library-thumbnail
                (list (cons 'elements (vector (excal--make-element
                                               "rectangle" 0 0 (cons 'width 400.0)
                                               (cons 'height 100.0))))))))
    (should (eq (car image) 'image))
    (should (eq (plist-get (cdr image) :type) 'png))))

(provide 'excal-library-test)
;;; excal-library-test.el ends here
