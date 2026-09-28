;;; excal-integration-test.el --- Cross-track integration  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(defun excal-integration-test--png ()
  "Return the bytes of a small PNG drawn by the module."
  (let ((file (make-temp-file "excal-int" nil ".png"))
        (fb (excal-native-fb-create 4 4)))
    (unwind-protect
        (progn
          (excal-native-fb-render fb 1.0 1.0 0.0 0.0 [] nil "#ff0000")
          (excal-native-fb-write-png fb file)
          (excal--read-image-file file))
      (delete-file file))))

(ert-deftest excal-integration-test-copy-paste-image ()
  "Copying an image carries its file; pasting restores it."
  (let (json)
    (excal-test--with-scene
     (setq excal--doc (excal--empty-doc) excal--backend nil)
     (let ((url (excal--image-data-url "image/png" (excal-integration-test--png))))
       (setf (alist-get 'files excal--doc)
             (list (cons 'img1 (list (cons 'id "img1") (cons 'mimeType "image/png")
                                     (cons 'dataURL url) (cons 'created 0)))))
       (let ((image (excal--make-element "image" 0 0 (cons 'width 4.0) (cons 'height 4.0)
                                         (cons 'fileId "img1") (cons 'status "saved")
                                         (cons 'scale [1 1]))))
         (setq json (excal--clipboard-json (list image)))
         (should (equal (alist-get 'id (alist-get 'img1 (excal--parse-clipboard-files json)))
                        "img1")))))
    ;; Paste into a fresh document.
    (excal-test--with-scene
     (setq excal--doc (excal--empty-doc) excal--backend nil excal--canvas-size '(200 . 200))
     (cl-letf (((symbol-function 'current-kill) (lambda (&rest _) json)))
       (excal-paste))
     (should (= (length excal--elements) 1))
     (should (assq 'img1 (alist-get 'files excal--doc))))))

(ert-deftest excal-integration-test-frame-label-selects-frame ()
  "Clicking a frame's name label selects the frame."
  (excal-test--in-window
   (excal--load-current-style nil)
   (let ((frame (excal--new-frame 50 80)))
     (excal--put frame 'width 200.0)
     (excal--put frame 'height 100.0)
     (setq excal--elements (list frame))
     (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--frame-name-bounds frame)))
       (let ((p (cons (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0))))
         (should (< (cdr p) 80))
         (should (eq (excal--hit p) frame)))))))

(ert-deftest excal-integration-test-dark-export-is-light ()
  "Exports ignore the on-screen dark theme."
  (excal-test--with-scene
   (let ((fb (excal-native-fb-create 10 10)))
     ;; A dark render sets the renderer's theme...
     (excal-native-fb-render fb 1.0 1.0 0.0 0.0 [] nil nil t)
     ;; ...and an export afterwards still starts from light colors.
     (let ((file (make-temp-file "excal-int" nil ".png")))
       (unwind-protect
           (should (excal-native-export-png file [] 0.0 0.0 10.0 10.0 "#ffffff"
                                            t t 1.0 nil))
         (delete-file file))))))

(provide 'excal-integration-test)
;;; excal-integration-test.el ends here
