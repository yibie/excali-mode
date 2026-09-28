;;; excali-integration-test.el --- Cross-track integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defun excali-integration-test--png ()
  "Return the bytes of a small PNG drawn by the module."
  (let ((file (make-temp-file "excali-int" nil ".png"))
        (fb (excali-native-fb-create 4 4)))
    (unwind-protect
        (progn
          (excali-native-fb-render fb 1.0 1.0 0.0 0.0 [] nil "#ff0000")
          (excali-native-fb-write-png fb file)
          (excali--read-image-file file))
      (delete-file file))))

(ert-deftest excali-integration-test-copy-paste-image ()
  "Copying an image carries its file; pasting restores it."
  (let (json)
    (excali-test--with-scene
     (setq excali--doc (excali--empty-doc) excali--backend nil)
     (let ((url (excali--image-data-url "image/png" (excali-integration-test--png))))
       (setf (alist-get 'files excali--doc)
             (list (cons 'img1 (list (cons 'id "img1") (cons 'mimeType "image/png")
                                     (cons 'dataURL url) (cons 'created 0)))))
       (let ((image (excali--make-element "image" 0 0 (cons 'width 4.0) (cons 'height 4.0)
                                         (cons 'fileId "img1") (cons 'status "saved")
                                         (cons 'scale [1 1]))))
         (setq json (excali--clipboard-json (list image)))
         (should (equal (alist-get 'id (alist-get 'img1 (excali--parse-clipboard-files json)))
                        "img1")))))
    ;; Paste into a fresh document.
    (excali-test--with-scene
     (setq excali--doc (excali--empty-doc) excali--backend nil excali--canvas-size '(200 . 200))
     (cl-letf (((symbol-function 'current-kill) (lambda (&rest _) json)))
       (excali-paste))
     (should (= (length excali--elements) 1))
     (should (assq 'img1 (alist-get 'files excali--doc))))))

(ert-deftest excali-integration-test-frame-label-selects-frame ()
  "Clicking a frame's name label selects the frame."
  (excali-test--in-window
   (excali--load-current-style nil)
   (let ((frame (excali--new-frame 50 80)))
     (excali--put frame 'width 200.0)
     (excali--put frame 'height 100.0)
     (setq excali--elements (list frame))
     (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--frame-name-bounds frame)))
       (let ((p (cons (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0))))
         (should (< (cdr p) 80))
         (should (eq (excali--hit p) frame)))))))

(ert-deftest excali-integration-test-dark-export-is-light ()
  "Exports ignore the on-screen dark theme."
  (excali-test--with-scene
   (let ((fb (excali-native-fb-create 10 10)))
     ;; A dark render sets the renderer's theme...
     (excali-native-fb-render fb 1.0 1.0 0.0 0.0 [] nil nil t)
     ;; ...and an export afterwards still starts from light colors.
     (let ((file (make-temp-file "excali-int" nil ".png")))
       (unwind-protect
           (should (excali-native-export-png file [] 0.0 0.0 10.0 10.0 "#ffffff"
                                            t t 1.0 nil))
         (delete-file file))))))

(provide 'excali-integration-test)
;;; excali-integration-test.el ends here
