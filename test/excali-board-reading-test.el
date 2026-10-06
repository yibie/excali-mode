;;; excali-board-reading-test.el --- Reading interactions -*- lexical-binding: t; -*-
(require 'excali-board-test)
(require 'excali-export)

(ert-deftest excali-board-reading-utf8-link-ranges ()
  (let* ((block (aref (excali-board-content-blocks "文字 [[https://example.org][A & B]] end") 0))
         (span (aref (aref block 2) 0)))
    (should (= 7 (aref span 0)))
    (should (= 12 (aref span 1)))
    (should (equal "https" (aref (aref span 2) 1)))
    (should-not (string-match-p "excali-link" (aref block 0)))
    (should (string-match-p "&amp;" (aref block 0)))))

(ert-deftest excali-board-reading-table-and-heading-links ()
  (let ((blocks (excali-board-content-blocks
                 "* [[https://example.org][Heading]]
| [[file:other.org][Cell]] | Plain |
")))
    (should (= 1 (length (aref (aref blocks 0) 2))))
    (should (= 1 (length (aref (aref blocks 1) 2))))))

(ert-deftest excali-board-reading-native-hit-wrap-and-blank ()
  (let* ((blocks (excali-board-content-blocks "[[https://example.org][MMMMMMMMMMMMMMMMMMMM]]"))
         (size (excali-native-board-measure blocks 100)))
    (should (> (aref size 1) 25))
    (should (equal [0 0] (excali-native-board-hit blocks 100 1 8)))
    (should (excali-native-board-hit blocks 100 1 30))
    (should-not (excali-native-board-hit blocks 100 200 8))
    (should-not (excali-native-board-hit blocks 100 1 10000))))

(ert-deftest excali-board-reading-safe-click-dispatch ()
  (excali-board-test--with
    (let* ((card (excali-board-new-note "[[https://example.org][Visit]]"))
           (info (aref (aref (aref (aref (excali-board--blocks card) 0) 2) 0) 2))
           url)
      (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq url u))))
        (excali-board-reading-open card info)
        (should (equal "https://example.org" url)))
      (excali-board-note--set card "[[elisp:(error \"do not run\")][Unsafe]]")
      (setq info (aref (aref (aref (aref (excali-board--blocks card) 0) 2) 0) 2))
      (should-error (excali-board-reading-open card info) :type 'user-error))))

(ert-deftest excali-board-reading-stale-link-not-followed ()
  (excali-board-test--with
    (let* ((card (excali-board-new-note "[[https://example.org][Visit]]"))
           (info (aref (aref (aref (aref (excali-board--blocks card) 0) 2) 0) 2)))
      (excali-board-note--set card "[[https://elsewhere.org][Visit]]")
      (should-error (excali-board-reading-open card info) :type 'user-error))))

(ert-deftest excali-board-reading-local-link-resolves-vault ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (let* ((card (excali-board-new-note "[[file:notes.org][Source]]"))
           (info (aref (aref (aref (aref (excali-board--blocks card) 0) 2) 0) 2))
           opened)
      (cl-letf (((symbol-function 'find-file-other-window) (lambda (f) (setq opened f))))
        (excali-board-reading-open card info)
        (should (file-equal-p opened file))))))

(ert-deftest excali-board-reading-image-layout-and-no-document-embedding ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (let ((image-file (expand-file-name "sample.svg" dir)))
      (with-temp-file image-file
        (insert "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='100'><rect width='200' height='100' fill='red'/></svg>"))
      (let* ((card (excali-board-new-note "[[file:sample.svg]]"))
             (before (copy-tree (excali--get card 'customData) t))
             (blocks (excali-board--blocks card))
             (size (excali-native-board-measure blocks 140)))
        (should (string-prefix-p "board-inline-" (aref (aref blocks 0) 3)))
        (should (= 100 (aref size 0)))
        (should (= 50 (aref size 1)))
        (should (equal [0 -1] (excali-native-board-hit blocks 140 10 10)))
        (should (equal before (excali--get card 'customData)))
        (should-not (alist-get 'files excali--doc))
        (delete-file image-file)
        (should-not (aref (aref (excali-board--blocks card) 0) 3))))))

(ert-deftest excali-board-reading-missing-and-remote-image-no-fetch ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (let* ((card (excali-board-new-note "[[file:missing.png]]

[[https://example.org/remote.png]]"))
           (blocks (excali-board--blocks card)))
      (should-not (aref (aref blocks 0) 3))
      (should (= 3 (length (aref blocks 1))))
      (should (string-match-p "Image" (aref (aref blocks 0) 0))))))

(ert-deftest excali-board-reading-scroll-and-rotation-hit ()
  (excali-board-test--with
    (let* ((card (excali-board-new-note "[[https://example.org][Link]]"))
           (point (cons (+ 22 (excali--get card 'x)) (+ 52 (excali--get card 'y)))))
      (should (equal "https" (aref (excali-board-reading-link-at card point) 1)))
      (excali--put card 'angle 0.5)
      (setq point (excali--rotate-point point (excali--box-center (excali--element-box card)) 0.5))
      (should (excali-board-reading-link-at card point))
      (should-not (excali-board-reading-link-at card '(0 . 0))))))

(ert-deftest excali-board-reading-source-marker-and-attachment-context ()
  (excali-board-test--with
    (with-current-buffer source
      (goto-char (point-min))
      (org-entry-put nil "DIR" "./")
      (org-end-of-subtree t t)
      (insert "\n[[attachment:picture.svg]]\n[[file:notes.org][Source link]]\n"))
    (with-temp-file (expand-file-name "picture.svg" dir)
      (insert "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='30'><rect width='40' height='30' fill='red'/></svg>"))
    (let ((card (excali-board--insert ref "Old"))
          (org-attach-use-inheritance t))
      (excali-board--refresh-card card)
      (let* ((blocks (excali-board--blocks card))
             (image (seq-find (lambda (b) (> (length b) 3)) blocks))
             (link-block (seq-find (lambda (b) (and (> (length b) 2)
                                                    (> (length (aref b 2)) 0))) blocks))
             (info (aref (aref link-block 2) 0))
             (marker (excali-board-reading--source-marker card (aref info 2))))
        (should (stringp (aref image 3)))
        (should (eq source (marker-buffer marker)))
        (set-marker marker nil)
        (let (opened)
          (cl-letf (((symbol-function 'find-file-other-window) (lambda (f) (setq opened f))))
            (excali-board-reading-open card (aref info 2))
            (should (file-equal-p opened file))))
        (excali--put card 'height 500.0)
        (excali-export-png "/tmp/excali-reading-verified.png")
        (let ((svg (expand-file-name "inline.svg" dir)))
          (excali-export-svg svg)
          (should (> (file-attribute-size (file-attributes svg)) 1000)))))))

(ert-deftest excali-board-reading-link-hit-after-card-scroll ()
  (excali-board-test--with
    (let* ((card (excali-board-new-note
                  (concat "* Title\n" (make-string 400 ?x)
                          "\n\n[[https://example.org][Link]]\n\n"
                          (make-string 400 ?z))))
           (blocks (excali-board--blocks card))
           (prefix (vconcat (seq-take blocks 2)))
           (offset (+ 12 (aref (excali-native-board-measure prefix 360) 1))))
      (excali-board--scroll card 0 offset)
      (should (= offset (cdr (excali-board--scroll-values card))))
      (should (equal "https"
                     (aref (excali-board-reading-link-at
                            card (cons (+ 22 (excali--get card 'x))
                                       (+ 52 (excali--get card 'y)))) 1))))))

(ert-deftest excali-board-reading-mouse-opens-only-after-click-release ()
  (excali-board-test--with
    (let* ((card (excali-board-new-note "[[https://example.org][Link]]"))
           (point (cons (+ 22 (excali--get card 'x)) (+ 52 (excali--get card 'y))))
           (release '(mouse-1)) opened)
      (cl-letf (((symbol-function 'excali--select-event-view) #'ignore)
                ((symbol-function 'excali--event-scene-xy) (lambda (_) point))
                ((symbol-function 'excali-board--card-at) (lambda (_) card))
                ((symbol-function 'excali--await-release) (lambda () release))
                ((symbol-function 'excali-board-reading-open)
                 (lambda (c _) (setq opened c))))
        (excali-board-mouse-down '(down-mouse-1))
        (should (eq opened card))
        (setq opened nil release '(drag-mouse-1))
        (excali-board-mouse-down '(down-mouse-1))
        (should-not opened)))))

(provide 'excali-board-reading-test)
