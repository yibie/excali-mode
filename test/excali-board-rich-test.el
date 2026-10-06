;;; excali-board-rich-test.el --- Rich board regression coverage -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'excali-board-test)
(require 'excali-export)

(ert-deftest excali-board-content-complete-safe-and-styled ()
  (let* ((text "* Heading <&>\n:PROPERTIES:\n:SECRET: hidden\n:END:\n*bold* /italic/ =literal= [[https://example.org][Link]]\n\n- [X] Finished\n\n** Child\nChild body\n\n#+begin_src emacs-lisp\n(error \"never execute\")\n#+end_src\n")
         (blocks (excali-board-content-blocks text))
         (joined (mapconcat (lambda (b) (aref b 0)) blocks "\n")))
    (should (string-match-p "<b>Heading &lt;&amp;&gt;</b>" joined))
    (should (string-match-p "<b>bold</b>" joined))
    (should (string-match-p "<i>italic</i>" joined))
    (should (string-match-p (regexp-quote "[x] Finished") joined))
    (should (string-match-p "Child body" joined))
    (should (string-match-p "never execute" joined))
    (should-not (string-match-p "SECRET" joined))
    (seq-doseq (block blocks) (should-not (text-properties-at 0 (aref block 0))))))

(ert-deftest excali-board-content-table-widths-independent-and-repeatable ()
  (let* ((text "| A | A much wider column |\n| B | value |\n")
         (first (excali-board-content-blocks text)))
    (should (equal first (excali-board-content-blocks text)))
    (should (string-prefix-p "<tt>│ A │" (aref (aref first 0) 0)))
    (should (= 1 (aref (aref first 0) 1)))))

(ert-deftest excali-board-rich-refresh-and-resize-never-grow-card ()
  (excali-board-test--with
   (let ((card (excali-board--insert ref "Old")))
     (with-current-buffer source
       (goto-char (point-min)) (org-end-of-subtree t t)
       (insert (make-string 4000 ?a)))
     (excali-board--refresh-card card)
     (should (= 220 (excali--get card 'height)))
     (should (= 360 (excali--get card 'width)))
     (should (string-match-p "Child body"
                             (mapconcat (lambda (b) (aref b 0))
                                        (excali-board--blocks card) "\n")))
     (excali--put card 'height 110.0)
     (excali--put card 'width 180.0)
     (excali--layout-bound-text card 'se)
     (should (= 110 (excali--get card 'height)))
     (should (= 180 (excali--get card 'width))))))

(ert-deftest excali-board-native-measure-wraps-and-keeps-wide-blocks ()
  (let ((wrapped (vector (vector (make-string 500 ?x) 0)))
        (wide (vector (vector (make-string 500 ?x) 1))))
    (should (> (aref (excali-native-board-measure wrapped 160) 1)
               (aref (excali-native-board-measure wrapped 400) 1)))
    (should (> (aref (excali-native-board-measure wide 160) 0) 160))
    (should (equal [0.0 0.0] (excali-native-board-measure [] 160)))
    (should (vectorp (excali-native-board-measure [["<broken>" 0] [] 42] 160)))))

(ert-deftest excali-board-scroll-clamps-persists-and-preserves-geometry ()
  (excali-board-test--with
   (let ((card (excali-board--insert ref "Old")))
     (with-current-buffer source
       (goto-char (point-min)) (org-end-of-subtree t t)
       (dotimes (_ 100) (insert "\nLong paragraph\n")))
     (excali-board--refresh-card card)
     (excali-board--scroll card 999999 999999)
     (let ((scroll (excali-board--scroll-values card)))
       (should (= (car scroll) 0))
       (should (< 0 (cdr scroll) 999999))
       (should (= 220 (excali--get card 'height)))
       (setq excali--file (expand-file-name "scroll.excalidraw" dir))
       (excali-board-save)
       (let ((saved (aref (alist-get 'elements (excali--read-file excali--file)) 0)))
         (should (= (car (excali-board--scroll-values saved)) (car scroll)))
         (should (= (cdr (excali-board--scroll-values saved)) (cdr scroll)))))
     (excali-board--scroll card -999999 -999999)
     (should (equal '(0 . 0) (excali-board--scroll-values card))))))

(ert-deftest excali-board-native-cache-and-fallback ()
  (excali-board-test--with
   (let ((card (excali-board--insert ref "Old")))
     (excali-board--refresh-card card)
     (let* ((label (excali--bound-text-of card))
            (native (excali--native-element card)))
       (should (eq native (excali--native-element card)))
       (should (= 0 (aref (excali--native-element label) 15)))
       (let ((excali-native-element-function nil))
         (should (= 100 (aref (excali--native-element label) 15))))
       (excali--touch card)
       (should-not (eq native (excali--native-element card)))))))

(ert-deftest excali-board-rich-cache-survives-unavailable-source ()
  (excali-board-test--with
   (let ((card (excali-board--insert ref "Old")))
     (excali-board--refresh-card card)
     (let ((cache (copy-tree (excali-board--blocks card) t)))
       (with-current-buffer source (erase-buffer))
       (excali-board-refresh-all)
       (should excali-board--errors)
       (should (equal cache (excali-board--blocks card)))
       (should (vectorp (excali--native-element card)))))))

(ert-deftest excali-board-indirect-editor-edits-subtree-not-copy ()
  (excali-board-test--with
   (let* ((card (excali-board--insert ref "Old"))
          (editor (excali-board--make-editor card)))
     (unwind-protect
         (progn
           (with-current-buffer editor
             (should (eq (buffer-base-buffer) source))
             (should (buffer-narrowed-p))
             (goto-char (point-max))
             (insert "\nEdited within card.\n")
             (should-not (string-match-p "Second heading" (buffer-string))))
           (with-current-buffer source
             (should (buffer-modified-p))
             (should (string-match-p "Edited within card" (buffer-string)))))
       (when (buffer-live-p editor)
         (with-current-buffer editor (excali-board-edit-finish))))
     (should (buffer-live-p source))
     (should-not (buffer-live-p editor))
     (should (string-match-p "Edited within card"
                             (alist-get 'sourceText
                                        (alist-get 'excaliBoardContent
                                                   (excali--get card 'customData)))))
     (with-temp-buffer
       (insert-file-contents file)
       (should-not (string-match-p "Edited within card" (buffer-string)))))))

(ert-deftest excali-board-replace-source-retains-id-and-layout ()
  (excali-board-test--with
   (let* ((card (excali-board--insert ref "Old"))
          (id (excali--get card 'id))
          (marker (with-current-buffer source
                    (goto-char (point-max))
                    (org-back-to-heading t) (point-marker))))
     (cl-letf (((symbol-function 'excali-board--read-heading) (lambda (_) marker)))
       (excali-board-replace-source file))
     (should (equal id (excali--get card 'id)))
     (should (= 60 (excali--get card 'x)))
     (should (= 220 (excali--get card 'height)))
     (should-not (equal "board-heading" (alist-get 'id (excali-board--reference card))))
     (should (string-match-p "Second heading"
                             (alist-get 'sourceText
                                        (alist-get 'excaliBoardContent
                                                   (excali--get card 'customData))))))))

(ert-deftest excali-board-connect-binds-and-follows-moved-card ()
  (excali-board-test--with
   (let* ((one (excali-board--insert ref "One"))
          (two (excali-board--insert ref "Two")))
     (excali--select (list one two))
     (let ((arrow (excali-board-connect "supports")))
       (should (equal (excali--get one 'id) (excali--binding-element-id arrow 'start)))
       (should (equal (excali--get two 'id) (excali--binding-element-id arrow 'end)))
       (should (equal "supports" (excali--get (excali--bound-text-of arrow) 'originalText)))
       (let ((old (copy-tree (excali--linear-global-points arrow))))
         (excali--put two 'x (+ 200 (excali--get two 'x)))
         (excali--touch two)
         (excali--update-bound-arrows (list two))
         (should-not (equal old (excali--linear-global-points arrow)))))
     (with-current-buffer source (should-not (buffer-modified-p))))))

(ert-deftest excali-board-attachment-stays-local-and-opens-safely ()
  (excali-board-test--with
   (excali-board-insert-attachment file)
   (let* ((card (car excali--selection))
          opened)
     (should (equal file (alist-get 'file (alist-get 'excaliBoardAttachment
                                                     (excali--get card 'customData)))))
     (cl-letf (((symbol-function 'find-file-other-window)
                (lambda (f) (setq opened f)
                  (should-not enable-local-eval))))
       (excali-board-open-attachment))
     (should (equal opened file))
     (should-error (excali-board-insert-attachment "/ssh:host:/tmp/test.pdf")
                   :type 'user-error))))

(ert-deftest excali-board-rich-export-renders-native-content ()
  (excali-board-test--with
   (let* ((card (excali-board--insert ref "Old"))
          (png (expand-file-name "rich.png" dir))
          (svg (expand-file-name "rich.svg" dir)))
     (excali-board--refresh-card card)
     (excali-export-png png)
     (excali-export-svg svg)
     (should (> (file-attribute-size (file-attributes png)) 1000))
     (should (> (file-attribute-size (file-attributes svg)) 1000)))))

(ert-deftest excali-board-native-rich-content-clips-to-card ()
  (excali-board-test--with
   (let* ((card (excali-board--insert ref "Old"))
          (a (excali-native-fb-create 520 360))
          (b (excali-native-fb-create 520 360)))
     (excali-board--refresh-card card)
     (let ((excali-native-element-function nil))
       (excali-native-fb-render a 1.0 1.0 0.0 0.0
                                (vector (excali--native-element card)) nil))
     (excali-native-fb-render b 1.0 1.0 0.0 0.0
                              (vector (excali--native-element card)) nil)
     (should (> (excali-native-fb-diff a b) 0))
     ;; Text and scrollbars must not paint beyond the rectangle.
     (dolist (y '(0 30 58 282 300 359))
       (dotimes (x 520)
         (should (= (excali-native-fb-pixel a x y)
                    (excali-native-fb-pixel b x y)))))
     (dolist (x '(0 30 58 422 450 519))
       (dotimes (y 360)
         (should (= (excali-native-fb-pixel a x y)
                    (excali-native-fb-pixel b x y))))))))

(ert-deftest excali-board-editor-save-saves-base-not-board ()
  (excali-board-test--with
   (let* ((card (excali-board--insert ref "Old"))
          (editor (excali-board--make-editor card)))
     (unwind-protect
         (with-current-buffer editor
           (goto-char (point-max)) (insert "\nExplicitly saved.\n")
           (save-buffer))
       (when (buffer-live-p editor)
         (with-current-buffer editor (excali-board-edit-finish))))
     (with-temp-buffer
       (insert-file-contents file)
       (should (string-match-p "Explicitly saved" (buffer-string))))
     (should-not excali--file))))

(ert-deftest excali-board-wheel-hover-needs-no-selection ()
  (excali-board-test--with
    (let ((card (excali-board--insert ref "Old")) delegated)
      (with-current-buffer source
        (goto-char (point-min)) (org-end-of-subtree t t)
        (insert (make-string 2000 ?a)))
      (excali-board--refresh-card card)
      (setq excali--selection nil)
      (cl-letf (((symbol-function 'excali--select-event-view) #'ignore)
                ((symbol-function 'excali--event-scene-xy) #'cadr)
                ((symbol-function 'excali-wheel) (lambda (_) (setq delegated t))))
        (excali-board-wheel '(wheel-down (100 . 150)))
        (should-not delegated)
        (should (> (cdr (excali-board--scroll-values card)) 0))
        (should-not excali--selection)
        (dolist (event '((wheel-down (100 . 75))
                         (wheel-down (10 . 10))
                         (C-wheel-down (100 . 150))))
          (setq delegated nil)
          (excali-board-wheel event)
          (should delegated))))))

(ert-deftest excali-board-scrollbar-drag-does-not-move-card ()
  (excali-board-test--with
    (let ((card (excali-board--insert ref "Old")) (commits 0))
      (with-current-buffer source
        (goto-char (point-min)) (org-end-of-subtree t t)
        (insert (make-string 2000 ?a)))
      (excali-board--refresh-card card)
      (setq excali--selection nil excali--tool 'select)
      (cl-letf (((symbol-function 'excali--select-event-view) #'ignore)
                ((symbol-function 'excali--event-scene-xy) #'cadr)
                ((symbol-function 'excali--commit) (lambda () (cl-incf commits)))
                ((symbol-function 'excali--drag-loop)
                 (lambda (move &optional _) (funcall move '(drag-mouse-1 (414 . 240))))))
        (excali-board-mouse-down '(down-mouse-1 (414 . 110))))
      (should (= 1 commits))
      (should (> (cdr (excali-board--scroll-values card)) 0))
      (should (= 60 (excali--get card 'x)))
      (should (= 60 (excali--get card 'y)))
      (should (= 220 (excali--get card 'height)))
      (should-not excali--selection))))

(ert-deftest excali-board-scrollbars-horizontal-and-rotation ()
  (excali-board-test--with
    (let ((card (excali-board--insert ref "Old")))
      (with-current-buffer source
        (goto-char (point-min)) (org-end-of-subtree t t)
        (insert "\n#+begin_example\n" (make-string 500 ?a) "\n#+end_example\n"))
      (excali-board--refresh-card card)
      (should (eq 'x (car (excali-board--scrollbar-at card '(100 . 214)))))
      (should-not (excali-board--scrollbar-at card '(100 . 90)))
      (excali--put card 'angle (/ float-pi 2))
      (let* ((scene '(160 . 274))
             (rotated (excali--rotate-point scene
                                            (excali--box-center (excali--element-box card))
                                            (/ float-pi 2)))
             (local (excali-board--local-point card rotated)))
        (should (< (abs (- (car local) 100)) 0.001))
        (should (< (abs (- (cdr local) 214)) 0.001))))))

(ert-deftest excali-board-nonscrollbar-mouse-delegates ()
  (excali-board-test--with
    (let ((card (excali-board--insert ref "Old")) delegated)
      (excali-board--refresh-card card)
      (cl-letf (((symbol-function 'excali--select-event-view) #'ignore)
                ((symbol-function 'excali--event-scene-xy) #'cadr)
                ((symbol-function 'excali-mouse-down) (lambda (_) (setq delegated t))))
        (excali-board-mouse-down '(down-mouse-1 (100 . 80)))
        (should delegated))
      (should (eq (lookup-key excali-board-mode-map [excali-canvas down-mouse-1])
                  #'excali-board-mouse-down))
      (should (eq (lookup-key excali-mode-map [down-mouse-1]) #'excali-mouse-down)))))

(ert-deftest excali-board-wheel-survives-global-precision-mode ()
  (require 'pixel-scroll)
  (let ((was-enabled pixel-scroll-precision-mode))
    (unwind-protect
        (progn
          (pixel-scroll-precision-mode 1)
          (excali-board-test--with
            (should (eq (key-binding [wheel-down]) #'excali-board-wheel))
            (should (eq (key-binding [wheel-up]) #'excali-board-wheel))
            (should (eq (key-binding [excali-canvas wheel-down]) #'excali-board-wheel))
            (should (eq (key-binding [touch-end]) #'ignore))
            (should (eq (command-remapping 'mwheel-scroll) #'excali-board-wheel)))
          (with-temp-buffer
            (org-mode)
            (should (eq (key-binding [wheel-down]) #'pixel-scroll-precision))))
      (pixel-scroll-precision-mode (if was-enabled 1 -1)))))

(ert-deftest excali-board-wheel-continuous-events-advance-content ()
  (excali-board-test--with
    (let ((card (excali-board--insert ref "Old")))
      (with-current-buffer source
        (goto-char (point-min)) (org-end-of-subtree t t)
        (dotimes (_ 120) (insert "\nLong paragraph for continuous wheel input.\n")))
      (excali-board--refresh-card card)
      (setq excali--selection nil)
      (cl-letf (((symbol-function 'excali--select-event-view) #'ignore)
                ((symbol-function 'excali--event-scene-xy) #'cadr))
        (dotimes (_ 30)
          (let ((old (cdr (excali-board--scroll-values card))))
            (excali-board-wheel '(wheel-down (100 . 150) 1 1 (0 . 3.5)))
            (should (> (cdr (excali-board--scroll-values card)) old))))))))

(ert-deftest excali-board-wheel-burst-key-dispatch-not-ignore ()
  (excali-board-test--with
    (dolist (direction '("up" "down" "left" "right"))
      (dolist (count '("" "double-" "triple-"))
        (dolist (modifiers '("" "S-" "C-" "C-S-"))
          (let ((key (kbd (format "%s<%swheel-%s>" modifiers count direction))))
            (should (eq (key-binding key t) #'excali-board-wheel))
            (should (eq (key-binding (vconcat [excali-canvas] key) t)
                        #'excali-board-wheel))))))))

(ert-deftest excali-board-wheel-burst-replays-through-hotspot-bindings ()
  (excali-board-test--with
    (let ((card (excali-board--insert ref "Old")))
      (with-current-buffer source
        (goto-char (point-min)) (org-end-of-subtree t t)
        (dotimes (_ 150) (insert "\nContinuous burst paragraph.\n")))
      (excali-board--refresh-card card)
      (setq excali--selection nil)
      (cl-letf (((symbol-function 'excali--select-event-view) #'ignore)
                ((symbol-function 'excali--event-scene-xy) #'cadr))
        ;; Mirrors the user's NS burst.  Crucially, look up the actual
        ;; prefixed binding instead of calling the wheel handler directly.
        (dolist (name '(wheel-down double-wheel-down triple-wheel-down
                       triple-wheel-down triple-wheel-down triple-wheel-down))
          (let* ((old (cdr (excali-board--scroll-values card)))
                 (command (key-binding (vector 'excali-canvas name) t)))
            (funcall command (list name '(100 . 150) 3 1 '(0.0 . -20.0)))
            (should (> (cdr (excali-board--scroll-values card)) old))))))))

(provide 'excali-board-rich-test)
;;; excali-board-rich-test.el ends here
