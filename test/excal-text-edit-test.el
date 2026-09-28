;;; excal-text-edit-test.el --- On-canvas text editing  -*- lexical-binding: t; -*-

;; Keys go through the real command loop with `execute-kbd-macro', in a
;; window showing a buffer with the excal keymap; text widths are mocked
;; to 10px per character.

(require 'ert)
(require 'excal)
(require 'excal-test)

(defmacro excal-text-edit-test--scene (&rest body)
  "Run BODY in a window-backed scene with the excal keymap."
  `(excal-test--in-window
    (use-local-map excal-mode-map)
    (setq excal--elements nil buffer-read-only t)
    (excal--load-current-style nil)
    (clrhash excal--char-width-cache)
    (clrhash excal--line-width-cache)
    (unwind-protect
        (cl-letf (((symbol-function 'excal--line-width)
                   (lambda (line &rest _) (* 10.0 (length line)))))
          (excal--history-reset)
          ,@body)
      (when excal--text-edit (excal--text-edit-teardown))
      (clrhash excal--char-width-cache)
      (clrhash excal--line-width-cache))))

(defun excal-text-edit-test--keys (keys)
  "Type KEYS, a `kbd' string or an event vector, as the user would."
  (execute-kbd-macro (if (stringp keys) (kbd keys) keys)))

(defun excal-text-edit-test--text ()
  "Return the text of the edit in progress."
  (excal--text-edit-string))

(defun excal-text-edit-test--new-text ()
  "Start editing a new text element at 100, 100 and return it."
  (excal--insert-text 100 100)
  (car (last excal--elements)))

(ert-deftest excal-text-edit-test-typing ()
  "Characters, tool letters included, go into the text; RET breaks lines."
  (excal-text-edit-test--scene
   (let ((text (excal-text-edit-test--new-text)))
     (should excal-text-edit-mode)
     (excal-text-edit-test--keys "r a <backspace> e c t RET x")
     (should (equal (excal-text-edit-test--text) "rect\nx"))
     (should (equal (excal--get text 'originalText) "rect\nx"))
     (should (eq excal--tool 'select))
     ;; Live layout: two lines of 20px at font size 20, line height 1.25.
     (should (= (excal--get text 'width) 40.0))
     (should (= (excal--get text 'height) 50.0)))))

(ert-deftest excal-text-edit-test-submit ()
  "Escape submits: the text stays selected and history records one step."
  (excal-text-edit-test--scene
   (let ((text (excal-text-edit-test--new-text)))
     (excal-text-edit-test--keys "h i")
     (should (= (length excal--undo-stack) 1))
     (excal-text-edit-test--keys "<escape>")
     (should-not excal--text-edit)
     (should-not excal-text-edit-mode)
     (should (equal (excal--get text 'originalText) "hi"))
     (should (equal excal--selection (list text)))
     (should (= (length excal--undo-stack) 2))
     (should buffer-read-only)
     (should (= (buffer-size) 0))
     (excal-undo)
     (should (null excal--elements)))))

(ert-deftest excal-text-edit-test-empty-deleted ()
  "Submitting empty text deletes the element."
  (excal-text-edit-test--scene
   (excal-text-edit-test--new-text)
   (excal-text-edit-test--keys "SPC C-<return>")
   (should (null excal--elements))
   (should (null excal--selection))))

(ert-deftest excal-text-edit-test-cancel-restores ()
  "C-g puts existing text back."
  (excal-text-edit-test--scene
   (let ((text (excal--make-text-element 0 0 "old")))
     (setq excal--elements (list text))
     (excal--select (list text))
     (excal-text-edit-test--keys "RET")
     (should excal--text-edit)
     (excal-text-edit-test--keys "C-a n e w C-g")
     (should-not excal--text-edit)
     (should (equal (excal--get text 'originalText) "old")))))

(ert-deftest excal-text-edit-test-emacs-keys ()
  "Keys only Emacs binds edit the text; undo and kill work within it."
  (excal-text-edit-test--scene
   (excal-text-edit-test--new-text)
   (excal-text-edit-test--keys "a b c SPC d e f C-a C-k")
   (should (equal (excal-text-edit-test--text) ""))
   (excal-text-edit-test--keys "C-y")
   (should (equal (excal-text-edit-test--text) "abc def"))
   (excal-text-edit-test--keys "M-b M-DEL")
   (should (equal (excal-text-edit-test--text) "def"))
   (should excal--text-edit)))

(ert-deftest excal-text-edit-test-excal-key-submits ()
  "A key the excal keymap binds submits, then runs."
  (excal-text-edit-test--scene
   (let ((text (excal-text-edit-test--new-text))
         (theme excal--theme))
     (excal-text-edit-test--keys "x M-D")
     (should-not excal--text-edit)
     (should (equal (excal--get text 'originalText) "x"))
     (should-not (eq excal--theme theme)))))

(ert-deftest excal-text-edit-test-tab-indents ()
  "Tab indents the touched lines by four spaces; S-Tab outdents."
  (excal-text-edit-test--scene
   (excal-text-edit-test--new-text)
   (excal-text-edit-test--keys "a RET b TAB")
   (should (equal (excal-text-edit-test--text) "a\n    b"))
   (excal-text-edit-test--keys "<backtab>")
   (should (equal (excal-text-edit-test--text) "a\nb"))))

(ert-deftest excal-text-edit-test-positions ()
  "Caret indices map onto wrapped lines, dropping spaces at soft breaks."
  (excal-text-edit-test--scene
   (let ((text (excal--make-text-element 0 0 "")))
     (excal--put text 'autoResize :false)
     (excal--put text 'width 40.0)
     (setq excal--elements (list text))
     (excal--set-text text "aaa bbb\ncc")
     (should (equal (excal--get text 'text) "aaa\nbbb\ncc"))
     (should (equal (excal--text-edit-positions text)
                    [(0 . 0) (0 . 1) (0 . 2) (0 . 3) (1 . 0) (1 . 1) (1 . 2) (1 . 3)
                     (2 . 0) (2 . 1) (2 . 2)]))
     (should (equal (excal--text-edit-xy text '(1 . 2)) '(20.0 . 25.0))))))

(ert-deftest excal-text-edit-test-vertical-motion ()
  "Up and down move by wrapped lines, keeping the column."
  (excal-text-edit-test--scene
   (let ((text (excal--make-text-element 0 0 "")))
     (excal--put text 'autoResize :false)
     (excal--put text 'width 40.0)
     (setq excal--elements (list text))
     (excal--set-text text "aaa bbb")
     (excal--select (list text))
     (excal-text-edit-test--keys "RET")
     ;; Point is at the end, line 1 column 3.
     (excal-text-edit-test--keys "<up>")
     (should (= (excal--text-edit-index) 3))
     (excal-text-edit-test--keys "<left> <down>")
     (should (= (excal--text-edit-index) 6))
     (excal-text-edit-test--keys "S-<up>")
     (should (use-region-p))
     (should (= (- (region-end) (region-beginning)) 4)))))

(ert-deftest excal-text-edit-test-click-moves-caret ()
  "A click on the text moves the caret; a click elsewhere submits."
  (excal-text-edit-test--scene
   (let ((text (excal--make-text-element 0 0 "abcd")))
     (setq excal--elements (list text))
     (excal--select (list text))
     (excal-text-edit-test--keys "RET")
     (let ((posn (excal-test--posn 21 5)))
       (excal-text-edit-test--keys
        (vector (list 'down-mouse-1 posn) (list 'mouse-1 posn))))
     (should (= (excal--text-edit-index) 2))
     (let ((posn (excal-test--posn 300 300)))
       (excal-text-edit-test--keys
        (vector (list 'down-mouse-1 posn) (list 'mouse-1 posn))))
     (should-not excal--text-edit)
     ;; The click went on to deselect.
     (should (null excal--selection)))))

(ert-deftest excal-text-edit-test-container-label ()
  "RET on a shape edits a new centered label; the shape stays selected."
  (excal-text-edit-test--scene
   (let ((rect (excal--make-element "rectangle" 0 0 (cons 'width 100.0)
                                    (cons 'height 40.0))))
     (setq excal--elements (list rect))
     (excal--select (list rect))
     (excal-text-edit-test--keys "RET l a b e l <escape>")
     (let ((label (excal--bound-text-of rect)))
       (should label)
       (should (equal (excal--get label 'originalText) "label"))
       (should (equal (excal--get label 'textAlign) "center"))
       (should (equal excal--selection (list rect))))
     ;; A cancelled new label leaves no trace.
     (let ((other (excal--make-element "rectangle" 200 0 (cons 'width 100.0)
                                       (cons 'height 40.0))))
       (setq excal--elements (append excal--elements (list other)))
       (excal--deselect)
       (excal--select (list other))
       (excal-text-edit-test--keys "RET x C-g")
       (should-not (excal--bound-text-of other))
       (should (eq (alist-get 'boundElements other) :null))))))

(ert-deftest excal-text-edit-test-overlays ()
  "While editing, the caret replaces the selection box and handles."
  (excal-text-edit-test--scene
   (excal-text-edit-test--new-text)
   (excal-text-edit-test--keys "a b")
   (let ((overlays (excal--overlay-natives)))
     (should (= (length (seq-filter (lambda (o) (equal (aref o 0) "ov-rect")) overlays)) 1))
     (should-not (seq-find (lambda (o) (member (aref o 0) '("ov-handle" "ov-circle")))
                           overlays))
     ;; The caret sits after "ab".
     (let ((caret (seq-find (lambda (o) (equal (aref o 0) "ov-rect")) overlays)))
       (should (< (abs (- (+ (aref caret 1) (/ (aref caret 3) 2)) 120)) 1e-6))))))

(ert-deftest excal-text-edit-test-canvas-resync-keeps-edit ()
  "Resizing the canvas keeps the text being edited and point."
  (excal-text-edit-test--scene
   (excal-text-edit-test--new-text)
   (excal-text-edit-test--keys "a b c <left>")
   (setq excal--canvas-size nil excal--backend 'canvas)
   (excal--sync-canvas (selected-window))
   (should (equal (excal-text-edit-test--text) "abc"))
   (should (= (excal--text-edit-index) 2))
   (excal-text-edit-test--keys "x <escape>")
   (should (equal (excal--get (car excal--elements) 'originalText) "abxc"))
   ;; Only the canvas is left.
   (should (= (buffer-size) 1))))

(ert-deftest excal-text-edit-test-input-method ()
  "Input methods insert through the buffer like any typing."
  (excal-text-edit-test--scene
   (let ((text (excal-text-edit-test--new-text)))
     (activate-input-method "chinese-py")
     (unwind-protect
         (progn
           ;; Quail reads "ni" and SPC, then hands over the character.
           (setq unread-command-events (listify-key-sequence "ni "))
           (while unread-command-events
             (let* ((keys (vconcat (read-key-sequence nil)))
                    (last-command-event (aref keys (1- (length keys)))))
               (command-execute (key-binding keys) nil keys)))
           (should (equal (excal--get text 'originalText) "你")))
       (deactivate-input-method)))))

(provide 'excal-text-edit-test)
;;; excal-text-edit-test.el ends here
