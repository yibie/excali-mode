;;; excal-text-edit.el --- On-canvas (WYSIWYG) text editing  -*- lexical-binding: t; -*-

;;; Commentary:

;; Upstream's textWysiwyg.tsx puts a <textarea> over the text being
;; edited (docs/excalidraw-spec.md §2b.13).  Here the "textarea" is a
;; stretch of real buffer text: while an edit is on, the excal buffer
;; starts with the text being edited, followed by a newline and then
;; the canvas.  That stretch is covered by an overlay displaying as
;; nothing, so the display is unchanged, and point lives inside it
;; (point adjustment, which would move it out, is off meanwhile).
;;
;; Because the text is ordinary buffer text, all of Emacs's editing
;; applies unchanged: self-insertion, input methods (quail inserts its
;; preedit into the buffer, and so does the NS port's system IME through
;; `ns-put-working-text'), kill and yank, shift selection, undo.  Every
;; change is copied into the element (`excal--preview-text') and the
;; scene re-rendered; the caret and the selection are drawn as
;; overlays computed from point and mark against the wrapped layout.
;;
;; A modal `read-event' loop was rejected: it bypasses the command loop
;; that input methods and the NS text-input events rely on.
;;
;; Keys follow upstream: Enter inserts a newline; Escape, C-RET and
;; s-RET submit; Tab and S-Tab indent and outdent the lines touched by
;; the region by four spaces.  C-g cancels, restoring the text (an
;; Emacs addition; upstream has no cancel).  Keys the minor mode does
;; not bind fall through: a key the excal keymap binds (a tool letter
;; is not one, it is self-inserted) or a click outside the text submits
;; the edit and then runs as usual; other keys (C-a, M-f, C-k, M-x,
;; wheel events, ...) run as they would in any buffer.  Empty text is
;; deleted on submit.  The whole edit is one undo step of the scene.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-history)
(require 'excal-text)

(declare-function excal--drag-loop "excal-edit")
(declare-function excal--preview-text "excal-edit")
(declare-function excal--container-geometry "excal-edit")
(declare-function excal--restore-geometry "excal-edit")
(declare-function excal-increase-font-size "excal-actions")
(declare-function excal-decrease-font-size "excal-actions")
(defvar excal-mode-map)
(defvar excal-text-edit-mode)

(defconst excal--text-edit-tab-size 4 "TAB_SIZE of the text editor.")

(defvar-local excal--text-edit nil
  "The text edit in progress, or nil.
A plist: :element, :container, :geometry (the container's box before
the edit), :original (text), :new (non-nil if the element was created
for this edit), :bound (the container's `boundElements' before a new
label), :beg and :end (markers around the edited text), :overlay,
:read-only, :undo (the buffer's saved `buffer-undo-list') and :goal-x.")

;;;; Region

(defun excal--text-edit-string ()
  "Return the text being edited."
  (let ((edit excal--text-edit))
    (buffer-substring-no-properties (plist-get edit :beg) (plist-get edit :end))))

(defun excal--text-edit-index ()
  "Return point as an index into the normalized edited text."
  (excal--text-edit-index-of (point)))

(defun excal--text-edit-index-of (pos)
  "Return buffer position POS as an index into the normalized edited text."
  (let ((beg (plist-get excal--text-edit :beg)))
    (length (excal--normalize-text
             (buffer-substring-no-properties beg (max beg (min pos (plist-get excal--text-edit :end))))))))

(defun excal--text-edit-pos-of (index)
  "Return the buffer position of normalized INDEX, the inverse of the above."
  (let ((pos (plist-get excal--text-edit :beg))
        (end (plist-get excal--text-edit :end)))
    (while (and (< pos end) (< (excal--text-edit-index-of pos) index))
      (setq pos (1+ pos)))
    pos))

(defun excal--text-edit-open-region (text)
  "Insert TEXT at the buffer start, hidden, with the canvas after it."
  (let ((buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-modification-hooks t))
    (goto-char (point-min))
    (insert text)
    (let ((end (point)))
      (insert "\n")
      ;; The separator and the canvas can't be edited from the region.
      (put-text-property end (point-max) 'read-only t)
      (put-text-property end (1+ end) 'rear-nonsticky t)
      (let ((overlay (make-overlay (point-min) (1+ end) nil nil nil)))
        ;; Not `invisible': commands such as `kill-line' skip invisible
        ;; text, which would reach past the separator.
        (overlay-put overlay 'display "")
        (overlay-put overlay 'excal-text-edit t)
        (setq excal--canvas-start (copy-marker (1+ end)))
        (list :beg (copy-marker (point-min))
              :end (copy-marker end t)
              :overlay overlay)))))

(defun excal--text-edit-close-region ()
  "Remove the edited text and the separator from the buffer."
  (let ((buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (edit excal--text-edit))
    (delete-overlay (plist-get edit :overlay))
    (delete-region (plist-get edit :beg) (1+ (plist-get edit :end)))
    (remove-list-of-text-properties (point-min) (point-max) '(read-only rear-nonsticky))
    (setq excal--canvas-start nil)
    (goto-char (point-min))))

;;;; Layout

(defun excal--text-edit-lines (element)
  "Return ELEMENT's wrapped lines."
  (split-string (or (excal--get element 'text) "") "\n"))

(defun excal--text-edit-positions (element)
  "Return a vector giving (LINE . COLUMN) for each caret index of ELEMENT.
Indices count characters of `originalText'; lines and columns refer to
the wrapped `text'.  Whitespace dropped at a soft break belongs to the
start of the next line."
  (let* ((orig (or (excal--get element 'originalText) ""))
         (wrapped (or (excal--get element 'text) ""))
         (n (length orig)) (m (length wrapped))
         (positions (make-vector (1+ n) nil))
         (i 0) (j 0) (line 0) (start 0))
    (while (< i n)
      ;; At a soft break the caret stays at the end of the line above.
      (unless (aref positions i)
        (aset positions i (cons line (- j start))))
      (cond
       ((and (< j m) (eq (aref wrapped j) (aref orig i)))
        (when (eq (aref orig i) ?\n)
          (setq line (1+ line) start (1+ j)))
        (setq i (1+ i) j (1+ j)))
       ((and (< j m) (eq (aref wrapped j) ?\n))
        ;; A soft break.
        (setq line (1+ line) start (1+ j) j (1+ j)))
       (t (setq i (1+ i)))))
    (aset positions n (cons line (- j start)))
    positions))

(defun excal--text-edit-line-height (element)
  "Return ELEMENT's line height in scene units."
  (pcase-let ((`(,size ,_family ,line-height) (excal--text-font element)))
    (* size line-height)))

(defun excal--text-edit-line-x (element line)
  "Return the offset of wrapped LINE of ELEMENT from its left edge."
  (pcase-let* ((`(,size ,family ,_) (excal--text-font element))
               (text (or (nth line (excal--text-edit-lines element)) ""))
               (width (excal--line-width text size family))
               (box (float (or (excal--get element 'width) 0))))
    (pcase (excal--get element 'textAlign)
      ("center" (/ (- box width) 2))
      ("right" (- box width))
      (_ 0.0))))

(defun excal--text-edit-xy (element position)
  "Return the unrotated scene (X . Y) of the top of caret POSITION.
POSITION is (LINE . COLUMN) from `excal--text-edit-positions'."
  (pcase-let* ((`(,size ,family ,_) (excal--text-font element))
               (`(,line . ,column) position)
               (text (or (nth line (excal--text-edit-lines element)) ""))
               (prefix (substring text 0 (min column (length text)))))
    (cons (+ (excal--get element 'x) (excal--text-edit-line-x element line)
             (excal--line-width prefix size family))
          (+ (excal--get element 'y) (* line (excal--text-edit-line-height element))))))

(defun excal--text-edit-center (element)
  "Return ELEMENT's center (X . Y)."
  (cons (+ (excal--get element 'x) (/ (excal--get element 'width) 2.0))
        (+ (excal--get element 'y) (/ (excal--get element 'height) 2.0))))

(defun excal--text-edit-local (element point)
  "Return scene POINT in ELEMENT's unrotated frame."
  (let ((angle (float (or (excal--get element 'angle) 0))))
    (if (zerop angle) point
      (excal--rotate-point point (excal--text-edit-center element) (- angle)))))

(defun excal--text-edit-index-at (element point)
  "Return the caret index of ELEMENT nearest scene POINT."
  (let* ((local (excal--text-edit-local element point))
         (positions (excal--text-edit-positions element))
         (lines (length (excal--text-edit-lines element)))
         (line (max 0 (min (1- lines)
                           (floor (/ (- (cdr local) (excal--get element 'y))
                                     (excal--text-edit-line-height element)))))))
    (excal--text-edit-nearest element positions line (car local))))

(defun excal--text-edit-nearest (element positions line x)
  "Return the index in POSITIONS on LINE whose caret is nearest X."
  (let (best best-distance)
    (dotimes (i (length positions))
      (let ((p (aref positions i)))
        (when (= (car p) line)
          (let ((d (abs (- (car (excal--text-edit-xy element p)) x))))
            (when (or (null best) (< d best-distance))
              (setq best i best-distance d))))))
    best))

;;;; Overlays

(defun excal--text-edit-rect (element x y w h &rest props)
  "Return an ov-rect at unrotated X, Y sized W, H, turned with ELEMENT."
  (let* ((angle (float (or (excal--get element 'angle) 0)))
         (center (excal--rotate-point (cons (+ x (/ w 2)) (+ y (/ h 2)))
                                      (excal--text-edit-center element) angle)))
    (apply #'excal--ov "ov-rect" (- (car center) (/ w 2)) (- (cdr center) (/ h 2)) w h
           :angle angle props)))

(defun excal--text-edit-overlays ()
  "Return the caret and selection overlays of the edit in progress."
  (when-let* ((edit excal--text-edit)
              (element (plist-get edit :element))
              ((memq element excal--elements)))
    (let* ((positions (excal--text-edit-positions element))
           (last (1- (length positions)))
           (height (excal--text-edit-line-height element))
           (caret (min last (excal--text-edit-index)))
           overlays)
      (when (use-region-p)
        (let ((from (min last (excal--text-edit-index-of (region-beginning))))
              (to (min last (excal--text-edit-index-of (region-end))))
              (fill (concat (excal--selection-color) "4d")))
          (dotimes (line (1+ (- (car (aref positions to)) (car (aref positions from)))))
            (let* ((line (+ line (car (aref positions from))))
                   (a (if (= line (car (aref positions from))) (aref positions from)
                        (cons line 0)))
                   (b (if (= line (car (aref positions to))) (aref positions to)
                        (cons line (length (nth line (excal--text-edit-lines element))))))
                   (xa (car (excal--text-edit-xy element a)))
                   (xb (max (+ xa (/ 4.0 excal--zoom)) (car (excal--text-edit-xy element b)))))
              (push (excal--text-edit-rect element xa (+ (excal--get element 'y) (* line height))
                                           (- xb xa) height :fill fill)
                    overlays)))))
      (let* ((xy (excal--text-edit-xy element (aref positions caret)))
             (w (/ 1.5 excal--zoom)))
        (push (excal--text-edit-rect element (- (car xy) (/ w 2)) (cdr xy) w height
                                     :fill (if (eq excal--theme 'dark) "#e0e0e0"
                                             (or (excal--get element 'strokeColor) "#1e1e1e")))
              overlays))
      (nreverse overlays))))

;;;; Session

(defun excal--text-edit-sync (&rest _)
  "Copy the edited text into the element and render."
  (when-let* ((edit excal--text-edit))
    (let ((element (plist-get edit :element)))
      (when (memq element excal--elements)
        (excal--preview-text element (plist-get edit :container) (plist-get edit :geometry)
                             (excal--text-edit-string))
        (excal--render)))))

(defun excal--text-edit-post-command ()
  "Keep point in the edited text and redraw the caret."
  (when-let* ((edit excal--text-edit))
    (if (not (memq (plist-get edit :element) excal--elements))
        ;; Something removed the element: drop the edit.
        (excal--text-edit-teardown)
      (goto-char (max (plist-get edit :beg) (min (point) (plist-get edit :end))))
      (unless (memq this-command '(excal-text-edit-previous-line excal-text-edit-next-line))
        (setq excal--text-edit (plist-put edit :goal-x nil)))
      (excal--render))))

(defun excal--text-edit-start (element &rest props)
  "Start editing text ELEMENT on the canvas.
PROPS may give :new (ELEMENT was just created), :bound (the container's
`boundElements' before a new label) and :at (a scene point to put the
caret at)."
  (when excal--text-edit (excal-text-edit-submit))
  (let* ((container (excal--container-of element))
         (original (or (excal--get element 'originalText) (excal--get element 'text) ""))
         (region (excal--text-edit-open-region original)))
    (setq excal--text-edit
          (append (list :element element :container container
                        :geometry (excal--container-geometry container)
                        :original original :new (plist-get props :new)
                        :bound (plist-get props :bound)
                        :read-only buffer-read-only :undo buffer-undo-list)
                  region)
          excal--history-hold t
          buffer-read-only nil
          buffer-undo-list nil)
    (deactivate-mark)
    (goto-char (if-let* ((at (plist-get props :at)))
                   (excal--text-edit-pos-of (excal--text-edit-index-at element at))
                 (plist-get region :end)))
    (setq-local global-disable-point-adjustment t)
    (add-hook 'after-change-functions #'excal--text-edit-sync nil t)
    (add-hook 'post-command-hook #'excal--text-edit-post-command 90 t)
    (excal-text-edit-mode 1)
    (excal--render)))

(defun excal--text-edit-teardown ()
  "End the edit and return its text; the element is left as it is."
  (let* ((edit excal--text-edit)
         (text (excal--text-edit-string)))
    (kill-local-variable 'global-disable-point-adjustment)
    (remove-hook 'after-change-functions #'excal--text-edit-sync t)
    (remove-hook 'post-command-hook #'excal--text-edit-post-command t)
    (excal-text-edit-mode -1)
    (deactivate-mark)
    (excal--text-edit-close-region)
    (setq buffer-read-only (plist-get edit :read-only)
          buffer-undo-list (plist-get edit :undo)
          excal--text-edit nil
          excal--history-hold nil)
    text))

(defun excal--text-edit-drop (element edit)
  "Remove ELEMENT, whose edit EDIT left it empty or was cancelled."
  (let ((container (plist-get edit :container)))
    (if container
        (progn
          (excal--remove-bound-text container element)
          (when (plist-get edit :new)
            ;; Leave a cancelled new label no trace in the container.
            (excal--put container 'boundElements (plist-get edit :bound))
            (excal--restore-geometry container (plist-get edit :geometry))))
      (setq excal--elements (delq element excal--elements)))))

(defun excal--text-edit-finish (cancel)
  "End the edit: submit it, or with CANCEL non-nil restore the text."
  (when-let* ((edit excal--text-edit))
    (let* ((element (plist-get edit :element))
           (container (plist-get edit :container))
           (text (excal--text-edit-teardown)))
      (when (memq element excal--elements)
        (cond
         ((and cancel (plist-get edit :new))
          (excal--text-edit-drop element edit))
         (cancel
          (when container (excal--restore-geometry container (plist-get edit :geometry)))
          (excal--set-text element (plist-get edit :original)))
         ((string-empty-p (string-trim text))
          (excal--text-edit-drop element edit)
          (excal--deselect))
         (t
          (excal--preview-text element container (plist-get edit :geometry) text)))
        (excal--deselect)
        (cond ((memq element excal--elements)
               (excal--select (list (or container element))))
              (container (excal--select (list container)))))
      (excal--render)
      (excal--commit))))

;;;; Commands

(defun excal-text-edit-submit ()
  "Finish editing the text (Escape, C-RET).  Empty text is deleted."
  (interactive)
  (excal--text-edit-finish nil))

(defun excal-text-edit-cancel ()
  "Stop editing and put the text back as it was."
  (interactive)
  (excal--text-edit-finish t))

(defun excal-text-edit-newline ()
  "Insert a line break."
  (interactive)
  (insert "\n"))

(defun excal--text-edit-lines-bounds ()
  "Return (BEG . END) of the buffer lines touched by point or the region."
  (let ((beg (if (use-region-p) (region-beginning) (point)))
        (end (if (use-region-p) (region-end) (point))))
    (cons (save-excursion (goto-char beg)
                          (max (plist-get excal--text-edit :beg) (line-beginning-position)))
          (save-excursion (goto-char end)
                          (min (plist-get excal--text-edit :end) (line-end-position))))))

(defun excal-text-edit-indent ()
  "Indent the lines touched by point or the region by four spaces."
  (interactive)
  (pcase-let ((`(,beg . ,end) (excal--text-edit-lines-bounds))
              (deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (let ((end (copy-marker end)))
        (goto-char beg)
        (while (progn (insert (make-string excal--text-edit-tab-size ?\s))
                      (and (search-forward "\n" end t) (< (point) end))))
        (set-marker end nil)))))

(defun excal-text-edit-outdent ()
  "Remove up to four leading spaces from the lines touched."
  (interactive)
  (pcase-let ((`(,beg . ,end) (excal--text-edit-lines-bounds))
              (deactivate-mark nil))
    (save-excursion
      (let ((end (copy-marker end)))
        (goto-char beg)
        (while (progn (let ((n 0))
                        (while (and (< n excal--text-edit-tab-size) (eq (char-after) ?\s))
                          (delete-char 1)
                          (setq n (1+ n))))
                      (and (search-forward "\n" end t) (<= (point) end))))
        (set-marker end nil)))))

(defun excal-text-edit-select-all ()
  "Select all the text being edited."
  (interactive)
  (goto-char (plist-get excal--text-edit :end))
  (push-mark (plist-get excal--text-edit :beg) t t))

(defun excal--text-edit-vertical (lines)
  "Move point LINES wrapped lines down, keeping its horizontal position."
  (let* ((element (plist-get excal--text-edit :element))
         (positions (excal--text-edit-positions element))
         (index (min (1- (length positions)) (excal--text-edit-index)))
         (here (aref positions index))
         (goal (or (plist-get excal--text-edit :goal-x)
                   (car (excal--text-edit-xy element here))))
         (target (+ (car here) lines))
         (last-line (1- (length (excal--text-edit-lines element)))))
    (setq excal--text-edit (plist-put excal--text-edit :goal-x goal))
    (goto-char
     (cond ((< target 0) (plist-get excal--text-edit :beg))
           ((> target last-line) (plist-get excal--text-edit :end))
           (t (excal--text-edit-pos-of
               (excal--text-edit-nearest element positions target goal)))))))

(defun excal-text-edit-previous-line ()
  "Move to the wrapped line above."
  (interactive "^")
  (excal--text-edit-vertical -1))

(defun excal-text-edit-next-line ()
  "Move to the wrapped line below."
  (interactive "^")
  (excal--text-edit-vertical 1))

(defmacro excal--text-edit-shifted (command)
  "Return a command running COMMAND as if shift-translated, to extend the region."
  `(lambda ()
     ,(format "Run `%s', extending the region." command)
     (interactive)
     (let ((this-command-keys-shift-translated t))
       (call-interactively #',command))))

(defun excal--text-edit-in-element-p (point)
  "Return non-nil if scene POINT is on the text being edited."
  (let* ((element (plist-get excal--text-edit :element))
         (local (excal--text-edit-local element point))
         (pad (/ 10.0 excal--zoom))
         (x (excal--get element 'x)) (y (excal--get element 'y)))
    (and (<= (- x pad) (car local) (+ x (excal--get element 'width) pad))
         (<= (- y pad) (cdr local) (+ y (excal--get element 'height) pad)))))

(defun excal--text-edit-click (event)
  "Put the caret at mouse EVENT on the edited text; dragging selects.
A double click selects the word."
  (let* ((element (plist-get excal--text-edit :element))
         (pos (lambda (ev)
                (excal--text-edit-pos-of
                 (excal--text-edit-index-at element (excal--event-scene-xy ev)))))
         (start (funcall pos event)))
    (deactivate-mark)
    (goto-char start)
    (if (memq 'double (event-modifiers event))
        (let ((beg (plist-get excal--text-edit :beg)) (end (plist-get excal--text-edit :end)))
          (skip-syntax-backward "w_" beg)
          (push-mark (point) t t)
          (skip-syntax-forward "w_" end))
      (excal--render)
      (excal--drag-loop
       (lambda (ev)
         (let ((here (funcall pos ev)))
           (unless (= here start)
             (unless (region-active-p) (push-mark start t t))
             (goto-char here)
             (excal--render)
             nil)))))))

(defconst excal--text-edit-passthrough-events
  '(mouse-movement wheel-up wheel-down wheel-left wheel-right pinch
                   help-echo select-window switch-frame focus-in focus-out)
  "Events that never end the edit.")

(defun excal-text-edit-other ()
  "Handle a key or event the editor does not bind itself.
A click on the text moves the caret.  A click elsewhere, or a key the
excal keymap binds, submits the edit and is then handled as usual.
Anything else runs its ordinary binding without ending the edit."
  (interactive)
  (let* ((keys (this-command-keys-vector))
         (event (aref keys (1- (length keys))))
         (basic (event-basic-type event))
         (mods (event-modifiers event))
         (click (and (mouse-event-p event) (memq 'down mods)))
         (excal-binding (let ((excal-text-edit-mode nil)) (lookup-key excal-mode-map keys)))
         (excal-binding (and (not (numberp excal-binding)) excal-binding)))
    (cond
     ;; Releases belong to a press already handled.
     ((and (mouse-event-p event) (not click)))
     ((and click (eq basic 'mouse-1)
           (excal--text-edit-in-element-p (excal--event-scene-xy event)))
      (excal--text-edit-click event))
     ((or click (and excal-binding (not (memq basic excal--text-edit-passthrough-events))))
      (excal-text-edit-submit)
      (setq unread-command-events (append (listify-key-sequence keys) unread-command-events)))
     (t
      (let ((binding (let ((excal-text-edit-mode nil)) (key-binding keys t))))
        (cond
         ((keymapp binding) (set-transient-map binding))
         ((commandp binding)
          (setq this-command binding)
          (call-interactively binding nil keys))))))))

(defvar excal-text-edit-mode-map
  (let ((map (make-keymap)))
    (set-char-table-range (nth 1 map) (cons #x20 (max-char)) #'self-insert-command)
    (define-key map [t] #'excal-text-edit-other)
    (dolist (binding
             `(("RET" . excal-text-edit-newline) ("<return>" . excal-text-edit-newline)
               ("S-<return>" . excal-text-edit-newline) ("C-j" . excal-text-edit-newline)
               ("<escape>" . excal-text-edit-submit) ("C-<return>" . excal-text-edit-submit)
               ("s-<return>" . excal-text-edit-submit) ("C-g" . excal-text-edit-cancel)
               ("TAB" . excal-text-edit-indent) ("<backtab>" . excal-text-edit-outdent)
               ("DEL" . delete-backward-char) ("<backspace>" . delete-backward-char)
               ("<delete>" . delete-forward-char) ("C-d" . delete-char)
               ("<left>" . backward-char) ("<right>" . forward-char)
               ("<up>" . excal-text-edit-previous-line) ("<down>" . excal-text-edit-next-line)
               ("S-<left>" . ,(excal--text-edit-shifted backward-char))
               ("S-<right>" . ,(excal--text-edit-shifted forward-char))
               ("S-<up>" . ,(excal--text-edit-shifted excal-text-edit-previous-line))
               ("S-<down>" . ,(excal--text-edit-shifted excal-text-edit-next-line))
               ("C-p" . excal-text-edit-previous-line) ("C-n" . excal-text-edit-next-line)
               ("C-/" . undo) ("C-_" . undo) ("s-z" . undo)
               ("C-?" . undo-redo) ("C-M-_" . undo-redo) ("s-Z" . undo-redo)
               ("C-w" . kill-region) ("s-x" . kill-region)
               ("M-w" . kill-ring-save) ("s-c" . kill-ring-save)
               ("C-y" . yank) ("s-v" . yank)
               ("s-a" . excal-text-edit-select-all)
               ("s-<" . excal-decrease-font-size) ("s->" . excal-increase-font-size)
               ("<mouse-movement>" . excal-mouse-move)))
      (define-key map (kbd (car binding)) (cdr binding)))
    ;; Meta keys are looked up in the ESC map, which needs its own default.
    (define-key (lookup-key map [27]) [t] #'excal-text-edit-other)
    map)
  "Keymap of `excal-text-edit-mode'.")

(define-minor-mode excal-text-edit-mode
  "Minor mode on while text is edited on the canvas.
\\{excal-text-edit-mode-map}"
  :lighter " Text"
  :keymap excal-text-edit-mode-map)

(provide 'excal-text-edit)
;;; excal-text-edit.el ends here
