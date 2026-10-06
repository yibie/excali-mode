;;; excali-board-ui.el --- Rich card views and editing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Board-only rendering, scrolling, and indirect Org editing.
;;; Code:
(require 'excali)
(require 'org)
(require 'excali-board-note)
(require 'excali-flowchart)
(require 'excali-board-reading)
(require 'excali-board-media)

(declare-function excali-board-vault-read-file "excali-board-vault" (prompt &optional kind))

(declare-function excali-native-board-measure "excali-module" (blocks width))
(declare-function excali-board-save "excali-board" ())
(declare-function excali-board--reference "excali-board" (card))
(declare-function excali-board--selected-card "excali-board" ())
(declare-function excali-board--resolve "excali-board" (reference))
(declare-function excali-board--require-board "excali-board" ())
(declare-function excali-board--read-heading "excali-board" (file))
(declare-function excali-board--capture-heading "excali-board" (marker))
(declare-function excali-board--refresh-card "excali-board" (card))
(declare-function excali-board--watch-sources "excali-board" ())
(declare-function excali-board--sync-source "excali-board" (source))
(declare-function excali-board--schedule "excali-board" (&rest ignored))
(declare-function excali-board-note-at-arrow-end "excali-board-organize" (&optional end text))
(declare-function excali-board-create-region "excali-board-organize" (name))
(declare-function excali-board-goto-organization "excali-board-organize" ())
(defvar excali-board-mode-map)
(defvar-local excali-board--native-cache nil)
(defvar-local excali-board--editor nil)
(defvar-local excali-board--editing-board nil)
(defvar-local excali-board--editing-frame nil)
(defvar-local excali-board--editor-window nil)
(defvar-local excali-board--editor-card-id nil)
(defvar-local excali-board--expanded-window nil)

(defun excali-board--blocks (card)
  "Return CARD's rich blocks with local images prepared for native rendering."
  (or (excali-board-media-blocks card) (excali-board-reading-blocks card)))

(defun excali-board--layout (card text)
  "Lay out a clipped fallback TEXT in CARD, never growing the card."
  (when (or (excali-board--reference card) (excali-board-note-p card)
            (excali-board-media-data card))
    (let* ((width (max 1 (- (excali--get card 'width) 40)))
           (height (max 0 (- (excali--get card 'height) 64)))
           (lines (excali--wrap-text (or (excali--get text 'originalText) "")
                                     18 2 width))
           (lines (string-join (seq-take (split-string lines "\n")
                                         (floor (/ height 23.0))) "\n"))
           (size (excali--measure-string lines 18 2 1.25)))
      (dolist (pair `((text . ,lines) (fontSize . 18) (fontFamily . 2)
                      (width . ,(min width (car size))) (height . ,(min height (cdr size)))
                      (x . ,(+ 20 (excali--get card 'x)))
                      (y . ,(+ 44 (excali--get card 'y)))
                      (angle . ,(or (excali--get card 'angle) 0))))
        (excali--put text (car pair) (cdr pair)))
      (excali--touch text))
    t))

(defun excali-board--scroll-values (card)
  "Return clamped (X . Y) scroll offsets for CARD's present dimensions."
  (let* ((view (alist-get 'excaliBoardView (excali--get card 'customData)))
         (measure (excali-native-board-measure (excali-board--blocks card)
                                               (excali--get card 'width))))
    (cons (max 0 (min (let ((x (alist-get 'x view))) (if (numberp x) x 0))
                      (max 0 (- (aref measure 0) (max 1 (- (excali--get card 'width) 40))))))
          (max 0 (min (let ((y (alist-get 'y view))) (if (numberp y) y 0))
                      (max 0 (- (aref measure 1) (max 1 (- (excali--get card 'height) 64)))))))))

(defun excali-board--native (element native)
  "Extend NATIVE for ELEMENT with rich content; leave ordinary shapes alone."
  (let* ((card (if (equal (excali--get element 'type) "text")
                   (excali--container-of element) element))
         (blocks (and card (excali-board--blocks card))))
    (if (not (and (vectorp blocks) (> (length blocks) 0))) native
      (let* ((key (list native (excali--get card 'version) (aref native 15) blocks))
             (cached (gethash element excali-board--native-cache)))
        (if (equal key (car cached)) (cdr cached)
          (let ((copy (copy-sequence native)))
            (if (not (eq element card)) (aset copy 15 0)
              (let ((scroll (excali-board--scroll-values card)))
                (aset copy 24
                      (vconcat (aref native 24)
                               (vector "boardBlocks" blocks
                                       "boardTitle" (let ((file (alist-get 'file (excali-board--reference card))))
                                                      (cond ((excali-board-media-data card)
                                                             (upcase (alist-get 'type (excali-board-media-data card))))
                                                            ((stringp file) (file-name-nondirectory file))
                                                            (t "Board note")))
                                       "boardScrollX" (car scroll) "boardScrollY" (cdr scroll))))))
            (puthash element (cons key copy) excali-board--native-cache)
            copy))))))

(defun excali-board--scroll (card dx dy &optional transient)
  "Scroll CARD by DX and DY without resizing it.
TRANSIENT defers the history commit until a scrollbar drag finishes."
  (let* ((old (excali-board--scroll-values card))
         (data (copy-tree (excali--get card 'customData) t)))
    (setf (alist-get 'excaliBoardView data)
          `((x . ,(+ (car old) dx)) (y . ,(+ (cdr old) dy))))
    (excali--put card 'customData data)
    (let ((new (excali-board--scroll-values card)))
      (setf (alist-get 'excaliBoardView data) `((x . ,(car new)) (y . ,(cdr new)))))
    (excali--touch card)
    (unless transient (excali--commit))
    (excali--render)
    (excali--sync-views)))

(defun excali-board-scroll-up ()
  "Scroll the selected Org card down one page."
  (interactive)
  (let ((card (excali-board--selected-card)))
    (excali-board--scroll card 0 (max 40 (- (excali--get card 'height) 84)))))

(defun excali-board-scroll-down ()
  "Scroll the selected Org card up one page."
  (interactive)
  (let ((card (excali-board--selected-card)))
    (excali-board--scroll card 0 (- (max 40 (- (excali--get card 'height) 84))))))

(defun excali-board--card-at (point)
  "Return the topmost rich card at scene POINT, without selecting it."
  (let* ((hit (excali--hit point))
         (card (if (equal (excali--get hit 'type) "text")
                   (excali--container-of hit) hit)))
    (and card (excali-board--blocks card) card)))

(defun excali-board--local-point (card point)
  "Convert scene POINT to CARD's unrotated local coordinates."
  (let ((p (excali--rotate-point
            point (excali--box-center (excali--element-box card))
            (- (excali--element-angle card)))))
    (cons (- (car p) (excali--get card 'x))
          (- (cdr p) (excali--get card 'y)))))

(defun excali-board-wheel (event)
  "Scroll the card body under EVENT without requiring selection.
The header and empty canvas pan normally; control-wheel always zooms."
  (interactive "e")
  (excali--select-event-view event)
  (let* ((point (excali--event-scene-xy event))
         (card (excali-board--card-at point))
         (local (and card (excali-board--local-point card point))))
    (if (and local
             (<= 2 (car local) (- (excali--get card 'width) 2))
             (<= 42 (cdr local) (- (excali--get card 'height) 2))
             (not (memq 'control (event-modifiers event))))
        (let ((delta (excali--wheel-delta event)))
          (excali-board--scroll card (- (car delta)) (- (cdr delta))))
      (excali-wheel event))))

(defun excali-board--scrollbar-at (card point)
  "Return scrollbar geometry at local POINT on CARD, or nil.
The result is (AXIS START TRAVEL MAX-OFFSET THUMB-START THUMB-LENGTH).
Geometry matches the native renderer, with a wider mouse hit target."
  (let* ((w (excali--get card 'width)) (h (excali--get card 'height))
         (vw (- w 40)) (vh (- h 64))
         (size (excali-native-board-measure (excali-board--blocks card) w))
         (scroll (excali-board--scroll-values card))
         (x (car point)) (y (cdr point)))
    (when (and (> vw 0) (> vh 0))
      (cond
       ((and (> (aref size 1) vh) (<= (- w 13) x (- w 2))
             (<= 44 y (+ 44 vh)))
        (let* ((thumb (min vh (max 18 (/ (* vh vh) (aref size 1)))))
               (travel (- vh thumb)) (limit (- (aref size 1) vh)))
          (list 'y 44 travel limit (+ 44 (* travel (/ (cdr scroll) limit))) thumb)))
       ((and (> (aref size 0) vw) (<= (- h 13) y (- h 2))
             (<= 20 x (+ 20 vw)))
        (let* ((thumb (min vw (max 18 (/ (* vw vw) (aref size 0)))))
               (travel (- vw thumb)) (limit (- (aref size 0) vw)))
          (list 'x 20 travel limit (+ 20 (* travel (/ (car scroll) limit))) thumb)))))))

(defun excali-board-mouse-down (event)
  "Operate a card scrollbar at EVENT, or delegate normal drawing gestures."
  (interactive "e")
  (excali--select-event-view event)
  (let* ((point (excali--event-scene-xy event))
         (card (and (eq excali--tool 'select) (excali-board--card-at point)))
         (local (and card (excali-board--local-point card point)))
         (bar (and local (excali-board--scrollbar-at card local)))
         (link (and card (not bar)
                    (not (memq 'shift (event-modifiers event)))
                    (excali-board-reading-link-at card point))))
    (cond
     (link
      (let ((release (excali--await-release)))
        (when (and release
                   (not (memq 'drag (event-modifiers release)))
                   (equal link (excali-board-reading-link-at
                                card (excali--event-scene-xy release))))
          (excali-board-reading-open card link))))
     ((not bar) (excali-mouse-down event))
     (t
      (pcase-let* ((`(,axis ,start ,travel ,limit ,thumb-start ,thumb) bar)
                   (coordinate (if (eq axis 'x) (car local) (cdr local)))
                   (grab (if (<= thumb-start coordinate (+ thumb-start thumb))
                             (- coordinate thumb-start) (/ thumb 2.0))))
        (cl-labels
         ((move (ev)
            (let* ((p (excali-board--local-point card (excali--event-scene-xy ev)))
                   (coordinate (if (eq axis 'x) (car p) (cdr p)))
                   (offset (if (> travel 0)
                               (* limit (/ (- coordinate start grab) (float travel))) 0))
                   (old (excali-board--scroll-values card)))
              (excali-board--scroll
               card (if (eq axis 'x) (- offset (car old)) 0)
               (if (eq axis 'y) (- offset (cdr old)) 0) t))
            nil))
         ;; Clicking the track centers the thumb; dragging preserves its grip.
         (unwind-protect
             (progn (move event) (excali--drag-loop #'move))
           (excali--commit)
           (excali--sync-views))))))))

(defun excali-board--editor-change (&rest _)
  "Schedule refresh from the base buffer of the indirect Org editor."
  (when-let* ((base (buffer-base-buffer)))
    (with-current-buffer base (excali-board--schedule))))

(defun excali-board-edit-finish ()
  "Close the popup or expanded editor without implicitly saving."
  (interactive)
  (excali-board-note--sync)
  (let ((board excali-board--editing-board)
        (frame excali-board--editing-frame)
        (origin excali-board--editor-window)
        (expanded excali-board--expanded-window)
        (base (buffer-base-buffer))
        (editor (current-buffer)))
    (when (and (window-live-p expanded) (eq (window-buffer expanded) editor))
      (quit-window nil expanded))
    (when (frame-live-p frame) (delete-frame frame t))
    (when (buffer-live-p editor) (kill-buffer editor))
    (when (buffer-live-p base) (excali-board--sync-source base))
    (when (buffer-live-p board)
      (with-current-buffer board (setq excali-board--editor nil))
      (when-let* ((window (if (and (window-live-p origin)
                                  (eq (window-buffer origin) board))
                             origin (get-buffer-window board t))))
        (select-frame-set-input-focus (window-frame window))
        (select-window window)))))

(defun excali-board-edit-save ()
  "Save the board for an independent note, or the Org source for a reference."
  (interactive)
  (if excali-board-note--id
      (progn (excali-board-note--sync)
             (with-current-buffer excali-board--editing-board (excali-board-save)))
    (save-buffer)))

(defvar excali-board-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'excali-board-edit-finish)
    (define-key map (kbd "<escape>") #'excali-board-edit-finish)
    (define-key map (kbd "C-x C-s") #'excali-board-edit-save)
    (define-key map (kbd "C-c C-e") #'excali-board-edit-expand)
    map))

(define-minor-mode excali-board-edit-mode
  "Edit a card in a fixed popup or ordinary Emacs window.
C-c C-c closes without saving.  C-c C-e expands to an ordinary window.
C-x C-s saves the Org source, or the board for an independent note.
There is no discard command: text is shared with the source buffer."
  :lighter " Card" :keymap excali-board-edit-mode-map)

(defun excali-board--close-editor ()
  "Close this board's editor when its board buffer goes away."
  (when (buffer-live-p excali-board--editor)
    (with-current-buffer excali-board--editor (excali-board-edit-finish))))

(defun excali-board--editor-killed ()
  "Remove an editor's child frame and release its board association."
  (when (frame-live-p excali-board--editing-frame)
    (delete-frame excali-board--editing-frame t))
  (when (buffer-live-p excali-board--editing-board)
    (with-current-buffer excali-board--editing-board
      (setq excali-board--editor nil))))

(defun excali-board--popup-rect (parent)
  "Return a centered popup rectangle in PARENT, independent of canvas geometry.
Limit its outer size to 720 by 480 pixels, leaving a margin on small frames."
  (let* ((pw (frame-native-width parent))
         (ph (frame-native-height parent))
         (width (max 1 (min 720 (- pw 48))))
         (height (max 1 (min 480 (- ph 80)))))
    (list (max 0 (/ (- pw width) 2))
          (max 0 (/ (- ph height) 2)) width height)))

(defun excali-board--position-editor (frame rect)
  "Place child FRAME at RECT without changing focus or editor contents."
  (pcase-let ((`(,x ,y ,width ,height) rect)
              (frame-resize-pixelwise t))
    (set-frame-size
     frame
     (max 1 (- width (- (frame-pixel-width frame) (frame-text-width frame))))
     (max 1 (- height (- (frame-pixel-height frame) (frame-text-height frame))))
     t)
    (set-frame-position frame x y)))

(defun excali-board-edit-expand ()
  "Move the current card editor into an ordinary window without recreating it."
  (interactive)
  (unless (and excali-board-edit-mode (buffer-live-p excali-board--editing-board))
    (user-error "Not in a board card editor"))
  (let* ((editor (current-buffer))
         (frame excali-board--editing-frame)
         (origin excali-board--editor-window)
         (parent (or (and (frame-live-p frame) (frame-parent frame))
                     (and (window-live-p origin) (window-frame origin))
                     (selected-frame)))
         (window (with-selected-frame parent
                   (display-buffer
                    editor '((display-buffer-reuse-window display-buffer-pop-up-window)
                             (inhibit-same-window . t))))))
    (unless (window-live-p window)
      (user-error "Cannot open an editor window"))
    (setq excali-board--expanded-window window
          excali-board--editing-frame nil)
    (when (frame-live-p frame) (delete-frame frame t))
    (select-frame-set-input-focus (window-frame window))
    (select-window window)))

(defun excali-board--make-editor (card)
  "Return an indirect Org editor for CARD's complete subtree."
  (if (excali-board-note-p card)
      (let ((editor (excali-board-note--editor card)))
        (with-current-buffer editor
          (add-hook 'kill-buffer-hook #'excali-board--editor-killed nil t)
          (excali-board-edit-mode 1))
        (setq excali-board--editor editor)
        editor)
    (let* ((marker (excali-board--resolve (excali-board--reference card)))
           (board (current-buffer))
           (editor
            (with-current-buffer (marker-buffer marker)
              (clone-indirect-buffer (generate-new-buffer-name " *Org card*") nil))))
      (with-current-buffer editor
        (widen)
        (goto-char marker)
        (org-narrow-to-subtree)
        (org-fold-show-all)
        (setq-local excali-board--editing-board board)
        (setq-local header-line-format "Org source · C-c C-c: close · C-x C-s: save Org")
        (remove-hook 'after-change-functions #'excali-board--schedule t)
        (add-hook 'after-change-functions #'excali-board--editor-change nil t)
        (add-hook 'kill-buffer-hook #'excali-board--editor-killed nil t)
        (excali-board-edit-mode 1))
      (set-marker marker nil)
      (setq excali-board--editor editor)
      editor)))

(defun excali-board-edit ()
  "Edit the selected card in a fixed popup, independent of pan and zoom."
  (interactive)
  (excali-board--require-board)
  (let* ((card (excali-board--selected-card))
         (id (excali--get card 'id))
         (window (if (eq (window-buffer (selected-window)) (current-buffer))
                     (selected-window) (get-buffer-window (current-buffer))))
         (parent (and window (window-frame window)))
         (existing excali-board--editor))
    (unless (and window (display-graphic-p parent))
      (user-error "Popup editing requires a graphical board window"))
    (if (and (buffer-live-p existing)
             (equal id (buffer-local-value 'excali-board--editor-card-id existing)))
        (with-current-buffer existing
          (if (frame-live-p excali-board--editing-frame)
              (progn
                (make-frame-visible excali-board--editing-frame)
                (select-frame-set-input-focus excali-board--editing-frame))
            (excali-board-edit-expand)))
      (excali-board--close-editor)
      (let* ((rect (excali-board--popup-rect parent))
             (editor (excali-board--make-editor card))
             frame)
        (condition-case err
            (progn
              (setq frame (make-frame
                           `((parent-frame . ,parent) (minibuffer . nil)
                             (undecorated . t) (visibility . nil)
                             (width . (text-pixels . ,(nth 2 rect)))
                             (height . (text-pixels . ,(nth 3 rect)))
                             (internal-border-width . 8) (child-frame-border-width . 1)
                             (menu-bar-lines . 0) (tool-bar-lines . 0)
                             (tab-bar-lines . 0) (vertical-scroll-bars . right)
                             (no-accept-focus . nil) (no-other-frame . t))))
              (excali-board--position-editor frame rect)
              (set-window-buffer (frame-root-window frame) editor)
              (with-current-buffer editor
                (setq excali-board--editing-frame frame
                      excali-board--editor-window window
                      excali-board--editor-card-id id)
                (setq-local
                 header-line-format
                 (list
                  (replace-regexp-in-string
                   "%" "%%"
                   (save-excursion
                     (goto-char (point-min))
                     (string-trim (buffer-substring-no-properties
                                   (line-beginning-position) (line-end-position))
                                  "[* 	]+" "[ 	]+")))
                  "  |  C-c C-c: done · C-c C-e: expand · C-x C-s: save")))
              (make-frame-visible frame)
              (select-frame-set-input-focus frame))
          (error
           (when (frame-live-p frame) (delete-frame frame t))
           (kill-buffer editor)
           (setq excali-board--editor nil)
           (signal (car err) (cdr err))))))))

(defun excali-board-replace-source (file)
  "Replace the selected card's heading from FILE, retaining layout and edges."
  (interactive (list (read-file-name "New Org source: " nil nil t)))
  (when (excali-board-note-p (excali-board--selected-card))
    (user-error "Convert this note with excali-board-note-to-heading first"))
  (let* ((card (excali-board--selected-card))
         (marker (excali-board--read-heading (expand-file-name file))))
    (unwind-protect
        (let ((reference (car (excali-board--capture-heading marker)))
              (data (copy-tree (excali--get card 'customData) t)))
          (setf (alist-get 'excaliOrg data) reference)
          (setq data (assq-delete-all 'excaliBoardContent data))
          (setq data (assq-delete-all 'excaliBoardView data))
          (excali--put card 'customData data)
          (excali--touch card)
          (excali-board--refresh-card card)
          (excali-board--watch-sources)
          (excali--commit)
          (excali--render))
      (set-marker marker nil))))

(defun excali-board-insert-attachment (file)
  "Insert local FILE as a static media card or a generic linked attachment."
  (interactive (list (excali-board-vault-read-file "Attachment: ")))
  (excali-board--require-board)
  (setq file (expand-file-name file))
  (when (or (file-remote-p file) (not (file-regular-p file)))
    (user-error "Choose an existing local file"))
  (if (excali-board-media-type file)
      (excali-board-insert-media file)
    (let ((card (excali--make-element
                 "rectangle" 60.0 60.0 '(width . 300.0) '(height . 120.0)
                 '(backgroundColor . "#ffffff") '(fillStyle . "solid")
                 '(roughness . 0) '(strokeWidth . 1)
                 (cons 'link (concat "file://" file))
                 (cons 'customData `((excaliBoardAttachment . ((file . ,file))))))))
      (setq excali--elements (append excali--elements (list card)))
      (excali--set-text (excali--add-bound-text card)
			(concat "Attachment\n" (file-name-nondirectory file)))
      (excali--select (list card))
      (excali--commit)
      (excali--render))))

(defun excali-board-open-attachment ()
  "Open the selected local attachment in Emacs, never via a shell."
  (interactive)
  (let* ((element (car excali--selection))
         (card (if (equal (excali--get element 'type) "text")
                   (excali--container-of element) element))
         (file (or (alist-get 'file (excali-board-media-data card))
                   (alist-get 'file (alist-get 'excaliBoardAttachment
                                              (excali--get card 'customData))))))
    (unless (and (= (length (seq-remove #'excali--bound-text-p excali--selection)) 1)
                 (stringp file) (file-name-absolute-p file)
                 (not (file-remote-p file)) (file-regular-p file))
      (user-error "Select one available local attachment"))
    (let ((enable-local-eval nil) (enable-local-variables :safe))
      (find-file-other-window file))))

(defun excali-board-connect (label)
  "Connect two selected shapes with a bound arrow and optional LABEL.
Direction follows selection order.  Org source links are not modified."
  (interactive "sRelationship label (optional): ")
  (excali-board--require-board)
  (let ((nodes (seq-remove #'excali--bound-text-p excali--selection)))
    (unless (and (= (length nodes) 2) (seq-every-p #'excali--flowchart-node-p nodes))
      (user-error "Select two cards/shapes"))
    (let* ((from (car nodes)) (to (cadr nodes))
           (dx (- (excali--get to 'x) (excali--get from 'x)))
           (dy (- (excali--get to 'y) (excali--get from 'y)))
           (direction (if (> (abs dx) (abs dy))
                          (if (> dx 0) 'right 'left)
                        (if (> dy 0) 'down 'up)))
           (arrow (excali--flowchart-arrow from to direction)))
      (excali--put arrow 'endArrowhead "arrow")
      (setq excali--elements (append excali--elements (list arrow)))
      (unless (string-empty-p label)
        (excali--set-text (excali--add-bound-text arrow) label))
      (excali--select (list arrow))
      (excali--commit)
      (excali--render)
      arrow)))

(defun excali-board--setup-keys ()
  "Install board-only key bindings."
  ;; Global precision scrolling is a minor mode, so its direct wheel
  ;; bindings outrank this major mode.  Remap its commands locally rather
  ;; than disabling the user's scrolling mode in unrelated buffers.
  (define-key excali-board-mode-map [remap pixel-scroll-precision] #'excali-board-wheel)
  (define-key excali-board-mode-map [remap mwheel-scroll] #'excali-board-wheel)
  ;; Do not let precision mode synthesize buffer scrolling after touch-end.
  (define-key excali-board-mode-map [remap pixel-scroll-start-momentum] #'ignore)
  (define-key excali-board-mode-map [down-mouse-1] #'excali-board-mouse-down)
  ;; NS promotes a wheel burst to double-/triple-wheel events.  Explicit
  ;; bindings are essential: the canvas prefix's [t] -> ignore catches them
  ;; before Emacs can fall back to a single-wheel binding.
  (dolist (direction '("up" "down" "left" "right"))
    (dolist (count '("" "double-" "triple-"))
      (dolist (modifiers '("" "S-" "C-" "C-S-"))
        (define-key excali-board-mode-map
                    (kbd (format "%s<%swheel-%s>" modifiers count direction))
                    #'excali-board-wheel))))
  (define-key excali-board-mode-map (kbd "C-c C-j") #'excali-board-note-at-arrow-end)
  (define-key excali-board-mode-map (kbd "C-c C-t") #'excali-board-create-region)
  (define-key excali-board-mode-map (kbd "C-c C-v") #'excali-board-goto-organization)
  (define-key excali-board-mode-map (kbd "C-c C-n") #'excali-board-new-note)
  (define-key excali-board-mode-map (kbd "C-c C-w") #'excali-board-note-to-heading)
  (define-key excali-board-mode-map (kbd "C-c C-e") #'excali-board-edit)
  (define-key excali-board-mode-map (kbd "M-n") #'excali-board-scroll-up)
  (define-key excali-board-mode-map (kbd "M-p") #'excali-board-scroll-down)
  (define-key excali-board-mode-map (kbd "C-c C-a") #'excali-board-insert-attachment)
  (define-key excali-board-mode-map (kbd "C-c C-f") #'excali-board-open-attachment)
  (define-key excali-board-mode-map (kbd "C-c C-l") #'excali-board-connect))

;; Reloading this file should update existing boards without reinitializing
;; their buffer-local scene state.  During initial loading/byte compilation
;; the board's map may not exist yet; mode initialization installs it then.
(when (boundp 'excali-board-mode-map)
  (excali-board--setup-keys))

(provide 'excali-board-ui)
;;; excali-board-ui.el ends here
