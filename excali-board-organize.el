;;; excali-board-organize.el --- Connected cards and named organization -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Reuse standard arrow bindings, groupIds and frames.  Only group display
;; names need customData, so other Excalidraw editors retain normal structure.
;;; Code:
(require 'excali-flowchart)
(require 'excali-board-note)
(declare-function excali-board--require-board "excali-board" ())
(declare-function excali-board--insert "excali-board" (reference content &optional note))

(defmacro excali-board-organize--transaction (&rest body)
  "Run BODY as one scene edit, rolling back on failure."
  (declare (indent 0) (debug t))
  `(let ((snapshot (excali--snapshot))
         (editing-id (and excali--editing-linear (excali--get excali--editing-linear 'id)))
         result)
     (condition-case err
         (let ((excali--history-hold t)) (setq result (progn ,@body)))
       ((error quit)
        (excali--restore snapshot)
        (setq excali--editing-linear (and editing-id (excali--live-element-by-id editing-id)))
        (signal (car err) (cdr err))))
     (excali--commit)
     (excali--render)
     (excali--sync-views)
     result))

(defun excali-board-organize--name (name)
  "Validate nonempty single-line NAME and return it trimmed."
  (unless (and (stringp name) (not (string-empty-p (string-trim name)))
               (not (string-match-p "[\n\r]" name)))
    (user-error "Use a nonempty, single-line name"))
  (string-trim name))

(defun excali-board-organize--selection ()
  "Return unlocked selected shapes, not bound labels or deleted elements."
  (let ((nodes (seq-remove #'excali--bound-text-p excali--selection)))
    (unless nodes (user-error "Select cards or shapes first"))
    (when (seq-some (lambda (e) (or (excali--get e 'locked)
                                   (excali--get e 'isDeleted))) nodes)
      (user-error "Unlock the selected elements first"))
    nodes))

(defun excali-board-organize--arrow ()
  "Return the single selected unlocked arrow."
  (let ((nodes (excali-board-organize--selection)))
    (unless (and (= 1 (length nodes)) (equal "arrow" (excali--get (car nodes) 'type)))
      (user-error "Select one arrow with a free endpoint"))
    (car nodes)))

(defun excali-board-organize-free-end-at (arrow point)
  "Return the free endpoint of ARROW near scene POINT, or nil."
  (when (and arrow (equal "arrow" (excali--get arrow 'type))
             (not (excali--get arrow 'locked)))
    (let ((points (excali--linear-global-points arrow))
          (radius (/ 10.0 (max 0.01 excali--zoom))))
      (seq-find
       (lambda (end)
         (let ((p (if (eq end 'start) (car points) (car (last points)))))
           (and p (not (excali--binding-element-id arrow end))
                (<= (sqrt (+ (expt (- (car point) (car p)) 2)
                             (expt (- (cdr point) (cdr p)) 2))) radius))))
       '(end start)))))

;;;###autoload
(defun excali-board-note-at-arrow-end (&optional end text)
  "Create an independent Org card at the selected arrow's free END.
With one free endpoint choose it automatically; with two ask start or end.
Keep the existing arrow, label and opposite binding.  TEXT defaults to a
new note.  The entire operation is one undo step."
  (interactive)
  (require 'excali-board)
  (excali-board--require-board)
  (let* ((arrow (excali-board-organize--arrow))
         (free (seq-remove (lambda (e) (excali--binding-element-id arrow e)) '(start end))))
    (unless free (user-error "Both endpoints are bound; detach one first"))
    (setq end (or end (if (= 1 (length free)) (car free)
                       (intern (completing-read "Create at endpoint: " '("end" "start") nil t)))))
    (unless (memq end free) (user-error "That endpoint is already bound"))
    (let* ((points (excali--linear-global-points arrow))
           (ordered (if (eq end 'start) points (reverse points)))
           (tip (car ordered))
           (previous (seq-find (lambda (p) (not (equal tip p))) (cdr ordered))))
      (unless previous (user-error "Arrow needs two distinct points"))
      (excali-board-organize--transaction
        (let* ((card (excali-board--insert nil "" t))
               (dx (- (car tip) (car previous)))
               (dy (- (cdr tip) (cdr previous)))
               (horizontal (>= (abs dx) (abs dy)))
               (w (excali--get card 'width)) (h (excali--get card 'height))
               (x (if horizontal (if (>= dx 0) (+ (car tip) 6) (- (car tip) w 6))
                    (- (car tip) (/ w 2))))
               (y (if horizontal (- (cdr tip) (/ h 2))
                    (if (>= dy 0) (+ (cdr tip) 6) (- (cdr tip) h 6)))))
          (excali--put card 'x x)
          (excali--put card 'y y)
          (excali-board-note--set card (or text "* New note\nWrite your ideas here.\n"))
          (excali--touch card)
          (if (excali--elbow-p arrow) (excali--elbow-bind-end arrow end card)
            (excali--bind-end arrow end card tip))
          (excali--update-bound-arrows (list card))
          ;; Join an existing region only when the whole new card fits.
          (when-let* ((frame (excali--live-element-by-id (excali--get arrow 'frameId))))
            (when (excali--inside-p (excali--element-box card) (excali--element-box frame))
              (excali--set-frame (list card) frame)
              (excali--place-below (cons card (excali--labels-of (list card))) frame)))
          (excali--deselect)
          (excali--select (list card))
          card)))))

(defun excali-board-organize--set-group-name (members id name)
  "Store NAME for standard group ID on its MEMBERS."
  (dolist (member members)
    (let* ((data (copy-tree (excali--get member 'customData) t))
           (names (append (alist-get 'excaliBoardGroups data) nil)))
      (setq names (seq-remove (lambda (g) (equal id (alist-get 'id g))) names))
      (setf (alist-get 'excaliBoardGroups data)
            (vconcat names (vector `((id . ,id) (name . ,name)))))
      (excali--put member 'customData data)
      (excali--touch member))))

;;;###autoload
(defun excali-board-name-group (name)
  "Group the selection under NAME using standard Excalidraw groupIds.
For a visible titled boundary, use `excali-board-create-region' instead."
  (interactive "sGroup name: ")
  (require 'excali-board)
  (excali-board--require-board)
  (setq name (excali-board-organize--name name))
  (let* ((nodes (excali-board-organize--selection))
         (members (delete-dups (append nodes (excali--labels-of nodes))))
         (id (excali--new-id)))
    (unless (> (length nodes) 1) (user-error "Select at least two shapes"))
    (when (seq-some #'excali--frame-p nodes) (user-error "Regions cannot be grouped"))
    (excali-board-organize--transaction
      (dolist (e members)
        (excali--put e 'groupIds (vconcat (excali--get e 'groupIds) (vector id))))
      (excali-board-organize--set-group-name members id name)
      (excali--deselect)
      (excali--select members)
      id)))

;;;###autoload
(defun excali-board-rename-group (name)
  "Rename the selected outer group without changing its membership."
  (interactive "sNew group name: ")
  (require 'excali-board)
  (excali-board--require-board)
  (setq name (excali-board-organize--name name))
  (let* ((nodes (excali-board-organize--selection))
         (ids (delete-dups (mapcar #'excali--unit-group nodes)))
         (id (car ids)))
    (unless (and id (= 1 (length ids))) (user-error "Select one group"))
    (excali-board-organize--transaction
      (excali-board-organize--set-group-name (excali--group-members id) id name))))

;;;###autoload
(defun excali-board-create-region (name)
  "Enclose the selected shapes in a named standard frame.
Include their labels and arrows connecting selected shapes.  Never adopt
unselected neighboring shapes or nest frames."
  (interactive "sRegion name: ")
  (require 'excali-board)
  (excali-board--require-board)
  (setq name (excali-board-organize--name name))
  (let* ((nodes (excali-board-organize--selection))
         (ids (mapcar (lambda (e) (excali--get e 'id)) nodes))
         (arrows (seq-filter
                  (lambda (e) (and (equal (excali--get e 'type) "arrow")
                                   (member (excali--binding-element-id e 'start) ids)
                                   (member (excali--binding-element-id e 'end) ids)))
                  (excali--live-elements)))
         (members (delete-dups (append nodes arrows (excali--labels-of (append nodes arrows))))))
    (when (seq-some (lambda (e) (excali--get e 'locked)) members)
      (user-error "Unlock the selection and its connecting arrows first"))
    (when (seq-some (lambda (e) (or (excali--frame-p e) (excali--get e 'frameId))) members)
      (user-error "Select shapes outside existing regions; frames cannot nest"))
    ;; A region must never split a group.
    (dolist (e members)
      (when-let* ((group (excali--unit-group e)))
        (unless (seq-every-p (lambda (m) (memq m members)) (excali--group-members group))
          (user-error "Select the whole group before creating a region"))))
    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--elements-bounds members)))
      (excali-board-organize--transaction
        (let ((frame (excali--new-frame (- x1 30) (- y1 40))))
          (excali--put frame 'width (+ (- x2 x1) 60))
          (excali--put frame 'height (+ (- y2 y1) 70))
          (excali--put frame 'name name)
          (excali--touch frame)
          (setq excali--elements (append excali--elements (list frame)))
          (excali--set-frame members frame)
          (excali--place-below members frame)
          (excali--deselect)
          (excali--select (list frame))
          frame)))))

(defun excali-board-organize--choices ()
  "Return named group/region completion choices with disambiguating IDs."
  (let (choices seen)
    (dolist (e (excali--live-elements))
      (when (excali--frame-p e)
        (push (cons (format "Region: %s [%s]" (excali-frame-name e) (excali--get e 'id))
                    (list e)) choices))
      (dolist (group (append (alist-get 'excaliBoardGroups (excali--get e 'customData)) nil))
        (let ((id (alist-get 'id group)))
          (when (and (not (member id seen)) (seq-contains-p (excali--get e 'groupIds) id))
            (push id seen)
            (push (cons (format "Group: %s [%s]" (alist-get 'name group) id)
                        (excali--group-members id)) choices)))))
    (nreverse choices)))

;;;###autoload
(defun excali-board-goto-organization ()
  "Choose a named group or region and bring it into view."
  (interactive)
  (require 'excali-board)
  (excali-board--require-board)
  (let ((choices (excali-board-organize--choices)))
    (unless choices (user-error "No named groups or regions"))
    (let ((members (cdr (assoc (completing-read "Group or region: " choices nil t) choices))))
      (excali--deselect)
      (excali--select members)
      (excali--zoom-to (excali--elements-bounds members) 'scale-down)
      (excali--render))))

(provide 'excali-board-organize)
;;; excali-board-organize.el ends here
