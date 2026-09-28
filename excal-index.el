;;; excal-index.el --- Fractional z-order indices  -*- lexical-binding: t; -*-

;;; Commentary:

;; Every Excalidraw element carries an `index', a base-62 "order key"
;; that sorts like the element's position in the scene.  The array order
;; stays authoritative for z-order; the keys are kept in sync with it so
;; that files merge cleanly with other Excalidraw clients.
;;
;; The key generator is a port of packages/fractional-indexing (itself
;; the CC0 `fractional-indexing' npm package by David Greenspan and
;; Rocicorp); the sync functions port syncInvalidIndices and
;; syncMovedIndices from packages/element/src/fractionalIndex.ts.
;;
;; Elements of types excal does not know are left out of indexing: they
;; keep whatever index they had, and upstream Excalidraw drops them on
;; load anyway.
;;
;; Where the keys are maintained:
;; - `excal--restore-elements' runs `excal--sync-indices' on load.
;; - `excal--insert-elements' (paste, duplicate) and `excal--reorder'
;;   (z-order commands) call `excal--sync-moved-indices', so only the
;;   inserted or moved elements get new keys, as upstream.
;; - Every other change (new elements drawn by the tools, anything that
;;   bypasses the above) is caught by `excal--sync-indices-maybe', which
;;   `excal--open' adds to `post-command-hook' ahead of `excal--commit'.
;;   It checks the keys in one cheap pass and repairs them only when some
;;   are missing or out of order, so history snapshots always hold valid
;;   keys.

;;; Code:

(require 'cl-lib)
(require 'excal-core)

(defconst excal--base-62-digits
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  "Digits of fractional index keys, in ascending order.")

(defconst excal--index-smallest-integer (concat "A" (make-string 26 ?0))
  "The integer part that has no predecessor; not a valid key by itself.")

(defconst excal--indexed-types
  '("rectangle" "diamond" "ellipse" "text" "arrow" "line" "freedraw" "image"
    "frame" "magicframe" "iframe" "embeddable" "stickynote" "selection")
  "Element types that take part in fractional indexing.")

(define-error 'excal-index-error "Invalid fractional index")

;;;; Key generation (packages/fractional-indexing/src/index.ts)

(defsubst excal--index-digit-value (char)
  "Return the base-62 value of CHAR, or nil."
  (cl-position char excal--base-62-digits))

(defun excal--index-midpoint (a b)
  "Return a digit string strictly between fractional parts A and B.
B may be nil for no upper bound."
  (let ((zero ?0))
    (when (and b (not (string< a b)))
      (signal 'excal-index-error (list (format "%s >= %s" a b))))
    (when (or (and (> (length a) 0) (eq (aref a (1- (length a))) zero))
              (and b (> (length b) 0) (eq (aref b (1- (length b))) zero)))
      (signal 'excal-index-error (list "trailing zero")))
    (catch 'done
      (when (and b (> (length b) 0))
        (let ((n 0))
          (while (and (< n (length b))
                      (eq (if (< n (length a)) (aref a n) zero) (aref b n)))
            (cl-incf n))
          (when (> n 0)
            (throw 'done (concat (substring b 0 n)
                                 (excal--index-midpoint (substring a (min n (length a)))
                                                        (substring b n)))))))
      (let ((digit-a (if (> (length a) 0) (excal--index-digit-value (aref a 0)) 0))
            (digit-b (if (and b (> (length b) 0))
                         (excal--index-digit-value (aref b 0))
                       ;; JS: b[0] of "" is undefined, indexOf gives -1.
                       (if b -1 (length excal--base-62-digits)))))
        (cond
         ((> (- digit-b digit-a) 1)
          ;; Math.round(0.5 * (a + b)): halves round up.
          (string (aref excal--base-62-digits
                        (floor (+ (* 0.5 (+ digit-a digit-b)) 0.5)))))
         ((and b (> (length b) 1)) (substring b 0 1))
         (t (concat (string (aref excal--base-62-digits digit-a))
                    (excal--index-midpoint
                     (if (> (length a) 0) (substring a 1) "") nil))))))))

(defun excal--index-integer-length (head)
  "Return the length of the integer part starting with HEAD."
  (cond ((<= ?a head ?z) (+ (- head ?a) 2))
        ((<= ?A head ?Z) (+ (- ?Z head) 2))
        (t (signal 'excal-index-error
                   (list (format "invalid order key head: %c" head))))))

(defun excal--index-integer-part (key)
  "Return the integer part of KEY."
  (let ((len (excal--index-integer-length (aref key 0))))
    (when (> len (length key))
      (signal 'excal-index-error (list (format "invalid order key: %s" key))))
    (substring key 0 len)))

(defun excal--validate-order-key (key)
  "Signal `excal-index-error' unless KEY is a valid order key."
  (when (or (not (stringp key)) (string-empty-p key)
            (equal key excal--index-smallest-integer)
            (not (cl-every #'excal--index-digit-value key)))
    (signal 'excal-index-error (list (format "invalid order key: %s" key))))
  (let* ((i (excal--index-integer-part key))
         (f (substring key (length i))))
    (when (and (> (length f) 0) (eq (aref f (1- (length f))) ?0))
      (signal 'excal-index-error (list (format "invalid order key: %s" key))))))

(defun excal--valid-order-key-p (key)
  "Return non-nil if KEY is a valid order key."
  (condition-case nil (progn (excal--validate-order-key key) t)
    (excal-index-error nil)))

(defun excal--index-validate-integer (int)
  "Signal an error unless INT is a well-formed integer part."
  (unless (= (length int) (excal--index-integer-length (aref int 0)))
    (signal 'excal-index-error
            (list (format "invalid integer part of order key: %s" int)))))

(defun excal--index-increment-integer (x)
  "Return the integer part after X, or nil if X is the largest."
  (excal--index-validate-integer x)
  (let* ((head (aref x 0))
         (digs (append (substring x 1) nil))
         (vec (vconcat digs))
         (carry t)
         (base (length excal--base-62-digits)))
    (cl-loop for i downfrom (1- (length vec)) to 0
             while carry
             do (let ((d (1+ (excal--index-digit-value (aref vec i)))))
                  (if (= d base)
                      (aset vec i ?0)
                    (aset vec i (aref excal--base-62-digits d))
                    (setq carry nil))))
    (setq digs (append vec nil))
    (if carry
        (cond
         ((eq head ?Z) "a0")
         ((eq head ?z) nil)
         (t (let ((h (1+ head)))
              (if (> h ?a)
                  (setq digs (append digs (list ?0)))
                (setq digs (butlast digs)))
              (concat (string h) digs))))
      (concat (string head) digs))))

(defun excal--index-decrement-integer (x)
  "Return the integer part before X, or nil if X is the smallest."
  (excal--index-validate-integer x)
  (let* ((head (aref x 0))
         (vec (vconcat (substring x 1)))
         (borrow t)
         (top (aref excal--base-62-digits (1- (length excal--base-62-digits)))))
    (cl-loop for i downfrom (1- (length vec)) to 0
             while borrow
             do (let ((d (1- (excal--index-digit-value (aref vec i)))))
                  (if (= d -1)
                      (aset vec i top)
                    (aset vec i (aref excal--base-62-digits d))
                    (setq borrow nil))))
    (let ((digs (append vec nil)))
      (if borrow
          (cond
           ((eq head ?a) (string ?Z top))
           ((eq head ?A) nil)
           (t (let ((h (1- head)))
                (if (< h ?Z)
                    (setq digs (append digs (list top)))
                  (setq digs (butlast digs)))
                (concat (string h) digs))))
        (concat (string head) digs)))))

(defun excal--index-between (a b)
  "Return an order key strictly between keys A and B.
Either may be nil for no bound; with both nil, return \"a0\"."
  (when a (excal--validate-order-key a))
  (when b (excal--validate-order-key b))
  (when (and a b (not (string< a b)))
    (signal 'excal-index-error (list (format "%s >= %s" a b))))
  (cond
   ((null a)
    (if (null b)
        "a0"
      (let* ((ib (excal--index-integer-part b))
             (fb (substring b (length ib))))
        (cond
         ((equal ib excal--index-smallest-integer)
          (concat ib (excal--index-midpoint "" fb)))
         ((string< ib b) ib)
         (t (or (excal--index-decrement-integer ib)
                (signal 'excal-index-error (list "cannot decrement any more"))))))))
   ((null b)
    (let* ((ia (excal--index-integer-part a))
           (fa (substring a (length ia)))
           (i (excal--index-increment-integer ia)))
      (or i (concat ia (excal--index-midpoint fa nil)))))
   (t
    (let* ((ia (excal--index-integer-part a))
           (fa (substring a (length ia)))
           (ib (excal--index-integer-part b))
           (fb (substring b (length ib))))
      (if (equal ia ib)
          (concat ia (excal--index-midpoint fa fb))
        (let ((i (or (excal--index-increment-integer ia)
                     (signal 'excal-index-error
                             (list "cannot increment any more")))))
          (if (string< i b)
              i
            (concat ia (excal--index-midpoint fa nil)))))))))

(defun excal--index-n-between (a b n)
  "Return a list of N ascending order keys strictly between A and B."
  (cond
   ((<= n 0) nil)
   ((= n 1) (list (excal--index-between a b)))
   ((null b)
    (let* ((c (excal--index-between a nil)) (result (list c)))
      (dotimes (_ (1- n))
        (setq c (excal--index-between c nil))
        (push c result))
      (nreverse result)))
   ((null a)
    (let* ((c (excal--index-between nil b)) (result (list c)))
      (dotimes (_ (1- n))
        (setq c (excal--index-between nil c))
        (push c result))
      result))
   (t
    (let* ((mid (/ n 2))
           (c (excal--index-between a b)))
      (append (excal--index-n-between a c mid)
              (list c)
              (excal--index-n-between c b (- n mid 1)))))))

;;;; Keeping element indices in sync (packages/element/src/fractionalIndex.ts)

(defun excal--indexed-p (element)
  "Return non-nil if ELEMENT takes part in fractional indexing."
  (member (alist-get 'type element) excal--indexed-types))

(defun excal--index-key-of (element)
  "Return ELEMENT's index if it is a valid order key, else nil."
  (let ((key (alist-get 'index element)))
    (and (stringp key) (excal--valid-order-key-p key) key)))

(defun excal--index-bump (element)
  "Bump ELEMENT's version like upstream `mutateElement'."
  (excal--put element 'version
              (1+ (let ((v (alist-get 'version element)))
                    (if (numberp v) v 0))))
  (excal--put element 'versionNonce (random (ash 1 31)))
  (excal--put element 'updated (truncate (* 1000 (float-time)))))

(defun excal--index-valid-p (key lower upper)
  "Port of `isValidFractionalIndex': KEY between LOWER and UPPER."
  (and key
       (cond ((and lower upper) (and (string< lower key) (string< key upper)))
             (upper (string< key upper))
             (lower (string< lower key))
             (t t))))

(defun excal--invalid-index-groups (keys)
  "Return runs of invalid keys in vector KEYS, as upstream.
Each run is a list (LOWER I... UPPER) of positions: the valid bounds
around the positions I that need new keys."
  (let ((n (length keys)) (lower-idx -1) (upper-idx 0)
        lower upper groups (i 0))
    (cl-labels
        ((key (j) (and (>= j 0) (< j n) (aref keys j)))
         (get-lower (j)
           (let ((lb (key lower-idx)) (cand (key (1- j))))
             (if (or (and (not lb) cand) (and lb cand (string< lb cand)))
                 (cons cand (1- j))
               (cons lb lower-idx))))
         (get-upper (j)
           (let ((ub (key upper-idx)))
             (if (and ub (< j upper-idx))
                 (cons ub upper-idx)
               (let ((k upper-idx) found)
                 (while (and (not found) (< (cl-incf k) n))
                   (let ((cand (key k)))
                     (when (or (and (not ub) cand) (and ub cand (string< ub cand)))
                       (setq found (cons cand k)))))
                 (or found (cons nil k)))))))
      (while (< i n)
        (pcase-setq `(,lower . ,lower-idx) (get-lower i)
                    `(,upper . ,upper-idx) (get-upper i))
        (if (excal--index-valid-p (aref keys i) lower upper)
            (cl-incf i)
          (let ((group (list i lower-idx)))
            (catch 'break
              (while (< (cl-incf i) n)
                (pcase-let ((`(,nl . ,nli) (get-lower i))
                            (`(,nu . ,nui) (get-upper i)))
                  (when (excal--index-valid-p (aref keys i) nl nu)
                    (throw 'break nil))
                  (setq lower nl lower-idx nli upper nu upper-idx nui)
                  (push i group))))
            (push upper-idx group)
            (push (nreverse group) groups))))
      (nreverse groups))))

(defun excal--index-apply-groups (elements keys groups)
  "Give new keys to the elements of vector ELEMENTS in GROUPS.
KEYS holds the current valid keys (nil for invalid ones).  Return the
list of (ELEMENT . NEW-KEY)."
  (let (updates)
    (dolist (group groups)
      (let* ((lower (car group))
             (upper (car (last group)))
             (inner (butlast (cdr group)))
             (new (excal--index-n-between
                   (and (>= lower 0) (< lower (length keys)) (aref keys lower))
                   (and (>= upper 0) (< upper (length keys)) (aref keys upper))
                   (length inner))))
        (cl-mapc (lambda (i key) (push (cons (aref elements i) key) updates))
                 inner new)))
    (nreverse updates)))

(defun excal--index-commit (updates)
  "Store the (ELEMENT . KEY) UPDATES, bumping changed elements."
  (pcase-dolist (`(,element . ,key) updates)
    (unless (equal (alist-get 'index element) key)
      (excal--put element 'index key)
      (excal--index-bump element))))

(defun excal--sync-indices (&optional elements)
  "Give valid, increasing indices to ELEMENTS (default: the scene).
Port of `syncInvalidIndices': only missing, malformed or out-of-order
keys are regenerated.  Elements are modified in place; return ELEMENTS."
  (let* ((elements (or elements excal--elements))
         (vec (vconcat (seq-filter #'excal--indexed-p elements)))
         (keys (vconcat (mapcar #'excal--index-key-of vec))))
    (excal--index-commit
     (condition-case nil
         (excal--index-apply-groups vec keys (excal--invalid-index-groups keys))
       ;; Cannot happen with validated bounds; renumber everything if it does.
       (excal-index-error
        (cl-mapcar #'cons (append vec nil)
                   (excal--index-n-between nil nil (length vec))))))
    elements))

(defun excal--sync-moved-indices (moved &optional elements)
  "Give new indices to the MOVED elements of ELEMENTS (default: the scene).
Port of `syncMovedIndices': each run of moved elements gets keys between
its neighbours' keys; if that leaves the scene invalid, fall back to
`excal--sync-indices'.  Call after inserting or reordering elements."
  (let* ((elements (or elements excal--elements))
         (moved-set (make-hash-table :test #'eq))
         (vec (vconcat (seq-filter #'excal--indexed-p elements)))
         (n (length vec))
         groups (i 0))
    (dolist (e moved) (puthash e t moved-set))
    (while (< i n)
      (if (not (gethash (aref vec i) moved-set))
          (cl-incf i)
        (let ((group (list i (1- i))))
          (while (and (< (cl-incf i) n) (gethash (aref vec i) moved-set))
            (push i group))
          (push i group)
          (push (nreverse group) groups))))
    (let ((updates
           (condition-case nil
               (let* ((raw (vconcat (mapcar (lambda (e)
                                              (let ((k (alist-get 'index e)))
                                                (and (stringp k) k)))
                                            vec)))
                      (updates (excal--index-apply-groups vec raw (nreverse groups)))
                      (candidate (copy-sequence raw))
                      (positions (make-hash-table :test #'eq)))
                 (dotimes (j n) (puthash (aref vec j) j positions))
                 (pcase-dolist (`(,e . ,key) updates)
                   (aset candidate (gethash e positions) key))
                 ;; validateFractionalIndices against the direct neighbours.
                 (dotimes (j n)
                   (unless (and (aref candidate j)
                                (excal--valid-order-key-p (aref candidate j))
                                (excal--index-valid-p
                                 (aref candidate j)
                                 (and (> j 0) (aref candidate (1- j)))
                                 (and (< (1+ j) n) (aref candidate (1+ j)))))
                     (signal 'excal-index-error (list "invalid after move"))))
                 updates)
             (excal-index-error 'invalid))))
      (if (eq updates 'invalid)
          (excal--sync-indices elements)
        (excal--index-commit updates)))
    elements))

(defun excal--indices-in-order-p (&optional elements)
  "Return non-nil if ELEMENTS' indices are all strings, strictly increasing.
This is the cheap check behind `excal--sync-indices-maybe'; keys are not
re-validated, since every key in the scene was validated on load or
generated here."
  (let ((prev nil) (ok t))
    (dolist (e (or elements excal--elements) ok)
      (when (and ok (excal--indexed-p e))
        (let ((key (alist-get 'index e)))
          (if (and (stringp key) (or (null prev) (string< prev key)))
              (setq prev key)
            (setq ok nil)))))))

(defun excal--sync-indices-maybe ()
  "Repair the scene's indices if some are missing or out of order.
Runs from `post-command-hook' before `excal--commit'."
  (when (and excal--elements (not (excal--indices-in-order-p)))
    (excal--sync-indices)))

(provide 'excal-index)
;;; excal-index.el ends here
