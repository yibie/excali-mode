;;; excali-history.el --- Undo and redo  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; History is a stack of scene snapshots taken after each command that
;; changed the scene.  A snapshot lists frozen copies of the elements;
;; copies are cached per element id and version, so elements that did not
;; change share one copy across snapshots and a step costs only the
;; elements it touched.  This relies on every mutation going through
;; `excali--touch', which bumps `version'.
;;
;; Selection and view changes are not recorded, as in Excalidraw.

;;; Code:

(require 'excali-core)
(require 'excali-view)

(defcustom excali-history-limit 100
  "Maximum number of undo steps kept per buffer."
  :type 'integer
  :group 'excali)

(defvar-local excali--undo-stack nil
  "Snapshots, newest first; the head is the current scene.")
(defvar-local excali--redo-stack nil "Snapshots undone, newest first.")
(defvar-local excali--frozen nil
  "Hash table mapping element ids to (VERSION . FROZEN-COPY).")

(defun excali--freeze (element)
  "Return a frozen copy of ELEMENT, reusing the cached one if unchanged."
  (let* ((id (excali--get element 'id))
         (version (excali--get element 'version))
         (cached (gethash id excali--frozen)))
    (if (and cached (equal (car cached) version))
        (cdr cached)
      (let ((copy (copy-tree element t)))
        (puthash id (cons version copy) excali--frozen)
        copy))))

(defun excali--snapshot ()
  "Return a snapshot of the current scene."
  (list :elements (mapcar #'excali--freeze excali--elements)
        :selection (mapcar (lambda (e) (excali--get e 'id)) excali--selection)))

(defun excali--same-scene-p (a b)
  "Return non-nil if snapshots A and B hold the same frozen elements."
  (let ((ea (plist-get a :elements)) (eb (plist-get b :elements)))
    (and (= (length ea) (length eb))
         (cl-every #'eq ea eb))))

(defun excali--history-reset ()
  "Start a fresh history whose only entry is the current scene."
  (setq excali--frozen (make-hash-table :test #'equal)
        excali--history-hold nil
        excali--redo-stack nil
        excali--undo-stack (list (excali--snapshot))))

(defvar-local excali--history-hold nil
  "Non-nil while a change is pending and must not be recorded yet.
Flowchart creation holds the history until its nodes are committed.")

(defun excali--commit ()
  "Record the scene as an undo step if it changed since the last one.
Nothing is recorded while `excali--history-hold' is set."
  (when (and excali--frozen (not excali--history-hold))
    (let ((snapshot (excali--snapshot)))
      (unless (excali--same-scene-p snapshot (car excali--undo-stack))
        (push snapshot excali--undo-stack)
        (setq excali--redo-stack nil)
        (when (> (length excali--undo-stack) excali-history-limit)
          (setcdr (nthcdr (1- excali-history-limit) excali--undo-stack) nil))))))

(defun excali--restore (snapshot)
  "Replace the scene with fresh, mutable copies of SNAPSHOT's elements."
  (setq excali--elements (mapcar (lambda (e) (copy-tree e t))
                                (plist-get snapshot :elements)))
  ;; The restored copies equal the frozen ones, so keep them cached.
  (cl-mapc (lambda (live frozen)
             (puthash (excali--get live 'id)
                      (cons (excali--get live 'version) frozen)
                      excali--frozen))
           excali--elements (plist-get snapshot :elements))
  (let ((ids (plist-get snapshot :selection)))
    (setq excali--selection
          (seq-filter (lambda (e) (member (excali--get e 'id) ids))
                      excali--elements))))

(defun excali-undo ()
  "Undo the last change to the scene."
  (interactive)
  (if (null (cdr excali--undo-stack))
      (message "No further undo information")
    (push (pop excali--undo-stack) excali--redo-stack)
    (excali--restore (car excali--undo-stack))
    (excali--render)))

(defun excali-redo ()
  "Redo the last undone change."
  (interactive)
  (if (null excali--redo-stack)
      (message "No further redo information")
    (let ((snapshot (pop excali--redo-stack)))
      (push snapshot excali--undo-stack)
      (excali--restore snapshot))
    (excali--render)))

(provide 'excali-history)
;;; excali-history.el ends here
