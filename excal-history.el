;;; excal-history.el --- Undo and redo  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; History is a stack of scene snapshots taken after each command that
;; changed the scene.  A snapshot lists frozen copies of the elements;
;; copies are cached per element id and version, so elements that did not
;; change share one copy across snapshots and a step costs only the
;; elements it touched.  This relies on every mutation going through
;; `excal--touch', which bumps `version'.
;;
;; Selection and view changes are not recorded, as in Excalidraw.

;;; Code:

(require 'excal-core)
(require 'excal-view)

(defcustom excal-history-limit 100
  "Maximum number of undo steps kept per buffer."
  :type 'integer
  :group 'excal)

(defvar-local excal--undo-stack nil
  "Snapshots, newest first; the head is the current scene.")
(defvar-local excal--redo-stack nil "Snapshots undone, newest first.")
(defvar-local excal--frozen nil
  "Hash table mapping element ids to (VERSION . FROZEN-COPY).")

(defun excal--freeze (element)
  "Return a frozen copy of ELEMENT, reusing the cached one if unchanged."
  (let* ((id (excal--get element 'id))
         (version (excal--get element 'version))
         (cached (gethash id excal--frozen)))
    (if (and cached (equal (car cached) version))
        (cdr cached)
      (let ((copy (copy-tree element t)))
        (puthash id (cons version copy) excal--frozen)
        copy))))

(defun excal--snapshot ()
  "Return a snapshot of the current scene."
  (list :elements (mapcar #'excal--freeze excal--elements)
        :selection (mapcar (lambda (e) (excal--get e 'id)) excal--selection)))

(defun excal--same-scene-p (a b)
  "Return non-nil if snapshots A and B hold the same frozen elements."
  (let ((ea (plist-get a :elements)) (eb (plist-get b :elements)))
    (and (= (length ea) (length eb))
         (cl-every #'eq ea eb))))

(defun excal--history-reset ()
  "Start a fresh history whose only entry is the current scene."
  (setq excal--frozen (make-hash-table :test #'equal)
        excal--history-hold nil
        excal--redo-stack nil
        excal--undo-stack (list (excal--snapshot))))

(defvar-local excal--history-hold nil
  "Non-nil while a change is pending and must not be recorded yet.
Flowchart creation holds the history until its nodes are committed.")

(defun excal--commit ()
  "Record the scene as an undo step if it changed since the last one.
Nothing is recorded while `excal--history-hold' is set."
  (when (and excal--frozen (not excal--history-hold))
    (let ((snapshot (excal--snapshot)))
      (unless (excal--same-scene-p snapshot (car excal--undo-stack))
        (push snapshot excal--undo-stack)
        (setq excal--redo-stack nil)
        (when (> (length excal--undo-stack) excal-history-limit)
          (setcdr (nthcdr (1- excal-history-limit) excal--undo-stack) nil))))))

(defun excal--restore (snapshot)
  "Replace the scene with fresh, mutable copies of SNAPSHOT's elements."
  (setq excal--elements (mapcar (lambda (e) (copy-tree e t))
                                (plist-get snapshot :elements)))
  ;; The restored copies equal the frozen ones, so keep them cached.
  (cl-mapc (lambda (live frozen)
             (puthash (excal--get live 'id)
                      (cons (excal--get live 'version) frozen)
                      excal--frozen))
           excal--elements (plist-get snapshot :elements))
  (let ((ids (plist-get snapshot :selection)))
    (setq excal--selection
          (seq-filter (lambda (e) (member (excal--get e 'id) ids))
                      excal--elements))))

(defun excal-undo ()
  "Undo the last change to the scene."
  (interactive)
  (if (null (cdr excal--undo-stack))
      (message "No further undo information")
    (push (pop excal--undo-stack) excal--redo-stack)
    (excal--restore (car excal--undo-stack))
    (excal--render)))

(defun excal-redo ()
  "Redo the last undone change."
  (interactive)
  (if (null excal--redo-stack)
      (message "No further redo information")
    (let ((snapshot (pop excal--redo-stack)))
      (push snapshot excal--undo-stack)
      (excal--restore snapshot))
    (excal--render)))

(provide 'excal-history)
;;; excal-history.el ends here
