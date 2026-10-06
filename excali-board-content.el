;;; excali-board-content.el --- Safe Org blocks for board cards -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Parse Org without export, Babel execution, or evaluating links.
;; Blocks use escaped Pango markup, not HTML or executable content.
;;; Code:
(require 'org-element)

(defun excali-board-content--escape (text)
  "Escape TEXT for Pango markup."
  (setq text (substring-no-properties text))
  (dolist (pair '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")))
    (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
  text)

(defvar excali-board-content--links nil)
(defvar excali-board-content--serial 0)

(defun excali-board-content--plain (markup)
  "Return plain text of our escaped MARKUP (not arbitrary HTML)."
  (let ((text (replace-regexp-in-string "<[^>]*>" "" markup)))
    (dolist (pair '(("&lt;" . "<") ("&gt;" . ">") ("&amp;" . "&")))
      (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t)))
    text))

(defun excali-board-content--link (object body)
  "Render link OBJECT with BODY and a temporary source-position marker."
  (let* ((serial (cl-incf excali-board-content--serial))
         (info (vector (org-element-property :begin object)
                       (org-element-property :type object)
                       (org-element-property :path object)
                       (org-element-property :search-option object)))
         (label (if (string-empty-p body)
                    (excali-board-content--escape (org-element-property :raw-link object))
                  body)))
    (push (cons serial info) excali-board-content--links)
    (format "<excali-link-%d><span foreground='#2563a6' underline='single'>%s</span></excali-link-%d>"
            serial label serial)))

(defun excali-board-content--block (markup nowrap)
  "Strip internal markers from MARKUP and return a block with UTF-8 link ranges."
  (let ((rest (string-trim-right markup)) (out "") spans)
    (while (string-match "<excali-link-\\([0-9]+\\)>" rest)
      (let* ((serial (string-to-number (match-string 1 rest)))
             (prefix (substring rest 0 (match-beginning 0)))
             (tail (substring rest (match-end 0)))
             (end (string-match (format "</excali-link-%d>" serial) tail)))
        (unless end (error "Unclosed internal link marker"))
        (setq out (concat out prefix))
        (let ((start (string-bytes (encode-coding-string (excali-board-content--plain out) 'utf-8))))
          (setq out (concat out (substring tail 0 end)))
          (push (vector start
                        (string-bytes (encode-coding-string (excali-board-content--plain out) 'utf-8))
                        (cdr (assq serial excali-board-content--links))) spans))
        (setq rest (substring tail (+ end (length (format "</excali-link-%d>" serial)))))))
    (vector (concat out rest) (if nowrap 1 0) (vconcat (nreverse spans)))))

(defun excali-board-content--image-p (object)
  "Whether OBJECT is a local, undescribed Org image link."
  (and (not (stringp object)) (eq (org-element-type object) 'link)
       (member (org-element-property :type object) '("file" "attachment"))
       (not (org-element-contents object))
       (member (downcase (or (file-name-extension (org-element-property :path object)) ""))
               '("png" "jpg" "jpeg" "gif" "webp" "svg" "bmp" "ico" "avif"))))

(defun excali-board-content--inline (object)
  "Render Org OBJECT as escaped inline markup."
  (if (stringp object) (excali-board-content--escape object)
    (let* ((type (org-element-type object))
           (body (mapconcat #'excali-board-content--inline
                            (org-element-contents object) ""))
           (value (org-element-property :value object)))
      (concat
       (pcase type
         ('bold (format "<b>%s</b>" body))
         ('italic (format "<i>%s</i>" body))
         ('underline (format "<u>%s</u>" body))
         ('strike-through (format "<s>%s</s>" body))
         ((or 'code 'verbatim)
          (format "<tt>%s</tt>" (excali-board-content--escape (or value ""))))
         ('link (excali-board-content--link object body))
         ('line-break "\n")
         (_ (if value (excali-board-content--escape value) body)))
       (make-string (or (org-element-property :post-blank object) 0) ?\s)))))

(defun excali-board-content--table (table)
  "Render TABLE in aligned monospace columns, preserving wide content."
  (let ((rows
         (delq nil (mapcar
                    (lambda (row)
                      (when (eq (org-element-property :type row) 'standard)
                        (mapcar (lambda (cell)
                                  (string-trim
                                   (mapconcat #'excali-board-content--inline
                                             (org-element-contents cell) "")))
                                (org-element-contents row))))
                    (org-element-contents table))))
        widths)
    (dolist (row rows)
      (cl-loop for cell in row for i from 0 do
               (while (<= (length widths) i) (setq widths (append widths (list 0))))
               (setf (nth i widths) (max (nth i widths) (string-width (excali-board-content--plain cell))))))
    (format "<tt>%s</tt>"
            (mapconcat
              (lambda (row)
                (concat "│ "
                        (mapconcat #'identity
                                   (cl-loop for cell in row for width in widths
                                            collect (concat cell (make-string
                                                                  (- width (string-width (excali-board-content--plain cell))) ?\s)))
                                   " │ ")
                        " │"))
              rows "\n"))))

(defun excali-board-content-blocks (text)
  "Return formatted blocks for all of Org TEXT, without executing it.
Each block is [MARKUP NOWRAP LINKS] with optional local image metadata.
NOWRAP is 0 or 1; link ranges use UTF-8 byte offsets for native Pango hit tests."
  (with-temp-buffer
    (insert text)
    (let ((org-mode-hook nil) (org-inhibit-startup t))
      (org-mode))
    (let ((tree (org-element-parse-buffer))
          (excali-board-content--links nil) (excali-board-content--serial 0) blocks)
      (cl-labels
       ((emit (markup &optional nowrap)
          (unless (string-empty-p (string-trim markup))
            (push (excali-board-content--block markup nowrap) blocks)))
        (walk (node depth)
          (let ((type (org-element-type node)))
            (pcase type
              ('headline
               (emit (format "<span size='%d'><b>%s%s</b></span>"
                             (if (= depth 0) 24576 20480)
                             (if-let* ((todo (org-element-property :todo-keyword node)))
                                 (concat (excali-board-content--escape todo) " ") "")
                             (mapconcat #'excali-board-content--inline
                                        (org-element-property :title node) "")))
               (mapc (lambda (child) (walk child (1+ depth)))
                     (org-element-contents node)))
              ('paragraph
               (let ((text ""))
                 (dolist (part (org-element-contents node))
                   (if (excali-board-content--image-p part)
                       (progn
                         (emit text) (setq text "")
                         (push (vector
                                (excali-board-content--escape
                                 (concat "[Image: " (org-element-property :path part) "]"))
                                0 []
                                (vector (org-element-property :begin part)
                                        (org-element-property :type part)
                                        (org-element-property :path part) nil))
                               blocks))
                     (setq text (concat text (excali-board-content--inline part)))))
                 (emit text)))
              ('table (emit (excali-board-content--table node) t))
              ((or 'src-block 'example-block 'fixed-width)
               (emit (format "<tt>%s</tt>"
                             (excali-board-content--escape
                              (or (org-element-property :value node) ""))) t))
              ('item
               (let* ((children (org-element-contents node))
                      (first (car children))
                      (prefix
                       (concat (make-string (* 2 (max 0 (1- depth))) ?\s)
                               (pcase (org-element-property :checkbox node)
                                 ('on "[x] ") ('off "[ ] ") ('trans "[-] ") (_ "• "))
                               (if-let* ((tag (org-element-property :tag node)))
                                   (concat (mapconcat #'excali-board-content--inline tag "") " — ") ""))))
                 (if (eq (org-element-type first) 'paragraph)
                     (progn
                       (if (seq-some #'excali-board-content--image-p
                                     (org-element-contents first))
                           (progn (emit prefix) (walk first depth))
                         (emit (concat prefix
                                       (mapconcat #'excali-board-content--inline
                                                  (org-element-contents first) ""))))
                       (setq children (cdr children)))
                   (emit prefix))
                 (mapc (lambda (child) (walk child (1+ depth))) children)))
              ((or 'property-drawer 'drawer 'planning 'comment 'comment-block
                   'keyword 'babel-call) nil)
              (_ (dolist (child (org-element-contents node))
                   (unless (stringp child) (walk child depth))))))))
       (walk tree 0))
      (vconcat (nreverse blocks)))))

(provide 'excali-board-content)
;;; excali-board-content.el ends here
