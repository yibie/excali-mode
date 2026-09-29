;;; excali-dsl.el --- Draw scenes from a text DSL  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A text language for diagrams, compatible with the `.edsl' files of
;; excalidraw-dsl (github.com/tyrchen/excalidraw-dsl): nodes, edges and
;; edge chains, containers and groups, component types, connection
;; blocks and layer templates, under optional YAML front matter.  A
;; scene is laid out automatically (excali-dsl-layout.el) and drawn with
;; excali's own elements, so arrows stay bound to their shapes and labels
;; to their containers when the result is edited.
;;
;;     ---
;;     direction: LR
;;     ---
;;     client[Web Client]
;;     container "Backend" as backend {
;;       api[API] { backgroundColor: "#a5d8ff" }
;;       db[Database] { shape: ellipse }
;;       api -> db: query
;;     }
;;     client -> api: HTTPS
;;
;; Entry points: `excali-dsl-mode' for .edsl files, where C-c C-c draws
;; the buffer (`excali-dsl-render'); `excali-dsl-yank' and
;; `excali-dsl-insert-file' add a diagram to an excali scene; and the
;; functions `excali-dsl-parse', `excali-dsl-elements' and
;; `excali-dsl-scene' for programs.  docs/dsl.md lists the syntax.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'excali-core)
(require 'excali-text)
(require 'excali-binding)
(require 'excali-elbow)
(require 'excali-restore)
(require 'excali-dsl-layout)

(declare-function excali--open "excali")
(declare-function excali--insert-elements "excali-clipboard")
(declare-function excali--view-center "excali-select")
(declare-function excali--elements-bounds "excali-select")
(declare-function excali--render "excali-view")
(declare-function excali-zoom-to-fit "excali-actions")
(declare-function excali--history-reset "excali-history")
(defvar excali--theme)
(defvar excali--file)
(defvar excali--doc)

(defgroup excali-dsl nil
  "Drawing excali scenes from text."
  :group 'excali)

(defcustom excali-dsl-render-on-save nil
  "Non-nil means saving an .edsl buffer draws it again, if drawn before."
  :type 'boolean)

(defcustom excali-dsl-indent-offset 2
  "Indentation step of `excali-dsl-mode'."
  :type 'integer)

(define-error 'excali-dsl-error "Diagram DSL error")

(defvar excali-dsl--templates nil
  "Templates known while parsing: name to (KIND . DEFINITION).")

;;;; Errors

(defun excali-dsl--error (format-string &rest args)
  "Signal `excali-dsl-error' at the current line with FORMAT-STRING, ARGS."
  (signal 'excali-dsl-error (list (line-number-at-pos) (apply #'format format-string args))))

(defun excali-dsl-error-message (err)
  "Return a message for the `excali-dsl-error' ERR."
  (format "line %d: %s" (nth 1 err) (nth 2 err)))

;;;; YAML front matter (the subset .edsl files use)

(defun excali-dsl--yaml-scalar (text)
  "Return the YAML scalar TEXT as a Lisp value."
  (let ((text (string-trim text)))
    (cond
     ((string-match "\\`\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"" text)
      (replace-regexp-in-string "\\\\\\(.\\)" "\\1" (match-string 1 text) t))
     ((string-match "\\`'\\([^']*\\)'" text) (match-string 1 text))
     ((string-match-p "\\`-?[0-9]+\\(\\.[0-9]+\\)?\\'" text) (string-to-number text))
     ((member text '("true" "yes" "on")) t)
     ((member text '("false" "no" "off")) :false)
     ((member text '("null" "~" "")) nil)
     ((string-match "\\`\\[\\(.*\\)\\]\\'" text)
      (vconcat (mapcar #'excali-dsl--yaml-scalar
                       (split-string (match-string 1 text) "," t "[ \t]+"))))
     ;; An unquoted value ends at a comment.
     ((string-match "\\`\\(.*?\\)[ \t]+#" text) (string-trim (match-string 1 text)))
     (t text))))

(defun excali-dsl--yaml-lines (text)
  "Return TEXT's meaningful lines as (INDENT . CONTENT)."
  (let (lines)
    (dolist (line (split-string text "\n") (nreverse lines))
      (unless (string-match-p "\\`[ \t]*\\(#.*\\)?\\'" line)
        (string-match "\\`\\([ \t]*\\)\\(.*\\)" line)
        (push (cons (length (match-string 1 line)) (string-trim-right (match-string 2 line)))
              lines)))))

(defun excali-dsl--yaml-block (lines indent)
  "Parse LINES (a list, consumed from its head) at INDENT.
Return (VALUE . REST): an alist of (KEY . VALUE) with string keys, or
a vector for a list of `- ' items."
  (if (and lines (string-prefix-p "- " (concat (cdar lines) " ")))
      (let (items)
        (while (and lines (= (caar lines) indent) (string-prefix-p "-" (cdar lines)))
          (push (excali-dsl--yaml-scalar (substring (cdar lines) 1)) items)
          (setq lines (cdr lines)))
        (cons (vconcat (nreverse items)) lines))
    (let (entries)
      (while (and lines (= (caar lines) indent))
        (let ((content (cdar lines)))
          (setq lines (cdr lines))
          (if (string-match "\\`\\(\"[^\"]*\"\\|[^:]+?\\)[ \t]*:\\(?:[ \t]+\\(.*\\)\\)?\\'" content)
              (let ((key (excali-dsl--yaml-scalar (match-string 1 content)))
                    (value (match-string 2 content)))
                (if (and (or (null value) (string-empty-p (string-trim value)))
                         lines (> (caar lines) indent))
                    (let ((nested (excali-dsl--yaml-block lines (caar lines))))
                      (push (cons (format "%s" key) (car nested)) entries)
                      (setq lines (cdr nested)))
                  (push (cons (format "%s" key) (excali-dsl--yaml-scalar (or value ""))) entries)))
            ;; Anything else (an odd line) is skipped with its children.
            (while (and lines (> (caar lines) indent)) (setq lines (cdr lines))))))
      (cons (nreverse entries) lines))))

(defun excali-dsl--parse-yaml (text)
  "Parse the front matter TEXT into an alist with string keys."
  (let ((lines (excali-dsl--yaml-lines text)))
    (if lines (car (excali-dsl--yaml-block lines (caar lines))) nil)))

;;;; Lexing

(defconst excali-dsl--id-regexp "[[:alnum:]_.]+" "Node ids.")
(defconst excali-dsl--arrow-regexp "<->\\|->\\|---\\|--\\|~>" "Edge operators.")
(defconst excali-dsl--group-keywords
  '("group" "flow" "service" "layer" "component" "subsystem" "zone" "cluster")
  "Words starting a group: basic, flow and semantic groups.")

(defun excali-dsl--skip ()
  "Skip whitespace, comments and statement separators."
  (while (progn (skip-chars-forward " \t\r\n;")
                (when (eq (char-after) ?#)
                  (skip-chars-forward "^\n")
                  t))))

(defun excali-dsl--skip-blank ()
  "Skip spaces and tabs only."
  (skip-chars-forward " \t\r"))

(defun excali-dsl--looking-at-word (word)
  "Return non-nil if WORD, as a whole word, is at point."
  (looking-at (concat (regexp-quote word) "\\_>")))

(defun excali-dsl--read-id (&optional what)
  "Read an id at point, or signal an error mentioning WHAT."
  (if (looking-at excali-dsl--id-regexp)
      (progn (goto-char (match-end 0)) (match-string-no-properties 0))
    (excali-dsl--error "Expected %s" (or what "an id"))))

(defun excali-dsl--read-string ()
  "Read a double-quoted string at point, or return nil."
  (when (looking-at "\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"")
    (goto-char (match-end 0))
    (replace-regexp-in-string "\\\\\\(.\\)" "\\1" (match-string-no-properties 1) t)))

(defun excali-dsl--read-bracket-label ()
  "Read a `[label]' at point, or return nil."
  (when (looking-at "\\[\\([^]\"\n[]*\\)\\]")
    (goto-char (match-end 0))
    (string-trim (match-string-no-properties 1))))

(defun excali-dsl--expect (char)
  "Skip blanks and CHAR, or signal an error."
  (excali-dsl--skip)
  (if (eq (char-after) char)
      (forward-char)
    (excali-dsl--error "Expected `%c'" char)))

;;;; Blocks: { key: value; ... }

(defun excali-dsl--read-value ()
  "Read an attribute value at point: string, list, number, color or word."
  (excali-dsl--skip-blank)
  (cond
   ((eq (char-after) ?\") (excali-dsl--read-string))
   ((eq (char-after) ?\[)
    (forward-char)
    (let (items)
      (while (progn (excali-dsl--skip)
                    (not (eq (char-after) ?\])))
        (when (eobp) (excali-dsl--error "Missing `]'"))
        (push (or (excali-dsl--read-string)
                  (excali-dsl--read-id "a list item"))
              items)
        (excali-dsl--skip)
        (when (eq (char-after) ?,) (forward-char)))
      (forward-char)
      (vconcat (nreverse items))))
   ((looking-at "\\(-?[0-9]+\\(?:\\.[0-9]+\\)?\\)\\(?:px\\)?\\(?:[ \t]*\\(?:[;,}\n]\\|\\'\\)\\)")
    (goto-char (match-end 1))
    (when (looking-at "px") (forward-char 2))
    (string-to-number (match-string 1)))
   ((looking-at "#[[:xdigit:]]\\{3,8\\}\\_>")
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   ((looking-at "\\(true\\|false\\)\\_>")
    (goto-char (match-end 0))
    (if (equal (match-string 1) "true") t :false))
   ((looking-at "[^;,}\n]+")
    (goto-char (match-end 0))
    (string-trim (match-string-no-properties 0)))
   (t (excali-dsl--error "Expected a value"))))

(defun excali-dsl--read-block ()
  "Read a `{ ... }' block at point and return its entries.
Entries are (KEY . VALUE) with string keys; a nested block's value is
its own list of entries, a list's a vector."
  (excali-dsl--expect ?{)
  (let (entries)
    (while (progn (excali-dsl--skip) (not (eq (char-after) ?})))
      (when (eobp) (excali-dsl--error "Missing `}'"))
      (let ((key (or (excali-dsl--read-string)
                     (and (looking-at "[[:alnum:]_.-]+")
                          (progn (goto-char (match-end 0)) (match-string-no-properties 0)))
                     (excali-dsl--error "Expected an attribute name"))))
        (excali-dsl--skip-blank)
        (when (eq (char-after) ?:)
          (forward-char)
          (excali-dsl--skip-blank)
          (when (eq (char-after) ?\n) (excali-dsl--skip)))
        (push (cons key (if (eq (char-after) ?{)
                            (excali-dsl--read-block)
                          (excali-dsl--read-value)))
              entries))
      (excali-dsl--skip-blank)
      (when (memq (char-after) '(?\; ?,)) (forward-char)))
    (forward-char)
    (nreverse entries)))

(defun excali-dsl--brace-label-p ()
  "Return non-nil if the `{' at point opens an edge label, not attributes.
A label holds no `name:'."
  (save-excursion
    (forward-char)
    (let ((start (point)))
      (and (search-forward "}" (line-end-position) t)
           (not (string-match-p "[[:alnum:]_\"]+[ \t]*:"
                                (buffer-substring-no-properties start (1- (point)))))))))

;;;; Statements

(defun excali-dsl--parse-statements (&optional in-cluster)
  "Parse statements up to a closing `}' when IN-CLUSTER, else to the end.
Return a list of statements; in a cluster, attribute lines come back
as (attrs . ENTRIES)."
  (let (out)
    (catch 'done
      (while t
        (excali-dsl--skip)
        (cond
         ((eobp)
          (when in-cluster (excali-dsl--error "Missing `}'"))
          (throw 'done nil))
         ((eq (char-after) ?})
          (unless in-cluster (excali-dsl--error "Unexpected `}'"))
          (forward-char)
          (throw 'done nil))
         (t (setq out (append (reverse (excali-dsl--parse-statement in-cluster)) out))))))
    (nreverse out)))

(defun excali-dsl--keyword-form-p (word)
  "Return non-nil if keyword WORD at point starts its own statement.
That is when an arrow does not follow it, so a node may still be
called `layer' or `service'."
  (save-excursion
    (goto-char (+ (point) (length word)))
    (excali-dsl--skip-blank)
    (not (or (looking-at excali-dsl--arrow-regexp)
             (looking-at "\\[")
             (eolp)))))

(defun excali-dsl--parse-statement (in-cluster)
  "Parse one statement at point; return a list of statements.
IN-CLUSTER allows attribute lines."
  (let ((line (line-number-at-pos)))
    (cond
     ((and (excali-dsl--looking-at-word "componentType")
           (excali-dsl--keyword-form-p "componentType"))
      (forward-char (length "componentType"))
      (excali-dsl--skip)
      (let* ((name (excali-dsl--read-id "a component type name"))
             (body (progn (excali-dsl--skip) (excali-dsl--read-block))))
        (list (list 'component-type :name name :body body :line line))))
     ((and (excali-dsl--looking-at-word "container")
           (excali-dsl--keyword-form-p "container"))
      (forward-char (length "container"))
      (list (excali-dsl--parse-cluster 'container "container" line)))
     ((and (looking-at (concat (regexp-opt excali-dsl--group-keywords) "\\_>"))
           (let ((word (match-string 0)))
             (and (excali-dsl--keyword-form-p word)
                  (save-excursion
                    (goto-char (match-end 0))
                    (excali-dsl--skip-blank)
                    (or (eq (char-after) ?\")
                        (and (member word '("group" "flow"))
                             (looking-at excali-dsl--id-regexp)))))))
      (looking-at (concat (regexp-opt excali-dsl--group-keywords) "\\_>"))
      (let ((word (match-string-no-properties 0)))
        (goto-char (match-end 0))
        (list (excali-dsl--parse-cluster 'group word line))))
     ((and (excali-dsl--looking-at-word "connections")
           (excali-dsl--keyword-form-p "connections"))
      (forward-char (length "connections"))
      (excali-dsl--skip)
      (excali-dsl--connection-edges (excali-dsl--read-block) line))
     ((and (excali-dsl--looking-at-word "connection")
           (excali-dsl--keyword-form-p "connection"))
      (forward-char (length "connection"))
      (excali-dsl--skip)
      (excali-dsl--connection-edges (excali-dsl--read-block) line))
     ((and (excali-dsl--looking-at-word "template")
           (excali-dsl--keyword-form-p "template"))
      (forward-char (length "template"))
      (excali-dsl--skip)
      (let* ((name (excali-dsl--read-id "a template name"))
             (body (progn (excali-dsl--skip) (excali-dsl--read-block))))
        (push (cons name (cons 'layers body)) excali-dsl--templates)
        nil))
     ((and (excali-dsl--looking-at-word "diagram")
           (excali-dsl--keyword-form-p "diagram"))
      (forward-char (length "diagram"))
      (excali-dsl--skip)
      (let* ((title (or (excali-dsl--read-string) (excali-dsl--read-id "a diagram title")))
             (body (progn (excali-dsl--skip) (excali-dsl--read-block))))
        (excali-dsl--diagram title body line)))
     ((and (excali-dsl--looking-at-word "layout")
           (save-excursion (forward-char 6) (excali-dsl--skip) (eq (char-after) ?{)))
      (forward-char 6)
      (excali-dsl--skip)
      (list (list 'layout :body (excali-dsl--read-block) :line line)))
     ((and in-cluster (looking-at "\\([[:alnum:]_]+\\)[ \t]*:\\([^:]\\|$\\)"))
      ;; A cluster's own attribute, or `style: { ... }'.
      (let ((key (match-string-no-properties 1)))
        (goto-char (match-end 1))
        (excali-dsl--skip-blank)
        (forward-char)
        (excali-dsl--skip-blank)
        (if (eq (char-after) ?{)
            (list (cons 'attrs (excali-dsl--read-block)))
          (list (cons 'attrs (list (cons key (excali-dsl--read-value))))))))
     ((and in-cluster (excali-dsl--looking-at-word "style")
           (save-excursion (forward-char 5) (excali-dsl--skip) (eq (char-after) ?{)))
      (forward-char 5)
      (excali-dsl--skip)
      (list (cons 'attrs (excali-dsl--read-block))))
     ((looking-at excali-dsl--id-regexp)
      (excali-dsl--parse-node-or-edges line))
     (t (excali-dsl--error "Unexpected `%s'"
                           (buffer-substring-no-properties
                            (point) (min (line-end-position) (+ (point) 20))))))))

(defun excali-dsl--parse-cluster (kind word line)
  "Parse a container or group after its keyword WORD.
KIND is `container' or `group'.  Upstream writes `container \"Label\"
as id {'; the older `container id \"Label\" {' and `group id:type {'
are read too."
  (excali-dsl--skip-blank)
  (let (id label type)
    (when (and (looking-at excali-dsl--id-regexp) (not (excali-dsl--looking-at-word "as")))
      (setq id (excali-dsl--read-id))
      (when (eq (char-after) ?:)
        (forward-char)
        (setq type (excali-dsl--read-id "a group type")))
      (excali-dsl--skip-blank))
    (setq label (excali-dsl--read-string))
    (excali-dsl--skip-blank)
    (when (excali-dsl--looking-at-word "as")
      (forward-char 2)
      (excali-dsl--skip-blank)
      (setq id (excali-dsl--read-id "an id after `as'")))
    (excali-dsl--skip)
    (unless (eq (char-after) ?{)
      (excali-dsl--error "Expected `{' after %s" word))
    (forward-char)
    (let* ((body (excali-dsl--parse-statements t))
           (attrs (apply #'append (mapcar #'cdr (seq-filter (lambda (s) (eq (car s) 'attrs)) body)))))
      (list kind :id id :label label :kind (or type word) :attrs attrs
            :body (seq-remove (lambda (s) (eq (car s) 'attrs)) body) :line line))))

(defun excali-dsl--read-edge-label ()
  "Read an edge label after `:' up to the end of the line, `;', `{' or `}'."
  (excali-dsl--skip-blank)
  (or (excali-dsl--read-string)
      (when (looking-at "[^;{}\n]+")
        (goto-char (match-end 0))
        (let ((text (string-trim (match-string-no-properties 0))))
          ;; A trailing comment is not part of the label.
          (string-trim (replace-regexp-in-string "[ \t]+#.*\\'" "" text))))))

(defun excali-dsl--parse-node-or-edges (line)
  "Parse a node, an edge chain or a template instance at point.
Return a list of statements, starting at LINE."
  (let* ((id (excali-dsl--read-id))
         (label (progn (excali-dsl--skip-blank)
                       (or (excali-dsl--read-bracket-label) (excali-dsl--read-string))))
         (template (assoc id excali-dsl--templates)))
    (excali-dsl--skip-blank)
    (cond
     ;; `microservice user_service { name: "User" }'
     ((and template (eq (cadr template) 'yaml) (null label)
           (looking-at excali-dsl--id-regexp))
      (let* ((instance (excali-dsl--read-id))
             (params (progn (excali-dsl--skip) (if (eq (char-after) ?{) (excali-dsl--read-block) nil))))
        (excali-dsl--instantiate-yaml (cddr template) instance params line)))
     ((save-excursion (excali-dsl--skip) (looking-at excali-dsl--arrow-regexp))
      (excali-dsl--parse-chain id label line))
     (t
      (let (type attrs)
        (when (looking-at "@\\([[:alnum:]_]+\\)")
          (setq type (match-string-no-properties 1))
          (goto-char (match-end 0)))
        (when (save-excursion (excali-dsl--skip) (eq (char-after) ?{))
          (excali-dsl--skip)
          (setq attrs (excali-dsl--read-block)))
        (list (list 'node :id id :label label :type type :attrs attrs :line line)))))))

(defun excali-dsl--parse-chain (first first-label line)
  "Parse an edge chain starting at node FIRST (labelled FIRST-LABEL).
Return node statements for labelled references and one edge per link."
  (let ((nodes (list (list first first-label)))
        (arrows nil) (segment-labels nil)
        chain-label attrs routing)
    (while (progn (excali-dsl--skip) (looking-at excali-dsl--arrow-regexp))
      (push (match-string-no-properties 0) arrows)
      (goto-char (match-end 0))
      (excali-dsl--skip)
      (let* ((id (excali-dsl--read-id "a node after the arrow"))
             (label (progn (excali-dsl--skip-blank) (excali-dsl--read-bracket-label))))
        (push (list id label) nodes)
        ;; `a -> b "label"' labels this link.
        (excali-dsl--skip-blank)
        (push (excali-dsl--read-string) segment-labels)))
    (setq nodes (nreverse nodes) arrows (nreverse arrows)
          segment-labels (nreverse segment-labels))
    (excali-dsl--skip-blank)
    (cond ((eq (char-after) ?:) (forward-char) (setq chain-label (excali-dsl--read-edge-label)))
          ((and (eq (char-after) ?{) (excali-dsl--brace-label-p))
           (forward-char)
           (setq chain-label (string-trim (buffer-substring-no-properties
                                           (point) (1- (search-forward "}")))))))
    (when (save-excursion (excali-dsl--skip-blank) (eq (char-after) ?{))
      (excali-dsl--skip-blank)
      (setq attrs (excali-dsl--read-block)))
    (excali-dsl--skip-blank)
    (when (looking-at "@\\(straight\\|orthogonal\\|curved\\|auto\\)")
      (setq routing (match-string-no-properties 1))
      (goto-char (match-end 0)))
    (append
     (delq nil (mapcar (lambda (n) (when (nth 1 n)
                                     (list 'node :id (car n) :label (nth 1 n) :line line
                                           :reference t)))
                       nodes))
     (cl-loop for (a b) on nodes while b
              for arrow in arrows for seg in segment-labels
              collect (list 'edge :from (car a) :to (car b) :arrow arrow
                            :label (or seg chain-label)
                            :attrs (if routing (cons (cons "routing" routing) attrs) attrs)
                            :line line)))))

(defun excali-dsl--connection-edges (body line)
  "Return edges for a `connection' or `connections' BODY at LINE."
  (let* ((from (cdr (assoc "from" body)))
         (to (cdr (assoc "to" body)))
         (style (cdr (assoc "style" body)))
         (targets (if (vectorp to) (append to nil) (list to))))
    (unless (and from to) (excali-dsl--error "A connection needs `from' and `to'"))
    (mapcar (lambda (target)
              (list 'edge :from (format "%s" from) :to (format "%s" target)
                    :arrow (pcase (cdr (assoc "type" style))
                             ("line" "--") (_ "->"))
                    :label (cdr (assoc "label" style))
                    :attrs (append
                            (pcase (cdr (assoc "type" style))
                              ((and s (or "dashed" "dotted")) (list (cons "strokeStyle" s))))
                            (seq-remove (lambda (a) (member (car a) '("type" "label"))) style))
                    :line line))
            targets)))

;;;; Templates

(defun excali-dsl--slug (text)
  "Return an id made of TEXT."
  (let ((s (downcase (replace-regexp-in-string "[^[:alnum:]]+" "_" (string-trim text)))))
    (if (string-empty-p s) "item" s)))

(defun excali-dsl--instantiate-yaml (definition instance params line)
  "Expand the front matter template DEFINITION as INSTANCE with PARAMS.
Its nodes become INSTANCE.KEY, labelled with `$name' (and any other
`$param') substituted; its `edges' link them."
  (let ((subst (lambda (text)
                 (let ((text (format "%s" text)))
                   (dolist (p params text)
                     (setq text (string-replace (concat "$" (car p)) (format "%s" (cdr p)) text))))))
        nodes edges)
    (dolist (entry definition)
      (if (equal (car entry) "edges")
          (seq-doseq (spec (cdr entry))
            (if (string-match "\\`[ \t]*\\([[:alnum:]_.]+\\)[ \t]*\\(<->\\|->\\|---\\|--\\|~>\\)[ \t]*\\([[:alnum:]_.]+\\)" spec)
                (push (list 'edge :from (concat instance "." (match-string 1 spec))
                            :to (concat instance "." (match-string 3 spec))
                            :arrow (match-string 2 spec) :line line)
                      edges)
              (signal 'excali-dsl-error (list line (format "Bad template edge `%s'" spec)))))
        (push (list 'node :id (concat instance "." (car entry))
                    :label (funcall subst (cdr entry)) :line line)
              nodes)))
    (append (nreverse nodes) (nreverse edges))))

(defun excali-dsl--diagram (title body line)
  "Expand `diagram TITLE { template: NAME }' (BODY) at LINE.
A layer template draws each layer as a container of its components,
linked by the template's connection pattern."
  (let* ((name (cdr (assoc "template" body)))
         (template (and name (assoc name excali-dsl--templates))))
    (unless template
      (signal 'excali-dsl-error (list line (if name (format "Unknown template `%s'" name)
                                             "A diagram needs `template:'"))))
    (unless (eq (cadr template) 'layers)
      (signal 'excali-dsl-error (list line (format "Template `%s' has no layers" name))))
    (let* ((definition (cddr template))
           (layers (cdr (assoc "layers" definition)))
           (pattern (cdr (assoc "pattern" (cdr (assoc "connections" definition)))))
           (layout (cdr (assoc "layout" definition)))
           (used (make-hash-table :test #'equal))
           (unique (lambda (text)
                     (let* ((base (excali-dsl--slug text)) (id base) (n 1))
                       (while (gethash id used) (setq id (format "%s_%d" base (cl-incf n))))
                       (puthash id t used)
                       id)))
           (layer-ids nil) (statements nil))
      (dolist (layer layers)
        (let* ((components (append (cdr (assoc "components" (cdr layer))) nil))
               (arrangement (cdr (assoc "layout" (cdr layer))))
               (ids (mapcar (lambda (c) (cons (funcall unique c) c)) components)))
          (push (mapcar #'car ids) layer-ids)
          (push (list 'container :id (funcall unique (car layer)) :label (car layer)
                      :kind "layer"
                      :attrs (and arrangement (list (cons "layout" arrangement)))
                      :body (mapcar (lambda (c) (list 'node :id (car c) :label (cdr c) :line line))
                                    ids)
                      :line line)
                statements)))
      (setq layer-ids (nreverse layer-ids))
      (let ((edge (lambda (a b arrow) (list 'edge :from a :to b :arrow arrow :line line)))
            edges)
        (pcase pattern
          ("each-to-next-layer"
           (cl-loop for (upper lower) on layer-ids while lower
                    do (dolist (a upper) (dolist (b lower) (push (funcall edge a b "->") edges)))))
          ("mesh"
           (let ((all (apply #'append layer-ids)))
             (cl-loop for (a . rest) on all
                      do (dolist (b rest) (push (funcall edge a b "--") edges)))))
          ((and (pred stringp)
                (guard (string-match "\\`star([ \t]*\"?\\([^\")]+\\)" pattern)))
           (let* ((hub (match-string 1 pattern))
                  (all (apply #'append layer-ids))
                  (hub-id (or (seq-find (lambda (id) (equal id (excali-dsl--slug hub))) all)
                              (signal 'excali-dsl-error
                                      (list line (format "No component `%s' for star()" hub))))))
             (dolist (b all) (unless (equal b hub-id) (push (funcall edge hub-id b "->") edges)))))
          ((or "custom" 'nil) nil)
          (_ (signal 'excali-dsl-error (list line (format "Unknown connection pattern `%s'" pattern)))))
        (append (list (list 'title :text title :line line))
                (and layout (list (list 'layout :body layout :line line)))
                (nreverse statements)
                (nreverse edges))))))

;;;; Parsing

(defun excali-dsl-parse (string)
  "Parse diagram DSL STRING and return (CONFIG . STATEMENTS).
CONFIG is the front matter as an alist with string keys.  Signal
`excali-dsl-error', with a line number and a message, on bad input."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (config (excali-dsl--templates nil))
      (skip-chars-forward " \t\r\n")
      (when (looking-at "---[ \t]*$")
        (let ((start (line-beginning-position 2)) (opening (point)))
          (forward-line 1)
          (unless (re-search-forward "^---[ \t]*$" nil t)
            (goto-char opening)
            (excali-dsl--error "Front matter has no closing `---'"))
          (setq config (excali-dsl--parse-yaml
                        (buffer-substring-no-properties start (match-beginning 0))))))
      (dolist (entry (cdr (assoc "templates" config)))
        (push (cons (car entry) (cons 'yaml (cdr entry))) excali-dsl--templates))
      (cons config (excali-dsl--parse-statements)))))

;;;; The model

(cl-defstruct (excali-dsl--node (:constructor excali-dsl--make-node))
  id label type attrs parent line x y w h element)

(cl-defstruct (excali-dsl--cluster (:constructor excali-dsl--make-cluster))
  id label kind attrs parent children line x y w h element label-element)

(cl-defstruct (excali-dsl--edge (:constructor excali-dsl--make-edge))
  from to arrow label attrs parent line waypoints)

(defun excali-dsl--config (config key &rest aliases)
  "Return KEY (or one of ALIASES) from front matter CONFIG or its layout_options."
  (let ((options (cdr (assoc "layout_options" config))))
    (cl-loop for k in (cons key aliases)
             for v = (or (cdr (assoc k config)) (cdr (assoc k options)))
             when v return v)))

(defun excali-dsl--direction (value)
  "Return the layout direction TB, BT, LR or RL named by VALUE, or nil."
  (pcase (and value (downcase (format "%s" value)))
    ((or "tb" "td" "down" "vertical" "top-to-bottom") "TB")
    ((or "bt" "up" "bottom-to-top") "BT")
    ((or "lr" "right" "horizontal" "left-to-right") "LR")
    ((or "rl" "left" "right-to-left") "RL")))

(defun excali-dsl--build (parsed)
  "Build the diagram model of PARSED, from `excali-dsl-parse'.
Return a plist :config :root :nodes :clusters :edges :types :title."
  (let* ((config (car parsed))
         (nodes (make-hash-table :test #'equal))
         (clusters (make-hash-table :test #'equal))
         (types (make-hash-table :test #'equal))
         (root (excali-dsl--make-cluster :id nil :kind "root"))
         (edges nil) (title nil) (counter 0)
         (layout nil))
    ;; Component types from the front matter.
    (dolist (entry (cdr (assoc "component_types" config)))
      (puthash (car entry) (cdr entry) types))
    (cl-labels
        ((add-child (cluster item)
           (unless (memq item (excali-dsl--cluster-children cluster))
             (setf (excali-dsl--cluster-children cluster)
                   (append (excali-dsl--cluster-children cluster) (list item)))))
         (remove-child (cluster item)
           (setf (excali-dsl--cluster-children cluster)
                 (delq item (excali-dsl--cluster-children cluster))))
         (walk (statements cluster)
           (dolist (s statements)
             (pcase (car s)
               ('node
                (let* ((id (plist-get (cdr s) :id))
                       (node (gethash id nodes)))
                  (if node
                      (progn
                        (when (plist-get (cdr s) :label)
                          (setf (excali-dsl--node-label node) (plist-get (cdr s) :label)))
                        (when (plist-get (cdr s) :type)
                          (setf (excali-dsl--node-type node) (plist-get (cdr s) :type)))
                        (setf (excali-dsl--node-attrs node)
                              (append (excali-dsl--node-attrs node) (plist-get (cdr s) :attrs)))
                        ;; A definition, not a mere reference, places it.
                        (unless (plist-get (cdr s) :reference)
                          (remove-child (excali-dsl--node-parent node) node)
                          (setf (excali-dsl--node-parent node) cluster)
                          (add-child cluster node)))
                    (setq node (excali-dsl--make-node
                                :id id :label (plist-get (cdr s) :label)
                                :type (plist-get (cdr s) :type)
                                :attrs (plist-get (cdr s) :attrs)
                                :parent cluster :line (plist-get (cdr s) :line)))
                    (puthash id node nodes)
                    (add-child cluster node))))
               ('edge
                (push (excali-dsl--make-edge
                       :from (plist-get (cdr s) :from) :to (plist-get (cdr s) :to)
                       :arrow (plist-get (cdr s) :arrow) :label (plist-get (cdr s) :label)
                       :attrs (plist-get (cdr s) :attrs) :parent cluster
                       :line (plist-get (cdr s) :line))
                      edges))
               ((or 'container 'group)
                (let* ((id (or (plist-get (cdr s) :id)
                               (format "%s_%d" (car s) (cl-incf counter))))
                       (child (excali-dsl--make-cluster
                               :id id :label (plist-get (cdr s) :label)
                               :kind (if (eq (car s) 'container) "container"
                                       (plist-get (cdr s) :kind))
                               :attrs (plist-get (cdr s) :attrs) :parent cluster
                               :line (plist-get (cdr s) :line))))
                  (when (gethash id clusters)
                    (signal 'excali-dsl-error
                            (list (plist-get (cdr s) :line) (format "Duplicate container `%s'" id))))
                  (puthash id child clusters)
                  (add-child cluster child)
                  (walk (plist-get (cdr s) :body) child)))
               ('component-type
                (let* ((body (plist-get (cdr s) :body))
                       (style (cdr (assoc "style" body)))
                       (shape (cdr (assoc "shape" body))))
                  (puthash (plist-get (cdr s) :name)
                           (append (and shape (list (cons "shape" shape)))
                                   (seq-remove (lambda (e) (member (car e) '("shape" "style"))) body)
                                   style)
                           types)))
               ('layout (setq layout (append layout (plist-get (cdr s) :body))))
               ('title (setq title (plist-get (cdr s) :text)))))))
      (walk (cdr parsed) root)
      (setq edges (nreverse edges))
      ;; Resolve edge ends; unknown ids become nodes where first used.
      (dolist (edge edges)
        (dolist (end '(from to))
          (let ((id (if (eq end 'from) (excali-dsl--edge-from edge) (excali-dsl--edge-to edge))))
            (unless (or (gethash id nodes) (gethash id clusters))
              (let ((resolved (excali-dsl--resolve id nodes clusters)))
                (if resolved
                    (if (eq end 'from) (setf (excali-dsl--edge-from edge) resolved)
                      (setf (excali-dsl--edge-to edge) resolved))
                  (let ((node (excali-dsl--make-node :id id :parent (excali-dsl--edge-parent edge)
                                                     :line (excali-dsl--edge-line edge))))
                    (puthash id node nodes)
                    (add-child (excali-dsl--edge-parent edge) node)))))))))
    ;; Component types.
    (maphash (lambda (_ node)
               (let ((type (or (excali-dsl--node-type node)
                               (cdr (assoc "type" (excali-dsl--node-attrs node))))))
                 (when type
                   (let ((style (gethash (format "%s" type) types :missing)))
                     (when (eq style :missing)
                       (signal 'excali-dsl-error
                               (list (excali-dsl--node-line node)
                                     (format "Unknown component type `%s'" type))))
                     (setf (excali-dsl--node-attrs node)
                           (append (excali-dsl--node-attrs node) style))))))
             nodes)
    (list :config config :root root :nodes nodes :clusters clusters
          :edges edges :types types :title title :layout layout)))

(defun excali-dsl--resolve (id nodes clusters)
  "Return the node or cluster id qualified reference ID stands for, or nil.
`backend.api' finds `api' in container `backend', or a unique `api'."
  (when (string-match-p "\\." id)
    (let* ((parts (split-string id "\\."))
           (last (car (last parts)))
           (node (gethash last nodes)))
      (cond ((and node
                  (let ((parent (excali-dsl--node-parent node)))
                    (or (null (butlast parts))
                        (and parent (equal (excali-dsl--cluster-id parent)
                                           (car (last parts 2)))))))
             last)
            (node last)
            ((gethash last clusters) last)))))

;;;; Styles

(defun excali-dsl--attr (attrs &rest keys)
  "Return the first of KEYS found in ATTRS."
  (cl-loop for k in keys for v = (cdr (assoc k attrs)) when v return v))

(defun excali-dsl--color (value)
  "Return VALUE as an Excalidraw color string, or nil."
  (when value
    (let ((s (format "%s" value)))
      (cond ((string-match "\\`#[[:xdigit:]]+\\'" s) (downcase s))
            ((string-equal s "transparent") s)
            ((string-match "\\`rgba?(\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\)" s)
             (format "#%02x%02x%02x" (string-to-number (match-string 1 s))
                     (string-to-number (match-string 2 s)) (string-to-number (match-string 3 s))))
            ((and (fboundp 'color-name-to-rgb) (ignore-errors (color-name-to-rgb s)))
             (apply #'format "#%02x%02x%02x"
                    (mapcar (lambda (c) (round (* 255 c))) (color-name-to-rgb s))))
            (t s)))))

(defun excali-dsl--font (value default)
  "Return the font family id VALUE names, or DEFAULT."
  (let ((name (and value (format "%s" value))))
    (cond ((null name) default)
          ((numberp value) value)
          ((cdr (assoc-string name excali-font-family-ids t)))
          ((member (downcase name) '("hand-drawn" "handdrawn")) 5)
          ((equal (downcase name) "normal") 6)
          ((equal (downcase name) "code") 8)
          (t default))))

(defun excali-dsl--arrowhead (value default)
  "Return the Excalidraw arrowhead VALUE names, or DEFAULT."
  (pcase (and value (downcase (format "%s" value)))
    ('nil default)
    ((or "none" "null" "false") :null)
    ((or "dot" "circle") "circle")
    ("triangle" "triangle") ("diamond" "diamond") ("bar" "bar") ("arrow" "arrow")
    (name name)))

(defun excali-dsl--defaults (config)
  "Return the default style from front matter CONFIG as a plist."
  (let ((sketchiness (excali-dsl--config config "sketchiness" "roughness")))
    (list :roughness (if (numberp sketchiness) (min 2 (max 0 (round sketchiness))) 1)
          :stroke-width (let ((w (excali-dsl--config config "stroke_width" "strokeWidth")))
                          (if (numberp w) w 2))
          :font (excali-dsl--font (excali-dsl--config config "font") excali-default-font-family)
          :font-size (let ((s (excali-dsl--config config "fontSize" "font_size")))
                       (if (numberp s) s excali-default-font-size))
          :routing (format "%s" (or (excali-dsl--config config "routing" "edge_routing" "edges")
                                    "straight")))))

(defun excali-dsl--shape (node)
  "Return the element type NODE is drawn as."
  (pcase (and (excali-dsl--attr (excali-dsl--node-attrs node) "shape")
              (downcase (format "%s" (excali-dsl--attr (excali-dsl--node-attrs node) "shape"))))
    ((or "ellipse" "circle" "cylinder" "oval") "ellipse")
    ((or "diamond" "rhombus" "decision") "diamond")
    ("text" "text")
    (_ "rectangle")))

;;;; Sizes

(defun excali-dsl--label (node)
  "Return NODE's label: its label or its id."
  (or (excali-dsl--node-label node) (excali-dsl--node-id node)))

(defun excali-dsl--node-size (node defaults)
  "Return (WIDTH . HEIGHT) for NODE, fitting its label, given DEFAULTS."
  (let* ((attrs (excali-dsl--node-attrs node))
         (size (or (excali-dsl--attr attrs "fontSize" "font_size") (plist-get defaults :font-size)))
         (family (excali-dsl--font (excali-dsl--attr attrs "font") (plist-get defaults :font)))
         (text (excali--normalize-text (excali-dsl--label node)))
         (measured (excali--measure-string text size family (excali--line-height family)))
         (shape (excali-dsl--shape node))
         (w (excali-dsl--attr attrs "width")) (h (excali-dsl--attr attrs "height")))
    (if (equal shape "text")
        (cons (float (or w (car measured))) (float (or h (cdr measured))))
      ;; An explicit size is kept unless the label would not fit.
      (let ((fit-w (excali--container-dimension-for-text (car measured) shape))
            (fit-h (excali--container-dimension-for-text (cdr measured) shape)))
        (cons (float (if (numberp w) (max w fit-w)
                       (max 120 (excali--container-dimension-for-text (+ (car measured) 30) shape))))
              (float (if (numberp h) (max h fit-h)
                       (max 60 (excali--container-dimension-for-text (+ (cdr measured) 20) shape)))))))))

;;;; Layout

(defconst excali-dsl--cluster-padding 30 "Space inside a container around its content.")
(defconst excali-dsl--cluster-label-size 16 "Font size of container labels.")

(defun excali-dsl--child-of (cluster item)
  "Return the child of CLUSTER that holds ITEM (a node or cluster), or nil."
  (let ((x item))
    (while (and x (not (eq (if (excali-dsl--node-p x) (excali-dsl--node-parent x)
                             (excali-dsl--cluster-parent x))
                           cluster)))
      (setq x (if (excali-dsl--node-p x) (excali-dsl--node-parent x) (excali-dsl--cluster-parent x))))
    x))

(defun excali-dsl--item (id model)
  "Return the node or cluster ID names in MODEL."
  (or (gethash id (plist-get model :nodes)) (gethash id (plist-get model :clusters))))

(defun excali-dsl--item-size (item)
  "Return ITEM's (WIDTH . HEIGHT)."
  (if (excali-dsl--node-p item)
      (cons (excali-dsl--node-w item) (excali-dsl--node-h item))
    (cons (excali-dsl--cluster-w item) (excali-dsl--cluster-h item))))

(defun excali-dsl--set-item-position (item x y)
  "Place ITEM's top-left at X, Y relative to its parent's content."
  (if (excali-dsl--node-p item)
      (setf (excali-dsl--node-x item) x (excali-dsl--node-y item) y)
    (setf (excali-dsl--cluster-x item) x (excali-dsl--cluster-y item) y)))

(defun excali-dsl--rank-spacing (edges direction options)
  "Return the layer spacing leaving room for the labels of EDGES.
DIRECTION says whether labels lie across (TB, BT) or along (LR, RL) the
gap; OPTIONS holds the configured spacing and font."
  (let ((font (plist-get options :font)) (extent 0))
    (dolist (edge edges)
      (when-let* ((label (excali-dsl--edge-label edge))
                  ((not (string-empty-p label))))
        (let ((size (excali--measure-string label 16 font (excali--line-height font))))
          (setq extent (max extent (if (member direction '("LR" "RL")) (car size) (cdr size)))))))
    (max (plist-get options :rank-spacing) (if (> extent 0) (+ extent 60) 0))))

(defun excali-dsl--layout-cluster (cluster model options)
  "Lay out CLUSTER's children and size CLUSTER; OPTIONS is a plist.
Child clusters are laid out first and then placed as single boxes, so
clusters never overlap and always hold their members.  Child positions
are relative to CLUSTER's content origin."
  (dolist (child (excali-dsl--cluster-children cluster))
    (unless (excali-dsl--node-p child)
      (excali-dsl--layout-cluster child model options)))
  (let* ((children (vconcat (excali-dsl--cluster-children cluster)))
         (index (let ((h (make-hash-table :test #'eq)))
                  (dotimes (i (length children)) (puthash (aref children i) i h))
                  h))
         (sizes (vconcat (mapcar #'excali-dsl--item-size children)))
         (links nil) (link-edges nil)
         (attrs (excali-dsl--cluster-attrs cluster))
         (direction (or (excali-dsl--direction (excali-dsl--attr attrs "direction"))
                        (plist-get options :direction)))
         (arrangement (excali-dsl--attr attrs "layout")))
    (dolist (edge (plist-get model :edges))
      (let* ((a (excali-dsl--child-of cluster (excali-dsl--item (excali-dsl--edge-from edge) model)))
             (b (excali-dsl--child-of cluster (excali-dsl--item (excali-dsl--edge-to edge) model))))
        (when (and a b (not (eq a b)))
          (push (cons (gethash a index) (gethash b index)) links)
          (push edge link-edges))))
    (setq links (nreverse links) link-edges (nreverse link-edges))
    (let ((positions
           (cond
            ((= (length children) 0) [])
            ((and arrangement (null links)
                  (string-match "\\`\\(horizontal\\|vertical\\|grid(\\([0-9]+\\))\\)\\'"
                                (format "%s" arrangement)))
             (excali-dsl-layout-grid
              sizes
              (pcase (match-string 1 (format "%s" arrangement))
                ("horizontal" (length children))
                ("vertical" 1)
                (_ (string-to-number (match-string 2 (format "%s" arrangement)))))
              (plist-get options :node-spacing)))
            (t
             (let ((result (excali-dsl-layout-graph
                            sizes links :direction direction
                            :node-spacing (plist-get options :node-spacing)
                            :rank-spacing (excali-dsl--rank-spacing link-edges direction options))))
               ;; Long links between direct child nodes bend around the
               ;; layers they cross.
               (cl-mapc (lambda (edge waypoints)
                          (when (and waypoints
                                     (excali-dsl--node-p (excali-dsl--item (excali-dsl--edge-from edge) model))
                                     (excali-dsl--node-p (excali-dsl--item (excali-dsl--edge-to edge) model))
                                     (eq (excali-dsl--child-of cluster (excali-dsl--item (excali-dsl--edge-from edge) model))
                                         (excali-dsl--item (excali-dsl--edge-from edge) model))
                                     (eq (excali-dsl--child-of cluster (excali-dsl--item (excali-dsl--edge-to edge) model))
                                         (excali-dsl--item (excali-dsl--edge-to edge) model)))
                            (setf (excali-dsl--edge-waypoints edge) (cons cluster waypoints))))
                        link-edges (cdr result))
               (car result)))))
          (max-x 0.0) (max-y 0.0))
      (dotimes (i (length children))
        (let ((p (aref positions i)) (s (aref sizes i)))
          (excali-dsl--set-item-position (aref children i) (car p) (cdr p))
          (setq max-x (max max-x (+ (car p) (car s)))
                max-y (max max-y (+ (cdr p) (cdr s))))))
      (if (null (excali-dsl--cluster-parent cluster))
          (setf (excali-dsl--cluster-w cluster) max-x (excali-dsl--cluster-h cluster) max-y)
        (let* ((pad (let ((p (excali-dsl--attr attrs "padding")))
                      (if (numberp p) p excali-dsl--cluster-padding)))
               (header (if (excali-dsl--cluster-label cluster)
                           (+ (cdr (excali--measure-string
                                    (excali-dsl--cluster-label cluster) excali-dsl--cluster-label-size
                                    (plist-get options :font)
                                    (excali--line-height (plist-get options :font))))
                              10)
                         0))
               (label-w (if (excali-dsl--cluster-label cluster)
                            (car (excali--measure-string
                                  (excali-dsl--cluster-label cluster) excali-dsl--cluster-label-size
                                  (plist-get options :font)
                                  (excali--line-height (plist-get options :font))))
                          0)))
          (setf (excali-dsl--cluster-w cluster) (max (+ max-x (* 2 pad)) (+ label-w (* 2 pad)))
                (excali-dsl--cluster-h cluster) (+ max-y (* 2 pad) header))
          ;; Where the content starts, inside the box.
          (setf (excali-dsl--cluster-attrs cluster)
                (append (list (cons :content (cons pad (+ pad header)))) attrs)))))))

(defun excali-dsl--absolute (cluster x y)
  "Turn positions under CLUSTER, whose content starts at X, Y, absolute."
  (dolist (child (excali-dsl--cluster-children cluster))
    (if (excali-dsl--node-p child)
        (setf (excali-dsl--node-x child) (+ x (excali-dsl--node-x child))
              (excali-dsl--node-y child) (+ y (excali-dsl--node-y child)))
      (let ((cx (+ x (excali-dsl--cluster-x child))) (cy (+ y (excali-dsl--cluster-y child)))
            (content (cdr (assq :content (excali-dsl--cluster-attrs child)))))
        (setf (excali-dsl--cluster-x child) cx (excali-dsl--cluster-y child) cy)
        (excali-dsl--absolute child (+ cx (car content)) (+ cy (cdr content)))))))

(defun excali-dsl--layout-options (model)
  "Return the layout options of MODEL: direction, spacing and font."
  (let* ((config (plist-get model :config))
         (layout (plist-get model :layout))
         (spacing (cdr (assoc "spacing" layout))))
    (list :direction (or (excali-dsl--direction (cdr (assoc "direction" layout)))
                         (excali-dsl--direction
                          (excali-dsl--config config "direction" "rankdir" "rank_dir"))
                         "TB")
          :node-spacing (or (cdr (assoc "node_spacing" spacing))
                            (excali-dsl--config config "nodeSpacing" "nodesep" "node_spacing")
                            60)
          :rank-spacing (or (cdr (assoc "layer_spacing" spacing))
                            (excali-dsl--config config "rankSpacing" "ranksep" "rank_spacing")
                            90)
          :font (plist-get (excali-dsl--defaults config) :font))))

(defun excali-dsl--layout (model)
  "Size and place every node and cluster of MODEL, in scene coordinates."
  (let ((defaults (excali-dsl--defaults (plist-get model :config)))
        (options (excali-dsl--layout-options model)))
    (maphash (lambda (_ node)
               (pcase-let ((`(,w . ,h) (excali-dsl--node-size node defaults)))
                 (setf (excali-dsl--node-w node) w (excali-dsl--node-h node) h)))
             (plist-get model :nodes))
    (let ((root (plist-get model :root)))
      (excali-dsl--layout-cluster root model options)
      (excali-dsl--absolute root 0.0 0.0)
      ;; Manual layout: nodes with x and y keep them.
      (when (equal (format "%s" (excali-dsl--config (plist-get model :config) "layout")) "manual")
        (maphash (lambda (_ node)
                   (let ((x (excali-dsl--attr (excali-dsl--node-attrs node) "x"))
                         (y (excali-dsl--attr (excali-dsl--node-attrs node) "y")))
                     (when (and (numberp x) (numberp y))
                       (setf (excali-dsl--node-x node) (float x) (excali-dsl--node-y node) (float y)))))
                 (plist-get model :nodes))))
    model))

;;;; Drawing

(defconst excali-dsl--group-colors
  '(("group" "#6b7280" "#f3f4f6") ("flow" "#3b82f6" "#dbeafe")
    ("service" "#8b5cf6" "#f3e8ff") ("layer" "#f59e0b" "#fef3c7")
    ("component" "#10b981" "#d1fae5") ("subsystem" "#ef4444" "#fee2e2")
    ("zone" "#06b6d4" "#cffafe") ("cluster" "#ec4899" "#fce7f3"))
  "Default stroke and background colors of groups by kind, as upstream.")

(defun excali-dsl--custom-data (id)
  "Return the customData marking an element as drawn for DSL ID."
  (list (cons 'edslId id)))

(defun excali-dsl--add (element)
  "Put ELEMENT on top of the scene being drawn and return it."
  (setq excali--elements (append excali--elements (list element)))
  element)

(defun excali-dsl--draw-cluster (cluster defaults)
  "Draw CLUSTER's box and label, then its children's clusters.
DEFAULTS is the default style."
  (when (excali-dsl--cluster-parent cluster)
    (let* ((attrs (excali-dsl--cluster-attrs cluster))
           (kind (excali-dsl--cluster-kind cluster))
           (colors (cdr (assoc kind excali-dsl--group-colors)))
           (container (equal kind "container"))
           (group-id (excali--new-id))
           (box (excali-dsl--add
                 (excali--make-element
                  "rectangle" (excali-dsl--cluster-x cluster) (excali-dsl--cluster-y cluster)
                  (cons 'width (float (excali-dsl--cluster-w cluster)))
                  (cons 'height (float (excali-dsl--cluster-h cluster)))
                  (cons 'strokeColor (or (excali-dsl--color (excali-dsl--attr attrs "strokeColor" "color"))
                                         (if container "#868e96" (or (car colors) "#6b7280"))))
                  (cons 'backgroundColor (or (excali-dsl--color (excali-dsl--attr attrs "backgroundColor" "fill"))
                                             (if container "#f8f9fa" (or (cadr colors) "#f3f4f6"))))
                  (cons 'fillStyle (format "%s" (or (excali-dsl--attr attrs "fillStyle") "solid")))
                  (cons 'strokeWidth (or (excali-dsl--attr attrs "strokeWidth") (if container 1 2)))
                  (cons 'strokeStyle (format "%s" (or (excali-dsl--attr attrs "strokeStyle")
                                                      (if (equal kind "flow") "dashed" "solid"))))
                  (cons 'roughness (or (excali-dsl--attr attrs "roughness") (plist-get defaults :roughness)))
                  (cons 'opacity (or (excali-dsl--attr attrs "opacity") (if container 50 30)))
                  (cons 'roundness '((type . 3)))
                  (cons 'groupIds (vector group-id))
                  (cons 'customData (excali-dsl--custom-data (excali-dsl--cluster-id cluster)))))))
      (setf (excali-dsl--cluster-element cluster) box)
      (when-let* ((label (excali-dsl--cluster-label cluster)))
        (let ((font (excali-dsl--font (excali-dsl--attr attrs "font") (plist-get defaults :font))))
          (setf (excali-dsl--cluster-label-element cluster)
                (excali-dsl--add
                 (excali--make-text-element
                  (+ (excali-dsl--cluster-x cluster) excali-dsl--cluster-padding)
                  (+ (excali-dsl--cluster-y cluster) (/ excali-dsl--cluster-padding 2.0))
                  label
                  (cons 'fontSize (or (excali-dsl--attr attrs "fontSize") excali-dsl--cluster-label-size))
                  (cons 'fontFamily font)
                  (cons 'strokeColor (or (excali-dsl--color (excali-dsl--attr attrs "textColor"))
                                         (if container "#495057" (or (car colors) "#495057"))))
                  (cons 'groupIds (vector group-id)))))))))
  (dolist (child (excali-dsl--cluster-children cluster))
    (unless (excali-dsl--node-p child)
      (excali-dsl--draw-cluster child defaults))))

(defun excali-dsl--draw-node (node defaults)
  "Draw NODE and its label with DEFAULTS; return the shape."
  (let* ((attrs (excali-dsl--node-attrs node))
         (shape (excali-dsl--shape node))
         (font (excali-dsl--font (excali-dsl--attr attrs "font") (plist-get defaults :font)))
         (size (or (excali-dsl--attr attrs "fontSize" "font_size") (plist-get defaults :font-size)))
         (text-color (excali-dsl--color (excali-dsl--attr attrs "textColor" "color")))
         (label (excali-dsl--label node))
         (element
          (if (equal shape "text")
              (excali-dsl--add
               (excali--make-text-element
                (+ (excali-dsl--node-x node) (/ (excali-dsl--node-w node) 2.0))
                (+ (excali-dsl--node-y node) (/ (excali-dsl--node-h node) 2.0))
                label
                (cons 'textAlign "center") (cons 'verticalAlign "middle")
                (cons 'fontSize size) (cons 'fontFamily font)
                (cons 'strokeColor (or text-color "#1e1e1e"))
                (cons 'customData (excali-dsl--custom-data (excali-dsl--node-id node)))))
            (let* ((background (excali-dsl--color (excali-dsl--attr attrs "backgroundColor" "fill")))
                   (fill (excali-dsl--attr attrs "fillStyle" "fill"))
                   (rounded (excali-dsl--attr attrs "roundness" "rounded"))
                   (element
                    (excali-dsl--add
                     (excali--make-element
                      shape (excali-dsl--node-x node) (excali-dsl--node-y node)
                      (cons 'width (float (excali-dsl--node-w node)))
                      (cons 'height (float (excali-dsl--node-h node)))
                      (cons 'strokeColor (or (excali-dsl--color (excali-dsl--attr attrs "strokeColor"))
                                             "#1e1e1e"))
                      (cons 'backgroundColor (if (and background (string-prefix-p "#" background))
                                                 background "transparent"))
                      (cons 'fillStyle (if (and (stringp fill)
                                                (member fill '("hachure" "cross-hatch" "solid" "zigzag")))
                                           fill "solid"))
                      (cons 'strokeWidth (or (excali-dsl--attr attrs "strokeWidth")
                                             (plist-get defaults :stroke-width)))
                      (cons 'strokeStyle (format "%s" (or (excali-dsl--attr attrs "strokeStyle") "solid")))
                      (cons 'roughness (let ((r (excali-dsl--attr attrs "roughness")))
                                         (if (numberp r) (min 2 (max 0 r)) (plist-get defaults :roughness))))
                      (cons 'opacity (or (excali-dsl--attr attrs "opacity") 100))
                      (cons 'roundness (if (and (numberp rounded) (<= rounded 0))
                                           :null
                                         (list (cons 'type (if (equal shape "rectangle") 3 2)))))
                      (cons 'customData (excali-dsl--custom-data (excali-dsl--node-id node)))))))
              (unless (string-empty-p label)
                (let ((text (excali--add-bound-text
                             element (cons 'fontSize size) (cons 'fontFamily font)
                             (cons 'lineHeight (excali--line-height font))
                             (cons 'strokeColor (or text-color "#1e1e1e")))))
                  (excali--set-text text label)))
              element))))
    (setf (excali-dsl--node-element node) element)))

(defun excali-dsl--content-origin (cluster)
  "Return where CLUSTER's content starts, in scene coordinates."
  (if (null (excali-dsl--cluster-parent cluster))
      (cons 0.0 0.0)
    (let ((content (cdr (assq :content (excali-dsl--cluster-attrs cluster)))))
      (cons (+ (excali-dsl--cluster-x cluster) (car content))
            (+ (excali-dsl--cluster-y cluster) (cdr content))))))

(defun excali-dsl--element-center (element)
  "Return the center of ELEMENT's box."
  (cons (+ (excali--get element 'x) (/ (excali--get element 'width) 2.0))
        (+ (excali--get element 'y) (/ (excali--get element 'height) 2.0))))

(defun excali-dsl--side-point (element toward)
  "Return the middle of ELEMENT's side facing the point TOWARD."
  (pcase-let* ((`(,cx . ,cy) (excali-dsl--element-center element))
               (dx (- (car toward) cx)) (dy (- (cdr toward) cy))
               (w (/ (excali--get element 'width) 2.0)) (h (/ (excali--get element 'height) 2.0)))
    (if (>= (* (abs dy) w) (* (abs dx) h))
        (cons cx (+ cy (if (> dy 0) h (- h))))
      (cons (+ cx (if (> dx 0) w (- w))) cy))))

(defconst excali-dsl--detour-margin 16 "Clearance of an edge going around a shape.")

(defun excali-dsl--segment-hits-box-p (p q box)
  "Return the entry parameter if segment P-Q crosses BOX (X1 Y1 X2 Y2), else nil."
  (let ((t0 0.0) (t1 1.0)
        (dx (- (car q) (car p))) (dy (- (cdr q) (cdr p))))
    (catch 'miss
      (cl-loop for (pk qk) in (list (list (- dx) (- (car p) (nth 0 box)))
                                    (list dx (- (nth 2 box) (car p)))
                                    (list (- dy) (- (cdr p) (nth 1 box)))
                                    (list dy (- (nth 3 box) (cdr p))))
               do (if (zerop pk)
                      (when (< qk 0) (throw 'miss nil))
                    (let ((r (/ qk pk)))
                      (if (< pk 0) (setq t0 (max t0 r)) (setq t1 (min t1 r))))))
      (and (< t0 t1) t0))))

(defun excali-dsl--detour (p q obstacles &optional depth)
  "Return points from P to Q going around the OBSTACLES boxes it crosses."
  (let* ((depth (or depth 0))
         (hit (and (< depth 6)
                   (car (sort (delq nil (mapcar (lambda (box)
                                                  (when-let* ((u (excali-dsl--segment-hits-box-p p q box)))
                                                    (cons u box)))
                                                obstacles))
                              (lambda (a b) (< (car a) (car b))))))))
    (if (null hit)
        (list p q)
      (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (cdr hit))
                   (m excali-dsl--detour-margin)
                   (dx (- (car q) (car p))) (dy (- (cdr q) (cdr p)))
                   (vertical (>= (abs dy) (abs dx)))
                   (via
                    (if vertical
                        (let* ((cy (/ (+ y1 y2) 2.0))
                               (x (+ (car p) (* dx (/ (- cy (cdr p)) (if (zerop dy) 1 dy)))))
                               (side (if (< (- x x1) (- x2 x)) (- x1 m) (+ x2 m))))
                          (if (> dy 0) (list (cons side (- y1 m)) (cons side (+ y2 m)))
                            (list (cons side (+ y2 m)) (cons side (- y1 m)))))
                      (let* ((cx (/ (+ x1 x2) 2.0))
                             (y (+ (cdr p) (* dy (/ (- cx (car p)) (if (zerop dx) 1 dx)))))
                             (side (if (< (- y y1) (- y2 y)) (- y1 m) (+ y2 m))))
                        (if (> dx 0) (list (cons (- x1 m) side) (cons (+ x2 m) side))
                          (list (cons (+ x2 m) side) (cons (- x1 m) side)))))))
        (append (butlast (excali-dsl--detour p (car via) obstacles (1+ depth)))
                (butlast (excali-dsl--detour (car via) (cadr via) obstacles (1+ depth)))
                (excali-dsl--detour (cadr via) q obstacles (1+ depth)))))))

(defun excali-dsl--obstacles (model a b)
  "Return the boxes an edge between elements A and B should go around.
Every shape but A and B, and every container holding neither."
  (let ((inside (lambda (element cluster)
                  (let ((box (excali-dsl--box (excali-dsl--cluster-element cluster)))
                        (c (excali-dsl--element-center element)))
                    (and (<= (nth 0 box) (car c) (nth 2 box)) (<= (nth 1 box) (cdr c) (nth 3 box))))))
        boxes)
    (maphash (lambda (_ node)
               (let ((e (excali-dsl--node-element node)))
                 (unless (or (eq e a) (eq e b)) (push (excali-dsl--box e) boxes))))
             (plist-get model :nodes))
    (maphash (lambda (_ cluster)
               (let ((e (excali-dsl--cluster-element cluster)))
                 (unless (or (eq e a) (eq e b)
                             (funcall inside a cluster) (funcall inside b cluster))
                   (push (excali-dsl--box e) boxes))))
             (plist-get model :clusters))
    boxes))

(defun excali-dsl--box (element)
  "Return ELEMENT's box (X1 Y1 X2 Y2)."
  (let ((x (excali--get element 'x)) (y (excali--get element 'y)))
    (list x y (+ x (excali--get element 'width)) (+ y (excali--get element 'height)))))

(defun excali-dsl--draw-edge (edge model defaults)
  "Draw EDGE of MODEL as an arrow bound to its ends; DEFAULTS is the style."
  (let* ((from (excali-dsl--item (excali-dsl--edge-from edge) model))
         (to (excali-dsl--item (excali-dsl--edge-to edge) model))
         (a (if (excali-dsl--node-p from) (excali-dsl--node-element from)
              (excali-dsl--cluster-element from)))
         (b (if (excali-dsl--node-p to) (excali-dsl--node-element to)
              (excali-dsl--cluster-element to))))
    (when (and a b (not (eq a b)))
      (let* ((attrs (excali-dsl--edge-attrs edge))
             (arrow-op (excali-dsl--edge-arrow edge))
             (routing (downcase (format "%s" (or (excali-dsl--attr attrs "routing")
                                                 (if (equal arrow-op "~>") "curved"
                                                   (plist-get defaults :routing))))))
             (elbow (member routing '("orthogonal" "elbow")))
             (curved (member routing '("curved" "round")))
             (ca (excali-dsl--element-center a))
             (cb (excali-dsl--element-center b))
             (waypoints (and (not elbow) (excali-dsl--edge-waypoints edge)
                             (let ((origin (excali-dsl--content-origin
                                            (car (excali-dsl--edge-waypoints edge)))))
                               (mapcar (lambda (p) (cons (+ (car origin) (car p))
                                                         (+ (cdr origin) (cdr p))))
                                       (cdr (excali-dsl--edge-waypoints edge))))))
             (points (cond (elbow (list (excali-dsl--side-point a cb) (excali-dsl--side-point b ca)))
                           ((not curved)
                            ;; Straight segments, bent around what they would cross.
                            (let ((obstacles (excali-dsl--obstacles model a b))
                                  (route (append (list ca) waypoints (list cb))))
                              (cons (car route)
                                    (cl-loop for (p q) on route while q
                                             append (cdr (excali-dsl--detour p q obstacles))))))
                           (curved
                            (let* ((mx (/ (+ (car ca) (car cb)) 2.0)) (my (/ (+ (cdr ca) (cdr cb)) 2.0))
                                   (dx (- (car cb) (car ca))) (dy (- (cdr cb) (cdr ca))))
                              (list ca (cons (- mx (* 0.15 dy)) (+ my (* 0.15 dx))) cb)))
                           (t (list ca cb))))
             (origin (car points))
             (style (excali-dsl--attr attrs "strokeStyle" "style"))
             (arrow (excali-dsl--add
                     (excali--make-element
                      "arrow" (car origin) (cdr origin)
                      (cons 'points (vconcat (mapcar (lambda (p) (vector (float (- (car p) (car origin)))
                                                                         (float (- (cdr p) (cdr origin)))))
                                                     points)))
                      (cons 'strokeColor (or (excali-dsl--color (excali-dsl--attr attrs "strokeColor" "color"))
                                             "#1e1e1e"))
                      (cons 'strokeWidth (or (excali-dsl--attr attrs "strokeWidth" "width")
                                             (plist-get defaults :stroke-width)))
                      (cons 'strokeStyle (if (member style '("dashed" "dotted" "solid")) style "solid"))
                      (cons 'roughness (or (excali-dsl--attr attrs "roughness") (plist-get defaults :roughness)))
                      (cons 'roundness (if curved '((type . 2)) :null))
                      (cons 'startBinding :null) (cons 'endBinding :null)
                      (cons 'startArrowhead
                            (excali-dsl--arrowhead (excali-dsl--attr attrs "startArrowhead")
                                                   (if (equal arrow-op "<->") "arrow" :null)))
                      (cons 'endArrowhead
                            (excali-dsl--arrowhead (excali-dsl--attr attrs "endArrowhead")
                                                   (if (member arrow-op '("--" "---")) :null "arrow")))
                      (cons 'elbowed :false)
                      (cons 'customData (excali-dsl--custom-data
                                         (format "%s->%s" (excali-dsl--edge-from edge)
                                                 (excali-dsl--edge-to edge))))))))
        (excali--linear-extent arrow)
        (if elbow
            (progn
              (excali--elbow-make arrow)
              (excali--elbow-bind-end arrow 'start a)
              (excali--elbow-bind-end arrow 'end b)
              (excali--elbow-route-fresh arrow))
          (excali--bind-end arrow 'start a ca)
          (excali--bind-end arrow 'end b cb)
          (excali--update-arrow arrow))
        (when-let* ((label (excali-dsl--edge-label edge))
                    ((not (string-empty-p label))))
          (let* ((font (excali-dsl--font (excali-dsl--attr attrs "font") (plist-get defaults :font)))
                 (text (excali--add-bound-text
                        arrow (cons 'fontSize (or (excali-dsl--attr attrs "fontSize") 16))
                        (cons 'fontFamily font) (cons 'lineHeight (excali--line-height font))
                        (cons 'strokeColor (or (excali-dsl--color (excali-dsl--attr attrs "textColor"))
                                               "#1e1e1e")))))
            (excali--set-text text label)))
        arrow))))

(defun excali-dsl--draw (model)
  "Draw the laid-out MODEL and return its elements, bottom to top."
  (with-temp-buffer
    (setq-local excali--elements nil)
    (setq-local excali--native-cache (make-hash-table :test #'eq))
    (let ((defaults (excali-dsl--defaults (plist-get model :config)))
          (root (plist-get model :root)))
      (when-let* ((title (plist-get model :title)))
        (excali-dsl--add
         (excali--make-text-element
          (/ (excali-dsl--cluster-w root) 2.0) -60 title
          (cons 'textAlign "center") (cons 'fontSize 28)
          (cons 'fontFamily (plist-get defaults :font)))))
      (excali-dsl--draw-cluster root defaults)
      (let (nodes)
        (maphash (lambda (_ node) (push node nodes)) (plist-get model :nodes))
        (dolist (node (sort nodes (lambda (a b) (< (or (excali-dsl--node-line a) 0)
                                                   (or (excali-dsl--node-line b) 0)))))
          (excali-dsl--draw-node node defaults)))
      (dolist (edge (plist-get model :edges))
        (excali-dsl--draw-edge edge model defaults))
      excali--elements)))

;;;; Programs

(defun excali-dsl-model (string)
  "Parse, build and lay out diagram DSL STRING; return the model plist."
  (excali-dsl--layout (excali-dsl--build (excali-dsl-parse string))))

(defun excali-dsl-elements (string)
  "Return the Excalidraw elements diagram DSL STRING draws.
Elements are alists as in .excalidraw files, bottom to top, with fresh
ids; each shape and arrow records its DSL id in `customData.edslId'.
Signal `excali-dsl-error' on bad input."
  (excali-dsl--draw (excali-dsl-model string)))

(defun excali-dsl-scene (string)
  "Return the .excalidraw document diagram DSL STRING draws.
The document is restored like a loaded file (fractional indices and all),
ready for `excali--serialize-doc'.  Front matter `theme: dark' and
`background_color' go into its app state."
  (let* ((parsed (excali-dsl-parse string))
         (model (excali-dsl--layout (excali-dsl--build parsed)))
         (config (plist-get model :config))
         (doc (excali--empty-doc))
         (background (excali-dsl--color (excali-dsl--config config "background_color"
                                                            "backgroundColor"))))
    (setf (alist-get 'elements doc) (vconcat (excali-dsl--draw model)))
    (when background
      (setf (alist-get 'viewBackgroundColor (alist-get 'appState doc)) background))
    (when (equal (excali-dsl--config config "theme") "dark")
      (setf (alist-get 'theme (alist-get 'appState doc)) "dark"))
    (excali--restore-doc doc)))

(defun excali-dsl-write (string file)
  "Write the scene diagram DSL STRING draws to the .excalidraw FILE."
  (let* ((doc (excali-dsl-scene string))
         (text (excali--serialize-doc doc (append (alist-get 'elements doc) nil))))
    (with-temp-file file
      (setq buffer-file-coding-system 'utf-8-unix)
      (insert text))
    file))

;;;; Commands

(defvar-local excali-dsl--scene-buffer nil
  "The excali buffer this .edsl buffer was last drawn in.")

(defun excali-dsl--signal-user (err &optional name)
  "Report the `excali-dsl-error' ERR as a user error, prefixed with NAME."
  (user-error "%s%s" (if name (concat name ":") "") (excali-dsl-error-message err)))

(defun excali-dsl-render ()
  "Draw the diagram in the current .edsl buffer in an excali buffer.
Drawing again replaces the scene in the same buffer and keeps its view."
  (interactive)
  (let* ((source (buffer-substring-no-properties (point-min) (point-max)))
         (name (format "*excali %s*" (if buffer-file-name
                                         (file-name-nondirectory buffer-file-name)
                                       (buffer-name))))
         (doc (condition-case err (excali-dsl-scene source)
                (excali-dsl-error (excali-dsl--signal-user err (buffer-name)))))
         (dark (equal (excali-dsl--config (car (excali-dsl-parse source)) "theme") "dark"))
         (scene excali-dsl--scene-buffer))
    (if (buffer-live-p scene)
        (with-current-buffer scene
          (setq excali--doc doc
                excali--elements (append (alist-get 'elements doc) nil)
                excali--theme (if dark 'dark 'light))
          (when (hash-table-p excali--native-cache) (clrhash excali--native-cache))
          (excali--history-reset)
          (excali--render)
          (display-buffer scene))
      (let ((origin (selected-window)))
        (let ((display-buffer-overriding-action
               '((display-buffer-reuse-window display-buffer-pop-up-window)
                 (inhibit-same-window . t))))
          (setq scene (excali--open doc nil name)))
        (with-current-buffer scene
          (when dark
            (setq excali--theme 'dark)
            (excali--render))
          (excali-zoom-to-fit))
        (setq excali-dsl--scene-buffer scene)
        (when (window-live-p origin) (select-window origin))))
    scene))

(defun excali-dsl-export (file)
  "Write the diagram in the current .edsl buffer to the .excalidraw FILE."
  (interactive
   (list (read-file-name "Write scene to: " nil nil nil
                         (concat (file-name-base (or buffer-file-name (buffer-name)))
                                 ".excalidraw"))))
  (condition-case err
      (excali-dsl-write (buffer-substring-no-properties (point-min) (point-max)) file)
    (excali-dsl-error (excali-dsl--signal-user err (buffer-name))))
  (message "Wrote %s" file))

(defun excali-dsl-insert (string)
  "Add the diagram DSL STRING draws to the current excali scene.
It is centered in the view and selected, one undo step."
  (let* ((elements (condition-case err (excali-dsl-elements string)
                     (excali-dsl-error (excali-dsl--signal-user err))))
         (bounds (excali--elements-bounds elements))
         (center (excali--view-center))
         (dx (- (car center) (/ (+ (nth 0 bounds) (nth 2 bounds)) 2.0)))
         (dy (- (cdr center) (/ (+ (nth 1 bounds) (nth 3 bounds)) 2.0))))
    (dolist (e elements)
      (excali--put e 'x (float (+ (excali--get e 'x) dx)))
      (excali--put e 'y (float (+ (excali--get e 'y) dy))))
    (excali--insert-elements elements)
    (excali--render)
    elements))

(defun excali-dsl-yank ()
  "Add the diagram DSL at the front of the kill ring to the excali scene.
For pasting a diagram written elsewhere, for example by a program."
  (interactive)
  (excali-dsl-insert (current-kill 0)))

(defun excali-dsl-insert-file (file)
  "Add the diagram in the .edsl FILE to the current excali scene."
  (interactive "fDiagram file: ")
  (excali-dsl-insert (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))

(defun excali-dsl--after-save ()
  "Draw the diagram again after saving, per `excali-dsl-render-on-save'."
  (when (and excali-dsl-render-on-save (buffer-live-p excali-dsl--scene-buffer))
    (excali-dsl-render)))

;;;; The major mode

(defconst excali-dsl--keywords
  '("container" "group" "flow" "service" "layer" "component" "subsystem" "zone"
    "cluster" "componentType" "connection" "connections" "template" "diagram"
    "layout" "style" "as" "layers")
  "Keywords of the diagram DSL.")

(defvar excali-dsl-font-lock-keywords
  `(("\\`---\\(?:.\\|\n\\)*?^---" 0 font-lock-preprocessor-face)
    (,(concat "\\_<" (regexp-opt excali-dsl--keywords) "\\_>") . font-lock-keyword-face)
    (,excali-dsl--arrow-regexp . font-lock-builtin-face)
    ("\\[\\([^]\"\n[]*\\)\\]" 1 font-lock-string-face)
    ("\\_<\\([[:alnum:]_.-]+\\)[ \t]*:" 1 font-lock-variable-name-face)
    ("#[[:xdigit:]]\\{6\\}\\_>" . font-lock-constant-face)
    ("@[[:alnum:]_]+" . font-lock-type-face)
    ("^[ \t]*\\([[:alnum:]_.]+\\)" 1 font-lock-function-name-face))
  "Font lock rules of `excali-dsl-mode'.")

(defvar excali-dsl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table of `excali-dsl-mode'.")

(defun excali-dsl-indent-line ()
  "Indent the current line by its brace depth."
  (interactive)
  (let* ((depth (save-excursion
                  (beginning-of-line)
                  (car (syntax-ppss))))
         (closing (save-excursion (back-to-indentation) (looking-at "[]}]")))
         (column (* excali-dsl-indent-offset (max 0 (- depth (if closing 1 0)))))
         (offset (- (current-column) (current-indentation))))
    (indent-line-to column)
    (when (> offset 0) (forward-char offset))))

(defvar-keymap excali-dsl-mode-map
  "C-c C-c" #'excali-dsl-render
  "C-c C-e" #'excali-dsl-export)

;;;###autoload
(define-derived-mode excali-dsl-mode prog-mode "EDSL"
  "Major mode for diagrams in the excalidraw-dsl language.
\\<excali-dsl-mode-map>\\[excali-dsl-render] draws the buffer in an excali \
buffer, \\[excali-dsl-export] writes it to an .excalidraw file.

\\{excali-dsl-mode-map}"
  (setq-local comment-start "# "
              comment-start-skip "#+[ \t]*"
              font-lock-defaults '(excali-dsl-font-lock-keywords)
              indent-line-function #'excali-dsl-indent-line)
  (add-hook 'after-save-hook #'excali-dsl--after-save nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.edsl\\'" . excali-dsl-mode))

(provide 'excali-dsl)
;;; excali-dsl.el ends here
