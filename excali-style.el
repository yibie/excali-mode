;;; excali-style.el --- Style properties and the style panel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Each buffer has a current style, Excalidraw's `currentItem*' app
;; state, which new elements take.  Setting a property changes the
;; current style and every selected element the property applies to.
;; `excali-style' is a transient panel over all properties.

;;; Code:

(require 'transient)
(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-text)
(require 'excali-elbow)

;;;; Properties

(defconst excali-style-properties
  '((strokeColor
     :app currentItemStrokeColor :default "#1e1e1e" :label "Stroke"
     :types ("rectangle" "stickynote" "ellipse" "diamond" "freedraw" "arrow"
             "line" "text" "embeddable")
     :choices (("Black" . "#1e1e1e") ("Red" . "#e03131") ("Green" . "#2f9e44")
               ("Blue" . "#1971c2") ("Orange" . "#f08c00")))
    (backgroundColor
     :app currentItemBackgroundColor :default "transparent" :label "Background"
     :types ("rectangle" "stickynote" "iframe" "embeddable" "ellipse" "diamond"
             "line" "freedraw")
     :choices (("Transparent" . "transparent") ("Red" . "#ffc9c9")
               ("Green" . "#b2f2bb") ("Blue" . "#a5d8ff") ("Yellow" . "#ffec99")))
    (fillStyle
     :app currentItemFillStyle :default "solid" :label "Fill"
     :types ("rectangle" "iframe" "embeddable" "ellipse" "diamond" "line" "freedraw")
     :choices (("Hachure" . "hachure") ("Cross-hatch" . "cross-hatch")
               ("Solid" . "solid") ("Zigzag" . "zigzag")))
    (strokeWidth
     ;; The style holds a key; elements get the width for their type.
     :app currentItemStrokeWidthKey :default "medium" :label "Stroke width"
     :types ("rectangle" "iframe" "embeddable" "ellipse" "diamond" "freedraw"
             "arrow" "line")
     :choices (("Thin" . "thin") ("Bold" . "medium") ("Extra bold" . "bold")))
    (strokeStyle
     :app currentItemStrokeStyle :default "solid" :label "Stroke style"
     :types ("rectangle" "iframe" "embeddable" "ellipse" "diamond" "arrow" "line")
     :choices (("Solid" . "solid") ("Dashed" . "dashed") ("Dotted" . "dotted")))
    (roughness
     :app currentItemRoughness :default 1 :label "Sloppiness"
     :types ("rectangle" "iframe" "embeddable" "ellipse" "diamond" "arrow" "line"
             "stickynote")
     :choices (("Architect" . 0) ("Artist" . 1) ("Cartoonist" . 2)))
    (roundness
     :app currentItemRoundness :default "round" :label "Edges"
     :types ("rectangle" "iframe" "embeddable" "line" "diamond" "stickynote" "image")
     :choices (("Sharp" . "sharp") ("Round" . "round")))
    (opacity
     :app currentItemOpacity :default 100 :label "Opacity" :types t)
    (fontFamily
     :app currentItemFontFamily :default 5 :label "Font" :types ("text")
     :choices (("Hand-drawn (Excalifont)" . 5) ("Normal (Nunito)" . 6)
               ("Code (Comic Shanns)" . 8) ("Lilita One" . 7)
               ("Liberation Sans" . 9) ("Assistant" . 10) ("Virgil" . 1)
               ("Helvetica" . 2) ("Cascadia" . 3)))
    (fontSize
     :app currentItemFontSize :default 20 :label "Font size" :types ("text")
     :choices (("Small" . 16) ("Medium" . 20) ("Large" . 28) ("Extra large" . 36)))
    (textAlign
     :app currentItemTextAlign :default "left" :label "Text align" :types ("text")
     :choices (("Left" . "left") ("Center" . "center") ("Right" . "right")))
    (verticalAlign
     ;; Only meaningful for text inside a shape (`shouldAllowVerticalAlign').
     :app currentItemVerticalAlign :default "top" :label "Vertical align"
     :types ("text")
     :choices (("Top" . "top") ("Middle" . "middle") ("Bottom" . "bottom")))
    (startArrowhead
     :app currentItemStartArrowhead :default nil :label "Start arrowhead"
     :types ("arrow" "line") :choices excali--arrowhead-choices)
    (endArrowhead
     :app currentItemEndArrowhead :default "arrow" :label "End arrowhead"
     :types ("arrow") :choices excali--arrowhead-choices)
    (arrowType
     :app currentItemArrowType :default "round" :label "Arrow type"
     :types ("arrow")
     :choices (("Sharp" . "sharp") ("Round" . "round") ("Elbow" . "elbow"))))
  "Style properties: element key and plist of metadata.
:app is the app-state key saved in .excalidraw files, :types the element
types the property applies to (t for all), :choices the offered values.")

(defconst excali--arrowhead-choices
  '(("None" . nil) ("Arrow" . "arrow") ("Bar" . "bar") ("Circle" . "circle")
    ("Circle outline" . "circle_outline") ("Triangle" . "triangle")
    ("Triangle outline" . "triangle_outline") ("Diamond" . "diamond")
    ("Diamond outline" . "diamond_outline")
    ("Cardinality: one" . "cardinality_one")
    ("Cardinality: many" . "cardinality_many")
    ("Cardinality: one or many" . "cardinality_one_or_many")
    ("Cardinality: exactly one" . "cardinality_exactly_one")
    ("Cardinality: zero or one" . "cardinality_zero_or_one")
    ("Cardinality: zero or many" . "cardinality_zero_or_many"))
  "Arrowhead values offered by the style panel.")

(defun excali--style-meta (property key)
  "Return metadata KEY of style PROPERTY."
  (plist-get (alist-get property excali-style-properties) key))

(defun excali--style-choices (property)
  "Return the (LABEL . VALUE) choices of PROPERTY."
  (let ((choices (excali--style-meta property :choices)))
    (if (symbolp choices) (symbol-value choices) choices)))

(defun excali--style-applies-p (property element)
  "Return non-nil if PROPERTY applies to ELEMENT."
  (let ((types (excali--style-meta property :types)))
    (or (eq types t) (member (excali--get element 'type) types))))

;;;; Current style

(defvar-local excali--current-style nil
  "Alist of style property values that new elements take.")

(defun excali--style-value (property)
  "Return the current value of style PROPERTY."
  (let ((cell (assq property excali--current-style)))
    (if cell (cdr cell) (excali--style-meta property :default))))

(defun excali--load-current-style (app-state)
  "Initialize the current style from APP-STATE, a .excalidraw alist."
  (setq excali--current-style
        (mapcar (lambda (entry)
                  (let* ((property (car entry))
                         (value (alist-get (plist-get (cdr entry) :app) app-state
                                           :missing)))
                    (cons property
                          (if (eq value :missing)
                              (plist-get (cdr entry) :default)
                            (unless (eq value :null) value)))))
                excali-style-properties)))

(defun excali--save-current-style (app-state)
  "Return APP-STATE with the current style stored in it.
A property is written only if APP-STATE already has it or it differs from
the default, so saving an untouched file leaves its app state as it was."
  (let ((state (copy-alist app-state)))
    (pcase-dolist (`(,property . ,meta) excali-style-properties)
      (let ((key (plist-get meta :app))
            (value (excali--style-value property)))
        (when (or (assq key state)
                  (not (equal value (plist-get meta :default))))
          (setf (alist-get key state) (or value :null)))))
    state))

(defun excali--roundness-type (type)
  "Return the roundness type an element of TYPE rounds with.
3 (adaptive radius) for rectangles, images and embeds; 2 (proportional
radius) for lines, arrows, diamonds and sticky notes."
  (if (member type '("rectangle" "embeddable" "iframe" "image")) 3 2))

(defun excali--roundness-for (type)
  "Return the JSON roundness a new element of TYPE gets, or :null.
Arrows follow the arrow type rather than the edges setting, as upstream."
  (if (if (equal type "arrow")
          (equal (excali--style-value 'arrowType) "round")
        (and (equal (excali--style-value 'roundness) "round")
             (excali--style-applies-p 'roundness (list (cons 'type type)))))
      (list (cons 'type (excali--roundness-type type)))
    :null))

(defun excali--stroke-width-for (type key)
  "Return the stroke width KEY stands for on an element of TYPE.
Freedraw strokes use a thinner scale (`FREEDRAW_STROKE_WIDTH')."
  (let ((table (if (equal type "freedraw")
                   '(("thin" . 0.5) ("medium" . 1) ("bold" . 2) ("extraBold" . 4))
                 '(("thin" . 1) ("medium" . 2) ("bold" . 4) ("extraBold" . 8)))))
    (or (cdr (assoc key table)) (cdr (assoc "medium" table)))))

(defun excali--element-stroke-width-key (element)
  "Return the stroke width key matching ELEMENT's width, or its number."
  (let* ((width (excali--get element 'strokeWidth))
         (type (excali--get element 'type)))
    (or (seq-find (lambda (key) (equal (excali--stroke-width-for type key) width))
                  '("thin" "medium" "bold" "extraBold"))
        width)))

(defun excali--apply-current-style (element)
  "Give the new ELEMENT the current style, where properties apply."
  (excali--put element 'roundness (excali--roundness-for (excali--get element 'type)))
  (pcase-dolist (`(,property . ,_) excali-style-properties)
    (when (and (not (memq property '(roundness arrowType)))
               (excali--style-applies-p property element))
      (excali--put element property
                  (if (eq property 'strokeWidth)
                      (excali--stroke-width-for (excali--get element 'type)
                                               (excali--style-value property))
                    (or (excali--style-value property) :null)))))
  (when (equal (excali--get element 'type) "text")
    (excali--measure-text element))
  element)

;;;; Setting properties

(defun excali--element-style-value (property element)
  "Return PROPERTY of ELEMENT in style-panel terms."
  (pcase property
    ('arrowType (cond ((excali--get element 'elbowed) "elbow")
                      ((excali--get element 'roundness) "round")
                      (t "sharp")))
    ('roundness (if (excali--get element 'roundness) "round" "sharp"))
    ('strokeWidth (excali--element-stroke-width-key element))
    (_ (excali--get element property))))

(defun excali--set-element-style (element property value)
  "Set PROPERTY of ELEMENT to VALUE and keep it consistent."
  (pcase property
    ('arrowType
     ;; Elbow arrows are converted and routed; see `excali--elbow-convert'.
     (excali--elbow-convert element (equal value "elbow"))
     (excali--put element 'roundness
                 (if (equal value "round")
                     (list (cons 'type (excali--roundness-type (excali--get element 'type))))
                   :null)))
    ('roundness
     (excali--put element 'roundness
                 (if (equal value "round")
                     (list (cons 'type (excali--roundness-type (excali--get element 'type))))
                   :null)))
    ('strokeWidth
     (excali--put element 'strokeWidth
                 (excali--stroke-width-for (excali--get element 'type) value)))
    (_ (excali--put element property (if (null value) :null value))))
  (when (equal (excali--get element 'type) "text")
    (pcase property
      ((or 'fontFamily 'fontSize) (excali--text-font-changed element property))
      ((or 'textAlign 'verticalAlign)
       (when (excali--container-of element) (excali--redraw-text element)))))
  (excali--touch element))

(defconst excali--text-style-properties
  '(fontFamily fontSize textAlign verticalAlign)
  "Properties that a selected shape passes on to its label.")

(defun excali--style-targets (property)
  "Return the elements PROPERTY changes: the selection, plus labels.
Text properties set on a selected shape apply to its bound text."
  (append excali--selection
          (and (memq property excali--text-style-properties)
               (delq nil (mapcar (lambda (e)
                                   (and (not (memq (excali--bound-text-of e)
                                                   excali--selection))
                                        (excali--bound-text-of e)))
                                 excali--selection)))))

(defun excali-set-style (property value)
  "Set style PROPERTY to VALUE for the selection and for new elements."
  (setf (alist-get property excali--current-style) value)
  (let ((changed nil))
    (dolist (e (excali--style-targets property))
      (when (excali--style-applies-p property e)
        (excali--set-element-style e property value)
        (setq changed t)))
    (when changed (excali--render))))

(defun excali--shown-style-value (property)
  "Return PROPERTY's value for display: the selection's, else the current one."
  (let ((values (delete-dups
                 (mapcar (lambda (e) (excali--element-style-value property e))
                         (seq-filter (lambda (e) (excali--style-applies-p property e))
                                     excali--selection)))))
    (cond ((cdr values) 'mixed)
          (values (car values))
          (t (excali--style-value property)))))

(defun excali--style-label (property value)
  "Return a short label for VALUE of PROPERTY."
  (cond ((eq value 'mixed) "mixed")
        ((car (rassoc value (excali--style-choices property))))
        ((null value) "none")
        (t (format "%s" value))))

(defun excali--read-style-value (property)
  "Read a value for PROPERTY in the minibuffer."
  (let* ((label (excali--style-meta property :label))
         (choices (excali--style-choices property))
         (current (excali--style-label property (excali--shown-style-value property))))
    (cond
     ((eq property 'opacity)
      (max 0 (min 100 (read-number (format "%s (0-100): " label)
                                   (excali--style-value property)))))
     ((memq property '(strokeColor backgroundColor))
      (let ((input (completing-read (format "%s (name or #rrggbb, now %s): "
                                            label current)
                                    (mapcar #'car choices))))
        (or (cdr (assoc input choices))
            (and (string-match-p "\\`#[0-9a-fA-F]\\{3,8\\}\\'" input) input)
            (user-error "Not a color: %s" input))))
     (t
      (cdr (assoc (completing-read (format "%s (now %s): " label current)
                                   (mapcar #'car choices) nil t)
                  choices))))))

(defmacro excali--define-style-command (property)
  "Define `excali-style-PROPERTY', a command that reads and sets PROPERTY."
  (let ((name (intern (format "excali-style-%s" property))))
    `(defun ,name (value)
       ,(format "Set style property `%s' of the selection and new elements to VALUE."
                property)
       (interactive (list (excali--read-style-value ',property)))
       (excali-set-style ',property value))))

(excali--define-style-command strokeColor)
(excali--define-style-command backgroundColor)
(excali--define-style-command fillStyle)
(excali--define-style-command strokeWidth)
(excali--define-style-command strokeStyle)
(excali--define-style-command roughness)
(excali--define-style-command roundness)
(excali--define-style-command opacity)
(excali--define-style-command fontFamily)
(excali--define-style-command fontSize)
(excali--define-style-command textAlign)
(excali--define-style-command verticalAlign)
(excali--define-style-command startArrowhead)
(excali--define-style-command endArrowhead)
(excali--define-style-command arrowType)

;;;; Panel

(defun excali--style-description (property)
  "Return the panel description for PROPERTY, showing its value."
  (format "%-16s %s" (excali--style-meta property :label)
          (propertize (excali--style-label property
                                          (excali--shown-style-value property))
                      'face 'transient-value)))

;;;###autoload (autoload 'excali-style "excali-style" nil t)
(transient-define-prefix excali-style ()
  "Change style properties of the selection and of new elements."
  [["Shape"
    ("s" excali-style-strokeColor :description (lambda () (excali--style-description 'strokeColor)) :transient t)
    ("b" excali-style-backgroundColor :description (lambda () (excali--style-description 'backgroundColor)) :transient t)
    ("f" excali-style-fillStyle :description (lambda () (excali--style-description 'fillStyle)) :transient t)
    ("w" excali-style-strokeWidth :description (lambda () (excali--style-description 'strokeWidth)) :transient t)
    ("d" excali-style-strokeStyle :description (lambda () (excali--style-description 'strokeStyle)) :transient t)
    ("r" excali-style-roughness :description (lambda () (excali--style-description 'roughness)) :transient t)
    ("e" excali-style-roundness :description (lambda () (excali--style-description 'roundness)) :transient t)
    ("o" excali-style-opacity :description (lambda () (excali--style-description 'opacity)) :transient t)]
   ["Text"
    ("F" excali-style-fontFamily :description (lambda () (excali--style-description 'fontFamily)) :transient t)
    ("z" excali-style-fontSize :description (lambda () (excali--style-description 'fontSize)) :transient t)
    ("a" excali-style-textAlign :description (lambda () (excali--style-description 'textAlign)) :transient t)
    ("A" excali-style-verticalAlign :description (lambda () (excali--style-description 'verticalAlign)) :transient t)]
   ["Arrow"
    ("<" excali-style-startArrowhead :description (lambda () (excali--style-description 'startArrowhead)) :transient t)
    (">" excali-style-endArrowhead :description (lambda () (excali--style-description 'endArrowhead)) :transient t)
    ("t" excali-style-arrowType :description (lambda () (excali--style-description 'arrowType)) :transient t)]])

(provide 'excali-style)
;;; excali-style.el ends here
