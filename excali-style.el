;;; excali-style.el --- Style properties and the style panel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Each buffer has a current style, Excalidraw's `currentItem*' app
;; state, which new elements take.  Setting a property changes the
;; current style and every selected element the property applies to.
;; `excali-style' is a transient panel laid out like upstream's
;; properties panel: it offers the properties the selection or the
;; drawing tool takes, and each opens a panel of its own, the colors a
;; picker like upstream's (fifteen colors on q..b, their shades on 1..5).

;;; Code:

(require 'transient)
(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-text)
(require 'excali-elbow)

;;;; Properties

(eval-and-compile
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
     :app currentItemOpacity :default 100 :label "Opacity" :types t
     ;; Upstream's slider: 0 to 100 in steps of 10.
     :choices (("0" . 0) ("10" . 10) ("20" . 20) ("30" . 30) ("40" . 40) ("50" . 50)
               ("60" . 60) ("70" . 70) ("80" . 80) ("90" . 90) ("100" . 100))
     :keys ["-" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"] :column 6)
    (fontFamily
     :app currentItemFontFamily :default 5 :label "Font family" :types ("text")
     :choices (("Hand-drawn" . 5) ("Normal" . 6) ("Code" . 8) ("Lilita One" . 7)
               ("Liberation Sans" . 9) ("Assistant" . 10) ("Virgil" . 1)
               ("Helvetica" . 2) ("Cascadia" . 3)))
    (fontSize
     :app currentItemFontSize :default 20 :label "Font size" :types ("text")
     :choices (("Small" . 16) ("Medium" . 20) ("Large" . 28) ("Very large" . 36)))
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
     :choices (("Sharp arrow" . "sharp") ("Curved arrow" . "round")
               ("Elbow arrow" . "elbow"))))
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
  "Arrowhead values offered by the style panel (`ARROWHEADS').")

(defun excali--style-meta (property key)
  "Return metadata KEY of style PROPERTY."
  (plist-get (alist-get property excali-style-properties) key))

(defun excali--style-choices (property)
  "Return the (LABEL . VALUE) choices of PROPERTY."
  (let ((choices (excali--style-meta property :choices)))
    (if (symbolp choices) (symbol-value choices) choices))))

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

;;;; Colors (colors.ts)

(eval-and-compile
  (defconst excali--color-palette
    '((gray "#f8f9fa" "#e9ecef" "#ced4da" "#868e96" "#343a40")
      (red "#fff5f5" "#ffc9c9" "#ff8787" "#fa5252" "#e03131")
      (pink "#fff0f6" "#fcc2d7" "#f783ac" "#e64980" "#c2255c")
      (grape "#f8f0fc" "#eebefa" "#da77f2" "#be4bdb" "#9c36b5")
      (violet "#f3f0ff" "#d0bfff" "#9775fa" "#7950f2" "#6741d9")
      (blue "#e7f5ff" "#a5d8ff" "#4dabf7" "#228be6" "#1971c2")
      (cyan "#e3fafc" "#99e9f2" "#3bc9db" "#15aabf" "#0c8599")
      (teal "#e6fcf5" "#96f2d7" "#38d9a9" "#12b886" "#099268")
      (green "#ebfbee" "#b2f2bb" "#69db7c" "#40c057" "#2f9e44")
      (yellow "#fff9db" "#ffec99" "#ffd43b" "#fab005" "#f08c00")
      (orange "#fff4e6" "#ffd8a8" "#ffa94d" "#fd7e14" "#e8590c")
      (bronze "#f8f1ee" "#eaddd7" "#d2bab0" "#a18072" "#846358"))
    "COLOR_PALETTE: five open-color shades (50/200/400/600/800) per family.")

  (defconst excali--color-picker-entries
    '(transparent white gray black bronze cyan blue violet grape pink
                  green teal yellow orange red)
    "The color picker's colors in upstream's order.
`DEFAULT_ELEMENT_STROKE_COLOR_PALETTE', the same for backgrounds.")

  (defconst excali--color-picker-keys
    ["q" "w" "e" "r" "t" "a" "s" "d" "f" "g" "z" "x" "c" "v" "b"]
    "Upstream's `colorPickerHotkeyBindings', row by row."))

(defconst excali--default-shade '((strokeColor . 4) (backgroundColor . 1))
  "Shade the picker starts from (`DEFAULT_ELEMENT_*_COLOR_INDEX').")

(defun excali--picker-color (entry shade)
  "Return the color picker ENTRY stands for at SHADE."
  (pcase entry
    ('transparent "transparent")
    ('white "#ffffff")
    ('black "#1e1e1e")
    (_ (nth shade (alist-get entry excali--color-palette)))))

(defun excali--color-family (color)
  "Return (FAMILY . SHADE) if COLOR is a palette shade, else nil."
  (and (stringp color)
       (cl-loop for (family . shades) in excali--color-palette
                for i = (cl-position (downcase color) shades :test #'equal)
                when i return (cons family i))))

(defun excali--picker-shade (property)
  "Return the shade the picker of color PROPERTY shows its families at.
That of the current color if it is a palette shade, else the default."
  (or (cdr (excali--color-family (excali--shown-style-value property)))
      (alist-get property excali--default-shade)))

(defun excali--swatch (color)
  "Return a small colored square showing COLOR."
  (cond ((equal color "transparent")
         (propertize "░░" 'face '(:foreground "#adb5bd")))
        ((and (stringp color) (string-prefix-p "#" color))
         (propertize "  " 'face (list :background color)))
        (t "  ")))

(defun excali--picker-description (property index)
  "Describe color picker entry INDEX of PROPERTY: swatch, name, mark."
  (let* ((entry (nth index excali--color-picker-entries))
         (color (excali--picker-color entry (excali--picker-shade property)))
         (current (equal color (excali--shown-style-value property))))
    (concat (excali--swatch color) " "
            (propertize (capitalize (symbol-name entry))
                        'face (if current 'transient-value 'default)))))

(defun excali--shade-description (property shade)
  "Describe SHADE of the current color family of PROPERTY."
  (let* ((family (car (excali--color-family (excali--shown-style-value property))))
         (color (and family (nth shade (alist-get family excali--color-palette)))))
    (cond (color
           (concat (excali--swatch color) " "
                   (propertize color 'face (if (equal color (excali--shown-style-value property))
                                               'transient-value 'default))))
          ((zerop shade) "(this color has no shades)")
          (t ""))))

(defun excali--pick-color (property index)
  "Set color PROPERTY to picker entry INDEX at the picker's shade."
  (excali-set-style property (excali--picker-color (nth index excali--color-picker-entries)
                                                  (excali--picker-shade property))))

(defun excali--pick-shade (property shade)
  "Set color PROPERTY to SHADE of its current family."
  (when-let* ((family (car (excali--color-family (excali--shown-style-value property)))))
    (excali-set-style property (nth shade (alist-get family excali--color-palette)))))

(defun excali--read-hex-color (property)
  "Read a hex color for PROPERTY."
  (let ((input (string-trim (read-string (format "%s (hex): "
                                                 (excali--style-meta property :label))
                                         (let ((v (excali--shown-style-value property)))
                                           (and (stringp v) (string-prefix-p "#" v) v))))))
    (unless (string-prefix-p "#" input) (setq input (concat "#" input)))
    (if (string-match-p "\\`#[0-9a-fA-F]\\{3\\}\\([0-9a-fA-F]\\{3\\}\\([0-9a-fA-F]\\{2\\}\\)?\\)?\\'" input)
        (downcase input)
      (user-error "Not a hex color: %s" input))))

(defun excali-style--done ()
  "Leave a style sub-panel, back to the style panel if it opened it."
  (interactive))

(defmacro excali--define-color-panel (property)
  "Define `excali-style-PROPERTY', a color picker panel like upstream's.
The fifteen colors take the keys q w e r t / a s d f g / z x c v b and
the shades of the current color 1 to 5, as upstream's picker does."
  (let* ((name (intern (format "excali-style-%s" property)))
         (pick (lambda (i) (intern (format "excali-style--%s-%s" property
                                           (nth i excali--color-picker-entries)))))
         (shade (lambda (k) (intern (format "excali-style--%s-shade-%d" property (1+ k)))))
         (hex (intern (format "excali-style--%s-hex" property)))
         (label (downcase (excali--style-meta property :label))))
    `(progn
       ,@(cl-loop for i below 15
                  collect `(defun ,(funcall pick i) ()
                             ,(format "Set the %s color to %s." label
                                      (nth i excali--color-picker-entries))
                             (interactive)
                             (excali--pick-color ',property ,i)))
       ,@(cl-loop for k below 5
                  collect `(defun ,(funcall shade k) ()
                             ,(format "Set the %s color to shade %d of its family." label (1+ k))
                             (interactive)
                             (excali--pick-shade ',property ,k)))
       (defun ,hex (color)
         ,(format "Set the %s color to the hex COLOR." label)
         (interactive (list (excali--read-hex-color ',property)))
         (excali-set-style ',property color))
       (transient-define-prefix ,name ()
         ,(format "Pick the %s color of the selection and of new elements." label)
         [:description (lambda () (excali--panel-title ',property))
          ,@(cl-loop for col below 5
                     collect (vconcat
                              (cl-loop for row below 3
                                       for i = (+ (* row 5) col)
                                       collect `(,(aref excali--color-picker-keys i)
                                                 ,(funcall pick i)
                                                 :description
                                                 (lambda () (excali--picker-description ',property ,i))
                                                 :transient t))))]
         [["Shades"
           ,@(cl-loop for k below 5
                      collect `(,(number-to-string (1+ k)) ,(funcall shade k)
                                :description (lambda () (excali--shade-description ',property ,k))
                                :inapt-if-not (lambda () (excali--color-family
                                                          (excali--shown-style-value ',property)))
                                :transient t))]
          [""
           ("#" "Hex code…" ,hex :transient t)
           ("RET" "Done" excali-style--done :transient transient--do-return)]]))))

(excali--define-color-panel strokeColor)
(excali--define-color-panel backgroundColor)

;;;; Choice panels

(defun excali--choice-description (property value label)
  "Describe choice LABEL, standing for VALUE of PROPERTY, marking the current one."
  (if (equal value (excali--shown-style-value property))
      (propertize (concat "● " label) 'face 'transient-value)
    (concat "  " label)))

(defun excali--panel-title (property)
  "Return a panel title for PROPERTY with its current value."
  (let ((value (excali--shown-style-value property)))
    (concat (propertize (excali--style-meta property :label) 'face 'transient-heading)
            "  "
            (propertize (excali--style-label property value) 'face 'transient-value)
            (if (and (memq property '(strokeColor backgroundColor)) (stringp value))
                (concat " " (excali--swatch value))
              ""))))

(eval-and-compile
  (defconst excali--choice-keys
    ["1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "a" "b" "c" "d" "e" "f" "g"]
    "Keys of the choices in a choice panel, in order."))

(defmacro excali--define-choice-panel (property &optional custom)
  "Define `excali-style-PROPERTY', a panel of PROPERTY's choices.
Choosing one sets it and returns to the style panel.  With CUSTOM, a
prompt that reads CUSTOM (a number) offers other values too."
  (let* ((name (intern (format "excali-style-%s" property)))
         (label (excali--style-meta property :label))
         (choices (excali--style-choices property))
         (keys (or (excali--style-meta property :keys) excali--choice-keys))
         (cmd (lambda (i) (intern (format "excali-style--%s-%d" property (1+ i)))))
         (other (intern (format "excali-style--%s-other" property)))
         (suffixes
          (cl-loop for (choice . value) in choices for i from 0
                   collect `(,(aref keys i) ,(funcall cmd i)
                             :description (lambda () (excali--choice-description
                                                      ',property ',value ,choice))
                             :transient transient--do-return)))
         (columns (seq-partition suffixes (or (excali--style-meta property :column) 8))))
    `(progn
       ,@(cl-loop for (choice . value) in choices for i from 0
                  collect `(defun ,(funcall cmd i) ()
                             ,(format "Set %s to %s." (downcase label) choice)
                             (interactive)
                             (excali-set-style ',property ',value)))
       ,@(when custom
           `((defun ,other (value)
               ,(format "Set %s to VALUE." (downcase label))
               (interactive (list (read-number ,(format "%s: " label)
                                               (excali--style-value ',property))))
               (excali-set-style ',property ,(if (eq custom 'percent)
                                                 '(max 0 (min 100 value))
                                               '(max 1 value))))))
       (transient-define-prefix ,name ()
         ,(format "Set %s of the selection and of new elements." (downcase label))
         [:description (lambda () (excali--panel-title ',property))
          ,@(mapcar #'vconcat columns)
          ,@(when custom
              `([("=" "Other…" ,other :transient transient--do-return)]))]))))

(excali--define-choice-panel fillStyle)
(excali--define-choice-panel strokeWidth)
(excali--define-choice-panel strokeStyle)
(excali--define-choice-panel roughness)
(excali--define-choice-panel roundness)
(excali--define-choice-panel arrowType)
(excali--define-choice-panel startArrowhead)
(excali--define-choice-panel endArrowhead)
(excali--define-choice-panel fontFamily)
(excali--define-choice-panel fontSize size)
(excali--define-choice-panel textAlign)
(excali--define-choice-panel verticalAlign)
(excali--define-choice-panel opacity percent)

;;;; The style panel

(declare-function excali--selection-units "excali-actions")
(declare-function excali-duplicate "excali-clipboard")
(declare-function excali-delete-selected "excali-edit")
(declare-function excali-group "excali-edit")
(declare-function excali-ungroup "excali-edit")
(declare-function excali-set-link "excali-erase")
(declare-function excali-bring-forward "excali-edit")
(declare-function excali-send-backward "excali-edit")
(declare-function excali-bring-to-front "excali-edit")
(declare-function excali-send-to-back "excali-edit")
(declare-function excali-align-left "excali-actions")
(declare-function excali-align-hcenter "excali-actions")
(declare-function excali-align-right "excali-actions")
(declare-function excali-align-top "excali-actions")
(declare-function excali-align-vcenter "excali-actions")
(declare-function excali-align-bottom "excali-actions")
(declare-function excali-distribute-horizontally "excali-actions")
(declare-function excali-distribute-vertically "excali-actions")

(defun excali--tool-element-type ()
  "Return the element type the current tool draws, or nil."
  (pcase excali--tool
    ((or 'rectangle 'ellipse 'diamond 'arrow 'line 'freedraw 'text 'stickynote 'frame)
     (symbol-name excali--tool))
    ('autoshape "freedraw")))

(defun excali--style-shown-p (property)
  "Return non-nil if the style panel offers PROPERTY now.
Upstream shows a property if the drawing tool or a selected element
takes it (`getShapeActionPredicates'); with nothing selected and no
drawing tool, every property is offered for new elements."
  (let ((type (excali--tool-element-type))
        (targets (excali--style-targets property)))
    (and (or (and (null targets) (null type))
             (and type (excali--style-applies-p property (list (cons 'type type))))
             (seq-some (lambda (e) (excali--style-applies-p property e)) targets))
         (pcase property
           ;; Fill only matters under a background.
           ('fillStyle (not (equal (excali--shown-style-value 'backgroundColor) "transparent")))
           ;; Vertical alignment only for text in a container.
           ('verticalAlign (or (null targets)
                               (seq-some (lambda (e) (and (equal (excali--get e 'type) "text")
                                                          (excali--container-of e)))
                                         targets)))
           (_ t)))))

(defun excali--style-description (property)
  "Return the panel description for PROPERTY, showing its value.
Colors show a swatch after their name."
  (let ((value (excali--shown-style-value property)))
    (concat (format "%-14s " (excali--style-meta property :label))
            (propertize (excali--style-label property value) 'face 'transient-value)
            (if (and (memq property '(strokeColor backgroundColor)) (stringp value))
                (concat " " (excali--swatch value))
              ""))))

(defun excali--units-selected-p (n)
  "Return non-nil if more than N units are selected."
  (> (length (excali--selection-units)) n))

;;;###autoload (autoload 'excali-style "excali-style" nil t)
(transient-define-prefix excali-style ()
  "Change the style of the selection and of new elements.
The properties follow upstream's properties panel, in its order; each
opens a panel of its values."
  [["Style"
    ("s" excali-style-strokeColor
     :description (lambda () (excali--style-description 'strokeColor))
     :if (lambda () (excali--style-shown-p 'strokeColor)) :transient transient--do-recurse)
    ("b" excali-style-backgroundColor
     :description (lambda () (excali--style-description 'backgroundColor))
     :if (lambda () (excali--style-shown-p 'backgroundColor)) :transient transient--do-recurse)
    ("f" excali-style-fillStyle
     :description (lambda () (excali--style-description 'fillStyle))
     :if (lambda () (excali--style-shown-p 'fillStyle)) :transient transient--do-recurse)
    ("w" excali-style-strokeWidth
     :description (lambda () (excali--style-description 'strokeWidth))
     :if (lambda () (excali--style-shown-p 'strokeWidth)) :transient transient--do-recurse)
    ("d" excali-style-strokeStyle
     :description (lambda () (excali--style-description 'strokeStyle))
     :if (lambda () (excali--style-shown-p 'strokeStyle)) :transient transient--do-recurse)
    ("r" excali-style-roughness
     :description (lambda () (excali--style-description 'roughness))
     :if (lambda () (excali--style-shown-p 'roughness)) :transient transient--do-recurse)
    ("e" excali-style-roundness
     :description (lambda () (excali--style-description 'roundness))
     :if (lambda () (excali--style-shown-p 'roundness)) :transient transient--do-recurse)
    ("o" excali-style-opacity
     :description (lambda () (excali--style-description 'opacity))
     :if (lambda () (excali--style-shown-p 'opacity)) :transient transient--do-recurse)]
   ["Arrow"
    :if (lambda () (seq-some #'excali--style-shown-p '(arrowType startArrowhead endArrowhead)))
    ("t" excali-style-arrowType
     :description (lambda () (excali--style-description 'arrowType))
     :if (lambda () (excali--style-shown-p 'arrowType)) :transient transient--do-recurse)
    ("<" excali-style-startArrowhead
     :description (lambda () (excali--style-description 'startArrowhead))
     :if (lambda () (excali--style-shown-p 'startArrowhead)) :transient transient--do-recurse)
    (">" excali-style-endArrowhead
     :description (lambda () (excali--style-description 'endArrowhead))
     :if (lambda () (excali--style-shown-p 'endArrowhead)) :transient transient--do-recurse)]
   ["Text"
    :if (lambda () (excali--style-shown-p 'fontFamily))
    ("F" excali-style-fontFamily
     :description (lambda () (excali--style-description 'fontFamily))
     :transient transient--do-recurse)
    ("z" excali-style-fontSize
     :description (lambda () (excali--style-description 'fontSize))
     :transient transient--do-recurse)
    ("a" excali-style-textAlign
     :description (lambda () (excali--style-description 'textAlign))
     :transient transient--do-recurse)
    ("A" excali-style-verticalAlign
     :description (lambda () (excali--style-description 'verticalAlign))
     :if (lambda () (excali--style-shown-p 'verticalAlign)) :transient transient--do-recurse)]]
  [:if (lambda () excali--selection)
   ["Layers"
    ("{" "Send to back" excali-send-to-back :transient t)
    ("[" "Send backward" excali-send-backward :transient t)
    ("]" "Bring forward" excali-bring-forward :transient t)
    ("}" "Bring to front" excali-bring-to-front :transient t)]
   ["Align"
    :if (lambda () (excali--units-selected-p 1))
    ("L" "Left" excali-align-left :transient t)
    ("C" "Center" excali-align-hcenter :transient t)
    ("R" "Right" excali-align-right :transient t)
    ("T" "Top" excali-align-top :transient t)
    ("M" "Middle" excali-align-vcenter :transient t)
    ("B" "Bottom" excali-align-bottom :transient t)
    ("H" "Distribute horizontally" excali-distribute-horizontally
     :if (lambda () (excali--units-selected-p 2)) :transient t)
    ("V" "Distribute vertically" excali-distribute-vertically
     :if (lambda () (excali--units-selected-p 2)) :transient t)]
   ["Actions"
    ("D" "Duplicate" excali-duplicate :transient t)
    ("X" "Delete" excali-delete-selected)
    ("g" "Group" excali-group :if (lambda () (excali--units-selected-p 1)) :transient t)
    ("G" "Ungroup" excali-ungroup :transient t)
    ("k" "Link…" excali-set-link)]])

(provide 'excali-style)
;;; excali-style.el ends here
