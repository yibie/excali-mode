;;; excal-style.el --- Style properties and the style panel  -*- lexical-binding: t; -*-

;;; Commentary:

;; Each buffer has a current style, Excalidraw's `currentItem*' app
;; state, which new elements take.  Setting a property changes the
;; current style and every selected element the property applies to.
;; `excal-style' is a transient panel over all properties.

;;; Code:

(require 'transient)
(require 'excal-core)
(require 'excal-view)
(require 'excal-select)

;;;; Properties

(defconst excal-style-properties
  '((strokeColor
     :app currentItemStrokeColor :default "#1e1e1e" :label "Stroke"
     :types t
     :choices (("Black" . "#1e1e1e") ("Red" . "#e03131") ("Green" . "#2f9e44")
               ("Blue" . "#1971c2") ("Orange" . "#f08c00")))
    (backgroundColor
     :app currentItemBackgroundColor :default "transparent" :label "Background"
     :types ("rectangle" "ellipse" "diamond" "line" "freedraw")
     :choices (("Transparent" . "transparent") ("Red" . "#ffc9c9")
               ("Green" . "#b2f2bb") ("Blue" . "#a5d8ff") ("Yellow" . "#ffec99")))
    (fillStyle
     :app currentItemFillStyle :default "solid" :label "Fill"
     :types ("rectangle" "ellipse" "diamond" "line" "freedraw")
     :choices (("Hachure" . "hachure") ("Cross-hatch" . "cross-hatch")
               ("Solid" . "solid") ("Zigzag" . "zigzag")))
    (strokeWidth
     :app currentItemStrokeWidth :default 2 :label "Stroke width"
     :types ("rectangle" "ellipse" "diamond" "line" "arrow" "freedraw")
     :choices (("Thin" . 1) ("Bold" . 2) ("Extra bold" . 4)))
    (strokeStyle
     :app currentItemStrokeStyle :default "solid" :label "Stroke style"
     :types ("rectangle" "ellipse" "diamond" "line" "arrow")
     :choices (("Solid" . "solid") ("Dashed" . "dashed") ("Dotted" . "dotted")))
    (roughness
     :app currentItemRoughness :default 1 :label "Sloppiness"
     :types ("rectangle" "ellipse" "diamond" "line" "arrow")
     :choices (("Architect" . 0) ("Artist" . 1) ("Cartoonist" . 2)))
    (roundness
     :app currentItemRoundness :default "round" :label "Edges"
     :types ("rectangle" "diamond" "line" "arrow")
     :choices (("Sharp" . "sharp") ("Round" . "round")))
    (opacity
     :app currentItemOpacity :default 100 :label "Opacity" :types t)
    (fontFamily
     :app currentItemFontFamily :default 5 :label "Font" :types ("text")
     :choices (("Hand-drawn (Excalifont)" . 5) ("Normal (Nunito)" . 6)
               ("Code (Comic Shanns)" . 8) ("Virgil" . 1) ("Helvetica" . 2)
               ("Cascadia" . 3)))
    (fontSize
     :app currentItemFontSize :default 20 :label "Font size" :types ("text")
     :choices (("Small" . 16) ("Medium" . 20) ("Large" . 28) ("Extra large" . 36)))
    (textAlign
     :app currentItemTextAlign :default "left" :label "Text align" :types ("text")
     :choices (("Left" . "left") ("Center" . "center") ("Right" . "right")))
    (startArrowhead
     :app currentItemStartArrowhead :default nil :label "Start arrowhead"
     :types ("arrow" "line") :choices excal--arrowhead-choices)
    (endArrowhead
     :app currentItemEndArrowhead :default "arrow" :label "End arrowhead"
     :types ("arrow") :choices excal--arrowhead-choices))
  "Style properties: element key and plist of metadata.
:app is the app-state key saved in .excalidraw files, :types the element
types the property applies to (t for all), :choices the offered values.")

(defconst excal--arrowhead-choices
  '(("None" . nil) ("Arrow" . "arrow") ("Bar" . "bar") ("Circle" . "circle")
    ("Circle outline" . "circle_outline") ("Triangle" . "triangle")
    ("Triangle outline" . "triangle_outline") ("Diamond" . "diamond")
    ("Diamond outline" . "diamond_outline"))
  "Arrowhead values offered by the style panel.")

(defun excal--style-meta (property key)
  "Return metadata KEY of style PROPERTY."
  (plist-get (alist-get property excal-style-properties) key))

(defun excal--style-choices (property)
  "Return the (LABEL . VALUE) choices of PROPERTY."
  (let ((choices (excal--style-meta property :choices)))
    (if (symbolp choices) (symbol-value choices) choices)))

(defun excal--style-applies-p (property element)
  "Return non-nil if PROPERTY applies to ELEMENT."
  (let ((types (excal--style-meta property :types)))
    (or (eq types t) (member (excal--get element 'type) types))))

;;;; Current style

(defvar-local excal--current-style nil
  "Alist of style property values that new elements take.")

(defun excal--style-value (property)
  "Return the current value of style PROPERTY."
  (let ((cell (assq property excal--current-style)))
    (if cell (cdr cell) (excal--style-meta property :default))))

(defun excal--load-current-style (app-state)
  "Initialize the current style from APP-STATE, a .excalidraw alist."
  (setq excal--current-style
        (mapcar (lambda (entry)
                  (let* ((property (car entry))
                         (value (alist-get (plist-get (cdr entry) :app) app-state
                                           :missing)))
                    (cons property
                          (if (eq value :missing)
                              (plist-get (cdr entry) :default)
                            (unless (eq value :null) value)))))
                excal-style-properties)))

(defun excal--save-current-style (app-state)
  "Return APP-STATE with the current style stored in it.
A property is written only if APP-STATE already has it or it differs from
the default, so saving an untouched file leaves its app state as it was."
  (let ((state (copy-alist app-state)))
    (pcase-dolist (`(,property . ,meta) excal-style-properties)
      (let ((key (plist-get meta :app))
            (value (excal--style-value property)))
        (when (or (assq key state)
                  (not (equal value (plist-get meta :default))))
          (setf (alist-get key state) (or value :null)))))
    state))

(defun excal--roundness-for (type)
  "Return the JSON roundness a new element of TYPE gets, or :null."
  (if (and (equal (excal--style-value 'roundness) "round")
           (excal--style-applies-p 'roundness (list (cons 'type type))))
      ;; 2 is proportional radius (linear elements and legacy shapes),
      ;; 3 adaptive radius (rectangles and diamonds).
      (list (cons 'type (if (member type '("line" "arrow")) 2 3)))
    :null))

(defun excal--apply-current-style (element)
  "Give the new ELEMENT the current style, where properties apply."
  (pcase-dolist (`(,property . ,_) excal-style-properties)
    (when (excal--style-applies-p property element)
      (pcase property
        ('roundness (excal--put element 'roundness
                                (excal--roundness-for (excal--get element 'type))))
        (_ (excal--put element property
                       (or (excal--style-value property) :null))))))
  (when (equal (excal--get element 'type) "text")
    (excal--measure-text element))
  element)

;;;; Setting properties

(defun excal--element-style-value (property element)
  "Return PROPERTY of ELEMENT in style-panel terms."
  (pcase property
    ('roundness (if (excal--get element 'roundness) "round" "sharp"))
    (_ (excal--get element property))))

(defun excal--set-element-style (element property value)
  "Set PROPERTY of ELEMENT to VALUE and keep it consistent."
  (pcase property
    ('roundness
     (excal--put element 'roundness
                 (if (equal value "round")
                     (list (cons 'type (if (member (excal--get element 'type)
                                                   '("line" "arrow"))
                                           2 3)))
                   :null)))
    (_ (excal--put element property (if (null value) :null value))))
  (when (and (equal (excal--get element 'type) "text")
             (memq property '(fontFamily fontSize)))
    (excal--measure-text element))
  (excal--touch element))

(defun excal-set-style (property value)
  "Set style PROPERTY to VALUE for the selection and for new elements."
  (setf (alist-get property excal--current-style) value)
  (let ((changed nil))
    (dolist (e excal--selection)
      (when (excal--style-applies-p property e)
        (excal--set-element-style e property value)
        (setq changed t)))
    (when changed (excal--render))))

(defun excal--shown-style-value (property)
  "Return PROPERTY's value for display: the selection's, else the current one."
  (let ((values (delete-dups
                 (mapcar (lambda (e) (excal--element-style-value property e))
                         (seq-filter (lambda (e) (excal--style-applies-p property e))
                                     excal--selection)))))
    (cond ((cdr values) 'mixed)
          (values (car values))
          (t (excal--style-value property)))))

(defun excal--style-label (property value)
  "Return a short label for VALUE of PROPERTY."
  (cond ((eq value 'mixed) "mixed")
        ((car (rassoc value (excal--style-choices property))))
        ((null value) "none")
        (t (format "%s" value))))

(defun excal--read-style-value (property)
  "Read a value for PROPERTY in the minibuffer."
  (let* ((label (excal--style-meta property :label))
         (choices (excal--style-choices property))
         (current (excal--style-label property (excal--shown-style-value property))))
    (cond
     ((eq property 'opacity)
      (max 0 (min 100 (read-number (format "%s (0-100): " label)
                                   (excal--style-value property)))))
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

(defmacro excal--define-style-command (property)
  "Define `excal-style-PROPERTY', a command that reads and sets PROPERTY."
  (let ((name (intern (format "excal-style-%s" property))))
    `(defun ,name (value)
       ,(format "Set style property `%s' of the selection and new elements to VALUE."
                property)
       (interactive (list (excal--read-style-value ',property)))
       (excal-set-style ',property value))))

(excal--define-style-command strokeColor)
(excal--define-style-command backgroundColor)
(excal--define-style-command fillStyle)
(excal--define-style-command strokeWidth)
(excal--define-style-command strokeStyle)
(excal--define-style-command roughness)
(excal--define-style-command roundness)
(excal--define-style-command opacity)
(excal--define-style-command fontFamily)
(excal--define-style-command fontSize)
(excal--define-style-command textAlign)
(excal--define-style-command startArrowhead)
(excal--define-style-command endArrowhead)

;;;; Panel

(defun excal--style-description (property)
  "Return the panel description for PROPERTY, showing its value."
  (format "%-16s %s" (excal--style-meta property :label)
          (propertize (excal--style-label property
                                          (excal--shown-style-value property))
                      'face 'transient-value)))

;;;###autoload (autoload 'excal-style "excal-style" nil t)
(transient-define-prefix excal-style ()
  "Change style properties of the selection and of new elements."
  [["Shape"
    ("s" excal-style-strokeColor :description (lambda () (excal--style-description 'strokeColor)) :transient t)
    ("b" excal-style-backgroundColor :description (lambda () (excal--style-description 'backgroundColor)) :transient t)
    ("f" excal-style-fillStyle :description (lambda () (excal--style-description 'fillStyle)) :transient t)
    ("w" excal-style-strokeWidth :description (lambda () (excal--style-description 'strokeWidth)) :transient t)
    ("d" excal-style-strokeStyle :description (lambda () (excal--style-description 'strokeStyle)) :transient t)
    ("r" excal-style-roughness :description (lambda () (excal--style-description 'roughness)) :transient t)
    ("e" excal-style-roundness :description (lambda () (excal--style-description 'roundness)) :transient t)
    ("o" excal-style-opacity :description (lambda () (excal--style-description 'opacity)) :transient t)]
   ["Text"
    ("F" excal-style-fontFamily :description (lambda () (excal--style-description 'fontFamily)) :transient t)
    ("z" excal-style-fontSize :description (lambda () (excal--style-description 'fontSize)) :transient t)
    ("a" excal-style-textAlign :description (lambda () (excal--style-description 'textAlign)) :transient t)]
   ["Arrow"
    ("<" excal-style-startArrowhead :description (lambda () (excal--style-description 'startArrowhead)) :transient t)
    (">" excal-style-endArrowhead :description (lambda () (excal--style-description 'endArrowhead)) :transient t)]])

(provide 'excal-style)
;;; excal-style.el ends here
