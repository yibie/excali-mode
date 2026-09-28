;;; excal-text.el --- Text layout: fonts, measurement, wrapping, bound text  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Excalidraw's text layout, ported from packages/common/src/font-metadata.ts
;; and packages/element/src/{textMeasurements,textWrapping,textElement,
;; linearElementEditor,stickyNote}.ts.  Layout decisions are made here;
;; the module only measures and draws single lines (excal-text.c).
;;
;; Fonts: files in `excal-fonts-directory' are registered with the font
;; backend when this file loads; `excal-font-families' chooses the Pango
;; family list per font id.
;;
;; API for other parts of excal (all mutate in place and `excal--touch'):
;;
;; - `excal--bound-text-of' CONTAINER, `excal--container-of' TEXT.
;; - `excal--refresh-bound-text' CONTAINER: re-wrap, grow and reposition
;;   CONTAINER's label.  Call after moving a container, editing an
;;   arrow's points, or any other container change.
;; - `excal--layout-bound-text' CONTAINER &optional HANDLE ...: the same
;;   after a resize by HANDLE (upstream `handleBindTextResize').
;; - `excal--redraw-text' TEXT: re-wrap and re-measure TEXT after its
;;   text or font changed, growing and following its container.
;; - `excal--text-scale' TEXT SCALE (corner resize: font scales),
;;   `excal--text-set-width' TEXT WIDTH (side resize: re-wrap),
;;   `excal--text-reset-auto-resize' TEXT.
;; - `excal--add-bound-text' CONTAINER: create an empty label.
;; - `excal--arrow-label-hole' ARROW: the rectangle cut out of an arrow
;;   behind its label; passed to the module as text extras.

;;; Code:

(require 'excal-core)
(require 'ucs-normalize)

(declare-function excal-native-text-width "excal-module")
(declare-function excal-native-add-fonts "excal-module")
(declare-function excal-native-set-font-family "excal-module")
(declare-function excal-native-font-family "excal-module")
(declare-function excal-native-font-resolve "excal-module")
(declare-function excal-native-font-backend "excal-module")

;;;; Constants (packages/common/src/constants.ts)

(defconst excal-font-family-ids
  '(("Virgil" . 1) ("Helvetica" . 2) ("Cascadia" . 3) ("Excalifont" . 5)
    ("Nunito" . 6) ("Lilita One" . 7) ("Comic Shanns" . 8)
    ("Liberation Sans" . 9) ("Assistant" . 10))
  "Excalidraw `FONT_FAMILY': family name to numeric id.")

(defconst excal-default-font-family 5 "`DEFAULT_FONT_FAMILY' (Excalifont).")
(defconst excal-default-font-size 20 "`DEFAULT_FONT_SIZE'.")
(defconst excal-min-font-size 1 "`MIN_FONT_SIZE'.")
(defconst excal-bound-text-padding 5 "`BOUND_TEXT_PADDING'.")
(defconst excal-arrow-label-width-fraction 0.7 "`ARROW_LABEL_WIDTH_FRACTION'.")
(defconst excal-arrow-label-font-size-to-min-width-ratio 11
  "`ARROW_LABEL_FONT_SIZE_TO_MIN_WIDTH_RATIO'.")
(defconst excal-text-autowrap-threshold 36 "`TEXT_AUTOWRAP_THRESHOLD'.")
(defconst excal-sticky-note-padding 16 "`STICKY_NOTE_PADDING'.")
(defconst excal-sticky-note-body-inset-y 52 "`STICKY_NOTE_BODY_INSET_Y'.")
(defconst excal-sticky-note-min-size 75 "`STICKY_NOTE_MIN_SIZE'.")
(defconst excal-sticky-note-min-font-size 16 "`STICKY_NOTE_MIN_FONT_SIZE'.")
(defconst excal-sticky-note-max-font-size 512 "`STICKY_NOTE_MAX_FONT_SIZE'.")
(defconst excal-sticky-note-fallback-font-size 28
  "`STICKY_NOTE_FALLBACK_FONT_SIZE'.")
(defconst excal-sticky-note-font-step 2 "`STICKY_NOTE_FONT_STEP'.")

(defconst excal--font-metadata
  ;; id unitsPerEm ascender descender lineHeight
  '((5 1000 886 -374 1.25)              ; Excalifont
    (6 1000 1011 -353 1.25)             ; Nunito
    (7 1000 923 -220 1.15)              ; Lilita One
    (8 1000 750 -250 1.25)              ; Comic Shanns
    (1 1000 886 -374 1.25)              ; Virgil
    (2 2048 1577 -471 1.15)             ; Helvetica
    (3 2048 1900 -480 1.2)              ; Cascadia
    (9 2048 1854 -434 1.15)             ; Liberation Sans
    (10 2048 1021 -287 1.25)            ; Assistant
    (100 1000 880 -144 1.25)            ; Xiaolai
    (1000 1000 886 -374 1.25))          ; Segoe UI Emoji
  "Excalidraw `FONT_METADATA' metrics, keyed by font id.")

(defun excal--font-metrics (family)
  "Return (UNITS-PER-EM ASCENDER DESCENDER LINE-HEIGHT) of font id FAMILY.
Unknown ids use Excalifont's metrics, as upstream."
  (cdr (or (assq family excal--font-metadata)
           (assq excal-default-font-family excal--font-metadata))))

(defun excal--line-height (family)
  "Return the default unitless line height of font id FAMILY (`getLineHeight')."
  (nth 3 (excal--font-metrics family)))

(defun excal--vertical-offset (family font-size line-height-px)
  "Return the first baseline's distance below the text top (`getVerticalOffset')."
  (pcase-let* ((`(,units ,ascender ,descender ,_) (excal--font-metrics family))
               (em (/ (float font-size) units))
               (gap (/ (+ (- line-height-px (* em ascender)) (* em descender)) 2)))
    (+ (* em ascender) gap)))

;;;; Fonts

(defgroup excal-text nil
  "Text and fonts in excal."
  :group 'excal)

(defcustom excal-fonts-directory (expand-file-name "fonts" excal--directory)
  "Directory whose font files are registered when excal loads.
`make fonts' downloads Excalidraw's fonts here."
  :type 'directory)

(defvar excal--line-width-cache (make-hash-table :test #'equal)
  "Line widths keyed by (LINE FONT-SIZE FONT-FAMILY).")

(defvar excal--char-width-cache (make-hash-table :test #'equal)
  "Upstream `charWidth' cache: widths keyed by (FONT-SIZE FONT-FAMILY CODE).")

(defun excal--apply-font-families (families)
  "Send FAMILIES, an alist (ID . PANGO-FAMILY-LIST), to the module."
  (dolist (entry families)
    (excal-native-set-font-family (car entry) (cdr entry)))
  (clrhash excal--line-width-cache)
  (clrhash excal--char-width-cache))

(defcustom excal-font-families nil
  "Pango family lists overriding the built-in ones, as (ID . FAMILIES).
ID is an Excalidraw font id (see `excal-font-family-ids'; 100 is the
Xiaolai CJK fallback, 1000 Segoe UI Emoji) and FAMILIES a
comma-separated Pango family list such as
\"Excalifont, Xiaolai, LXGW WenKai, sans-serif\".  Metrics
(line height, baseline) always follow the element's font id."
  :type '(alist :key-type integer :value-type string)
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'excal-native-set-font-family)
           (excal--apply-font-families value))))

(defun excal-register-fonts (&optional directory)
  "Register the font files in DIRECTORY with the font backend.
DIRECTORY defaults to `excal-fonts-directory'.  Return the number of
files registered."
  (interactive)
  (let* ((dir (or directory excal-fonts-directory))
         (count (or (and (file-directory-p dir) (excal-native-add-fonts dir)) 0)))
    (when (> count 0)
      (clrhash excal--line-width-cache)
      (clrhash excal--char-width-cache))
    (when (called-interactively-p 'interactive)
      (message "Registered %d font files from %s" count dir))
    count))

(defun excal-font-report ()
  "Show which fonts render each Excalidraw font family."
  (interactive)
  (with-help-window "*excal fonts*"
    (princ (format "Pango font map: %s\nFonts directory: %s\n\n"
                   (excal-native-font-backend) excal-fonts-directory))
    (dolist (entry (append excal-font-family-ids '(("Xiaolai" . 100))))
      (princ (format "%-16s %4d  %s\n%22s Latin: %s  CJK: %s\n"
                     (car entry) (cdr entry)
                     (excal-native-font-family (cdr entry)) ""
                     (excal-native-font-resolve "Hello" (cdr entry))
                     (excal-native-font-resolve "你好" (cdr entry)))))))

(excal-register-fonts)
(excal--apply-font-families excal-font-families)

;;;; Measurement (textMeasurements.ts)

(defun excal--normalize-text (text)
  "Normalize line ends to \\n and tabs to 8 spaces (`normalizeText')."
  (string-replace "\t" "        "
                  (replace-regexp-in-string "\r\n?" "\n" text t t)))

(defun excal--line-width (line font-size family)
  "Return the advance width of LINE (`getLineWidth')."
  (if (string-empty-p line)
      0.0
    (let ((key (list line font-size family)))
      (or (gethash key excal--line-width-cache)
          (progn
            (when (> (hash-table-count excal--line-width-cache) 20000)
              (clrhash excal--line-width-cache))
            (puthash key (excal-native-text-width line font-size family)
                     excal--line-width-cache))))))

(defun excal--char-width (char font-size family)
  "Return the width of CHAR through upstream's `charWidth' cache.
Like upstream, the cache is keyed by the first UTF-16 code unit, so
characters outside the BMP share entries within a surrogate block."
  (let* ((unit (if (> char #xFFFF)
                   (+ #xD800 (ash (- char #x10000) -10))
                 char))
         (key (list font-size family unit))
         (width (gethash key excal--char-width-cache)))
    (if (and width (/= width 0))
        width
      (puthash key (excal--line-width (string char) font-size family)
               excal--char-width-cache))))

(defun excal--measure-string (text font-size family line-height)
  "Return (WIDTH . HEIGHT) of TEXT as upstream `measureText'.
Empty lines count as a space; height is lines * size * line height."
  (let ((lines (split-string (excal--normalize-text text) "\n")))
    (cons (float (apply #'max (mapcar (lambda (line)
                                        (excal--line-width
                                         (if (string-empty-p line) " " line)
                                         font-size family))
                                      lines)))
          (* (length lines) font-size line-height))))

(defun excal--text-font (element)
  "Return (FONT-SIZE FAMILY LINE-HEIGHT) of text ELEMENT."
  (let ((family (or (excal--get element 'fontFamily) excal-default-font-family)))
    (list (or (excal--get element 'fontSize) excal-default-font-size)
          family
          (or (excal--get element 'lineHeight) (excal--line-height family)))))

(defun excal--auto-resize-p (element)
  "Return non-nil if text ELEMENT sizes itself to its text.
A missing `autoResize' counts as true, as upstream restore does."
  (let ((cell (assq 'autoResize element)))
    (or (null cell) (not (memq (cdr cell) '(nil :false :null))))))

(defun excal--min-text-width (font-size family line-height)
  "Return `getMinTextElementWidth': a space plus twice the padding."
  (+ (car (excal--measure-string "" font-size family line-height))
     (* 2 excal-bound-text-padding)))

;;;; Wrapping (textWrapping.ts)

;; Character classes of the line-break rules, as bits.
(defconst excal--wrap-ws 1)          ; COMMON.WHITESPACE, JS \s
(defconst excal--wrap-hyphen 2)      ; COMMON.HYPHEN
(defconst excal--wrap-open 4)        ; COMMON.OPENING
(defconst excal--wrap-close 8)       ; COMMON.CLOSING
(defconst excal--wrap-cjk 16)        ; CJK.CHAR
(defconst excal--wrap-cjk-open 32)   ; CJK.OPENING
(defconst excal--wrap-cjk-close 64)  ; CJK.CLOSING
(defconst excal--wrap-currency 128)  ; CJK.CURRENCY
(defconst excal--wrap-emoji-most 256) ; EMOJI.MOST
(defconst excal--wrap-emoji-any 512)  ; EMOJI.ANY
(defconst excal--wrap-emoji-mod 1024) ; \p{Emoji_Modifier}
(defconst excal--wrap-ri 2048)        ; \p{RI}

(defconst excal--extended-pictographic
  '(#xA9 #xAE #x203C #x2049 #x2122 #x2139 (#x2194 . #x2199) (#x21A9 . #x21AA)
    (#x231A . #x231B) #x2328 #x2388 #x23CF (#x23E9 . #x23F3) (#x23F8 . #x23FA)
    #x24C2 (#x25AA . #x25AB) #x25B6 #x25C0 (#x25FB . #x25FE) (#x2600 . #x2605)
    (#x2607 . #x2612) (#x2614 . #x2685) (#x2690 . #x2705) (#x2708 . #x2712)
    #x2714 #x2716 #x271D #x2721 #x2728 (#x2733 . #x2734) #x2744 #x2747 #x274C
    #x274E (#x2753 . #x2755) #x2757 (#x2763 . #x2767) (#x2795 . #x2797) #x27A1
    #x27B0 #x27BF (#x2934 . #x2935) (#x2B05 . #x2B07) (#x2B1B . #x2B1C) #x2B50
    #x2B55 #x3030 #x303D #x3297 #x3299 (#x1F000 . #x1F0FF) (#x1F10D . #x1F10F)
    #x1F12F (#x1F16C . #x1F171) (#x1F17E . #x1F17F) #x1F18E (#x1F191 . #x1F19A)
    (#x1F1AD . #x1F1E5) (#x1F201 . #x1F20F) #x1F21A #x1F22F (#x1F232 . #x1F23A)
    (#x1F23C . #x1F23F) (#x1F249 . #x1F3FA) (#x1F400 . #x1F53D)
    (#x1F546 . #x1F64F) (#x1F680 . #x1F6FF) (#x1F774 . #x1F77F)
    (#x1F7D5 . #x1F7FF) (#x1F80C . #x1F80F) (#x1F848 . #x1F84F)
    (#x1F85A . #x1F85F) (#x1F888 . #x1F88F) (#x1F8AE . #x1F8FF)
    (#x1F90C . #x1F93A) (#x1F93C . #x1F945) (#x1F947 . #x1FAFF)
    (#x1FC00 . #x1FFFD))
  "Extended_Pictographic code points (emoji-data.txt, Unicode 15).")

(defconst excal--cjk-scripts
  '(;; Han
    (#x2E80 . #x2E99) (#x2E9B . #x2EF3) (#x2F00 . #x2FD5) #x3005 #x3007
    (#x3021 . #x3029) (#x3038 . #x303B) (#x3400 . #x4DBF) (#x4E00 . #x9FFF)
    (#xF900 . #xFA6D) (#xFA70 . #xFAD9) (#x16FF0 . #x16FF1)
    (#x20000 . #x2A6DF) (#x2A700 . #x2EBEF) (#x2F800 . #x2FA1F)
    (#x30000 . #x323AF)
    ;; Hiragana
    (#x3041 . #x3096) (#x309D . #x309F) (#x1B001 . #x1B11F) #x1F200
    ;; Katakana
    (#x30A1 . #x30FA) (#x30FD . #x30FF) (#x31F0 . #x31FF) (#x32D0 . #x32FE)
    (#x3300 . #x3357) (#xFF66 . #xFF6F) (#xFF71 . #xFF9D) #x1B000
    (#x1AFF0 . #x1AFFE) (#x1B120 . #x1B122) (#x1B164 . #x1B167)
    ;; Hangul
    (#x1100 . #x11FF) (#x302E . #x302F) (#x3131 . #x318E) (#x3200 . #x321E)
    (#x3260 . #x327E) (#xA960 . #xA97C) (#xAC00 . #xD7A3) (#xD7B0 . #xD7C6)
    (#xD7CB . #xD7FB) (#xFFA0 . #xFFBE) (#xFFC2 . #xFFC7) (#xFFCA . #xFFCF)
    (#xFFD2 . #xFFD7) (#xFFDA . #xFFDC))
  "Code points of scripts Han, Hiragana, Katakana and Hangul.")

(defconst excal--wrap-classes (make-char-table 'excal-wrap 0)
  "Char table of `excal--wrap-*' class bits; see `excal--init-wrap-classes'.")

(defun excal--add-wrap-class (chars bit)
  "Add class BIT to CHARS: a string, or a list of chars and (FROM . TO)."
  (dolist (c (if (stringp chars) (string-to-list chars) chars))
    (if (consp c)
        (let ((i (car c)))
          (while (<= i (cdr c))
            (aset excal--wrap-classes i (logior (aref excal--wrap-classes i) bit))
            (setq i (1+ i))))
      (aset excal--wrap-classes c (logior (aref excal--wrap-classes c) bit)))))

(defun excal--init-wrap-classes ()
  "Fill `excal--wrap-classes' from the upstream character classes."
  (excal--add-wrap-class
   '(9 10 11 12 13 32 #xA0 #x1680 (#x2000 . #x200A) #x2028 #x2029 #x202F
       #x205F #x3000 #xFEFF)
   excal--wrap-ws)
  (excal--add-wrap-class "-" excal--wrap-hyphen)
  (excal--add-wrap-class "<([{" excal--wrap-open)
  (excal--add-wrap-class ">)]}.,:;!?…/" excal--wrap-close)
  (excal--add-wrap-class excal--cjk-scripts excal--wrap-cjk)
  (excal--add-wrap-class "｀＇＾〃〰〆＃＆＊＋－ー／＼＝｜￤〒￢￣" excal--wrap-cjk)
  (excal--add-wrap-class "（［｛〈《｟｢「『【〖〔〘〚＜〝" excal--wrap-cjk-open)
  (excal--add-wrap-class "）］｝〉》｠｣」』】〗〕〙〛＞。．，、〟‥？！：；・〜〞"
                         excal--wrap-cjk-close)
  (excal--add-wrap-class "￥￦￡￠＄" excal--wrap-currency)
  (let ((most (append excal--extended-pictographic
                      '((#x1F1E6 . #x1F1FF) (#x1F3FB . #x1F3FF)))))
    (excal--add-wrap-class most excal--wrap-emoji-most)
    ;; \p{Emoji}: the pictographs plus keycap bases.
    (excal--add-wrap-class (append most '(?# ?* (?0 . ?9))) excal--wrap-emoji-any))
  (excal--add-wrap-class '((#x1F3FB . #x1F3FF)) excal--wrap-emoji-mod)
  (excal--add-wrap-class '((#x1F1E6 . #x1F1FF)) excal--wrap-ri))

(excal--init-wrap-classes)

(defsubst excal--wrap-class (char)
  "Return the class bits of CHAR."
  (aref excal--wrap-classes char))

(defsubst excal--wrap-is (char bits)
  "Return non-nil if CHAR has any of the class BITS."
  (/= 0 (logand (aref excal--wrap-classes char) bits)))

(defun excal--emoji-joiner (string i)
  "Return the end of EMOJI.JOINER matched in STRING at I (I if none)."
  (let ((n (length string)))
    (cond
     ((>= i n) i)
     ((excal--wrap-is (aref string i) excal--wrap-emoji-mod) (1+ i))
     ((= (aref string i) #xFE0F)
      (if (and (< (1+ i) n) (= (aref string (1+ i)) #x20E3)) (+ i 2) (1+ i)))
     ((<= #xE0020 (aref string i) #xE007E)
      (let ((j i))
        (while (and (< j n) (<= #xE0020 (aref string j) #xE007E))
          (setq j (1+ j)))
        (if (and (< j n) (= (aref string j) #xE007F)) (1+ j) i)))
     (t i))))

(defun excal--emoji-at (string i)
  "Return the end of the emoji regex match in STRING at I, or nil."
  (let ((n (length string)))
    (cond
     ((and (< (1+ i) n)
           (excal--wrap-is (aref string i) excal--wrap-ri)
           (excal--wrap-is (aref string (1+ i)) excal--wrap-ri))
      (+ i 2))
     ((excal--wrap-is (aref string i) excal--wrap-emoji-most)
      (let ((j (excal--emoji-joiner string (1+ i))) (done nil))
        (while (and (not done) (< (1+ j) n) (= (aref string j) #x200D))
          (cond
           ((and (< (+ j 2) n)
                 (excal--wrap-is (aref string (1+ j)) excal--wrap-ri)
                 (excal--wrap-is (aref string (+ j 2)) excal--wrap-ri))
            (setq j (+ j 3)))
           ((excal--wrap-is (aref string (1+ j)) excal--wrap-emoji-any)
            (setq j (excal--emoji-joiner string (+ j 2))))
           (t (setq done t))))
        j))
     (t nil))))

(defun excal--break-between-p (string i)
  "Return non-nil if the advanced break regex matches STRING at I.
I is between the characters at I-1 and I; only zero-width rules apply."
  (let* ((a (excal--wrap-class (aref string (1- i))))
         (b (excal--wrap-class (aref string i)))
         (a-in (lambda (bits) (/= 0 (logand a bits))))
         (b-in (lambda (bits) (/= 0 (logand b bits)))))
    (or
     ;; Break.Before(WHITESPACE)
     (funcall b-in excal--wrap-ws)
     ;; Break.After(WHITESPACE, HYPHEN)
     (funcall a-in (logior excal--wrap-ws excal--wrap-hyphen))
     ;; Break.Before(CJK.CHAR, CJK.CURRENCY).NotPrecededBy(OPENING, CJK.OPENING)
     (and (funcall b-in (logior excal--wrap-cjk excal--wrap-currency))
          (not (funcall a-in (logior excal--wrap-open excal--wrap-cjk-open))))
     ;; Break.After(CJK.CHAR).NotFollowedBy(HYPHEN, CLOSING, CJK.CLOSING)
     (and (funcall a-in excal--wrap-cjk)
          (not (funcall b-in (logior excal--wrap-hyphen excal--wrap-close
                                     excal--wrap-cjk-close))))
     ;; Break.BeforeMany(CJK.OPENING).NotPrecededBy(OPENING)
     (and (funcall b-in excal--wrap-cjk-open)
          (not (funcall a-in (logior excal--wrap-cjk-open excal--wrap-open))))
     ;; Break.AfterMany(CJK.CLOSING).NotFollowedBy(CLOSING)
     (and (funcall a-in excal--wrap-cjk-close)
          (not (funcall b-in (logior excal--wrap-cjk-close excal--wrap-close))))
     ;; Break.AfterMany(CLOSING).FollowedBy(OPENING)
     (and (funcall a-in excal--wrap-close)
          (not (funcall b-in excal--wrap-close))
          (funcall b-in excal--wrap-open)))))

(defun excal--tokenize (line)
  "Split LINE into breakable tokens (upstream `parseTokens').
LINE is NFC-normalized first, then split like
`line.split(breakLineRegex).filter(Boolean)': emoji sequences are
tokens of their own, and zero-width rules break between characters."
  (let* ((s (ucs-normalize-NFC-string line))
         (n (length s)) (start 0) (i 0) tokens)
    (while (< i n)
      (let ((emoji (excal--emoji-at s i)))
        (cond
         (emoji
          (when (> i start) (push (substring s start i) tokens))
          (push (substring s i emoji) tokens)
          (setq start emoji i emoji))
         (t
          (when (and (> i start) (excal--break-between-p s i))
            (push (substring s start i) tokens)
            (setq start i))
          (setq i (1+ i))))))
    (when (> n start) (push (substring s start) tokens))
    (nreverse tokens)))

(defconst excal--js-space-regexp
  "[\t\n\v\f\r    -     　﻿]"
  "JavaScript's \\s.")

(defun excal--trim-end (string)
  "Return STRING without trailing JavaScript whitespace (`trimEnd')."
  (if (string-match (concat excal--js-space-regexp "+\\'") string)
      (substring string 0 (match-beginning 0))
    string))

(defun excal--single-character-p (token)
  "Return non-nil if TOKEN is one UTF-16 code unit (`isSingleCharacter')."
  (and (= (length token) 1) (< (aref token 0) #x10000)))

(defun excal--wrap-word (word size family max-width)
  "Split WORD into lines of at most MAX-WIDTH (upstream `wrapWord')."
  (if (excal--emoji-at word 0)
      ;; Emoji sequences are atomic.
      (list word)
    (let ((lines nil) (current "") (width 0))
      (dolist (char (string-to-list word))
        (let* ((w (excal--char-width char size family))
               (test (+ width w)))
          (if (<= test max-width)
              (setq current (concat current (string char)) width test)
            (unless (string-empty-p current) (push current lines))
            (setq current (string char) width w))))
      (unless (string-empty-p current) (push current lines))
      (nreverse lines))))

(defun excal--trim-line (line size family max-width)
  "Trim trailing whitespace of LINE beyond MAX-WIDTH (upstream `trimLine')."
  (if (<= (excal--line-width line size family) max-width)
      line
    (pcase-let* ((`(,trimmed ,spaces)
                  (if (string-match (concat "\\`\\(.+?\\)\\(" excal--js-space-regexp
                                            "+\\)\\'")
                                    line)
                      (list (match-string 1 line) (match-string 2 line))
                    (list (excal--trim-end line) "")))
                 (width (excal--line-width trimmed size family)))
      (catch 'full
        (dolist (char (string-to-list spaces))
          (let ((test (+ width (excal--char-width char size family))))
            (when (> test max-width) (throw 'full nil))
            (setq trimmed (concat trimmed (string char)) width test))))
      trimmed)))

(defun excal--wrap-line (line size family max-width)
  "Wrap the hard LINE into lines of at most MAX-WIDTH (upstream `wrapLine')."
  (let ((tokens (excal--tokenize line))
        (lines nil) (current "") (width 0))
    (while tokens
      (let* ((token (car tokens))
             (test-line (concat current token))
             (test-width (if (excal--single-character-p token)
                             (+ width (excal--char-width (aref token 0) size family))
                           (excal--line-width test-line size family))))
        (cond
         ;; Build up the line; whitespace never breaks it here.
         ((or (string-match-p excal--js-space-regexp token)
              (<= test-width max-width))
          (setq current test-line width test-width tokens (cdr tokens)))
         ;; The word alone is too long: break it into characters.
         ((string-empty-p current)
          (let ((pieces (excal--wrap-word token size family max-width)))
            (dolist (piece (butlast pieces)) (push piece lines))
            (setq current (or (car (last pieces)) "")
                  width (excal--line-width current size family)
                  tokens (cdr tokens))))
         ;; Start a new line with this token.
         (t
          (push (excal--trim-end current) lines)
          (setq current "" width 0)))))
    (unless (string-empty-p current)
      (push (excal--trim-line current size family max-width) lines))
    (nreverse lines)))

(defun excal--wrap-text (text size family max-width)
  "Wrap TEXT to MAX-WIDTH as upstream `wrapText' and return the result.
Hard line breaks are kept; a hard line is only wrapped when wider than
MAX-WIDTH.  A non-finite or negative MAX-WIDTH leaves TEXT unwrapped."
  (if (or (not (numberp max-width)) (isnan (float max-width))
          (= (abs max-width) 1.0e+INF) (< max-width 0))
      text
    (mapconcat
     (lambda (line)
       (if (<= (excal--line-width line size family) max-width)
           line
         (mapconcat #'identity (excal--wrap-line line size family max-width) "\n")))
     (split-string text "\n") "\n")))

;;;; Containers (textElement.ts)

(defconst excal--text-container-types
  '("rectangle" "stickynote" "ellipse" "diamond" "arrow")
  "Upstream `VALID_CONTAINER_TYPES'.")

(defun excal--text-container-p (element)
  "Return non-nil if ELEMENT can hold bound text (`isValidTextContainer')."
  (and element (member (excal--get element 'type) excal--text-container-types) t))

(defun excal--bound-text-id (container)
  "Return the id of CONTAINER's bound text, or nil (`getBoundTextElementId')."
  (let ((bound (excal--get container 'boundElements)))
    (and (sequencep bound)
         (alist-get 'id (seq-find (lambda (b) (equal (alist-get 'type b) "text"))
                                  bound)))))

(defun excal--bound-text-of (container)
  "Return CONTAINER's live bound text element, or nil."
  (when-let* ((id (excal--bound-text-id container))
              (text (excal--element-by-id id)))
    (unless (excal--get text 'isDeleted) text)))

(defun excal--container-of (text)
  "Return the live container of TEXT, or nil."
  (when-let* ((container (excal--element-by-id (excal--get text 'containerId))))
    (unless (excal--get container 'isDeleted) container)))

(defun excal--arrow-p (element)
  "Return non-nil if ELEMENT is an arrow."
  (equal (excal--get element 'type) "arrow"))

(defun excal--sticky-note-p (element)
  "Return non-nil if ELEMENT is a sticky note."
  (equal (excal--get element 'type) "stickynote"))

(defun excal--bound-text-max-width (container &optional text)
  "Return the widest text CONTAINER fits (`getBoundTextMaxWidth')."
  (let ((width (excal--get container 'width))
        (pad excal-bound-text-padding))
    (pcase (excal--get container 'type)
      ("arrow"
       (max (* excal-arrow-label-width-fraction width)
            (* (or (and text (excal--get text 'fontSize)) excal-default-font-size)
               excal-arrow-label-font-size-to-min-width-ratio)))
      ("ellipse" (- (round (* (/ width 2.0) (sqrt 2))) (* 2 pad)))
      ("diamond" (- (round (/ width 2.0)) (* 2 pad)))
      ("stickynote" (- width (* 2 excal-sticky-note-padding)))
      (_ (- width (* 2 pad))))))

(defun excal--bound-text-max-height (container text)
  "Return the tallest TEXT CONTAINER fits (`getBoundTextMaxHeight')."
  (let ((height (excal--get container 'height))
        (pad excal-bound-text-padding))
    (pcase (excal--get container 'type)
      ("stickynote" (max 0 (- height excal-sticky-note-body-inset-y)))
      ("arrow" (if (<= (- height (* pad 8 2)) 0) (excal--get text 'height) height))
      ("ellipse" (- (round (* (/ height 2.0) (sqrt 2))) (* 2 pad)))
      ("diamond" (- (round (/ height 2.0)) (* 2 pad)))
      (_ (- height (* 2 pad))))))

(defun excal--container-coords (container)
  "Return (X . Y), the top-left of CONTAINER's text box (`getContainerCoords')."
  (let* ((pad (if (excal--sticky-note-p container)
                  excal-sticky-note-padding
                excal-bound-text-padding))
         (w (excal--get container 'width)) (h (excal--get container 'height))
         (ox pad) (oy pad))
    (pcase (excal--get container 'type)
      ("ellipse" (setq ox (+ ox (* (/ w 2.0) (- 1 (/ (sqrt 2) 2))))
                       oy (+ oy (* (/ h 2.0) (- 1 (/ (sqrt 2) 2))))))
      ("diamond" (setq ox (+ ox (/ w 4.0)) oy (+ oy (/ h 4.0)))))
    (cons (+ (excal--get container 'x) ox) (+ (excal--get container 'y) oy))))

(defun excal--container-dimension-for-text (dimension type)
  "Return the container size fitting DIMENSION of text in container TYPE.
Upstream `computeContainerDimensionForBoundText'."
  (let ((dim (ceiling dimension)) (pad (* 2 excal-bound-text-padding)))
    (pcase type
      ("ellipse" (round (* (/ (+ dim pad) (sqrt 2)) 2)))
      ("arrow" (+ dim (* pad 8)))
      ("diamond" (* 2 (+ dim pad)))
      (_ (+ dim pad)))))

(defun excal--bound-text-position (container text)
  "Return (X . Y) for TEXT inside CONTAINER (`computeBoundTextPosition')."
  (if (excal--arrow-p container)
      (excal--arrow-label-position container text)
    (pcase-let* ((`(,cx . ,cy) (excal--container-coords container))
                 (max-h (excal--bound-text-max-height container text))
                 (max-w (excal--bound-text-max-width container text))
                 (tw (excal--get text 'width)) (th (excal--get text 'height))
                 (y (pcase (excal--get text 'verticalAlign)
                      ("top" cy)
                      ("bottom" (+ cy (- max-h th)))
                      (_ (if (excal--sticky-note-p container)
                             (+ cy (min (/ (- (excal--get container 'height)
                                              (* 2 excal-sticky-note-padding)
                                              th)
                                           2.0)
                                        (- max-h th)))
                           (+ cy (- (/ max-h 2.0) (/ th 2.0)))))))
                 (x (pcase (excal--get text 'textAlign)
                      ("left" cx)
                      ("right" (+ cx (- max-w tw)))
                      (_ (+ cx (- (/ max-w 2.0) (/ tw 2.0))))))
                 (angle (or (excal--get container 'angle) 0)))
      (if (= angle 0)
          (cons (float x) (float y))
        (pcase-let* ((`(,ccx . ,ccy)
                      (if (excal--sticky-note-p container)
                          (cons (+ (excal--get container 'x)
                                   (/ (excal--get container 'width) 2.0))
                                (+ (excal--get container 'y)
                                   (/ (excal--get container 'height) 2.0)))
                        (cons (+ cx (/ max-w 2.0)) (+ cy (/ max-h 2.0)))))
                     (`(,rx . ,ry) (excal--rotate-point (cons (+ x (/ tw 2.0)) (+ y (/ th 2.0)))
                                                        (cons ccx ccy) angle)))
          (cons (- rx (/ tw 2.0)) (- ry (/ th 2.0))))))))

(defun excal--container-center (container)
  "Return (X . Y) where new text in CONTAINER is anchored (`getContainerCenter')."
  (if (excal--arrow-p container)
      (excal--arrow-label-center container)
    (cons (+ (excal--get container 'x) (/ (excal--get container 'width) 2.0))
          (+ (excal--get container 'y) (/ (excal--get container 'height) 2.0)))))

(defun excal--position-after-height-change (container height anchor)
  "Return CONTAINER's y after changing its height to HEIGHT keeping ANCHOR.
ANCHOR is `top', `bottom' or `center' (the edge that stays put)."
  (let ((y (excal--get container 'y)) (dh (- height (excal--get container 'height))))
    (pcase anchor
      ('bottom (- y dh))
      ('center (- y (/ dh 2.0)))
      (_ y))))

(defun excal--invalidate-native (element)
  "Drop ELEMENT's cached native vector so it is rebuilt."
  (when (hash-table-p excal--native-cache)
    (remhash element excal--native-cache)))

(defun excal--redraw-text (text &optional container)
  "Re-wrap, measure and place TEXT (upstream `redrawTextBoundingBox').
CONTAINER defaults to TEXT's container.  Bound text wraps to the
container, which grows (never shrinks) to fit; free text with
`autoResize' false wraps to its width; other text keeps its lines and
takes the measured width.  Return TEXT."
  (let ((container (or container (excal--container-of text))))
    (if (and container (excal--sticky-note-p container))
        (excal--update-sticky-note-layout container text)
      (pcase-let* ((`(,size ,family ,lh) (excal--text-font text))
                   (auto (excal--auto-resize-p text))
                   (original (or (excal--get text 'originalText)
                                 (excal--get text 'text) ""))
                   (lines (if (or container (not auto))
                              (excal--wrap-text
                               original size family
                               (if container
                                   (excal--bound-text-max-width container text)
                                 (excal--get text 'width)))
                            (or (excal--get text 'text) "")))
                   (`(,w . ,h) (excal--measure-string lines size family lh)))
        (excal--put text 'text lines)
        (when (or auto (null (excal--get text 'width)))
          (excal--put text 'width w))
        (excal--put text 'height h)
        (when container
          (excal--put text 'angle (if (excal--arrow-p container)
                                      0
                                    (or (excal--get container 'angle) 0)))
          (let ((changed nil))
            (when (and (not (excal--arrow-p container))
                       (> h (excal--bound-text-max-height container text)))
              (excal--put container 'height
                          (float (excal--container-dimension-for-text
                                  h (excal--get container 'type))))
              (setq changed t))
            (when (> w (excal--bound-text-max-width container text))
              (excal--put container 'width
                          (float (excal--container-dimension-for-text
                                  w (excal--get container 'type))))
              (setq changed t))
            (if changed (excal--touch container) (excal--invalidate-native container)))
          (pcase-let ((`(,x . ,y) (excal--bound-text-position container text)))
            (excal--put text 'x x)
            (excal--put text 'y y)))
        (excal--touch text))))
  text)

(defun excal--refresh-bound-text (container)
  "Re-layout CONTAINER's bound text after CONTAINER changed.
Call after moving a container, changing an arrow's points, or any other
container change; see also `excal--layout-bound-text' for resizes."
  (when-let* ((text (excal--bound-text-of container)))
    (excal--redraw-text text container)))

(defun excal--layout-bound-text (container &optional handle keep-aspect
                                           from-center flip-y)
  "Refit CONTAINER's bound text after resizing CONTAINER by HANDLE.
HANDLE is a symbol such as `n', `se' or `e' (nil re-wraps as a corner
would).  KEEP-ASPECT, FROM-CENTER and FLIP-Y mirror upstream
`handleBindTextResize': a pure `n'/`s' drag without KEEP-ASPECT keeps
the lines; otherwise the text is re-wrapped.  If the text no longer fits
vertically the container grows, anchored at the edge opposite HANDLE."
  (if (excal--sticky-note-p container)
      (when-let* ((text (excal--bound-text-of container)))
        (excal--update-sticky-note-layout container text))
    (when-let* ((text (excal--bound-text-of container))
                ((not (string-empty-p (or (excal--get text 'text) "")))))
      (pcase-let* ((`(,size ,family ,lh) (excal--text-font text))
                   (lines (excal--get text 'text))
                   (w (excal--get text 'width)) (h (excal--get text 'height))
                   (max-w (excal--bound-text-max-width container text))
                   (max-h (excal--bound-text-max-height container text)))
        (when (or keep-aspect (not (memq handle '(n s))))
          (setq lines (excal--wrap-text (or (excal--get text 'originalText) lines)
                                        size family max-w))
          (pcase-let ((`(,mw . ,mh) (excal--measure-string lines size family lh)))
            (setq w mw h mh)))
        (when (> h max-h)
          (let* ((height (float (excal--container-dimension-for-text
                                 h (excal--get container 'type))))
                 (from-top (not (eq (and (memq handle '(n ne nw)) t)
                                    (and flip-y t)))))
            (unless (excal--arrow-p container)
              (excal--put container 'y
                          (float (excal--position-after-height-change
                                  container height
                                  (cond (from-center 'center)
                                        (from-top 'bottom)
                                        (t 'top))))))
            (excal--put container 'height height)
            (excal--touch container)))
        (excal--put text 'text lines)
        (excal--put text 'width w)
        (excal--put text 'height h)
        (unless (excal--arrow-p container)
          (pcase-let ((`(,x . ,y) (excal--bound-text-position container text)))
            (excal--put text 'x x)
            (excal--put text 'y y)))
        (excal--invalidate-native container)
        (excal--touch text)))))

;;;; Arrow labels (linearElementEditor.ts)

(defun excal--linear-global-points (element)
  "Return ELEMENT's points in scene coordinates, rotated by its angle."
  (let* ((x (excal--get element 'x)) (y (excal--get element 'y))
         (points (append (excal--get element 'points) nil))
         (angle (or (excal--get element 'angle) 0))
         (abs (mapcar (lambda (p) (cons (+ x (aref p 0)) (+ y (aref p 1)))) points)))
    (if (or (= angle 0) (null abs))
        abs
      (let* ((xs (mapcar #'car abs)) (ys (mapcar #'cdr abs))
             (cx (/ (+ (apply #'min xs) (apply #'max xs)) 2.0))
             (cy (/ (+ (apply #'min ys) (apply #'max ys)) 2.0)))
        (mapcar (lambda (p) (excal--rotate-point p (cons cx cy) angle)) abs)))))

(defun excal--curved-p (element)
  "Return non-nil if linear ELEMENT is drawn as a curve."
  (and (excal--get element 'roundness)
       (not (excal--get element 'elbowed))
       (> (length (excal--get element 'points)) 2)))

(defun excal--bezier-point (seg u)
  "Return the point at parameter U of cubic SEG (P0 C1 C2 P1)."
  (pcase-let ((`(,p0 ,c1 ,c2 ,p1) seg))
    (let* ((v (- 1 u))
           (a (* v v v)) (b (* 3 v v u)) (c (* 3 v u u)) (d (* u u u)))
      (cons (+ (* a (car p0)) (* b (car c1)) (* c (car c2)) (* d (car p1)))
            (+ (* a (cdr p0)) (* b (cdr c1)) (* c (cdr c2)) (* d (cdr p1)))))))

(defun excal--path-segments (element)
  "Return ELEMENT's path as segments: (A B) lines or (P0 C1 C2 P1) curves.
Curves use roughjs's `curve' construction (tightness 0) without jitter."
  (let ((pts (vconcat (excal--linear-global-points element))))
    (if (not (excal--curved-p element))
        (cl-loop for i from 0 below (1- (length pts))
                 collect (list (aref pts i) (aref pts (1+ i))))
      (let ((n (length pts)))
        (cl-loop for i from 0 below (1- n)
                 collect
                 (let* ((p0 (aref pts (max 0 (1- i)))) (p1 (aref pts i))
                        (p2 (aref pts (1+ i))) (p3 (aref pts (min (1- n) (+ i 2)))))
                   (list p1
                         (cons (+ (car p1) (/ (- (car p2) (car p0)) 6.0))
                               (+ (cdr p1) (/ (- (cdr p2) (cdr p0)) 6.0)))
                         (cons (- (car p2) (/ (- (car p3) (car p1)) 6.0))
                               (- (cdr p2) (/ (- (cdr p3) (cdr p1)) 6.0)))
                         p2)))))))

(defconst excal--curve-samples 32 "Samples used to measure curve arc length.")

(defun excal--segment-samples (seg)
  "Return SEG as a list of (LENGTH-SO-FAR . POINT), starting at 0."
  (if (= (length seg) 2)
      (list (cons 0.0 (car seg))
            (cons (excal--distance (car seg) (cadr seg)) (cadr seg)))
    (let ((prev (car seg)) (len 0.0) (out (list (cons 0.0 (car seg)))))
      (dotimes (k excal--curve-samples)
        (let ((p (excal--bezier-point seg (/ (1+ k) (float excal--curve-samples)))))
          (setq len (+ len (excal--distance prev p)) prev p)
          (push (cons len p) out)))
      (nreverse out))))

(defun excal--distance (a b)
  "Return the distance between points A and B."
  (sqrt (+ (expt (- (car b) (car a)) 2) (expt (- (cdr b) (cdr a)) 2))))

(defun excal--samples-point-at (samples fraction)
  "Return the point at FRACTION of the arc length of SAMPLES."
  (let* ((total (car (car (last samples))))
         (target (* fraction total))
         (prev (car samples)))
    (catch 'found
      (dolist (s (cdr samples))
        (when (<= target (car s))
          (let* ((span (- (car s) (car prev)))
                 (u (if (> span 0) (/ (- target (car prev)) span) 0.0)))
            (throw 'found (cons (+ (car (cdr prev)) (* u (- (car (cdr s)) (car (cdr prev)))))
                                (+ (cdr (cdr prev)) (* u (- (cdr (cdr s)) (cdr (cdr prev)))))))))
        (setq prev s))
      (cdr prev))))

(defun excal--arrow-label-center (arrow)
  "Return (X . Y), the default label center of ARROW (`getBoundTextElementCenter')."
  (let* ((points (excal--linear-global-points arrow)) (n (length points)))
    (cond
     ((= n 0) (cons (excal--get arrow 'x) (excal--get arrow 'y)))
     ((cl-oddp n) (nth (/ n 2) points))
     (t
      (let ((seg (nth (1- (/ n 2)) (excal--path-segments arrow))))
        (if (or (excal--get arrow 'elbowed) (= (length seg) 2))
            (let ((a (car seg)) (b (car (last seg))))
              (cons (/ (+ (car a) (car b)) 2.0) (/ (+ (cdr a) (cdr b)) 2.0)))
          (excal--samples-point-at (excal--segment-samples seg) 0.5)))))))

(defun excal--path-point-at (arrow parameter)
  "Return the point at arc-length PARAMETER (0..1) along ARROW, or nil.
Upstream `getPointAtPathParameter'."
  (let* ((segments (excal--path-segments arrow))
         (samples (mapcar #'excal--segment-samples segments))
         (lengths (mapcar (lambda (s) (car (car (last s)))) samples))
         (total (apply #'+ 0.0 lengths))
         (target (* (min 1.0 (max 0.0 parameter)) total))
         (sum 0.0) (i 0))
    (when segments
      (while (and (< i (1- (length segments)))
                  (> target (+ sum (nth i lengths))))
        (setq sum (+ sum (nth i lengths)) i (1+ i)))
      (let ((len (nth i lengths)))
        (excal--samples-point-at
         (nth i samples)
         (if (= len 0) 0.0 (min 1.0 (max 0.0 (/ (- target sum) len)))))))))

(defun excal--arrow-label-position (arrow text)
  "Return (X . Y) of label TEXT on ARROW (`computeBoundTextElementPosition')."
  (let* ((w (excal--get text 'width)) (h (excal--get text 'height))
         (position (excal--get text 'labelPosition))
         (point (cond
                 ((< (length (excal--get arrow 'points)) 2) nil)
                 ((numberp position) (or (excal--path-point-at arrow position)
                                         (excal--arrow-label-center arrow)))
                 (t (excal--arrow-label-center arrow)))))
    (if point
        (cons (- (car point) (/ w 2.0)) (- (cdr point) (/ h 2.0)))
      (cons (excal--get text 'x) (excal--get text 'y)))))

(defun excal--label-position-at (arrow point)
  "Return the `labelPosition' (0..1) of the path point of ARROW nearest POINT.
Upstream `handleBoundTextDragging'; set it on the label and call
`excal--refresh-bound-text' to move a label along its arrow."
  (let* ((samples (mapcar #'excal--segment-samples (excal--path-segments arrow)))
         (total 0.0) (best nil) (best-d 1.0e+INF))
    (dolist (seg samples)
      (let ((prev (car seg)))
        (dolist (s (cdr seg))
          ;; Nearest point on the chord PREV..S.
          (let* ((a (cdr prev)) (b (cdr s))
                 (dx (- (car b) (car a))) (dy (- (cdr b) (cdr a)))
                 (l2 (+ (* dx dx) (* dy dy)))
                 (u (if (> l2 0)
                        (min 1.0 (max 0.0 (/ (+ (* (- (car point) (car a)) dx)
                                                (* (- (cdr point) (cdr a)) dy))
                                             l2)))
                      0.0))
                 (q (cons (+ (car a) (* u dx)) (+ (cdr a) (* u dy))))
                 (d (excal--distance q point)))
            (when (< d best-d)
              (setq best-d d
                    best (+ total (car prev) (* u (- (car s) (car prev)))))))
          (setq prev s))
        (setq total (+ total (car (car (last seg)))))))
    (if (and best (> total 0)) (min 1.0 (max 0.0 (/ best total))) 0.5)))

(defun excal--arrow-label-hole (arrow)
  "Return [X Y W H], the part of ARROW cut out behind its label, or nil.
It is the label box grown by `excal-bound-text-padding' (renderElement.ts)."
  (when-let* (((excal--arrow-p arrow))
              (text (excal--bound-text-of arrow))
              ((not (string-empty-p (or (excal--get text 'text) "")))))
    (let ((pad excal-bound-text-padding))
      (vector (float (- (excal--get text 'x) pad)) (float (- (excal--get text 'y) pad))
              (float (+ (excal--get text 'width) (* 2 pad)))
              (float (+ (excal--get text 'height) (* 2 pad)))))))

;;;; Sticky notes (stickyNote.ts)

(defun excal--normalize-sticky-font-size (size)
  "Clamp SIZE for sticky notes (`normalizeStickyNoteFontSize')."
  (if (not (and (numberp size) (not (isnan (float size)))
                (/= (abs size) 1.0e+INF)))
      excal-sticky-note-fallback-font-size
    (min excal-sticky-note-max-font-size (max excal-min-font-size size))))

(defun excal--fit-sticky-font (fit base min-size max-w max-h warm-start)
  "Pick the largest font size on the sticky grid that FIT reports fitting.
Upstream `fitStickyNoteFont'; FIT maps a size to (TEXT SIZE W H)."
  (let* ((steps (max 0 (ceiling (/ (- base min-size) (float excal-sticky-note-font-step)))))
         (cache (make-hash-table))
         (at (lambda (i)
               (or (gethash i cache)
                   (puthash i (funcall fit (if (>= i steps) min-size
                                             (- base (* i excal-sticky-note-font-step))))
                            cache))))
         (fits (lambda (i) (let ((r (funcall at i)))
                             (and (<= (nth 2 r) max-w) (<= (nth 3 r) max-h))))))
    (if (= steps 0)
        (funcall at 0)
      (let ((warm (min steps (max 0 (round (/ (- base warm-start)
                                                (float excal-sticky-note-font-step))))))
            lo hi)
        (catch 'done
          (if (funcall fits warm)
              (if (or (= warm 0) (not (funcall fits (1- warm))))
                  (throw 'done (funcall at warm))
                (setq lo 0 hi (1- warm)))
            (when (= warm steps) (throw 'done (funcall at steps)))
            (setq lo (1+ warm) hi steps))
          (while (< lo hi)
            (let ((mid (ash (+ lo hi) -1)))
              (if (funcall fits mid) (setq hi mid) (setq lo (1+ mid)))))
          (funcall at lo))))))

(defun excal--update-sticky-note-layout (container text)
  "Fit TEXT's font into sticky note CONTAINER, growing it if needed.
Upstream `updateStickyNoteLayout' with the default top anchor."
  (let* ((base-w (max (excal--get container 'width) excal-sticky-note-min-size))
         (base-h (max (or (excal--get container 'baseHeight)
                          (excal--get container 'height))
                      excal-sticky-note-min-size))
         (original (or (excal--get text 'originalText) ""))
         (base-size (excal--normalize-sticky-font-size
                     (or (excal--get text 'baseFontSize) (excal--get text 'fontSize))))
         (min-size (min excal-sticky-note-min-font-size base-size))
         (max-w (max (- base-w (* 2 excal-sticky-note-padding)) 1))
         (max-h (max (- base-h excal-sticky-note-body-inset-y) 0))
         (family (or (excal--get text 'fontFamily) excal-default-font-family))
         (lh (or (excal--get text 'lineHeight) (excal--line-height family)))
         (fit (lambda (size)
                (let* ((lines (excal--wrap-text original size family max-w))
                       (m (excal--measure-string lines size family lh)))
                  (list lines size (car m) (cdr m)))))
         (blank (string-empty-p (string-trim original)))
         (fitted (if blank
                     (let ((m (excal--measure-string "" base-size family lh)))
                       (list "" base-size (car m) (cdr m)))
                   (excal--fit-sticky-font fit base-size min-size max-w max-h
                                           (excal--get text 'fontSize))))
         (height (if blank base-h
                   (max base-h (+ (nth 3 fitted) excal-sticky-note-body-inset-y)))))
    (excal--put container 'width (float base-w))
    (excal--put container 'height (float height))
    (excal--put container 'baseHeight base-h)
    (excal--touch container)
    (excal--put text 'text (nth 0 fitted))
    (excal--put text 'fontSize (nth 1 fitted))
    (excal--put text 'baseFontSize base-size)
    (excal--put text 'width (nth 2 fitted))
    (excal--put text 'height (nth 3 fitted))
    (excal--put text 'angle (or (excal--get container 'angle) 0))
    (pcase-let ((`(,x . ,y) (excal--bound-text-position container text)))
      (excal--put text 'x x)
      (excal--put text 'y y))
    (excal--touch text)))

;;;; Text elements

(defun excal--text-anchor-ratios (text-align vertical-align)
  "Return (AX . AY) anchor ratios (`getTextAnchorRatios')."
  (cons (pcase text-align ("center" 0.5) ("right" 1.0) (_ 0.0))
        (pcase vertical-align ("middle" 0.5) ("bottom" 1.0) (_ 0.0))))

(defun excal--make-text-element (x y text &rest props)
  "Return a new text element showing TEXT anchored at X, Y.
PROPS is an alist of field overrides such as fontSize, fontFamily,
textAlign, verticalAlign or containerId, as for `excal--make-element'.
Like upstream `newTextElement', X, Y is the alignment anchor: the
top-left corner for left/top aligned text."
  (let* ((element (apply #'excal--make-element "text" x y
                         (cons 'text "") (cons 'fontSize excal-default-font-size)
                         (cons 'fontFamily excal-default-font-family)
                         (cons 'textAlign "left") (cons 'verticalAlign "top")
                         (cons 'containerId :null) (cons 'originalText "")
                         (cons 'autoResize t) (cons 'lineHeight 1.25)
                         (cons 'baseFontSize :null) (cons 'labelPosition :null)
                         props))
         (family (excal--get element 'fontFamily))
         (normalized (excal--normalize-text text)))
    (unless (assq 'lineHeight props)
      (excal--put element 'lineHeight (excal--line-height family)))
    (excal--put element 'text normalized)
    (unless (assq 'originalText props)
      (excal--put element 'originalText normalized))
    (pcase-let* ((`(,size ,fam ,lh) (excal--text-font element))
                 (`(,w . ,h) (excal--measure-string normalized size fam lh))
                 (`(,ax . ,ay) (excal--text-anchor-ratios
                                (excal--get element 'textAlign)
                                (excal--get element 'verticalAlign))))
      (excal--put element 'width w)
      (excal--put element 'height h)
      (excal--put element 'x (float (- x (* w ax))))
      (excal--put element 'y (float (- y (* h ay)))))
    element))

(defun excal--adjust-xy-with-rotation (sides x y angle dx1 dy1 dx2 dy2)
  "Upstream `adjustXYWithRotation' for SIDES, a list of `n' `s' `e' `w'."
  (let ((c (cos angle)) (s (sin angle)))
    (cond
     ((and (memq 'e sides) (memq 'w sides)) (setq x (+ x dx1 dx2)))
     ((memq 'e sides)
      (setq x (+ x (* dx1 (+ 1 c)) (* dx2 (- 1 c)))
            y (+ y (* dx1 s) (* dx2 (- s)))))
     ((memq 'w sides)
      (setq x (+ x (* dx1 (- 1 c)) (* dx2 (+ 1 c)))
            y (+ y (* dx1 (- s)) (* dx2 s)))))
    (cond
     ((and (memq 'n sides) (memq 's sides))
      (setq y (+ y dy1 dy2)))
     ((memq 's sides)
      (setq x (+ x (* dy1 (- s)) (* dy2 s))
            y (+ y (* dy1 (+ 1 c)) (* dy2 (- 1 c)))))
     ((memq 'n sides)
      (setq x (+ x (* dy1 s) (* dy2 (- s)))
            y (+ y (* dy1 (- 1 c)) (* dy2 (+ 1 c))))))
    (cons x y)))

(defun excal--keep-text-anchor (element old-w old-h)
  "Move free text ELEMENT so its alignment anchor stays after a resize.
OLD-W, OLD-H are its size before; upstream `getAdjustedDimensions'."
  (let ((w (excal--get element 'width)) (h (excal--get element 'height))
        (align (excal--get element 'textAlign))
        (x (excal--get element 'x)) (y (excal--get element 'y)))
    (pcase-let
        ((`(,nx . ,ny)
          (if (and (equal align "center")
                   (equal (excal--get element 'verticalAlign) "middle")
                   (excal--auto-resize-p element))
              (cons (- x (/ (- w old-w) 2.0)) (- y (/ (- h old-h) 2.0)))
            (excal--adjust-xy-with-rotation
             (append '(s)
                     (and (member align '("center" "left")) '(e))
                     (and (member align '("center" "right")) '(w)))
             x y (or (excal--get element 'angle) 0)
             0 0 (/ (- old-w w) 2.0) (/ (- old-h h) 2.0)))))
      (excal--put element 'x (float nx))
      (excal--put element 'y (float ny)))))

(defun excal--set-text (element text)
  "Set text ELEMENT's source TEXT and re-layout it.
Bound text re-wraps to its container, fixed-width text to its width;
free auto-resizing text keeps its alignment anchor."
  (let ((normalized (excal--normalize-text text))
        (old-w (excal--get element 'width)) (old-h (excal--get element 'height)))
    (excal--put element 'originalText normalized)
    (excal--put element 'text normalized)
    (excal--redraw-text element)
    (unless (excal--container-of element)
      (excal--keep-text-anchor element (or old-w 0) (or old-h 0))
      (excal--touch element))
    element))

(defun excal--text-scale (element scale)
  "Scale text ELEMENT's font and box by SCALE (corner resize).
Upstream `resizeSingleTextElement' for handles with n or s: the font is
not rounded.  Return nil, changing nothing, when the font would drop
below `excal-min-font-size'.  The caller positions the element."
  (let ((size (* (excal--get element 'fontSize) scale)))
    (when (>= size excal-min-font-size)
      (excal--put element 'fontSize size)
      (excal--put element 'width (* (excal--get element 'width) scale))
      (excal--put element 'height (* (excal--get element 'height) scale))
      (excal--touch element)
      element)))

(defun excal--text-set-width (element width)
  "Give text ELEMENT a fixed WIDTH, re-wrapping it (side resize).
The width is at least `getMinTextElementWidth'; `autoResize' becomes
false.  The caller positions the element."
  (pcase-let* ((`(,size ,family ,lh) (excal--text-font element))
               (width (max (excal--min-text-width size family lh) width))
               (lines (excal--wrap-text (or (excal--get element 'originalText) "")
                                        size family width)))
    (excal--put element 'text lines)
    (excal--put element 'width (float width))
    (excal--put element 'height (cdr (excal--measure-string lines size family lh)))
    (excal--put element 'autoResize :false)
    (excal--touch element)
    element))

(defun excal--text-reset-auto-resize (element)
  "Make text ELEMENT size itself to its text again (`actionTextAutoResize')."
  (pcase-let* ((`(,size ,family ,lh) (excal--text-font element))
               (original (or (excal--get element 'originalText) ""))
               (`(,w . ,h) (excal--measure-string original size family lh))
               (`(,ax . ,ay) (excal--text-anchor-ratios
                              (excal--get element 'textAlign)
                              (excal--get element 'verticalAlign))))
    (excal--put element 'x (float (+ (excal--get element 'x)
                                     (* (- (excal--get element 'width) w) ax))))
    (excal--put element 'y (float (+ (excal--get element 'y)
                                     (* (- (excal--get element 'height) h) ay))))
    (excal--put element 'autoResize t)
    (excal--put element 'text original)
    (excal--put element 'width w)
    (excal--put element 'height h)
    (excal--touch element)
    element))

(defun excal--text-font-changed (element property)
  "Re-layout text ELEMENT after its font PROPERTY was set.
PROPERTY is `fontFamily' (the line height follows the family) or
`fontSize'.  Free auto-resizing text keeps its anchor
(`offsetElementAfterFontResize')."
  (when (eq property 'fontFamily)
    (excal--put element 'lineHeight
                (excal--line-height (excal--get element 'fontFamily))))
  (when (and (eq property 'fontSize) (excal--get element 'baseFontSize))
    (excal--put element 'baseFontSize (excal--get element 'fontSize)))
  (let ((old-w (excal--get element 'width)) (old-h (excal--get element 'height)))
    (excal--redraw-text element)
    (when (and (not (excal--container-of element)) (excal--auto-resize-p element))
      (let ((dw (- (excal--get element 'width) old-w))
            (dh (- (excal--get element 'height) old-h)))
        (excal--put element 'x
                    (float (- (excal--get element 'x)
                              (pcase (excal--get element 'textAlign)
                                ("center" (/ dw 2.0)) ("right" dw) (_ 0)))))
        (excal--put element 'y (float (- (excal--get element 'y) (/ dh 2.0))))
        (excal--touch element)))))

(defun excal--add-bound-text (container &rest props)
  "Create an empty label bound to CONTAINER and return it.
The label is centered and middle-aligned, placed right after CONTAINER
in z-order, and listed in CONTAINER's `boundElements'.  PROPS are field
overrides (for example the current style)."
  (pcase-let* ((`(,cx . ,cy) (excal--container-center container))
               (text (apply #'excal--make-text-element cx cy ""
                            (cons 'textAlign "center") (cons 'verticalAlign "middle")
                            (cons 'containerId (excal--get container 'id))
                            (cons 'angle (if (excal--arrow-p container) 0
                                           (or (excal--get container 'angle) 0)))
                            props))
               (bound (excal--get container 'boundElements)))
    (excal--put container 'boundElements
                (vconcat (and (sequencep bound) bound)
                         (list (list (cons 'type "text")
                                     (cons 'id (excal--get text 'id))))))
    (excal--touch container)
    (let ((tail (memq container excal--elements)))
      (if tail
          (setcdr tail (cons text (cdr tail)))
        (setq excal--elements (append excal--elements (list text)))))
    (excal--redraw-text text container)
    text))

(defun excal--remove-bound-text (container text)
  "Delete label TEXT from the scene and from CONTAINER's `boundElements'."
  (setq excal--elements (delq text excal--elements))
  (let ((bound (excal--get container 'boundElements)))
    (when (sequencep bound)
      (excal--put container 'boundElements
                  (vconcat (seq-remove (lambda (b) (equal (alist-get 'id b)
                                                          (excal--get text 'id)))
                                       bound))))
    (excal--touch container)))

;;;; Restore (restore.ts, case "text")

;;;; Native extras

(defun excal--text-native-extras (element)
  "Return the module's text extras for ELEMENT, see `excal--native-text-extras'."
  (pcase (excal--get element 'type)
    ("text"
     (pcase-let ((`(,size ,family ,lh) (excal--text-font element)))
       (vector "vertical-offset" (excal--vertical-offset family size (* size lh)))))
    ("arrow"
     (if-let* ((hole (excal--arrow-label-hole element)))
         (vector "label-hole" hole)
       []))
    (_ [])))

(provide 'excal-text)
;;; excal-text.el ends here
