;;; excali-text.el --- Text layout: fonts, measurement, wrapping, bound text  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Excalidraw's text layout, ported from packages/common/src/font-metadata.ts
;; and packages/element/src/{textMeasurements,textWrapping,textElement,
;; linearElementEditor,stickyNote}.ts.  Layout decisions are made here;
;; the module only measures and draws single lines (excali-text.c).
;;
;; Fonts: files in `excali-fonts-directory' are registered with the font
;; backend when this file loads; `excali-font-families' chooses the Pango
;; family list per font id.
;;
;; API for other parts of excali (all mutate in place and `excali--touch'):
;;
;; - `excali--bound-text-of' CONTAINER, `excali--container-of' TEXT.
;; - `excali--refresh-bound-text' CONTAINER: re-wrap, grow and reposition
;;   CONTAINER's label.  Call after moving a container, editing an
;;   arrow's points, or any other container change.
;; - `excali--layout-bound-text' CONTAINER &optional HANDLE ...: the same
;;   after a resize by HANDLE (upstream `handleBindTextResize').
;; - `excali--redraw-text' TEXT: re-wrap and re-measure TEXT after its
;;   text or font changed, growing and following its container.
;; - `excali--text-scale' TEXT SCALE (corner resize: font scales),
;;   `excali--text-set-width' TEXT WIDTH (side resize: re-wrap),
;;   `excali--text-reset-auto-resize' TEXT.
;; - `excali--add-bound-text' CONTAINER: create an empty label.
;; - `excali--arrow-label-hole' ARROW: the rectangle cut out of an arrow
;;   behind its label; passed to the module as text extras.

;;; Code:

(require 'excali-core)
(require 'ucs-normalize)

(declare-function excali-native-text-width "excali-module")
(declare-function excali-native-add-fonts "excali-module")
(declare-function excali-native-set-font-family "excali-module")
(declare-function excali-native-font-family "excali-module")
(declare-function excali-native-font-resolve "excali-module")
(declare-function excali-native-font-backend "excali-module")

;;;; Constants (packages/common/src/constants.ts)

(defconst excali-font-family-ids
  '(("Virgil" . 1) ("Helvetica" . 2) ("Cascadia" . 3) ("Excalifont" . 5)
    ("Nunito" . 6) ("Lilita One" . 7) ("Comic Shanns" . 8)
    ("Liberation Sans" . 9) ("Assistant" . 10))
  "Excalidraw `FONT_FAMILY': family name to numeric id.")

(defconst excali-default-font-family 5 "`DEFAULT_FONT_FAMILY' (Excalifont).")
(defconst excali-default-font-size 20 "`DEFAULT_FONT_SIZE'.")
(defconst excali-min-font-size 1 "`MIN_FONT_SIZE'.")
(defconst excali-bound-text-padding 5 "`BOUND_TEXT_PADDING'.")
(defconst excali-arrow-label-width-fraction 0.7 "`ARROW_LABEL_WIDTH_FRACTION'.")
(defconst excali-arrow-label-font-size-to-min-width-ratio 11
  "`ARROW_LABEL_FONT_SIZE_TO_MIN_WIDTH_RATIO'.")
(defconst excali-text-autowrap-threshold 36 "`TEXT_AUTOWRAP_THRESHOLD'.")
(defconst excali-sticky-note-padding 16 "`STICKY_NOTE_PADDING'.")
(defconst excali-sticky-note-body-inset-y 52 "`STICKY_NOTE_BODY_INSET_Y'.")
(defconst excali-sticky-note-min-size 75 "`STICKY_NOTE_MIN_SIZE'.")
(defconst excali-sticky-note-min-font-size 16 "`STICKY_NOTE_MIN_FONT_SIZE'.")
(defconst excali-sticky-note-max-font-size 512 "`STICKY_NOTE_MAX_FONT_SIZE'.")
(defconst excali-sticky-note-fallback-font-size 28
  "`STICKY_NOTE_FALLBACK_FONT_SIZE'.")
(defconst excali-sticky-note-font-step 2 "`STICKY_NOTE_FONT_STEP'.")

(defconst excali--font-metadata
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

(defun excali--font-metrics (family)
  "Return (UNITS-PER-EM ASCENDER DESCENDER LINE-HEIGHT) of font id FAMILY.
Unknown ids use Excalifont's metrics, as upstream."
  (cdr (or (assq family excali--font-metadata)
           (assq excali-default-font-family excali--font-metadata))))

(defun excali--line-height (family)
  "Return the default unitless line height of font id FAMILY (`getLineHeight')."
  (nth 3 (excali--font-metrics family)))

(defun excali--vertical-offset (family font-size line-height-px)
  "Return the first baseline's distance below the text top (`getVerticalOffset')."
  (pcase-let* ((`(,units ,ascender ,descender ,_) (excali--font-metrics family))
               (em (/ (float font-size) units))
               (gap (/ (+ (- line-height-px (* em ascender)) (* em descender)) 2)))
    (+ (* em ascender) gap)))

;;;; Fonts

(defgroup excali-text nil
  "Text and fonts in excali."
  :group 'excali)

(defcustom excali-fonts-directory (expand-file-name "fonts" excali--directory)
  "Directory whose font files are registered when excali loads.
`make fonts' downloads Excalidraw's fonts here."
  :type 'directory)

(defvar excali--line-width-cache (make-hash-table :test #'equal)
  "Line widths keyed by (LINE FONT-SIZE FONT-FAMILY).")

(defvar excali--char-width-cache (make-hash-table :test #'equal)
  "Upstream `charWidth' cache: widths keyed by (FONT-SIZE FONT-FAMILY CODE).")

(defun excali--apply-font-families (families)
  "Send FAMILIES, an alist (ID . PANGO-FAMILY-LIST), to the module."
  (dolist (entry families)
    (excali-native-set-font-family (car entry) (cdr entry)))
  (clrhash excali--line-width-cache)
  (clrhash excali--char-width-cache))

(defcustom excali-font-families nil
  "Pango family lists overriding the built-in ones, as (ID . FAMILIES).
ID is an Excalidraw font id (see `excali-font-family-ids'; 100 is the
Xiaolai CJK fallback, 1000 Segoe UI Emoji) and FAMILIES a
comma-separated Pango family list such as
\"Excalifont, Xiaolai, LXGW WenKai, sans-serif\".  Metrics
(line height, baseline) always follow the element's font id."
  :type '(alist :key-type integer :value-type string)
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'excali-native-set-font-family)
           (excali--apply-font-families value))))

(defun excali-register-fonts (&optional directory)
  "Register the font files in DIRECTORY with the font backend.
DIRECTORY defaults to `excali-fonts-directory'.  Return the number of
files registered."
  (interactive)
  (let* ((dir (or directory excali-fonts-directory))
         (count (or (and (file-directory-p dir) (excali-native-add-fonts dir)) 0)))
    (when (> count 0)
      (clrhash excali--line-width-cache)
      (clrhash excali--char-width-cache))
    (when (called-interactively-p 'interactive)
      (message "Registered %d font files from %s" count dir))
    count))

(defun excali-font-report ()
  "Show which fonts render each Excalidraw font family."
  (interactive)
  (with-help-window "*excali fonts*"
    (princ (format "Pango font map: %s\nFonts directory: %s\n\n"
                   (excali-native-font-backend) excali-fonts-directory))
    (dolist (entry (append excali-font-family-ids '(("Xiaolai" . 100))))
      (princ (format "%-16s %4d  %s\n%22s Latin: %s  CJK: %s\n"
                     (car entry) (cdr entry)
                     (excali-native-font-family (cdr entry)) ""
                     (excali-native-font-resolve "Hello" (cdr entry))
                     (excali-native-font-resolve "你好" (cdr entry)))))))

(excali-register-fonts)
(excali--apply-font-families excali-font-families)

;;;; Measurement (textMeasurements.ts)

(defun excali--normalize-text (text)
  "Normalize line ends to \\n and tabs to 8 spaces (`normalizeText')."
  (string-replace "\t" "        "
                  (replace-regexp-in-string "\r\n?" "\n" text t t)))

(defun excali--line-width (line font-size family)
  "Return the advance width of LINE (`getLineWidth')."
  (if (string-empty-p line)
      0.0
    (let ((key (list line font-size family)))
      (or (gethash key excali--line-width-cache)
          (progn
            (when (> (hash-table-count excali--line-width-cache) 20000)
              (clrhash excali--line-width-cache))
            (puthash key (excali-native-text-width line font-size family)
                     excali--line-width-cache))))))

(defun excali--char-width (char font-size family)
  "Return the width of CHAR through upstream's `charWidth' cache.
Like upstream, the cache is keyed by the first UTF-16 code unit, so
characters outside the BMP share entries within a surrogate block."
  (let* ((unit (if (> char #xFFFF)
                   (+ #xD800 (ash (- char #x10000) -10))
                 char))
         (key (list font-size family unit))
         (width (gethash key excali--char-width-cache)))
    (if (and width (/= width 0))
        width
      (puthash key (excali--line-width (string char) font-size family)
               excali--char-width-cache))))

(defun excali--measure-string (text font-size family line-height)
  "Return (WIDTH . HEIGHT) of TEXT as upstream `measureText'.
Empty lines count as a space; height is lines * size * line height."
  (let ((lines (split-string (excali--normalize-text text) "\n")))
    (cons (float (apply #'max (mapcar (lambda (line)
                                        (excali--line-width
                                         (if (string-empty-p line) " " line)
                                         font-size family))
                                      lines)))
          (* (length lines) font-size line-height))))

(defun excali--text-font (element)
  "Return (FONT-SIZE FAMILY LINE-HEIGHT) of text ELEMENT."
  (let ((family (or (excali--get element 'fontFamily) excali-default-font-family)))
    (list (or (excali--get element 'fontSize) excali-default-font-size)
          family
          (or (excali--get element 'lineHeight) (excali--line-height family)))))

(defun excali--auto-resize-p (element)
  "Return non-nil if text ELEMENT sizes itself to its text.
A missing `autoResize' counts as true, as upstream restore does."
  (let ((cell (assq 'autoResize element)))
    (or (null cell) (not (memq (cdr cell) '(nil :false :null))))))

(defun excali--min-text-width (font-size family line-height)
  "Return `getMinTextElementWidth': a space plus twice the padding."
  (+ (car (excali--measure-string "" font-size family line-height))
     (* 2 excali-bound-text-padding)))

;;;; Wrapping (textWrapping.ts)

;; Character classes of the line-break rules, as bits.
(defconst excali--wrap-ws 1)          ; COMMON.WHITESPACE, JS \s
(defconst excali--wrap-hyphen 2)      ; COMMON.HYPHEN
(defconst excali--wrap-open 4)        ; COMMON.OPENING
(defconst excali--wrap-close 8)       ; COMMON.CLOSING
(defconst excali--wrap-cjk 16)        ; CJK.CHAR
(defconst excali--wrap-cjk-open 32)   ; CJK.OPENING
(defconst excali--wrap-cjk-close 64)  ; CJK.CLOSING
(defconst excali--wrap-currency 128)  ; CJK.CURRENCY
(defconst excali--wrap-emoji-most 256) ; EMOJI.MOST
(defconst excali--wrap-emoji-any 512)  ; EMOJI.ANY
(defconst excali--wrap-emoji-mod 1024) ; \p{Emoji_Modifier}
(defconst excali--wrap-ri 2048)        ; \p{RI}

(defconst excali--extended-pictographic
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

(defconst excali--cjk-scripts
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

(defconst excali--wrap-classes (make-char-table 'excali-wrap 0)
  "Char table of `excali--wrap-*' class bits; see `excali--init-wrap-classes'.")

(defun excali--add-wrap-class (chars bit)
  "Add class BIT to CHARS: a string, or a list of chars and (FROM . TO)."
  (dolist (c (if (stringp chars) (string-to-list chars) chars))
    (if (consp c)
        (let ((i (car c)))
          (while (<= i (cdr c))
            (aset excali--wrap-classes i (logior (aref excali--wrap-classes i) bit))
            (setq i (1+ i))))
      (aset excali--wrap-classes c (logior (aref excali--wrap-classes c) bit)))))

(defun excali--init-wrap-classes ()
  "Fill `excali--wrap-classes' from the upstream character classes."
  (excali--add-wrap-class
   '(9 10 11 12 13 32 #xA0 #x1680 (#x2000 . #x200A) #x2028 #x2029 #x202F
       #x205F #x3000 #xFEFF)
   excali--wrap-ws)
  (excali--add-wrap-class "-" excali--wrap-hyphen)
  (excali--add-wrap-class "<([{" excali--wrap-open)
  (excali--add-wrap-class ">)]}.,:;!?…/" excali--wrap-close)
  (excali--add-wrap-class excali--cjk-scripts excali--wrap-cjk)
  (excali--add-wrap-class "｀＇＾〃〰〆＃＆＊＋－ー／＼＝｜￤〒￢￣" excali--wrap-cjk)
  (excali--add-wrap-class "（［｛〈《｟｢「『【〖〔〘〚＜〝" excali--wrap-cjk-open)
  (excali--add-wrap-class "）］｝〉》｠｣」』】〗〕〙〛＞。．，、〟‥？！：；・〜〞"
                         excali--wrap-cjk-close)
  (excali--add-wrap-class "￥￦￡￠＄" excali--wrap-currency)
  (let ((most (append excali--extended-pictographic
                      '((#x1F1E6 . #x1F1FF) (#x1F3FB . #x1F3FF)))))
    (excali--add-wrap-class most excali--wrap-emoji-most)
    ;; \p{Emoji}: the pictographs plus keycap bases.
    (excali--add-wrap-class (append most '(?# ?* (?0 . ?9))) excali--wrap-emoji-any))
  (excali--add-wrap-class '((#x1F3FB . #x1F3FF)) excali--wrap-emoji-mod)
  (excali--add-wrap-class '((#x1F1E6 . #x1F1FF)) excali--wrap-ri))

(excali--init-wrap-classes)

(defsubst excali--wrap-class (char)
  "Return the class bits of CHAR."
  (aref excali--wrap-classes char))

(defsubst excali--wrap-is (char bits)
  "Return non-nil if CHAR has any of the class BITS."
  (/= 0 (logand (aref excali--wrap-classes char) bits)))

(defun excali--emoji-joiner (string i)
  "Return the end of EMOJI.JOINER matched in STRING at I (I if none)."
  (let ((n (length string)))
    (cond
     ((>= i n) i)
     ((excali--wrap-is (aref string i) excali--wrap-emoji-mod) (1+ i))
     ((= (aref string i) #xFE0F)
      (if (and (< (1+ i) n) (= (aref string (1+ i)) #x20E3)) (+ i 2) (1+ i)))
     ((<= #xE0020 (aref string i) #xE007E)
      (let ((j i))
        (while (and (< j n) (<= #xE0020 (aref string j) #xE007E))
          (setq j (1+ j)))
        (if (and (< j n) (= (aref string j) #xE007F)) (1+ j) i)))
     (t i))))

(defun excali--emoji-at (string i)
  "Return the end of the emoji regex match in STRING at I, or nil."
  (let ((n (length string)))
    (cond
     ((and (< (1+ i) n)
           (excali--wrap-is (aref string i) excali--wrap-ri)
           (excali--wrap-is (aref string (1+ i)) excali--wrap-ri))
      (+ i 2))
     ((excali--wrap-is (aref string i) excali--wrap-emoji-most)
      (let ((j (excali--emoji-joiner string (1+ i))) (done nil))
        (while (and (not done) (< (1+ j) n) (= (aref string j) #x200D))
          (cond
           ((and (< (+ j 2) n)
                 (excali--wrap-is (aref string (1+ j)) excali--wrap-ri)
                 (excali--wrap-is (aref string (+ j 2)) excali--wrap-ri))
            (setq j (+ j 3)))
           ((excali--wrap-is (aref string (1+ j)) excali--wrap-emoji-any)
            (setq j (excali--emoji-joiner string (+ j 2))))
           (t (setq done t))))
        j))
     (t nil))))

(defun excali--break-between-p (string i)
  "Return non-nil if the advanced break regex matches STRING at I.
I is between the characters at I-1 and I; only zero-width rules apply."
  (let* ((a (excali--wrap-class (aref string (1- i))))
         (b (excali--wrap-class (aref string i)))
         (a-in (lambda (bits) (/= 0 (logand a bits))))
         (b-in (lambda (bits) (/= 0 (logand b bits)))))
    (or
     ;; Break.Before(WHITESPACE)
     (funcall b-in excali--wrap-ws)
     ;; Break.After(WHITESPACE, HYPHEN)
     (funcall a-in (logior excali--wrap-ws excali--wrap-hyphen))
     ;; Break.Before(CJK.CHAR, CJK.CURRENCY).NotPrecededBy(OPENING, CJK.OPENING)
     (and (funcall b-in (logior excali--wrap-cjk excali--wrap-currency))
          (not (funcall a-in (logior excali--wrap-open excali--wrap-cjk-open))))
     ;; Break.After(CJK.CHAR).NotFollowedBy(HYPHEN, CLOSING, CJK.CLOSING)
     (and (funcall a-in excali--wrap-cjk)
          (not (funcall b-in (logior excali--wrap-hyphen excali--wrap-close
                                     excali--wrap-cjk-close))))
     ;; Break.BeforeMany(CJK.OPENING).NotPrecededBy(OPENING)
     (and (funcall b-in excali--wrap-cjk-open)
          (not (funcall a-in (logior excali--wrap-cjk-open excali--wrap-open))))
     ;; Break.AfterMany(CJK.CLOSING).NotFollowedBy(CLOSING)
     (and (funcall a-in excali--wrap-cjk-close)
          (not (funcall b-in (logior excali--wrap-cjk-close excali--wrap-close))))
     ;; Break.AfterMany(CLOSING).FollowedBy(OPENING)
     (and (funcall a-in excali--wrap-close)
          (not (funcall b-in excali--wrap-close))
          (funcall b-in excali--wrap-open)))))

(defun excali--tokenize (line)
  "Split LINE into breakable tokens (upstream `parseTokens').
LINE is NFC-normalized first, then split like
`line.split(breakLineRegex).filter(Boolean)': emoji sequences are
tokens of their own, and zero-width rules break between characters."
  (let* ((s (ucs-normalize-NFC-string line))
         (n (length s)) (start 0) (i 0) tokens)
    (while (< i n)
      (let ((emoji (excali--emoji-at s i)))
        (cond
         (emoji
          (when (> i start) (push (substring s start i) tokens))
          (push (substring s i emoji) tokens)
          (setq start emoji i emoji))
         (t
          (when (and (> i start) (excali--break-between-p s i))
            (push (substring s start i) tokens)
            (setq start i))
          (setq i (1+ i))))))
    (when (> n start) (push (substring s start) tokens))
    (nreverse tokens)))

(defconst excali--js-space-regexp
  "[\t\n\v\f\r    -     　﻿]"
  "JavaScript's \\s.")

(defun excali--trim-end (string)
  "Return STRING without trailing JavaScript whitespace (`trimEnd')."
  (if (string-match (concat excali--js-space-regexp "+\\'") string)
      (substring string 0 (match-beginning 0))
    string))

(defun excali--single-character-p (token)
  "Return non-nil if TOKEN is one UTF-16 code unit (`isSingleCharacter')."
  (and (= (length token) 1) (< (aref token 0) #x10000)))

(defun excali--wrap-word (word size family max-width)
  "Split WORD into lines of at most MAX-WIDTH (upstream `wrapWord')."
  (if (excali--emoji-at word 0)
      ;; Emoji sequences are atomic.
      (list word)
    (let ((lines nil) (current "") (width 0))
      (dolist (char (string-to-list word))
        (let* ((w (excali--char-width char size family))
               (test (+ width w)))
          (if (<= test max-width)
              (setq current (concat current (string char)) width test)
            (unless (string-empty-p current) (push current lines))
            (setq current (string char) width w))))
      (unless (string-empty-p current) (push current lines))
      (nreverse lines))))

(defun excali--trim-line (line size family max-width)
  "Trim trailing whitespace of LINE beyond MAX-WIDTH (upstream `trimLine')."
  (if (<= (excali--line-width line size family) max-width)
      line
    (pcase-let* ((`(,trimmed ,spaces)
                  (if (string-match (concat "\\`\\(.+?\\)\\(" excali--js-space-regexp
                                            "+\\)\\'")
                                    line)
                      (list (match-string 1 line) (match-string 2 line))
                    (list (excali--trim-end line) "")))
                 (width (excali--line-width trimmed size family)))
      (catch 'full
        (dolist (char (string-to-list spaces))
          (let ((test (+ width (excali--char-width char size family))))
            (when (> test max-width) (throw 'full nil))
            (setq trimmed (concat trimmed (string char)) width test))))
      trimmed)))

(defun excali--wrap-line (line size family max-width)
  "Wrap the hard LINE into lines of at most MAX-WIDTH (upstream `wrapLine')."
  (let ((tokens (excali--tokenize line))
        (lines nil) (current "") (width 0))
    (while tokens
      (let* ((token (car tokens))
             (test-line (concat current token))
             (test-width (if (excali--single-character-p token)
                             (+ width (excali--char-width (aref token 0) size family))
                           (excali--line-width test-line size family))))
        (cond
         ;; Build up the line; whitespace never breaks it here.
         ((or (string-match-p excali--js-space-regexp token)
              (<= test-width max-width))
          (setq current test-line width test-width tokens (cdr tokens)))
         ;; The word alone is too long: break it into characters.
         ((string-empty-p current)
          (let ((pieces (excali--wrap-word token size family max-width)))
            (dolist (piece (butlast pieces)) (push piece lines))
            (setq current (or (car (last pieces)) "")
                  width (excali--line-width current size family)
                  tokens (cdr tokens))))
         ;; Start a new line with this token.
         (t
          (push (excali--trim-end current) lines)
          (setq current "" width 0)))))
    (unless (string-empty-p current)
      (push (excali--trim-line current size family max-width) lines))
    (nreverse lines)))

(defun excali--wrap-text (text size family max-width)
  "Wrap TEXT to MAX-WIDTH as upstream `wrapText' and return the result.
Hard line breaks are kept; a hard line is only wrapped when wider than
MAX-WIDTH.  A non-finite or negative MAX-WIDTH leaves TEXT unwrapped."
  (if (or (not (numberp max-width)) (isnan (float max-width))
          (= (abs max-width) 1.0e+INF) (< max-width 0))
      text
    (mapconcat
     (lambda (line)
       (if (<= (excali--line-width line size family) max-width)
           line
         (mapconcat #'identity (excali--wrap-line line size family max-width) "\n")))
     (split-string text "\n") "\n")))

;;;; Containers (textElement.ts)

(defconst excali--text-container-types
  '("rectangle" "stickynote" "ellipse" "diamond" "arrow")
  "Upstream `VALID_CONTAINER_TYPES'.")

(defun excali--text-container-p (element)
  "Return non-nil if ELEMENT can hold bound text (`isValidTextContainer')."
  (and element (member (excali--get element 'type) excali--text-container-types) t))

(defun excali--bound-text-id (container)
  "Return the id of CONTAINER's bound text, or nil (`getBoundTextElementId')."
  (let ((bound (excali--get container 'boundElements)))
    (and (sequencep bound)
         (alist-get 'id (seq-find (lambda (b) (equal (alist-get 'type b) "text"))
                                  bound)))))

(defun excali--bound-text-of (container)
  "Return CONTAINER's live bound text element, or nil."
  (when-let* ((id (excali--bound-text-id container))
              (text (excali--element-by-id id)))
    (unless (excali--get text 'isDeleted) text)))

(defun excali--container-of (text)
  "Return the live container of TEXT, or nil."
  (when-let* ((container (excali--element-by-id (excali--get text 'containerId))))
    (unless (excali--get container 'isDeleted) container)))

(defun excali--arrow-p (element)
  "Return non-nil if ELEMENT is an arrow."
  (equal (excali--get element 'type) "arrow"))

(defun excali--sticky-note-p (element)
  "Return non-nil if ELEMENT is a sticky note."
  (equal (excali--get element 'type) "stickynote"))

(defvar-local excali-text-padding-function nil
  "Optional function returning bound-text padding for a container.
Return nil to use the normal Excalidraw padding.  Used by derived modes.")

(defun excali--text-padding (container)
  "Return the bound-text inset for CONTAINER."
  (or (and container excali-text-padding-function
           (funcall excali-text-padding-function container))
      excali-bound-text-padding))

(defun excali--bound-text-max-width (container &optional text)
  "Return the widest text CONTAINER fits (`getBoundTextMaxWidth')."
  (let ((width (excali--get container 'width))
        (pad (excali--text-padding container)))
    (pcase (excali--get container 'type)
      ("arrow"
       (max (* excali-arrow-label-width-fraction width)
            (* (or (and text (excali--get text 'fontSize)) excali-default-font-size)
               excali-arrow-label-font-size-to-min-width-ratio)))
      ("ellipse" (- (round (* (/ width 2.0) (sqrt 2))) (* 2 pad)))
      ("diamond" (- (round (/ width 2.0)) (* 2 pad)))
      ("stickynote" (- width (* 2 excali-sticky-note-padding)))
      (_ (- width (* 2 pad))))))

(defun excali--bound-text-max-height (container text)
  "Return the tallest TEXT CONTAINER fits (`getBoundTextMaxHeight')."
  (let ((height (excali--get container 'height))
        (pad (excali--text-padding container)))
    (pcase (excali--get container 'type)
      ("stickynote" (max 0 (- height excali-sticky-note-body-inset-y)))
      ("arrow" (if (<= (- height (* pad 8 2)) 0) (excali--get text 'height) height))
      ("ellipse" (- (round (* (/ height 2.0) (sqrt 2))) (* 2 pad)))
      ("diamond" (- (round (/ height 2.0)) (* 2 pad)))
      (_ (- height (* 2 pad))))))

(defun excali--container-coords (container)
  "Return (X . Y), the top-left of CONTAINER's text box (`getContainerCoords')."
  (let* ((pad (if (excali--sticky-note-p container)
                  excali-sticky-note-padding
                (excali--text-padding container)))
         (w (excali--get container 'width)) (h (excali--get container 'height))
         (ox pad) (oy pad))
    (pcase (excali--get container 'type)
      ("ellipse" (setq ox (+ ox (* (/ w 2.0) (- 1 (/ (sqrt 2) 2))))
                       oy (+ oy (* (/ h 2.0) (- 1 (/ (sqrt 2) 2))))))
      ("diamond" (setq ox (+ ox (/ w 4.0)) oy (+ oy (/ h 4.0)))))
    (cons (+ (excali--get container 'x) ox) (+ (excali--get container 'y) oy))))

(defun excali--container-dimension-for-text (dimension type &optional container)
  "Return the container size fitting DIMENSION of text in container TYPE.
Upstream `computeContainerDimensionForBoundText'.
CONTAINER optionally supplies a derived mode's text padding."
  (let ((dim (ceiling dimension)) (pad (* 2 (excali--text-padding container))))
    (pcase type
      ("ellipse" (round (* (/ (+ dim pad) (sqrt 2)) 2)))
      ("arrow" (+ dim (* pad 8)))
      ("diamond" (* 2 (+ dim pad)))
      (_ (+ dim pad)))))

(defun excali--bound-text-position (container text)
  "Return (X . Y) for TEXT inside CONTAINER (`computeBoundTextPosition')."
  (if (excali--arrow-p container)
      (excali--arrow-label-position container text)
    (pcase-let* ((`(,cx . ,cy) (excali--container-coords container))
                 (max-h (excali--bound-text-max-height container text))
                 (max-w (excali--bound-text-max-width container text))
                 (tw (excali--get text 'width)) (th (excali--get text 'height))
                 (y (pcase (excali--get text 'verticalAlign)
                      ("top" cy)
                      ("bottom" (+ cy (- max-h th)))
                      (_ (if (excali--sticky-note-p container)
                             (+ cy (min (/ (- (excali--get container 'height)
                                              (* 2 excali-sticky-note-padding)
                                              th)
                                           2.0)
                                        (- max-h th)))
                           (+ cy (- (/ max-h 2.0) (/ th 2.0)))))))
                 (x (pcase (excali--get text 'textAlign)
                      ("left" cx)
                      ("right" (+ cx (- max-w tw)))
                      (_ (+ cx (- (/ max-w 2.0) (/ tw 2.0))))))
                 (angle (or (excali--get container 'angle) 0)))
      (if (= angle 0)
          (cons (float x) (float y))
        (pcase-let* ((`(,ccx . ,ccy)
                      (if (excali--sticky-note-p container)
                          (cons (+ (excali--get container 'x)
                                   (/ (excali--get container 'width) 2.0))
                                (+ (excali--get container 'y)
                                   (/ (excali--get container 'height) 2.0)))
                        (cons (+ cx (/ max-w 2.0)) (+ cy (/ max-h 2.0)))))
                     (`(,rx . ,ry) (excali--rotate-point (cons (+ x (/ tw 2.0)) (+ y (/ th 2.0)))
                                                        (cons ccx ccy) angle)))
          (cons (- rx (/ tw 2.0)) (- ry (/ th 2.0))))))))

(defun excali--container-center (container)
  "Return (X . Y) where new text in CONTAINER is anchored (`getContainerCenter')."
  (if (excali--arrow-p container)
      (excali--arrow-label-center container)
    (cons (+ (excali--get container 'x) (/ (excali--get container 'width) 2.0))
          (+ (excali--get container 'y) (/ (excali--get container 'height) 2.0)))))

(defun excali--position-after-height-change (container height anchor)
  "Return CONTAINER's y after changing its height to HEIGHT keeping ANCHOR.
ANCHOR is `top', `bottom' or `center' (the edge that stays put)."
  (let ((y (excali--get container 'y)) (dh (- height (excali--get container 'height))))
    (pcase anchor
      ('bottom (- y dh))
      ('center (- y (/ dh 2.0)))
      (_ y))))

(defun excali--invalidate-native (element)
  "Drop ELEMENT's cached native vector so it is rebuilt."
  (when (hash-table-p excali--native-cache)
    (remhash element excali--native-cache)))

(defvar-local excali-text-layout-function nil
  "Optional function (CONTAINER TEXT) that returns non-nil when handled.")

(defun excali--redraw-text (text &optional container)
  "Re-wrap, measure and place TEXT (upstream `redrawTextBoundingBox').
CONTAINER defaults to TEXT's container.  Bound text wraps to the
container, which grows (never shrinks) to fit; free text with
`autoResize' false wraps to its width; other text keeps its lines and
takes the measured width.  Return TEXT."
  (let ((container (or container (excali--container-of text))))
    (if (and container excali-text-layout-function
             (funcall excali-text-layout-function container text))
        text
      (if (and container (excali--sticky-note-p container))
          (excali--update-sticky-note-layout container text)
	(pcase-let* ((`(,size ,family ,lh) (excali--text-font text))
                     (auto (excali--auto-resize-p text))
                     (original (or (excali--get text 'originalText)
                                   (excali--get text 'text) ""))
                     (lines (if (or container (not auto))
				(excali--wrap-text
				 original size family
				 (if container
                                     (excali--bound-text-max-width container text)
                                   (excali--get text 'width)))
                              (or (excali--get text 'text) "")))
                     (`(,w . ,h) (excali--measure-string lines size family lh)))
          (excali--put text 'text lines)
          (when (or auto (null (excali--get text 'width)))
            (excali--put text 'width w))
          (excali--put text 'height h)
          (when container
            (excali--put text 'angle (if (excali--arrow-p container)
					 0
                                       (or (excali--get container 'angle) 0)))
            (let ((changed nil))
              (when (and (not (excali--arrow-p container))
			 (> h (excali--bound-text-max-height container text)))
		(excali--put container 'height
                             (float (excali--container-dimension-for-text
                                     h (excali--get container 'type) container)))
		(setq changed t))
              (when (> w (excali--bound-text-max-width container text))
		(excali--put container 'width
                             (float (excali--container-dimension-for-text
                                     w (excali--get container 'type) container)))
		(setq changed t))
              (if changed (excali--touch container) (excali--invalidate-native container)))
            (pcase-let ((`(,x . ,y) (excali--bound-text-position container text)))
              (excali--put text 'x x)
              (excali--put text 'y y)))
          (excali--touch text)))))
  text)

(defun excali--refresh-bound-text (container)
  "Re-layout CONTAINER's bound text after CONTAINER changed.
Call after moving a container, changing an arrow's points, or any other
container change; see also `excali--layout-bound-text' for resizes."
  (when-let* ((text (excali--bound-text-of container)))
    (excali--redraw-text text container)))

(defun excali--layout-bound-text (container &optional handle keep-aspect
                                            from-center flip-y)
  "Refit CONTAINER's bound text after resizing CONTAINER by HANDLE.
HANDLE is a symbol such as `n', `se' or `e' (nil re-wraps as a corner
would).  KEEP-ASPECT, FROM-CENTER and FLIP-Y mirror upstream
`handleBindTextResize': a pure `n'/`s' drag without KEEP-ASPECT keeps
the lines; otherwise the text is re-wrapped.  If the text no longer fits
vertically the container grows, anchored at the edge opposite HANDLE."
  (if (and excali-text-layout-function
           (excali--bound-text-of container)
           (funcall excali-text-layout-function container
                    (excali--bound-text-of container)))
      container
    (if (excali--sticky-note-p container)
	(when-let* ((text (excali--bound-text-of container)))
          (excali--update-sticky-note-layout container text))
      (when-let* ((text (excali--bound-text-of container))
                  ((not (string-empty-p (or (excali--get text 'text) "")))))
	(pcase-let* ((`(,size ,family ,lh) (excali--text-font text))
                     (lines (excali--get text 'text))
                     (w (excali--get text 'width)) (h (excali--get text 'height))
                     (max-w (excali--bound-text-max-width container text))
                     (max-h (excali--bound-text-max-height container text)))
          (when (or keep-aspect (not (memq handle '(n s))))
            (setq lines (excali--wrap-text (or (excali--get text 'originalText) lines)
                                           size family max-w))
            (pcase-let ((`(,mw . ,mh) (excali--measure-string lines size family lh)))
              (setq w mw h mh)))
          (when (> h max-h)
            (let* ((height (float (excali--container-dimension-for-text
                                   h (excali--get container 'type) container)))
                   (from-top (not (eq (and (memq handle '(n ne nw)) t)
                                      (and flip-y t)))))
              (unless (excali--arrow-p container)
		(excali--put container 'y
                             (float (excali--position-after-height-change
                                     container height
                                     (cond (from-center 'center)
                                           (from-top 'bottom)
                                           (t 'top))))))
              (excali--put container 'height height)
              (excali--touch container)))
          (excali--put text 'text lines)
          (excali--put text 'width w)
          (excali--put text 'height h)
          (unless (excali--arrow-p container)
            (pcase-let ((`(,x . ,y) (excali--bound-text-position container text)))
              (excali--put text 'x x)
              (excali--put text 'y y)))
          (excali--invalidate-native container)
          (excali--touch text))))))

;;;; Arrow labels (linearElementEditor.ts)

(defun excali--linear-global-points (element)
  "Return ELEMENT's points in scene coordinates, rotated by its angle."
  (let* ((x (excali--get element 'x)) (y (excali--get element 'y))
         (points (append (excali--get element 'points) nil))
         (angle (or (excali--get element 'angle) 0))
         (abs (mapcar (lambda (p) (cons (+ x (aref p 0)) (+ y (aref p 1)))) points)))
    (if (or (= angle 0) (null abs))
        abs
      (let* ((xs (mapcar #'car abs)) (ys (mapcar #'cdr abs))
             (cx (/ (+ (apply #'min xs) (apply #'max xs)) 2.0))
             (cy (/ (+ (apply #'min ys) (apply #'max ys)) 2.0)))
        (mapcar (lambda (p) (excali--rotate-point p (cons cx cy) angle)) abs)))))

(defun excali--curved-p (element)
  "Return non-nil if linear ELEMENT is drawn as a curve."
  (and (excali--get element 'roundness)
       (not (excali--get element 'elbowed))
       (> (length (excali--get element 'points)) 2)))

(defun excali--bezier-point (seg u)
  "Return the point at parameter U of cubic SEG (P0 C1 C2 P1)."
  (pcase-let ((`(,p0 ,c1 ,c2 ,p1) seg))
    (let* ((v (- 1 u))
           (a (* v v v)) (b (* 3 v v u)) (c (* 3 v u u)) (d (* u u u)))
      (cons (+ (* a (car p0)) (* b (car c1)) (* c (car c2)) (* d (car p1)))
            (+ (* a (cdr p0)) (* b (cdr c1)) (* c (cdr c2)) (* d (cdr p1)))))))

(defun excali--path-segments (element)
  "Return ELEMENT's path as segments: (A B) lines or (P0 C1 C2 P1) curves.
Curves use roughjs's `curve' construction (tightness 0) without jitter."
  (let ((pts (vconcat (excali--linear-global-points element))))
    (if (not (excali--curved-p element))
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

(defconst excali--curve-samples 32 "Samples used to measure curve arc length.")

(defun excali--segment-samples (seg)
  "Return SEG as a list of (LENGTH-SO-FAR . POINT), starting at 0."
  (if (= (length seg) 2)
      (list (cons 0.0 (car seg))
            (cons (excali--distance (car seg) (cadr seg)) (cadr seg)))
    (let ((prev (car seg)) (len 0.0) (out (list (cons 0.0 (car seg)))))
      (dotimes (k excali--curve-samples)
        (let ((p (excali--bezier-point seg (/ (1+ k) (float excali--curve-samples)))))
          (setq len (+ len (excali--distance prev p)) prev p)
          (push (cons len p) out)))
      (nreverse out))))

(defun excali--distance (a b)
  "Return the distance between points A and B."
  (sqrt (+ (expt (- (car b) (car a)) 2) (expt (- (cdr b) (cdr a)) 2))))

(defun excali--samples-point-at (samples fraction)
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

(defun excali--arrow-label-center (arrow)
  "Return (X . Y), the default label center of ARROW (`getBoundTextElementCenter')."
  (let* ((points (excali--linear-global-points arrow)) (n (length points)))
    (cond
     ((= n 0) (cons (excali--get arrow 'x) (excali--get arrow 'y)))
     ((cl-oddp n) (nth (/ n 2) points))
     (t
      (let ((seg (nth (1- (/ n 2)) (excali--path-segments arrow))))
        (if (or (excali--get arrow 'elbowed) (= (length seg) 2))
            (let ((a (car seg)) (b (car (last seg))))
              (cons (/ (+ (car a) (car b)) 2.0) (/ (+ (cdr a) (cdr b)) 2.0)))
          (excali--samples-point-at (excali--segment-samples seg) 0.5)))))))

(defun excali--path-point-at (arrow parameter)
  "Return the point at arc-length PARAMETER (0..1) along ARROW, or nil.
Upstream `getPointAtPathParameter'."
  (let* ((segments (excali--path-segments arrow))
         (samples (mapcar #'excali--segment-samples segments))
         (lengths (mapcar (lambda (s) (car (car (last s)))) samples))
         (total (apply #'+ 0.0 lengths))
         (target (* (min 1.0 (max 0.0 parameter)) total))
         (sum 0.0) (i 0))
    (when segments
      (while (and (< i (1- (length segments)))
                  (> target (+ sum (nth i lengths))))
        (setq sum (+ sum (nth i lengths)) i (1+ i)))
      (let ((len (nth i lengths)))
        (excali--samples-point-at
         (nth i samples)
         (if (= len 0) 0.0 (min 1.0 (max 0.0 (/ (- target sum) len)))))))))

(defun excali--arrow-label-position (arrow text)
  "Return (X . Y) of label TEXT on ARROW (`computeBoundTextElementPosition')."
  (let* ((w (excali--get text 'width)) (h (excali--get text 'height))
         (position (excali--get text 'labelPosition))
         (point (cond
                 ((< (length (excali--get arrow 'points)) 2) nil)
                 ((numberp position) (or (excali--path-point-at arrow position)
                                         (excali--arrow-label-center arrow)))
                 (t (excali--arrow-label-center arrow)))))
    (if point
        (cons (- (car point) (/ w 2.0)) (- (cdr point) (/ h 2.0)))
      (cons (excali--get text 'x) (excali--get text 'y)))))

(defun excali--label-position-at (arrow point)
  "Return the `labelPosition' (0..1) of the path point of ARROW nearest POINT.
Upstream `handleBoundTextDragging'; set it on the label and call
`excali--refresh-bound-text' to move a label along its arrow."
  (let* ((samples (mapcar #'excali--segment-samples (excali--path-segments arrow)))
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
                 (d (excali--distance q point)))
            (when (< d best-d)
              (setq best-d d
                    best (+ total (car prev) (* u (- (car s) (car prev)))))))
          (setq prev s))
        (setq total (+ total (car (car (last seg)))))))
    (if (and best (> total 0)) (min 1.0 (max 0.0 (/ best total))) 0.5)))

(defun excali--arrow-label-hole (arrow)
  "Return [X Y W H], the part of ARROW cut out behind its label, or nil.
It is the label box grown by `excali-bound-text-padding' (renderElement.ts)."
  (when-let* (((excali--arrow-p arrow))
              (text (excali--bound-text-of arrow))
              ((not (string-empty-p (or (excali--get text 'text) "")))))
    (let ((pad excali-bound-text-padding))
      (vector (float (- (excali--get text 'x) pad)) (float (- (excali--get text 'y) pad))
              (float (+ (excali--get text 'width) (* 2 pad)))
              (float (+ (excali--get text 'height) (* 2 pad)))))))

;;;; Sticky notes (stickyNote.ts)

(defun excali--normalize-sticky-font-size (size)
  "Clamp SIZE for sticky notes (`normalizeStickyNoteFontSize')."
  (if (not (and (numberp size) (not (isnan (float size)))
                (/= (abs size) 1.0e+INF)))
      excali-sticky-note-fallback-font-size
    (min excali-sticky-note-max-font-size (max excali-min-font-size size))))

(defun excali--fit-sticky-font (fit base min-size max-w max-h warm-start)
  "Pick the largest font size on the sticky grid that FIT reports fitting.
Upstream `fitStickyNoteFont'; FIT maps a size to (TEXT SIZE W H)."
  (let* ((steps (max 0 (ceiling (/ (- base min-size) (float excali-sticky-note-font-step)))))
         (cache (make-hash-table))
         (at (lambda (i)
               (or (gethash i cache)
                   (puthash i (funcall fit (if (>= i steps) min-size
                                             (- base (* i excali-sticky-note-font-step))))
                            cache))))
         (fits (lambda (i) (let ((r (funcall at i)))
                             (and (<= (nth 2 r) max-w) (<= (nth 3 r) max-h))))))
    (if (= steps 0)
        (funcall at 0)
      (let ((warm (min steps (max 0 (round (/ (- base warm-start)
                                                (float excali-sticky-note-font-step))))))
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

(defun excali--update-sticky-note-layout (container text)
  "Fit TEXT's font into sticky note CONTAINER, growing it if needed.
Upstream `updateStickyNoteLayout' with the default top anchor."
  (let* ((base-w (max (excali--get container 'width) excali-sticky-note-min-size))
         (base-h (max (or (excali--get container 'baseHeight)
                          (excali--get container 'height))
                      excali-sticky-note-min-size))
         (original (or (excali--get text 'originalText) ""))
         (base-size (excali--normalize-sticky-font-size
                     (or (excali--get text 'baseFontSize) (excali--get text 'fontSize))))
         (min-size (min excali-sticky-note-min-font-size base-size))
         (max-w (max (- base-w (* 2 excali-sticky-note-padding)) 1))
         (max-h (max (- base-h excali-sticky-note-body-inset-y) 0))
         (family (or (excali--get text 'fontFamily) excali-default-font-family))
         (lh (or (excali--get text 'lineHeight) (excali--line-height family)))
         (fit (lambda (size)
                (let* ((lines (excali--wrap-text original size family max-w))
                       (m (excali--measure-string lines size family lh)))
                  (list lines size (car m) (cdr m)))))
         (blank (string-empty-p (string-trim original)))
         (fitted (if blank
                     (let ((m (excali--measure-string "" base-size family lh)))
                       (list "" base-size (car m) (cdr m)))
                   (excali--fit-sticky-font fit base-size min-size max-w max-h
                                           (excali--get text 'fontSize))))
         (height (if blank base-h
                   (max base-h (+ (nth 3 fitted) excali-sticky-note-body-inset-y)))))
    (excali--put container 'width (float base-w))
    (excali--put container 'height (float height))
    (excali--put container 'baseHeight base-h)
    (excali--touch container)
    (excali--put text 'text (nth 0 fitted))
    (excali--put text 'fontSize (nth 1 fitted))
    (excali--put text 'baseFontSize base-size)
    (excali--put text 'width (nth 2 fitted))
    (excali--put text 'height (nth 3 fitted))
    (excali--put text 'angle (or (excali--get container 'angle) 0))
    (pcase-let ((`(,x . ,y) (excali--bound-text-position container text)))
      (excali--put text 'x x)
      (excali--put text 'y y))
    (excali--touch text)))

;;;; Text elements

(defun excali--text-anchor-ratios (text-align vertical-align)
  "Return (AX . AY) anchor ratios (`getTextAnchorRatios')."
  (cons (pcase text-align ("center" 0.5) ("right" 1.0) (_ 0.0))
        (pcase vertical-align ("middle" 0.5) ("bottom" 1.0) (_ 0.0))))

(defun excali--make-text-element (x y text &rest props)
  "Return a new text element showing TEXT anchored at X, Y.
PROPS is an alist of field overrides such as fontSize, fontFamily,
textAlign, verticalAlign or containerId, as for `excali--make-element'.
Like upstream `newTextElement', X, Y is the alignment anchor: the
top-left corner for left/top aligned text."
  (let* ((element (apply #'excali--make-element "text" x y
                         (cons 'text "") (cons 'fontSize excali-default-font-size)
                         (cons 'fontFamily excali-default-font-family)
                         (cons 'textAlign "left") (cons 'verticalAlign "top")
                         (cons 'containerId :null) (cons 'originalText "")
                         (cons 'autoResize t) (cons 'lineHeight 1.25)
                         (cons 'baseFontSize :null) (cons 'labelPosition :null)
                         props))
         (family (excali--get element 'fontFamily))
         (normalized (excali--normalize-text text)))
    (unless (assq 'lineHeight props)
      (excali--put element 'lineHeight (excali--line-height family)))
    (excali--put element 'text normalized)
    (unless (assq 'originalText props)
      (excali--put element 'originalText normalized))
    (pcase-let* ((`(,size ,fam ,lh) (excali--text-font element))
                 (`(,w . ,h) (excali--measure-string normalized size fam lh))
                 (`(,ax . ,ay) (excali--text-anchor-ratios
                                (excali--get element 'textAlign)
                                (excali--get element 'verticalAlign))))
      (excali--put element 'width w)
      (excali--put element 'height h)
      (excali--put element 'x (float (- x (* w ax))))
      (excali--put element 'y (float (- y (* h ay)))))
    element))

(defun excali--adjust-xy-with-rotation (sides x y angle dx1 dy1 dx2 dy2)
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

(defun excali--keep-text-anchor (element old-w old-h)
  "Move free text ELEMENT so its alignment anchor stays after a resize.
OLD-W, OLD-H are its size before; upstream `getAdjustedDimensions'."
  (let ((w (excali--get element 'width)) (h (excali--get element 'height))
        (align (excali--get element 'textAlign))
        (x (excali--get element 'x)) (y (excali--get element 'y)))
    (pcase-let
        ((`(,nx . ,ny)
          (if (and (equal align "center")
                   (equal (excali--get element 'verticalAlign) "middle")
                   (excali--auto-resize-p element))
              (cons (- x (/ (- w old-w) 2.0)) (- y (/ (- h old-h) 2.0)))
            (excali--adjust-xy-with-rotation
             (append '(s)
                     (and (member align '("center" "left")) '(e))
                     (and (member align '("center" "right")) '(w)))
             x y (or (excali--get element 'angle) 0)
             0 0 (/ (- old-w w) 2.0) (/ (- old-h h) 2.0)))))
      (excali--put element 'x (float nx))
      (excali--put element 'y (float ny)))))

(defun excali--set-text (element text)
  "Set text ELEMENT's source TEXT and re-layout it.
Bound text re-wraps to its container, fixed-width text to its width;
free auto-resizing text keeps its alignment anchor."
  (let ((normalized (excali--normalize-text text))
        (old-w (excali--get element 'width)) (old-h (excali--get element 'height)))
    (excali--put element 'originalText normalized)
    (excali--put element 'text normalized)
    (excali--redraw-text element)
    (unless (excali--container-of element)
      (excali--keep-text-anchor element (or old-w 0) (or old-h 0))
      (excali--touch element))
    element))

(defun excali--text-scale (element scale)
  "Scale text ELEMENT's font and box by SCALE (corner resize).
Upstream `resizeSingleTextElement' for handles with n or s: the font is
not rounded.  Return nil, changing nothing, when the font would drop
below `excali-min-font-size'.  The caller positions the element."
  (let ((size (* (excali--get element 'fontSize) scale)))
    (when (>= size excali-min-font-size)
      (excali--put element 'fontSize size)
      (excali--put element 'width (* (excali--get element 'width) scale))
      (excali--put element 'height (* (excali--get element 'height) scale))
      (excali--touch element)
      element)))

(defun excali--text-set-width (element width)
  "Give text ELEMENT a fixed WIDTH, re-wrapping it (side resize).
The width is at least `getMinTextElementWidth'; `autoResize' becomes
false.  The caller positions the element."
  (pcase-let* ((`(,size ,family ,lh) (excali--text-font element))
               (width (max (excali--min-text-width size family lh) width))
               (lines (excali--wrap-text (or (excali--get element 'originalText) "")
                                        size family width)))
    (excali--put element 'text lines)
    (excali--put element 'width (float width))
    (excali--put element 'height (cdr (excali--measure-string lines size family lh)))
    (excali--put element 'autoResize :false)
    (excali--touch element)
    element))

(defun excali--text-reset-auto-resize (element)
  "Make text ELEMENT size itself to its text again (`actionTextAutoResize')."
  (pcase-let* ((`(,size ,family ,lh) (excali--text-font element))
               (original (or (excali--get element 'originalText) ""))
               (`(,w . ,h) (excali--measure-string original size family lh))
               (`(,ax . ,ay) (excali--text-anchor-ratios
                              (excali--get element 'textAlign)
                              (excali--get element 'verticalAlign))))
    (excali--put element 'x (float (+ (excali--get element 'x)
                                     (* (- (excali--get element 'width) w) ax))))
    (excali--put element 'y (float (+ (excali--get element 'y)
                                     (* (- (excali--get element 'height) h) ay))))
    (excali--put element 'autoResize t)
    (excali--put element 'text original)
    (excali--put element 'width w)
    (excali--put element 'height h)
    (excali--touch element)
    element))

(defun excali--text-font-changed (element property)
  "Re-layout text ELEMENT after its font PROPERTY was set.
PROPERTY is `fontFamily' (the line height follows the family) or
`fontSize'.  Free auto-resizing text keeps its anchor
(`offsetElementAfterFontResize')."
  (when (eq property 'fontFamily)
    (excali--put element 'lineHeight
                (excali--line-height (excali--get element 'fontFamily))))
  (when (and (eq property 'fontSize) (excali--get element 'baseFontSize))
    (excali--put element 'baseFontSize (excali--get element 'fontSize)))
  (let ((old-w (excali--get element 'width)) (old-h (excali--get element 'height)))
    (excali--redraw-text element)
    (when (and (not (excali--container-of element)) (excali--auto-resize-p element))
      (let ((dw (- (excali--get element 'width) old-w))
            (dh (- (excali--get element 'height) old-h)))
        (excali--put element 'x
                    (float (- (excali--get element 'x)
                              (pcase (excali--get element 'textAlign)
                                ("center" (/ dw 2.0)) ("right" dw) (_ 0)))))
        (excali--put element 'y (float (- (excali--get element 'y) (/ dh 2.0))))
        (excali--touch element)))))

(defun excali--add-bound-text (container &rest props)
  "Create an empty label bound to CONTAINER and return it.
The label is centered and middle-aligned, placed right after CONTAINER
in z-order, and listed in CONTAINER's `boundElements'.  PROPS are field
overrides (for example the current style)."
  (pcase-let* ((`(,cx . ,cy) (excali--container-center container))
               (text (apply #'excali--make-text-element cx cy ""
                            (cons 'textAlign "center") (cons 'verticalAlign "middle")
                            (cons 'containerId (excali--get container 'id))
                            (cons 'angle (if (excali--arrow-p container) 0
                                           (or (excali--get container 'angle) 0)))
                            props))
               (bound (excali--get container 'boundElements)))
    (excali--put container 'boundElements
                (vconcat (and (sequencep bound) bound)
                         (list (list (cons 'type "text")
                                     (cons 'id (excali--get text 'id))))))
    (excali--touch container)
    (let ((tail (memq container excali--elements)))
      (if tail
          (setcdr tail (cons text (cdr tail)))
        (setq excali--elements (append excali--elements (list text)))))
    (excali--redraw-text text container)
    text))

(defun excali--remove-bound-text (container text)
  "Delete label TEXT from the scene and from CONTAINER's `boundElements'."
  (setq excali--elements (delq text excali--elements))
  (let ((bound (excali--get container 'boundElements)))
    (when (sequencep bound)
      (excali--put container 'boundElements
                  (vconcat (seq-remove (lambda (b) (equal (alist-get 'id b)
                                                          (excali--get text 'id)))
                                       bound))))
    (excali--touch container)))

;;;; Restore (restore.ts, case "text")

;;;; Native extras

(defun excali--text-native-extras (element)
  "Return the module's text extras for ELEMENT, see `excali--native-text-extras'."
  (pcase (excali--get element 'type)
    ("text"
     (pcase-let ((`(,size ,family ,lh) (excali--text-font element)))
       (vector "vertical-offset" (excali--vertical-offset family size (* size lh)))))
    ("arrow"
     (if-let* ((hole (excali--arrow-label-hole element)))
         (vector "label-hole" hole)
       []))
    (_ [])))

(provide 'excali-text)
;;; excali-text.el ends here
