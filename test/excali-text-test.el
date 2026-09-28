;;; excali-text-test.el --- Tests for text layout  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Wrapping cases are upstream's packages/element/tests/textWrapping.test.ts,
;; which mocks every UTF-16 code unit as 10px wide; `excali-text-test--mock'
;; does the same.

(require 'ert)
(require 'excali)

(defun excali-text-test--utf16-width (line &rest _)
  "Width of LINE as upstream's jest canvas mock: 10 per UTF-16 unit."
  (float (* 10 (apply #'+ 0 (mapcar (lambda (c) (if (> c #xFFFF) 2 1))
                                     (string-to-list line))))))

(defmacro excali-text-test--mock (&rest body)
  "Run BODY with mocked text widths and a fresh scene."
  (declare (indent 0))
  `(with-temp-buffer
     (setq excali--native-cache (make-hash-table :test #'eq)
           excali--zoom 1.0
           excali--elements nil)
     (clrhash excali--char-width-cache)
     (clrhash excali--line-width-cache)
     (unwind-protect
         (cl-letf (((symbol-function 'excali--line-width)
                    #'excali-text-test--utf16-width))
           ,@body)
       (clrhash excali--char-width-cache))))

(defun excali-text-test--wrap (text width)
  "Wrap TEXT at WIDTH with the mocked font."
  (excali--wrap-text text 10 5 width))

;;;; Font metadata

(ert-deftest excali-text-test-line-heights ()
  "Default line heights per family, Excalifont for unknown ids."
  (should (= (excali--line-height 5) 1.25))
  (should (= (excali--line-height 7) 1.15))
  (should (= (excali--line-height 2) 1.15))
  (should (= (excali--line-height 3) 1.2))
  (should (= (excali--line-height 12345) 1.25)))

(ert-deftest excali-text-test-vertical-offset ()
  "getVerticalOffset centers the ascender/descender box in the line."
  (should (< (abs (- (excali--vertical-offset 5 20 25.0) 17.62)) 1e-9))
  ;; Helvetica: 2048 units per em.
  (let* ((em (/ 20.0 2048)) (lh (* 20 1.15))
         (expected (+ (* em 1577) (/ (- lh (* em 1577) (* em 471)) 2))))
    (should (< (abs (- (excali--vertical-offset 2 20 lh) expected)) 1e-9))))

(ert-deftest excali-text-test-font-families ()
  "Every font id has a Pango family list with fallbacks."
  (should (string-prefix-p "Excalifont, Xiaolai" (excali-native-font-family 5)))
  (should (string-match-p "Emoji" (excali-native-font-family 6)))
  (should (string-match-p "Emoji" (excali-native-font-family 4242)))
  (should (stringp (excali-native-font-backend)))
  (should (> (length (excali-native-font-resolve "Hello 你好" 5)) 0))
  ;; Overrides round-trip and can be reset.
  (unwind-protect
      (progn (excali-native-set-font-family 6 "Menlo")
             (should (equal (excali-native-font-family 6) "Menlo")))
    (excali-native-set-font-family 6 nil))
  (should (string-prefix-p "Nunito" (excali-native-font-family 6))))

(ert-deftest excali-text-test-register-fonts-missing-dir ()
  "Registering a missing directory is harmless."
  (should (= (excali-register-fonts "/nonexistent/excali-fonts") 0))
  (should-not (excali-native-add-fonts "/nonexistent/excali-fonts")))

;;;; Measurement

(ert-deftest excali-text-test-measure-height-is-exact ()
  "Height is lines * size * line height, never font ascent/descent."
  (should (equal (cdr (excali-native-measure-text "a\n\nb" 20 5 1.25)) 75.0))
  (should (= (cdr (excali--measure-string "a\n\nb" 20 5 1.25)) 75.0))
  (should (= (cdr (excali--measure-string "x" 36 7 1.15)) (* 36 1.15))))

(ert-deftest excali-text-test-measure-consistency ()
  "Elisp and native measurements agree; widths scale with the font."
  (dolist (family '(5 6 8 2))
    (let ((native (excali-native-measure-text "Hello\nworld!!" 20 family 1.25))
          (lisp (excali--measure-string "Hello\nworld!!" 20 family 1.25)))
      (should (< (abs (- (car native) (car lisp))) 1e-6))
      (should (= (car lisp) (max (excali--line-width "Hello" 20 family)
                                 (excali--line-width "world!!" 20 family))))))
  ;; Nearly linear: CoreText fonts may apply size-dependent tracking.
  (let ((w20 (excali--line-width "Excalidraw 你好" 20 5))
        (w40 (excali--line-width "Excalidraw 你好" 40 5)))
    (should (< (abs (- (/ w40 w20) 2)) 0.01)))
  ;; Empty text measures as a space.
  (should (= (car (excali--measure-string "" 20 5 1.25))
             (excali--line-width " " 20 5)))
  ;; Tabs count as eight spaces.
  (should (= (car (excali--measure-string "\t" 20 5 1.25))
             (excali--line-width "        " 20 5))))

(ert-deftest excali-text-test-normalize ()
  "Line ends become \\n and tabs eight spaces."
  (should (equal (excali--normalize-text "a\r\nb\rc\td") "a\nb\nc        d")))

;;;; Tokens

(ert-deftest excali-text-test-tokenize-latin ()
  (should (equal (excali--tokenize "Excalidraw is a virtual collaborative whiteboard")
                 '("Excalidraw" " " "is" " " "a" " " "virtual" " "
                   "collaborative" " " "whiteboard")))
  (should (equal (excali--tokenize "Wikimedia- Foundation, a non-profit")
                 '("Wikimedia-" " " "Foundation," " " "a" " " "non-" "profit")))
  (should (equal (excali--tokenize "99,100.99") '("99,100.99"))))

(ert-deftest excali-text-test-tokenize-emoji ()
  (should (equal (excali--tokenize "😬🌍🗺🔥☂️👩🏽‍🦰👨‍👩‍👧‍👦👩🏾‍🔬🏳️‍🌈🧔‍♀️🧑‍🤝‍🧑🙅🏽‍♂️✅0️⃣🇨🇿🦅")
                 '("😬" "🌍" "🗺" "🔥" "☂️" "👩🏽‍🦰" "👨‍👩‍👧‍👦" "👩🏾‍🔬" "🏳️‍🌈" "🧔‍♀️"
                   "🧑‍🤝‍🧑" "🙅🏽‍♂️" "✅" "0️⃣" "🇨🇿" "🦅")))
  (should (equal (excali--tokenize
                  "😬a🌍b🗺c🔥d☂️《👩🏽‍🦰》👨‍👩‍👧‍👦德👩🏾‍🔬こ🏳️‍🌈安🧔‍♀️g🧑‍🤝‍🧑h🙅🏽‍♂️e✅f0️⃣g🇨🇿10🦅#hash")
                 '("😬" "a" "🌍" "b" "🗺" "c" "🔥" "d" "☂️" "《" "👩🏽‍🦰" "》"
                   "👨‍👩‍👧‍👦" "德" "👩🏾‍🔬" "こ" "🏳️‍🌈" "安" "🧔‍♀️" "g" "🧑‍🤝‍🧑" "h"
                   "🙅🏽‍♂️" "e" "✅" "f0️⃣g" "🇨🇿" "10" "🦅" "#hash"))))

(ert-deftest excali-text-test-tokenize-nfc ()
  "Decomposed characters are composed before tokenizing."
  (let ((text (ucs-normalize-NFD-string "čでäぴέ다й한")))
    (should (> (length text) 8))
    (should (equal (excali--tokenize text) '("č" "で" "ä" "ぴ" "έ" "다" "й" "한")))))

(ert-deftest excali-text-test-tokenize-cjk ()
  "Upstream's artificial CJK sample."
  (let ((tokens (excali--tokenize
                 "《道德經》醫-醫こんにちは世界！안녕하세요세계；요』,다.다...원/달(((다)))[[1]]〚({((한))>)〛(「た」)た…[Hello] \t　World？ニューヨーク・￥3700.55す。090-1234-5678￥1,000〜＄5,000「素晴らしい！」〔重要〕＃１：Taro君30％は、（たなばた）〰￥110±￥570で20℃〜9:30〜10:00【一番】")))
    (dolist (expected '("[[1]]" "[Hello]" "World？" "Taro" "《道" "德" "經》" "醫-" "醫"
                        "こ" "ん" "に" "ち" "は" "世" "ク・" "界！" "た…" "す。" "ュ"
                        "「素" "晴" "ら" "し" "い！」" "君" "は、" "（た" "な" "ば" "た）"
                        "で" "【一" "番】" "안" "녕" "하" "세" "요" "계；" "요』," "다."
                        "다..." "원/" "달" "(((다)))" "〚({((한))>)〛" "(「た」)"
                        "￥3700.55" "090-" "1234-" "5678" "￥1,000〜" "＄5,000" "１："
                        "30％" "￥110±" "20℃〜" "9:30〜" "10:00" " " "\t" "　" "ニ"
                        "ー" "ヨ" "〰" "＃"))
      (should (member expected tokens)))))

;;;; Wrapping (upstream cases, 10px per UTF-16 unit)

(ert-deftest excali-text-test-wrap-basics ()
  (excali-text-test--mock
    (should (equal (excali-text-test--wrap "Hello Excalidraw" 100) "Hello\nExcalidraw"))
    (dolist (width (list 0.0e+NaN -1 1.0e+INF))
      (should (equal (excali-text-test--wrap "Hello Excalidraw" width) "Hello Excalidraw")))
    (should (equal (excali-text-test--wrap "Hello😀" 10) "H\ne\nl\nl\no\n😀"))
    (should (equal (excali-text-test--wrap "don't wrap this number 99,100.99" 300)
                   "don't wrap this number\n99,100.99"))))

(ert-deftest excali-text-test-wrap-whitespace ()
  (excali-text-test--mock
    (should (equal (excali-text-test--wrap "Hello     " 50) "Hello"))
    (should (equal (excali-text-test--wrap "Hello     " 60) "Hello "))
    (should (equal (excali-text-test--wrap "  Hello  World" 90) "  Hello\nWorld"))
    (should (equal (excali-text-test--wrap "   Hello  World            " 90)
                   "   Hello\nWorld    "))
    (should (equal (excali-text-test--wrap "Hello   Wo rl  d                     " 100)
                   "Hello   Wo\nrl  d     "))
    (let ((text "  \t   Hello world"))
      (should (equal (excali-text-test--wrap text 120) "  \t   Hello\nworld"))
      (should (equal (excali-text-test--wrap text 60) "\nHello\nworld"))
      (should (equal (excali-text-test--wrap text 30) "\nHel\nlo\nwor\nld")))
    (should (equal (excali-text-test--wrap "Hello whats up     " 190) "Hello whats up     "))
    (let ((text "Hippopotomonstrosesquippedaliophobia        ??????"))
      (should (equal (excali-text-test--wrap text 400)
                     "Hippopotomonstrosesquippedaliophobia\n??????"))
      (should (equal (excali-text-test--wrap text 300)
                     "Hippopotomonstrosesquippedalio\nphobia        ??????"))
      (should (equal (excali-text-test--wrap text 180)
                     "Hippopotomonstrose\nsquippedaliophobia\n??????")))))

(ert-deftest excali-text-test-wrap-emoji-and-hyphens ()
  (excali-text-test--mock
    (should (equal (excali-text-test--wrap "😀🗺🔥👩🏽‍🦰👨‍👩‍👧‍👦🇨🇿" 1)
                   "😀\n🗺\n🔥\n👩🏽‍🦰\n👨‍👩‍👧‍👦\n🇨🇿"))
    (should (equal (excali-text-test--wrap
                    "Wikipedia is hosted by Wikimedia- Foundation, a non-profit organization that also hosts a range-of other projects"
                    110)
                   "Wikipedia\nis hosted\nby\nWikimedia-\nFoundation,\na non-\nprofit\norganizatio\nn that also\nhosts a\nrange-of\nother\nprojects"))
    (should (equal (excali-text-test--wrap "Hello thereusing-now" 100)
                   "Hello\nthereusing\n-now"))
    (let ((text "\tA) one tab\t\t- two tabs        - 8 spaces"))
      (should (equal (excali-text-test--wrap text 100)
                     "\tA) one\ntab\t\t- two\ntabs\n- 8 spaces"))
      (should (equal (excali-text-test--wrap text 50)
                     "\tA)\none\ntab\n- two\ntabs\n- 8\nspace\ns")))))

(ert-deftest excali-text-test-wrap-cjk ()
  (excali-text-test--mock
    (let ((text "안녕하세요こんにちは世界ｺﾝﾆﾁハ你好"))
      (should (equal (excali-text-test--wrap text 10)
                     "안\n녕\n하\n세\n요\nこ\nん\nに\nち\nは\n世\n界\nｺ\nﾝ\nﾆ\nﾁ\nハ\n你\n好"))
      (should (equal (excali-text-test--wrap text 30)
                     "안녕하\n세요こ\nんにち\nは世界\nｺﾝﾆ\nﾁハ你\n好")))
    (let ((text "a醫 醫      bb  你好  world-i-😀🗺🔥"))
      (should (equal (excali-text-test--wrap text 150)
                     "a醫 醫      bb  你\n好  world-i-😀🗺\n🔥"))
      (should (equal (excali-text-test--wrap text 50)
                     "a醫 醫\nbb  你\n好\nworld\n-i-😀\n🗺🔥"))
      (should (equal (excali-text-test--wrap text 30)
                     "a醫\n醫\nbb\n你好\nwor\nld-\ni-\n😀\n🗺\n🔥")))
    (should (equal (excali-text-test--wrap "HelloたWorld" 50) "Hello\nた\nWorld"))
    (should (equal (excali-text-test--wrap "HelloたWorld" 60) "Helloた\nWorld"))
    (should (equal (excali-text-test--wrap "こんにちは〃世界" 50) "こんにちは\n〃世界"))
    (should (equal (excali-text-test--wrap "こんにちは〃世界" 60) "こんにちは〃\n世界"))
    (should (equal (excali-text-test--wrap "Hello た。" 70) "Hello\nた。"))
    (should (equal (excali-text-test--wrap "Hello「たWorld」" 60) "Hello\n「た\nWorld」"))
    (should (equal (excali-text-test--wrap "「Helloた」World" 70) "「Hello\nた」World"))))

(ert-deftest excali-text-test-wrap-cjk-sentences ()
  (excali-text-test--mock
    (let ((text "中国你好！这是一个测试。\n我们来看看：人民币¥1234「很贵」\n（括号）、逗号，句号。空格 换行　全角符号…—"))
      (should (equal (excali-text-test--wrap text 80)
                     "中国你好！这是一\n个测试。\n我们来看看：人民\n币¥1234「很\n贵」\n（括号）、逗号，\n句号。空格 换行\n全角符号…—"))
      (should (equal (excali-text-test--wrap text 50)
                     "中国你好！\n这是一个测\n试。\n我们来看\n看：人民币\n¥1234\n「很贵」\n（括号）、\n逗号，句\n号。空格\n换行　全角\n符号…—")))
    (let ((text "한국 안녕하세요! 이것은 테스트입니다.\n우리 보자: 원화₩1234「비싸다」\n(괄호), 쉼표, 마침표.\n공백 줄바꿈　전각기호…—"))
      (should (equal (excali-text-test--wrap text 80)
                     "한국 안녕하세\n요! 이것은 테\n스트입니다.\n우리 보자: 원\n화₩1234「비\n싸다」\n(괄호), 쉼\n표, 마침표.\n공백 줄바꿈　전\n각기호…—"))
      (should (equal (excali-text-test--wrap text 60)
                     "한국 안녕하\n세요! 이것\n은 테스트입\n니다.\n우리 보자:\n원화\n₩1234\n「비싸다」\n(괄호),\n쉼표, 마침\n표.\n공백 줄바꿈\n전각기호…—")))))

(ert-deftest excali-text-test-wrap-lines-and-long-text ()
  (excali-text-test--mock
    (pcase-dolist (`(,width ,result)
                   '((70 "Hello\nwhats\nup") (15 "H\ne\nl\nl\no\nw\nh\na\nt\ns\nu\np")
                     (130 "Hello whats\nup") (240 "Hello whats up")
                     (50 "Hello\nwhats\nup")))
      (should (equal (excali-text-test--wrap "Hello whats up" width) result)))
    (pcase-dolist (`(,width ,result)
                   '((70 "Hello\n  whats\nup")
                     (15 "H\ne\nl\nl\no\n\nw\nh\na\nt\ns\nu\np")
                     (140 "Hello\n  whats up")))
      (should (equal (excali-text-test--wrap "Hello\n  whats up" width) result)))
    (let ((text "hellolongtextthisiswhatsupwithyouIamtypingggggandtypinggg break it now"))
      (should (equal (excali-text-test--wrap text 160)
                     "hellolongtextthi\nsiswhatsupwithyo\nuIamtypingggggan\ndtypinggg break\nit now"))
      (should (equal (excali-text-test--wrap text 120)
                     "hellolongtex\ntthisiswhats\nupwithyouIam\ntypingggggan\ndtypinggg\nbreak it now"))
      (should (equal (excali-text-test--wrap text 590)
                     "hellolongtextthisiswhatsupwithyouIamtypingggggandtypinggg\nbreak it now")))
    (should (equal (excali-text-test--wrap "A\n\nB" 100) "A\n\nB"))))

;;;; Text elements

(ert-deftest excali-text-test-new-text-element ()
  "newTextElement: defaults, per-family line height and anchoring."
  (excali-text-test--mock
    (let ((e (excali--make-text-element 10 20 "Hi\tthere")))
      (should (equal (excali--get e 'text) "Hi        there"))
      (should (equal (excali--get e 'originalText) "Hi        there"))
      (should (eq (excali--get e 'autoResize) t))
      (should (= (excali--get e 'lineHeight) 1.25))
      (should (= (excali--get e 'width) 150.0))
      (should (= (excali--get e 'height) 25.0))
      (should (= (excali--get e 'x) 10.0))
      (should (eq (alist-get 'baseFontSize e) :null))
      (should (eq (alist-get 'labelPosition e) :null)))
    (let ((e (excali--make-text-element 100 100 "abcd" (cons 'fontFamily 7)
                                       (cons 'textAlign "center")
                                       (cons 'verticalAlign "middle"))))
      (should (= (excali--get e 'lineHeight) 1.15))
      ;; The anchor point is the text center.
      (should (= (excali--get e 'x) 80.0))
      (should (= (excali--get e 'y) (- 100 (/ (* 20 1.15) 2)))))))

(ert-deftest excali-text-test-set-text-keeps-anchor ()
  "Editing free text keeps its alignment anchor (getAdjustedDimensions)."
  (excali-text-test--mock
    (let ((left (excali--make-text-element 0 0 "ab"))
          (center (excali--make-text-element 0 0 "ab" (cons 'textAlign "center")))
          (right (excali--make-text-element 0 0 "ab" (cons 'textAlign "right")))
          (middle (excali--make-text-element 0 0 "ab" (cons 'textAlign "center")
                                            (cons 'verticalAlign "middle"))))
      (let ((cx (+ (excali--get center 'x) 10)) (rx (+ (excali--get right 'x) 20))
            (my (+ (excali--get middle 'y) 12.5)))
        (dolist (e (list left center right middle)) (excali--set-text e "abcd\nef"))
        (should (= (excali--get left 'x) 0.0))
        (should (= (+ (excali--get center 'x) 20) cx))
        (should (= (+ (excali--get right 'x) 40) rx))
        (should (= (excali--get center 'y) 0.0))
        (should (= (+ (excali--get middle 'y) 25) my))))))

(ert-deftest excali-text-test-fixed-width-text ()
  "autoResize false wraps to the width; side resize and reset."
  (excali-text-test--mock
    (let ((e (excali--make-text-element 0 0 "Hello whats up")))
      (excali--text-set-width e 70)
      (should (eq (alist-get 'autoResize e) :false))
      (should (equal (excali--get e 'text) "Hello\nwhats\nup"))
      (should (= (excali--get e 'width) 70.0))
      (should (= (excali--get e 'height) 75.0))
      (should (equal (excali--get e 'originalText) "Hello whats up"))
      ;; Editing re-wraps at the fixed width.
      (excali--set-text e "Hello whats up doc")
      (should (equal (excali--get e 'text) "Hello\nwhats\nup doc"))
      (should (= (excali--get e 'width) 70.0))
      ;; The width never drops below a space plus padding.
      (excali--text-set-width e 1)
      (should (= (excali--get e 'width) 20.0))
      (excali--text-reset-auto-resize e)
      (should (eq (excali--get e 'autoResize) t))
      (should (equal (excali--get e 'text) "Hello whats up doc"))
      (should (= (excali--get e 'width) 180.0))
      (should (= (excali--get e 'height) 25.0)))))

(ert-deftest excali-text-test-scale ()
  "Corner resize scales the font without rounding."
  (excali-text-test--mock
    (let ((e (excali--make-text-element 0 0 "Hello")))
      (excali--text-scale e 1.5)
      (should (= (excali--get e 'fontSize) 30.0))
      (should (= (excali--get e 'width) 75.0))
      (should (= (excali--get e 'height) 37.5))
      (should-not (excali--text-scale e 0.01))
      (should (= (excali--get e 'fontSize) 30.0)))))

(ert-deftest excali-text-test-font-changes ()
  "Changing the family resets the line height; size keeps the anchor."
  (excali-text-test--mock
    (let ((e (excali--make-text-element 0 0 "Hello" (cons 'textAlign "center"))))
      (excali--put e 'fontFamily 7)
      (excali--text-font-changed e 'fontFamily)
      (should (= (excali--get e 'lineHeight) 1.15))
      (should (= (excali--get e 'height) 23.0))
      (let ((cx (+ (excali--get e 'x) (/ (excali--get e 'width) 2.0))))
        (excali--put e 'fontSize 40)
        (excali--text-font-changed e 'fontSize)
        (should (= (+ (excali--get e 'x) (/ (excali--get e 'width) 2.0)) cx))))))

;;;; Bound text

(defun excali-text-test--container (type w h &rest props)
  "Add a TYPE container of W x H at the origin to the scene."
  (let ((c (apply #'excali--make-element type 0 0 (cons 'width (float w))
                  (cons 'height (float h)) props)))
    (setq excali--elements (append excali--elements (list c)))
    c))

(defun excali-text-test--label (container text)
  "Give CONTAINER the label TEXT and return it."
  (let ((label (excali--add-bound-text container)))
    (excali--set-text label text)
    label))

(defun excali-text-test--xy (element)
  "Return (X Y) of ELEMENT."
  (list (excali--get element 'x) (excali--get element 'y)))

(defun excali-text-test--near (a b)
  "Return non-nil if the number lists A and B agree to 1e-6."
  (cl-every (lambda (x y) (< (abs (- x y)) 1e-6)) a b))

(ert-deftest excali-text-test-bound-text-placement ()
  "Centered labels in every container type (computeBoundTextPosition)."
  (excali-text-test--mock
    (let* ((rect (excali-text-test--container "rectangle" 200 100))
           (label (excali-text-test--label rect "Hello")))
      (should (equal (excali-text-test--xy label) '(75.0 37.5)))
      (should (equal (excali--get label 'containerId) (excali--get rect 'id)))
      (should (eq (excali--bound-text-of rect) label))
      (should (eq (excali--container-of label) rect))
      ;; The label directly follows its container.
      (should (eq (cadr (memq rect excali--elements)) label))
      (should (= (excali--bound-text-max-width rect label) 190)))
    (let* ((ellipse (excali-text-test--container "ellipse" 200 100))
           (label (excali-text-test--label ellipse "Hello"))
           (ox (+ 5 (* 100 (- 1 (/ (sqrt 2) 2)))))
           (oy (+ 5 (* 50 (- 1 (/ (sqrt 2) 2))))))
      (should (= (excali--bound-text-max-width ellipse label) 131))
      (should (= (excali--bound-text-max-height ellipse label) 61))
      (should (excali-text-test--near (excali-text-test--xy label)
                                     (list (+ ox 65.5 -25) (+ oy 30.5 -12.5)))))
    (let* ((diamond (excali-text-test--container "diamond" 200 100))
           (label (excali-text-test--label diamond "Hello")))
      (should (= (excali--bound-text-max-width diamond label) 90))
      (should (equal (excali-text-test--xy label) '(75.0 37.5))))
    (let* ((note (excali-text-test--container "stickynote" 250 250))
           (label (excali-text-test--label note "Hi")))
      (should (= (excali--bound-text-max-width note label) 218))
      (should (= (excali--bound-text-max-height note label) 198))
      (should (= (excali--get label 'x) (+ 16 (- 109 (/ (excali--get label 'width) 2.0)))))
      (should (= (excali--get label 'fontSize) 20)))))

(ert-deftest excali-text-test-bound-text-alignment ()
  "Top/bottom and left/right alignment inside a rectangle."
  (excali-text-test--mock
    (let* ((rect (excali-text-test--container "rectangle" 200 100))
           (label (excali--add-bound-text rect)))
      (excali--put label 'verticalAlign "top")
      (excali--put label 'textAlign "left")
      (excali--set-text label "Hello")
      (should (equal (excali-text-test--xy label) '(5.0 5.0)))
      (excali--put label 'verticalAlign "bottom")
      (excali--put label 'textAlign "right")
      (excali--redraw-text label)
      (should (equal (excali-text-test--xy label) '(145.0 70.0))))))

(ert-deftest excali-text-test-bound-text-rotated ()
  "A rotated container rotates its label about the text box center."
  (excali-text-test--mock
    (let* ((rect (excali-text-test--container "rectangle" 200 100 (cons 'angle (/ float-pi 2))))
           (label (excali-text-test--label rect "Hello")))
      ;; Centered labels stay centered.
      (should (excali-text-test--near (excali-text-test--xy label) '(75.0 37.5)))
      (should (= (excali--get label 'angle) (/ float-pi 2))))))

(ert-deftest excali-text-test-container-grows ()
  "Containers grow to fit wrapped text (computeContainerDimensionForBoundText)."
  (excali-text-test--mock
    (pcase-dolist (`(,type ,w ,h ,expected-h)
                   '(("rectangle" 100 40 60.0)
                     ("diamond" 200 40 120.0)
                     ("ellipse" 142 40 85.0)))
      (let* ((c (excali-text-test--container type w h))
             (label (excali-text-test--label c "a b c d e f g h")))
        (should (equal (excali--get label 'text) "a b c d e\nf g h"))
        (should (= (excali--get c 'height) expected-h))
        (should (= (excali--get c 'width) (float w)))))
    ;; A character wider than the box widens it.
    (let* ((c (excali-text-test--container "rectangle" 12 100))
           (label (excali-text-test--label c "ab")))
      (should (equal (excali--get label 'text) "a\nb"))
      (should (= (excali--get c 'width) 20.0)))
    ;; Containers never shrink on redraw.
    (let* ((c (excali-text-test--container "rectangle" 100 40))
           (label (excali-text-test--label c "a b c d e f g h")))
      (excali--set-text label "a")
      (should (= (excali--get c 'height) 60.0)))))

(ert-deftest excali-text-test-edit-preview-shrinks-back ()
  "While editing, a container grows and shrinks back to its original height."
  (excali-text-test--mock
    (let* ((c (excali-text-test--container "rectangle" 100 40))
           (label (excali-text-test--label c "a"))
           (geometry (excali--container-geometry c)))
      (excali--preview-text label c geometry "a b c d e f g h")
      (should (= (excali--get c 'height) 60.0))
      (excali--preview-text label c geometry "a b")
      (should (= (excali--get c 'height) 40.0)))))

(ert-deftest excali-text-test-layout-after-resize ()
  "handleBindTextResize: re-wrap on width change, grow from the right edge."
  (excali-text-test--mock
    (let* ((c (excali-text-test--container "rectangle" 200 40))
           (label (excali-text-test--label c "a b c d e f g h")))
      (should (equal (excali--get label 'text) "a b c d e f g h"))
      ;; Narrowed by the east handle: text re-wraps, container grows down.
      (excali--put c 'width 100.0)
      (excali--layout-bound-text c 'e)
      (should (equal (excali--get label 'text) "a b c d e\nf g h"))
      (should (= (excali--get c 'height) 60.0))
      (should (= (excali--get c 'y) 0.0))
      ;; Dragging the north handle grows upwards, keeping the bottom.
      (excali--put c 'height 40.0)
      (excali--layout-bound-text c 'n)
      (should (= (excali--get c 'height) 60.0))
      (should (= (excali--get c 'y) -20.0))
      (should (equal (excali-text-test--xy label) '(5.0 -15.0)))
      t)))

(ert-deftest excali-text-test-arrow-labels ()
  "Arrow labels: default center, labelPosition, max width and hole."
  (excali-text-test--mock
    (let* ((arrow (excali--make-element "arrow" 0 0 (cons 'points [[0.0 0.0] [200.0 0.0]])
                                       (cons 'width 200.0) (cons 'height 0.0)))
           (_ (setq excali--elements (list arrow)))
           (label (excali-text-test--label arrow "Hello")))
      (should (equal (excali-text-test--xy label) '(75.0 -12.5)))
      (should (= (excali--bound-text-max-width arrow label) 220))
      (should (= (excali--get label 'angle) 0))
      (should (equal (excali--arrow-label-hole arrow) [70.0 -17.5 60.0 35.0]))
      (should (equal (excali--native-text-extras arrow)
                     (vector "label-hole" [70.0 -17.5 60.0 35.0])))
      ;; Arrows never grow in height for their label.
      (should (= (excali--get arrow 'height) 0.0)))
    (let* ((arrow (excali--make-element
                   "arrow" 0 0 (cons 'points [[0.0 0.0] [100.0 0.0] [100.0 100.0]])))
           (_ (setq excali--elements (list arrow)))
           (label (excali-text-test--label arrow "ab")))
      ;; Odd point count: the middle point.
      (should (equal (excali-text-test--xy label) '(90.0 -12.5)))
      (excali--put label 'labelPosition 0.75)
      (excali--refresh-bound-text arrow)
      (should (excali-text-test--near (excali-text-test--xy label) '(90.0 37.5)))
      (should (< (abs (- (excali--label-position-at arrow '(110.0 . 50.0)) 0.75)) 1e-9)))
    (let* ((arrow (excali--make-element
                   "arrow" 0 0
                   (cons 'points [[0.0 0.0] [100.0 0.0] [100.0 100.0] [200.0 100.0]])))
           (_ (setq excali--elements (list arrow)))
           (label (excali-text-test--label arrow "ab")))
      ;; Even point count: the middle of the middle segment.
      (should (equal (excali-text-test--xy label) '(90.0 37.5))))))

(ert-deftest excali-text-test-remove-bound-text ()
  "Removing a label clears it from the scene and the container."
  (excali-text-test--mock
    (let* ((rect (excali-text-test--container "rectangle" 200 100))
           (label (excali-text-test--label rect "Hello")))
      (excali--remove-bound-text rect label)
      (should-not (memq label excali--elements))
      (should-not (excali--bound-text-of rect))
      (should (equal (excali--get rect 'boundElements) [])))))

(ert-deftest excali-text-test-style-reaches-label ()
  "Setting a font property on a selected shape changes its label."
  (excali-text-test--mock
    (setq excali--backend nil)
    (let* ((rect (excali-text-test--container "rectangle" 200 100))
           (label (excali-text-test--label rect "Hello")))
      (excali--select (list rect))
      (excali-set-style 'fontSize 40)
      (should (= (excali--get label 'fontSize) 40))
      (should (= (excali--get label 'height) 50.0))
      (should-not (assq 'fontSize rect))
      (excali-set-style 'verticalAlign "top")
      (should (= (excali--get label 'y) 5.0)))))

(ert-deftest excali-text-test-sticky-note-fits-font ()
  "Sticky-note text takes the largest grid font size that fits."
  (excali-text-test--mock
    (let* ((note (excali-text-test--container "stickynote" 250 250))
           (label (excali--add-bound-text note (cons 'fontSize 36))))
      (excali--set-text label (string-join (make-list 30 "word") " "))
      (let ((size (excali--get label 'fontSize)))
        (should (= 0 (mod (- 36 size) 2)))
        (should (<= (excali--get label 'height) 198))
        (should (<= (excali--get label 'width) 218))
        (should (= (excali--get label 'baseFontSize) 36))
        ;; One step larger would not fit.
        (let* ((bigger (+ size 2))
               (lines (excali--wrap-text (excali--get label 'originalText) bigger 5 218))
               (m (excali--measure-string lines bigger 5 1.25)))
          (should (or (> (car m) 218) (> (cdr m) 198))))))))

;;;; Commands

(defmacro excali-text-test--typing (result &rest body)
  "Run BODY with minibuffer text editing stubbed to preview and return RESULT."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'excali--edit-text-live)
              (lambda (element)
                (let* ((container (excali--container-of element))
                       (geometry (excali--container-geometry container)))
                  (if ,result
                      (excali--preview-text element container geometry ,result)
                    (when container (excali--restore-geometry container geometry))))
                ,result))
             ((symbol-function 'excali--render) #'ignore))
     ,@body))

(ert-deftest excali-text-test-edit-container-text ()
  "Enter on a shape adds a centered label; cancelling leaves no trace."
  (excali-text-test--mock
    (excali--load-current-style nil)
    (let ((rect (excali-text-test--container "rectangle" 100 40)))
      (excali-text-test--typing nil
        (excali--select (list rect))
        (excali-edit-text))
      (should (= (length excali--elements) 1))
      (should (eq (alist-get 'boundElements rect) :null))
      (should (= (excali--get rect 'height) 40.0))
      (excali-text-test--typing "a b c d e f g h"
        (excali--select (list rect))
        (excali-edit-text))
      (let ((label (excali--bound-text-of rect)))
        (should label)
        (should (equal (excali--get label 'textAlign) "center"))
        (should (equal (excali--get label 'verticalAlign) "middle"))
        (should (equal (excali--get label 'originalText) "a b c d e f g h"))
        (should (= (excali--get rect 'height) 60.0))
        ;; Aborting an edit of an existing label keeps it.
        (excali-text-test--typing nil
          (excali--select (list rect))
          (excali-edit-text))
        (should (eq (excali--bound-text-of rect) label))
        ;; Clearing the text deletes the label.
        (excali-text-test--typing "  "
          (excali--select (list rect))
          (excali-edit-text))
        (should-not (excali--bound-text-of rect))
        (should-not (memq label excali--elements))))))

;;;; Round trip and rendering

(ert-deftest excali-text-test-roundtrip-fields ()
  "Text fields survive saving and loading."
  (let ((out (make-temp-file "excali" nil ".excalidraw")))
    (unwind-protect
        (excali-text-test--mock
          (let* ((rect (excali-text-test--container "rectangle" 200 100))
                 (label (excali-text-test--label rect "Hello world")))
            (excali--put label 'labelPosition 0.3)
            (setq excali--doc (excali--empty-doc) excali--file out)
            (excali-save)
            (let* ((doc (excali--read-file out))
                   (saved (aref (alist-get 'elements doc) 1)))
              (dolist (key '(text originalText fontSize fontFamily textAlign
                                  verticalAlign containerId lineHeight x y width height))
                ;; Saving writes numbers like JavaScript, so 45.0 reads back as 45.
                (let ((a (alist-get key saved)) (b (alist-get key label)))
                  (should (if (and (numberp a) (numberp b)) (= a b) (equal a b)))))
              (should (eq (alist-get 'autoResize saved) t))
              (should (eq (alist-get 'baseFontSize saved) :null))
              (should (= (alist-get 'labelPosition saved) 0.3))
              (should (equal (alist-get 'boundElements (aref (alist-get 'elements doc) 0))
                             (vector (list (cons 'type "text")
                                           (cons 'id (excali--get label 'id)))))))))
      (delete-file out))))

(ert-deftest excali-text-test-restore-legacy ()
  "Legacy text gets defaults and keeps its effective line height.
Restore is `excali--restore-text' in excali-restore.el."
  (let ((e (excali--restore-text
            (list (cons 'type "text") (cons 'text "a\nb") (cons 'fontSize 20)
                  (cons 'fontFamily 1) (cons 'height 60.0))
            nil)))
    (should (= (excali--get e 'lineHeight) 1.5))
    (should (equal (excali--get e 'originalText) "a\nb"))
    (should (equal (excali--get e 'textAlign) "left"))
    (should (equal (excali--get e 'verticalAlign) "top"))
    (should (eq (excali--get e 'autoResize) t)))
  (let ((e (excali--restore-text
            (list (cons 'type "text") (cons 'text "a") (cons 'fontFamily 7)
                  (cons 'labelPosition 3))
            nil)))
    (should (= (excali--get e 'fontSize) 20))
    (should (= (excali--get e 'lineHeight) 1.15))
    (should (= (excali--get e 'labelPosition) 1.0))))

(ert-deftest excali-text-test-native-extras ()
  "Text elements pass their baseline offset to the module."
  (excali-text-test--mock
    (let ((e (excali--make-text-element 0 0 "x")))
      (should (equal (excali--native-text-extras e)
                     (vector "vertical-offset" (excali--vertical-offset 5 20 25.0)))))
    (should (equal (excali--native-text-extras
                    (excali--make-element "rectangle" 0 0))
                   []))))

(ert-deftest excali-text-test-render-lines ()
  "Each line is drawn: more lines change more pixels; alignment moves ink."
  (with-temp-buffer
    (setq excali--native-cache (make-hash-table :test #'eq) excali--zoom 1.0)
    (let* ((render (lambda (elements)
                     (let ((fb (excali-native-fb-create 300 120)))
                       (setq excali--elements elements)
                       (clrhash excali--native-cache)
                       (excali-native-fb-render fb 1.0 1.0 0.0 0.0
                                               (excali--visible-elements) nil)
                       fb)))
           (blank (funcall render nil))
           (one (funcall render (list (excali--make-text-element 10 10 "Hello"))))
           (two (funcall render (list (excali--make-text-element 10 10 "Hello\nHello"))))
           (right (let ((e (excali--make-text-element 10 10 "Hello")))
                    (excali--put e 'textAlign "right")
                    (excali--put e 'width 250.0)
                    (funcall render (list e)))))
      (should (> (excali-native-fb-mean-diff blank one) 0))
      (should (> (excali-native-fb-mean-diff blank two)
                 (* 1.8 (excali-native-fb-mean-diff blank one))))
      (should (> (excali-native-fb-mean-diff one right) 0)))))

;;; excali-text-test.el ends here
