;;; excal-text-test.el --- Tests for text layout  -*- lexical-binding: t; -*-

;; Wrapping cases are upstream's packages/element/tests/textWrapping.test.ts,
;; which mocks every UTF-16 code unit as 10px wide; `excal-text-test--mock'
;; does the same.

(require 'ert)
(require 'excal)

(defun excal-text-test--utf16-width (line &rest _)
  "Width of LINE as upstream's jest canvas mock: 10 per UTF-16 unit."
  (float (* 10 (apply #'+ 0 (mapcar (lambda (c) (if (> c #xFFFF) 2 1))
                                     (string-to-list line))))))

(defmacro excal-text-test--mock (&rest body)
  "Run BODY with mocked text widths and a fresh scene."
  (declare (indent 0))
  `(with-temp-buffer
     (setq excal--native-cache (make-hash-table :test #'eq)
           excal--zoom 1.0
           excal--elements nil)
     (clrhash excal--char-width-cache)
     (clrhash excal--line-width-cache)
     (unwind-protect
         (cl-letf (((symbol-function 'excal--line-width)
                    #'excal-text-test--utf16-width))
           ,@body)
       (clrhash excal--char-width-cache))))

(defun excal-text-test--wrap (text width)
  "Wrap TEXT at WIDTH with the mocked font."
  (excal--wrap-text text 10 5 width))

;;;; Font metadata

(ert-deftest excal-text-test-line-heights ()
  "Default line heights per family, Excalifont for unknown ids."
  (should (= (excal--line-height 5) 1.25))
  (should (= (excal--line-height 7) 1.15))
  (should (= (excal--line-height 2) 1.15))
  (should (= (excal--line-height 3) 1.2))
  (should (= (excal--line-height 12345) 1.25)))

(ert-deftest excal-text-test-vertical-offset ()
  "getVerticalOffset centers the ascender/descender box in the line."
  (should (< (abs (- (excal--vertical-offset 5 20 25.0) 17.62)) 1e-9))
  ;; Helvetica: 2048 units per em.
  (let* ((em (/ 20.0 2048)) (lh (* 20 1.15))
         (expected (+ (* em 1577) (/ (- lh (* em 1577) (* em 471)) 2))))
    (should (< (abs (- (excal--vertical-offset 2 20 lh) expected)) 1e-9))))

(ert-deftest excal-text-test-font-families ()
  "Every font id has a Pango family list with fallbacks."
  (should (string-prefix-p "Excalifont, Xiaolai" (excal-native-font-family 5)))
  (should (string-match-p "Emoji" (excal-native-font-family 6)))
  (should (string-match-p "Emoji" (excal-native-font-family 4242)))
  (should (stringp (excal-native-font-backend)))
  (should (> (length (excal-native-font-resolve "Hello 你好" 5)) 0))
  ;; Overrides round-trip and can be reset.
  (unwind-protect
      (progn (excal-native-set-font-family 6 "Menlo")
             (should (equal (excal-native-font-family 6) "Menlo")))
    (excal-native-set-font-family 6 nil))
  (should (string-prefix-p "Nunito" (excal-native-font-family 6))))

(ert-deftest excal-text-test-register-fonts-missing-dir ()
  "Registering a missing directory is harmless."
  (should (= (excal-register-fonts "/nonexistent/excal-fonts") 0))
  (should-not (excal-native-add-fonts "/nonexistent/excal-fonts")))

;;;; Measurement

(ert-deftest excal-text-test-measure-height-is-exact ()
  "Height is lines * size * line height, never font ascent/descent."
  (should (equal (cdr (excal-native-measure-text "a\n\nb" 20 5 1.25)) 75.0))
  (should (= (cdr (excal--measure-string "a\n\nb" 20 5 1.25)) 75.0))
  (should (= (cdr (excal--measure-string "x" 36 7 1.15)) (* 36 1.15))))

(ert-deftest excal-text-test-measure-consistency ()
  "Elisp and native measurements agree; widths scale with the font."
  (dolist (family '(5 6 8 2))
    (let ((native (excal-native-measure-text "Hello\nworld!!" 20 family 1.25))
          (lisp (excal--measure-string "Hello\nworld!!" 20 family 1.25)))
      (should (< (abs (- (car native) (car lisp))) 1e-6))
      (should (= (car lisp) (max (excal--line-width "Hello" 20 family)
                                 (excal--line-width "world!!" 20 family))))))
  ;; Nearly linear: CoreText fonts may apply size-dependent tracking.
  (let ((w20 (excal--line-width "Excalidraw 你好" 20 5))
        (w40 (excal--line-width "Excalidraw 你好" 40 5)))
    (should (< (abs (- (/ w40 w20) 2)) 0.01)))
  ;; Empty text measures as a space.
  (should (= (car (excal--measure-string "" 20 5 1.25))
             (excal--line-width " " 20 5)))
  ;; Tabs count as eight spaces.
  (should (= (car (excal--measure-string "\t" 20 5 1.25))
             (excal--line-width "        " 20 5))))

(ert-deftest excal-text-test-normalize ()
  "Line ends become \\n and tabs eight spaces."
  (should (equal (excal--normalize-text "a\r\nb\rc\td") "a\nb\nc        d")))

;;;; Tokens

(ert-deftest excal-text-test-tokenize-latin ()
  (should (equal (excal--tokenize "Excalidraw is a virtual collaborative whiteboard")
                 '("Excalidraw" " " "is" " " "a" " " "virtual" " "
                   "collaborative" " " "whiteboard")))
  (should (equal (excal--tokenize "Wikimedia- Foundation, a non-profit")
                 '("Wikimedia-" " " "Foundation," " " "a" " " "non-" "profit")))
  (should (equal (excal--tokenize "99,100.99") '("99,100.99"))))

(ert-deftest excal-text-test-tokenize-emoji ()
  (should (equal (excal--tokenize "😬🌍🗺🔥☂️👩🏽‍🦰👨‍👩‍👧‍👦👩🏾‍🔬🏳️‍🌈🧔‍♀️🧑‍🤝‍🧑🙅🏽‍♂️✅0️⃣🇨🇿🦅")
                 '("😬" "🌍" "🗺" "🔥" "☂️" "👩🏽‍🦰" "👨‍👩‍👧‍👦" "👩🏾‍🔬" "🏳️‍🌈" "🧔‍♀️"
                   "🧑‍🤝‍🧑" "🙅🏽‍♂️" "✅" "0️⃣" "🇨🇿" "🦅")))
  (should (equal (excal--tokenize
                  "😬a🌍b🗺c🔥d☂️《👩🏽‍🦰》👨‍👩‍👧‍👦德👩🏾‍🔬こ🏳️‍🌈安🧔‍♀️g🧑‍🤝‍🧑h🙅🏽‍♂️e✅f0️⃣g🇨🇿10🦅#hash")
                 '("😬" "a" "🌍" "b" "🗺" "c" "🔥" "d" "☂️" "《" "👩🏽‍🦰" "》"
                   "👨‍👩‍👧‍👦" "德" "👩🏾‍🔬" "こ" "🏳️‍🌈" "安" "🧔‍♀️" "g" "🧑‍🤝‍🧑" "h"
                   "🙅🏽‍♂️" "e" "✅" "f0️⃣g" "🇨🇿" "10" "🦅" "#hash"))))

(ert-deftest excal-text-test-tokenize-nfc ()
  "Decomposed characters are composed before tokenizing."
  (let ((text (ucs-normalize-NFD-string "čでäぴέ다й한")))
    (should (> (length text) 8))
    (should (equal (excal--tokenize text) '("č" "で" "ä" "ぴ" "έ" "다" "й" "한")))))

(ert-deftest excal-text-test-tokenize-cjk ()
  "Upstream's artificial CJK sample."
  (let ((tokens (excal--tokenize
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

(ert-deftest excal-text-test-wrap-basics ()
  (excal-text-test--mock
    (should (equal (excal-text-test--wrap "Hello Excalidraw" 100) "Hello\nExcalidraw"))
    (dolist (width (list 0.0e+NaN -1 1.0e+INF))
      (should (equal (excal-text-test--wrap "Hello Excalidraw" width) "Hello Excalidraw")))
    (should (equal (excal-text-test--wrap "Hello😀" 10) "H\ne\nl\nl\no\n😀"))
    (should (equal (excal-text-test--wrap "don't wrap this number 99,100.99" 300)
                   "don't wrap this number\n99,100.99"))))

(ert-deftest excal-text-test-wrap-whitespace ()
  (excal-text-test--mock
    (should (equal (excal-text-test--wrap "Hello     " 50) "Hello"))
    (should (equal (excal-text-test--wrap "Hello     " 60) "Hello "))
    (should (equal (excal-text-test--wrap "  Hello  World" 90) "  Hello\nWorld"))
    (should (equal (excal-text-test--wrap "   Hello  World            " 90)
                   "   Hello\nWorld    "))
    (should (equal (excal-text-test--wrap "Hello   Wo rl  d                     " 100)
                   "Hello   Wo\nrl  d     "))
    (let ((text "  \t   Hello world"))
      (should (equal (excal-text-test--wrap text 120) "  \t   Hello\nworld"))
      (should (equal (excal-text-test--wrap text 60) "\nHello\nworld"))
      (should (equal (excal-text-test--wrap text 30) "\nHel\nlo\nwor\nld")))
    (should (equal (excal-text-test--wrap "Hello whats up     " 190) "Hello whats up     "))
    (let ((text "Hippopotomonstrosesquippedaliophobia        ??????"))
      (should (equal (excal-text-test--wrap text 400)
                     "Hippopotomonstrosesquippedaliophobia\n??????"))
      (should (equal (excal-text-test--wrap text 300)
                     "Hippopotomonstrosesquippedalio\nphobia        ??????"))
      (should (equal (excal-text-test--wrap text 180)
                     "Hippopotomonstrose\nsquippedaliophobia\n??????")))))

(ert-deftest excal-text-test-wrap-emoji-and-hyphens ()
  (excal-text-test--mock
    (should (equal (excal-text-test--wrap "😀🗺🔥👩🏽‍🦰👨‍👩‍👧‍👦🇨🇿" 1)
                   "😀\n🗺\n🔥\n👩🏽‍🦰\n👨‍👩‍👧‍👦\n🇨🇿"))
    (should (equal (excal-text-test--wrap
                    "Wikipedia is hosted by Wikimedia- Foundation, a non-profit organization that also hosts a range-of other projects"
                    110)
                   "Wikipedia\nis hosted\nby\nWikimedia-\nFoundation,\na non-\nprofit\norganizatio\nn that also\nhosts a\nrange-of\nother\nprojects"))
    (should (equal (excal-text-test--wrap "Hello thereusing-now" 100)
                   "Hello\nthereusing\n-now"))
    (let ((text "\tA) one tab\t\t- two tabs        - 8 spaces"))
      (should (equal (excal-text-test--wrap text 100)
                     "\tA) one\ntab\t\t- two\ntabs\n- 8 spaces"))
      (should (equal (excal-text-test--wrap text 50)
                     "\tA)\none\ntab\n- two\ntabs\n- 8\nspace\ns")))))

(ert-deftest excal-text-test-wrap-cjk ()
  (excal-text-test--mock
    (let ((text "안녕하세요こんにちは世界ｺﾝﾆﾁハ你好"))
      (should (equal (excal-text-test--wrap text 10)
                     "안\n녕\n하\n세\n요\nこ\nん\nに\nち\nは\n世\n界\nｺ\nﾝ\nﾆ\nﾁ\nハ\n你\n好"))
      (should (equal (excal-text-test--wrap text 30)
                     "안녕하\n세요こ\nんにち\nは世界\nｺﾝﾆ\nﾁハ你\n好")))
    (let ((text "a醫 醫      bb  你好  world-i-😀🗺🔥"))
      (should (equal (excal-text-test--wrap text 150)
                     "a醫 醫      bb  你\n好  world-i-😀🗺\n🔥"))
      (should (equal (excal-text-test--wrap text 50)
                     "a醫 醫\nbb  你\n好\nworld\n-i-😀\n🗺🔥"))
      (should (equal (excal-text-test--wrap text 30)
                     "a醫\n醫\nbb\n你好\nwor\nld-\ni-\n😀\n🗺\n🔥")))
    (should (equal (excal-text-test--wrap "HelloたWorld" 50) "Hello\nた\nWorld"))
    (should (equal (excal-text-test--wrap "HelloたWorld" 60) "Helloた\nWorld"))
    (should (equal (excal-text-test--wrap "こんにちは〃世界" 50) "こんにちは\n〃世界"))
    (should (equal (excal-text-test--wrap "こんにちは〃世界" 60) "こんにちは〃\n世界"))
    (should (equal (excal-text-test--wrap "Hello た。" 70) "Hello\nた。"))
    (should (equal (excal-text-test--wrap "Hello「たWorld」" 60) "Hello\n「た\nWorld」"))
    (should (equal (excal-text-test--wrap "「Helloた」World" 70) "「Hello\nた」World"))))

(ert-deftest excal-text-test-wrap-cjk-sentences ()
  (excal-text-test--mock
    (let ((text "中国你好！这是一个测试。\n我们来看看：人民币¥1234「很贵」\n（括号）、逗号，句号。空格 换行　全角符号…—"))
      (should (equal (excal-text-test--wrap text 80)
                     "中国你好！这是一\n个测试。\n我们来看看：人民\n币¥1234「很\n贵」\n（括号）、逗号，\n句号。空格 换行\n全角符号…—"))
      (should (equal (excal-text-test--wrap text 50)
                     "中国你好！\n这是一个测\n试。\n我们来看\n看：人民币\n¥1234\n「很贵」\n（括号）、\n逗号，句\n号。空格\n换行　全角\n符号…—")))
    (let ((text "한국 안녕하세요! 이것은 테스트입니다.\n우리 보자: 원화₩1234「비싸다」\n(괄호), 쉼표, 마침표.\n공백 줄바꿈　전각기호…—"))
      (should (equal (excal-text-test--wrap text 80)
                     "한국 안녕하세\n요! 이것은 테\n스트입니다.\n우리 보자: 원\n화₩1234「비\n싸다」\n(괄호), 쉼\n표, 마침표.\n공백 줄바꿈　전\n각기호…—"))
      (should (equal (excal-text-test--wrap text 60)
                     "한국 안녕하\n세요! 이것\n은 테스트입\n니다.\n우리 보자:\n원화\n₩1234\n「비싸다」\n(괄호),\n쉼표, 마침\n표.\n공백 줄바꿈\n전각기호…—")))))

(ert-deftest excal-text-test-wrap-lines-and-long-text ()
  (excal-text-test--mock
    (pcase-dolist (`(,width ,result)
                   '((70 "Hello\nwhats\nup") (15 "H\ne\nl\nl\no\nw\nh\na\nt\ns\nu\np")
                     (130 "Hello whats\nup") (240 "Hello whats up")
                     (50 "Hello\nwhats\nup")))
      (should (equal (excal-text-test--wrap "Hello whats up" width) result)))
    (pcase-dolist (`(,width ,result)
                   '((70 "Hello\n  whats\nup")
                     (15 "H\ne\nl\nl\no\n\nw\nh\na\nt\ns\nu\np")
                     (140 "Hello\n  whats up")))
      (should (equal (excal-text-test--wrap "Hello\n  whats up" width) result)))
    (let ((text "hellolongtextthisiswhatsupwithyouIamtypingggggandtypinggg break it now"))
      (should (equal (excal-text-test--wrap text 160)
                     "hellolongtextthi\nsiswhatsupwithyo\nuIamtypingggggan\ndtypinggg break\nit now"))
      (should (equal (excal-text-test--wrap text 120)
                     "hellolongtex\ntthisiswhats\nupwithyouIam\ntypingggggan\ndtypinggg\nbreak it now"))
      (should (equal (excal-text-test--wrap text 590)
                     "hellolongtextthisiswhatsupwithyouIamtypingggggandtypinggg\nbreak it now")))
    (should (equal (excal-text-test--wrap "A\n\nB" 100) "A\n\nB"))))

;;;; Text elements

(ert-deftest excal-text-test-new-text-element ()
  "newTextElement: defaults, per-family line height and anchoring."
  (excal-text-test--mock
    (let ((e (excal--make-text-element 10 20 "Hi\tthere")))
      (should (equal (excal--get e 'text) "Hi        there"))
      (should (equal (excal--get e 'originalText) "Hi        there"))
      (should (eq (excal--get e 'autoResize) t))
      (should (= (excal--get e 'lineHeight) 1.25))
      (should (= (excal--get e 'width) 150.0))
      (should (= (excal--get e 'height) 25.0))
      (should (= (excal--get e 'x) 10.0))
      (should (eq (alist-get 'baseFontSize e) :null))
      (should (eq (alist-get 'labelPosition e) :null)))
    (let ((e (excal--make-text-element 100 100 "abcd" (cons 'fontFamily 7)
                                       (cons 'textAlign "center")
                                       (cons 'verticalAlign "middle"))))
      (should (= (excal--get e 'lineHeight) 1.15))
      ;; The anchor point is the text center.
      (should (= (excal--get e 'x) 80.0))
      (should (= (excal--get e 'y) (- 100 (/ (* 20 1.15) 2)))))))

(ert-deftest excal-text-test-set-text-keeps-anchor ()
  "Editing free text keeps its alignment anchor (getAdjustedDimensions)."
  (excal-text-test--mock
    (let ((left (excal--make-text-element 0 0 "ab"))
          (center (excal--make-text-element 0 0 "ab" (cons 'textAlign "center")))
          (right (excal--make-text-element 0 0 "ab" (cons 'textAlign "right")))
          (middle (excal--make-text-element 0 0 "ab" (cons 'textAlign "center")
                                            (cons 'verticalAlign "middle"))))
      (let ((cx (+ (excal--get center 'x) 10)) (rx (+ (excal--get right 'x) 20))
            (my (+ (excal--get middle 'y) 12.5)))
        (dolist (e (list left center right middle)) (excal--set-text e "abcd\nef"))
        (should (= (excal--get left 'x) 0.0))
        (should (= (+ (excal--get center 'x) 20) cx))
        (should (= (+ (excal--get right 'x) 40) rx))
        (should (= (excal--get center 'y) 0.0))
        (should (= (+ (excal--get middle 'y) 25) my))))))

(ert-deftest excal-text-test-fixed-width-text ()
  "autoResize false wraps to the width; side resize and reset."
  (excal-text-test--mock
    (let ((e (excal--make-text-element 0 0 "Hello whats up")))
      (excal--text-set-width e 70)
      (should (eq (alist-get 'autoResize e) :false))
      (should (equal (excal--get e 'text) "Hello\nwhats\nup"))
      (should (= (excal--get e 'width) 70.0))
      (should (= (excal--get e 'height) 75.0))
      (should (equal (excal--get e 'originalText) "Hello whats up"))
      ;; Editing re-wraps at the fixed width.
      (excal--set-text e "Hello whats up doc")
      (should (equal (excal--get e 'text) "Hello\nwhats\nup doc"))
      (should (= (excal--get e 'width) 70.0))
      ;; The width never drops below a space plus padding.
      (excal--text-set-width e 1)
      (should (= (excal--get e 'width) 20.0))
      (excal--text-reset-auto-resize e)
      (should (eq (excal--get e 'autoResize) t))
      (should (equal (excal--get e 'text) "Hello whats up doc"))
      (should (= (excal--get e 'width) 180.0))
      (should (= (excal--get e 'height) 25.0)))))

(ert-deftest excal-text-test-scale ()
  "Corner resize scales the font without rounding."
  (excal-text-test--mock
    (let ((e (excal--make-text-element 0 0 "Hello")))
      (excal--text-scale e 1.5)
      (should (= (excal--get e 'fontSize) 30.0))
      (should (= (excal--get e 'width) 75.0))
      (should (= (excal--get e 'height) 37.5))
      (should-not (excal--text-scale e 0.01))
      (should (= (excal--get e 'fontSize) 30.0)))))

(ert-deftest excal-text-test-font-changes ()
  "Changing the family resets the line height; size keeps the anchor."
  (excal-text-test--mock
    (let ((e (excal--make-text-element 0 0 "Hello" (cons 'textAlign "center"))))
      (excal--put e 'fontFamily 7)
      (excal--text-font-changed e 'fontFamily)
      (should (= (excal--get e 'lineHeight) 1.15))
      (should (= (excal--get e 'height) 23.0))
      (let ((cx (+ (excal--get e 'x) (/ (excal--get e 'width) 2.0))))
        (excal--put e 'fontSize 40)
        (excal--text-font-changed e 'fontSize)
        (should (= (+ (excal--get e 'x) (/ (excal--get e 'width) 2.0)) cx))))))

;;;; Bound text

(defun excal-text-test--container (type w h &rest props)
  "Add a TYPE container of W x H at the origin to the scene."
  (let ((c (apply #'excal--make-element type 0 0 (cons 'width (float w))
                  (cons 'height (float h)) props)))
    (setq excal--elements (append excal--elements (list c)))
    c))

(defun excal-text-test--label (container text)
  "Give CONTAINER the label TEXT and return it."
  (let ((label (excal--add-bound-text container)))
    (excal--set-text label text)
    label))

(defun excal-text-test--xy (element)
  "Return (X Y) of ELEMENT."
  (list (excal--get element 'x) (excal--get element 'y)))

(defun excal-text-test--near (a b)
  "Return non-nil if the number lists A and B agree to 1e-6."
  (cl-every (lambda (x y) (< (abs (- x y)) 1e-6)) a b))

(ert-deftest excal-text-test-bound-text-placement ()
  "Centered labels in every container type (computeBoundTextPosition)."
  (excal-text-test--mock
    (let* ((rect (excal-text-test--container "rectangle" 200 100))
           (label (excal-text-test--label rect "Hello")))
      (should (equal (excal-text-test--xy label) '(75.0 37.5)))
      (should (equal (excal--get label 'containerId) (excal--get rect 'id)))
      (should (eq (excal--bound-text-of rect) label))
      (should (eq (excal--container-of label) rect))
      ;; The label directly follows its container.
      (should (eq (cadr (memq rect excal--elements)) label))
      (should (= (excal--bound-text-max-width rect label) 190)))
    (let* ((ellipse (excal-text-test--container "ellipse" 200 100))
           (label (excal-text-test--label ellipse "Hello"))
           (ox (+ 5 (* 100 (- 1 (/ (sqrt 2) 2)))))
           (oy (+ 5 (* 50 (- 1 (/ (sqrt 2) 2))))))
      (should (= (excal--bound-text-max-width ellipse label) 131))
      (should (= (excal--bound-text-max-height ellipse label) 61))
      (should (excal-text-test--near (excal-text-test--xy label)
                                     (list (+ ox 65.5 -25) (+ oy 30.5 -12.5)))))
    (let* ((diamond (excal-text-test--container "diamond" 200 100))
           (label (excal-text-test--label diamond "Hello")))
      (should (= (excal--bound-text-max-width diamond label) 90))
      (should (equal (excal-text-test--xy label) '(75.0 37.5))))
    (let* ((note (excal-text-test--container "stickynote" 250 250))
           (label (excal-text-test--label note "Hi")))
      (should (= (excal--bound-text-max-width note label) 218))
      (should (= (excal--bound-text-max-height note label) 198))
      (should (= (excal--get label 'x) (+ 16 (- 109 (/ (excal--get label 'width) 2.0)))))
      (should (= (excal--get label 'fontSize) 20)))))

(ert-deftest excal-text-test-bound-text-alignment ()
  "Top/bottom and left/right alignment inside a rectangle."
  (excal-text-test--mock
    (let* ((rect (excal-text-test--container "rectangle" 200 100))
           (label (excal--add-bound-text rect)))
      (excal--put label 'verticalAlign "top")
      (excal--put label 'textAlign "left")
      (excal--set-text label "Hello")
      (should (equal (excal-text-test--xy label) '(5.0 5.0)))
      (excal--put label 'verticalAlign "bottom")
      (excal--put label 'textAlign "right")
      (excal--redraw-text label)
      (should (equal (excal-text-test--xy label) '(145.0 70.0))))))

(ert-deftest excal-text-test-bound-text-rotated ()
  "A rotated container rotates its label about the text box center."
  (excal-text-test--mock
    (let* ((rect (excal-text-test--container "rectangle" 200 100 (cons 'angle (/ float-pi 2))))
           (label (excal-text-test--label rect "Hello")))
      ;; Centered labels stay centered.
      (should (excal-text-test--near (excal-text-test--xy label) '(75.0 37.5)))
      (should (= (excal--get label 'angle) (/ float-pi 2))))))

(ert-deftest excal-text-test-container-grows ()
  "Containers grow to fit wrapped text (computeContainerDimensionForBoundText)."
  (excal-text-test--mock
    (pcase-dolist (`(,type ,w ,h ,expected-h)
                   '(("rectangle" 100 40 60.0)
                     ("diamond" 200 40 120.0)
                     ("ellipse" 142 40 85.0)))
      (let* ((c (excal-text-test--container type w h))
             (label (excal-text-test--label c "a b c d e f g h")))
        (should (equal (excal--get label 'text) "a b c d e\nf g h"))
        (should (= (excal--get c 'height) expected-h))
        (should (= (excal--get c 'width) (float w)))))
    ;; A character wider than the box widens it.
    (let* ((c (excal-text-test--container "rectangle" 12 100))
           (label (excal-text-test--label c "ab")))
      (should (equal (excal--get label 'text) "a\nb"))
      (should (= (excal--get c 'width) 20.0)))
    ;; Containers never shrink on redraw.
    (let* ((c (excal-text-test--container "rectangle" 100 40))
           (label (excal-text-test--label c "a b c d e f g h")))
      (excal--set-text label "a")
      (should (= (excal--get c 'height) 60.0)))))

(ert-deftest excal-text-test-edit-preview-shrinks-back ()
  "While editing, a container grows and shrinks back to its original height."
  (excal-text-test--mock
    (let* ((c (excal-text-test--container "rectangle" 100 40))
           (label (excal-text-test--label c "a"))
           (geometry (excal--container-geometry c)))
      (excal--preview-text label c geometry "a b c d e f g h")
      (should (= (excal--get c 'height) 60.0))
      (excal--preview-text label c geometry "a b")
      (should (= (excal--get c 'height) 40.0)))))

(ert-deftest excal-text-test-layout-after-resize ()
  "handleBindTextResize: re-wrap on width change, grow from the right edge."
  (excal-text-test--mock
    (let* ((c (excal-text-test--container "rectangle" 200 40))
           (label (excal-text-test--label c "a b c d e f g h")))
      (should (equal (excal--get label 'text) "a b c d e f g h"))
      ;; Narrowed by the east handle: text re-wraps, container grows down.
      (excal--put c 'width 100.0)
      (excal--layout-bound-text c 'e)
      (should (equal (excal--get label 'text) "a b c d e\nf g h"))
      (should (= (excal--get c 'height) 60.0))
      (should (= (excal--get c 'y) 0.0))
      ;; Dragging the north handle grows upwards, keeping the bottom.
      (excal--put c 'height 40.0)
      (excal--layout-bound-text c 'n)
      (should (= (excal--get c 'height) 60.0))
      (should (= (excal--get c 'y) -20.0))
      (should (equal (excal-text-test--xy label) '(5.0 -15.0)))
      t)))

(ert-deftest excal-text-test-arrow-labels ()
  "Arrow labels: default center, labelPosition, max width and hole."
  (excal-text-test--mock
    (let* ((arrow (excal--make-element "arrow" 0 0 (cons 'points [[0.0 0.0] [200.0 0.0]])
                                       (cons 'width 200.0) (cons 'height 0.0)))
           (_ (setq excal--elements (list arrow)))
           (label (excal-text-test--label arrow "Hello")))
      (should (equal (excal-text-test--xy label) '(75.0 -12.5)))
      (should (= (excal--bound-text-max-width arrow label) 220))
      (should (= (excal--get label 'angle) 0))
      (should (equal (excal--arrow-label-hole arrow) [70.0 -17.5 60.0 35.0]))
      (should (equal (excal--native-text-extras arrow)
                     (vector "label-hole" [70.0 -17.5 60.0 35.0])))
      ;; Arrows never grow in height for their label.
      (should (= (excal--get arrow 'height) 0.0)))
    (let* ((arrow (excal--make-element
                   "arrow" 0 0 (cons 'points [[0.0 0.0] [100.0 0.0] [100.0 100.0]])))
           (_ (setq excal--elements (list arrow)))
           (label (excal-text-test--label arrow "ab")))
      ;; Odd point count: the middle point.
      (should (equal (excal-text-test--xy label) '(90.0 -12.5)))
      (excal--put label 'labelPosition 0.75)
      (excal--refresh-bound-text arrow)
      (should (excal-text-test--near (excal-text-test--xy label) '(90.0 37.5)))
      (should (< (abs (- (excal--label-position-at arrow '(110.0 . 50.0)) 0.75)) 1e-9)))
    (let* ((arrow (excal--make-element
                   "arrow" 0 0
                   (cons 'points [[0.0 0.0] [100.0 0.0] [100.0 100.0] [200.0 100.0]])))
           (_ (setq excal--elements (list arrow)))
           (label (excal-text-test--label arrow "ab")))
      ;; Even point count: the middle of the middle segment.
      (should (equal (excal-text-test--xy label) '(90.0 37.5))))))

(ert-deftest excal-text-test-remove-bound-text ()
  "Removing a label clears it from the scene and the container."
  (excal-text-test--mock
    (let* ((rect (excal-text-test--container "rectangle" 200 100))
           (label (excal-text-test--label rect "Hello")))
      (excal--remove-bound-text rect label)
      (should-not (memq label excal--elements))
      (should-not (excal--bound-text-of rect))
      (should (equal (excal--get rect 'boundElements) [])))))

(ert-deftest excal-text-test-style-reaches-label ()
  "Setting a font property on a selected shape changes its label."
  (excal-text-test--mock
    (setq excal--backend nil)
    (let* ((rect (excal-text-test--container "rectangle" 200 100))
           (label (excal-text-test--label rect "Hello")))
      (excal--select (list rect))
      (excal-set-style 'fontSize 40)
      (should (= (excal--get label 'fontSize) 40))
      (should (= (excal--get label 'height) 50.0))
      (should-not (assq 'fontSize rect))
      (excal-set-style 'verticalAlign "top")
      (should (= (excal--get label 'y) 5.0)))))

(ert-deftest excal-text-test-sticky-note-fits-font ()
  "Sticky-note text takes the largest grid font size that fits."
  (excal-text-test--mock
    (let* ((note (excal-text-test--container "stickynote" 250 250))
           (label (excal--add-bound-text note (cons 'fontSize 36))))
      (excal--set-text label (string-join (make-list 30 "word") " "))
      (let ((size (excal--get label 'fontSize)))
        (should (= 0 (mod (- 36 size) 2)))
        (should (<= (excal--get label 'height) 198))
        (should (<= (excal--get label 'width) 218))
        (should (= (excal--get label 'baseFontSize) 36))
        ;; One step larger would not fit.
        (let* ((bigger (+ size 2))
               (lines (excal--wrap-text (excal--get label 'originalText) bigger 5 218))
               (m (excal--measure-string lines bigger 5 1.25)))
          (should (or (> (car m) 218) (> (cdr m) 198))))))))

;;;; Commands

(defmacro excal-text-test--typing (result &rest body)
  "Run BODY, which starts a text edit, then type RESULT over the text.
RESULT nil cancels the edit instead."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'excal--render) #'ignore))
     ,@body
     (let ((result ,result))
       (if (null result)
           (excal-text-edit-cancel)
         (delete-region (plist-get excal--text-edit :beg) (plist-get excal--text-edit :end))
         (insert result)
         (excal-text-edit-submit)))))

(ert-deftest excal-text-test-edit-container-text ()
  "Enter on a shape adds a centered label; cancelling leaves no trace."
  (excal-text-test--mock
    (excal--load-current-style nil)
    (let ((rect (excal-text-test--container "rectangle" 100 40)))
      (excal-text-test--typing nil
        (excal--select (list rect))
        (excal-edit-text))
      (should (= (length excal--elements) 1))
      (should (eq (alist-get 'boundElements rect) :null))
      (should (= (excal--get rect 'height) 40.0))
      (excal-text-test--typing "a b c d e f g h"
        (excal--select (list rect))
        (excal-edit-text))
      (let ((label (excal--bound-text-of rect)))
        (should label)
        (should (equal (excal--get label 'textAlign) "center"))
        (should (equal (excal--get label 'verticalAlign) "middle"))
        (should (equal (excal--get label 'originalText) "a b c d e f g h"))
        (should (= (excal--get rect 'height) 60.0))
        ;; Aborting an edit of an existing label keeps it.
        (excal-text-test--typing nil
          (excal--select (list rect))
          (excal-edit-text))
        (should (eq (excal--bound-text-of rect) label))
        ;; Clearing the text deletes the label.
        (excal-text-test--typing "  "
          (excal--select (list rect))
          (excal-edit-text))
        (should-not (excal--bound-text-of rect))
        (should-not (memq label excal--elements))))))

;;;; Round trip and rendering

(ert-deftest excal-text-test-roundtrip-fields ()
  "Text fields survive saving and loading."
  (let ((out (make-temp-file "excal" nil ".excalidraw")))
    (unwind-protect
        (excal-text-test--mock
          (let* ((rect (excal-text-test--container "rectangle" 200 100))
                 (label (excal-text-test--label rect "Hello world")))
            (excal--put label 'labelPosition 0.3)
            (setq excal--doc (excal--empty-doc) excal--file out)
            (excal-save)
            (let* ((doc (excal--read-file out))
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
                                           (cons 'id (excal--get label 'id)))))))))
      (delete-file out))))

(ert-deftest excal-text-test-restore-legacy ()
  "Legacy text gets defaults and keeps its effective line height.
Restore is `excal--restore-text' in excal-restore.el."
  (let ((e (excal--restore-text
            (list (cons 'type "text") (cons 'text "a\nb") (cons 'fontSize 20)
                  (cons 'fontFamily 1) (cons 'height 60.0))
            nil)))
    (should (= (excal--get e 'lineHeight) 1.5))
    (should (equal (excal--get e 'originalText) "a\nb"))
    (should (equal (excal--get e 'textAlign) "left"))
    (should (equal (excal--get e 'verticalAlign) "top"))
    (should (eq (excal--get e 'autoResize) t)))
  (let ((e (excal--restore-text
            (list (cons 'type "text") (cons 'text "a") (cons 'fontFamily 7)
                  (cons 'labelPosition 3))
            nil)))
    (should (= (excal--get e 'fontSize) 20))
    (should (= (excal--get e 'lineHeight) 1.15))
    (should (= (excal--get e 'labelPosition) 1.0))))

(ert-deftest excal-text-test-native-extras ()
  "Text elements pass their baseline offset to the module."
  (excal-text-test--mock
    (let ((e (excal--make-text-element 0 0 "x")))
      (should (equal (excal--native-text-extras e)
                     (vector "vertical-offset" (excal--vertical-offset 5 20 25.0)))))
    (should (equal (excal--native-text-extras
                    (excal--make-element "rectangle" 0 0))
                   []))))

(ert-deftest excal-text-test-render-lines ()
  "Each line is drawn: more lines change more pixels; alignment moves ink."
  (with-temp-buffer
    (setq excal--native-cache (make-hash-table :test #'eq) excal--zoom 1.0)
    (let* ((render (lambda (elements)
                     (let ((fb (excal-native-fb-create 300 120)))
                       (setq excal--elements elements)
                       (clrhash excal--native-cache)
                       (excal-native-fb-render fb 1.0 1.0 0.0 0.0
                                               (excal--visible-elements) nil)
                       fb)))
           (blank (funcall render nil))
           (one (funcall render (list (excal--make-text-element 10 10 "Hello"))))
           (two (funcall render (list (excal--make-text-element 10 10 "Hello\nHello"))))
           (right (let ((e (excal--make-text-element 10 10 "Hello")))
                    (excal--put e 'textAlign "right")
                    (excal--put e 'width 250.0)
                    (funcall render (list e)))))
      (should (> (excal-native-fb-mean-diff blank one) 0))
      (should (> (excal-native-fb-mean-diff blank two)
                 (* 1.8 (excal-native-fb-mean-diff blank one))))
      (should (> (excal-native-fb-mean-diff one right) 0)))))

;;; excal-text-test.el ends here
