;; -*- tab-width: 2 -*-

(ql:quickload '(:Dexador :cl-ppcre :cl-cffi-gtk))

(defpackage :mochosa
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :mochosa)

(load "compose-message-dialog.lisp")

(defmacro do-later (&body body)
  `(g-timeout-add
    1
    (lambda ()
      ,@body
      nil)))

(defparameter *auto-reload-time* 9000) ;;自動読み込みの時間
(defparameter *popup-time* 8000) ;;新着メッセージのポップアップ時間
(defparameter *sound*
  "/usr/share/sounds/Yaru/stereo/message-new-instant.ogg")
(defparameter *say-command* "vtsay")

(defparameter *auto-move* t)
(defparameter *auto-create* nil)
(defparameter *ac-name* "")
(defparameter *ac-email* "sage")
(defparameter *ac-verify* t)

(defconstant +option-variables+ '(*auto-reload-time* *popup-time* *sound* *say-command*
                                  *auto-move* *auto-create* *ac-name* *ac-email* *ac-verify*))

(defconstant +application-name+ "もげぞうβは超サイコー")

(defstruct mocho
  read-url
  (res-lst nil) ;;レスstructリスト
  (new-res-num 0)
  (elapsed 0) ;;カウントダウン表示用
  (interval 0)) ;;カウントダウン設定

(defun read-url-rawmode-url (read-url)
  (cl-ppcre:regex-replace "read"
                          read-url
                          "rawmode"))

(defmethod mocho-rawmode-url ((m mocho))
  (and (mocho-read-url m)
   (read-url-rawmode-url (mocho-read-url m))))

(declaim (ftype (function (string) string) fix-semicolonless-amp)
         (ftype (function (string) string) escape-stray-amp)
         (ftype (function (string) (values string list)) parse-untag-anchors)
         (ftype (function (string) string) linkify-urls)
         (ftype function untag-all))

(defstruct res
  number
  name
  mail
  date
  honbun
  title)

(defun make-res-from-string (string)
  (let ((fields (ppcre:split "<>" string)))
    (make-res :number (nth 0 fields)
              :name (nth 1 fields)
              :mail (nth 2 fields)
              :date (nth 3 fields)
              :honbun (nth 4 fields)
              :title (nth 5 fields)
              )))

(defmethod res-name-html ((r res))
  ;; 名前欄では & が &amp (←セミコロンなし)とエスケープされるので、こ
  ;; れを正しい &amp; に直す。
  (fix-semicolonless-amp (res-name r)))

(defmethod res-honbun-html ((r res))
  (multiple-value-bind
        (honbun path-list)
      (parse-untag-anchors (res-honbun r))
    (values (escape-stray-amp (linkify-urls honbun))
            path-list)))

;;文字実体参照の先頭でない、孤立したアンパサンドを &amp; に置換
(defun escape-stray-amp (res)
  (ppcre:regex-replace-all "&(?![#A-Za-z0-9]+;)" res "&amp;"))

(defun fix-semicolonless-amp (str)
  (ppcre:regex-replace-all "&amp" str "&amp;"))

;; メールのあるなしに応じて色を付けた名前のマークアップを返す。
(defmethod colorized-name-html ((r res))
  (if (string-equal "" (res-mail r))
      (format nil "<span color=\"#008800\"><b>~A</b></span>"
              (res-name-html r))
      (format nil "<span color=\"#0000FF\" underline=\"single\"><b>~A</b></span>"
              (res-name-html r))))

;; レス全体をHTMLとして生成する。
(defmethod compose-html ((r res))
  (multiple-value-bind
        (honbun path-list)
      (res-honbun-html r)
    (values
     (format nil "<span color=\"#0000FF\" underline=\"single\">~A</span>：~A：~A<br>~A<br>"
             (res-number r)
             (colorized-name-html r)
             (res-date r)
             honbun)
     path-list)))

;;URLがhで始まっていなかったらhを付ける
(defun supplement-h (url)
  (if (and (> (length url) 0) (eq #\h (aref url 0)))
      url
      (concatenate 'string "h" url)))


;;h?ttps? なURLをリンクにする
(defun linkify-urls (honbun)
  (ppcre:regex-replace-all
   "h?ttps?://[-_.!~*\'()a-zA-Z0-9;/?:@&=+$,%#]+"
   honbun
   (lambda (target-string start end match-start match-end reg-starts reg-ends)
     (declare (ignore start end reg-starts reg-ends))

     (let ((url (subseq target-string match-start match-end)))
       (format nil "<a href=\"~A\">~A</a>" (supplement-h url) url)))))

(defun parse-untag-anchors (honbun)
  "アンカーのタグを外した本文と、アンカーに指定されていたread.cgiへの
パスのリストを返す"
  (let ((path-list nil))
    (let ((honbun1 (ppcre:regex-replace-all
                    "<a href=\"(.*?)\" target=\"_blank\">(.*?)</a>"
                    honbun
                    (lambda (target-string start end match-start match-end reg-starts reg-ends)
                      (declare (ignore start end match-start match-end))

                      (let ((path (subseq target-string (aref reg-starts 0) (aref reg-ends 0)))
                            (text (subseq target-string (aref reg-starts 1) (aref reg-ends 1))))
                        (push path path-list)
                        (format nil "<span color=\"#0000FF\" underline=\"single\">~A</span>" text))))))
      (values honbun1  (reverse path-list)))))

;; HTMLの文字名をUnicodeコードポイントで返す。
(defun html-character-name->code-point (name)
  (let ((table '((|exclamation|  . #x0021)
                 (|quot|         . #x0022)
                 (|percent|      . #x0025)
                 (|amp|          . #x0026)
                 (|apos|         . #x0027)
                 (|add|          . #x002B)
                 (|lt|           . #x003C)
                 (|equal|        . #x003D)
                 (|gt|           . #x003E)
                 (|nbsp|         . #x00A0)
                 (|iexcl|        . #x00A1)
                 (|cent|         . #x00A2)
                 (|pound|        . #x00A3)
                 (|curren|       . #x00A4)
                 (|yen|          . #x00A5)
                 (|brvbar|       . #x00A6)
                 (|sect|         . #x00A7)
                 (|uml|          . #x00A8)
                 (|copy|         . #x00A9)
                 (|ordf|         . #x00AA)
                 (|laquo|        . #x00AB)
                 (|not|          . #x00AC)
                 (|shy|          . #x00AD)
                 (|reg|          . #x00AE)
                 (|macr|         . #x00AF)
                 (|deg|          . #x00B0)
                 (|plusmn|       . #x00B1)
                 (|sup2|         . #x00B2)
                 (|sup3|         . #x00B3)
                 (|acute|        . #x00B4)
                 (|micro|        . #x00B5)
                 (|para|         . #x00B6)
                 (|middot|       . #x00B7)
                 (|cedil|        . #x00B8)
                 (|sup1|         . #x00B9)
                 (|ordm|         . #x00BA)
                 (|raquo|        . #x00BB)
                 (|frac14|       . #x00BC)
                 (|frac12|       . #x00BD)
                 (|frac34|       . #x00BE)
                 (|iquest|       . #x00BF)
                 (|Agrave|       . #x00C0)
                 (|Aacute|       . #x00C1)
                 (|Acirc|        . #x00C2)
                 (|Atilde|       . #x00C3)
                 (|Auml|         . #x00C4)
                 (|Aring|        . #x00C5)
                 (|AElig|        . #x00C6)
                 (|Ccedil|       . #x00C7)
                 (|Egrave|       . #x00C8)
                 (|Eacute|       . #x00C9)
                 (|Ecirc|        . #x00CA)
                 (|Euml|         . #x00CB)
                 (|Igrave|       . #x00CC)
                 (|Iacute|       . #x00CD)
                 (|Icirc|        . #x00CE)
                 (|Iuml|         . #x00CF)
                 (|ETH|          . #x00D0)
                 (|Ntilde|       . #x00D1)
                 (|Ograve|       . #x00D2)
                 (|Oacute|       . #x00D3)
                 (|Ocirc|        . #x00D4)
                 (|Otilde|       . #x00D5)
                 (|Ouml|         . #x00D6)
                 (|times|        . #x00D7)
                 (|Oslash|       . #x00D8)
                 (|Ugrave|       . #x00D9)
                 (|Uacute|       . #x00DA)
                 (|Ucirc|        . #x00DB)
                 (|Uuml|         . #x00DC)
                 (|Yacute|       . #x00DD)
                 (|THORN|        . #x00DE)
                 (|szlig|        . #x00DF)
                 (|agrave|       . #x00E0)
                 (|aacute|       . #x00E1)
                 (|acirc|        . #x00E2)
                 (|atilde|       . #x00E3)
                 (|auml|         . #x00E4)
                 (|aring|        . #x00E5)
                 (|aelig|        . #x00E6)
                 (|ccedil|       . #x00E7)
                 (|egrave|       . #x00E8)
                 (|eacute|       . #x00E9)
                 (|ecirc|        . #x00EA)
                 (|euml|         . #x00EB)
                 (|igrave|       . #x00EC)
                 (|iacute|       . #x00ED)
                 (|icirc|        . #x00EE)
                 (|iuml|         . #x00EF)
                 (|eth|          . #x00F0)
                 (|ntilde|       . #x00F1)
                 (|ograve|       . #x00F2)
                 (|oacute|       . #x00F3)
                 (|ocirc|        . #x00F4)
                 (|otilde|       . #x00F5)
                 (|ouml|         . #x00F6)
                 (|divide|       . #x00F7)
                 (|oslash|       . #x00F8)
                 (|ugrave|       . #x00F9)
                 (|uacute|       . #x00FA)
                 (|ucirc|        . #x00FB)
                 (|uuml|         . #x00FC)
                 (|yacute|       . #x00FD)
                 (|thorn|        . #x00FE)
                 (|yuml|         . #x00FF)
                 (|OElig|        . #x0152)
                 (|oelig|        . #x0153)
                 (|Scaron|       . #x0160)
                 (|scaron|       . #x0161)
                 (|Yuml|         . #x0178)
                 (|fnof|         . #x0192)
                 (|circ|         . #x02C6)
                 (|tilde|        . #x02DC)
                 (|Alpha|        . #x0391)
                 (|Beta|         . #x0392)
                 (|Gamma|        . #x0393)
                 (|Delta|        . #x0394)
                 (|Epsilon|      . #x0395)
                 (|Zeta|         . #x0396)
                 (|Eta|          . #x0397)
                 (|Theta|        . #x0398)
                 (|Iota|         . #x0399)
                 (|Kappa|        . #x039A)
                 (|Lambda|       . #x039B)
                 (|Mu|           . #x039C)
                 (|Nu|           . #x039D)
                 (|Xi|           . #x039E)
                 (|Omicron|      . #x039F)
                 (|Pi|           . #x03A0)
                 (|Rho|          . #x03A1)
                 (|Sigma|        . #x03A3)
                 (|Tau|          . #x03A4)
                 (|Upsilon|      . #x03A5)
                 (|Phi|          . #x03A6)
                 (|Chi|          . #x03A7)
                 (|Psi|          . #x03A8)
                 (|Omega|        . #x03A9)
                 (|alpha|        . #x03B1)
                 (|beta|         . #x03B2)
                 (|gamma|        . #x03B3)
                 (|delta|        . #x03B4)
                 (|epsilon|      . #x03B5)
                 (|zeta|         . #x03B6)
                 (|eta|          . #x03B7)
                 (|theta|        . #x03B8)
                 (|iota|         . #x03B9)
                 (|kappa|        . #x03BA)
                 (|lambda|       . #x03BB)
                 (|mu|           . #x03BC)
                 (|nu|           . #x03BD)
                 (|xi|           . #x03BE)
                 (|omicron|      . #x03BF)
                 (|pi|           . #x03C0)
                 (|rho|          . #x03C1)
                 (|sigmaf|       . #x03C2)
                 (|sigma|        . #x03C3)
                 (|tau|          . #x03C4)
                 (|upsilon|      . #x03C5)
                 (|phi|          . #x03C6)
                 (|chi|          . #x03C7)
                 (|psi|          . #x03C8)
                 (|omega|        . #x03C9)
                 (|thetasym|     . #x03D1)
                 (|upsih|        . #x03D2)
                 (|piv|          . #x03D6)
                 (|ensp|         . #x2002)
                 (|emsp|         . #x2003)
                 (|thinsp|       . #x2009)
                 (|zwnj|         . #x200C)
                 (|zwj|          . #x200D)
                 (|lrm|          . #x200E)
                 (|rlm|          . #x200F)
                 (|ndash|        . #x2013)
                 (|mdash|        . #x2014)
                 (|horbar|       . #x2015)
                 (|lsquo|        . #x2018)
                 (|rsquo|        . #x2019)
                 (|sbquo|        . #x201A)
                 (|ldquo|        . #x201C)
                 (|rdquo|        . #x201D)
                 (|bdquo|        . #x201E)
                 (|dagger|       . #x2020)
                 (|Dagger|       . #x2021)
                 (|bull|         . #x2022)
                 (|hellip|       . #x2026)
                 (|permil|       . #x2030)
                 (|prime|        . #x2032)
                 (|Prime|        . #x2033)
                 (|lsaquo|       . #x2039)
                 (|rsaquo|       . #x203A)
                 (|oline|        . #x203E)
                 (|frasl|        . #x2044)
                 (|euro|         . #x20AC)
                 (|image|        . #x2111)
                 (|weierp|       . #x2118)
                 (|real|         . #x211C)
                 (|trade|        . #x2122)
                 (|alefsym|      . #x2135)
                 (|larr|         . #x2190)
                 (|uarr|         . #x2191)
                 (|rarr|         . #x2192)
                 (|darr|         . #x2193)
                 (|harr|         . #x2194)
                 (|crarr|        . #x21B5)
                 (|lArr|         . #x21D0)
                 (|uArr|         . #x21D1)
                 (|rArr|         . #x21D2)
                 (|dArr|         . #x21D3)
                 (|hArr|         . #x21D4)
                 (|forall|       . #x2200)
                 (|part|         . #x2202)
                 (|exist|        . #x2203)
                 (|empty|        . #x2205)
                 (|nabla|        . #x2207)
                 (|isin|         . #x2208)
                 (|notin|        . #x2209)
                 (|ni|           . #x220B)
                 (|prod|         . #x220F)
                 (|sum|          . #x2211)
                 (|minus|        . #x2212)
                 (|lowast|       . #x2217)
                 (|radic|        . #x221A)
                 (|prop|         . #x221D)
                 (|infin|        . #x221E)
                 (|ang|          . #x2220)
                 (|and|          . #x2227)
                 (|or|           . #x2228)
                 (|cap|          . #x2229)
                 (|cup|          . #x222A)
                 (|int|          . #x222B)
                 (|there4|       . #x2234)
                 (|sim|          . #x223C)
                 (|cong|         . #x2245)
                 (|asymp|        . #x2248)
                 (|ne|           . #x2260)
                 (|equiv|        . #x2261)
                 (|le|           . #x2264)
                 (|ge|           . #x2265)
                 (|sub|          . #x2282)
                 (|sup|          . #x2283)
                 (|nsub|         . #x2284)
                 (|sube|         . #x2286)
                 (|supe|         . #x2287)
                 (|oplus|        . #x2295)
                 (|otimes|       . #x2297)
                 (|perp|         . #x22A5)
                 (|sdot|         . #x22C5)
                 (|lceil|        . #x2308)
                 (|rceil|        . #x2309)
                 (|lfloor|       . #x230A)
                 (|rfloor|       . #x230B)
                 (|lang|         . #x2329)
                 (|rang|         . #x232A)
                 (|loz|          . #x25CA)
                 (|spades|       . #x2660)
                 (|clubs|        . #x2663)
                 (|hearts|       . #x2665)
                 (|diams|        . #x2666))))
    (let ((entry (assoc (intern name :mochosa) table)))
      (if entry
          (cdr entry)
          nil))))

;; (XMLベースの)Pangoマークアップが対応していない<br>タグと文字実体参
;; 照を、それぞれ改行文字と数値実体参照に置換する。
(defun html->pango-markup (html)
  (ppcre:regex-replace-all
   "<br>|&([A-Za-z]+);" html
   (lambda (target-string start end match-start match-end reg-starts reg-ends)
     (declare (ignore start end))
     (let (($& (subseq target-string match-start match-end)))
       (if (string-equal $& "<br>")
           (format nil "~A" #\Newline)
           (let* (($1 (subseq target-string (aref reg-starts 0) (aref reg-ends 0)))
                  (code-point (html-character-name->code-point $1)))
             (if code-point
                 (format nil "&#~A;" code-point)
                 (format nil "&amp;~A;" $1))))))))

(defun decode-numeric-character-references (str)
  (ppcre:regex-replace-all
   "&#(x[0-9A-Fa-f]+|\\d+);" str
   (lambda (target-string start end match-start match-end reg-starts reg-ends)
     (declare (ignore start end))
     (let* (($1 (subseq target-string (aref reg-starts 0) (aref reg-ends 0)))
            (code (if (eql #\x (aref $1 0)) ; x 大文字でもいい!？
                      (parse-integer (subseq $1 1) :radix 16)
                      (parse-integer $1 :radix 10))))
       (format nil "~A" (code-char code))))))

(defmethod parse-res ((r res))
  "レス作成　Pangoマークアップされた本文と、アンカーのパスリストを返す。"
  (multiple-value-bind
        (html path-list)
      (compose-html r)
    (values (html->pango-markup html) path-list)))

(defun anchor-path->rawmode-url (path)
  "アンカーのパスを該当レスを取得するrawmode.cgiへのURLに変換する。"
  (concatenate 'string "https://jbbs.shitaraba.net"
               (ppcre:regex-replace "read\.cgi" path "rawmode.cgi")))

(defun anchor-path->range (path)
  (multiple-value-bind
        (match-start match-end reg-starts reg-ends)
      (ppcre:scan "/(\\d+)(-(\\d+))?$" path)
    (declare (ignore match-start match-end))
    (let ((left (subseq path (aref reg-starts 0) (aref reg-ends 0)))
          (right (if (aref reg-starts 1)
                     (subseq path (aref reg-starts 2) (aref reg-ends 2))
                     nil)))
      (if right
          (list (read-from-string left) (read-from-string right))
          (list (read-from-string left) (read-from-string left))))))

;;rawmode.cgiで取得されるDATを一行ごとのリストにする
(defun dat-lines (data-text)
  (ppcre:split "\\n" data-text))

(defmethod print-res (output-stream (r res))
  "レスをPangoマークアップにしてoutput-streamに出力する。"
  (format output-stream "~A" (parse-res r)))

(defmethod make-tooltip-text ((mocho mocho) anchor-path-list)
  "アンカーのリストから、該当レスを取得してPangoマークアップとして整形する。"
  (let ((ranges (mapcar #'anchor-path->range anchor-path-list))
        (output (make-string-output-stream)))
    (loop for range in ranges
       for i from 0
       do
         (loop for j from (first range) upto (second range)
            do
              (let ((r (find-if (lambda (s) (= j (read-from-string (res-number s)))) (mocho-res-lst mocho))))
                (when r
                  (terpri output)
                  (write-string (untag-all (parse-res r)) output)
                  ))))
    (get-output-stream-string output)))

;;DAT行のレスをGtkLabelとしてGtkBoxに追加する。
(defmethod add-res ((r res) mocho vbox1)
  (push r (mocho-res-lst mocho))
  (multiple-value-bind
        (tagged-text anchor-path-list)
      (parse-res r)
    (let* ((new-label (make-instance
                       'gtk-label
                       :xalign 0.0
                       :selectable t
                       :use-markup t
                       :wrap t
                       :wrap-mode :word-char
                       :label tagged-text)))
      (when anchor-path-list
        (setf (gtk-widget-tooltip-markup new-label) (make-tooltip-text mocho anchor-path-list)))
      (gtk-box-pack-start vbox1 new-label :expand nil)
      )))

;;新着レス
(defun get-res-line (url res-num)
  (handler-case (dex:get (concatenate 'string url (write-to-string res-num)))
    (USOCKET:NS-HOST-NOT-FOUND-ERROR () nil)
    (dex:http-request-bad-request () nil)
    (dex:http-request-failed (e) (declare (ignore e)) nil)
    ))

(defmethod notify ((r res))
  "新着メッセージ生成と新着メッセージのポップアップ生成。"
  (let* ((honbun (parse-res r)))
    (sb-ext:run-program "yomiage-send" (list "新着メッセージ" (decode-numeric-character-references honbun)) :search t)
    ))

(defun scroll-bot (vadj)
  "スクロールウィンドウの一番下へ。"
  (do-later
      (gtk-adjustment-set-value vadj (- (gtk-adjustment-upper vadj)
                                        (gtk-adjustment-page-size vadj)))))

(defun make-number-c (elapsed interval)
  "カウントダウン文字列生成。改造したいところ。"
  (let ((dots (make-string elapsed :initial-element #\█))
        (spaces (make-string (- interval elapsed) :initial-element #\░)))
    (format nil "待機中 <span color=\"#080\">~A</span>~A" dots spaces)))

(defun last-res-num (res-list)
  (when (null res-list)
    (error "empty res list"))
  (let ((fields (ppcre:split "<>" (car (last res-list)))))
    (read-from-string (car fields))))

(defun thread-url-p (url)
  (ppcre:scan "^https?://jbbs.shitaraba.net/bbs/read\\.cgi/[a-z]+/\\d+/\\d+/" url))

(defun normalize-thread-url (url)
  (multiple-value-bind
        (match-start match-end reg-starts reg-ends)
      (ppcre:scan "^https?://jbbs.shitaraba.net/bbs/read\\.cgi/[a-z]+/\\d+/\\d+/" url)
    (declare (ignore reg-starts reg-ends))
    (subseq url match-start match-end)))

(defun untag-all (string)
  (ppcre:regex-replace-all "<[^>]*>" string ""))

(defun speak (str)
  (let ((words (remove "" (ppcre:split "\\s+" *say-command*) :test #'string-equal)))
    (when words
      (handler-case
          (sb-ext:run-program (first words) (append (rest words) (list str)) :search t)
        (simple-error (e) (format t "~A~%" e))))))

(defmethod speak-res ((r res))
  (speak (decode-numeric-character-references (untag-all (res-honbun r)))))

(defun flush-draw-queue ()
  (loop while (g-main-context-iteration (cffi:null-pointer) nil)))

(defmethod auto-reload (url count-down-label (m mocho) vadj scrolled vbox1 window)
  "オートリロード新着レス表示。"
  (with-slots
        (elapsed interval new-res-num)
      m

    (when (>= new-res-num 1001)
      (setf (gtk-switch-active (main-window-load-switch window)) nil)
      (return-from auto-reload))

    (incf elapsed)
    (if (>= elapsed interval)
        (progn
          (setf (gtk-label-label count-down-label) "新着レス確認中")
          (flush-draw-queue)
          (loop with line
             while (not (equal "" (setf line (get-res-line url new-res-num))))
             do
               (let ((r (make-res-from-string line)))
                 (add-res r m vbox1)
                 (scroll-bot vadj)
                 (gtk-widget-show-all vbox1)
                 (flush-draw-queue)
                 (notify r)
                 (speak-res r)
                 (incf new-res-num)))
          (setf elapsed 0)
          (setf (gtk-label-label count-down-label) (make-number-c elapsed interval)))
        (progn
          (gtk-label-set-markup count-down-label (make-number-c elapsed interval))))))

(defun save-options ()
  "options.dat 1:オートリロード時間 2:ポップアップタイム 3:サウンド
設定保存。"
  (with-open-file (out "options.dat" :direction :output
                                     :if-exists :supersede)
    (dolist (var +option-variables+)
      (format out "(~S ~S)~%" var (eval var)))))

(defun load-options ()
  "設定読み込み。"
  (with-open-file (in "options.dat" :direction :input
                                    :if-does-not-exist nil)
    (when in
      (loop for pair = (read in nil)
            while pair
            do (eval `(setf ,(first pair) ,(second pair)))))))

(defun save-url (url)
  (with-open-file (out "URL.dat" :direction :output
                       :if-exists :supersede)
    (format out url)))

(defun load-url ()
  (with-open-file (in "URL.dat" :direction :input)
    (read-line in nil)))

(defclass thread-chooser (gtk-dialog)
  (list-store
   tree-view)
  (:metaclass gobject:gobject-class))

(defclass main-window (gtk-window)
  ((id :initform 0 :accessor main-window-id)
   (url-entry :accessor main-window-url-entry)
   (load-switch :accessor main-window-load-switch))
  (:metaclass gobject:gobject-class))

(defclass options-dialog (gtk-dialog)
  (auto-move-check-button
   auto-create-check-button
   ac-name-entry
   ac-email-entry
   ac-verify-check-button)
  (:metaclass gobject:gobject-class))

(defmethod main-window-quit ((w main-window))
  (when (/= (main-window-id w) 0)
    (g-source-remove (main-window-id w)))
  (leave-gtk-main))

  ;; (let* ((dlg (gtk-dialog-new-with-buttons "設定" window '(:modal)))
  ;;        (vbox (gtk-dialog-get-content-area dlg))
  ;;        (top-hbox (make-instance 'gtk-box :orientation :horizontal :spacing 6))
  ;;        (entry (gtk-entry-new))
  ;;        (test-btn (make-instance 'gtk-button :label "テスト")))

  ;;   (gtk-dialog-add-button dlg "gtk-cancel" :cancel)
  ;;   (gtk-dialog-add-button dlg "gtk-ok" :ok)
  ;;   (gtk-box-pack-start top-hbox (gtk-label-new "コマンド:") :expand nil)
  ;;   (gtk-box-pack-start top-hbox entry :expand t :fill t)
  ;;   (setf (gtk-entry-text entry) *say-command*)
  ;;   (setf (gtk-widget-tooltip-text test-btn) "「テスト」と読み上げます。")
  ;;   (gtk-box-pack-start top-hbox test-btn :expand nil)
  ;;   (g-signal-connect test-btn "clicked"
  ;;                     (lambda (w)
  ;;                       (declare (ignore w))
  ;;                       (let ((*say-command* (gtk-entry-text entry))) ; 動的束縛
  ;;                         (speak "テスト"))))
  ;;   (gtk-box-pack-start vbox top-hbox)
  ;;   (gtk-widget-show-all vbox)
  ;;   (case (gtk-dialog-run dlg)
  ;;     (:ok (setf *say-command* (gtk-entry-text entry))))
  ;;   (gtk-widget-destroy dlg)
(defmethod initialize-instance :after ((dlg options-dialog) &key transient-for)
  (let* ((vbox (gtk-dialog-get-content-area dlg)))
    (setf (gtk-window-transient-for dlg) transient-for)
    (setf (gtk-widget-size-request dlg) '(320 240))
    (setf (gtk-window-title dlg) "設定")

    (gtk-dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk-dialog-add-button dlg "gtk-ok" :ok)

    ;; (gtk-box-pack-start vbox (gtk-frame-new "スレ移動"))
    ;; (gtk-box-pack-start vbox (gtk-frame-new "スレ立て"))

    (with-slots (auto-move-check-button
                 auto-create-check-button
                 ac-name-entry
                 ac-email-entry
                 ac-verify-check-button)
        dlg
      (setf (gtk-box-spacing vbox) 6)
      (setf (gtk-widget-margin vbox) 6)
      (gtk-box-pack-start vbox (let ((cb (gtk-check-button-new-with-label "自動スレ移動")))
                                 (setq auto-move-check-button cb)) :expand nil)
      (gtk-box-pack-start vbox (let ((cb (gtk-check-button-new-with-label "自動スレ立て")))
                                 (setq auto-create-check-button cb)) :expand nil)
      (gtk-box-pack-start vbox
                          (let ((hbox (make-instance 'gtk-box :orientation :horizontal)))
                            (gtk-box-pack-start hbox (gtk-label-new "名前"))
                            (gtk-box-pack-start hbox (let ((e (gtk-entry-new)))
                                                       (setq ac-name-entry e)
                                                       ;;(setf (gtk-entry-visibility e) nil)
                                                       e))
                            ;;(gtk-box-pack-start hbox (gtk-check-button-new-with-label "👁"))
                            hbox) :expand nil)
      (gtk-box-pack-start vbox
                          (let ((hbox (make-instance 'gtk-box :orientation :horizontal)))
                            (gtk-box-pack-start hbox (gtk-label-new "E-mail"))
                            (gtk-box-pack-start hbox (let ((e (gtk-entry-new)))
                                                       (setq ac-email-entry e)
                                                       ;;(setf (gtk-entry-visibility e) nil)
                                                       e))
                            ;;(gtk-box-pack-start hbox (gtk-check-button-new-with-label "👁"))
                            hbox) :expand nil)
      (gtk-box-pack-start vbox
                          (let ((cb (gtk-check-button-new-with-label "投稿内容を確認")))
                            (setq ac-verify-check-button cb)
                            ;;(setf (gtk-widget-tooltip-text cb) "自動スレ立て時にダイアログボックスを開きます。")
                            cb
                            ) :expand nil)

      (gtk-widget-show-all vbox)
      )))

(defmethod main-window-open-options ((window main-window))
  (let ((dlg (make-instance 'options-dialog :transient-for window)))
    (with-slots (auto-move-check-button
                 auto-create-check-button
                 ac-name-entry
                 ac-email-entry
                 ac-verify-check-button)
        dlg
      (setf (gtk-toggle-button-active auto-move-check-button)   *auto-move*)
      (setf (gtk-toggle-button-active auto-create-check-button) *auto-create*)
      (setf (gtk-entry-text ac-name-entry)                      *ac-name*)
      (setf (gtk-entry-text ac-email-entry)                     *ac-email*)
      (setf (gtk-toggle-button-active ac-verify-check-button)   *ac-verify*)

      (case (gtk-dialog-run dlg)
        (:ok
         (setf *auto-move*   (gtk-toggle-button-active auto-move-check-button))
         (setf *auto-create* (gtk-toggle-button-active auto-create-check-button))
         (setf *ac-name*     (gtk-entry-text ac-name-entry))
         (setf *ac-email*    (gtk-entry-text ac-email-entry))
         (setf *ac-verify*   (gtk-toggle-button-active ac-verify-check-button))
         )
        (:cancel))
      )
    (gtk-widget-destroy dlg)))

(defun get-subjects (board)
  "スレッド一覧。文字列のリスト、(スレタイ レス数 スレッドID) を要素と
するリストを返す。"
  ;; したらばのsubject.txtにcharsetが指定されておらず、自動的に文字列
  ;; 化できないので、バイナリでダウンロードして文字列に変換する。
  (loop
     with subjects = nil
     with binary = (dex:get (format nil "https://jbbs.shitaraba.net/~A/subject.txt" board) :force-binary t)
     for line in (ppcre:split "\\n"
                              (babel:octets-to-string binary :encoding :eucjp))
     finally (return (nreverse (remove-duplicates subjects :test #'equal))) ; 最後にトップスレが追加されるのを削除
     do
       (multiple-value-bind
             (match-start match-end reg-starts reg-ends)
           (ppcre:scan "^(\\d+)\\.cgi,(.+)\\((\\d+)\\)$" line)
         (declare (ignore match-start match-end))
         (let ((id (subseq line (aref reg-starts 0) (aref reg-ends 0)))
               (title (subseq line (aref reg-starts 1) (aref reg-ends 1)))
               (nores (subseq line (aref reg-starts 2) (aref reg-ends 2))))
           (push (list title nores id) subjects)))))

(defmethod initialize-instance :after ((dlg thread-chooser) &key board)
  (let ((vbox (gtk-dialog-get-content-area dlg)))

    (setf (gtk-widget-size-request dlg) '(320 480))

    (gtk-dialog-add-button dlg "gtk-ok" :ok)
    (gtk-dialog-add-button dlg "gtk-cancel" :cancel)

    (let* ((subjects (get-subjects board))
           ;; Create a new list store with three columns
           (list-store (make-instance 'gtk-list-store
                                      :column-types
                                      '("gchararray" "gchararray" "gchararray")))
           (tree-view (make-instance 'gtk-tree-view :model list-store)))

      (setf (slot-value dlg 'list-store) list-store)
      (setf (slot-value dlg 'tree-view) tree-view)

      ;; タイトル レス数 [スレッドID]
      (loop for subject in subjects
         do
           (gtk-list-store-set list-store
                               (gtk-list-store-append list-store)
                               (nth 0 subject)
                               (nth 1 subject)
                               (nth 2 subject)
                               ))

      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "タイトル" renderer "text" 0)))
        (gtk-tree-view-append-column tree-view column))
      (let* ((renderer (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "レス" renderer "textb" 1)))
        (gtk-tree-view-append-column tree-view column))

      (let ((scrolled-window (make-instance 'gtk-scrolled-window
                                            :border-width 0
                                            :hscrollbar-policy :automatic
                                            :vscrollbar-policy :always)))
        (gtk-container-add scrolled-window tree-view)
        (gtk-box-pack-start vbox scrolled-window))
      (g-signal-connect
       tree-view
       "row-activated"
       (lambda (tree-view path column)
         (declare (ignore tree-view path column))
         (gtk-dialog-response dlg :ok)
         ))

      (gtk-widget-show-all vbox)
      )))

(defmethod main-window-open-thread ((window main-window) board)

  (let* ((dlg (make-instance 'thread-chooser :title "スレッド一覧" :board board)))
    (setf (gtk-window-transient-for dlg) window)
    (case (gtk-dialog-run dlg)
      (:ok
       (let ((selection (gtk-tree-view-get-selection (slot-value dlg 'tree-view))))
         (let ((iter (gtk-tree-selection-get-selected selection)))
           (when iter
             (let ((thread (first (gtk-tree-model-get (slot-value dlg 'list-store) iter 2))))
               (main-window-start window board thread))))))
      (:cancel nil))
    (gtk-widget-destroy dlg)))

(defmethod main-window-start ((window main-window) board thread)
  (setf (gtk-switch-active (main-window-load-switch window)) nil)
  (setf (gtk-entry-text (main-window-url-entry window))
        (format nil "https://jbbs.shitaraba.net/bbs/read.cgi/~A/~A/" board thread))
  (setf (gtk-switch-active (main-window-load-switch window)) t))

(defun next-thread-title (title)
  (let* ((tokens (nreverse (ppcre:all-matches-as-strings "\\d+|[^\\d]+" title)))
         (pos (position-if (lambda (token) (ppcre:scan "^\\d+$" token)) tokens)))
    (when pos
      (setf (car (nthcdr pos tokens)) (write-to-string (1+ (read-from-string (nth pos tokens)))))
      (format nil "~{~A~}" (nreverse tokens)))))

(defun prev-thread-title (title)
  (let* ((tokens (nreverse (ppcre:all-matches-as-strings "\\d+|[^\\d]+" title)))
         (pos (position-if (lambda (token) (ppcre:scan "^\\d+$" token)) tokens)))
    (when pos
      (let ((n (read-from-string (nth pos tokens))))
        (when (> n 1)
          (setf (car (nthcdr pos tokens)) (write-to-string (1- n)))
          (format nil "~{~A~}" (nreverse tokens)))))))

(defun next-thread (current-title subjects)
  (let ((target (next-thread-title current-title)))
    (unless target
      (return-from next-thread nil))

    (loop for subject in subjects
       do
         (destructuring-bind (title nores id)
             subject
           (when (string-equal title target)
             (return-from next-thread subject))))
    nil))

(defun read-url-board (url)
  (multiple-value-bind
        (match-start match-end reg-starts reg-ends)
      (ppcre:scan "^https?://jbbs.shitaraba.net/bbs/read\\.cgi/([a-z]+/\\d+)/\\d+/" url)
    (declare (ignore match-end))
    (if match-start
        (subseq url (aref reg-starts 0) (aref reg-ends 0))
        nil)))

(defun read-url-thread (url)
  (multiple-value-bind
        (match-start match-end reg-starts reg-ends)
      (ppcre:scan "^https?://jbbs.shitaraba.net/bbs/read\\.cgi/[a-z]+/\\d+/(\\d+)/" url)
    (declare (ignore match-end))
    (if match-start
        (subseq url (aref reg-starts 0) (aref reg-ends 0))
        nil)))

(defun read-url-thread-title (url)
  (res-title (make-res-from-string (get-res-line (read-url-rawmode-url url) 1))))

(defmethod initialize-instance :after ((window main-window) &key)
  (let* (
         (mocho (make-mocho :interval (floor *auto-reload-time* 1000)))

         ;; ウィジェット
         (scrolled     (make-instance 'gtk-scrolled-window
                                      :border-width 0
                                      :hscrollbar-policy :automatic
                                      :vscrollbar-policy :always))
         (menu-bar              (gtk-menu-bar-new))
         (file-menu-item        (gtk-menu-item-new-with-label "ファイル"))
         (file-menu             (gtk-menu-new))
         (quit-menu-item        (gtk-menu-item-new-with-label "終了"))
         (tools-menu-item       (gtk-menu-item-new-with-label "ツール"))
         (tools-menu            (gtk-menu-new))
         (open-thread-menu-item (gtk-menu-item-new-with-label "スレッドを開く..."))
         (post-menu-item        (gtk-menu-item-new-with-label "投稿..."))
         (new-thread-menu-item  (gtk-menu-item-new-with-label "スレッド作成..."))
         (options-menu-item     (gtk-menu-item-new-with-label "設定"))
         (options-menu          (gtk-menu-new))
         (general-menu-item     (gtk-menu-item-new-with-label "全般"))
         (notify-menu-item      (gtk-menu-item-new-with-label "通知"))
         (vadj                  (gtk-scrolled-window-get-vadjustment scrolled))
         (title-label  (make-instance 'gtk-label
                                      :label "URL"))
         (url-entry    (make-instance 'gtk-entry
                                      :text (or (load-url) "")
                                      :width-request 400))
         (load-switch  (make-instance 'gtk-switch :active nil
                                      :height-request 20
                                      :width-request 40
                                      :expand nil))
         (status-label (make-instance 'gtk-label
                                      :label "停止中"
                                      :xalign 0.0))
         (new-res-btn  (make-instance 'gtk-button
                                      :label "最新レス"
                                      :height-request 20
                                      :width-request 40
                                      :expand nil))
         (tl-vbox      (make-instance 'gtk-box ;;レス表示部分
                                      :orientation :vertical
                                      :border-width 12
                                      :spacing 6))
         (top-hbox     (make-instance 'gtk-box ;;URLとか
                                      :orientation :horizontal
                                      :spacing 6
                                      :expand nil))
         (bottom-hbox  (make-instance 'gtk-box ;;ステータスエリア
                                      :orientation :horizontal
                                      :spacing 6
                                      :expand nil))
         (win-vbox     (make-instance 'gtk-box
                                      :orientation :vertical
                                      :expand nil
                                      :spacing 6)))
    (setf (main-window-url-entry window) url-entry)
    (setf (main-window-load-switch window) load-switch)

    ;;menu
      (gtk-container-add win-vbox menu-bar)

      ;; File
      (gtk-menu-shell-append menu-bar file-menu-item)
      (setf (gtk-menu-item-submenu file-menu-item) file-menu)
      (gtk-menu-shell-append file-menu quit-menu-item)

      ;; Go
      (let ((menu (gtk-menu-new)))
        (gtk-menu-shell-append menu-bar (let ((mi (gtk-menu-item-new-with-label "移動")))
                                          (setf (gtk-menu-item-submenu mi) menu)
                                          mi))
        (gtk-menu-shell-append menu (let ((mi (gtk-menu-item-new-with-label "次スレ")))
                                      (g-signal-connect
                                       mi
                                       "activate"
                                       (lambda (w)
                                         (declare (ignore w))
                                         (and (mocho-read-url mocho)
                                              (let* ((board (read-url-board (mocho-read-url mocho)))
                                                     (ctitle (read-url-thread-title (mocho-read-url mocho)))
                                                     (ntitle (next-thread-title ctitle))
                                                     (sub (find-if (lambda (sub) (string-equal (first sub) ntitle))
                                                                   (get-subjects board))))
                                                  (if sub
                                                      (main-window-start window board (third sub))
                                                      (let ((dlg (gtk-message-dialog-new
                                                                  window
                                                                  '(:destroy-with-parent)
                                                                  :error
                                                                  :close
                                                                  "次スレ~%~A~%がみつかりません。"
                                                                  ntitle)))
                                                        (gtk-dialog-run dlg)
                                                        (gtk-widget-destroy dlg))
                                                      )))))
                                       mi))
        (gtk-menu-shell-append menu (let ((mi (gtk-menu-item-new-with-label "前スレ")))
                                      (g-signal-connect
                                       mi
                                       "activate"
                                       (lambda (w)
                                         (declare (ignore w))
                                         (and (mocho-read-url mocho)
                                              (let* ((board (read-url-board (mocho-read-url mocho)))
                                                     (ctitle (read-url-thread-title (mocho-read-url mocho)))
                                                     (ptitle (prev-thread-title ctitle))
                                                     (sub (find-if (lambda (sub) (string-equal (first sub) ptitle))
                                                                   (get-subjects board))))
                                                  (if sub
                                                      (main-window-start window board (third sub))
                                                      (let ((dlg (gtk-message-dialog-new
                                                                  window
                                                                  '(:destroy-with-parent)
                                                                  :error
                                                                  :close
                                                                  "前スレ~%~A~%がみつかりません。"
                                                                  ptitle)))
                                                        (gtk-dialog-run dlg)
                                                        (gtk-widget-destroy dlg))
                                                      )))))
                                      mi)))

      ;;Tools
      (gtk-menu-shell-append menu-bar tools-menu-item)
      (setf (gtk-menu-item-submenu tools-menu-item) tools-menu)
      (gtk-menu-shell-append tools-menu open-thread-menu-item)
      (gtk-menu-shell-append tools-menu (gtk-separator-menu-item-new))
      (gtk-menu-shell-append tools-menu post-menu-item)
      (gtk-menu-shell-append tools-menu new-thread-menu-item)
      (g-signal-connect post-menu-item "activate"
                        (lambda (w)
                          (declare (ignore w))
                          (block nil
                            (unless (mocho-read-url mocho)
                              (return)) ;?
                            (let ((dlg (make-instance 'compose-message-dialog
                                                      :transient-for window
                                                      :thread-title "")))
                              (case (gtk-dialog-run dlg)
                                (:ok
                                 (with-slots (text-view name-entry email-entry)
                                     dlg
                                   (let ((name (gtk-entry-text name-entry))
                                         (email (gtk-entry-text email-entry))
                                         (body (gtk-text-buffer-text
                                                (gtk-text-view-get-buffer text-view)))
                                         (board (read-url-board (mocho-read-url mocho)))
                                         (thread (read-url-thread (mocho-read-url mocho))))
                                     (post-message-shitaraba board thread name email body))))
                                (:cancel))
                              (gtk-widget-destroy dlg)))))

      ;;Options
      (gtk-menu-shell-append menu-bar options-menu-item)
      (setf (gtk-menu-item-submenu options-menu-item) options-menu)
      (gtk-menu-shell-append options-menu general-menu-item)
      (gtk-menu-shell-append options-menu notify-menu-item)

      (g-signal-connect
       quit-menu-item
       "activate"
       (lambda (widget)
         (declare (ignore widget))
         (main-window-quit window)))

      (g-signal-connect
       open-thread-menu-item
       "activate"
       (lambda (widget)
         (declare (ignore widget))
         (let ((url (gtk-entry-text url-entry)))
           (when (read-url-board url)
             (main-window-open-thread window (read-url-board url))))))

      (g-signal-connect
       general-menu-item
       "activate"
       (lambda (widget) (declare (ignore widget))
               (main-window-open-options window)))

      ;; Define the signal handlers
      (g-signal-connect window "destroy"
                        #'main-window-quit)
      (gtk-box-pack-start top-hbox title-label :expand nil :fill nil :padding 0)

      (gtk-box-pack-start top-hbox url-entry :expand t :fill t :padding 0)
      (gtk-box-pack-start top-hbox load-switch :expand nil :fill nil)

      (gtk-box-pack-start bottom-hbox status-label :expand t :fill t )
      (gtk-box-pack-start bottom-hbox new-res-btn :expand nil :fill nil)
      (gtk-box-pack-start win-vbox top-hbox :expand nil :fill nil)

      (g-signal-connect
       url-entry "activate"
       (lambda (widget)
         (declare (ignore widget))
         (setf (gtk-switch-active load-switch) t)))

      ;;スイッチ オートリロード
      (g-signal-connect
       load-switch
       "notify::active"
       (lambda (widget param)
         (declare (ignore param))

         (if (gtk-switch-active widget) ;; t:on nil:off
             (if (thread-url-p (gtk-entry-text url-entry))
                 (progn
                   (let ((url (normalize-thread-url (gtk-entry-text url-entry))))
                     (setf (mocho-read-url mocho) url)
                     (setf (gtk-entry-text url-entry) url)
                     (save-url url))

                   ;;一回読み込んだあとにもっかい読み込むときレス消す
                   (gtk-widget-destroy tl-vbox)
                   (setf (mocho-res-lst mocho) nil)

                   (setf tl-vbox (make-instance 'gtk-box
                                                      :orientation :vertical
                                                      :border-width 12
                                                      :spacing 6))
                   (gtk-label-set-markup status-label "スレッド読み込み中")
                   (do-later
                       ;; 読み込み処理開始。
                       (let ((data (handler-case (dex:get (mocho-rawmode-url mocho))
                                     (USOCKET:NS-HOST-NOT-FOUND-ERROR () nil)
                                     (dex:http-request-bad-request () nil)
                                     (dex:http-request-failed (e) (declare (ignore e)) nil))))
                         (when data
                           (let ((lines (dat-lines data)))
                             (setf (mocho-new-res-num mocho) (1+ (last-res-num lines)))
                             (dolist (line lines)
                               (let ((r (make-res-from-string line)))
                                 (when (string-equal "1" (res-number r))
                                   (setf (gtk-window-title window)
                                         (format nil "~A - ~A" (res-title r) +application-name+)))

                                 (add-res r mocho tl-vbox)
                                 )))))
                     (gtk-scrolled-window-add-with-viewport scrolled tl-vbox)
                     (gtk-widget-show-all scrolled)
                     (setf (main-window-id window) (g-timeout-add
                               1000
                               (lambda ()
                                 (auto-reload (mocho-rawmode-url mocho)
                                              status-label mocho vadj scrolled tl-vbox window)
                                 ;;常にカウントダウンするのでtを返す
                                 t)))
                     (do-later
                         (gtk-adjustment-set-value vadj (- (gtk-adjustment-upper vadj)
                                                           (gtk-adjustment-page-size vadj))))))
                 (progn ; else
                   (setf (gtk-switch-active load-switch) nil)
                   (gtk-label-set-markup status-label "<span color=\"#800\">エラー</span>: スレッドのURLではありません")))
             (when (/= 0 (main-window-id window)) ;;わからん オートリロード止めるはず
               (g-source-remove (main-window-id window))
               (setf (main-window-id window) 0)
               (setf (mocho-elapsed mocho) 0)
               (gtk-label-set-markup status-label "停止中")))))
      (g-signal-connect ;;最新レスへ スクロールウィンドウの一番下に行く
       new-res-btn
       "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (gtk-adjustment-set-value vadj (- (gtk-adjustment-upper vadj)
                                           (gtk-adjustment-page-size vadj)))))
      (gtk-box-pack-start win-vbox scrolled)
      (gtk-box-pack-start win-vbox bottom-hbox :expand nil :fill nil)
      (gtk-container-add window win-vbox)
  ))

(defun main ()
  (load-options)
  (within-main-loop
    (let* ((window (make-instance 'main-window
                                  :type :toplevel
                                  :title +application-name+
                                  :border-width 6
                                  :width-request 550
                                  :height-request 500)))
      (gtk-widget-show-all window)))
  (unwind-protect
       (join-gtk-main)
    (save-options)
    ))

;;(main)
