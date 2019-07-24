;; -*- tab-width: 2 -*-

(ql:quickload '(:Dexador :cl-ppcre :cl-cffi-gtk))

(defpackage :mochosa
  (:use :gtk :gdk :gdk-pixbuf :gobject
        :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :mochosa)

(defparameter *auto-reload-time* 9000) ;;自動読み込みの時間
(defparameter *popup-time* 8000) ;;新着メッセージのポップアップ時間
(defparameter *sound*
  ;;'("/usr/share/sounds/purple/receive.wav")) ;;効果音の場所
  "/usr/share/sounds/Yaru/stereo/message-new-instant.ogg")
(defparameter *say-command* "vtsay")

(defconstant +application-name+ "もげぞうβは超サイコー")

(defstruct mocho
  (res-lst nil) ;;レスstructリスト
  (dlog-lst nil)
  (new-res-num 0)
  (countdown 0) ;;カウントダウン表示用
  (cdn 0)) ;;カウントダウン設定

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

;;アンカーのタグを外した本文と、アンカーに指定されていたread.cgiへの
;;パスのリストを返す
(defun parse-untag-anchors (honbun)
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

;;レス作成　Pangoマークアップされた本文と、アンカーのパスリストを返す
(defmethod parse-res ((r res))
  (multiple-value-bind
        (html path-list)
      (compose-html r)
    (values (html->pango-markup html) path-list)))

;;アンカーのパスを該当レスを取得するrawmode.cgiへのURLに変換する
(defun anchor-path->rawmode-url (path)
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

;;レスをPangoマークアップにしてoutput-streamに出力する
(defmethod print-res (output-stream (r res))
  (format output-stream "~A" (parse-res r)))

;;アンカーのリストから、該当レスを取得してPangoマークアップとして整形する
(defmethod make-tooltip-text ((mocho mocho) anchor-path-list)
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

;;ダイアログの位置を調整
(defun reset-dialog-position (mocho)
  (loop for dialog in (reverse (mocho-dlog-lst mocho))
        for i from 0
        do (gtk-window-move dialog ;;ダイアログ表示する位置
                            (- (gdk-screen-width) 400)
                            (* i 130))
           (gtk-widget-show dialog)))

;;新着レス
(defun get-new-res (url new-res-num)
  (handler-case (dex:get (concatenate 'string url (write-to-string new-res-num)))
    (USOCKET:NS-HOST-NOT-FOUND-ERROR () nil)
    (dex:http-request-bad-request () nil)
    (dex:http-request-failed (e) (declare (ignore e)) nil)
    ))

;;新着メッセージ生成と新着メッセージのポップアップ生成
(defmethod make-dialog ((r res) (mocho mocho))
  (let* ((honbun (parse-res r)))
    ;;ダイアログフォント
    (sb-ext:run-program "paplay" (list *sound*) :search t) ;;音ならす
    (sb-ext:run-program "notify-send" (list "新着メッセージ" honbun) :search t)
    ))

;;スクロールウィンドウの一番下へ
(defun scroll-bot (vadj)
  (g-timeout-add 600 (lambda ()
                         (gtk-adjustment-set-value vadj (- (gtk-adjustment-upper vadj)
                                                           (gtk-adjustment-page-size vadj)))
                         nil)))

;;カウントダウン文字列生成  改造したいところ
(defun make-number-c (auto-time c-d-n)
  (let ((out (make-string-output-stream)))
    (loop for i from c-d-n downto 0
          repeat auto-time
          do (format out "~a " (write-to-string i)))
    (get-output-stream-string out)))

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

(defun get-voicetext-license-key ()
  (with-open-file (in "/home/plonk/.voiceapi" :direction :input)
    (read-line in)))

(defun untag-all (string)
  (ppcre:regex-replace-all "<[^>]*>" string ""))

(defun speak (str)
  (let ((words (remove "" (ppcre:split "\\s+" *say-command*) :test #'string-equal)))
    (when words
      (handler-case
          (sb-ext:run-program (first words) (append (rest words) (list str)) :search t)
        (simple-error (e) (format t "~A~%" e))))))

(defmethod speak-res ((r res))
  (speak (untag-all (res-honbun r))))

;;オートリロード新着レス表示
(defun auto-reload (url count-down-label mocho vadj scrolled hbox1 vbox1)
  (let ((cd (incf (mocho-countdown mocho)))
        (cdn (mocho-cdn mocho)))
    (setf (gtk-label-label count-down-label)
          (make-number-c cd cdn))
    (gtk-widget-show-all hbox1)
    (when (> cd cdn)
      (loop with line
         while (not (equal "" (setf line (get-new-res url (mocho-new-res-num mocho)))))
         do
           (let ((r (make-res-from-string line)))
             (make-dialog r mocho)
             (add-res r mocho vbox1)
             (scroll-bot vadj)
             (gtk-widget-show-all scrolled)
             (speak-res r)
             (incf (mocho-new-res-num mocho))
             ))
      (setf (mocho-countdown mocho) 1))))



;;ポップアップサウンド設定
(defun set-popup-sound (window)
  (let ((hoge (gtk-file-chooser-dialog-new "sound" window :open "OK" :ok "cancel" :cancel)))
    (gtk-file-chooser-set-current-folder hoge "/usr/share/sounds")
    (case (gtk-dialog-run hoge)
      (:ok (setf *sound* (gtk-file-chooser-get-filename hoge)))
      (:cancel nil))
    (gtk-widget-destroy hoge)))

;;options.dat 1:オートリロード時間 2:ポップアップタイム 3:サウンド
;;設定保存
(defun save-options ()
  (with-open-file (out "options.dat" :direction :output
                                     :if-exists :supersede)
    (dolist (value `(,*auto-reload-time* ,*popup-time* ,*sound* ,*say-command*))
      (format out "~s~%" value))))

;;設定読み込み
(defun load-options ()
  (with-open-file (in "options.dat" :direction :input
                                    :if-does-not-exist nil)
    (when in
      (loop for line = (read in nil)
            while line
            for i in '(*auto-reload-time* *popup-time* *sound* *say-command*)
            do (eval `(setf ,i ,line))))))

(defun main ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title +application-name+
                                  :border-width 6
                                  :width-request 550
                                  :height-request 500))
           (scrolled (make-instance 'gtk-scrolled-window
                                    :border-width 0
                                    :hscrollbar-policy :automatic
                                    :vscrollbar-policy :always))
           (menu-1 (gtk-menu-bar-new))
           (options-item (gtk-menu-item-new-with-label "Options"))
           (options-menu (gtk-menu-new))
           (sound-item (gtk-menu-item-new-with-label "Set Popup Sound"))
           (speech-item (gtk-menu-item-new-with-label "Speech..."))
           (vadj (gtk-scrolled-window-get-vadjustment scrolled))
           (title-label (make-instance 'gtk-label :label "URL"))
           (auto-load-label (make-instance 'gtk-label :label "自動読込"))
           (preurl (with-open-file (in "URL.dat" :direction :input)
                     (read-line in nil))) ;;前回開いたURL

           (title-entry (make-instance 'gtk-entry :text (if (null preurl) "" preurl) :width-request 400))
           (rawmode-url nil) (mocho (make-mocho :cdn (floor *auto-reload-time* 1000)))
           (id 0) ;;(res-array (make-array 1000))
           (load-button (make-instance 'gtk-button :label "読込"
                                              :height-request 20 :width-request 40 :expnad nil))
           (l-switch (make-instance 'gtk-switch :active nil
                                                :height-request 20 :width-request 40 :expnad nil))
           (load-label (make-instance 'gtk-label :label "読み込み中"
                                                 :rgba (gdk-rgba-parse "Blue") ))
           (count-down-label (make-instance 'gtk-label :label "(´・ω・｀)" :xalign 0.0))
           (test-btn (make-instance 'gtk-button :label "最新レス"
                                                :height-request 20 :width-request 40 :expnad nil))
           (vbox1 (make-instance 'gtk-box ;;レス表示部分
                                 :orientation :vertical
                                 :border-width 12
                                 :spacing 6))
           (hbox1 (make-instance 'gtk-box ;;オートリロードしてるとこ
                                 :orientation :horizontal
                                 :spacing 6 :expand nil))
           (hbox (make-instance 'gtk-box ;;URLとか
                                :orientation :horizontal
                                :spacing 6 :expand nil))
           (vbox2 (make-instance 'gtk-box ;;vbox1とhbox1とhboxいれてる？
                                 :orientation :vertical
                                 :expand nil
                                 :spacing 6)))
      (load-options);;初期設定読み込み
      ;;menu
      (gtk-menu-shell-append menu-1 options-item)
      (setf (gtk-menu-item-submenu options-item) options-menu)
      (gtk-menu-shell-append options-menu sound-item)
      (gtk-menu-shell-append options-menu speech-item)
      (gtk-container-add vbox2 menu-1)
      ;;サウンド設定
      (g-signal-connect sound-item "activate"
                        (lambda (widget)
                          (declare (ignore widget))
                          (set-popup-sound window)))
      (g-signal-connect speech-item "activate"
                        (lambda (widget)
                          (declare (ignore widget))

                          (let* ((dlg (gtk-dialog-new-with-buttons "読み上げ設定" window '(:modal)))
                                 (vbox (gtk-dialog-get-content-area dlg))
                                 (hbox (make-instance 'gtk-box :orientation :horizontal :spacing 6))
                                 (entry (gtk-entry-new))
                                 (test-btn (make-instance 'gtk-button :label "テスト")))
                            (gtk-dialog-add-button dlg "gtk-cancel" :cancel)
                            (gtk-dialog-add-button dlg "gtk-ok" :ok)
                            (gtk-box-pack-start hbox (gtk-label-new "コマンド:") :expand nil)
                            (gtk-box-pack-start hbox entry :expand t :fill t)
                            (setf (gtk-entry-text entry) *say-command*)
                            (setf (gtk-widget-tooltip-text test-btn) "現在の設定で「テスト」と読み上げます。")
                            (gtk-box-pack-start hbox test-btn :expand nil)
                            (g-signal-connect test-btn "clicked"
                                              (lambda (w)
                                                (declare (ignore w))
                                                (let ((*say-command* (gtk-entry-text entry))) ; 動的束縛
                                                  (speak "テスト"))))
                            (gtk-box-pack-start vbox hbox)
                            (gtk-widget-show-all vbox)
                            (case (gtk-dialog-run dlg)
                              (:ok (setf *say-command* (gtk-entry-text entry))))
                            (gtk-widget-destroy dlg)
                            )
                          ))
      ;; Define the signal handlers
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (when (/= id 0)
                            (g-source-remove id))
                          (leave-gtk-main)))
      (gtk-box-pack-start hbox title-label :expand nil :fill nil :padding 0)

      (gtk-box-pack-start hbox title-entry :expand t :fill t :padding 0)
      (gtk-box-pack-start hbox load-button :expand nil :fill nil)

      (gtk-box-pack-start hbox1 auto-load-label :expand nil :fill nil)
      (gtk-box-pack-start hbox1 l-switch :expand nil :fill nil)
      (gtk-box-pack-start hbox1 load-label :expand nil :fill nil)
      (gtk-box-pack-start hbox1 count-down-label :expand t :fill t )
      (gtk-box-pack-start hbox1 test-btn :expand nil :fill nil)
      (gtk-box-pack-start vbox2 hbox :expand nil :fill nil)

      (g-signal-connect
       title-entry "activate"
       (lambda (widget)
         (declare (ignore widget))
         (gtk-button-clicked load-button)))

      (g-signal-connect ;;読み込みボタン
       load-button "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (when (thread-url-p (gtk-entry-text title-entry))
           (setf (gtk-entry-text title-entry)
                 (normalize-thread-url (gtk-entry-text title-entry)))
           (with-open-file (out "URL.dat" :direction :output
                                          :if-exists :supersede)
             (format out (gtk-entry-text title-entry))) ;;読み込んだURLを書き出す

           ;;一回読み込んだあとにもっかい読み込むときレス消す
           (gtk-widget-destroy vbox1)
           (setf (mocho-res-lst mocho) nil)

           (setf vbox1 (make-instance 'gtk-box
                                      :orientation :vertical
                                      :border-width 12
                                      :spacing 6))
           (setf rawmode-url (cl-ppcre:regex-replace "read"
                                             (gtk-entry-text title-entry)
                                             "rawmode"))
           (let ((data (handler-case (dex:get rawmode-url)
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

                     (add-res r mocho vbox1)
                     )))))
           (setf (gtk-switch-active l-switch) t)
           (gtk-scrolled-window-add-with-viewport scrolled vbox1)
           (g-timeout-add ;;0.5秒後に一番下にいく
            500
            (lambda ()
              (gtk-adjustment-set-value vadj (- (gtk-adjustment-upper vadj)
                                                (gtk-adjustment-page-size vadj)))
              nil))
           (gtk-widget-show-all scrolled))))
      ;;スイッチ オートリロード
      (g-signal-connect
       l-switch
       "notify::active"
       (lambda (widget param)
         (declare (ignore param))
         (if (gtk-switch-active widget) ;; t:on nil:off
             (when rawmode-url
               (setf id (g-timeout-add
                         1000
                         (lambda ()
                           (auto-reload rawmode-url count-down-label mocho vadj scrolled hbox1 vbox1)
                           (gtk-widget-show-all hbox1)
                           ;;常にカウントダウンするのでtを返す
                           t))))
             (when (/= 0 id) ;;わからん オートリロード止めるはず
               (g-source-remove id)
               (setf (mocho-countdown mocho) 1)))))
      (g-signal-connect ;;最新レスへ スクロールウィンドウの一番下に行く
       test-btn
       "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (gtk-adjustment-set-value vadj (- (gtk-adjustment-upper vadj)
                                           (gtk-adjustment-page-size vadj)))))
      (gtk-box-pack-start vbox2 scrolled)
      (gtk-box-pack-start vbox2 hbox1 :expand nil :fill nil)
      ;; Put the table into the window
      (gtk-container-add window vbox2)
      ;; Show the window
      (gtk-widget-show-all window)))
  (unwind-protect
       (join-gtk-main)

    (save-options)
    ))

;;(main)
