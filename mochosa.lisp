;; -*- tab-width: 2 -*-

(ql:quickload '(:Dexador :cl-ppcre :cl-cffi-gtk))


(load "setting.lisp")


(defpackage :mochosa
  (:use :gtk :gdk :gdk-pixbuf :gobject :setting-list
   :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :mochosa)

(defconstant +application-name+ "もげぞうβは超サイコー")

(defstruct dlog
  (lst nil)
  (new-res-num 0)
  (countdown 0) ;;カウントダウン表示用
  (cdn 0)) ;;カウントダウン設定

(declaim (ftype (function (string) string) fix-semicolonless-amp)
         (ftype (function (string) string) escape-stray-amp)
         (ftype (function (string) (values string list)) parse-untag-anchors)
         (ftype (function (string) string) linkify-urls))

(defun res-number (res)
  (first res))
(defun res-name-html (res)
  ;; 名前欄では & が &amp (←セミコロンなし)とエスケープされるので、こ
  ;; れを正しい &amp; に直す。
  (fix-semicolonless-amp (second res)))
(defun res-mail (res)
  (third res))
(defun res-date (res)
  (fourth res))
(defun res-honbun-html (res)
  (multiple-value-bind
        (honbun path-list)
      (parse-untag-anchors (fifth res))
    (values (escape-stray-amp (linkify-urls honbun))
            path-list)))
(defun res-title (res)
  (sixth res))

;;文字実体参照の先頭でない、孤立したアンパサンドを &amp; に置換
(defun escape-stray-amp (res)
  (ppcre:regex-replace-all "&(?![#A-Za-z0-9]+;)" res "&amp;"))

(defun fix-semicolonless-amp (str)
  (ppcre:regex-replace-all "&amp" str "&amp;"))

;; メールのあるなしに応じて色を付けた名前のマークアップを返す。
(defun colorized-name-html (res)
  (if (string-equal "" (res-mail res))
      (format nil "<span color=\"#008800\"><b>~A</b></span>"
              (res-name-html res))
      (format nil "<span color=\"#0000FF\" underline=\"single\"><b>~A</b></span>"
              (res-name-html res))))

;; レス全体をHTMLとして生成する。
(defun compose-html (res)
  (multiple-value-bind
        (honbun path-list)
      (res-honbun-html res)
    (values
     (format nil "<span color=\"#0000FF\" underline=\"single\">~A</span>：~A：~A<br>~A<br>"
             (res-number res)
             (colorized-name-html res)
             (res-date res)
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
(defun make-res (res-string)
  (multiple-value-bind
        (html path-list)
      (compose-html (ppcre:split "<>" res-string))
    (values (html->pango-markup html) path-list)))

;;アンカーのパスを該当レスを取得するrawmode.cgiへのURLに変換する
(defun anchor-path->rawmode-url (path)
  (concatenate 'string "https://jbbs.shitaraba.net"
               (ppcre:regex-replace "read\.cgi" path "rawmode.cgi")))

;;ramode.cgiで取得されるDATを一行ごとのリストにする
(defun dat-lines (data-text)
  (ppcre:split "\\n" data-text))

;;レスをPangoマークアップにしてoutput-streamに出力する
(defun print-res (output-stream res-1)
  (format output-stream "~A" (make-res res-1)))

;;アンカーのリストから、該当レスを取得してPangoマークアップとして整形する
(defun make-tooltip-text (anchor-path-list)
  (let ((rawmode-urls (mapcar #'anchor-path->rawmode-url anchor-path-list))
        (output (make-string-output-stream)))
    (dolist (url rawmode-urls)
      (dolist (dat-line (dat-lines (nth-value 0 (dex:get url))))
        (print-res output dat-line)
        (terpri output))) ;; 一行あける。
    (get-output-stream-string output)))

;;DAT行のレスをGtkLabelとしてGtkBoxに追加する。
(defun make-res-2 (dat-line vbox1)
  (multiple-value-bind
        (tagged-text anchor-path-list)
      (make-res dat-line)
    (let* ((new-label (make-instance
                       'gtk-label :xalign 0.0 :selectable t :use-markup t :wrap t
                       :label tagged-text)))
      (when anchor-path-list
        (setf (gtk-widget-tooltip-markup new-label) (make-tooltip-text anchor-path-list)))
      (gtk-widget-modify-font new-label (pango-font-description-from-string *font*))
      (gtk-box-pack-start vbox1 new-label :expand nil))))

;;ダイアログの位置を調整
(defun reset-dialog-position (dlog)
  (loop for dialog in (reverse (dlog-lst dlog))
        for i from 0
        do (gtk-window-move dialog ;;ダイアログ表示する位置
			                      (- (gdk-screen-width) 400)
			                      (* i 130))
           (gtk-widget-show dialog)))

;;新着レス
(defun get-new-res (url new-res-num)
  (nth-value 0 (dex:get (concatenate 'string url (write-to-string new-res-num)))))

;;新着メッセージ生成と新着メッセージのポップアップ生成
(defun make-dialog (new-res dlog vadj)
  (let* ((ress (make-res new-res))
	       (dialog (make-instance 'gtk-message-dialog
				                        :message-type :info
				                        :buttons :none :use-markup t
				                        :width-request 300
				                        :height-request 50
				                        :text ;;"新着メッセージ"
				                        ;;:secondary-text
				                        ress)))
	  ;;新着メッセージをvbox1へ
    ;;(make-res-2 new-res vbox1)
	  ;;ダイアログフォント
	  (gtk-widget-modify-font dialog
				                    (pango-font-description-from-string
				                     *font*))
	  (gtk-window-move dialog ;;ダイアログ表示する位置
			               (- (gdk-screen-width) 400)
			               (* (length (dlog-lst dlog)) 130))
    (push dialog (dlog-lst dlog)) ;;リストに追加
	  (sb-ext:run-program "/usr/bin/paplay" *sound*) ;;音ならす
	  (gtk-widget-show dialog) ;;ダイアログ表示
	  ;;popuoが出た時だけ(一度だけ)カウントダウンなのでnilを返す
	  (g-timeout-add *popup-time* (lambda () ;;時間たったらダイアログ消す
					                        (gtk-widget-destroy dialog)
                                  (setf (dlog-lst dlog)
                                        (remove dialog
                                                (dlog-lst dlog) :test #'equalp))
                                  (when (dlog-lst dlog)
                                    (reset-dialog-position dlog))
					                        nil))
	  (g-timeout-add 300 (lambda () ;;スクロールウィンドウの一番下へ
                         (gtk-adjustment-set-value vadj (- (gtk-adjustment-upper vadj)
                                                           (gtk-adjustment-page-size vadj)))
                         nil))))
;;(gtk-widget-show-all scrolled)))

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

;;オートリロード新着レス表示
(defun auto-reload (url count-down-label dialog vadj scrolled hbox1 vbox1)
  (let ((cd (incf (dlog-countdown dialog)))
        (cdn (dlog-cdn dialog)))
    (setf (gtk-label-label count-down-label)
          (make-number-c cd cdn))
    (gtk-widget-show-all hbox1)
    (when (> cd cdn)
      (loop for res = (get-new-res url (dlog-new-res-num dialog)) then res
            while (not (equal "" res))
            do (make-dialog res dialog vadj)
               (make-res-2 res vbox1)
               (gtk-widget-show-all scrolled)
               (incf (dlog-new-res-num dialog))
               (setf res (get-new-res url (dlog-new-res-num dialog))))
      (setf (dlog-countdown dialog) 1))))

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
           (vadj (gtk-scrolled-window-get-vadjustment scrolled))
           (title-label (make-instance 'gtk-label :label "URL"))
           (auto-load-label (make-instance 'gtk-label :label "自動読込"))
           (preurl (with-open-file (in "URL.dat" :direction :input)
                     (read-line in nil))) ;;前回開いたURL

           (title-entry (make-instance 'gtk-entry :text (if (null preurl) "" preurl) :width-request 400))
           (url nil) (dialog (make-dlog :cdn (floor *auto-reload-time* 1000)))
           (id 0) ;;(res-array (make-array 1000))
           (button (make-instance 'gtk-button :label "読込"
                                              :height-request 20 :width-request 40 :expnad nil))
           (l-switch (make-instance 'gtk-switch :active nil
                                                :height-request 20 :width-request 40 :expnad nil))
           (load-label (make-instance 'gtk-label :label "読み込み中"))
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
      ;; Define the signal handlers
      (g-signal-connect window "destroy"
                        (lambda (w)
                          (declare (ignore w))
                          (when (/= id 0)
                            (g-source-remove id))
                          (leave-gtk-main)))
      (setf (gtk-widget-tooltip-text button) "hoge")
      (gtk-box-pack-start hbox title-label :expand nil :fill nil :padding 0)

      (gtk-box-pack-start hbox title-entry :expand t :fill t :padding 0)
      (gtk-box-pack-start hbox button :expand nil :fill nil)

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
         (gtk-button-clicked button)))

      (g-signal-connect ;;読み込みボタン
       button "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (when (thread-url-p (gtk-entry-text title-entry))
           (setf (gtk-entry-text title-entry)
                 (normalize-thread-url (gtk-entry-text title-entry)))
           (with-open-file (out "URL.dat" :direction :output
                                          :if-exists :supersede)
             (format out (gtk-entry-text title-entry))) ;;読み込んだURLを書き出す
           (gtk-widget-destroy vbox1) ;;一回読み込んだあとにもっかい読み込むときレス消す
           (setf vbox1 (make-instance 'gtk-box
                                      :orientation :vertical
                                      :border-width 12
                                      :spacing 6))
           (setf url (cl-ppcre:regex-replace "read"
                                             (gtk-entry-text title-entry)
                                             "rawmode"))
           (let ((res-list
                   (ppcre:split "\\n"
                                (nth-value 0 (dex:get url)))))
             (setf (dlog-new-res-num dialog) (1+ (last-res-num res-list)))
             (dolist (res res-list)
               (let ((r (ppcre:split "<>" res)))
                 (when (string-equal "1" (res-number r))
                     (setf (gtk-window-title window)
                           (format nil "~A - ~A" +application-name+ (res-title r))))

               (make-res-2 res vbox1))))
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
             (when url
               (setf id (g-timeout-add
                         1000
                         (lambda ()
                           (auto-reload url count-down-label dialog vadj scrolled hbox1 vbox1)
                           (gtk-widget-show-all hbox1)
                           ;;常にカウントダウンするのでtを返す
                           t))))
             (when (/= 0 id) ;;わからん オートリロード止めるはず
               (g-source-remove id)
               (setf (dlog-countdown dialog) 1)))))
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
  (join-gtk-main))

;;(main)
