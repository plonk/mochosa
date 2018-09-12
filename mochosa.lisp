;; -*- tab-width: 2 -*-

(ql:quickload '(:Dexador :cl-ppcre :cl-cffi-gtk))


(load "setting.lisp")


(defpackage :mochosa
  (:use :gtk :gdk :gdk-pixbuf :gobject :setting-list
   :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :mochosa)


(defstruct dlog
  (lst nil))

;;文字実体参照の先頭でない、孤立したアンパサンドを &amp; に置換
(defun escape-amp (res)
  (ppcre:regex-replace-all "&(?![#A-Za-z0-9]+;)" res "&amp;"))

;;<br>を改行文字に置き換える
(defun regex-br (res honbun)
  (ppcre:regex-replace-all "<br>"
                           (concatenate 'string (first res) ":" (second res) ":"
                                        (fourth res) "<br>"
                                        honbun "<br>")
                           (format nil "~%")))

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
                        text)))))
      (values honbun1  (reverse path-list)))))

;;レス作成　GTKマークアップされた本文と、アンカーのパスリストを返す
(defun make-res (res-string)
  (let* ((res-1 (ppcre:split "<>" res-string))
	       (honbun (fifth res-1)))
    (multiple-value-bind
        (honbun1 path-list)
        (parse-untag-anchors honbun)
      (values (regex-br res-1 (linkify-urls honbun1)) path-list))))

;;アンカーのパスを該当レスを取得するrawmode.cgiへのURLに変換する
(defun anchor-path->rawmode-url (path)
  (concatenate 'string "https://jbbs.shitaraba.net"
               (ppcre:regex-replace "read\.cgi" path "rawmode.cgi")))

;;ramode.cgiで取得されるDATを一行ごとのリストにする
(defun dat-lines (data-text)
  (ppcre:split "\\n" data-text))

;;レスをGTKマークアップにしてoutput-streamに出力する
(defun print-res (output-stream res-1)
  (format output-stream "~A" (make-res res-1)))

;;アンカーのリストから、該当レスを取得してGTKマークアップとして整形する
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
  (let ((tooltip-text nil))
    (multiple-value-bind
        (tagged-text anchor-path-list)
        (make-res dat-line)

      (when anchor-path-list
        (setf tooltip-text (make-tooltip-text anchor-path-list)))

      (let* ((new-label (make-instance
                         'gtk-label :xalign 0.0 :selectable t :use-markup t :wrap t
                         :label tagged-text)))
        (when tooltip-text
          (setf (gtk-widget-tooltip-markup new-label) tooltip-text))
        (gtk-widget-modify-font new-label (pango-font-description-from-string *font*))
        (gtk-box-pack-start vbox1 new-label :expand nil)))))

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
  (escape-amp (nth-value 0 (dex:get
                            (concatenate 'string url (write-to-string new-res-num))))))

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



(defun main ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window
                                  :type :toplevel
                                  :title "もげぞうβは超サイコー"
                                  :border-width 0
                                  :width-request 550
                                  :height-request 500))
           (scrolled (make-instance 'gtk-scrolled-window
                                    :border-width 12
                                    :hscrollbar-policy :automatic
                                    :vscrollbar-policy :always))
           (new-res-num 0)
           (vadj (gtk-scrolled-window-get-vadjustment scrolled))
           (title-label (make-instance 'gtk-label :label "URL:"))
           (auto-load-label (make-instance 'gtk-label :label "自動読込:"))
           (preurl (with-open-file (in "URL.dat" :direction :input)
                     (read-line in nil))) ;;前回開いたURL

           (title-entry (make-instance 'gtk-entry :text (if (null preurl) "" preurl) :width-request 400))
           (url nil) (dialog (make-dlog))
           (id 0) ;;(res-array (make-array 1000))
           (auto-time 1)
           (c-d-n (floor *auto-reload-time* 1000)) ;;カウントダウン用
           (button (make-instance 'gtk-button :label "読込"
                                              :height-request 20 :width-request 40 :expnad nil))
           (l-switch (make-instance 'gtk-switch :active nil
                                                :height-request 20 :width-request 40 :expnad nil))
           (load-label (make-instance 'gtk-label :label "読み込み中:"))
           (count-down-label (make-instance 'gtk-label :label "(´・ω・｀)"))
           (test-btn (make-instance 'gtk-button :label "最新レス"
                                                :height-request 20 :width-request 40 :expnad nil))
           (vbox1 (make-instance 'gtk-box ;;レス表示部分
                                 :orientation :vertical
                                 :border-width 12
                                 :spacing 6))
           (hbox1 (make-instance 'gtk-box ;;オートリロードしてるとこ
                                 :orientation :horizontal
                                 :spacing 1 :expand nil))
           (hbox (make-instance 'gtk-box ;;URLとか
                                :orientation :horizontal
                                :spacing 1 :expand nil))
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

      (gtk-box-pack-start hbox title-entry :expand nil :fill nil :padding 0)
      (gtk-box-pack-start hbox button :expand nil :fill nil)

      (gtk-box-pack-start hbox1 auto-load-label :expand nil :fill nil)
      (gtk-box-pack-start hbox1 l-switch :expand nil :fill nil)
      (gtk-box-pack-start hbox1 load-label :expand nil :fill nil)
      (gtk-box-pack-start hbox1 count-down-label :expand nil :fill nil)
      (gtk-box-pack-start hbox1 test-btn :expand nil :fill nil)
      (gtk-box-pack-start vbox2 hbox :expand nil :fill nil)

      (g-signal-connect ;;読み込みボタン
       button "clicked"
       (lambda (widget)
         (declare (ignore widget))
         (when (ppcre:scan "shitaraba.net/bbs" (gtk-entry-text title-entry))
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
             (setf new-res-num (1+ (length res-list)))
             (dolist (res res-list)
               (make-res-2 (escape-amp res) vbox1)))
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
                           (setf (gtk-label-label count-down-label)
                                 (make-number-c auto-time c-d-n))
                           (incf auto-time)
                           (when (> auto-time c-d-n)
                             (let ((res (get-new-res url new-res-num)))
                               (when (not (equal "" res))
                                 (make-dialog res dialog vadj)
                                 (make-res-2 res vbox1)
                                 (gtk-widget-show-all scrolled)
                                 (incf new-res-num))
                               (setf auto-time 1)))
                           (gtk-widget-show-all hbox1)
                           ;;常にカウントダウンするのでtを返す
                           t))))
             (when (/= 0 id) ;;わからん オートリロード止めるはず
               (g-source-remove id)
               (setf auto-time 1)))))
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

(main)
