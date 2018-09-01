(ql:quickload '(:Dexador :cl-ppcre :cl-cffi-gtk))


(load "setting.lisp")


(defpackage :gtk-test
  (:use :gtk :gdk :gdk-pixbuf :gobject :setting-list
   :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :gtk-test)


(defstruct dlog
  (lst nil))

;;取得したレスの中の特殊文字っぽいものを使用できるよう（エラーでないように）に置き換え
(defun tikan-dayo (res)
  (let* ((res-kai (ppcre:regex-replace-all "~" res "~~"))
	       (res-ampa (ppcre:regex-replace-all "&(?![#A-Za-z0-9]+;)" res-kai "&amp;")))
    res-ampa))

;;<br>を~%に置き換える formatで改行するように
(defun regex-br (res1 honbun)
  (ppcre:regex-replace-all
   "<br>"
   (concatenate 'string (first res1) ":" (second res1)
		            ":" (fourth res1) "~%  "
		            honbun "~%")
   "~%  "))

;;本文のなかに >>数字 なリンクがあるときにただの >>数字 に置き換える
(defun tikan-link-bun (honbun)
  (let* ((okikae (ppcre:scan-to-strings "(<a.*?</a>)" honbun))
	       (tikan-a (ppcre:scan-to-strings "(&gt;&gt;[0-9]+</a>)" honbun))
	       (tikan (ppcre:regex-replace "</a>" tikan-a ""))
	       (honbun-t (ppcre:regex-replace okikae honbun tikan)))
    honbun-t))

;;本文の後ろにリンクくっつける
(defun add-link (url honbun)
  (concatenate 'string honbun "~%" "<a href=\"" url "\">リンク</a>"))

;;レス作成
(defun make-res (res)
  (let* ((res-1 (ppcre:split "<>" res))
         (honbun (fifth res-1))
	       (ttp (ppcre:scan-to-strings "(ttps?://[-_.!~*\'()a-zA-Z0-9;/?:@&=+$,%#]+)" honbun))
	       (http (ppcre:scan-to-strings "(https?://[-_.!~*\'()a-zA-Z0-9;/?:@&=+$,%#]+)" honbun)))
    (if (ppcre:scan "(<a.*</a>)" honbun)
        (setf honbun (tikan-link-bun honbun)))
    (cond
      ((stringp ttp) (setf honbun (add-link (concatenate 'string "h" ttp) honbun)))
      ((stringp http) (setf honbun (add-link http honbun))))
    (ppcre:regex-replace-all "<br>"
                             (concatenate 'string (first res-1) ":" (second res-1) ":"
					                                (fourth res-1) "~%  "
                                          honbun "~%")
                             "~%  ")))

;;
(defun make-res-2 (res vbox1)
  (let* ((res-1 (ppcre:split "<>" res))
	       (honbun (fifth res-1))
	       (tooltip? t)
	       (tooltip-t nil))
    (cond ((ppcre:scan "(<a.*</a>)" honbun) ;;本文の中に過去レス指定があったらツールチップで表示
	         (loop ;;改造したいところ
	               (if (ppcre:scan "(<a.*</a>)" honbun) ;;怪しい
		                 (let* ((url1 (ppcre:scan-to-strings "(/bbs.*?\")" honbun))
			                      (url2 (ppcre:regex-replace "\"" url1 ""))
			                      (url3 (concatenate 'string "http://jbbs.shitaraba.net" url2))
			                      (url4 (ppcre:regex-replace "read.cgi" url3 "rawmode.cgi"))
			                      (honbun-t (tikan-link-bun honbun))
			                      (tooltip-res1 (tikan-dayo (nth-value 0 (dex:get url4))))
			                      (tool-res1 (ppcre:split "<>" tooltip-res1))
			                      (tool-honbun (fifth tool-res1)))
		                   (if (equal "" tooltip-res1)
		                       (setf tooltip? nil)
		                       (if (ppcre:scan "(<a.*</a>)" tool-honbun)
			                         (progn
			                           (loop ;;怪しい
			                                 (if (ppcre:scan "(<a.*</a>)" tool-honbun)
				                                   (let* ((tool-honbun-m (tikan-link-bun tool-honbun)))
				                                     (setf tool-honbun tool-honbun-m))
				                                   (return)))
			                           (let* ((tooltip-res2 (regex-br tool-res1 tool-honbun)))
			                             (if tooltip-t
				                               (setf tooltip-t (concatenate 'string tooltip-t "~%" tooltip-res2))
				                               (setf tooltip-t tooltip-res2))))
			                         (let* ((tooltip-res2 (make-res tooltip-res1)))
			                           (if tooltip-t
				                             (setf tooltip-t (concatenate 'string tooltip-t "~%" tooltip-res2))
				                             (setf tooltip-t tooltip-res2)))))
		                   (setf honbun honbun-t))
		                 (return)))
	         (let* ((new-res (regex-br res-1 honbun))
		              (new-label (make-instance
			                        'gtk-label :xalign 0.0 :selectable t :use-markup t :wrap t
					                    :label (format nil new-res))))
	           (if tooltip?
		             (setf (gtk-widget-tooltip-markup new-label) (format nil tooltip-t)))
	           (gtk-widget-modify-font new-label
				                             (pango-font-description-from-string
				                              *font*))
	           (gtk-box-pack-start vbox1 new-label :expand nil)))
	        (t ;;普通にレス生成
	         (let* ((res-t (make-res res))
		              (new-label
		                (make-instance
		                 'gtk-label :xalign 0.0 :selectable t :use-markup t :wrap t
				             :label (format nil res-t))))
	           (gtk-widget-modify-font new-label
				                             (pango-font-description-from-string
				                              *font*))
	           (gtk-box-pack-start vbox1 new-label :expand nil))))))

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
  (tikan-dayo (nth-value 0 (dex:get
                            (concatenate 'string url (write-to-string new-res-num))))))

;;新着メッセージ生成と新着メッセージのポップアップ生成
(defun make-dialog (new-res dlog vbox1 scrolled vadj)
  (let* ((ress (make-res new-res))
	       (dialog (make-instance 'gtk-message-dialog
				                        :message-type :info
				                        :buttons :none :use-markup t
				                        :width-request 300
				                        :height-request 50
				                        :text ;;"新着メッセージ"
				                        ;;:secondary-text
				                        (format nil ress))))
	  ;;新着メッセージをvbox1へ
	  (make-res-2 new-res vbox1)
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
	  (g-timeout-add 1000 (lambda () ;;スクロールウィンドウの一番下へ
			                    (gtk-adjustment-set-value vadj (- (gtk-adjustment-upper vadj)
								                                            (gtk-adjustment-page-size vadj)))
				                  nil))
	  (gtk-widget-show-all scrolled)))

;;カウントダウン文字列生成  改造したいところ
(defun make-number-c (auto-time c-d-n)
  (let ((num c-d-n)
	      (num-str nil))
    (dotimes (i auto-time)
      (setf num-str (concatenate 'string
				                         num-str " " (write-to-string num)))
      (decf num))
    num-str))



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
	        (id 0)
          (load-label-id 0) (auto-time 1)
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
			                   (g-source-remove id)
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
			  (if (ppcre:scan "shitaraba.net/bbs" (gtk-entry-text title-entry))
			      (progn
				      (with-open-file (out "URL.dat" :direction :output
							                               :if-exists :supersede)
				        (format out (gtk-entry-text title-entry))) ;;読み込んだURLを書き出す
			        (gtk-widget-destroy vbox1) ;;なんだ？
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
				          (make-res-2 (tikan-dayo res) vbox1)))))
			  (setf (gtk-switch-active l-switch) t)
			  (gtk-scrolled-window-add-with-viewport scrolled vbox1)
			  (gtk-widget-show-all scrolled)))
     ;;スイッチ オートリロード
     (g-signal-connect
      l-switch
      "notify::active"
      (lambda (widget param)
        (declare (ignore param))
        (if (gtk-switch-active widget);; t:on nil:off
	          (if url
		            (setf id (g-timeout-add
                          *auto-reload-time*
					                (lambda ()
					                  (let ((res (get-new-res url new-res-num)))
                              (when (not (equal "" res))
                                (make-dialog res dialog vbox1 scrolled vadj)
                                (setf new-res-num (1+ new-res-num))))

					                  ;;常にカウントダウンするのでtを返す
					                  t))
			                load-label-id (g-timeout-add
                                     1000
						                         (lambda ()
							                         (setf (gtk-label-label count-down-label)
							                               (make-number-c auto-time c-d-n))
							                         (incf auto-time)
							                         (if (> auto-time c-d-n)
							                             (setf auto-time 1))
							                         (gtk-widget-show-all hbox1)
							                         ;;常にカウントダウンするのでtを返す
							                         t))))
	          (if (/= 0 id) ;;わからん オートリロード止めるはず
		            (progn (g-source-remove id)
			                 (g-source-remove load-label-id)
			                 (setf auto-time 1))))))
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
