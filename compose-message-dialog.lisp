(in-package :mochosa)

(defclass compose-message-dialog (gtk-dialog)
  (text-view
   name-entry
   email-entry)
  (:metaclass gobject:gobject-class))

(defun url-encode-params (alist)
  (format nil "~{~A~^&~}"
          (mapcar (lambda (pair)
                    (format nil "~A=~A"
                            (quri:url-encode (car pair))
                            (quri:url-encode (cdr pair))))
                  alist)))

(defun post-message-shitaraba (board thread-id name mail body)
  (let ((url (format nil "https://jbbs.shitaraba.net/bbs/write.cgi/~A/~A/" board thread-id))
        (referer (format nil "https://jbbs.shitaraba.net/bbs/read.cgi/~A/~A/" board thread-id)))

    (destructuring-bind
          (bbs dir)
        (ppcre:split "/" board)
      (dex:post
       url
       :headers `(("User-Agent" . "MogezouBetaWaChooSaikoo/0.1.0")
                  ("Referer" . ,referer)
                  (:content-type . "application/x-www-form-urlencoded"))
       :content (url-encode-params
                 `(("BBS" . ,bbs) ("KEY" . ,thread-id)
                   ("DIR" . ,dir) ("NAME" . ,(to-euc name))
                   ("MAIL" . ,(to-euc mail)) ("MESSAGE" . ,(to-euc body))))))))

(defun to-html-entity (char)
  (babel:string-to-octets
   (format nil "&#~A;" (char-code char))
   :encoding :ascii))

(defun to-euc (string)
  (let* ((ls (apply #'concatenate 'list
                    (map
                     'list
                     (lambda (c)
                       (handler-case
                           (babel:string-to-octets (string c) :encoding :eucjp)
                         ;; 文字がEUCにエンコードできなかった場合は、
                         ;; babel が nil に触って TYPE-ERROR になる。
                         (type-error (e)
                           (to-html-entity c))
                         ))
                     string))))
    (make-array (length ls) :element-type '(unsigned-byte 8) :initial-contents ls)
  ))

(defmethod initialize-instance :after ((dlg compose-message-dialog) &key transient-for)
  (let* ((vbox (gtk-dialog-get-content-area dlg)))
    (setf (gtk-window-transient-for dlg) transient-for)
    (setf (gtk-widget-size-request dlg) '(320 240))
    (setf (gtk-window-title dlg) "投稿")

    (gtk-dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk-dialog-add-button dlg "gtk-ok" :ok)

    (setf (gtk-box-spacing vbox) 6)
    (setf (gtk-widget-margin vbox) 6)

    (let ((hbox (make-instance 'gtk-box :orientation :horizontal :spacing 6))
          (label (gtk-label-new "名前"))
          (entry (gtk-entry-new)))
      (setf (slot-value dlg 'name-entry) entry)
      (gtk-box-pack-start hbox label)
      (gtk-box-pack-start hbox entry)
      (gtk-box-pack-start vbox hbox :expand nil))

    (let ((hbox (make-instance 'gtk-box :orientation :horizontal :spacing 6))
          (label (gtk-label-new "E-mail"))
          (entry (gtk-entry-new)))
      (setf (slot-value dlg 'email-entry) entry)
      (gtk-box-pack-start hbox label)
      (gtk-box-pack-start hbox entry)
      (gtk-box-pack-start vbox hbox :expand nil))

    (let ((text-view (make-instance 'gtk-text-view))
          (scrolled-window (make-instance 'gtk-scrolled-window)))
      (setf (slot-value dlg 'text-view) text-view)
      (gtk-container-add scrolled-window text-view)
      (gtk-box-pack-start vbox scrolled-window))

    (gtk-widget-show-all vbox)
  ))
