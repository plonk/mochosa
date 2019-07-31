(in-package :mochosa)

(defclass notification-settings-dialog (gtk-dialog)
  (entry-box)
  (:metaclass gobject:gobject-class))

(defmethod notification-settings-dialog-get-programs ((dlg notification-settings-dialog))
  (let* ((ebox (first (gtk-container-get-children (gtk-dialog-get-content-area dlg))))
         ls)
    (loop for ent in (gtk-container-get-children ebox)
       do
         (let ((text (gtk-entry-text ent)))
           (if (not (string-equal "" text))
               (push text ls))))
    (nreverse ls)))

(defmethod initialize-instance :after ((dlg notification-settings-dialog) &key transient-for)
  (let* ((vbox (gtk-dialog-get-content-area dlg))
         (ebox (make-instance 'gtk-box :orientation :vertical :spacing 6)))

    (setf (slot-value dlg 'entry-box) ebox)

    (setf (gtk-window-transient-for dlg) transient-for)
    (setf (gtk-widget-size-request dlg) '(320 240))
    (setf (gtk-window-title dlg) "通知設定")

    (gtk-dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk-dialog-add-button dlg "gtk-ok" :ok)

    (setf (gtk-box-spacing vbox) 6)
    (setf (gtk-widget-margin vbox) 6)

    (loop for cmdline in *notification-programs*
       do
         (let ((e (gtk-entry-new)))
           (setf (gtk-entry-text e) cmdline)
           (gtk-box-pack-start ebox e :expand nil)))

    (gtk-box-pack-start vbox ebox :expand nil)

    (let ((btn (gtk-button-new-from-stock "gtk-add")))
      (g-signal-connect
       btn "clicked"
       (lambda (w)
         (declare (ignore w))
         (gtk-box-pack-start ebox (gtk-entry-new))
         (gtk-widget-show-all ebox)))
      
      (gtk-box-pack-start vbox btn :expand nil))

    (let ((btn (gtk-button-new-with-label "テスト")))
      (g-signal-connect
       btn "clicked"
       (lambda (w)
         (declare (ignore w))
         (let ((*notification-programs* (notification-settings-dialog-get-programs dlg)))
           (let ((r (make-res :number "1"
                              :name "名無しさん"
                              :mail "sage"
                              :date "1970/01/01(木) 09:00:00"
                              :honbun "テスト"
                              :title "テストスレ")))
             (notify r)))))
      (gtk-box-pack-start vbox btn :expand nil))

    (gtk-widget-show-all vbox)))
