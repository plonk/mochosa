;;ここの全部ウィンドウで設定できるようにしたい

(defpackage :setting-list
  (:use :common-lisp)
  (:export #:*auto-reload-time* #:*popup-time* #:*font* #:*sound*))

(in-package :setting-list)

;;単位はミリ秒 9000→９秒
(defparameter *auto-reload-time* 9000) ;;自動読み込みの時間
(defparameter *popup-time* 8000) ;;新着メッセージのポップアップ時間
(defparameter *font* "Sans Regular 13") ;;フォント設定　"[FAMILY-LIST] [STYLE-OPTIONS] [SIZE]"
(defparameter *sound*
  ;;'("/usr/share/sounds/purple/receive.wav")) ;;効果音の場所
  '("/usr/share/sounds/ubuntu/stereo/dialog-question.ogg"))
