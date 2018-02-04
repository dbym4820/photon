(in-package :cl-user)
(defpackage photon.utils
  (:use :cl)
  (:export :timestamp))
(in-package :photon.utils)

(defun timestamp (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (s m h dd mm yy day) (decode-universal-time universal-time)
    (format NIL "~[Monday~;Tuesday~;Wednesday~;Thursday~;Friday~;Saturday~;Sunday~] ~
                 the ~d~[st~;nd~;rd~:;th~] ~
                 of ~[January~;February~;March~;April~;May~;June~;July~;August~;September~;October~;November~;December~] ~
                 ~d, ~
                 ~2,'0d:~2,'0d:~2,'0d"
            day dd (1- (mod dd 10)) (1- mm) yy h m s)))
