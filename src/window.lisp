(in-package :cl-user)
(defpackage photon.window
  (:use :cl+qt)
  (:import-from :qtools
                :with-main-window
                :define-widget
                :define-subwidget)
  (:import-from :photon.utils
                :timestamp)
  (:export :generate-window
           :display-window
           :vanish-window))
(in-package :photon.window)

(defpackage photon.window
  (:use #:cl+qt)
  (:export :main))
(in-package :photon.window)
(named-readtables:in-readtable :qtools)

(define-widget main (QWidget)
  ())

(define-subwidget (main button) (q+:make-qpushbutton "Click Me!" main))

(define-subwidget (main layout) (q+:make-qhboxlayout main)
  (q+:add-widget layout button))

(defun timestamp (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (s m h dd mm yy day) (decode-universal-time universal-time)
    (format NIL "~[Monday~;Tuesday~;Wednesday~;Thursday~;Friday~;Saturday~;Sunday~] ~
                 the ~d~[st~;nd~;rd~:;th~] ~
                 of ~[January~;February~;March~;April~;May~;June~;July~;August~;September~;October~;November~;December~] ~
                 ~d, ~
                 ~2,'0d:~2,'0d:~2,'0d"
            day dd (1- (mod dd 10)) (1- mm) yy h m s)))

(define-slot (main button-pressed) ()
  (declare (connected button (released)))
  (q+:qmessagebox-information
   main "Hello World!"
   (format NIL "Hello, dear sir/madam.

You are running ~a v~a on ~a.
It is now ~a."

           (lisp-implementation-type)
           (lisp-implementation-version)
           (machine-type)
           (timestamp))))

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defun point (x y)
  (make-instance 'point :x x :y y))

(defclass rectangle ()
  ((w :initarg :w :initform 0 :accessor w)
   (h :initarg :h :initform 0 :accessor h)))

(defclass positioned-rectangle (point rectangle)
  ((top :initarg :top :initform 0 :accessor top)
   (left :initarg :left :initform 0 :accessor left)))

(defgeneric paint (thing painter)
  (:method ((point point) painter)
    (q+:draw-point painter (x point) (y point)))
  (:method ((rect rectangle) painter)
    (q+:draw-rect painter 0 0 (w rect) (h rect)))
  (:method ((rect positioned-rectangle) painter)
    (q+:draw-rect painter (- (x rect) (left rect)) (- (y rect) (top rect)) (w rect) (h rect))))

(defgeneric visible (thing qrect)
  (:method ((point point) qrect)
    (and (<= (q+:left qrect) (x point) (q+:right qrect))
         (<= (q+:top qrect) (y point) (q+:bottom qrect))))
  (:method ((rect positioned-rectangle) qrect)
    (and (or (<= (q+:left qrect) (- (x rect) (left rect)) (q+:right qrect))
             (<= (q+:left qrect) (+ (- (x rect) (left rect)) (w rect)) (q+:right qrect)))
         (or (<= (q+:top qrect) (- (y rect) (top rect)) (q+:bottom qrect))
             (<= (q+:top qrect) (+ (- (y rect) (top rect)) (h rect)) (q+:bottom qrect)))))
  (:method ((rect positioned-rectangle) (point point))
    (and (<= (- (x rect) (left rect)) (x point) (+ (- (x rect) (left rect)) (w rect)))
         (<= (- (y rect) (top rect)) (y point) (+ (- (y rect) (top rect)) (h rect))))))

(defgeneric make-args (thing)
  (:method ((point point))
    `(:x ,(x point)
      :y ,(y point)
      ,@(when (next-method-p)
          (call-next-method))))
  (:method ((rect rectangle))
    `(:w ,(w rect)
      :h ,(h rect)
      ,@(when (next-method-p)
          (call-next-method))))
  (:method ((rect positioned-rectangle))
    `(:top ,(top rect)
      :left ,(left rect)
      ,@(when (next-method-p)
(call-next-method)))))

(defun main ()
  (with-main-window (window (make-instance 'main))))












(defun generate-window ()
  (with-main-window (window (make-instance 'editor))))

(defun display-window ()
  nil)

(defun vanish-window ()
  nil)
