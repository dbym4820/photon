;;;; package.lisp
;;;; Copyright (c) 2013 Robert Smith

(defpackage #:map-set
  (:use #:cl)
  (:export #:map-set
           #:make-map-set
           #:map-set-p
           
           #:ms-count
           #:ms-member-p
           #:ms-insert
           #:ms-delete
           #:ms-random
           #:ms-map
           #:ms-for-each))

