(in-package :cl-user)
(defpackage photon.module
  (:use :cl)
  (:export :defphoton-module))
(in-package :photon.module)

(defmacro defphoton-module (module-name (&rest args) &body body)
  `(let ((*package* (find-package "photon")))
     (defun ,module-name (,@args)
       ,@body)))
