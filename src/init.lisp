(in-package :cl-user)
(defpackage photon.init
  (:use :cl)
  (:import-from :uiop
		:directory-exists-p)
  (:export :init))
(in-package :photon.init)

(defun init ()
  (unless (directory-exists-p "~/.photon")
    (make-dirs '("~/.photon/"))))

(defun make-dirs (&rest pathnames)
  (let ((path (car pathnames))
        (path-rest (cdr pathnames)))
    (cond ((null path) t)
            (t
                (ensure-directories-exist path)
                   (eval `(make-dirs ,@path-rest))))))
