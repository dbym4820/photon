#|
  This file is a part of clack-static-asset-middleware project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

#|
  Author: Matt Novenstern (fisxoj@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-static-asset-middleware-asd
  (:use :cl :asdf))
(in-package :clack-static-asset-middleware-asd)

(defsystem clack-static-asset-middleware
  :version "1.1"
  :author "Matt Novenstern"
  :license "MIT"
  :depends-on (:alexandria
               :uiop
               :ironclad
               :cl-ppcre
               :local-time
               :trivial-mimes)
  :components ((:module "src"
                :components
                ((:file "clack-static-asset-middleware"))))
  :description "A cache busting static file middleware for the clack web framework."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op clack-static-asset-middleware-test))))
