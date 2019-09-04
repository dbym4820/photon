#|
  This file is a part of clack-static-asset-middleware project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-static-asset-djula-helpers-asd
  (:use :cl :asdf))
(in-package :clack-static-asset-djula-helpers-asd)

(defsystem clack-static-asset-djula-helpers
  :version "1.0"
  :author "Matt Novenstern"
  :license "MIT"
  :depends-on (:clack-static-asset-middleware
               :djula)
  :components ((:module "src"
                :components
                ((:file "djula"))))
  :description "Djula helpers for `clack-static-asset-middleware`"
  :in-order-to ((test-op (test-op clack-static-asset-middleware-test))))
