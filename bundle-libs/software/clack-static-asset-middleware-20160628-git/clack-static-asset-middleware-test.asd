#|
  This file is a part of clack-static-asset-middleware project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-static-asset-middleware-test-asd
  (:use :cl :asdf))
(in-package :clack-static-asset-middleware-test-asd)

(defsystem clack-static-asset-middleware-test
  :author "Matt Novenstern"
  :license "MIT"
  :depends-on (:clack-static-asset-middleware
               :clack-static-asset-djula-helpers
               :prove
               :lack-test)
  :components ((:module "t"
                :components
                ((:test-file "clack-static-asset-middleware"))))
  :description "Test system for clack-static-asset-middleware"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
