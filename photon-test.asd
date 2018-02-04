#|
  This file is a part of photon project.
  Copyright (c) 2017 Tomoki ABURATANI (aburatanitomoki@gmail.com)
|#

(in-package :cl-user)
(defpackage photon-test-asd
  (:use :cl :asdf))
(in-package :photon-test-asd)

(defsystem photon-test
  :author "Tomoki ABURATANI"
  :license "MIT"
  :depends-on (:photon
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "photon"))))
  :description "Test system for photon"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
