(in-package :cl-user)
(defpackage trivial-extract-test-asd
  (:use :cl :asdf))
(in-package :trivial-extract-test-asd)

(defsystem trivial-extract-test
  :author "Fernando Borretti"
  :license "MIT"
  :description "Tests for trivial-extract"
  :depends-on (:trivial-extract
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "trivial-extract")))))
