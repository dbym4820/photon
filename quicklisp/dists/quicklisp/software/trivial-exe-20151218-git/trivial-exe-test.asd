(defsystem trivial-exe-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:trivial-exe
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "trivial-exe")))))
