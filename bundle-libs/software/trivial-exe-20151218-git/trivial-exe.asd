(defsystem trivial-exe
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/ceramic/trivial-exe"
  :bug-tracker "https://github.com/ceramic/trivial-exe/issues"
  :source-control (:git "git@github.com:ceramic/trivial-exe.git")
  :depends-on (:uiop
               #-(or win32 mswindows)
               :osicat)
  :components ((:module "src"
                :serial t
                :components
                ((:file "trivial-exe"))))
  :description "Tools for working with executables"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op trivial-exe-test))))
