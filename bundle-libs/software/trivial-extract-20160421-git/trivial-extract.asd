(defsystem trivial-extract
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/eudoxia0/trivial-extract"
  :bug-tracker "https://github.com/eudoxia0/trivial-extract/issues"
  :source-control (:git "git@github.com:eudoxia0/trivial-extract.git")
  :depends-on (:archive
               :zip
               :deflate
               :which
               ;; Utilities
               :cl-fad
               :uiop
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "cl")
                 (:file "native")
                 (:file "trivial-extract"))))
  :description "Extract .tar/.tar.gz/.zip files."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op trivial-extract-test))))
