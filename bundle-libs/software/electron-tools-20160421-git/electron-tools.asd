(defsystem electron-tools
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/ceramic/electron-tools"
  :bug-tracker "https://github.com/ceramic/electron-tools/issues"
  :source-control (:git "git@github.com:ceramic/electron-tools.git")
  :depends-on (:trivial-download
               :trivial-extract
               #-(or win32 mswindows)
               :osicat
               :trivial-exe
               :uiop)
  :components ((:module "src"
                :serial t
                :components
                ((:file "electron-tools"))))
  :description "Download, extract, and run Electron binaries."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op electron-tools-test))))
