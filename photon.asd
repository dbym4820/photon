(in-package :cl-user)
(defpackage photon-asd
  (:use :cl :asdf))
(in-package :photon-asd)

(defsystem photon
  :version "0.1"
  :author "Tomoki ABURATANI"
  :license "MIT"
  :homepage "https://github.com/dbym4820/photon"
  :depends-on (:qtools
	       :qtcore
               :qtgui
               :uiop
               :cl-fad
	       :cl-ppcre
	       :alexandria
               :local-time
               :ningle
               :bordeaux-threads
               :cffi
	       :dexador
	       :trivial-shell
	       :jonathan)
  :serial t
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "window")
                 (:file "concept")
                 (:file "init")
                 (:file "photon"))))
  :build-operation "qt-program-op"
  :build-pathname "photon"
  :entry-point "photon:start-photon"
  :description "Concept Level Programing Environment"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op photon-test))))
