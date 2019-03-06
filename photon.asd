(in-package :cl-user)
(defpackage photon-asd
  (:use :cl :asdf))
(in-package :photon-asd)

(defsystem photon
  :version "0.2.0"
  :author "Tomoki ABURATANI"
  :license "MIT"
  :homepage "https://github.com/dbym4820/photon"
  :depends-on (:uiop
               :cl-fad
	       :cl-ppcre
	       :alexandria
               :local-time
               :bordeaux-threads
	       :dexador
	       :trivial-shell
	       :split-sequence
	       :cl-project
	       :jonathan
	       :xmls)
  :components ((:module "src"
                :components
                ((:file "ontology")
		 (:file "hozo" :depends-on ("ontology"))
		 (:file "owl" :depends-on ("ontology"))
		 (:file "rdf" :depends-on ("ontology"))
		 (:file "module")
		 (:file "init")
		 (:file "photon" :depends-on ("ontology" "hozo" "owl" "rdf" "module")))))
  :description "Ontology Based System Extenstion Framework"
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
