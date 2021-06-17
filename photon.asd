(in-package :cl-user)
(defpackage photon-asd
  (:use :cl :asdf))
(in-package :photon-asd)

(defsystem photon
  :version "1.0.0"
  :author "Tomoki ABURATANI"
  :license "MIT"
  ;; :email "aburatanitomoki@gmail.com"
  :homepage "https://github.com/dbym4820/photon"
  :depends-on (:ceramic
	       :clack
	       :clack-static-asset-middleware
	       :ningle
	       :uiop
               :cl-fad
	       :alexandria
	       :dexador
	       :split-sequence
	       :cl-project
	       :usocket
	       :jonathan
	       :xmls)
  :components ((:module "src"
                :components
		((:file "photon" :depends-on ("init" install ontology system-module launcher viewer gui))
		 (:file "init")
		 (:module "install"
		  :components
		  ((:file "install")))
		 (:module "system-module"
		  :components
		  ((:file "module")))
		 (:module "gui"
		  :components
		  ((:file "gui" :depends-on ("server"))
		   (:file "server")))
		 (:module "ontology"
		  :components
                  ((:file "ontology")
		   (:file "hozo" :depends-on ("ontology"))
		   (:file "owl" :depends-on ("ontology"))
		   (:file "rdf" :depends-on ("ontology"))))
		 (:module "launcher"
		  :components
		  ((:file "launcher")))
		 (:module "viewer"
		  :components
		  ((:file "viewer"))))))
  :description "Ontology Based System Extenstion Framework"
  :in-order-to ((test-op (test-op photon-test))))
