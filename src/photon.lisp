(in-package :cl-user)
(defpackage photon
  (:use :cl)
  (:import-from :photon.hozo
		:convert-ontology)
  (:import-from :photon.ontology
		:concept
                :make-ontology
		:make-concept
                :add-concept
		:append-concept
                :clear-ontology
		:show-concepts
                :find-concept
		:find-attribute
                :show-attribute)
  (:export :convert-ontology
	   :make-concept
	   :make-ontology
	   :add-concept
	   :show-concepts
	   :find-concept))
(in-package :photon)
