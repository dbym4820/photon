(in-package :cl-user)
(defpackage photon
  (:use :cl)
  (:import-from :photon.hozo
		:convert-ontology-hozo)
  (:import-from :photon.owl
		:convert-ontology-owl)
  (:import-from :photon.rdf
		:convert-ontology-rdf)
  (:import-from :photon.ontology
  		:*default-ontology*
                :*default-ontology-file*

                :concept-name
		:property-list
	        :child-concept-list
		:instantiation
                :parent-concept

                :make-ontology
		:make-concept
                :add-concept
		:append-concept
                :clear-ontology

                :show-concepts
		:show-all-class-concept
                :show-all-instance
		:find-concept
                :find-attribute
		:show-attribute
                :get-restricted-concept

		:same-concept-p
                :ancestor-list
		:ancestor-p
                :parent-p
		
		:update-parent-child-concept)
  (:export :convert-ontology
           :set-default-ontology
	   :make-concept
	   :make-ontology
	   :add-concept

           :show-concepts
	   :show-all-class-concept
	   :show-all-instance
           :find-concept
	   :find-attribute
           :show-attribute
	   :get-restricted-concept

           :concept-name
	   :property-list
	   :child-concept-list
	   :instantiation
	   :parent-concept
           :role-name
	   :class-restriction
	   :role-holder
	   :cardinality
	   :concept-type

           :append-concept
           :clear-ontology

           :same-concept-p
	   :ancestor-list
	   :ancestor-p
	   :parent-p
	   ))
(in-package :photon)

#|
オントロジーのCLOSコンバート
|#
(defun convert-ontology (&key (ontology-type :hozo) (file-path *default-ontology-file*) (ont *default-ontology*) (update t))
  (case ontology-type
    (:hozo (convert-ontology-hozo :file-path file-path :ont ont :update update))
    (:owl (convert-ontology-owl))
    (:rdf (convert-ontology-rdf))
    (otherwise (convert-ontology-hozo))))

(convert-ontology)

