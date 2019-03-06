(in-package :cl-user)
(defpackage photon
  (:use :cl)
  (:import-from :photon.init
		:photon-env-init
		:get-ontology
		:set-ontology
		:list-ontology)
  (:import-from :photon.hozo
		:convert-ontology-hozo)
  (:import-from :photon.owl
		:convert-ontology-owl)
  (:import-from :photon.rdf
		:convert-ontology-rdf)
  (:import-from :photon.ontology
		;; params
  		:*default-ontology*
                :*default-ontology-file*
		;; concept class params
                :concept-name
		:class-restriction
		:property-list
	        :child-concept-list
		:instantiation
		:val
                :parent-concept
		;; ontology api
                :make-ontology
		:make-concept
                :add-concept
		:append-concept
                :clear-ontology
		;; concept api
                :show-concepts
		:show-all-class-concept
                :show-all-instance
		:find-concept
                :find-attribute
		:show-attribute
                :get-restricted-concepts
		;; predicates
		:same-concept-p
		:ancestor-p
                :parent-p
		;; other functions
                :ancestor-list
                :get-concept-type		
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
           :get-restricted-concepts

           :concept-name
	   :property-list
	   :child-concept-list
	   :instantiation
	   :val
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

	   :get-concept-type

	   :init
	   :get-ontology
	   :set-ontology
	   :list-ontology
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


(defun init (&key (ontology-type :hozo) (file-path *default-ontology-file*) (ont *default-ontology*) (update t))
  (init)
  (convert-ontology :ontology-type ontology-type :file-path file-path :ont ont :update update)
  (format t "Initialize completed!"))
  
