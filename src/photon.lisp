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
                :make-ontology
		:make-concept
                :add-concept
		:append-concept
                :clear-ontology
		:show-concepts
		:describe-all-info
                :find-concept
		:find-attribute
                :show-attribute
		:update-parent-child-concept)
  (:export :convert-ontology
           :set-default-ontology
	   :concept-name
	   :make-concept
	   :make-ontology
	   :add-concept
	   :show-concepts
           :describe-all-info
	   :find-concept
           :append-concept
           :clear-ontology
           :find-attribute
           :show-attribute
	   :concept-inherit-p))
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

