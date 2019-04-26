(in-package :cl-user)
(defpackage photon.rdf
  (:use :cl)
  (:import-from :alexandria
                :read-file-into-string)
  (:import-from :photon.ontology
  		:*default-ontology*
                :*default-ontology-file*
		:make-concept
		:find-concept
                :clear-ontology
		:add-concept
                :append-concept
		:show-concepts
		:update-parent-child-concept)
  (:export :convert-ontology-rdf))
(in-package :photon.rdf)

#|
RDFオントロジーのコンバート処理
|#
(defun convert-ontology-rdf ())

