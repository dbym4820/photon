(in-package :cl-user)
(defpackage photon.owl
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
  (:export :convert-ontology-owl))
(in-package :photon.owl)

#|
OWLオントロジーのコンバート処理
|#
(defun convert-ontology-owl ())

