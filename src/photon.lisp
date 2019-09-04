(in-package :cl-user)
(defpackage photon
  (:use :cl)
  (:import-from :alexandria
		:make-keyword)
  (:import-from :photon.init

		:get-help
		:get-env
		:get-config
		:get-result
		
		:photon-env-init
		:set-config
		:set-env
		:set-result
		:get-ontology
		:set-ontology
                :get-ontology-details
                :set-ontology-details
		:list-ontology

		:+photon-user-ontology-directory+)
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
  (:import-from :photon.install
		:install-ontology
		:installed-directory-name)
  (:import-from :photon.launcher
		:photon-launcher)
  (:import-from :photon.viewer
		:photon-viewer)
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
           :get-help
	   :get-env
	   :get-config
	   :get-ontology
	   :set-ontology
	   :list-ontology
	   
	   :install
	   :switch-main-ontology

	   :viewer

	   :run
	   ))
(in-package :photon)

#|
オントロジーのCLOSコンバート
|#

(defparameter *main-ontology*
  (or (get-config "main-ontology") *default-ontology*))

(defun update-main-ontology ()
  (setf *main-ontology*
	(or
	 (get-config "main-ontology")
	 *default-ontology*)))

(defun convert-ontology (&key (ontology-type :hozo) (file-path *main-ontology*) (ont *default-ontology*) (update t))
  (case ontology-type
    (:hozo
     (format t "Convert as Hozo formed ontology~%")
     (convert-ontology-hozo :file-path file-path :ont ont :update update)
     (set-result "concept-list"
		 (format nil "~{~A~^ ~}" (show-concepts))))
    (:owl
     (format t "Convert as OWL formed ontology~%")
     (convert-ontology-owl))
    (:rdf
     (format t "Convert as RDF/XML formed ontology~%")
     (convert-ontology-rdf))
    (otherwise (convert-ontology-hozo))))

(defun init (&key (ontology-type :hozo) (file-path *default-ontology-file*) (ont *default-ontology*) (update t))
  (photon-env-init)
  (set-config "main-ontology" (format nil "~A~A" +photon-user-ontology-directory+ "sample-ontology"))
  (update-main-ontology)
  (prog1
      (convert-ontology :ontology-type ontology-type
			:file-path file-path
			:ont ont
			:update update)
    (get-all-converted-ontology-details))
  (format t "Initialize completed!~%"))

(defun get-all-converted-ontology-details ()
  (mapcar #'(lambda (concept-object)
	      (set-ontology-details
	       (concept-name concept-object)
	       (show-attribute concept-object)))
	  (mapcar #'find-concept
		  (show-concepts)))
  (format t "Writing ontology details is finished!~%"))

(defun install (ontology-repository-name)
  (progn
    (install-ontology ontology-repository-name)
    (set-config "main-ontology" (installed-directory-name ontology-repository-name))
    (update-main-ontology)
    (format t "Ontology is Downloaded in ~A~%" (get-config "main-ontology"))
    (prog1
	(convert-ontology)
      (get-all-converted-ontology-details))))

(defun switch-main-ontology (ontology-name)
  (set-config "main-ontology" ontology-name)
  (update-main-ontology)
  (prog1
      (convert-ontology)
    (get-all-converted-ontology-details)))

(defun viewer (command &optional optional-command)
  (photon-viewer (make-keyword command) optional-command))

(defun run (concept-name)
  (photon-launcher (make-keyword concept-name)))
