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
		:find-ontology-path
		:delete-photon-directory
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
		:concept-id
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
                :find-concept-from-id
                :find-attribute
		:show-attribute
                :get-restricted-concepts
                :get-part-concepts-class-restriction
                :get-part-concepts-role-name
                :get-part-concepts-role-and-restriction
                :get-part-concept-info
                :get-single-restricted-concepts
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
  (:import-from :photon.server
		:defphoton-route)
  (:import-from :photon.viewer
		:photon-viewer)
  (:import-from :photon.gui
		:launch-gui)
  (:export :convert-ontology
           :set-default-ontology
	   :make-concept
	   :make-ontology
	   :add-concept

           :show-concepts
	   :show-all-class-concept
	   :show-all-instance
           :find-concept
	   :find-concept-from-id
	   :find-attribute
           :show-attribute
           :get-restricted-concepts

	   :concept-id
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

	   :get-part-concepts-class-restriction
           :get-part-concepts-role-name
           :get-part-concepts-role-and-restriction
           :get-part-concept-info
           :get-single-restricted-concepts
           :get-restricted-concepts
	   :get-concept-from-id
	   :get-concept-type

	   :init
           :get-help
	   :get-env
	   :get-config
	   :get-ontology

	   :list-ontology
           :find-ontology-path
	   :delete-photon-directory
	   
	   :install
	   :switch-main-ontology

	   :launch-gui
	   :defphoton-route

           :set-local-ontology
   
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
     (format t "~%Convert as Hozo formed ontology~%")
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
  (set-config "main-ontology"
	      (format nil "~A~A" +photon-user-ontology-directory+ "sample-ontology.xml"))
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
	(convert-ontology :update t)
      (get-all-converted-ontology-details))))


(defun set-local-ontology (ontology-pathname &optional other-name)
  (set-ontology ontology-pathname :other-name other-name)
  (switch-main-ontology (caar (last (photon:list-ontology))) (cadar (last (photon:list-ontology)))))


(defun switch-main-ontology (ontology-name &optional file-name)
  (set-config "main-ontology" (if file-name file-name ontology-name))
  (update-main-ontology)
  (prog1
      (if file-name
	  (convert-ontology :file-path file-name :update t)
	  (convert-ontology))
    (get-all-converted-ontology-details)))

(defun viewer (command &optional optional-command)
  (photon-viewer (make-keyword command) optional-command))

(defun run (concept-name)
  (photon-launcher (make-keyword concept-name)))
