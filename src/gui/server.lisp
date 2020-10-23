(in-package :cl-user)
(defpackage photon.server
  (:use :cl)
  (:shadow :photon.ontology)
  (:import-from :jonathan
		:to-json)
  (:export
   :defphoton-route
   :start-photon-server
   :stop-photon-server
   ))
(in-package :photon.server)

;;; functions for generate json
(defun find-concept-from-string (arg)
  (let* ((concept-info (photon.ontology:find-concept arg))
  	 (concept-name (photon.ontology:concept-name concept-info))
  	 (concept-id (photon.ontology:concept-id concept-info))
  	 (properties (mapcar #'photon.ontology:concept-name (remove-if #'null (photon.ontology:property-list concept-info))))
  	 (is-instance (if (photon.ontology:instantiation concept-info) "true" "false"))
  	 (parent-concept (photon.ontology:concept-name (photon.ontology:parent-concept concept-info)))
  	 (child-concepts (mapcar #'photon.ontology:concept-name (photon.ontology:child-concept-list concept-info))))
    (format nil "~A"
  	    (to-json
  	     `(("id" . ,concept-id)
  	       ("concept-name" . ,concept-name)
  	       ("is-instance" . ,is-instance)
  	       ("properties" . ,properties)
  	       ("parent-concept" . ,parent-concept)
  	       ("child-concepts" . ,child-concepts))
  	     :from :alist))))

  
(defun find-concept-from-id (arg)
  (let* ((concept-info (photon.ontology:find-concept-from-id arg))
	 (concept-name (photon.ontology:concept-name concept-info))
	 (concept-id (photon.ontology:concept-id concept-info))
	 (properties (mapcar #'photon.ontology:concept-name (remove-if #'null (photon.ontology:property-list concept-info))))
	 (is-instance (if (photon.ontology:instantiation concept-info) "true" "false"))
	 (parent-concept (photon.ontology:concept-name (photon.ontology:parent-concept concept-info)))
	 (child-concepts (mapcar #'photon.ontology:concept-name (photon.ontology:child-concept-list concept-info))))
    (format nil "~A"
	    (to-json
	     `(("id" . ,concept-id)
	       ("concept-name" . ,concept-name)
	       ("is-instance" . ,is-instance)
	       ("properties" . ,properties)
	       ("parent-concept" . ,parent-concept)
	       ("child-concepts" . ,child-concepts))
	     :from :alist))))

;;; server main
(defparameter *photon-server* nil)
(defparameter *photon-server-app* (make-instance 'ningle:<app>))
(defun start-photon-server ()
  (setf *photon-server*
	(clack:clackup *photon-server-app* :port 5050)))
(defun stop-photon-server ()
  (clack:stop *photon-server*))
  
(defmacro defphoton-route (name (params &rest route-args) &body body)
  `(setf (ningle:route *photon-server-app* ,name ,@route-args)
         #'(lambda (,params)
	     (declare (ignorable ,params))
	     (eval ,@body))))

(defphoton-route "/" (param)
  (format nil "{}"))

(defphoton-route "/concept-name/:concept" (param)
  (format nil "~A"
	  (handler-case
	      (find-concept-from-string
	       (cdr (assoc 'concept param :test #'string=)))
	    (error (e)
	      (declare (ignore e))
	      "{}"))))

(defphoton-route "/concept-id/:concept" (param)
  (format nil "~A"
	  (handler-case
	      (find-concept-from-id
	       (cdr (assoc 'concept param :test #'string=)))
	    (error (e)
	      (declare (ignore e))
	      "{}"))))
