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

(defun make-concept-from-string (arg)
  "functions for generate json"
  (if (photon.ontology:find-concept arg)
      (let* ((concept-info (photon.ontology:find-concept arg))
  	     (concept-name (photon.ontology:concept-name concept-info))
  	     (concept-id (photon.ontology:concept-id concept-info))
  	     (properties (mapcar #'(lambda (c)
				(cons (photon.ontology:concept-name c)
				      (photon.ontology:class-restriction c)))
				 (remove-if #'null
					    (photon.ontology:property-list concept-info))))
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
  		 :from :alist)))
      (format nil "{ ~A }" (photon.ontology:find-concept arg))))

  
(defun make-concept-from-id (arg)
  (if (photon.ontology:find-concept-from-id arg)
      (let* ((concept-info (photon.ontology:find-concept-from-id arg))
	     (concept-name (photon.ontology:concept-name concept-info))
	     (concept-id (photon.ontology:concept-id concept-info))
	     (properties (mapcar #'(lambda (c)

				(cons (photon.ontology:concept-name c)
				      (photon.ontology:class-restriction c)))
				 (remove-if #'null
					    (photon.ontology:property-list concept-info))))
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
		 :from :alist)))
      "{}"))

;;; server main
(defparameter *photon-server* nil)
(defparameter *default-photon-server-port* 5050)
(defparameter *photon-server-app* (make-instance 'ningle:<app>))
(defun start-photon-server (&optional port)
  (setf *photon-server*
	(clack:clackup *photon-server-app* :port
		       (or port *default-photon-server-port*))))
(defun stop-photon-server ()
  (clack:stop *photon-server*))
  
(defmacro defphoton-route (name (params &rest route-args) &body body)
  `(setf (ningle:route *photon-server-app* ,name ,@route-args)
         #'(lambda (,params)
	     (declare (ignorable ,params))
	     (eval ,@body))))

(defphoton-route "/" (param)
  (to-json '() :from :alist))


(defphoton-route "/all-concept" (param)
  (format nil "~A"
	  (to-json (remove "whole-root" (photon.ontology:show-concepts) :test #'string=) :from :alist)))

(defphoton-route "/concept-name/:concept" (param)
  (format nil "~A"
	  (handler-case
	      (make-concept-from-string
	       (cdr (assoc 'concept param :test #'string=)))
	    (error (e)
	      (declare (ignore e))
	      (to-json '() :from :alist)))))

(defphoton-route "/concept-id/:concept" (param)
  (format nil "~A"
	  (handler-case
	      (make-concept-from-id
	       (cdr (assoc 'concept param :test #'string=)))
	    (error (e)
	      (declare (ignore e))
	      (to-json '() :from :alist)))))
