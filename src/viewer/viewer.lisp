(in-package :cl-user)
(defpackage photon.viewer
  (:use :cl)
  (:import-from :photon.init
		:get-result
		:get-config
		:get-env
		:get-help
                :get-ontology-details
		)
  (:export :photon-viewer))
(in-package :photon.viewer)

(defun photon-viewer (target &optional optional-concept)
  (case target
    ;; about result
    (:|result/concept-list|
     (get-result "concept-list"))
    (:|current/ontology|
     (get-config "main-ontology"))
    (:|ontology/details|
     (get-ontology-details optional-concept))
    ;; about help
    (:|help/system|
     (get-help "system-help"))
    (:|help/install|
     (get-help "install"))
    (:|help/details|
     (get-help "details"))
    ;; about config
    (:|config/version|
     (get-config "version"))
    (otherwise
     (format nil "no result"))))
