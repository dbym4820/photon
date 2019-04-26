(in-package :cl-user)
(defpackage photon.viewer
  (:use :cl)
  (:import-from :photon.init
		:get-result
		:get-config
		:get-env
		:get-help)
  (:export :photon-viewer))
(in-package :photon.viewer)

(defun photon-viewer (target)
  (case target
    ;; about result
    (:|result/concept-list|
     (get-result "concept-list"))
    ;; about help
    (:|help/system|
     (get-help "system-help"))
    (:|help/install|
     (get-help "install"))
    ;; about config
    (:|config/version|
     (get-config "version"))
    (otherwise
     (format nil "no result"))))
