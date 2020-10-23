(in-package :cl-user)
(defpackage photon.gui
  (:use :cl)
  (:import-from :alexandria
		:read-file-into-string
		:read-file-into-byte-vector)
  (:import-from :ningle
		:<app>
		:route)
  (:import-from :ceramic.resource
		:define-resources
		:resource-directory)
  (:import-from :ceramic
		:define-entry-point
		:make-window
		:show)
  (:import-from :photon.server
		:start-photon-server
		:stop-photon-server)
  (:export
   :photon-gui-setup
   :launch-gui))
(in-package :photon.gui)

(defparameter *static-file-path*
  (asdf:system-relative-pathname :photon "src/gui/"))

#|
Routing utils
|#
(defun return-static-text-file (file-path-from-static-dir)
  "(return-static-file \"vis/vis.min.js\")"
  (read-file-into-string (format nil "~A~A" *static-file-path* file-path-from-static-dir)))

(defun return-static-vector-file (file-path-from-static-dir)
  "(return-static-vector-file \"img/vis/sample.png\")"
  (read-file-into-byte-vector (format nil "~A~A" *static-file-path* file-path-from-static-dir)))

(defparameter *photon-gui-port* 20000)
(defparameter *current-window* nil)

#|
Ceramic Windowの設定
|#
(ceramic.window::define-trivial-operation setIgnoreMouseEvents "setIgnoreMouseEvents(true)"
  :docstring "make window become untansible"
  :sync t)

(ceramic.window::define-trivial-operation setAwareMouseEvents "setIgnoreMouseEvents(false)"
  :docstring "make window become tansible"
  :sync t)

(defun photon-gui-setup ()
  (ceramic:setup))

(defun start-window ()
  (handler-case (run-server)
    (usocket:address-in-use-error ()
      t))
  (handler-case (ceramic:stop)
    (unbound-slot ()
      t))
  (handler-case (ceramic:start)
    (usocket:address-in-use-error ()
      (ceramic:stop)
      (ceramic:start)))
  (let ((window (make-window :url (format nil "http://localhost:~A/" *photon-gui-port*)
			     :title "trans"
			     :transparent "true"
			     :frame "false")))
    (setf *current-window* window)
    (show window)))

(defparameter *photon-server-app*
  (make-instance '<app>))

(defmacro defroute (name (params &rest route-args) &body body)
  `(setf (ningle:route *photon-server-app* ,name ,@route-args)
         #'(lambda (,params)
	     (declare (ignorable ,params))
	     (eval ,@body))))

(defroute "/" (param)
  "<html><head><link rel='stylesheet' href='/static/css/photon.css' /><script type='text/javascript' src='/static/js/photon.js'></script></head><body><canvas id='myCanvas'></canvas></body></html>")

(defroute "/static/img/:image-name" (params :method :get)
  `(return-static-vector-file ,(format nil "~A~A" "static/img/" (cdr (assoc 'image-name params :test #'string=)))))

(defroute "/static/css/:file-name" (params :method :get)
  `(return-static-text-file ,(format nil "~A~A" "static/css/" (cdr (assoc 'file-name params :test #'string=)))))

(defroute "/static/js/:file-name" (params :method :get)
  `(return-static-text-file ,(format nil "~A~A" "static/js/" (cdr (assoc 'file-name params :test #'string=)))))


(defparameter *photon-gui-server* nil)
  
(defun run-server ()
  (setf *photon-gui-server*
	(clack:clackup *photon-server-app* :port *photon-gui-port*)))

(defun stop-server ()
  (clack:stop *photon-gui-server*))


(defun launch-gui (command)
  (cond ((eql command :start-server)
	;;(start-window)
	 (start-photon-server))
	((eql command :stop-server)
	 (stop-photon-server))
	(t
	 (format nil "no command"))))
