(in-package :cl-user)
(defpackage photon.install
  (:use :cl)
  (:export :install-ontology
	   :installed-directory-name))
(in-package :photon.install)


(defparameter *sample-ontology-url* "https://raw.githubusercontent.com/dbym4820/ontologies/master/anime-ontology/anime-ontology.xml")

(defun parse-repository-name (repository-name)
  (split-sequence:split-sequence "/" repository-name :test #'string=))

(defun encoding-url (input)
  (let ((octet-list (loop for x in (map 'list #'identity (babel:string-to-octets input))
			  collect x)))
    (format nil "~{~A~}"
	    (mapcar #'(lambda (octet)
			(format nil "%~x" octet))
		    octet-list))))

(defun github-repository-name-p (repository-name)
  t)

(defun build-github-gitcontent-url (repository-name &key (branch "master") (file-type "xml"))
  (if (github-repository-name-p repository-name)
      (let* ((parsed-repository-name (mapcar #'encoding-url (parse-repository-name repository-name)))
	     (github-user-name (first parsed-repository-name))
	     (repository (second parsed-repository-name))
	     (rest-name (format nil "~{~A~^/~}"
				(reverse (cdr (reverse (cddr parsed-repository-name))))))
	     (file-name (format nil "~A.~A" (car (last parsed-repository-name)) file-type)))
	(format nil "https://raw.githubusercontent.com/~A/~A/~A/~A/~A"
		github-user-name
		repository
		branch
		rest-name
		file-name))
      ""))

(defun installed-directory-name (relative-directory &key (file-type "xml"))
  (format nil "~A~A.~A" +photon-user-ontology-directory+ relative-directory file-type))


(defun expand-ontology-repository-name (repository-name &key (branch "master") (file-type "xml"))
  "expand-ontology-repository-name \"dbym4820/ontologies/anime-ontology/anime-ontology\"/"
  (build-github-gitcontent-url repository-name :branch branch :file-type file-type))


(defun download-ontology (ontology-repository-name &key (branch "master") (file-type "xml"))
  (multiple-value-bind (return-body status)
      (handler-case (dex:get (expand-ontology-repository-name ontology-repository-name :branch branch :file-type file-type))
	(usocket:ns-host-not-found-error ()
	  (values "" 404))
	(dexador.error:http-request-not-found ()
	  (values "" 404))
	(dexador.error:http-request-internal-server-error ()
	  (values "" 505)))
    (case status
      (404
       (values "" (format nil "ontology-not-found") nil))
      (505
       (values "" (format nil "internal-server-error") nil))
      (otherwise
       (values return-body "" t)))))

(defun make-install-directories (repository-name)
  (ensure-directories-exist
   (pathname
    (format nil "~A/~{~A/~}"
	    photon.init::+photon-user-ontology-directory+
	    (reverse
	     (cdr
	      (reverse
	       (parse-repository-name repository-name))))))))
   
(defun install-ontology (ontology-repository-name &key (branch "master") (file-type "xml"))
  (multiple-value-bind (data status success-p)
      (download-ontology ontology-repository-name :branch branch :file-type file-type)
    (if success-p
	(progn
	  (let ((new-ontology-path
		  (format nil "~A/~A.~A"
			  photon.init::+photon-user-ontology-directory+
			  ontology-repository-name
			  file-type)))
	    (make-install-directories ontology-repository-name)
	    (with-open-file (out new-ontology-path
				 :direction :output
				 :if-exists :supersede
				 :if-does-not-exist :create)
	    (write-line data out))
	    new-ontology-path))
	status)))

