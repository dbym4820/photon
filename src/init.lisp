(in-package :cl-user)
(defpackage photon.init
  (:use :cl)
  (:import-from :alexandria
                :read-file-into-string)
  (:import-from :uiop
                :directory-exists-p
		:file-exists-p
		:delete-file-if-exists)
  (:import-from :cl-fad
		:copy-file
	        :list-directory
	        :walk-directory
	        :delete-directory-and-files
		:directory-pathname-p
		:merge-pathnames-as-directory
                :merge-pathnames-as-file)
  (:import-from :cl-ppcre
                :regex-replace-all)
  (:export :photon-env-init
	   :get-ontology
	   :get-env
	   :get-config
	   :get-help
	   :get-result

	   :set-config
	   :set-env
	   :set-result
	   :set-ontology
	   :list-ontology
           :find-ontology-path
	   :delete-photon-directory))
(in-package :photon.init)

#| 
Constant valiables
|#
(defvar +user-home-dirname+
  (namestring
   (user-homedir-pathname)))
(defvar +photon-user-directory+
  (concatenate 'string
	       +user-home-dirname+
	       ".photon/"))
(defvar +photon-user-ontology-directory+
  (concatenate 'string
	       +user-home-dirname+
	       ".photon/ontology/"))
(defvar +photon-project-env-directory+
  (namestring (directory-namestring (asdf:system-relative-pathname "photon" "src/env/"))))
(defvar +photon-project-ontology-directory+
  (namestring (directory-namestring (asdf:system-relative-pathname "photon" "src/ontology/ontology/"))))

#|
Directory utilities
|#
(defun escape-wild-string (str)
  "escape wild-string like '[~~]' and '?' as '\\[~~\\]' and '\\?'"
  (regex-replace-all
   "([\\?\\[\\]\\*\\&\\%\\$\\^\\(\\)\\_])"
   str
   "\\\\\\1"
   :preserve-case t)
  )

(defun check-directory-duplicate-p (directory-pathname)
  "return t when target directory has been existed"
  (directory-exists-p directory-pathname))

(defun make-directory (new-directory-pathname)
  "make directory when target directory has not created yet"
  (unless (check-directory-duplicate-p new-directory-pathname)
    (ensure-directories-exist new-directory-pathname)))

(defun make-directories (&rest pathnames)
  "make multi-directories"
  (loop for path in pathnames
	unless (check-directory-duplicate-p path)
	  do (make-directory path)))

(defun copy-directory (from to)
  "copy directory"
  (flet ((rel-path (parent child)
           (subseq (namestring child)
                   (length (namestring parent)))))
    (walk-directory from
		    (lambda (child)
		      (if (directory-pathname-p child)
			  (ensure-directories-exist
			   (merge-pathnames-as-directory to (rel-path from child)))
			  (copy-file child
				     (merge-pathnames-as-file
				      to
				      (rel-path from child))
				     :overwrite t)))
		    :directories :breadth-first)))

#|
Attribute utilities
|#
;; (get-attribute "user/user-name") => "tomoki"
;; (get-attribute "user/user-name") => "tomoki"
(defun get-attribute (attribute-name)
  (let ((f-name
	  (escape-wild-string
	   (concatenate 'string
			+photon-user-directory+
			attribute-name))))
    (if (file-exists-p f-name)
	(read-file-into-string f-name)
	"")))

;; (set-attribute "user/user-name" "tomoki")
;; (set-attribute "user/user-name" "tomoki")
(defun set-attribute (attribute-name attribute-content)
  (let ((f-name
	  (escape-wild-string
	   (concatenate 'string
			+photon-user-directory+
			attribute-name))))
    (with-open-file (file-var f-name :direction :output
				     :if-exists :supersede
				     :if-does-not-exist :create)
      (write-string
       attribute-content file-var))))

#|
Help utilities
|#
(defmacro defgetter (getter-name-symbol attribute-type-name)
  `(let* ((package *package*)
	  (*package* (find-package :photon.init)))
     (defgeneric ,getter-name-symbol (command-name)
       (:method ((command-name string))
	 (get-attribute
	  (concatenate 'string
		       ,attribute-type-name
		       "/"
		       command-name))))
     (setf *package* package)))

(defmacro defsetter (getter-name-symbol attribute-type-name)
  `(let* ((package *package*)
	  (*package* (find-package :photon.init)))
     (defgeneric ,getter-name-symbol (command-name attribute-value)
       (:method ((command-name string) (attribute-value string))
	 (let ((d-name (concatenate 'string
				    +photon-user-directory+
				    ,attribute-type-name "/")))
	   (make-directory d-name))
	 (set-attribute
	  (concatenate 'string
		       ,attribute-type-name
		       "/"
		       command-name)
	  attribute-value)))
     (setf *package* package)))

#|
Initialization
|#

(defgetter get-help "help")
(defgetter get-config "config")
(defgetter get-env "env")
(defgetter get-result "tmp-result")
(defgetter get-ontology "ontology")
(defgetter get-ontology-details "ontology-details")

(defsetter set-env "env")
(defsetter set-config "config")
(defsetter set-result "tmp-result")
(defsetter set-ontology-details "ontology-details")

(defun set-ontology (ontology-file-path-string &key other-name overwrite without-directory)
  "this setter is special one amoung photon.init"
  (cond (without-directory
	    (copy-file (truename ontology-file-path-string)
		       (concatenate 'string
				    +photon-user-ontology-directory+
				    (if other-name
					other-name
					(concatenate 'string
						     (pathname-name ontology-file-path-string)
						     "."
						     (pathname-type ontology-file-path-string))))
		       :overwrite overwrite))
	((null without-directory)
	    (copy-file (truename ontology-file-path-string)
		       (concatenate 'string
				    +photon-user-ontology-directory+
				    (if other-name
					other-name
					(concatenate 'string
						     (reduce #'(lambda (s1 s2)
								 (concatenate 'string s1 "/" s2))
							     (cdr (pathname-directory (truename ontology-file-path-string))))
						     "/"
						     (pathname-name ontology-file-path-string)
						     "."
						     (pathname-type ontology-file-path-string))))
		       :overwrite overwrite)))
  (format t "Ontology ~A has registered successfully!~%"
	  (concatenate 'string
		       (pathname-name ontology-file-path-string)
		       "."
		       (pathname-type ontology-file-path-string))))

(defun list-ontology ()
  (let ((ontology-list nil))
    (walk-directory +photon-user-ontology-directory+
		    #'(lambda (file)
			(let* ((f-truename (namestring file))
			       (f-name (pathname-name f-truename))
			       ;; (f-type (pathname-type f-truename))
			       ;; (f (concatenate 'string f-truename "/" f-name "." f-type))
			       )
			  (push (list f-name f-truename) ontology-list))))
    ontology-list))

(defun find-ontology-path (ontology-name)
  (second (assoc ontology-name (list-ontology) :test #'string=)))

(defun delete-photon-directory ()
  (delete-directory-and-files +photon-user-directory+))

(defun photon-env-init ()
  (format t "Initialize photon home directory: ~A ...~%" +photon-user-directory+)
  (make-directory +photon-user-directory+)
  (make-directory +photon-user-ontology-directory+)
  (copy-directory
   +photon-project-env-directory+
   +photon-user-directory+)
  (set-ontology
   (concatenate 'string
		+photon-project-ontology-directory+
		"sample-ontology.xml")
   :overwrite t)
  (format t "Following ontology has registered as default... ~%~{~t~t * ~@A~^~%~}~%"
	  (list-ontology)))
