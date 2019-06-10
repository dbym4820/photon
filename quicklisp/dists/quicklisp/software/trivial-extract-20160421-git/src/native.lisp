(in-package :cl-user)
(defpackage trivial-extract.native
  (:use :cl)
  (:export :extract-tar
           :extract-gzip
           :extract-zip)
  (:documentation "Decompression using native command line utilities."))
(in-package :trivial-extract.native)

(defun extract-tar (binary pathname)
  "Extract a tarball to its containing directory."
  (uiop:run-program (format nil "~S -xf ~S -C ~S"
                            (namestring binary)
                            (namestring pathname)
                            (namestring (uiop:pathname-directory-pathname pathname))))
  t)

(defun extract-gzip (binary pathname)
  "Extract a .tar.gz file to its containing directory."
  (uiop:run-program (format nil "~S xzf ~S -C ~S"
                            (namestring binary)
                            (namestring pathname)
                            (namestring (uiop:pathname-directory-pathname pathname))))
  t)

(defun extract-zip (binary pathname)
  "Extract a .zip file to its containing directory."
  (uiop:run-program (format nil "~S -o ~S -d ~S"
                              (namestring binary)
                              (namestring pathname)
                              (namestring (uiop:pathname-directory-pathname pathname))))
  t)
