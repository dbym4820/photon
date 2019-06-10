(in-package :cl-user)
(defpackage trivial-extract
  (:use :cl)
  (:import-from :alexandria
                :if-let)
  (:export :extract-tar
           :extract-gzip
           :extract-zip
           :extract))
(in-package :trivial-extract)

(defun extract-tar (pathname)
  "Extract a tarball to its containing directory."
  (if-let (tar (which:which "tar"))
    (trivial-extract.native:extract-tar tar pathname)
    (trivial-extract.cl:extract-tar pathname)))

(defun extract-gzip (pathname)
  "Extract a .tar.gz file to its containing directory."
  (if-let (tar (which:which "tar"))
    (trivial-extract.native:extract-gzip tar pathname)
    (trivial-extract.cl:extract-gzip pathname)))

(defun extract-zip (pathname)
  "Extract a .zip file to its containing directory."
  (if-let (unzip (which:which "unzip"))
    (trivial-extract.native:extract-zip unzip pathname)
    (trivial-extract.cl:extract-zip pathname)))

(defmacro try-except (try on-failure)
  `(handler-case
       ,try
       (t () ,on-failure)))

(defun extract (pathname)
  "Best-effort extraction."
  (try-except
   (extract-gzip pathname)
   (try-except
    (extract-tar pathname)
    (extract-zip pathname))))
