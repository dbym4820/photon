(in-package :cl-user)
(defpackage trivial-extract-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :trivial-extract-test)

(def-suite extract)
(in-suite extract)

(defparameter +tmp+
  (asdf:system-relative-pathname :trivial-extract #p"t/tmp/"))

(defparameter +tar-file+
  (asdf:system-relative-pathname :trivial-extract #p"t/file.tar"))

(defparameter +gzip-file+
  (asdf:system-relative-pathname :trivial-extract #p"t/file.tar.gz"))

(defparameter +zip-file+
  (asdf:system-relative-pathname :trivial-extract #p"t/file.zip"))

(defun test-extract (extractor file)
  (let ((copy (merge-pathnames #p"tmp.file" +tmp+)))
    (finishes
      (ensure-directories-exist +tmp+))
    (finishes
      (uiop:copy-file file copy))
    (is-true
     (funcall extractor copy))
    (dolist (extracted (list #p"dir/file.txt"
                             #p"dir/subdir/file.txt"))
      (is-true
       (probe-file (merge-pathnames extracted +tmp+))))
    (finishes
      (uiop:delete-directory-tree +tmp+ :validate t))))

(test .tar
  (dolist (f (list #'trivial-extract:extract-tar
                   #'trivial-extract.cl:extract-tar))
    (test-extract f +tar-file+)))

(test .tar.gz
  (dolist (f (list #'trivial-extract:extract-gzip
                   #'trivial-extract.cl:extract-gzip))
    (test-extract f +gzip-file+)))

(test .zip
  (dolist (f (list #'trivial-extract:extract-zip
                   #'trivial-extract.cl:extract-zip))
    (test-extract f +zip-file+)))

(defun run-tests ()
  (run! 'extract))
