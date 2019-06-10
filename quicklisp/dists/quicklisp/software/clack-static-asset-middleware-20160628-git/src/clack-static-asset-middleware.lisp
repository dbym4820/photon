(in-package :cl-user)
(defpackage clack-static-asset-middleware
  (:use :cl)
  (:import-from #:alexandria
                #:starts-with-subseq
                #:if-let)

  (:export #:+default-filter-function+
           #:+default-cache-buster-function+
           #:+default-cache-unbuster-function+

           #:*asset-lookup-table*

           #:busted-uri-for-path

           #:*clack-static-asset-middleware*))
(in-package :clack-static-asset-middleware)

(defparameter +default-filter-function+
  (lambda (p) (char= (char (pathname-name p) 0) #\.))
  "Function to filter out files that would be hidden on a unix system.  Namely, anything that starts with a `#\.`.")

(defparameter +default-cache-buster-function+
  (lambda (p cache-string)
    (make-pathname :defaults p
                   :name (format nil "~a_~a" (pathname-name p) cache-string)))

  "Function to make a new pathname with a cache-busting string appended.  The default it to convert `style.css` to `style_[hex-string].css`. You can override this by passing a different function to the middleware.")

(defparameter +default-cache-unbuster-function+
  (lambda (path-string)
    (multiple-value-bind (match-p matches) (ppcre:scan-to-strings "^(.+)_[a-fA-F0-9]{32}(\..+)" path-string)
      (if match-p
          (concatenate 'string (svref matches 0) (svref matches 1))
          ;; If we couldn't recognize this as a cache busted path,
          ;; return the string and hope it's still a path we can resolve.
          path-string)))

  "Function to undo the cache busting.")

(defvar *asset-lookup-table* (make-hash-table :test 'equal)
  "Lookup table for files in the static asset directory.  Maps relative filenames to cache-busted versions,
e.g. 'style/homepage.css => style/homepage_2867f3f83a6a91ad4a19a6cd45536152.css")


(defvar *base-path* "/"
  "Base path of the assets on the server.  Maybe something like \"static/\".")


(defun md5-file (pathname &key buffer digest)
  "Generate a string of hex digits for a given pathname."
  (ironclad:byte-array-to-hex-string (ironclad:digest-file :md5 pathname :buffer buffer :digest digest)))


(defun root-relative-path (pathname root)
  "Produces a relative pathname relative to the root of the asset directory.  The string that might be specified in a template, for example."
  (subseq (uiop:native-namestring pathname) (length (uiop:native-namestring root))))


(defun walk-directory (directory &key (filter-function +default-filter-function+))
  "Collects all the files in `directory` except those excluded by `filter-function`."
  (let (directories)
    (uiop:collect-sub*directories directory t t (lambda (p) (push p directories)))

    (remove-if filter-function (apply #'append (mapcar #'uiop:directory-files directories)))))


(defun inventory-files (pathnames path root
                        &key
                          (filter-function +default-filter-function+)
                          (cache-buster-function +default-cache-buster-function+))
  (loop
     with asset-table = (make-hash-table :test 'equal)
     ;; Reusable buffer/digest as suggested http://method-combination.net/lisp/ironclad/#digest-tips
     with buffer = (make-array 8192 :element-type '(unsigned-byte 8))
     with digest = (make-array (ironclad:digest-length :md5)
                               :element-type '(unsigned-byte 8))
     for p in pathnames
     for root-relative-path = (root-relative-path p root)
     unless (funcall filter-function p)
     do (setf (gethash root-relative-path asset-table)
              (funcall cache-buster-function
                       (concatenate 'string "/" (uiop:native-namestring path) root-relative-path)
                       (md5-file p :buffer buffer :digest digest)))

     finally (return asset-table)))


(defun asset-response (relative-path root)
  "Generate the `clack` respone with an file or a 404.  Expects `relative-path` to be a string with no leading `/`."
  (let ((file (uiop:merge-pathnames* relative-path root)))
    (handler-case
        (with-open-file (stream file :direction :input)
          `(200
            (:content-type ,(mimes:mime file)
                           :content-length ,(file-length stream)
                           :cache-control "public, max-age=31556926"
                           :vary "Accept-Encoding"  ;; https://www.maxcdn.com/blog/accept-encoding-its-vary-important/
                           :last-modified ,(local-time:format-rfc1123-timestring nil (local-time:universal-to-timestamp (file-write-date file))))
            ,file))

      (file-error ()
        '(404
          (:content-type "text/plain"
           :content-length 9)
          ("Not Found"))))))


(defun busted-uri-for-path (path)
  "Lookup the busted resource uri for a given path.  Returns the given path if none is found."
  (or (gethash path *asset-lookup-table* nil) (pathname (concatenate 'string "/" *base-path* path))))


(defparameter *clack-static-asset-middleware*
  (lambda (app &key
                 (path "static/") (root #P"./")
                 (cache-buster-function +default-cache-buster-function+)
                 (cache-unbuster-function +default-cache-unbuster-function+)
                 (filter-function +default-filter-function+))

    ;; Collect hashes of static assets
    (let ((asset-table (inventory-files (walk-directory root :filter-function filter-function)
                                        path
                                        root
                                        :cache-buster-function cache-buster-function)))
      (lambda (env)
        ;; Strip the leading /
        (let ((request-path (subseq (getf env :path-info) 1)))

          ;; Is this a request for us to handle? (Is it a path under `path`?)
          (multiple-value-bind (asset-request-p relative-path) (starts-with-subseq path request-path :return-suffix t)

            (if asset-request-p
                ;; Serve the asset
                (asset-response (funcall cache-unbuster-function relative-path) root)

                ;; Bind the asset table so helper functions inside the request can
                ;; get to it.
                (let ((*asset-lookup-table* asset-table)
                      (*base-path* path))
                  (funcall app env)))))))))
