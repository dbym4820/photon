(in-package :cl-user)
(defpackage clack.test.suite
  (:use :cl
        :prove)
  (:import-from :clack.test
                :*clack-test-handler*
                :*clack-test-port*
                :*clack-test-access-port*
                :*enable-debug*
                :subtest-app)
  (:import-from :flexi-streams
                :octet
                :octets-to-string)
  (:import-from :http-body
                :parse)
  (:export :run-server-tests))
(in-package :clack.test.suite)

(defvar *clack-pathname*
  (asdf:system-source-directory :clack))

(defun localhost (&optional (path "/"))
  (clack.test:localhost path *clack-test-access-port*))

(defun run-server-tests (handler-name)
  "Run tests for clack.handler.
Handler name is a keyword and doesn't include the clack.handler prefix.
For example, if you have a handler `clack.handler.foo',
you would call like this: `(run-server-tests :foo)'."
  (let ((*clack-test-handler* handler-name)
        (*package* (find-package :clack.test.suite))
        (dex:*use-connection-pool* nil))
    (plan 36)
    #+thread-support
    (%run-tests)
    #-thread-support
    (skip 36 "because your Lisp doesn't support threads")
    (finalize)))

(defun get-header (headers key)
  (gethash (string-downcase key) headers))

(defun file-size (file)
  (with-open-file (in file :direction :input)
    (file-length in)))


;; Tests

(defun %run-tests ()
  (subtest-app "list body"
      (lambda (env)
        (declare (ignore env))
        '(200 (:content-type "text/plain") ("Hello" "World")))
    (multiple-value-bind (body status)
        (dex:get (localhost))
      (is status 200)
      (is body "HelloWorld")))

  (subtest-app "SCRIPT-NAME"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :script-name))))
    (ok (member (dex:get (localhost)) '(nil "") :test #'equal)))

  (subtest-app "GET"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(format nil "Hello, ~A" (getf env :query-string)))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/?name=fukamachi"))
      (is status 200)
      (is (get-header headers :content-type)
          "text/plain; charset=utf-8")
      (is body "Hello, name=fukamachi")))

  (subtest-app "POST"
      (lambda (env)
        (let ((body (make-array 11 :element-type '(unsigned-byte 8))))
          (read-sequence body (getf env :raw-body))
          `(200
            (:content-type "text/plain; charset=utf-8"
             :client-content-length ,(getf env :content-length)
             :client-content-type ,(getf env :content-type))
            (,(format nil "Hello, ~A" (babel:octets-to-string body))))))
    (multiple-value-bind (body status headers)
        (dex:post (localhost)
                  :content '(("name" . "eitaro")))
      (is status 200)
      (is (get-header headers :client-content-length) 11)
      (is (get-header headers :client-content-type) "application/x-www-form-urlencoded")
      (is body "Hello, name=eitaro")))

  (subtest-app "big POST"
      (lambda (env)
        (let ((body
                (make-array (getf env :content-length)
                            :element-type 'octet)))
          (read-sequence body (getf env :raw-body))
          `(200
            (:content-type "text/plain; charset=utf-8"
             :client-content-length ,(getf env :content-length)
             :client-content-type ,(getf env :content-type))
            (,(flex:octets-to-string body)))))
    (let* ((chunk
             (with-output-to-string (chunk)
               (dotimes (i 12000) (write-string "abcdefgh" chunk))
               chunk))
           (len (length chunk)))
      (multiple-value-bind (body status headers)
          (dex:post (localhost)
                    :headers
                    `((:content-type . "application/octet-stream")
                      (:content-length . ,len))
                    :content chunk)
        (is status 200)
        (is (get-header headers :client-content-length)
            len)
        (is (length body) len))))

  (subtest-app "big POST (chunked)"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8"
           :client-content-length ,(getf env :content-length)
           :client-content-type ,(getf env :content-type))
          (,(let* ((body (getf env :raw-body))
                   (buffer (make-array 1024 :element-type '(unsigned-byte 8))))
              (apply #'concatenate 'string
                     (loop for read-bytes = (read-sequence buffer body)
                           collect (flex:octets-to-string (subseq buffer 0 read-bytes))
                           while (= read-bytes 1024)))))))
    (let* ((chunk
             (with-output-to-string (chunk)
               (dotimes (i 12000) (write-string "abcdefgh" chunk))
               chunk))
           (len (length chunk)))
      (multiple-value-bind (body status headers)
          (dex:post (localhost)
                    :headers '((:content-type . "application/octet-stream")
                               (:content-length . nil))
                    :content chunk)
        (is status 200)
        (if (eq *clack-test-handler* :fcgi)
            (skip 1 "because FCGI handler always adds :CONTENT-TYPE")
            (is (get-header headers :client-content-length)
                nil))
        (is (length body) len))))

  (subtest-app "url-scheme"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :url-scheme))))
    (multiple-value-bind (body status headers)
        (dex:post (localhost))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (is body "http")))

  (subtest-app "return pathname"
      (lambda (env)
        (declare (ignore env))
        `(200
          (:content-type "text/plain; charset=utf-8")
          ,(merge-pathnames #p"tmp/file.txt" *clack-pathname*)))
    (multiple-value-bind (body status headers)
        (dex:get (localhost))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (like body "This is a text for test.")))

  (subtest-app "binary file"
      (lambda (env)
        (declare (ignore env))
        (let ((file (merge-pathnames #p"tmp/redhat.png" *clack-pathname*)))
          `(200
            (:content-type "image/png"
             :content-length ,(file-size file))
            ,file)))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/redhat.png"))
      (is status 200)
      (is (get-header headers :content-type) "image/png")
      (if (eq *clack-test-handler* :wookie)
          (is (get-header headers :transfer-encoding) "chunked"
              "Wookie always returns with Transfer-Encoding: chunked and no Content-Length.")
          (ok (get-header headers :content-length)))
      (is (length body) 12155)))

  (subtest-app "bigger file"
      (lambda (env)
        (declare (ignore env))
        (let ((file (merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*)))
          `(200
            (:content-type "image/jpeg"
             :content-length ,(file-size file))
            ,file)))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/jellyfish.jpg"))
      (is status 200)
      (is (get-header headers :content-type) "image/jpeg")
      (if (eq *clack-test-handler* :wookie)
          (is (get-header headers :transfer-encoding) "chunked"
              "Wookie always returns with Transfer-Encoding: chunked and no Content-Length.")
          (ok (get-header headers :content-length)))
      (is (length body) 139616)))

  (subtest-app "handle HTTP-Header"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(gethash "foo" (getf env :headers)))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/foo/?ediweitz=weitzedi")
                 :headers '(("Foo" . "Bar")))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (is body "Bar")))

  (subtest-app "handler HTTP-Cookie"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(gethash "cookie" (getf env :headers)))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/foo/?ediweitz=weitzedi")
                 :headers '(("Cookie" . "foo")))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (is body "foo")))

  (subtest-app "validate env"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(with-output-to-string (str)
              (loop for h in '(:request-method
                               :path-info
                               :query-string
                               :server-name
                               :server-port)
                    do (format str "~A:~S~%" h (getf env h)))))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/foo/?ediweitz=weitzedi"))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (is body (format nil "~{~A~%~}"
                       `("REQUEST-METHOD::GET"
                         "PATH-INFO:\"/foo/\""
                         "QUERY-STRING:\"ediweitz=weitzedi\""
                         ,(if (eq *clack-test-handler* :fcgi)
                              "SERVER-NAME:\"localhost\"" ;; probably the name from Nginx conf
                              "SERVER-NAME:\"127.0.0.1\"")
                         ,(format nil "SERVER-PORT:~D" *clack-test-access-port*))))))

  (subtest-app "validate env (must be integer)"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(with-output-to-string (str)
              (loop for h in '(:server-port
                               :remote-port
                               :content-length)
                    do (format str "~A:~A~%" h (typep (getf env h) '(or integer null))))))))
    (multiple-value-bind (body status headers)
        (dex:post (localhost)
                  :content '(("name" . "eitaro")))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (is body (format nil "~{~A~%~}"
                       `("SERVER-PORT:T"
                         "REMOTE-PORT:T"
                         "CONTENT-LENGTH:T")))))

  (subtest-app "% encoding in PATH-INFO"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :path-info))))
    (is (dex:get (localhost "/foo/bar%2cbaz")) "/foo/bar,baz"))

  (subtest-app "% double encoding in PATH-INFO"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :path-info))))
    (is (dex:get (localhost "/foo/bar%252cbaz")) "/foo/bar%2cbaz"))

  (subtest-app "% encoding in PATH-INFO (outside of URI characters)"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :path-info))))
    (is (dex:get (localhost "/foo%E3%81%82"))
        (format nil "/foo~A"
                (flex:octets-to-string #(#xE3 #x81 #x82) :external-format :utf-8))))

  (subtest-app "Invalid UTF-8 encoded PATH-INFO"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :path-info))))
      (if (eq *clack-test-handler* :wookie)
          (skip 1 "because do-urlencode Wookie uses cannot decode invalid UTF8 strings anyways")
          (like (dex:get (localhost "/%E3%81%82%BF%27%22%28"))
                (format nil "/あ~A"
                        #+abcl "\\?"
                        #-abcl #\Replacement_Character))))

  (subtest-app "SERVER-PROTOCOL is required"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(prin1-to-string (getf env :server-protocol)))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/foo/?ediweitz=weitzedi"))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (like body "^:HTTP/1\\.[01]$")))

  (subtest-app "SCRIPT-NAME should not be nil"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(princ-to-string (not (null (getf env :script-name)))))))
    (is (dex:get (localhost "/foo/?ediweitz=weitzedi"))
        "T"
        :test #'equalp))

  (let ((*enable-debug* nil)
        (*error-output* (make-broadcast-stream)))
    (subtest-app "Do not crash when the app dies"
        (lambda (env)
          (declare (ignore env))
          (error "Throwing an exception from app handler. Server shouldn't crash."))
      (handler-case (dex:get (localhost))
        (dex:http-request-internal-server-error ()
          (pass "500 Internal Server Error")))))

  (subtest-app "multi headers (request)"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(gethash "foo" (getf env :headers)))))
    (like
     (dex:get (localhost)
              :headers '(("Foo" . "bar")
                                         ("Foo" . "baz")))
     "^bar,\\s*baz$"))

  (subtest-app "multi headers (response)"
      (lambda (env)
        (declare (ignore env))
        `(200
          (:content-type "text/plain; charset=utf-8"
           :x-foo "foo"
           :x-foo "bar, baz")
          ("hi")))
    (let ((headers (nth-value 2 (dex:get (localhost)))))
      (like (get-header headers :x-foo) "foo,\\s*bar,\\s*baz")))

  (subtest-app "Do not set COOKIE"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8"
           :x-cookie ,(not (null (getf env :cookie))))
          (,(gethash "cookie" (getf env :headers)))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost)
                 :headers '(("Cookie" . "foo=bar")))
      (is status 200)
      (is (get-header headers :x-cookie) nil)
      (is body "foo=bar")))

  ;; NOTE: This may fail on Hunchentoot because of its bug.
  ;;   Hunchentoot returns Content-Type header
  ;;   though 304 Not Modified.
  ;; And Wookie also always returns Transfer-Encoding header.
  (subtest-app "no entity headers on 304"
      (lambda (env)
        (declare (ignore env))
        `(304 nil nil))
    (if (or (eq *clack-test-handler* :hunchentoot)
            (eq *clack-test-handler* :toot)
            (eq *clack-test-handler* :wookie))
        (skip 5 (format nil "because of ~:(~A~)'s bug" *clack-test-handler*))
        (multiple-value-bind (body status headers)
            (dex:get (localhost))
          (is status 304)
          (is body #() :test #'equalp)
          (is (nth-value 1 (get-header headers :content-type)) nil "No Content-Type")
          (is (nth-value 1 (get-header headers :content-length)) nil "No Content-Length")
          (is (nth-value 1 (get-header headers :transfer-encoding)) nil "No Transfer-Encoding"))))

  (subtest-app "REQUEST-URI is set"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :request-uri))))
    (if (eq *clack-test-handler* :toot)
        (skip 1 "because of ~:(~A~)'s bug" *clack-test-handler*)
        (is (dex:get (localhost "/foo/bar%20baz%73?x=a")) "/foo/bar%20baz%73?x=a")))

  (subtest-app "a big header value > 128 bytes"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(gethash "x-foo" (getf env :headers)))))
    (let ((chunk
            (with-output-to-string (chunk)
              (dotimes (i 12000) (write-string "abcdefgh" chunk))
              chunk)))
      (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
        (multiple-value-bind (body status)
            (dex:get (localhost)
                     :headers `(("X-Foo" . ,chunk)))
          (if (eq :fcgi *clack-test-handler*)
              (progn
                (is status 400)
                (like body "400 Request Header Or Cookie Too Large"))
              (progn
                (is status 200)
                (is body chunk)))))))

  (subtest-app "CRLF output"
      (lambda (env)
        (declare (ignore env))
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(format nil "Foo: Bar~A~A~A~AHello World"
                    #\Return #\NewLine #\Return #\NewLine))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost))
      (is status 200)
      (is (get-header headers :foo) nil)
      (is body (format nil "Foo: Bar~A~A~A~AHello World"
                       #\Return #\NewLine #\Return #\NewLine))))

  (subtest-app "test 404"
      (lambda (env)
        (declare (ignore env))
        '(404
          (:content-type "text/plain; charset=utf-8")
          ("Not Found")))
    (multiple-value-bind (body status)
        (handler-bind ((dex:http-request-not-found #'dex:ignore-and-continue))
          (dex:get (localhost)))
      (is status 404)
      (is body "Not Found")))

  (subtest-app "request -> input seekable"
      (lambda (env)
        (let ((body (make-array 4 :element-type '(unsigned-byte 8))))
          (read-sequence body (getf env :raw-body))
          `(200
            (:content-type "text/plain; charset=utf-8")
            (,(babel:octets-to-string body)))))
    (is (dex:post (localhost)
                  :content "body")
        "body"))

  (subtest-app "Content-Length 0 is not set Transfer-Encoding"
      (lambda (env)
        (declare (ignore env))
        `(200
          (:content-length 0
           :content-type "text/plain")
          ("")))
    (multiple-value-bind (body status headers)
        (dex:get (localhost))
      (is status 200)
      (is (get-header headers :client-transfer-encoding) nil)
      (is body "")))

  (subtest-app "handle Authorization header"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8"
           :x-authorization ,(not (null (gethash "authorization" (getf env :headers)))))
          (,(gethash "authorization" (getf env :headers) ""))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost)
                 :headers '(("Authorization" . "Basic XXXX")))
      (is status 200)
      (is (get-header headers :x-authorization) "T"
          :test #'equalp)
      (is body "Basic XXXX"))
    ;; XXX: On Wookie handler, this raises USOCKET:CONNECTION-REFUSED-ERROR.
    (unless (eq *clack-test-handler* :wookie)
      (multiple-value-bind (body status headers)
          (dex:get (localhost))
        (is status 200)
        (is (get-header headers :x-authorization) nil)
        (ok (member body '(nil "") :test #'equal)))))

  (subtest-app "repeated slashes"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :path-info))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/foo///bar/baz"))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (is body "/foo///bar/baz")))

  (subtest-app "file upload"
      (lambda (env)
        (destructuring-bind (name body params headers)
            (car (http-body:parse
                  (getf env :content-type)
                  (getf env :content-length)
                  (getf env :raw-body)))
          (declare (ignore name params headers))
          `(200
            (:content-type "text/plain; charset=utf-8")
            (,(let* ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))
                     (read-bytes (read-sequence buffer body)))
                (flex:octets-to-string (subseq buffer 0 read-bytes)))))))
    (multiple-value-bind (body status)
        (dex:post (localhost)
                  :content
                  `(("file" . ,(merge-pathnames #p"tmp/file.txt" *clack-pathname*))))
      (is status 200)
      (is body "This is a text for test.
")))

  (subtest-app "large file upload"
      (lambda (env)
        (destructuring-bind (name body params headers)
            (car (http-body:parse
                  (getf env :content-type)
                  (getf env :content-length)
                  (getf env :raw-body)))
          (declare (ignore name params headers))
          (let ((body-file
                  (uiop:with-temporary-file (:stream out :pathname tmp
                                             :direction :output
                                             :element-type '(unsigned-byte 8)
                                             :keep t)
                    (alexandria:copy-stream body out)
                    tmp)))
            `(200
              (:content-type "text/plain")
              (,(if (equalp (ironclad:digest-file :sha1 body-file)
                            (ironclad:digest-file :sha1 (merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*)))
                    "ok"
                    (format nil "ng (~A)" body-file)))))))
    (multiple-value-bind (body status)
        (dex:post (localhost)
                  :content
                  `(("file" . ,(merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*))))
      (is status 200)
      (is body "ok")))

  (subtest-app "streaming"
      (lambda (env)
        (declare (ignore env))
        (lambda (res)
          (let ((writer (funcall res '(200 (:content-type "text/plain")))))
            (loop for i from 0 to 2
                  do (sleep 1)
                     (funcall writer (format nil "~S~%" i)))
            (funcall writer "" :close t))))
    (if (find *clack-test-handler* '(:hunchentoot
                                     :toot
                                     :fcgi
                                     :wookie
                                     :woo))
        (multiple-value-bind (body status)
            (dex:get (localhost))
          (is status 200)
          (is body (format nil "0~%1~%2~%")))
        (skip 2 (format nil "because ~:(~A~) doesn't support streaming" *clack-test-handler*)))))
