(in-package :cl-user)
(defpackage clack-static-asset-middleware-test
  (:use :cl
        :clack-static-asset-middleware
        :prove)
  (:import-from #:lack.test
                #:generate-env))
(in-package :clack-static-asset-middleware-test)

;; NOTE: To run this test file, execute `(asdf:test-system :clack-static-asset-middleware)' in your Lisp.

(plan 8)

(subtest "File filters"

  (ok (not (funcall clack-static-asset-middleware::+default-filter-function+ (pathname "/home/user/file.txt")))
      "Non-dotted files allowed.")
  (ok (funcall clack-static-asset-middleware::+default-filter-function+ (pathname "/home/user/.file.txt"))
      "Dotted files are filtered."))

(subtest "Cache buster"
         (is (funcall clack-static-asset-middleware:+default-cache-buster-function+ (pathname "style/homepage.css") "2867f3f83a6a91ad4a19a6cd45536152")
             (pathname "style/homepage_2867f3f83a6a91ad4a19a6cd45536152.css")
             "Cache busting works."))

(subtest "Cache *un*buster"
         (is (funcall clack-static-asset-middleware:+default-cache-unbuster-function+ "style/homepage_2867f3f83a6a91ad4a19a6cd45536152.css")
             "style/homepage.css"
             "Parses busted urls correctly.")
         (is (funcall clack-static-asset-middleware:+default-cache-unbuster-function+ "style/homepage.css")
             "style/homepage.css"
             "Allows unbusted urls, too."))


(subtest "MD5 Files"
  (is (clack-static-asset-middleware::md5-file (asdf:system-relative-pathname :clack-static-asset-middleware-test "t/assets/potato.txt"))
      "f7cd4ebd7c09e497bd3f7da2ab4451e7"
      "Text file.")

    (is (clack-static-asset-middleware::md5-file (asdf:system-relative-pathname :clack-static-asset-middleware-test "t/assets/images/gustywinds.jpg"))
        "2867f3f83a6a91ad4a19a6cd45536152"
        "Image file."))

(subtest "Relative pathnames"

  (let ((root (pathname #P"/var/www/assets/")))
    (is (clack-static-asset-middleware::root-relative-path (pathname "/var/www/assets/potato.jpg") root)
        "potato.jpg"
        "Files in the base of 'root' directory resolve correctly.")

    (is (clack-static-asset-middleware::root-relative-path (pathname "/var/www/assets/styles/best-assets.css") root)
        "styles/best-assets.css"
        "Files in subdirectories resolve correctly.")))

(subtest "Serving assets"
  (let ((app (funcall clack-static-asset-middleware:*clack-static-asset-middleware*
                      (lambda (env) (declare (ignore env))
                              '(200 (:content-type "text/plain") ("Happy Valentine!")))
                      :root (asdf:system-relative-pathname :clack-static-asset-middleware "t/assets/")
                      :path "static/")))

    (destructuring-bind (status headers body) (funcall app (generate-env "/static/images/gustywinds_2867f3f83a6a91ad4a19a6cd45536152.jpg"))
      (declare (ignore headers body))
      (is status 200 "Busted assets are found."))

    (destructuring-bind (status headers body) (funcall app (generate-env "/static/images/gustywinds_2867f3f83a6a91ad4a19a6cd45536152.jpg?potato=pizza"))
      (declare (ignore headers body))
      (is status 200 "Busted assets with querystrings are found."))

    (destructuring-bind (status headers body) (funcall app (generate-env "/static/images/gustywinds.jpg"))
      (declare (ignore headers body))
      (is status 200 "Unbusted urls are found."))

    (destructuring-bind (status headers body) (funcall app (generate-env "/"))
      (declare (ignore headers body))
      (is status 200 "The app still receives requests."))

    (destructuring-bind (status headers body) (funcall app (generate-env "/static/madness/hypnotoad.png"))
      (declare (ignore headers body))
      (is status 404 "Missing assets 404."))))

(subtest "Getting cache busted uris"
  (let ((app (funcall clack-static-asset-middleware:*clack-static-asset-middleware*
                      (lambda (env) (declare (ignore env))
                              `(200 (:content-type "text/plain") (,(busted-uri-for-path "images/gustywinds.jpg"))))
                      :root (asdf:system-relative-pathname :clack-static-asset-middleware "t/assets/")
                      :path "static/")))

    (destructuring-bind (status headers body) (funcall app (generate-env "/"))
      (declare (ignore headers status))
      (is (car body) #P"/static/images/gustywinds_2867f3f83a6a91ad4a19a6cd45536152.jpg"
          "Cache busted uri's lookup.")))

  (let ((app (funcall clack-static-asset-middleware:*clack-static-asset-middleware*
                      (lambda (env) (declare (ignore env))
                              `(200 (:content-type "text/plain") (,(busted-uri-for-path "email/logo.png"))))
                      :root (asdf:system-relative-pathname :clack-static-asset-middleware "t/assets/")
                      :path "static/")))

    (destructuring-bind (status headers body) (funcall app (generate-env "/"))
      (declare (ignore headers status))
      (is (car body) #P"/static/email/logo.png"
          "Even if we don't find a url, the given uri is returned."))))

(subtest "Djula template helpers"

  (djula:add-template-directory
   (asdf:system-relative-pathname :clack-static-asset-middleware #p"t/templates/"))

  (let* ((+template+ (djula:compile-template* "style.html"))
         (app (funcall clack-static-asset-middleware:*clack-static-asset-middleware*
                       (lambda (env) (declare (ignore env))
                               `(200 (:content-type "text/plain") (,(djula:render-template* +template+ ))))
                       :root (asdf:system-relative-pathname :clack-static-asset-middleware "t/assets/")
                       :path "static/")))

    (destructuring-bind (status headers body) (funcall app (generate-env "/"))
      (declare (ignore headers status))
      (is (car body) "<link rel=\"stylesheet\" href=\"/static/styles/cool_a05b624b84b58992a24f93011325878f.css\">
"
          "Stylesheet tag works.")))

  (let* ((+template+ (djula:compile-template* "static.html"))
         (app (funcall clack-static-asset-middleware:*clack-static-asset-middleware*
                       (lambda (env) (declare (ignore env))
                               `(200 (:content-type "text/plain") (,(djula:render-template* +template+ ))))
                       :root (asdf:system-relative-pathname :clack-static-asset-middleware "t/assets/")
                       :path "static/")))

    (destructuring-bind (status headers body) (funcall app (generate-env "/"))
      (declare (ignore headers status))
      (is (car body) "<img src=\"/static/images/gustywinds_2867f3f83a6a91ad4a19a6cd45536152.jpg\" />
"
          "Static asset tag works."))))

(finalize)
