(in-package :cl-user)
(defpackage photon-test
  (:use :cl
        :photon
        :prove))
(in-package :photon-test)

;; NOTE: To run this test file, execute `(asdf:test-system :photon)' in your Lisp.

(plan 3)

(subtest "photon initialize"
  (ok (not (photon:init))))

(subtest "basic-concept check"
  (mapcar #'(lambda (c)
	      (is-type (find-concept c) 'photon.ontology::basic-concept))
	  (show-concepts)))

(subtest "whole-root existence check"
  (is (photon:concept-name (photon:find-concept "whole-root")) "whole-root"))

(finalize)
