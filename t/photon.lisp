(in-package :cl-user)
(defpackage photon-test
  (:use :cl)
  (:import-from :photon
		:init
		:show-concepts
		:concept-name
		:find-concept)
  (:import-from :prove
		:plan
		:subtest
		:ok :ng
		:is
		:finalize))
(in-package :photon-test)

;; NOTE: To run this test file, execute `(asdf:test-system :photon)' in your Lisp.

(plan 3)

(subtest "photon initialize"
  (ok (not (init))))

(subtest "basic-concept check"
  (mapcar #'(lambda (c)
	      (is-type (find-concept c) 'photon.ontology::basic-concept))
	  (show-concepts)))

(subtest "whole-root existence check"
  (is (concept-name (find-concept "whole-root")) "whole-root"))

(finalize)
