(in-package :cl-user)
(defpackage photon-test
  (:use :cl
        :photon
        :prove))
(in-package :photon-test)

;; NOTE: To run this test file, execute `(asdf:test-system :photon)' in your Lisp.

(plan 3)

(subtest "convert-test"
  (ok (not (not (photon:convert-ontology)))))

(subtest "basic-concept check"
  (loop for c in (photon:show-concepts)
	do (is-type (photon:find-concept c) 'photon.ontology::basic-concept)))

(subtest "whole-root existence check"
  (is "whole-root" (photon:concept-name (photon:find-concept "whole-root"))))


(finalize)
