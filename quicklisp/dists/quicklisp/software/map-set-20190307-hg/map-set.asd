;;;; map-set.asd
;;;; Copyright (c) 2013 Robert Smith

(asdf:defsystem #:map-set
  :serial t
  :description "Set-like data structure."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE)"
  :components ((:file "package")
               (:file "map-set")))
