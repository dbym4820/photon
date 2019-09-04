;;; -*- mode: lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software available under an MIT-style license. See LICENSE  ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008 ITA Software, Inc.  All rights reserved.      ;;;
;;;                                                                  ;;;
;;; Original author: Francois-Rene Rideau                            ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (or #+asdf3.1 (version<= "3.1.2" (asdf-version)))
  (error "ASDF 3.1.2 required"))

(defsystem "lisp-invocation"
  :version "1.0.14"
  :author ("Francois-Rene Rideau")
  :maintainer "Francois-Rene Rideau"
  :licence "MIT"
  :description "Invoking Lisp subprocesses from Lisp"
  :long-description "lisp-invocation allows you to portably execute Lisp code
as subprocesses of a current Lisp process.
All known command-line accessible Common Lisp implementations are supported."
  :class package-inferred-system
  :depends-on ("lisp-invocation/lisp-invocation" "lisp-invocation/implementations"))

(defsystem "lisp-invocation/all"
  :version (:read-file-form "lisp-invocation.asd" :at (1 3))
  :depends-on ("lisp-invocation"
               "lisp-invocation/non-special"
               "lisp-invocation/allegro-variants"))
