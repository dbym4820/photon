(in-package :cl-user)
(defpackage photon
  (:use :cl)
  (:import-from :photon.window
                :generate-window
                :display-window
                :vanish-window)
  (:import-from :photon.concept
                :convert-concept)
  (:export :start-photon
           :stop-photon))
(in-package :photon)

(defun start-photon ()
  )


(defun stop-photon ())
