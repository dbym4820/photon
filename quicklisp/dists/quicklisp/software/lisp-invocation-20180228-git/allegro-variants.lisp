(uiop:define-package :lisp-invocation/allegro-variants
  (:use :common-lisp :fare-utils :uiop :lisp-invocation/lisp-invocation)
  (:import-from #:lisp-invocation ;; slot names
   #:name #:flags #:disable-debugger #:load-flag #:eval-flag
   #:image-flag #:default-image #:image-executable-p #:standalone-executable
   #:arguments-end #:argument-control)
  (:export #:all-allegro-variants #:current-lisp-variant))

(in-package :lisp-invocation/allegro-variants)

#|
Allegro is supported in the following flavors:
   allegro, allegro8, allegromodern, allegromodern8,
   allegro_s, allegro8_s, allegromodern_s, allegromodern8_s (SMP variants)
   allegro_64, allegro8_64, allegromodern_64, allegromodern8_64 (64-bit variants),
   allegro_64_s, allegro8_64_s, allegromodern_64_s, allegromodern8_64_s, (SMP, 64-bit variants)
Allegro CL is a special case: instead of setting environment variables for the specific runtime
locations, you may simply specify the Allegro install directories using these variables:
    ALLEGRO64DIR, ALLEGRO64SDIR (64-bit Allegro and SMP Allegro, respectively),
    ALLEGRODIR, and ALLEGROSDIR.
|#

(defclass allegro-implementation (simple-lisp-implementation)
  ((build :initarg :build :reader allegro-implementation-build)))

(defmethod lisp-implementation-invocation-arglist
    ((implementation allegro-implementation)
     &key
       lisp-path
       (lisp-flags :default)
       (image-path nil image-path-p)
       load
       eval
       arguments
       debugger
       cross-compile
       console)
  (nest
   (if (not (and console (allegro-implementation-build implementation)))
       (call-next-method))
   (with-slots (name flags disable-debugger load-flag eval-flag
                image-flag default-image image-executable-p standalone-executable build
                arguments-end argument-control)
       implementation)
   (let* ((implementation-type (lisp-implementation-implementation-type implementation))
          (envexe (getenv-pathname (lisp-environment-variable-name
                                    :type implementation-type :prefix (when cross-compile "X")))))
     (unless image-path-p
       (setf image-path (if envexe
                            (native-namestring (make-pathname :defaults envexe :type "dxl"))
                            default-image))))
   (append
    (when (or (null image-path) (not image-executable-p))
      (ensure-list
       (or
        (when lisp-path (ensure-list lisp-path))
        (ensure-path-executable lisp-path)
        (when envexe (ensure-path-executable (make-pathname :name build :defaults envexe)))
        name)))
    (when (and image-path (not image-executable-p))
      (list image-flag))
    (when image-path
      (list
       (if image-executable-p
           (ensure-path-executable image-path)
           image-path)))
    (if (eq lisp-flags :default)
        flags
        lisp-flags)
    (unless debugger
      disable-debugger)
    (mapcan (if load-flag
                (lambda (x) (list load-flag (native-namestring x)))
                (lambda (x) (list eval-flag (format nil "(load ~S)" (native-namestring x)))))
            (ensure-list load))
    (when eval
      (list eval-flag eval))
    (when arguments
      (unless argument-control
        (error "Can't reliably pass arguments to Lisp implementation ~A" implementation-type))
      (cons arguments-end arguments)))))

(defun all-allegro-variants (&optional (windowsp (os-windows-p)))
  "Return a list of possible Allegro variants based on built-in information
and environment variables.  The returned list is made up of argument lists for
REGISTER-LISP-IMPLEMENTATION."
  ;; Beware that on Windows, we use the non-multithreaded console application build.exe
  ;; so that we may have console I/O on the existing stdin and stdout, that we may capture.
  (while-collecting (c)
    (loop
      :for (smpvar smpname smpfullname) :in `(("" "" "") ("S" :_s " (SMP)")) :do
      (loop
        :for (bitsvar bitsname bitsfullname) :in '(("" "" "") ("64" "_64" " (64-bit words)"))
        :for dirvar = (format nil "~:@(ALLEGRO~A~ADIR~)" bitsvar smpvar)
        :for dir = (getenv-pathname dirvar :want-absolute t :ensure-directory t) :do
          (loop :for (charname charfullname) :in '(("" "") ("8" " (8-bit chars)")) :do
            (loop
              :for (caseexe casename casefullname) :in
              '(("a" "" "") ("m" :modern " (modern syntax)"))
              :for allegro-variant = (conc-keyword :allegro casename charname bitsname smpname)
              :for fullname = (strcat "Allegro CL"
                                      casefullname charfullname bitsfullname smpfullname)
              :for executable = (format nil "~(~alisp~a~)" caseexe charname)
              :for build = (when windowsp (if (emptyp charfullname) "buildi" "build"))
              :for (exepath imgpath) = (if windowsp
                                           (list (subpathname dir (strcat build ".exe"))
                                                 (subpathname dir executable :type "dxl"))
                                           (list (subpathname dir executable) nil)) :do
              (c `(allegro-implementation
                   ,allegro-variant
                   :fullname ,fullname
                   :name ,(native-namestring exepath)
                   :default-image ,(native-namestring imgpath)
                   :build ,build
                   :feature :allegro ;; do we want a more discriminating feature expression?
                   :flags ("-qq")
                   :eval-flag "-e"
                   :load-flag "-L"
                   ;; :quit-flags ("-kill")
                   :arguments-end "--"
                   :image-flag "-I"
                   :image-executable-p nil
                   :standalone-executable nil
                   :argument-control t
                   :disable-debugger ("-batch") ; see also -#D -#C -#!
                   :quit-format "(excl:exit ~A :quiet t)"
                   :dump-format "(progn (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) (excl:dumplisp :name ~A :suppress-allegro-cl-banner t))"))))))))


(defun current-lisp-variant ()
  (let ((type (implementation-type)))
    (case type
      (:acl
       (conc-keyword
        :allegro
        ;; I would have liked to make it depend only on the rebindable *features*, but there's
        ;; nothing in Allegro's *features* for case sensitivity, though there is for :ICS support.
        #+allegro (when (eq excl:*current-case-mode* :case-sensitive-lower) :modern)
        (unless (featurep :ics) "8") ;; in uiop/os, we use: (excl:ics-target-case (:-ics "8"))
        (when (featurep :64bit) "_64")
        (when (featurep :smp) :_s)))
      (:ecl
       (if (featurep :ecl-bytecmp)
           :ecl_bytecodes
           :ecl))
      ;; Unshorten some aliases
      (:cmu :cmucl)
      (:lwpe :lispworks-personal-edition)
      (:lw :lispworks)
      (:smbx :symbolics)
      (otherwise
       type))))

(map () 'register-lisp-implementation* (all-allegro-variants))
