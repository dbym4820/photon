;;; Lisp implementations
#+xcvb (module (:build-depends-on ("/asdf")))

(uiop:define-package :lisp-invocation/lisp-invocation
  (:nicknames :lisp-invocation)
  (:use :cl :uiop)
  (:export
   #:lisp-implementation
   #:simple-lisp-implementation
   #:define-lisp-implementation
   #:get-lisp-implementation
   #:ensure-path-executable
   #:lisp-implementation-implementation-type
   #:lisp-implementation-fullname
   #:lisp-implementation-name
   #:lisp-implementation-feature
   #:lisp-implementation-flags
   #:lisp-implementation-eval-flag
   #:lisp-implementation-load-flag
   #:lisp-implementation-arguments-end
   #:lisp-implementation-environment-variable
   #:lisp-implementation-image-flag
   #:lisp-implementation-image-executable-p
   #:lisp-implementation-standalone-executable
   #:lisp-implementation-argument-control
   #:lisp-implementation-disable-debugger
   #:lisp-implementation-directory-variable
   #:lisp-implementation-invoker
   #:lisp-environment-variable-name
   #:lisp-invocation-arglist
   #:lisp-implementation-invocation-arglist
   #:invoke-lisp #:invoke-lisp-directly #:invoke-lisp-via-script
   #:register-lisp-implementation
   #:register-lisp-implementation*
   #:quit-form
   #:save-image-form))

(in-package :lisp-invocation)

(defvar *lisp-implementations* (make-hash-table :test 'equal)
  "Dictionary of known Lisp implementations")

(defclass lisp-implementation ()
  (;; the first also names the environment variable for the lisp-path, as per cl-launch
   (identifiers :initarg :identifiers :reader lisp-implementation-identifiers)))

(defmethod lisp-implementation-implementation-type ((impl lisp-implementation))
  (first (lisp-implementation-identifiers impl)))

(defclass simple-lisp-implementation (lisp-implementation)
  ((fullname :initarg :fullname :reader lisp-implementation-fullname)
   (name :initarg :name :reader lisp-implementation-name)
   (feature :initarg :feature :reader lisp-implementation-feature)
   (environment-variable :initform nil :initarg :environment-variable :reader lisp-implementation-environment-variable)
   (flags :initarg :flags :reader lisp-implementation-flags)
   (eval-flag :initarg :eval-flag :reader lisp-implementation-eval-flag)
   (load-flag :initarg :load-flag :reader lisp-implementation-load-flag)
   (arguments-end :initarg :arguments-end :reader lisp-implementation-arguments-end)
   (image-flag :initarg :image-flag :reader lisp-implementation-image-flag)
   (image-executable-p :initarg :image-executable-p :reader lisp-implementation-image-executable-p)
   (default-image :initform nil :initarg :default-image :reader lisp-implementation-default-image)
   (standalone-executable :initarg :standalone-executable :reader lisp-implementation-standalone-executable)
   (argument-control :initarg :argument-control :reader lisp-implementation-argument-control)
   (disable-debugger :initarg :disable-debugger :reader lisp-implementation-disable-debugger)
   (directory-variable :initform nil :initarg :directory-variable :reader lisp-implementation-directory-variable)
   ;; fasl-type cfasl-type
   (invoker :initform nil :initarg :invoker :reader lisp-implementation-invoker)
   (quit-format :initarg :quit-format :reader lisp-implementation-quit-format)
   (dump-format :initarg :dump-format :reader lisp-implementation-dump-format)))

(defmacro define-lisp-implementation (key (&optional class) &rest keys)
  `(apply 'register-lisp-implementation ',class ',key ',keys))

(defun register-lisp-implementation (class identifiers &rest keys)
  "Register the lisp implementation identified by the IDENTIFIERS argument (a
keyword or list of keywords), with given option KEYS."
  (let* ((identifiers (ensure-list identifiers))
         (implementation (apply #'make-instance (or class 'simple-lisp-implementation)
                                :identifiers identifiers keys)))
    (dolist (id identifiers)
      (assert (keywordp id))
      (setf (gethash id *lisp-implementations*) implementation))))

(defun register-lisp-implementation* (x)
  "Register the lisp implementation described by the list X, which consists of a name
followed by a plist of keywords and arguments."
  (apply 'register-lisp-implementation x))

(defun get-lisp-implementation (&optional (implementation-type (implementation-type)))
  (or (gethash implementation-type *lisp-implementations*)
      (error "Unknown Lisp implementation type ~S" implementation-type)))

(defun ensure-path-executable (x)
  (when x
    (let ((n (native-namestring x)))
      (cond
	((asdf::absolute-pathname-p x) n)
	((asdf::os-unix-p) (format nil "./~A" n))
	(t n)))))

(defun lisp-environment-variable-name (&key (type (implementation-type)) prefix suffix)
  (let* ((implementation (get-lisp-implementation type))
         (name (or (lisp-implementation-environment-variable implementation)
                (first (lisp-implementation-identifiers implementation)))))
    (when (eq prefix t) (setf prefix "X"))
    (when (eq suffix t) (setf prefix "_OPTIONS"))
    (format nil "~@[~A~]~:@(~A~)~@[~A~]" prefix name suffix)))

(defun lisp-invocation-arglist (&rest keys &key implementation-type &allow-other-keys)
  (apply 'lisp-implementation-invocation-arglist
         (get-lisp-implementation implementation-type)
         (remove-plist-key :implementation-type keys)))

(defmethod lisp-implementation-invocation-arglist
    ((implementation simple-lisp-implementation)
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
  (declare (ignore console))
  (nest
   (with-slots (name flags disable-debugger load-flag eval-flag
                image-flag default-image image-executable-p standalone-executable
                arguments-end argument-control)
       implementation)
   (let ((implementation-type (lisp-implementation-implementation-type implementation)))
    (unless image-path-p (setf image-path default-image)))
   (append
    (when (or (null image-path) (not image-executable-p))
      (ensure-list
       (or
        (when (consp lisp-path) lisp-path)
        (ensure-path-executable lisp-path)
        (getenvp (lisp-environment-variable-name
                  :type implementation-type
                  :prefix (when cross-compile "X")))
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

(defun lisp-invoker (&optional (implementation-type (implementation-type)))
  (or (lisp-implementation-invoker (get-lisp-implementation implementation-type))
      'invoke-lisp-directly))

(defun invoke-lisp
    (&rest keys
     &key (implementation-type (implementation-type))
       lisp-path
       (lisp-flags :default)
       image-path
       console
       load
       eval
       arguments
       debugger
       cross-compile
       (run-program 'run-program)
       run-program-args)
  (declare (ignore lisp-path lisp-flags image-path console load eval arguments debugger
                   cross-compile run-program run-program-args))
  (apply (lisp-invoker implementation-type)
         keys))

(defun invoke-lisp-directly
    (&rest keys
     &key (implementation-type (implementation-type))
       lisp-path
       (lisp-flags :default)
       image-path
       console
       load
       eval
       arguments
       debugger
       cross-compile
       (run-program 'run-program)
       run-program-args)
  (declare (ignore implementation-type lisp-path lisp-flags image-path
                   console load eval arguments debugger cross-compile))
  (apply run-program
         (apply 'lisp-invocation-arglist (remove-plist-keys '(:run-program :run-program-args) keys))
         run-program-args))

(defun invoke-lisp-via-script
    (&rest keys
     &key implementation-type
       lisp-path
       lisp-flags
       image-path
       console
       load
       eval
       arguments
       debugger
       cross-compile
       (run-program 'run-program)
       run-program-args)
  (declare (ignore implementation-type lisp-path lisp-flags image-path debugger cross-compile
                   console run-program run-program-args))
  (with-temporary-file (:stream s :pathname p :type "lisp")
    (when arguments
      (format s "(unless (find-package :uiop/image) (defpackage :uiop/image (:use :cl)))~%~
                 (defparameter uiop/image::*command-line-arguments* '~S)~%"
              arguments))
    (loop :for l :in (ensure-list load) :do (format s "(cl:load ~S)~%" l))
    (format s "~@[~A~]~%" eval)
    :close-stream
    (apply 'invoke-lisp-directly
           :load (native-namestring p)
           (remove-plist-keys '(:load :eval) keys))))


;;; Avoiding use of a compiled-in driver in the build process

(defun quit-form (&key code (implementation-type (implementation-type)))
  "Returns the correct form to quit lisp, based on the value of lisp-implementation.
Can optionally be given a unix status CODE to exit with"
  (format nil (slot-value (get-lisp-implementation implementation-type) 'quit-format)
	  (or code 0)))

(defun save-image-form (filepath &optional (implementation-type (implementation-type)))
  "Returns the lisp form to save the lisp image to the given filepath"
  (format nil (slot-value (get-lisp-implementation implementation-type) 'dump-format)
	  filepath))
