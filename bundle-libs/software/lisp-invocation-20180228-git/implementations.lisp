(defpackage :lisp-invocation/implementations
  (:use :common-lisp :uiop :lisp-invocation/lisp-invocation))
(in-package :lisp-invocation/implementations)

#|
Supported test lisp implementations include:
   allegro (acl), abcl, ccl (clozure), clasp, clisp, cmucl (cmu),
   ecl, ecl_bytecodes, gcl, lispworks, mkcl, sbcl, scl, xcl.

Not really supported are:
   corman (cormanlisp), lispworks-personal-edition

Not supported at all are:
   mcl (rmcl), genera
|#

(define-lisp-implementation :abcl ()
  :fullname "Armed Bear Common Lisp"
  :name "abcl"
  :feature :abcl
  :flags ("--noinform" "--noinit" "--nosystem")
  :eval-flag "--eval"
  :load-flag "--load"
  :arguments-end "--"
  :image-flag nil
  :image-executable-p t
  :standalone-executable nil
  :argument-control t
  :disable-debugger ("--batch") ;; ???
  :quit-format "(ext:quit :status ~A)"
  :dump-format nil)

(define-lisp-implementation (:allegro :acl) ()
  :fullname "Allegro CL"
  :name "alisp" ;; on Windows, you might prefer the Win32 console application build.exe, which seems to be an mlisp variant ("modern" syntax)
  :feature :allegro
  :flags ("-qq") ; -q only ? on windows, +c ? On Allegro 5 and earlier, -Q and/or -QQ ?
  :eval-flag "-e"
  :load-flag "-L"
  ; :quit-flags ("-kill")
  :arguments-end "--"
  :image-flag "-I"
  :image-executable-p nil
  :standalone-executable nil
  :argument-control t
  :disable-debugger ("-batch") ; see also -#D -#C -#!
  :quit-format "(excl:exit ~A :quiet t)"
  :dump-format "(progn (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) (excl:dumplisp :name ~A :suppress-allegro-cl-banner t))")

(define-lisp-implementation (:ccl :clozure) () ;; demand 1.4 or later.
  :fullname "Clozure Common Lisp"
  ;; formerly OpenMCL, forked from MCL, formerly Macintosh Common Lisp, nee Coral Common Lisp
  ;; Random note: (finish-output) is essential for ccl, that won't do it by default,
  ;; unlike the other lisp implementations tested.
  :name "ccl"
  :feature :clozure
  :flags ("--no-init" "--quiet")
  :eval-flag "--eval" ; -e
  :load-flag "--load"
  :image-flag "--image-name" ; -I
  :image-executable-p t
  :standalone-executable t
  :arguments-end "--"
  :argument-control t ;; must be fixed now, but double-checking needed.
  :disable-debugger ("--batch")
  :directory-variable "CCL_DEFAULT_DIRECTORY"
  :quit-format "(let ((x ~A)) (finish-output *standard-output*) (finish-output *error-output*) (ccl:quit x))"
  :dump-format "(save-application ~S :prepend-kernel t)")

(define-lisp-implementation :clasp ()
  :fullname "CLASP"
  :name "clasp"
  :feature :clasp
  :flags ("--norc" "--noinit")
  :eval-flag "--eval"
  :load-flag "--load" ;; ???
  :image-flag nil
  :image-executable-p t
  :arguments-end "--"
  :argument-control t ;; must be fixed now, but double-checking needed.
  :disable-debugger ()
  :quit-format "(si:quit ~A)"
  :dump-format nil) ;; Cannot dump with CLASP. Link instead.

(define-lisp-implementation :clisp ()
  :fullname "GNU CLISP"
  :name "clisp"
  :feature :clisp
  :flags ("-norc" "--quiet" "--quiet" "-ansi") ;; don't use -I, for it induces extra prompt outputs.
  :eval-flag "-x"
  :load-flag "-i"
  :arguments-end "--"
  :image-executable-p t
  :image-flag "-M"
  :standalone-executable t ;; requires clisp 2.48 or later
  :argument-control t ;; *BUT* even a standalone-executable always accepts --clisp-x and such.
  :disable-debugger ("-on-error" "exit") ;; otherwise, -on-error debug
  :quit-format "(ext:quit ~A)"
  :dump-format "(ext:saveinitmem ~S :quiet t :executable t)")

(define-lisp-implementation (:cmucl :cmu) ()
  :fullname "CMU CL"
  :name "cmucl"
  :feature :cmu
  :flags ("-quiet" "-noinit")
  :eval-flag "-eval"
  :load-flag "-load"
  :arguments-end "--"
  :image-executable-p t
  :image-flag "-core"
  :argument-control t
  :disable-debugger ("-batch")
  :quit-format "(unix:unix-exit ~A)"
  :dump-format "(extensions:save-lisp ~S :executable t)")

(define-lisp-implementation (:corman :cormanlisp) () ;; someone please add more complete support
  :fullname "Corman Lisp"
  :name () ;; There's a clconsole.exe, but what are the options?
  :feature :cormanlisp
  :quit-format "(win:exitprocess ~A)")

(define-lisp-implementation :ecl () ;; demand 10.4.2 or later.
  :fullname "Embeddable Common-Lisp"
  :name "ecl"
  :feature :ecl
  :flags ("-norc")
  :eval-flag "-eval" ; -e
  :load-flag "-load"
  :image-flag nil
  :image-executable-p t
  :arguments-end "--"
  :argument-control t ;; must be fixed now, but double-checking needed.
  :disable-debugger ()
  :quit-format "(si:quit ~A)"
  :dump-format nil) ;; Cannot dump with ECL. Link instead.

(define-lisp-implementation :ecl_bytecodes () ;; ECL using its bytecode compiler.
  :fullname "Embeddable Common-Lisp (using bytecodes compiler)"
  :flags ("-norc" "-eval" "(ext::install-bytecodes-compiler)")
  :name "ecl"
  :feature (:and :ecl :ecl-bytecmp)
  :environment-variable "ECL"
  :eval-flag "-eval" ; -e
  :load-flag "-load"
  :image-flag nil
  :image-executable-p t
  :arguments-end "--"
  :argument-control t ;; must be fixed now, but double-checking needed.
  :disable-debugger ()
  :quit-format "(si:quit ~A)"
  :dump-format nil) ;; Cannot dump with ECL. Link instead.

(define-lisp-implementation :gcl () ;; Demand 2.8.0, if it is ever released. In ANSI mode.
  :fullname "GNU Common Lisp"
  :name "gcl" ;; On debian, we might have to export GCL_ANSI=t to ensure the ANSI variant is used.
  :feature :gcl
  :flags ()
  :eval-flag "-eval" ; -e
  :load-flag "-load"
  :image-flag nil
  :image-executable-p t
  :arguments-end "--" ;; -f ?
  :disable-debugger ("-batch")
  :quit-format "(lisp:quit ~A)"
  :dump-format "(progn (si::set-hole-size 500) (si::gbc nil) (si::sgc-on t) (si::save-system ~A))")

(define-lisp-implementation (:lispworks :lw) ()
  :fullname "LispWorks"
  :name "lispworks-console" ;; This assumes you dumped a proper image for batch processing...
  ;; If you have a licensed copy of lispworks,
  ;; you can obtain the "lispworks" binary with, e.g.
  ;; echo '(hcl:save-image "lispworks-console" :environment nil)' > /tmp/build.lisp ;
  ;; ./lispworks-7-0-0-x86-linux -siteinit - -init - -build /tmp/build.lisp
  ;; Note that you also need to copy the license file to
  ;; .../lispworks/lib/7-0-0-0/config/lwlicense
  ;; and/or the same directory as your binary,
  ;; for it to work on dumped binaries in all locations, with, e.g.
  ;; (system::copy-file ".../lwlicense" (make-pathname :name "lwlicense" :type nil :defaults filename))
  :feature :lispworks
  :flags ("-site-init" "-" "-init" "-")
  ;; As of 7.0.0, LispWorks (still) fails to stop processing arguments with "--" or any marker.
  ;; http://www.lispworks.com/documentation/lw70/LW/html/lw-203.htm
  ;; Therefore we can't "just" tuck arguments at the end of a command-line, and instead we use
  ;; exec_lisp_file to create a script that initializes arguments and pass that to LispWorks.
  ;; Since we don't use -eval, we use -build instead of -load to load the script. LispWorks
  ;; calls all the -eval and -load in order, then the -siteinit, -init and finally -build.
  ;; Note that we don't use -build, and so if you don't quit as part of your -eval and -load,
  ;; then LispWorks will load the site and user init files then start graphical environment.
  ;; This is probably not what a portable Lisp program using lisp-invocation wants;
  ;; but then again, such program is responsible for quitting as part of eval and load forms.
  :eval-flag "-eval" :load-flag "-load"
  :arguments-end nil ;; Unhappily, there is no end of arguments marker for LispWorks,
  :invoker invoke-lisp-via-script ;; so we use this invoker kludge.
  :image-flag nil
  :image-executable-p t
  :standalone-executable t
  :argument-control t
  :disable-debugger ()
  :quit-format "(lispworks:quit :status ~A :confirm nil :return nil :ignore-errors-p t)"
  :dump-format "(lispworks:deliver 'xcvb-driver:resume ~A 0 :interface nil)") ; "(hcl:save-image ~A :environment nil)"

(define-lisp-implementation :lispworks-personal ()
  :fullname "LispWorks Personal Edition"
  :name () ;; In LispWorks Personal, the slave worker executes you!
  :feature :lispworks-personal-edition)

(define-lisp-implementation :mkcl ()
  :fullname "ManKai Common-Lisp"
  :name "mkcl"
  :feature :mkcl
  :flags ("-norc")
  :eval-flag "-eval" ; -e
  :load-flag "-load"
  :image-flag nil
  :image-executable-p t
  :arguments-end "--"
  :argument-control t ;; must be fixed now, but double-checking needed.
  :disable-debugger ()
  :quit-format "(mk-ext:quit :exit-code ~A)"
  :dump-format nil) ;; Cannot dump with ECL. Link instead.

(define-lisp-implementation :sbcl ()
  :fullname "Steel Bank Common Lisp"
  :name "sbcl"
  :feature :sbcl
  :flags ("--noinform" "--no-userinit" "--no-sysinit") ;; minimize non-determinism form user's env
  :eval-flag "--eval" ;; Note: SBCL's eval can only handle one form per argument.
  :load-flag "--load"
  :arguments-end "--end-toplevel-options"
  :image-flag "--core"
  :image-executable-p t
  :standalone-executable t ;; requires sbcl 1.0.21.24 or later.
  :argument-control t
  :disable-debugger ("--disable-debugger")
  :directory-variable "SBCL_HOME"
  :quit-format "(let ((exit (find-symbol \"EXIT\" :sb-ext)) (quit (find-symbol \"QUIT\" :sb-ext)) (code ~A)) (cond (exit (funcall exit :code code)) (quit (funcall quit :unix-status code))))"
  :dump-format "(sb-ext:save-lisp-and-die ~S :executable t)")

(define-lisp-implementation :scl ()
  :fullname "Scieneer Common Lisp" ; use 1.3.9 or later
  :name "scl"
  :feature :scl
  :flags ("-quiet" "-noinit")
  :eval-flag "-eval"
  :load-flag "-load"
  :arguments-end "--"
  :image-flag "-core"
  :argument-control nil ;; cmucl will always scan all the arguments for -eval... EVIL!
  :disable-debugger ("-batch")
  :quit-format "(unix:unix-exit ~A)"
  :dump-format "(extensions:save-lisp ~S)")

(define-lisp-implementation :xcl ()
  :fullname "XCL"
  :name "xcl"
  :feature :xcl
  :flags ("--no-userinit" "--no-siteinit" "--noinform")
  :eval-flag "--eval"
  :load-flag "--load"
  :arguments-end "--"
  :image-flag nil
  :image-executable-p nil
  :standalone-executable nil
  :disable-debugger ()
  :quit-format "(ext:quit :status ~A)"
  :dump-format nil)

