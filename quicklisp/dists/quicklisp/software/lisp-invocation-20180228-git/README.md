# lisp-invocation

lisp-invocation is a small library for starting Lisp processes from Common Lisp.

lisp-invocation actually only computes the proper command-line invocations;
to actually run a process, you will use
[uiop:run-program](http://cliki.net/uiop),
[inferior-shell:run](http://cliki.net/inferior-shell), or
[external-program:start](http://cliki.net/external-program).


## Usage

A simple example: Starting an SBCL process, running some code and quitting.
Here we use [uiop:run-program](http://cliki.net/uiop) to run the process
and extract its output.

    (let ((impl :sbcl))
      (uiop:run-program
       (lisp-invocation:lisp-invocation-arglist
        :implementation-type impl
        :eval (format nil "(progn (format t ~S '~S) ~A)"
                      "~{~31R~^ ~}~%" '(595756 9556552524 643802 496307950)
                      (lisp-invocation:quit-form :code 0 :implementation-type impl)))
       :output :string))


## Requirements

`lisp-invocation` depends on ASDF 3.1.2 or later for its package-inferred-system,
but you'd want it 3.1.2 anyway for its enhanced uiop:run-program.


## License

Free Software available under an MIT-style license.

Copyright (c) 2008 ITA Software, Inc.  All rights reserved.
