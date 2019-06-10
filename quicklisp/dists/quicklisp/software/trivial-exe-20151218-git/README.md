# trivial-exe

[![Build Status](https://travis-ci.org/ceramic/trivial-exe.svg?branch=master)](https://travis-ci.org/ceramic/trivial-exe)
[![Coverage Status](https://coveralls.io/repos/ceramic/trivial-exe/badge.svg?branch=master&service=github)](https://coveralls.io/github/ceramic/trivial-exe?branch=master)

Tools for working with executables

# Usage

```lisp
CL-USER> (ql:quickload :trivial-exe)

CL-USER> (trivial-exe:executable-pathname)
#P"/usr/local/bin/sbcl"

CL-USER> (trivial-exe:ensure-executable #p"/path/to/binary")
T
```

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.
