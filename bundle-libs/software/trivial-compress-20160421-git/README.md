# trivial-compress

[![Build Status](https://travis-ci.org/ceramic/trivial-compress.svg?branch=master)](https://travis-ci.org/ceramic/trivial-compress)

Compress a directory.

# Usage

~~~lisp
(trivial-compress:tar #p"/my/files/" #p"/files.tar")

(trivial-compress:zip #p"/my/files/" #p"/files.zip")
~~~

# License

Copyright (c) 2016 Fernando Borretti

Licensed under the MIT License.
