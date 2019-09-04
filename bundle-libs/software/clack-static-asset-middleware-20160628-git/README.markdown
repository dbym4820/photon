# Clack-Static-Asset-Middleware
[![Build Status](https://travis-ci.org/fisxoj/clack-static-asset-middleware.svg?branch=master)](https://travis-ci.org/fisxoj/clack-static-asset-middleware) [![Coverage Status](https://coveralls.io/repos/github/fisxoj/clack-static-asset-middleware/badge.svg?branch=master)](https://coveralls.io/github/fisxoj/clack-static-asset-middleware?branch=master)[![Quicklisp](http://quickdocs.org/badge/clack-static-asset-middleware.svg)](http://quickdocs.org/clack-static-asset-middleware/)

The static asset middleware is for handling versioned static assets.  That means that, when the assets of your webapp change, it will be referred to by a different filename, allowing a browser to instantly know that it needs to download a new file.  Assets are served with the maximum cache time headers set so browsers will never ask for them again.

It comes with the `clack-static-asset-djula-helpers` package which extends the djula templating language to allow inserting cache busted urls for your files easily.

## Usage

Use by wrapping your clack app in `clack-static-asset-middleware:*clack-static-asset-middleware*`.

```lisp
(funcall clack-static-asset-middleware:*clack-static-asset-middleware*
                      clack-app
		      :root (asdf:system-relative-pathname :webapp #p"static/")
		      :path "static/")
```

It can be configured by specifying:

- `path`: the path on your app that you want your assets to be served from.  That is `https://my-webapp.com/{path}/images/some-sheep_12..532.png`.
- `root`: The root directory that your assets reside in on the filesystem.
- `cache-buster-function`: A function of two arguments `(pathname cache-string)` that generates an appropriate, cache-busted name for a file.  The default creates filenames like `images/some-sheep_12421345423fea543265cf43226512a6.png`.
- `cache-unbuster-function`: A function that takes a cache-busted url and resolves it to the un-busted filename.
- `filter-function`: A function that identifies files that should not be served but are in the `root` directory anyway.  It acceps a pathname and return a boolean where `t` means 'do not serve' and `nil` means the file is good to serve.  By default, it blocks files beginnig with a `.`, which would be hidden on a UNIX filesystem.


Once you have done that, you can call `busted-uri-for-path` to convert a relative path on your system into an appropriate cache busted url suitable for inserting into templating and showing your friends.  There is a helper package for accessing this functionality within the [Djula](https://github.com/mmontone/) templating language below.

### Real-World Usage

This middleware is not optimized to serve files.  It should not be particularly slow, but it was not designed to be your CDN.  In real-world situations, you should consider putting a CDN or nginx proxy in front of your app for best performance.  I have not done this, yet, but it's probably something I should work on.

Ideally, you might set this up behind an nginx reverse proxy and let it handle un-busting your URLs like so:

```
server {
  # ...

  location ~* ^/<YOUR PATH HERE>/(\w+)/([^/]+)_\d+\.(js|css|png|jpg|jpeg|gif|ico)$ {
    alias <YOUR ROOT HERE>/$1/$2.$3;
    add_header Vary Accept-Encoding;
    expires max;
  }

  # ...
}
```
Example taken from the blog of [Ben Ripkens](http://blog.bripkens.de/2012/03/nginx-cache-busting/).

## Installation

`clack-static-asset-middleware` is not yet in quicklisp, so clone it into your quicklisp `local-projects` directory. Then, run

```lisp
(ql:quickload :clack-static-asset-middleware)
```

or refer to it in your system definition

```lisp
(asdf:defsystem my-great-webapp
    ...
    :depends-on (#:clack-static-asset-middleware)
    ..
    )
```

## Template Helpers

Right now, there is a helper package, `clack-static-asset-djula-helpers`, which provides extensions to the [Djula](https://github.com/mmontone/) templating language for inserting busted URLs into templates.  Once you've loaded the system, there will be two new djula tags available.

- `asset-path`: Inserts a busted url to the given path.
  ```
  <img src="{% asset-path "images/gustywinds.jpg" %}" /> => <img src="/static/images/gustywinds_423534...a3.jpg" />
  ```
- `stylesheet-tag`: Inserts a busted url conveniently inside a stylesheet tag.
  ```
  {% stylesheet-tag "styles/cool.css" %} => <link rel="stylesheet" href="/static/styles/cool_a05b6...878f.css">
  ```

## Author

* Matt Novenstern (fisxoj@gmail.com)

## Copyright

Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)

## License

Licensed under the MIT License.
