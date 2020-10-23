# Photon: Ontology Based System Extension Framework in Common Lisp 

[![Build Status](https://travis-ci.com/dbym4820/photon.svg?branch=master)](https://travis-ci.com/dbym4820/photon)

## Functions

- Download ontology from Github
- Read and convert ontology into CLOS object
- Build web-server with ontology


## Installation

1. Install Homebrew

```
~ $ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
```

2. Install roswell via Homebrew (or Linuxbrew)

```
# (For Linux: You have to install linuxbrew at first. See [Linuxbrew installation](https://github.com/Linuxbrew))
# Install roswell via home(linux)brew
~ $ brew install roswell

# Ensure path to Photon binary in the Roswell Directory
~ $ echo "export PATH='$(HOME)/.roswell/bin':$PATH" >> ~/.profile
```

3. Install Photon from Github via Roswell

```
# Install photon via roswell
~ $ ros install dbym4820/photon

# install via github
~ $ git clone https://github.com/dbym4820/photon.git
```

4. Initialize Photon

```
~ $ photon init
```


## Usage

## Usage as roswell script

Please go to [this page](./roswell/).



### Editting Ontology by using developed Ontology Editors

- [法造(Hozo) - Ontology Editor](http://www.hozo.jp/download_en.html)



### API for CommonLisp

#### API of Initialization

- Download ontology from github

```
CL-USER> (photon:install "dbym4820/ontologies/japanese-conversation/ontology3")
```

- Environment Initializer (Making .photon directory at home directory)

```
CL-USER> (photon:init)
```

- Converting Ontology

```
# ファイルパスの指定がなければ，デフォルトファイルを参照
CL-USER> (photon:convert-ontology "/path/to/xml")
("whole-root" ....)
```



#### API of Ontology Management

- Show all concepts in converted ontology

```
CL-USER> (photon:show-concepts)
("whole-root" ....)
```

- Show all concepts in converted ontology without instance concepts

```
CL-USER> (photon:show-all-class-concept)
("whole-root" ....)
```

- Show all instanced concepts in converted ontology

```
CL-USER> (photon:show-all-instance)
("some instance" ....)
```

- Get CLOS object of a concept from concept label

```
CL-USER> (photon:find-concept "concept-label")
#<PHOTON.ONTOLOGY::BASIC-CONCEPT #x302001FB00FD>
```

- 概念クラスの属性を調べる（部分概念や属性概念，それらの個数制約や概念名など）

```
CL-USER> (photon:concept-name (photon:parent-concept (photon:find-concept "concept-label")))
#<PHOTON.ONTOLOGY::BASIC-CONCEPT #x302001FB015D>
```


#### API of Web Server

- PhotonのWebサーバーのルーティングを書く

```
CL-USER> (photon:defphoton-route "/:arg" (params) (format nil "~A" (cdr (assoc 'arg :test #'string=))))
```

- PhotonのWebサーバーを立ち上げる・落とす

```
CL-USER> (photon:launch-gui :start-server 5000)
Hunchentoot server is started.
Listening on localhost:5000.

CL-USER> (photon:launch-gui :stop-server)
```



## Copyright

Copyright (c) 2018 Tomoki ABURATANI (aburatanitomoki@gmail.com)

## License

Licensed under the MIT License.
