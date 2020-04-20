# Photon: Ontology Based System Extension Framework in Common Lisp 

[![Build Status](https://travis-ci.com/dbym4820/photon.svg?branch=master)](https://travis-ci.com/dbym4820/photon)

## Issue

- 空白のあるオントロジーへの対応が辛い

## Functions you are able to carry out by using PHOTON

- Download ontology from Github
- Read and convert ontology into CLOS object
- 

## Status

### Ontology Downloader
- [X] From Github

### Ontology Converter from Ontology into CLOS
- [X] Hozo formatted XML ontology
- [ ] XML/RDF-form ontology
- [ ] OWL-form ontology

## Installation

1. Install roswell via Homebrew (or Linuxbrew)

```
# (For Linux: You have to install linuxbrew at first. See [Linuxbrew installation](https://github.com/Linuxbrew))
# Install roswell via home(linux)brew
$ brew install roswell

# Ensure path to Photon binary in the Roswell Directory
$ echo "export PATH='$(HOME)/.roswell/bin':$PATH" >> ~/.profile
```

2. Install Photon from Github via Roswell

```
# Install photon via roswell
~ $ ros install dbym4820/photon

# install via github
~ $ git clone https://github.com/dbym4820/photon.git
```

## Usage

## Usage as roswell script

Please go to [this page](./roswell/).

### Editting Ontology by using developed Ontology Editors

- [法造(Hozo) - Ontology Editor](http://www.hozo.jp/download_en.html)

### API for CommonLisp

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

## Copyright

Copyright (c) 2018 Tomoki ABURATANI (aburatanitomoki@gmail.com)

## License

Licensed under the MIT License.
