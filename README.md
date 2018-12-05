# Photon: Ontology Based Syystem Extension Framework in Common Lisp 

[![Build Status](https://travis-ci.org/dbym4820/photon.svg?branch=master)](https://travis-ci.org/dbym4820/photon)

## What is Photon

I intend to develop this framework become a function implement framework based on Ontology in Common Lisp

## Status

- [X] For 法造形式オントロジー
- [ ] XML/RDF形式オントロジー
- [ ] OWL形式オントロジー

## Installation

You need to install roswell at first.

```
# (For Linux: You have to install linuxbrew at first. See [Linuxbrew installation](https://github.com/Linuxbrew))
# Install roswell via home(linux)brew
$ brew install roswell

# Install photon via roswell
$ ros install dbym4820/photon

# Ensure path to Photon binary in the Roswell Directory
$ echo "export PATH='$(HOME)/.roswell/bin':$PATH" >> ~/.profile
```

## Usage

### As Roswell script

- show all concepts (--file or -f option)

```
$ photon -f /path/to/ontology/file.xml
Any, アニメ, TVアニメ, アニメ映画, Webアニメ, キャラクター, 女性キャラクター, 男性キャラクター, 実在物, 非実在物, 概念的存在, 実際的存在, 生物, 非生物, 哺乳類, 鳥類, 爬虫類, 人間, 物語, 伝記, 神話, NEW, 擬人化キャラクター, その他のキャラクター
```

- show all concepts as list (--list or -l option)

```
$ photon -l /path/to/ontology/file.xml
Any
アニメ
TVアニメ
アニメ映画
Webアニメ
キャラクター
...
```

- get parent/super concept (--parent or -p option)

```
$ photon -p アニメ /path/to/ontology/file.xml
実在物
```

- get child/sub concepts 

```
$ photon -c アニメ /path/to/ontology/file.xml
TVアニメ
アニメ映画
Webアニメ
```

- show concept conditions (--attribute or -r option)

```
$ photon -r アニメ /path/to/ontology/file.xml
Concept name:   'アニメ'
Property list:  '作品名[string].(1..)', '放送年度[年度].(1..)', '流通形態[メディア].(1..)', '放送の長さ[時間].(1..)', 'シナリオ[シナリオ].(1)', '制作陣role[制作].(1)', '想定対象者[人間].(1)'
Parent concept: '実在物'
Child concepts: 'TVアニメ', 'アニメ映画', 'Webアニメ'
```

### As Common Lisp library

In preparation


## Copyright

Copyright (c) 2018 Tomoki ABURATANI (aburatanitomoki@gmail.com)

## License

Licensed under the MIT License.
