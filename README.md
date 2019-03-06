# Photon: Ontology Based System Extension Framework in Common Lisp 

[![Build Status](https://travis-ci.com/dbym4820/photon.svg?branch=master)](https://travis-ci.com/dbym4820/photon)

## What is Photon

オントロジーを基礎としたシステム開発用フレームワーク

## Status

- [X] 法造形式XMLオントロジー
- [ ] XML/RDF形式オントロジー
- [ ] OWL形式オントロジー

## Installation

- まず，Roswellをインストールしてください．

```
# (For Linux: You have to install linuxbrew at first. See [Linuxbrew installation](https://github.com/Linuxbrew))
# Install roswell via home(linux)brew
$ brew install roswell

# Ensure path to Photon binary in the Roswell Directory
$ echo "export PATH='$(HOME)/.roswell/bin':$PATH" >> ~/.profile
```

- 次に，PhotonをRoswell経由もしくはGithubからインストールしてください

```
# Install photon via roswell
~ $ ros install dbym4820/photon

# install via github
~ $ git clone https://github.com/dbym4820/photon.git
```

## Usage

### オントロジーファイルの開発（外部のオントロジ０エディタを利用）

- [法造(Hozo) - オントロジーエディタ](http://www.hozo.jp/download_en.html)

### Common Lisp用API

- Photonの初期化（ホームディレクトリにphoton用のディレクトリを作成）

```
CL-USER> (photon:init)
```

- xml形式のオントロジーをCLOSに変換

```
# ファイルパスの指定がなければ，デフォルトファイルを参照
CL-USER> (photon:convert-concept "/path/to/xml")
("whole-root" ....)
```

- コンバートしたオントロジーに含まれるすべての概念を出力

```
CL-USER> (photon:show-concepts)
("whole-root" ....)
```

- インスタンス以外の概念をすべて出力

```
CL-USER> (photon:show-all-class-concept)
("whole-root" ....)
```

- インスタンス化された概念だけを出力

```
CL-USER> (photon:show-all-instance)
("some instance" ....)
```

- 概念ラベルからCLOS形式に直された概念クラスを取得

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
