(in-package :cl-user)
(defpackage photon.ontology
  (:use :cl)
  (:export :*default-ontology*
           :*default-ontology-file*
	   :set-default-ontology
	   :concept-name
	   :make-ontology
           :make-concept
           :add-concept
           :append-concept
           :clear-ontology
           :show-concepts
           :find-concept
           :find-attribute
           :show-attribute
	   :update-parent-child-concept))
(in-package :photon.ontology)

;;; オントロジー取得元のファイル
(defparameter *default-ontology-file*
  (concatenate 'string
	       (namestring (asdf:system-source-directory 'photon))
	       "src/ontology/anime-ontology.xml"))

;;; 取得元オントロジーファイル設定
(defun set-default-ontology (file-path-string)
  (setf *default-ontology-file* file-path-string))

#|
オントロジーに表れる概念クラスの定義
|#
(defclass concept ()
  ((concept-name :initform "any" :initarg :name :accessor concept-name)))

(defclass basic-concept (concept)
  ((property-list :initform nil :initarg :property :accessor property-list)
   (parent-concept :initform nil :initarg :parent-concept :accessor parent-concept)
   (child-concept-list :initform nil :initarg :child-concept-list :accessor child-concept-list)))

(defclass non-basic-concept (concept)
  ((role-name :initform "new" :initarg :name :accessor concept-name)
   (class-restriction :initform nil :initarg :class-restriction :accessor class-restriction)
   (role-holder :initform "new" :initarg :role-holder :accessor role-holder)
   (cardinality :initarg :cardinality :accessor cardinality)
   (concept-type :initform :part-of :initarg :concept-type :reader concept-type)))

(defclass attribute-concept (non-basic-concept)
  ((concept-type :initform :attribute-of)))

(defclass part-of-concept (non-basic-concept)
  ((concept-type :initform :part-of)))

(defclass relation-concept (concept)
  ((relation-name :initform "new" :initarg :name :accessor concept-name)
   (part-concept-list :initform "new" :initarg :part :accessor part-list)))

(defclass relation-part (part-of-concept)
  ((concept-type :initform :relation-part-of)))

(defclass ontology ()
  ((theme :initform "any" :initarg :ontology-theme :reader theme)
   (concept-list :initform `(,(make-instance 'basic-concept :name "whole-root")) :accessor concept-list)))

#|
オントロジー定義の操作に関するメソッド群
|#
;;; オントロジー定義
(defun make-ontology (&optional (ontology-theme "any"))
  (make-instance 'ontology :ontology-theme ontology-theme))

;;; 概念定義
(defun make-concept (concept-name &key (c-type :basic-concept) (class-restriction (make-instance 'basic-concept)) (cardinality 1) (rh-name ""))
  (cond ((eql c-type :basic-concept)
         (make-instance 'basic-concept :name concept-name))
        ((eql c-type :attribute-concept)
         (make-instance 'attribute-concept :name concept-name :class-restriction class-restriction :cardinality cardinality :role-holder rh-name))
        ((eql c-type :part-of-concept)
         (make-instance 'part-of-concept :name concept-name :class-restriction class-restriction :cardinality cardinality ))
        (t 
         (make-instance 'basic-concept :name concept-name))))

(defgeneric append-concept (concept ontology))

;;; オントロジーに基本概念を挿入
(defmethod append-concept ((new-class basic-concept) (target-ontology ontology))
  (setf (concept-list target-ontology)
        (append
	 (if (consp (concept-list target-ontology))
	     (concept-list target-ontology)
	     `(,(concept-list target-ontology)))
	 (cons new-class nil))))

;;; オントロジーに関係概念を挿入
(defmethod append-concept ((new-relation relation-part) (target-ontology ontology))
  (setf (concept-list target-ontology)
        (append
	 (if (consp (concept-list target-ontology))
	     (concept-list target-ontology)
	     `(,(concept-list target-ontology)))
	 (cons new-relation nil))))

;;; 基本概念にプロパティを挿入
(defmethod append-concept ((new-property non-basic-concept) (target-concept basic-concept))
  (setf (property-list target-concept)
        (append
	 (if (consp (property-list target-concept))
	     (property-list target-concept)
	     `(,(property-list target-concept)))
	 (cons new-property nil))))

;;; 関係概念にプロパティを挿入
(defmethod append-concept ((new-property part-of-concept) (target-relation relation-concept))
  (setf (property-list target-relation)
        (append
	 (if (consp (property-list target-relation))
	     (property-list target-relation)
	     `(,(property-list target-relation)))
	 (cons new-property nil))))

;;; 概念のデータを更新
(defun update-parent-child-concept (concept parent child-list)
  (setf (parent-concept concept) parent
	(child-concept-list concept) child-list))

;;; オントロジーの中身をクリア
(defgeneric clear-ontology (ontology)
  (:method (ontology)
    (setf (concept-list ontology) `(,(make-instance 'basic-concept :name "whole-root")))))

#|
デフォルトのオントロジーセット
|#
;;; デフォルトのオントロジー
(defparameter *default-ontology* (make-instance 'ontology))

#|
CLOSオントロジー操作用API
|#

;;; オントロジーの中身表示
(defun show-ontology (&optional (ont *default-ontology*))
  (concept-list ont))

;;; オントロジーの中身（ラベル）表示
(defun show-concepts (&optional (ont *default-ontology*))
  (mapcar #'concept-name (show-ontology ont)))

;;; 基本概念の追加
(defun add-concept (concept-name &optional (ont *default-ontology*) &key (concept-type :basic-concept))
  (declare (ignorable concept-type))
  (append-concept (make-concept concept-name) ont))

;;; オントロジーの検索
(defun find-concept (concept-name &optional (ont *default-ontology*))
  (find-if #'(lambda (c)
	       (when (string= concept-name (concept-name c)) t))
	   (show-ontology ont)))

;;; 基本概念の属性検索
(defun find-attribute ())

;;; CLOSオントロジーの各パラメータを文字列として表示
(defun show-attribute (attribute concept)
  (if (null concept) nil
      (let ((return-value
	      (cond ((eql attribute :proper)
		     (property-list concept))
		    ((eql attribute :role-name)
		     (concept-name concept))
		    ((eql attribute :class-restriction)
		     (class-restriction concept))
		    ((eql attribute :cardinality)
		     (cardinality concept))
		    ((eql attribute :concept-type)
		     (concept-type concept))
		    ((eql attribute :parent)
		     (parent-concept concept))
		    ((eql attribute :children)
		     (child-concept-list concept))
		    (t
		     nil))))
	(cond ((listp return-value)
	       (remove-if #'null return-value))
	      (t
	       return-value)))))

;;; 第１引数として与えた基本概念が第２引数として与えた基本概念の下位概念かを調べる述語（継承関係の有無を調べる）
;;; 後にCLOSクラスを引数として取るメソッドに変更
(defgeneric concept-inherit-p (source-concept target-concept &key))
(defmethod concept-inherit-p ((source-concept basic-concept) (target-concept basic-concept) &key (ont *default-ontology*) )
  (labels ((rec-pred (sou)
	     (cond ((null sou) nil)
		   ((string= (concept-name sou) (concept-name target-concept)) t)
		   (t (rec-pred (find-concept (show-attribute :parent sou)))))))
    (rec-pred source-concept)))
