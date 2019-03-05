(in-package :cl-user)
(defpackage photon.ontology
  (:use :cl)
  (:export :*default-ontology*
           :*default-ontology-file*
	   :set-default-ontology

	   :concept-name
	   :property-list
	   :child-concept-list
	   :instantiation
	   :parent-concept

	   :role-name
	   :class-restriction
	   :role-holder
	   :cardinality
	   :concept-type

	   :make-ontology
           :make-concept
           :add-concept
           :append-concept
           :clear-ontology

           :show-concepts
	   :show-all-class-concept
	   :show-all-instance
	   :find-concept
           :find-attribute
           :show-attribute
	   :get-restricted-concept

	   :update-parent-child-concept

	   :same-concept-p
	   :ancestor-list
	   :ancestor-p
	   :parent-p
	   ))
(in-package :photon.ontology)

;;; オントロジー取得元のファイル
(defparameter *default-ontology-file*
  (concatenate 'string
	       (namestring (asdf:system-source-directory 'photon))
	       "src/ontology/sample-ontology.xml"))

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
   (child-concept-list :initform nil :initarg :child-concept-list :accessor child-concept-list)
   (instantiation :initform nil :initarg :instantiation :accessor instantiation)))

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

(defun show-concepts (&optional (ont *default-ontology*))
  "オントロジーの中身（ラベル）をすべて表示"
  (mapcar #'concept-name (show-ontology ont)))

(defun show-all-class-concept (&optional (ont *default-ontology*))
  "インスタンスではない概念の検索"
  (mapcar #'concept-name
	  (remove-if #'instantiation
		     (mapcar #'(lambda (c)
				 (find-concept c ont))
			     (show-concepts)))))

(defun show-all-instance (&optional (ont *default-ontology*))
  "インスタンスの検索"
  (mapcar #'concept-name
	  (remove-if-not #'instantiation
			 (mapcar #'(lambda (c)
				     (find-concept c ont))
				 (show-concepts)))))

;;; 基本概念の追加
(defun add-concept (concept-name &optional (ont *default-ontology*) &key (concept-type :basic-concept))
  (declare (ignorable concept-type))
  (append-concept (make-concept concept-name) ont))

#|
オントロジー検索用API
|#
;;; オントロジーの検索
(defun find-concept (concept-name &optional (ont *default-ontology*))
  (find-if #'(lambda (c)
	       (when (string= concept-name (concept-name c)) t))
	   (show-ontology ont)))


;;; CLOSオントロジーの各パラメータを文字列として表示
(defgeneric show-attributes (concept &key))
(defmethod show-attribute ((concept concept) &key (ont *default-ontology*))
  (declare (ignorable ont))
  (format nil
	  (concatenate 'string
		       "Included properties: ~A~%"
		       "Role name: ~A~%"
		       "Class restriction: ~A~%"
		       "Cardinalities: ~A~%"
		       "Parent concept: ~A~%"
		       "Child concepts: ~A~%")
	  (property-list concept)
	  (concept-name concept)
	  (class-restriction concept)
	  (cardinality concept)
	  (parent-concept concept)
	  (child-concept-list concept)))
(defmethod show-attribute ((concept string) &key (ont *default-ontology*))
  (declare (ignorable ont))
  (let ((concept-object (find-concept concept)))
    (unless concept-object
      (format nil
	      (concatenate 'string
			   "Included properties: ~A~%"
			   "Role name: ~A~%"
			   "Class restriction: ~A~%"
			   "Cardinalities: ~A~%"
			   "Parent concept: ~A~%"
			   "Child concepts: ~A~%")
	      (mapcar #'(lambda (c)
			  (unless c
			    (concept-name c)))
		      (property-list concept-object))
	      (concept-name concept-object)
	      (class-restriction concept-object)
	      (cardinality concept-object)
	      (parent-concept concept-object)
	      (child-concept-list concept-object)))))

#|
概念の関係性検索に関する関数群
|#

;;; 第1引数と第2っ引数の概念が同じ概念かどうかを評価
(defgeneric same-concept-p (concept1 concept2)
  (:method ((concept1 concept) (concept2 concept))
      (eq concept1 concept2)))

;;; 第1引数の概念が第2引数の概念の子概念かどうかを判定
(defgeneric parent-p (concept1 concept2))
(defmethod parent-p ((parent-candidate concept) (child-candidate concept))
  (eq (parent-concept child-candidate) parent-candidate))
(defmethod parent-p ((parent-candidate-string string) (child-candidate-string string))
  (let ((parent-concept (find-concept parent-candidate-string))
	(child-concept (find-concept child-candidate-string)))
    (eq parent-concept (parent-concept child-concept))))

;;; 継承系列に属する先祖概念をリスト
(defun ancestor-list (concept &optional acc)
  (cond ((null concept) concept)
	((eq (parent-concept concept) (find-concept "whole-root")) (append acc (list concept)))
	(t
	 (ancestor-list (parent-concept concept) (append acc (list concept))))))

;;; 第1引数の概念が第2引数の概念の先祖概念であるかを評価
(defgeneric ancestor-p (concept1 concept2))
(defmethod ancestor-p ((ancestor-candidate concept) (descendant-candidate concept))
  (when (find ancestor-candidate (ancestor-list descendant-candidate))
    t))
(defmethod ancestor-p ((ancestor-candidate string) (descendant-candidate string))
  (when (find (find-concept ancestor-candidate)
	      (ancestor-list (find-concept descendant-candidate)))
    t))




(defun get-all-part-concept-restriction-and-role ()
  "全ての基本概念について部分概念を取り出し，それらのクラス制約とロール概念名を取得する"
  (mapcar #'(lambda (d)
	      (cons d (get-part-concepts-role-rest d)))
	  (show-concepts)))

(defun get-restricted-concept (class-restriction)
  "特定のクラス制約を持つ部分概念を備えた基本概念を取得する"
  (let* ((c-list (get-part-concept-info))
	 (c-list-which-has-part-concept
	   (mapcar #'cdr c-list)))
    (remove-if #'null
	       (mapcar #'car 
		       (mapcar #'(lambda (d1 d2)
				   (when d2 (mapcar #'(lambda (d3)
							(when (string=
							       class-restriction
							       (cdr d3))
							  d1))
						    (cdr d1))))
			       c-list
			       c-list-which-has-part-concept)))))
	
