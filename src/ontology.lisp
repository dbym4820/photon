(in-package :cl-user)
(defpackage photon.ontology
  (:use :cl)
  (:import-from :alexandria
		:flatten)
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
	   :val
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
	   :get-restricted-concepts

	   :get-concept-type
	   
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
   (val :initform "" :initarg :val :accessor val)
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
(defun make-concept (concept-name &key (c-type :basic-concept) (class-restriction (make-instance 'basic-concept)) (cardinality 1) (rh-name "") (val ""))
  (cond ((eql c-type :basic-concept)
         (make-instance 'basic-concept :name concept-name))
        ((eql c-type :attribute-concept)
         (make-instance 'attribute-concept :name concept-name :class-restriction class-restriction :cardinality cardinality :role-holder rh-name :val val))
        ((eql c-type :part-of-concept)
         (make-instance 'part-of-concept :name concept-name :class-restriction class-restriction :cardinality cardinality :val val))
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
(defmethod show-attribute ((concept basic-concept) &key (ont *default-ontology*))
  (declare (ignorable ont))
  (format nil
	  "~%Concept name: ~A~%Included properties: ~A~%Instantiation: ~A~%Parent concept: ~A~%Child concepts: ~A~%"
	  (concept-name concept)
	  (or
	   (mapcar #'concept-name
		   (remove-if #'null (property-list concept)))
	   "nothing")
	  (if (instantiation concept) "TRUE" "FALSE")
	  (concept-name (parent-concept concept))
	  (or
	   (mapcar #'concept-name
		   (child-concept-list concept))
	   "nothing")))
(defmethod show-attribute ((concept-string string) &key (ont *default-ontology*))
  (declare (ignorable ont))
  (let ((concept (find-concept concept-string)))
    (format nil
	    "~%Concept name: ~A~%Included properties: ~A~%Instantiation: ~A~%Parent concept: ~A~%Child concepts: ~A~%"
	    (concept-name concept)
	    (or
	     (mapcar #'concept-name
		     (remove-if #'null (property-list concept)))
	     "nothing")
	    (if (instantiation concept) "TRUE" "FALSE")
	    (concept-name (parent-concept concept))
	    (or
	     (mapcar #'concept-name
		     (child-concept-list concept))
	     "nothing"))))
(defmethod show-attribute ((concept non-basic-concept) &key (ont *default-ontology*))
  (declare (ignorable ont))
  (format nil
	  "~%Role name: ~A~%Class restriction: ~A~%Cardinality: ~A~%value: ~A~%"
	  (concept-name concept)
	  (class-restriction concept)
	  (cardinality concept)
	  (val concept)))


#|
概念の確認用API
|#
(defun get-concept-type (concept)
  (let ((concept-class (class-of concept))
	(basic (find-class 'basic-concept))
	(non (find-class 'non-basic-concept)))
    (cond
      ((eq concept-class basic)
       (if (instantiation concept) :instance-concept :basic-concept))
      ((eq concept-class non)
       (cond ((eq (concept-type concept) :attribute-of) :attribute-of)
	     ((eq (concept-type concept) :part-of) :part-of)
	     (t :others)))
      (t :others))))

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


(defun ancestor-list (concept &optional acc)
  "継承系列に属する先祖概念をリスト"
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

(defun get-part-concepts-class-restriction (concept-name-string)
  "基本概念が備える部分概念のクラス制約を取得"
  (mapcar #'class-restriction
	  (remove-if #'null (property-list (find-concept concept-name-string)))))

(defun get-part-concepts-role-name (concept-name-string)
  "基本概念が備える部分概念のロール概念を取得"
  (mapcar #'concept-name
	  (remove-if #'null
		     (property-list (find-concept concept-name-string)))))

(defun get-part-concepts-role-and-restriction (concept-name-string)
  "基本概念が備える「ロール概念ークラス制約」のリストを作成"
  (mapcar #'cons
	  (get-part-concepts-role-name concept-name-string)
	  (get-part-concepts-class-restriction concept-name-string)))

(defun get-part-concept-info ()
  "すべての概念について「基本概念(ロール概念ークラス制約)」のリストを作成"
  (mapcar #'(lambda (d)
	      (cons d (get-part-concepts-role-and-restriction d)))
	  (show-concepts)))

(defun get-single-restricted-concepts (class-restriction-string)
  "単一のクラス制約を持つ部分概念を備えた基本概念を取得する"
  (mapcar #'car
	  (remove-if #'null
		     (mapcar #'(lambda (slots)
				 (when (find class-restriction-string (cdr slots) :key #'cdr :test #'string=)
				   slots))
			     (get-part-concept-info)))))

(defun get-restricted-concepts (&rest class-restriction-string-list)
  "複数のクラス制約を持つ（満たす）部分概念を備えた基本概念を取得する"
  (if (= 1 (length class-restriction-string-list))
      (get-single-restricted-concepts (first class-restriction-string-list))
      (let* ((candidates
	       (loop for c in class-restriction-string-list
		     collect (get-single-restricted-concepts c)))
	     (first-candidate (car candidates))
	     (multi-matched-position
	       (mapcar #'(lambda (x)
			   (not
			    (position nil
				      (mapcar #'(lambda (other-candidate-list)
						  (not (null (position x other-candidate-list :test #'string=))))
					      (cdr candidates)))))
		       first-candidate)))
	(loop for all-matched-p in multi-matched-position
	      for c in first-candidate
	      when all-matched-p
		collect c))))


