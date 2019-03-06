(in-package :cl-user)
(defpackage photon.hozo
  (:use :cl)
  (:import-from :alexandria
                :read-file-into-string)
  (:import-from :photon.ontology
  		:*default-ontology*
                :*default-ontology-file*
		:make-concept
		:find-concept
                :clear-ontology
		:add-concept
                :append-concept
		:show-concepts
		
		:instantiation

		:update-parent-child-concept)
  (:export :convert-ontology-hozo))
(in-package :photon.hozo)

#|
オントロジーのXMLからオブジェクトへのコンバート
|#

;;; オントロジーファイルをXMLリストに変換
(defun convert-ontology-xml (file-path)
  (setf *default-ontology-file* file-path)
  (xmls:parse-to-list (read-file-into-string file-path) :compress-whitespace t))

;;; ファイル名の取得
(defun get-expected-file-name-tags (&optional (xml-file-path *default-ontology-file*))
  (second (second (second (convert-ontology-xml xml-file-path)))))

;;; オントロジーIDの取得
(defun get-ont-id-tag (&optional (xml-file-path *default-ontology-file*))
  (second (first (second (convert-ontology-xml xml-file-path)))))

;;; 概念定義本体の取得
(defun get-w-concept-tags (&optional (xml-file-path *default-ontology-file*))
  (cdr (remove-if #'null (fourth (convert-ontology-xml xml-file-path)))))

;;; リストが示すXMLタグ名を取得
(defun get-tag-name (tag-list)
  (first tag-list))

;;; ConceptXMLのリストが指定したタグのものかを判別
(defun tag-p (tag-string target-list)
  (when (and (listp target-list) (stringp (car target-list)))
    (when (and (string= tag-string (get-tag-name target-list)))
      t)))

;;; Conceptを表す塊のリストを取得
(defun get-concept-tags (&optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (mapcar #'(lambda (tag-list)
			 (when (string= (get-tag-name tag-list) "CONCEPT")
			   (cdr tag-list)))
		     (get-w-concept-tags xml-file-path))))

;;; 各Conceptのラベル（名前）リストを取得
(defun get-concept-label (&optional (xml-file-path *default-ontology-file*))
  (mapcar #'(lambda (concept-tag-list)
	      (second (let ((concept-info
			      (mapcar #'(lambda (tag-list)
					  (when (tag-p "LABEL" tag-list)
					    tag-list))
				      concept-tag-list)))
			(remove-if #'null (second concept-info)))))
	  (get-concept-tags xml-file-path)))

;;; 概念定義の先頭からの出現番号
(defun get-concept-position-from-ahead (concept-name &optional (xml-file-path *default-ontology-file*))
  (position concept-name (get-concept-label xml-file-path) :test #'string=))

;;; 特定概念の塊リストを取り出す
(defun get-specific-concept-tags (concept-name &optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (nth (get-concept-position-from-ahead concept-name xml-file-path) (get-concept-tags xml-file-path))))

;;; 部分/属性概念のリストを取得
(defun get-concept-slot-tags (concept-name &optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (mapcar #'(lambda (slot)
			 (when (tag-p "SLOT" slot)
			   slot))
		     (find-if #'(lambda (concept-tag)
				  (when (tag-p "SLOTS" concept-tag) t))
			      (get-specific-concept-tags concept-name xml-file-path)))))

;;; 概念がインスタンスであるか否かを取得
(defun instance-concept-p (concept &optional (xml-file-path *default-ontology-file*))
  (equalp "true" (second (assoc "instantiation" (car (get-specific-concept-tags concept xml-file-path)) :test #'equalp))))

;;; 指定基本概念のスロット内の1属性を抽出
(defun get-attribute-from-slot-tags (concept-name attribute &optional (xml-file-path *default-ontology-file*))
  (let ((concept-tags (get-concept-slot-tags concept-name xml-file-path)))
    (labels ((tmp-func (att)
               (find-if #'(lambda (attr)
                            (when (tag-p attribute attr)
                              t))
                        (second att))))
       (mapcar #'(lambda (slot-tag)
		   (tmp-func slot-tag))
	       concept-tags))))

;;; 指定された基本概念が持つスロット情報を取得
(defun get-slot-tags (concept-name &optional (xml-file-path *default-ontology-file*))
  (mapcar #'list
          (get-attribute-from-slot-tags concept-name "role" xml-file-path)
          (get-attribute-from-slot-tags concept-name "kind" xml-file-path)
          (get-attribute-from-slot-tags concept-name "class_constraint" xml-file-path)
          (get-attribute-from-slot-tags concept-name "rh_name" xml-file-path)
          (get-attribute-from-slot-tags concept-name "num" xml-file-path)
	  (get-attribute-from-slot-tags concept-name "value" xml-file-path)))


#|
オントロジーのCLOSへのコンバート
|#
(defun convert-ontology-hozo (&key (file-path *default-ontology-file*) (ont *default-ontology*) (update t))
  (if update
      (progn
        (clear-ontology ont)
	(format t "Converting Basic concepts...~%")
        (convert-basic-concept file-path ont)
	(format t "Converting IS-A relations...~%")
        (convert-isa-relation file-path ont)
	(format t "Converting Part/Attribute concepts...~%")
        (convert-part-attribute-concept file-path ont)
	(format t "Converting Instance concepts...~%")
	(convert-instantiation file-path ont)
	(format t "Finalize...~%")
        (show-concepts ont))
      file-path))

;;; 基本概念の変換
(defun convert-basic-concept (&optional (xml-file-path *default-ontology-file*) (ont *default-ontology*))
  (let ((c-list (get-concept-label xml-file-path)))
    (loop for c in c-list
	  do (add-concept c ont :concept-type :basic-concept)
	  finally (return
		    (format nil "~A" (show-concepts ont))))))


;;; is-a関係の変換
(defun convert-isa-relation (&optional (xml-file-path *default-ontology-file*) (ont *default-ontology*))
  (loop for c in (show-concepts)
	do (let ((parent-concept-label (or
					(first (get-parent-concept c xml-file-path))
					"whole-root"))
		 (child-concept-labels (or (get-child-concept c xml-file-path)
					   (list))))
	     (update-parent-child-concept
	      (find-concept c ont)
	      (find-concept parent-concept-label ont)
	      (mapcar #'(lambda (c2)
			  (find-concept c2 ont))
		      child-concept-labels)))))

;;; instanceかどうかをチェック
(defun convert-instantiation (&optional (xml-file-path *default-ontology-file*) (ont *default-ontology*))
  (loop for c in (remove-if #'(lambda (c-s) (string= c-s "whole-root")) (show-concepts))
	do (setf (instantiation (find-concept c ont))
		 (instance-concept-p c xml-file-path))))

;;; 部分/属性概念の変換
(defun convert-part-attribute-concept (&optional (xml-file-path *default-ontology-file*) (ont *default-ontology*))
  (let ((c-list (get-concept-label xml-file-path)))
    (loop for c in c-list ;; c mean anime title string
          do (map 'list #'(lambda (slot)
			    (remove-if #'null
				       (let ((role-name (princ-to-string (second (assoc "role" slot :test #'string=))))
					     (class-const (princ-to-string (second (assoc "class_constraint" slot :test #'string=))))
					     (rh-name (princ-to-string (second (assoc "rh_name" slot :test #'string=))))
					     (cardinality (format nil "~A" (second (assoc "num" slot :test #'string=))))
					     (val (format nil "~A" (second (assoc "value" slot :test #'string=)))))
					 (append-concept
					  (make-concept role-name :c-type :part-of-concept
								  :class-restriction class-const
								  :cardinality cardinality
								  :rh-name rh-name
								  :val val)
					  (find-concept c ont)))))
		  (get-slot-tags c))
          finally (format nil "~A" (show-concepts ont)))))

;;; XMLリストから基本概念の親子関係を取得
(defun get-child-parent (&optional (xml-file-path *default-ontology-file*))
  (mapcar #'(lambda (child-parent-id-list)
	      (let ((child-concept-label (second (first (first child-parent-id-list))))
		    (parent-concept-label (second (second (first child-parent-id-list)))))
	        (cons child-concept-label parent-concept-label)))
	  (remove-if #'null
		     (mapcar #'(lambda (tag-list)
				 (when (string= (get-tag-name tag-list) "ISA")
				   (cdr tag-list)))
			     (get-w-concept-tags xml-file-path)))))

;;; 親概念名を基に子概念を取得
(defun get-child-concept (concept-name &optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (mapcar #'(lambda (child-parent)
			 (when (string= (cdr child-parent) concept-name)
			   (car child-parent)))
		     (get-child-parent xml-file-path))))

;;; 子概念名を基に親概念を取得
(defun get-parent-concept (concept-name &optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (mapcar #'(lambda (child-parent)
			 (when (string= (car child-parent) concept-name)
			   (cdr child-parent)))
		     (get-child-parent xml-file-path))))

