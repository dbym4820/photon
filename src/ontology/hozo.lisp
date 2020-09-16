(in-package :cl-user)
(defpackage photon.hozo
  (:use :cl)
  (:import-from :alexandria
                :read-file-into-string
		:write-string-into-file)
  (:import-from :split-sequence
		:split-sequence)
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
;; (defun get-concept-position-from-ahead (concept-name &optional (xml-file-path *default-ontology-file*))
;;   (position concept-name (get-concept-label xml-file-path) :test #'string=))
(defun get-concept-position-from-ahead (concept-name concept-list)
  (position concept-name concept-list :test #'string=))


;;; 特定概念の塊リストを取り出す
(defun get-specific-concept-tags (concept-name concept-list &optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (nth
	      (get-concept-position-from-ahead concept-name concept-list)
	      (get-concept-tags xml-file-path))))
  
;;; 部分/属性概念のリストを取得
(defun get-concept-slot-tags (concept-name concept-list &optional (xml-file-path *default-ontology-file*))
  (remove-if #'null
	     (mapcar #'(lambda (slot)
			 (when (tag-p "SLOT" slot)
			   slot))
		     (find-if #'(lambda (concept-tag)
				  (when (tag-p "SLOTS" concept-tag) t))
			      (get-specific-concept-tags concept-name concept-list xml-file-path)))))

;;; 概念がインスタンスであるか否かを取得
(defun instance-concept-p (concept-name concept-list &optional (xml-file-path *default-ontology-file*))
  (equalp "true"
	  (second
	   (assoc "instantiation"
		  (car (get-specific-concept-tags concept-name concept-list xml-file-path))
		  :test #'equalp))))

;;; 指定したラベルを持つConceptのIDを取得
(defun get-concept-id (concept-name concept-list &optional (xml-file-path *default-ontology-file*))
  (second
   (assoc "id"
	  (car (get-specific-concept-tags concept-name concept-list xml-file-path))
	  :test #'equalp)))



;;; 指定基本概念のスロット内の1属性を抽出
(defun get-attribute-from-slot-tags (concept-name attribute concept-list &optional (xml-file-path *default-ontology-file*))
  (let ((concept-tags (get-concept-slot-tags concept-name concept-list xml-file-path)))
    (labels ((tmp-func (att)
	       (find-if #'(lambda (attr)
                            (tag-p attribute attr))
                        (second att))))
       (mapcar #'(lambda (slot-tag)
		   (tmp-func slot-tag))
	       concept-tags))))

;;; 指定された基本概念が持つスロット情報を取得
(defun get-slot-tags (concept-name concept-list &optional (xml-file-path *default-ontology-file*))
  (mapcar #'list
	  (get-attribute-from-slot-tags concept-name "id" concept-list xml-file-path)
          (get-attribute-from-slot-tags concept-name "role" concept-list xml-file-path)
          (get-attribute-from-slot-tags concept-name "kind" concept-list xml-file-path)
          (get-attribute-from-slot-tags concept-name "class_constraint" concept-list xml-file-path)
          (get-attribute-from-slot-tags concept-name "rh_name" concept-list xml-file-path)
          (get-attribute-from-slot-tags concept-name "num" concept-list xml-file-path)
	  (get-attribute-from-slot-tags concept-name "value" concept-list xml-file-path)))


#|
オントロジーのCLOSへのコンバート
|#
(defun convert-ontology-hozo (&key (file-path *default-ontology-file*) (ont *default-ontology*) (update t))
  (if update
      (progn
	(set-xml-struct file-path)
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
	(convert-basic-concept-node-id file-path ont)
        (format t "~t~t Converted Concepts~%~t~t~t * ~{~A~^ ~}~%~%" (show-concepts ont)))
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
		      child-concept-labels))))
  (convert-basic-concept-node-id xml-file-path ont))

;;; ノードIDの付与
(defun convert-basic-concept-node-id (&optional (xml-file-path *default-ontology-file*) (ont *default-ontology*))
  (let ((concept-label-list (show-concepts)))
    (loop for c in concept-label-list
	  unless (string= c "whole-root")
	    do (setf (photon.ontology::concept-id (find-concept c ont))
		     (get-concept-id c concept-label-list xml-file-path)))))


;;; instanceかどうかをチェック
(defun convert-instantiation (&optional (xml-file-path *default-ontology-file*) (ont *default-ontology*))
  (let ((concept-list (get-concept-label xml-file-path)))
    (loop for c in (remove-if #'(lambda (c-s) (string= c-s "whole-root")) (show-concepts))
	  do (setf (instantiation (find-concept c ont))
		   (instance-concept-p c concept-list xml-file-path)))))

;;; 部分/属性概念の変換
(defun convert-part-attribute-concept (&optional (xml-file-path *default-ontology-file*) (ont *default-ontology*))
  (let* ((c-list (get-concept-label xml-file-path))
	 (concept-list (get-concept-label xml-file-path)))
    (loop for c in c-list ;; c mean anime title string
          do (mapcar #'(lambda (slot)
    	     		 (remove-if #'null
    	     			    (let ((concept-id
					    (princ-to-string
	     				     (second
	     				      (assoc "id" slot :test #'string=))))
					  (role-name
	     				    (princ-to-string
	     				     (second
	     				      (assoc "role" slot :test #'string=))))
    	     				  (class-const
	     				    (princ-to-string
	     				     (second
	     				      (assoc "class_constraint" slot :test #'string=))))
    	     				  (rh-name
	     				    (princ-to-string
	     				     (second
	     				      (assoc "rh_name" slot :test #'string=))))
    	     				  (cardinality
	     				    (format nil "~A"
	     					    (second
	     					     (assoc "num" slot :test #'string=))))
    	     				  (val
	     				    (format nil "~A"
	     					    (second
	     					     (assoc "value" slot :test #'string=)))))
    	     			      (append-concept
    	     			       (make-concept role-name :c-type :part-of-concept
						               :concept-id concept-id
    	     						       :class-restriction class-const
    	     						       :cardinality cardinality
    	     						       :rh-name rh-name
    	     						       :val val)
    	     			       (find-concept c ont)))))
    	     (get-slot-tags c concept-list))
          finally (format nil "~A" (show-concepts ont)))
    ))

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







#|------------------------------------------------------------------------------|#
#|------------------ XMLオブジェクトとの対応を保ったままにする -----------------|#
;; tessutoyou
;; (photon.hozo::renew-xml-struct (photon.hozo::xml-objector "/Users/tomoki/Desktop/ontology.xml"))

(defparameter *xml-struct* nil)
(defparameter *newest-hozo-file-path* nil)
(defun xml-objector (file-path)
  (xmls:parse (read-file-into-string file-path) :compress-whitespace t))
(defun set-xml-struct (file-path)
  (setf *xml-struct* (xml-objector file-path))
  (setf *newest-hozo-file-path* file-path))

;;(set-xml-struct "~/.photon/ontology/english-conversation.xml")

(defun find-ont-id-struct (xml-struct)
  (second (assoc "ont_id" (xmls:node-attrs xml-struct) :test #'string=)))

#|--------------- 抽出機(基本概念（CONCEPT））を前提 |#
;;; 特定ノード（Struct）のラベルを抽出
(defun extract-node-label (node-struct)
  (car
   (remove-if #'null
	      (mapcar #'(lambda (node)
			  (when (string= "LABEL" (xmls:node-name node))
			    (car (xmls:node-children node))))
		      (xmls:node-children node-struct)))))

;;; 特定ノード（Struct）の座標を抽出
(defun extract-node-position (node-struct)
  (car
   (remove-if #'null
	      (mapcar #'(lambda (node)
			  (when (string= "POS" (xmls:node-name node))
			    (reverse (xmls:node-attrs node))))
		      (xmls:node-children node-struct)))))


;;; 特定ノード（Struct）のIDを抽出
(defun extract-node-id (node-struct)
  (let ((id (assoc "id" (xmls:node-attrs node-struct) :test #'string=)))
    (when id (second id))))

;;; 特定ノード（Struct）のinstantiationを抽出
(defun extract-node-instantiation (node-struct)
  (let ((inst (assoc "instantiation" (xmls:node-attrs node-struct) :test #'string=)))
    (when inst (second inst))))

(defun extract-node-slots (node-struct)
  (remove-if #'null
	     (mapcar #'(lambda (d)
			 (when (string= (xmls:node-name d) "SLOTS")
			   d))
		     node-struct)))


#|------------- 検索機 |#
;;; ノード名が対象のものと合致するまでCHILDRENのノードを検索
(defun node-searcher-by-structure-name (target-node-name &optional (struct *xml-struct*))
  (labels ((rec-point (tmp-struct)
	     (if (string= (xmls:node-name tmp-struct) target-node-name)
		 tmp-struct
		 (remove-if #'null
			    (loop for node in (xmls:node-children tmp-struct)
				  when (and node (eq (type-of node) 'xmls:node))
				    collect (rec-point node))))))
    (alexandria:flatten (rec-point struct))))

;;; 属性でノード検索
(defun node-searcher-by-attribute (extractor attribute-value &optional (struct *xml-struct*)) 
  (loop for node in (node-searcher-by-structure-name "CONCEPT" struct)
	when (string= (funcall extractor node) attribute-value)
	collect node))

;;; ノードをID属性で検索
(defun node-searcher-by-id (node-id &optional (struct *xml-struct*)) 
  (first (node-searcher-by-attribute #'extract-node-id node-id struct)))

;;; インスタンスノードかどうかで検索
(defun instance-node-searcher (&optional (struct *xml-struct*)) 
  (node-searcher-by-attribute #'extract-node-instantiation "true" struct))

;;; 基本概念ノードのラベルで検索
(defun node-searcher-by-label (node-label &optional (struct *xml-struct*))
  (first (node-searcher-by-attribute #'extract-node-label node-label struct)))

;;; 各基本概念（問いインスタンス）に当たるノードのPart-of（すなわちクエスチョンのプレースホルダーに入る問い）部分と，そのRequiredKeywordを抽出
(defun extract-part-and-keyword (node)
  (let* ((node-lst
	   (extract-node-slots
	    (xmls:node-children node)))
	 (n-lst (flatten
		 (mapcar #'xmls:node-children
			 node-lst))))
    (mapcar #'(lambda (d)
		(list (second (assoc "role" (xmls:node-attrs d) :test #'string=))
		      (loop for tmp in (car
					(mapcar #'(lambda (sd)
						    (flatten (mapcar #'xmls:node-children (xmls:node-children sd))))
						(loop for x in (xmls:node-children d)
						      when (string= "SLOTS" (xmls:node-name x))
							collect x)))
			    when (string= (xmls:node-name tmp) "SUB_L")
			      collect (second (assoc "class_const" (xmls:node-attrs tmp) :test #'string=)))))
	    n-lst)))
    


  
#|------------- 処理 |#
;;; IDのインクリメントする
(defun generate-increamented-newest-node-id (id)
  (let* ((delimiter "_")
	 (contamined-symbol (split-sequence delimiter id :test #'string=))
	 (garbage-id (first contamined-symbol))
	 (main-id (second contamined-symbol))
	 (id-type (subseq main-id 0 1))
	 (id-number (parse-integer (subseq main-id 1))))
    (format nil "~A_~A~A" garbage-id id-type (1+ id-number))))

;;; ノードを追加する
(defun add-basic-concept-node-as-strusture (label &optional parent child))

;;; ノード情報を書き換える
;; attributesを書き換える
(defun rewrite-node-attributes (node-struct target-attribute-string replacement)
  (let ((attributes
	  (remove-if #'(lambda (attr-list)
			 (when (string= (car attr-list) target-attribute-string) t))
		     (xmls:node-attrs node-struct)))
	(replace-pair (list target-attribute-string replacement)))
    (setf (xmls:node-attrs node-struct) (push replace-pair attributes))))

;; 座標を書き換える
(defun rewrite-node-position (node-struct x y)
  (let ((coordinate (list (list "x" x) (list "y" y))))
    (loop for node in (xmls:node-children node-struct)
	  when (string= "POS" (xmls:node-name node))
	    do (setf (xmls:node-attrs node) coordinate))))

;; ラベルを書き換える(CONCEPTの中だけなので，ISAも別途書き換える)
(defun rewrite-node-label-only-concept (node-struct new-label)
  (let ((new-label-object (list new-label)))
    (loop for node in (xmls:node-children node-struct)
	  when (string= "LABEL" (xmls:node-name node))
	    do (setf (xmls:node-children node) new-label-object))))

(defun rewrite-node-add-child (node-struct child-node-struct)
  (setf (xmls:node-children node-struct) (push child-node-struct (xmls:node-children node-struct))))

(defun replot-hozo (&optional (pathname-string new-xml-struct *newest-hozo-file-path*))
  "Rewrite hozo file, e.g.: (progn (photon.hozo::set-xml-struct \"~/Desktop/photon.xml\") (photon.hozo::rewrite-node-position (photon.hozo::node-searcher-by-label \"Any\") 500 0) (photon.hozo::replot-hozo))"
  (write-string-into-file
   (format nil "~A~%~A"
	   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	   (xmls:toxml new-xml-struct))
   pathname-string :if-exists :supersede :if-does-not-exist :create))


#| 高次元処理 |#


