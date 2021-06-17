(in-package :cl-user)
(defpackage photon.hozo
  (:use :cl)
  (:import-from :alexandria
                :read-file-into-string
		:flatten
		:write-string-into-file)
  (:import-from :split-sequence
		:split-sequence)
  (:import-from :photon.ontology
  		:*default-ontology*
                :*default-ontology-file*
		:make-concept
		:find-concept
		:find-concept-from-id
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
        (clear-ontology ont)
	(set-xml-struct file-path)
	(format t "Converting Basic concepts...~%")
        (convert-concept ont)
	(format t "Converting Concept ID / IS-A relation / Instance-p...~%")
        (convert-basic-info ont)
	(format t "Converting Part/Attribute concepts...~%")
        (convert-part-attribute-concept ont)
	(format t "Finalize...~%")
        (format t "~t~t Converted Concepts~%~t~t~t * ~{~A~^ ~}~%~%" (show-concepts ont))
	t
	)
      file-path))

(defun convert-concept (&optional (ont *default-ontology*))
  "基本概念の変換"
  (let ((c-list (concatenate 'list (find-basic-concept-names) (find-instance-names))))
    (loop for c in c-list
	  do (add-concept c ont :concept-type :basic-concept)
	  finally (return
		    (format nil "~{~A~^, ~}" c-list)))))

(defun convert-basic-info (&optional (ont *default-ontology*))
  "諸基本情報のの変換"
  (loop for c in (concatenate 'list (find-basic-concept-names) (find-instance-names))
	do (let ((target-concept (find-concept c ont)) ;; CLOSオブジェクト
		 (target-concept-structure (find-concept-node-struct-from-name c)) ;; XMLのStruct
		 (parent-concept-label
		   (or
		    (find-parent-concept c)
		    "whole-root"))
		 (child-concept-labels
		   (or
		    (find-children-concepts c)
		    (list))))

	     ;; 基本概念・インスタンスのID
	     (setf (photon.ontology::concept-id target-concept)
		   (extract-node-id (find-concept-node-struct-from-name c)))
	     
	     ;; is-a関係
	     (update-parent-child-concept
	      target-concept
	      (find-concept parent-concept-label ont)
	      (mapcar #'(lambda (c2)
			  (find-concept c2 ont))
		      child-concept-labels))

	     ;; instanceかどうか
	     (setf (instantiation target-concept)
		   (instance-p c))

	     )))


(defun convert-part-attribute-concept (&optional (ont *default-ontology*))
  "部分/属性概念の変換"
    (loop for c in (concatenate 'list (find-basic-concept-names) (find-instance-names))
	  do (let* ((target-concept (find-concept c ont)) ;; CLOSオブジェクト
		    (target-concept-structure (find-concept-node-struct-from-name c))) ;; XMLのStruct
	       (append-slot target-concept-structure target-concept))))

(defun append-slot (node-structure root-concept)
  "NodeのStructからRoleコンセプトのCLOSオブジェクトを作成"
  (when node-structure
    (mapcar #'(lambda (slot)
		(labels ((attr-str (att)
			   ;; スロット内容を取り出す処理
			   (second
			    (assoc att (xmls:node-attrs slot) :test #'string=))))
		  ;; ここでPartコンセプト生成＋Append
		  (let ((new-part-concept
			  (make-concept (attr-str "role")
					:c-type :part-of-concept
					:concept-id (attr-str "id")
					:class-restriction (attr-str "class_constraint")
					:cardinality (attr-str "num")
					:rh-name (attr-str "rh_name")
					:val (attr-str "val")))
			(next-slots (xmls:node-children slot)))
		    (append-concept new-part-concept root-concept)
		    (when (and next-slots (listp next-slots))
		      (mapcar #'(lambda (s)
				  (when
				      (or (equal (xmls:node-name s) "SLOTS") (not (equal (xmls:node-name s) "PART_TREE")))
				    (append-slot s new-part-concept)))
			      next-slots)))))
	    (extract-node-slots node-structure))))


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
#|------------------ XMLオブジェクトとの対応を保ったままにする(構造体で再実装) -----------------|#
;; testyou
;; (photon.hozo::renew-xml-struct (photon.hozo::xml-objector "/Users/tomoki/Desktop/ontology.xml"))

;; (photon.hozo::set-xml-struct "/Users/tomoki/Dropbox/Project-Myself/photon/src/ontology/ontology/sample-ontology.xml")
;; (photon.hozo::set-xml-struct "/Users/tomoki/Dropbox/Laboratory/progress/In-Progress-Works/00-研究遂行/02-思考整理支援システム（Forest）/研究活動オントロジー/user-ontology.xml")
(defparameter *xml-struct* nil)
(defparameter *newest-hozo-file-path* nil)
(defun xml-objector (file-path)
  (xmls:parse (read-file-into-string file-path) :compress-whitespace t))
(defun set-xml-struct (file-path)
  (setf *xml-struct* (xml-objector file-path))
  (setf *newest-hozo-file-path* file-path))

;;(set-xml-struct "~/.photon/ontology/english-conversation.xml")

(defun find-ont-id-struct (&optional (xml-struct *xml-struct*))
  (second (assoc "ont_id" (xmls:node-attrs xml-struct) :test #'string=)))

(defun find-ont-filename-struct (&optional (xml-struct *xml-struct*))
  (second (assoc "filename" (xmls:node-attrs xml-struct) :test #'string=)))

(defun find-w-concept-struct (&optional (xml-struct *xml-struct*))
  "概念階層定義を抽出"
  (second (xmls:node-children xml-struct)))

(defun find-r-concept-struct (&optional (xml-struct *xml-struct*))
  "関係概念定義を抽出"
  (third (xmls:node-children xml-struct)))

#|--------------- 抽出機(基本概念（CONCEPT））を前提 |#
;;; 特定ノード（Struct）のラベルを抽出
(defun extract-node-label (node-struct)
  (car
   (remove-if #'null
	      (mapcar #'(lambda (node)
			  (when (string= "LABEL" (xmls:node-name node))
			    (car (xmls:node-children node))))
		      (xmls:node-children node-struct)))))


(defun extract-node-position (node-struct)
  "特定ノード（Struct）の座標を抽出"
  (car
   (remove-if #'null
	      (mapcar #'(lambda (node)
			  (when (string= "POS" (xmls:node-name node))
			    (reverse (xmls:node-attrs node))))
		      (xmls:node-children node-struct)))))


(defun extract-node-id (node-struct)
  "特定ノード（Struct）のIDを抽出"
  (let ((id (assoc "id" (xmls:node-attrs node-struct) :test #'string=)))
    (when id (second id))))

(defun extract-node-instantiation (node-struct)
  "特定ノード（Struct）のinstantiationを抽出"
  (let ((inst (assoc "instantiation" (xmls:node-attrs node-struct) :test #'string=)))
    (when inst (second inst))))

(defun extract-node-slots (node-struct)
  "特定ノード（Struct）がもつ部分概念・属性概念を抽出"
  (let ((tmp
	  (find-if (lambda (c)
		     (and c
			  (string= (xmls:node-name c) "SLOTS")))
		   (xmls:node-children node-struct))))
    (when tmp (xmls:node-children tmp))))


(defun find-basic-concept-names (&optional (xml-struct *xml-struct*))
  "概念名の一覧取得"
  (let ((concepts (xmls:node-children (find-w-concept-struct xml-struct))))
    (loop for c in concepts
	  when (and
		(string= (xmls:node-name c) "CONCEPT") ;; 概念ノード定義かどうか
		(null (assoc "instantiation" (xmls:node-attrs c) :test #'string=)) ;; インスタンスではないかどうか（データなし）
		(not (string= "true" (second (assoc "instantiation" (xmls:node-attrs c) :test #'string=))))) ;; インスタんか．データあり
	    collect (extract-node-label c))))

(defun find-instance-names (&optional (xml-struct *xml-struct*))
  " インスタンス概念名の一覧取得"
  (let ((concepts (xmls:node-children (find-w-concept-struct xml-struct))))
    (loop for c in concepts
	  when (and
		(string= (xmls:node-name c) "CONCEPT") ;; 概念ノード定義かどうか
		(assoc "instantiation" (xmls:node-attrs c) :test #'string=))
	    collect (extract-node-label c))))

(defun find-isa-links (&optional (xml-struct *xml-struct*))
  "is-aリンクの一覧取得"
  ;; まずISAをParent-Childのペアリストに起こす
  (let* ((concepts (xmls:node-children (find-w-concept-struct xml-struct)))
	 (p-c-list (loop for c in concepts
			 when (string= (xmls:node-name c) "ISA") ;; 親子定義かどうか
			   collect (let ((att (xmls:node-attrs c)))
				     (list (second (assoc "parent" att :test #'string=))
					   (second (assoc "child" att :test #'string=)))))))
    ;; １つのコンセプトに対して，子供になるコンセプトのリストをまとめる
    (loop for x in (concatenate 'list (find-basic-concept-names) (find-instance-names)) ;; 基本概念＋インスタンスの概念名一覧で回す
	  collect (list
		   x
		   (loop for c in p-c-list
			 when (string= x (first c))
			   collect (second c))))
    ))

(defun find-children-concepts (concept-name &optional (xml-struct *xml-struct*))
  "特定の概念の小概念一覧を取得する"
  (second (assoc concept-name (find-isa-links xml-struct) :test #'string=)))

(defun find-parent-concept (concept-name &optional (xml-struct *xml-struct*))
  "特定の概念の親概念名を取得する(1個だけという前提)"
  ;; まずISAをParent-Childのペアリストに起こす
  (let* ((concepts (xmls:node-children (find-w-concept-struct xml-struct)))
	 (p-c-list (loop for c in concepts
			 when (string= (xmls:node-name c) "ISA") ;; 親子定義かどうか
			   collect (let ((att (xmls:node-attrs c)))
				     (list (second (assoc "child" att :test #'string=))
					   (second (assoc "parent" att :test #'string=)))))))
    ;; １つのコンセプトに対して，親になるコンセプトのリストをまとめる
    (second (assoc concept-name p-c-list :test #'string=))))


(defun find-concept-node-struct-from-name (concept-name &optional (xml-struct *xml-struct*))
  "基本概念を概念名でStructを抽出"
  (let ((concepts (xmls:node-children (find-w-concept-struct xml-struct))))
    (loop for c in concepts
	  when (and
		(string= (xmls:node-name c) "CONCEPT") ;; 概念ノード定義かどうか
		(string= (extract-node-label c) concept-name)) ;; 概念名チェック
	    return c)))

(defun find-concept-node-struct-from-id (concept-id &optional (xml-struct *xml-struct*))
  "基本概念をIDでStructを抽出"
  (let ((concepts (xmls:node-children (find-w-concept-struct xml-struct))))
    (loop for c in concepts
	  when (and
		(string= (xmls:node-name c) "CONCEPT") ;; 概念ノード定義かどうか
		(string= (extract-node-id c) concept-id)) ;; 概念名チェック
	    return (extract-node-id c))))

(defun instance-p (concept-name)
  "概念がインスタンスかどうかを判別"
  (not (null (find concept-name (find-instance-names) :test #'string=))))
  

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
;; (defun extract-part-and-keyword (node)
;;   (let* ((node-lst
;; 	   (extract-node-slots
;; 	    (xmls:node-children node)))
;; 	 (n-lst (flatten
;; 		 (mapcar #'xmls:node-children
;; 			 node-lst))))
;;     (mapcar #'(lambda (d)
;; 		(list (second (assoc "role" (xmls:node-attrs d) :test #'string=))
;; 		      (loop for tmp in (car
;; 					(mapcar #'(lambda (sd)
;; 						    (flatten (mapcar #'xmls:node-children (xmls:node-children sd))))
;; 						(loop for x in (xmls:node-children d)
;; 						      when (string= "SLOTS" (xmls:node-name x))
;; 							collect x)))
;; 			    when (string= (xmls:node-name tmp) "SUB_L")
;; 			      collect (second (assoc "class_const" (xmls:node-attrs tmp) :test #'string=)))))
;; 	    n-lst)))
    

#|------- Utilities  --------|# 
(defun get-concept-labels ()
  "概念ノードのラベル一覧を取得"
  (mapcar #'extract-node-label
	  (node-searcher-by-structure-name "CONCEPT")))

(defun get-instance-labels ()
  "インスタンスノードのラベル一覧を取得"
  (mapcar #'extract-node-label
	  (instance-node-searcher)))

(defun instance-concept-p (concept-name)
  "インスタンスノードかどうかを調べる"
  (not (null (find concept-name (get-instance-labels) :test #'string=))))

(defun get-concept-id (concept-name)
  "指定したラベルを持つConceptのIDを取得"
  (extract-node-id
   (node-searcher-by-label concept-name)))

(defun get-sub-concepts (concept-name)
  "指定したラベルを持つノードのサブ概念の一覧を返す"
  (xmls:node-children
   (node-searcher-by-label concept-name)))


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


