(in-package #:manardb)

(stefil:in-suite manardb-test)

(stefil:deftest test-gc-nil ()
  (with-transaction (:message "GC")
    (loop repeat 10 do
	  (simple-tree-create-test)
	  (stefil:is (plusp (count-all-instances 'tree)))
	  (gc nil)
	  (stefil:is (zerop (count-all-instances 'tree))))))

(stefil:deftest test-gc-one (&optional (genval "GC"))
  (with-transaction (:message "GC one")
    (gc nil)
    (let ((obj (simple-tree-create-test genval)))
      (loop repeat 10 do
	    (gc (list (lisp-object-to-mptr obj)))
	    (stefil:is (= 1 (count-all-instances 'tree)))
	    (simple-tree-consistency-test obj genval)))))

(stefil:deftest test-gc-marray (&key (first t) (len 10) (depth 6))
  (assert (evenp len))
  (with-transaction (:message "GC marray")
    (gc nil)
    (let* ((list (loop repeat len collect (test-make-complex-tree depth)))
	  (count (count-all-instances 'tree)))
      (mapcar 'test-consistency-of-complex-tree (retrieve-all-instances 'tree))
      (stefil:is (= count (count-all-instances 'tree)))

      (test-gc-marray-half :first first :count count :list list))))

(stefil:deftest test-gc-marray-half (&key first count list)
  (let ((half (if first 
		  (subseq list 0 (/ (length list) 2))
		  (loop for x on list by #'cddr collect (second x)))))
    (gc (mapcar 'lisp-object-to-mptr half))
    (stefil:is (= count (* 2 (count-all-instances 'tree))))
    (loop repeat 5 do
	  (gc (mapcar 'lisp-object-to-mptr (retrieve-all-instances 'tree))))
    (stefil:is (= count (* 2 (count-all-instances 'tree))))
    (mapcar 'test-consistency-of-complex-tree (retrieve-all-instances 'tree))))