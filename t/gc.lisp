(in-package #:manardb.test)

(stefil:in-suite manardb-test)

(stefil:deftest test-gc-nil (&key (gc 'gc))
  (with-transaction (:message "GC")
    (loop repeat 10 do
	  (simple-tree-create-test)
	  (stefil:is (plusp (count-all-instances 'tree)))
	  (funcall gc nil)
	  (stefil:is (zerop (count-all-instances 'tree))))))

(stefil:deftest test-gc-one (&key (genval "GC") (gc 'gc))
  (with-transaction (:message "GC one")
    (funcall gc nil)
    (let ((obj (simple-tree-create-test genval)))
      (loop repeat 10 do
	    (gc (list (lisp-object-to-mptr obj)))
	    (stefil:is (= 1 (count-all-instances 'tree)))
	    (simple-tree-consistency-test obj genval)))))

(defun test-gc-marray (&key (first t) (len 10) (depth 6) (gc 'gc) (gc-repeat 5) print)
  (assert (evenp len))
  (with-transaction (:message "GC marray")
    (funcall gc nil)
    (let* ((list (loop repeat len collect (test-make-complex-tree depth)))
	  (count (count-all-instances 'tree)))
     
      (labels (
	       (remaining ()
		 (let ((remaining (remove-if 'tree-parent (retrieve-all-instances 'tree))))
		   (stefil:is (= (length remaining) len))
		   (when print (format t "~&Remaining = ~A~%" remaining))
		   remaining))

	       (consistent ()
		 (stefil:without-test-progress-printing ;;; too much progress
		   (mapcar 'test-consistency-of-complex-tree (remaining)))
		 (stefil:is (= count (count-all-instances 'tree)))))

	(consistent)

	(loop repeat gc-repeat do
	      (gc (remaining))
	      (consistent)))

      

      (test-gc-marray-half :first first :count count :list list :gc gc :gc-repeat gc-repeat :print print))))

(defun test-gc-marray-half (&key first count list gc (gc-repeat 5) print)
  (let ((half (if first 
		  (subseq list 0 (/ (length list) 2))
		  (loop for x on list by #'cddr collect (second x)))))
    (funcall gc half)
    (stefil:is (= count (* 2 (count-all-instances 'tree))))

    (flet ((remaining ()
	     (let ((remaining (remove-if 'tree-parent (retrieve-all-instances 'tree))))
	       (stefil:is (= (length half) (length remaining)))
	       (when print (format t "~&Remaining = ~A~%" remaining))
	       remaining)))
      (stefil:without-test-progress-printing ;;; too much progress
	(mapcar 'test-consistency-of-complex-tree (remaining)))
     (loop repeat gc-repeat 
	   do
	   (funcall gc (remaining))
	   (stefil:is (= count (* 2 (count-all-instances 'tree))))
	   (stefil:without-test-progress-printing ;;; too much progress
	     (mapcar 'test-consistency-of-complex-tree (remaining)))))))


(stefil:deftest test-rewrite-gc ()
  (test-gc-marray :gc (lambda (seq) (rewrite-gc seq))))

(stefil:deftest test-gc ()
  (test-gc-marray :first nil)
  (test-gc-marray :first t ))
