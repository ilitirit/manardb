(in-package #:manardb)

(stefil:in-suite manardb-test)

(stefil:deftest create-tree-class-test ()
  (eval `
   (defmmclass tree ()
     ((numval :type (unsigned-byte 50) :initform 666 :initarg :numval)
      (general-val :initform 'gen-val :initarg :general-val :accessor tree-general-val)
      (left :type tree :initform nil :accessor tree-left)
      (right :type tree :initform nil :accessor tree-right)
      (empty :initarg :another-slot :accessor tree-another-slot))))
  (stefil:is (find-class 'tree)))

(stefil:deftest simple-tree-create-test (&optional (gen-val "This is a string"))
  (create-tree-class-test)
  (let ((tree (make-instance 'tree :general-val gen-val)))
    (setf (tree-left tree) tree)
    (setf (tree-right (tree-left tree)) 'right)
    (stefil:is (eq 'right (tree-right (tree-left tree))))
    (setf (tree-left (tree-left tree)) nil)
    (stefil:is (eq nil (tree-left tree)))
    (setf (tree-another-slot tree) (make-marray 10 :initial-element tree))
      
    (simple-tree-consistency-test tree gen-val)
    tree))

(stefil:deftest simple-tree-consistency-test (tree gen-val)
  (stefil:is (eq (tree-right tree) 'right))
  (stefil:is (equalp (slot-value tree 'numval) 666))
  (stefil:is (equalp (tree-general-val tree) gen-val))
  (stefil:is (not (eq (tree-general-val tree) gen-val)))
  (stefil:is (= 10 (marray-length (tree-another-slot tree))))
  (loop for i below (marray-length (tree-another-slot tree))
	do (stefil:is (meq (marray-ref (tree-another-slot tree) i) tree))))

(stefil:deftest symbol-slot-tree-create-test (&optional (symbol :keyword))
  (create-tree-class-test)
  (let ((tree (make-instance 'tree :another-slot symbol)))
    (stefil:is (eq symbol (funcall 'tree-another-slot tree)))
    (stefil:is (eq symbol (slot-value tree 'empty)))
    (stefil:is (eq 'gen-val (slot-value tree 'general-val)))
    tree))


(stefil:deftest test-make-complex-tree (&optional (depth 6))
  (cond ((plusp depth)
	 (let ((tree
		(make-instance 'tree :numval depth :another-slot 
			       (let ((m (make-marray depth)))
				 (loop for i below depth 
				       for last-tree = nil then tree
				       for tree = (funcall 'test-make-complex-tree (1- depth))
				 do 
				       (when last-tree
					 (setf (tree-right last-tree) tree))
				       (setf (marray-ref m i) 
					     tree))
				 m))))
	   (test-consistency-of-complex-tree tree depth)
	   tree))
	(t
	 'leaf)))

(stefil:deftest test-consistency-of-complex-tree (tree &optional (depth (slot-value tree 'numval)))
  (let ((marray (slot-value tree 'empty)))
    (stefil:is (= depth (slot-value tree 'numval)))
    (loop for i below depth
	  for last-ref = nil then ref
	  for ref = (marray-ref marray i)
	  do
	  (cond ((= 1 depth)
		 (stefil:is (eq 'leaf ref)))
		 (t
		  (when last-ref
		    (stefil:is (not (meq ref (tree-right ref))))
		    (stefil:is (meq ref (tree-right last-ref))))
		  (funcall 'test-consistency-of-complex-tree ref (1- depth)))))))
