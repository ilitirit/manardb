(in-package #:manardb)

(stefil:in-suite manardb-test)

(stefil:deftest create-tree-class-test ()
  (eval `
   (defmmclass tree ()
     ((numval :type (unsigned-byte 50) :initform 666)
      (general-val :initform 'gen-val :initarg :general-val :accessor tree-general-val)
      (left :type tree :initform nil :accessor tree-left)
      (right :type tree :initform nil :accessor tree-right)
      (empty :initarg :another-slot :accessor tree-another-slot))))
  (stefil:is (find-class 'tree)))

(stefil:deftest simple-tree-create-test (&optional (gen-val "This is a string"))
  (create-tree-class-test)
  (let ((tree (make-instance 'tree :general-val gen-val)))
    (stefil:is (equalp (slot-value tree 'numval) 666))
    (stefil:is (equalp (tree-general-val tree) gen-val))
    (stefil:is (not (eq (tree-general-val tree) gen-val)))
    (setf (tree-left tree) tree)
    (setf (tree-right (tree-left tree)) 'right)
    (stefil:is (eq (tree-right tree) 'right))
    (setf (tree-left (tree-left tree)) nil)
    tree))

(stefil:deftest symbol-slot-tree-create-test (&optional (symbol :keyword))
  (create-tree-class-test)
  (let ((tree (make-instance 'tree :another-slot symbol)))
    (stefil:is (eq symbol (funcall 'tree-another-slot tree)))
    (stefil:is (eq symbol (slot-value tree 'empty)))
    (stefil:is (eq 'gen-val (slot-value tree 'general-val)))
    tree))



