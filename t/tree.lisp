(in-package #:manardb)

(stefil:in-suite manardb-test)

(defclass tree ()
  ((numval :type (unsigned-byte 50) :initform 666)
   (general-val :initform nil :initarg :general-val :accessor tree-general-val)
   (left :type tree :initform nil :accessor tree-left)
   (right :type tree :initform nil :accessor tree-right))
  (:metaclass mm-metaclass))

(stefil:deftest simple-tree-create-test (&optional (gen-val "This is a string"))
  (let ((tree (make-instance 'tree :general-val gen-val)))
    (stefil:is (equalp (tree-general-val tree) gen-val))
    (stefil:is (not (eq (tree-general-val tree) gen-val)))
    (setf (tree-left tree) tree)
    (setf (tree-right (tree-left tree)) 'right)
    (stefil:is (eq (tree-right tree) 'right))
    (setf (tree-left (tree-left tree)) nil)
    tree))

