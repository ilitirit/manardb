(in-package #:manardb.test)

(stefil:in-suite manardb-test)

(stefil:deftest test-simple-defclass ()
  (eval `(defclass test-empty-class ()
	   ()
	   (:metaclass mm-metaclass)))

  (eval `(defclass test-byte-class ()
	   ((slot :type (unsigned-byte 8)))
	   (:metaclass mm-metaclass)))

  (eval `(defclass test-boxed-class ()
	   ((slot :initform 1))
	   (:metaclass mm-metaclass)))
  )

(stefil:deftest test-create-two-slot-class ()
  (eval 
   `(manardb:defmmclass two-slot ()
      ((basic-slot :initarg :basic-slot :initform (error "Please provide a value for the basic slot"))
       (marray :initarg :marray :initform
	       (manardb:make-marray
		1000
		:initial-element
		nil))))))

(stefil:deftest test-nil-slots-are-not-created ()
  (test-create-two-slot-class)
  (let ((m (make-instance 'two-slot :basic-slot nil)))
    (stefil:is (not (slot-value m 'basic-slot)))
    m))

(stefil:deftest test-create-instance-of-two-slot (&optional (vals (list nil 0 :keyword "string")))
  (test-create-two-slot-class)
  (loop for val in vals do
	(let ((m (make-instance 'two-slot :basic-slot val :marray val)))
	  (stefil:is (equalp val (slot-value m 'marray)))
	  (stefil:is (equalp val (slot-value m 'basic-slot))))))
