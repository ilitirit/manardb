(in-package #:manardb)

(stefil:in-suite manardb-test)

(stefil:deftest defclass-simple-test ()
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