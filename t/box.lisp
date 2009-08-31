(in-package #:manardb.test)

(stefil:in-suite manardb-test)

(defun box-unbox (object)
  (mptr-to-lisp-object (lisp-object-to-mptr object)))

(stefil:deftest box-numbers-test ()
  (loop for num in '(0 1 -1 255 -127 -128 1000 1000000000 -10000000)
	do (stefil:is (= (box-unbox num) num))))


(stefil:deftest unbox-nil-test ()
  (stefil:is (eq nil (mptr-to-lisp-object 0)))
  (stefil:is (= 0 (box-object nil))))

(stefil:deftest box-cons-test ()
  (loop repeat 10
	for cons = nil then (cons cons nil)
	do (stefil:is (equal cons (box-unbox cons)))))

(stefil:deftest box-unspecialized-array-test ()
  (loop for array in (list 
		      (make-array 0)
		      (make-array 10 :element-type t :initial-element nil))
	do
	(stefil:is (equalp array (box-unbox array)))))

(stefil:deftest box-numeric-array-test ()
  (loop for (limit type) in `((256 (unsigned-byte 8))
			      (,(ash 1 9) (unsigned-byte 64))
			      (,(ash 1 31) (unsigned-byte 64))
			      (,(ash 1 30) (signed-byte 64))
			      (,most-positive-double-float double-float)
			      (,most-positive-single-float single-float))
	do (loop for len in '(2 1 10 100 1000 10000) do
		 (let ((array (make-array len :element-type type)))
		   (loop for i below len do
			 (setf (aref array i) (random limit)))
		   (stefil:is (equalp array (box-unbox array)))))))

(stefil:deftest box-string-test ()
  (loop for string in '("" "a" "one two three" #.(string (code-char 1000))) do
	(stefil:is (string= string (box-unbox string))))) 
