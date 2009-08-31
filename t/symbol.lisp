(in-package #:manardb.test)

(stefil:in-suite manardb-test)

(stefil:deftest symbol-tag-is-zero-test ()
  (stefil:is (= (mm-metaclass-tag (find-class 'mm-symbol)) 0)))

(stefil:deftest store-keyword-test ()
  (loop for keyword in '(:keyword :key :errorp)
	for mptr = (lisp-object-to-mptr keyword)
	do (stefil:is (eq keyword (mptr-to-lisp-object mptr)))
	(stefil:is (= mptr (lisp-object-to-mptr keyword)))))

(stefil:deftest store-all-symbols-test (&optional 
					(packages (list (find-package '#:cl) 
							(find-package '#:manardb))))
  (macrolet ((do-all-syms ((var) &body body)
	       (alexandria:with-gensyms (package)
		 `(loop for ,package in packages do
		       (do-all-symbols (,var ,package)
			 ,@body)))))
    (stefil:without-test-progress-printing ;;; too much progress
      (let ((table (make-hash-table)))
	(flet ((add (sym mptr)
		 (let ((orig (gethash sym table)))
		   (when orig
		     (stefil:is (= orig mptr)))
		   (setf (gethash sym table) mptr))))
	 (do-all-syms (sym)
	   (add sym (lisp-object-to-mptr sym)))
	 (iter (for (sym mptr) in table)
	       (stefil:is (= mptr (lisp-object-to-mptr sym))))
	 (do-all-syms (sym)
	   (stefil:is (eq sym (mptr-to-lisp-object (lisp-object-to-mptr sym)))))
	 (iter (for (sym mptr) in table)
	       (stefil:is (eq sym (mptr-to-lisp-object mptr))))
	 (do-all-syms (sym)
	   (add sym (lisp-object-to-mptr sym))))))))

