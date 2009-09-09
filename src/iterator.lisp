(in-package #:manardb)

(defmacro doclass ((var class-specifier &key fresh-instances reverse) &body body)
  (alexandria:with-unique-names (tag class mtagmap 
				      last-index first-index instantiator len
				      index)
    `(let* ((,class (force-class ,class-specifier))
	    (,tag (mm-metaclass-tag ,class))
	    (,len (mm-metaclass-len ,class)))
       (declare (type mindex ,len))
       (when ,tag   ; if finalize-inheritance has not yet been called
	 (let ((,mtagmap (mtagmap ,tag)))
	   (unless (mtagmap-closed-p ,mtagmap)
	      (let* ((,instantiator (mtagmap-instantiator ,mtagmap))
		     (,last-index (mtagmap-last-index ,mtagmap))
		     (,first-index (mtagmap-first-index ,mtagmap)))
		(declare (type mindex ,last-index ,first-index))
		(when (> ,last-index ,first-index)
		  (decf ,last-index ,len)
		  (let ((,index ,(if reverse `,last-index `,first-index)))
		    (loop ,(if fresh-instances `for `with) ,var = (funcall ,instantiator ,index) 
			  do (let ,(when fresh-instances `((,var ,var))) ,@body)
			  (when (= ,index ,(if reverse `,first-index `,last-index))
			    (return))
			  (,(if reverse `decf `incf) ,index ,len)
			  ,@(unless fresh-instances `((setf (%ptr ,var) (make-mptr ,tag ,index))))))))))))))
  
(defun mm-subclasses (class)
  (remove-duplicates
   (list* class (loop for c in (class-direct-subclasses class)
		      when (typep class 'mm-metaclass)
		      appending (mm-subclasses c)))))

(defmacro dosubclasses ((var class-specifier &rest options) &body body)
  (alexandria:with-unique-names (one-class class)
    `(flet ((,one-class (,class)
	     (doclass (,var ,class ,@options)
		      ,@body)))
      (loop for ,class in (mm-subclasses (force-class ,class-specifier))
	    do (,one-class ,class)))))

(defun retrieve-all-instances (class)
  (let (ret)
    (dosubclasses (p class :fresh-instances t)
      (push p ret))
    ret))

(defun count-all-instances (class)
  (loop for c in (mm-subclasses (force-class class))
	for m = (mm-metaclass-mtagmap c)
	summing
	(if (mtagmap-closed-p m)
	    0
	    (mtagmap-count m))))

