(in-package #:manardb)

;; XXX this can be made more efficient if classname is known to be constant at expansion time
(defmacro doclass ((var classname) &body body)
  (alexandria:with-unique-names (len tag class mtagmap mptr last-mptr)
    `(let* ((,class (force-class ,classname))
	    (,tag (mm-metaclass-tag ,class))
	    (,len (mm-metaclass-len ,class))
	    (,mtagmap (mtagmap ,tag))
	    (,last-mptr (make-mptr ,tag (mtagmap-last-index ,mtagmap)))
	    (,mptr (make-mptr ,tag (mtagmap-first-index ,mtagmap)))
	    (,var (make-instance ,class '%ptr ,mptr)))
       (loop while (> ,last-mptr ,mptr)
	     do (locally ,@body)
	     (setf (%ptr ,var) (incf ,mptr ,len))))))

(defun mm-subclasses (class)
  (remove-duplicates
   (list* class (loop for c in (class-direct-subclasses class)
		      when (typep class 'mm-metaclass)
		      appending (mm-subclasses c)))))

(defmacro dosubclasses ((var classname) &body body)
  (alexandria:with-unique-names (one-class class)
    `(flet ((,one-class (,class)
	     (doclass (,var ,class)
		      ,@body)))
      (loop for ,class in (mm-subclasses (force-class ,classname))
	    do (,one-class ,class)))))