(in-package #:manardb)

;; XXX this can be made more efficient if class-specifier is known to be constant at expansion time
(defmacro doclass ((var class-specifier &key fresh-instances) &body body)
  (alexandria:with-unique-names (step tag class mtagmap mptr last-mptr)
    `(let* ((,class (force-class ,class-specifier))
	    (,tag (mm-metaclass-tag ,class))
	    (,step (ash (mm-metaclass-len ,class) +mtag-bits+)))
       (when ,tag ; if finalize-inheritance has not yet been called
	 (let ((,mtagmap (mtagmap ,tag)))
	   (when (not (mtagmap-closed-p ,mtagmap))
	     (let* ((,last-mptr (make-mptr ,tag (mtagmap-last-index ,mtagmap)))
		    (,mptr (make-mptr ,tag (mtagmap-first-index ,mtagmap))))
	       ,(cond (fresh-instances
			`(loop while (> ,last-mptr ,mptr)
			       do (let ((,var (make-instance ,class '%ptr ,mptr))) ,@body)
			       (incf ,mptr ,step)))
		      (t
		       `(let ((,var (make-instance ,class '%ptr ,mptr)))
			  (loop while (> ,last-mptr ,mptr)
				do (locally ,@body)
				 (setf (%ptr ,var) (incf ,mptr ,step)))))))))))))

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
  (mtagmap-count (mm-metaclass-mtagmap class)))

