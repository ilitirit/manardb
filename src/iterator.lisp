(in-package #:manardb)

;; XXX this can be made more efficient if class-specifier is known to be constant at expansion time
(defmacro doclass ((var class-specifier &key fresh-instances reverse) &body body)
  (alexandria:with-unique-names (step tag class mtagmap mptr last-mptr first-mptr)
    (let ((body-form
	   (if fresh-instances
	       `(let ((,var (make-instance ,class '%ptr ,mptr))) ,@body)
	       `(locally ,@body)))
	  (incf-step (if reverse `decf `incf))
	  (cmp (if reverse 
		   `(>= ,mptr ,first-mptr)
		   `(> ,last-mptr ,mptr))))
     `(let* ((,class (force-class ,class-specifier))
	     (,tag (mm-metaclass-tag ,class))
	     (,step (ash (mm-metaclass-len ,class) +mtag-bits+)))
	(declare (type mptr ,step))
	(when ,tag   ; if finalize-inheritance has not yet been called
	  (let ((,mtagmap (mtagmap ,tag)))
	    (unless (mtagmap-closed-p ,mtagmap)
	      (let* ((,last-mptr (make-mptr ,tag (mtagmap-last-index ,mtagmap)))
		     (,first-mptr (make-mptr ,tag (mtagmap-first-index ,mtagmap)))
		     (,mptr ,(if reverse `(- ,last-mptr ,step) `,first-mptr))
		     ,@(unless fresh-instances `((,var (make-instance ,class '%ptr ,mptr)))))
		(loop while ,cmp
		      do ,body-form
		      (,incf-step ,mptr ,step)
		      ,@(unless fresh-instances
				`((setf (%ptr ,var) ,mptr))))))))))))

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

