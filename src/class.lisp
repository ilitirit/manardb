(in-package #:manardb)

(defmethod finalize-inheritance :after ((class mm-metaclass))
  (setup-mtagmap-for-metaclass class)
  (loop for slot in (class-slots class) do
	(when (slot-definition-memory-mapped slot)
	  (mm-effective-slot-definition-setup slot)))
  class)

(defun mm-metaclass-filename (class)
  (assert (class-name class) (class) "Cannot mmap anonymous classes.") ; is possible but not implemented or sensible(?)
  (check-type (class-name class) symbol)
  
  (merge-pathnames 
   (make-pathname 
    :name (flet ((clean (str)
		   (remove-if-not #'alphanumericp str)))
	    (let ((name (class-name class)))
	      (concatenate 'string (clean (package-name (symbol-package name))) 
			   "-" (clean (symbol-name name))))))
   *mmap-pathname-defaults*))

(declaim (ftype (function (mm-metaclass &optional mindex) mptr) mm-metaclass-alloc))
(defun-speedy mm-metaclass-alloc (class &optional (amount 1))
  (declare (type mindex amount))
  (make-mptr (mm-metaclass-tag class) 
	     (mtagmap-alloc (mm-metaclass-mtagmap class) 
			 (* amount (mm-metaclass-len class)))))

(defun mm-metaclass-object-instantiator (class)
  (typecase (slot-value class 'object-instantiator)
    (null
     (the mm-object-instantiator
       (compile nil
		`(lambda (index)
		   (declare (optimize speed) (type mindex index))
		   (make-instance ,class '%ptr (make-mptr ,(mm-metaclass-tag class) index))))))
    (list (let ((f (first (slot-value class 'object-instantiator)))) 
	    (or (ignore-errors (alexandria:ensure-function f)) f)))))

(defun setup-mtagmap-for-metaclass (class)
  (when (zerop (mm-metaclass-len class))
    (warn "Pointlessly memory mapping a class with zero length objects: ~A" class))
  (with-slots (tag mtagmap)
      class
    (unless tag
      (let ((existing
	     (loop for m across *mtagmaps*
		   for a from 0
		   thereis 
		   (when (and m (equalp (class-name class) (class-name (mtagmap-class m))))
		     (assert (= (mm-metaclass-len (mtagmap-class m)) (mm-metaclass-len class)))
		     a))))
	(setf tag (or existing
		      (next-available-tag)))

	(assert tag (*mtagmaps*) "No more tags available (too many types defined in the memory mapped database).")))

    (unless (mtagmap tag)
      (setf (mtagmap tag)
	    (make-mtagmap))
      (setf (mtagmap-layout (mtagmap tag)) (mm-metaclass-slot-layout class)))

    (assert-class-slot-layout class (mtagmap-layout (mtagmap tag)))


    (setf mtagmap (mtagmap tag) 
	  (mtagmap-class mtagmap) class
	  (mtagmap-object-instantiator mtagmap) (mm-metaclass-object-instantiator class)))
  
  class)

(defmethod initialize-instance :before ((instance mm-object) &rest initargs)
  (declare (optimize speed) (dynamic-extent initargs))
  (cond ((eq '%ptr (first initargs)) ;;; XXX this is for speed; if %ptr is given it must be first
	 (setf (%ptr instance) 
	       (second initargs)))
	(t
	 (let ((class (class-of instance)))
	   (assert-class-slot-layout class (mm-metaclass-slot-layout class))
	   (mtagmap-check (mm-metaclass-mtagmap class))
	   (setf (%ptr instance) (mm-metaclass-alloc class))
	   
    ;;; XXX this is a horrible hack because we don't support unbound slots
    ;;; not in shared-initialize because that is more likely to destroy the system's optimizations
	   (let ((slot-definitions (class-slots class)))
	     (loop for s in slot-definitions do
		   (when (and (slot-definition-memory-mapped s)
			      (slot-definition-initfunction s))
		     (unless (get-properties initargs (slot-definition-initargs s))
		       (setf (slot-value-using-class class instance s) (funcall (slot-definition-initfunction s))))))))))
  instance)

(defun slot-definition-always-boundp (&rest args)
  (declare (ignore args))
  t)

(defun mm-effective-slot-definition-lambda-forms (slotd)
  (let* (
	 (offset (slot-value slotd 'offset))
	 (type 
	  (if (stored-cffi-type (slot-definition-type slotd))
	      (slot-definition-type slotd)
	      'mm-box))
	 (raw-access-form
	  `(d ,(if (zerop offset) `(mm-object-pointer object) 
		   `(cffi:inc-pointer (mm-object-pointer object) ,offset)) 0 
	      ,(if (eq type 'mm-box) 
		   'mptr
		   type)))
	 (read-form
	  (if (eq type 'mm-box)
	      `(mptr-to-lisp-object ,raw-access-form)
	      raw-access-form))
	 (declare-form
	  `(declare (optimize speed))))
    (values
     `(lambda (object) 
	,declare-form
	,read-form)
     `(lambda (new-val object)
	,declare-form
	(let ,(when (eq type 'mm-box)
		    `((new-val (lisp-object-to-mptr new-val)))) ;; note that (lisp-object-to-mptr new-val) can invalidate the current pointer
	  (setf ,raw-access-form new-val))
	new-val))))

(defun mm-effective-slot-definition-setup (slotd)
  (with-slots (offset)
      slotd
    (check-type offset mindex)
    
    (multiple-value-bind (reader writer)
	(mm-effective-slot-definition-lambda-forms slotd)
      (setf (slot-definition-reader-function slotd)
	    (compile nil
		     (eval reader))
	    (slot-definition-writer-function slotd)
	    (compile nil
		     (eval writer))
	 #- (and)
	 (slot-definition-boundp-function #'slot-definition-always-boundp)))
    (values)))


(defmacro with-raw-slot ((slotname classname &key (accessor-name slotname)) object-pointer &body body &environment env)
  (let* ((class (find-class classname t env))
	 (slotd (or (find slotname (class-slots class) :key #'slot-definition-name) 
		    (error "Class ~A has no slot ~A" classname slotname)))
	 (offset (slot-value slotd 'offset))
	 (slot-type (slot-definition-type slotd))
	 (d-slot-type (if (stored-cffi-type slot-type) slot-type 'mptr)))
    (alexandria:with-gensyms (apointer)
      `(let ((,apointer (cffi:inc-pointer ,object-pointer ,offset)))
	 (declare (type machine-pointer ,apointer))
	 (symbol-macrolet ((,accessor-name
			    (d ,apointer 0 ,d-slot-type)))
	   ,@body)))))

(defmacro with-pointer-slots (slotnames (object-pointer classname) &body body)
  (alexandria:once-only (object-pointer)
    (labels ((r (slotnames)
	       (if slotnames
		   `(with-raw-slot (,(first slotnames) ,classname) 
			,object-pointer
		      ,(r (rest slotnames)))
		   `(locally ,@body))))
      (r slotnames))))

(defun mm-metaclass-slot-layout (class)
  (let ((slots (class-slots class)))
    (loop for s in slots 
	  when (slot-exists-p s 'offset)
	  collect `(,(slot-definition-name s) ,(slot-value s 'offset) ,(stored-type-size (slot-definition-type s))))))

(defun layout-compatible-p (a b)
  (flet ((sort-layout (layout)
	   (sort (copy-list layout) #'> :key #'second)))
    (equalp 
     (mapcar #'rest (sort-layout a)) 
     (mapcar #'rest (sort-layout b)))))

(defun assert-class-slot-layout (class layout)
  (assert (layout-compatible-p layout (mm-metaclass-slot-layout class)) ()
	   "Layout for class ~A has changed from ~A" class layout)
  (when (mm-metaclass-mtagmap class)
    (assert (eq class (mtagmap-class (mm-metaclass-mtagmap class))))
    (assert (eq (mtagmap (mm-metaclass-tag class)) (mm-metaclass-mtagmap class)))
    (mtagmap-check (mm-metaclass-mtagmap class))))

(defmacro check-class-slot-layout (classname &optional (layout (mm-metaclass-slot-layout (find-class classname))))
  `(assert-class-slot-layout (find-class ',classname) ',layout))

(defmacro defmmclass (name direct-supers direct-slots &rest options)
  `(progn
     (eval-when (:execute :compile-toplevel :load-toplevel)
       (finalize-inheritance (defclass ,name ,direct-supers ,direct-slots 
			       ,@(if (assoc :metaclass options) 
				     options
				     `((:metaclass mm-metaclass) ,@options)))))

     (eval-when (:load-toplevel :execute)
       (check-class-slot-layout ,name))

     (find-class ',name)))
