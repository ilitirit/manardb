(in-package #:manardb)

(defmacro define-lisp-object-to-mptr ()
  `(defun-speedy lisp-object-to-mptr (obj)
       (typecase obj
	 (mm-object (%ptr obj))
	 (t (box-object obj)))))

(define-lisp-object-to-mptr) ;; should be redefined after box-object is
			   ;; defined, which needs many types to be
			   ;; defined, in a circular fashion

(defmacro with-constant-tag-for-class ((tagsym classname) &body body)
  (check-type tagsym symbol)
  (check-type classname symbol)
  (let ((class (find-class classname)))
    (ensure-finalize-inheritance class)
    (let ((tag (mm-metaclass-tag class)))
      (check-type tag mtag)
    
      `(progn
	 (eval-when (:load-toplevel :compile-toplevel :execute)
	   (assert (= ,tag ,(mm-metaclass-tag (find-class classname)))
		   () "The tag for classname ~A has changed; compiled code may be invalid" ',classname))
	 (symbol-macrolet ((,tagsym ,tag))
	   ,@body)))))

(defun-speedy force-mptr (obj)
  (etypecase obj
    (mptr obj)
    (mm-object (%ptr obj))))
		 
(defun-speedy mptr (obj)
  (force-mptr obj))

(defun-speedy force-tag (obj)
  (etypecase obj
    (mtag obj)
    (mtagmap (mm-metaclass-tag (mtagmap-class obj)))
    (symbol (mm-metaclass-tag (find-class obj)))
    (mm-metaclass (mm-metaclass-tag obj))
    (mm-object (mptr-tag (%ptr obj)))
    (mptr (mptr-tag obj))))

(defmethod finalize-inheritance :after ((class mm-metaclass))
  (setup-mtagmap-for-metaclass class)
  (setup-default-metaclass-functions class)
  class)

(defun setup-default-metaclass-functions (class)
  (with-slots (default-walker default-instantiator)
      class
    (setf default-walker
	  (let ((offsets (loop for slot in (class-slots class)
			       when (slot-definition-mmap-pointer-p slot)
			       collect (slot-value slot 'offset))))
	    (when offsets
	      (compile nil
		       `(lambda (mptr walker-func)
			  (declare (type mm-walk-func walker-func))
			  ,@(loop for offset in offsets collect
				  `(let ((p (+ mptr ,(ash offset +mtag-bits+))))
				     (funcall walker-func (dw (mptr-pointer p)) p 1)))))))

	  default-instantiator
	  (compile nil
		   `(lambda (index)
		      (declare (optimize speed) (type mindex index))
		      (make-instance ,class '%ptr (make-mptr ,(mm-metaclass-tag class) index))))))

  (loop for slot in (class-slots class) do
	(when (slot-definition-memory-mapped slot)
	  (mm-effective-slot-definition-setup slot))))

(defun mm-metaclass-filename (class)
  (assert (class-name class) (class) "Cannot mmap anonymous classes.") ; is possible but not implemented or sensible(?)
  (check-type (class-name class) symbol)
  
  (make-pathname 
   :name (flet ((clean (str)
		  (remove-if-not #'alphanumericp str)))
	   (let ((name (class-name class)))
	     (concatenate 'string (clean (package-name (symbol-package name))) 
			  "-" (clean (symbol-name name)))))))

(defun mm-metaclass-pathname (class)
  (merge-pathnames 
   (mm-metaclass-filename class)
   *mmap-pathname-defaults*))

(declaim (ftype (function (mm-metaclass &optional mindex) mptr) mm-metaclass-alloc))
(defun-speedy mm-metaclass-alloc (class &optional (amount 1))
  (declare (type mindex amount))
  (make-mptr (mm-metaclass-tag class) 
	     (mtagmap-alloc (mm-metaclass-mtagmap class) 
			 (* amount (mm-metaclass-len class)))))

(defun mm-metaclass-custom-function (class slot
				      &optional
				      (default-slot (let ((*package* #.*package*))
						      (alexandria:symbolicate 'default- slot))))
  (typecase (slot-value class slot)
    (null
     (slot-value class default-slot))
    (list (let ((f (first (slot-value class slot)))) 
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
		     a))))
	(setf tag (or existing
		      (next-available-tag)))

	(assert tag (*mtagmaps*) "No more tags available (too many types defined in the memory mapped database).")))

    (unless (mtagmap tag)
      (setf (mtagmap tag)
	    (make-mtagmap))
      (setf (mtagmap-layout (mtagmap tag)) (mm-metaclass-slot-layout class)))

    (assert-class-slot-layout class (mtagmap-layout (mtagmap tag)) :finalize nil)

    (setf mtagmap (mtagmap tag) 
	  (mtagmap-class mtagmap) class))

  
  class)

(defmethod initialize-instance :before ((instance mm-object) &rest initargs)
  (declare (optimize speed) (dynamic-extent initargs))
  (cond ((eq '%ptr (first initargs)) ;;; XXX this is for speed; if %ptr is given it must be first
	 (setf (%ptr instance) 
	       (second initargs)))
	(t
	 (let ((class (class-of instance)))
	   (setf (%ptr instance) (mm-metaclass-alloc class))
	   
    ;;; XXX this is a horrible hack because we don't support unbound slots
    ;;; not in shared-initialize because that is more likely to destroy the system's optimizations
	   (let ((slot-definitions (class-slots class)))
	     (loop for s in slot-definitions do
		   (when (and (slot-definition-memory-mapped s)
			      (slot-definition-initfunction s))
		     (unless (get-properties initargs (slot-definition-initargs s))
		       (setf (slot-value-using-class class instance s) 
			     (funcall (slot-definition-initfunction s))))))))))
  instance)

(defun slot-definition-always-boundp (&rest args)
  (declare (ignore args))
  t)

(defun slot-definition-mm-type (slotd)
  (if (stored-cffi-type (slot-definition-type slotd))
      (slot-definition-type slotd)
      'mm-box))

(defun mm-effective-slot-definition-lambda-forms (slotd)
  (let* (
	 (offset (slot-value slotd 'offset))
	 (type 
	  (slot-definition-mm-type slotd))
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


(defun mm-slot-offset (class slotname)
  (let* ((class (force-class class))
	 (slotd (find slotname (class-slots class) :key #'slot-definition-name)))
    (assert slotd)
    (assert (slot-definition-memory-mapped slotd))
    (slot-value slotd 'offset)))

(defmacro with-raw-slot ((slotname classname &key (accessor-name slotname)) object-pointer &body body &environment env)
  (let ((class (find-class classname t env)))
    (ensure-finalize-inheritance class)
    (let* (
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
	     ,@body))))))

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
  (ensure-finalize-inheritance class)
  (let ((slots (class-slots class)))
    (loop for s in slots 
	  when (slot-definition-memory-mapped s)
	  collect `(,(slot-definition-name s) ,(slot-value s 'offset) ,(stored-type-size (slot-definition-type s))
		     ,@(when (slot-definition-mmap-pointer-p s) `(:mmap-pointer t))))))

(defun layout-compatible-p (a b)
  (flet ((sort-layout (layout)
	   (sort (copy-list layout) #'> :key #'second)))
    (equalp 
     (mapcar #'rest (sort-layout a)) 
     (mapcar #'rest (sort-layout b)))))


(defun ensure-finalize-inheritance (class)
  (let ((class (force-class class)))
    (unless (class-finalized-p class)
      (finalize-inheritance class))))

(defun assert-class-slot-layout (class layout &key (finalize t))
  (when finalize 
    (ensure-finalize-inheritance class))
  (cassert (layout-compatible-p layout (mm-metaclass-slot-layout class)) ()
	   "Layout for class ~A has changed from ~A" class layout))

(defmacro check-class-slot-layout (classname &optional (layout (mm-metaclass-slot-layout (find-class classname))))
  `(assert-class-slot-layout (find-class ',classname) ',layout))

(defmacro defmmclass (name direct-supers direct-slots &rest options)
  `(progn
     (eval-when (:load-toplevel :execute :compile-toplevel) 
       (defclass ,name ,direct-supers ,direct-slots 
	 ,@(if (assoc :metaclass options) 
	       options
	       `((:metaclass mm-metaclass) ,@options)))
       (ensure-finalize-inheritance ',name))

     (eval-when (:execute)
       (check-class-slot-layout ,name))

     (find-class ',name)))



(defun tree-to-atoms-or-strings (tree)
  (typecase tree
    (integer tree)
    (null tree)
    (list
     (loop for i in tree collect (tree-to-atoms-or-strings i)))
    (t
     (princ-to-string tree))))

(defun mm-metaclass-schema (class)
  (with-standard-io-syntax
    (tree-to-atoms-or-strings
     (list
      (mm-metaclass-filename class)
      (mm-metaclass-tag class)
      (mm-metaclass-slot-layout class)))))


(defmacro with-cached-slots (slots instance &body body)
  (alexandria:with-unique-names (new-val)
   (let* ((tmps (loop for s in slots do (check-type s symbol) collect (gensym (symbol-name s))))
	  (funcs (loop for tmp in tmps collect tmp collect `(setf ,tmp)))
	  (ffuncs (loop for f in funcs collect `(function ,f))))
     (alexandria:once-only (instance)
       `(let ,(loop for tmp in tmps
		    for s in slots
		    collect `(,tmp (slot-value ,instance ',s))
		    )
	  (flet ,(loop for tmp in tmps for s in slots
		       collect
		       `(,tmp () ,tmp)
		       collect
		       `((setf ,tmp) (,new-val)
			 (setf ,tmp (setf (slot-value ,instance ',s) ,new-val))))
	    (declare (inline ,@funcs)
		     (ignorable ,@ffuncs)
		     (dynamic-extent ,@ffuncs))
	    (symbol-macrolet 
		,(loop for s in slots for tmp in tmps collect
		       `(,s (,tmp)))
	      ,@body)))))))

(defmethod print-object ((object mm-object) stream)
  (print-unreadable-object (object stream :type t)
    (let ((ptr (ptr object)))
      (format stream " M@~D(~D:~D)" ptr (mptr-tag ptr) (mptr-index ptr)))))