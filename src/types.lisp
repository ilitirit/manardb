(in-package #:manardb)

(defmmclass mm-symbol ()
  ((package-name :initarg :package)
   (symbol-name :initarg :symbol))
  (object-instantiator unbox-symbol))

(defmmclass mm-array ()
  ((length :type mindex :initarg :length)
   (base :type mptr :initarg :base))
  (object-instantiator unbox-array))

(defmmclass mm-string (mm-array)
  ()
  (object-instantiator unbox-string))

(defmmclass mm-cons ()
  ((a :initarg :car)
   (b :initarg :cdr))
  (object-instantiator unbox-cons))

(defmmclass mm-box ()
  ((ptr))
  (object-instantiator unbox-box))




(eval-when (:compile-toplevel :load-toplevel)
  (defun specialized-class-array-boxer-name (classname)
    (alexandria:symbolicate classname '-array-boxer))

  (defun generate-boxed-numeric-type (name &key type)
    (let ((unboxer (alexandria:symbolicate 'unbox- name)))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel)
	   (defmmclass ,name ()
	       ((value :type ,type))
	     (object-instantiator ,unboxer)))
	 (with-constant-tag-for-class (tag ,name)
	   (defun-speedy ,unboxer (index)
	     (d (mpointer tag index) 0 ,type)))
	 (define-box-array ,(specialized-class-array-boxer-name name) ,name ,type)))))


(defmacro define-boxed-numeric-types (&rest typespecs)
  (let (types)
    `(progn 
       ,@(loop for typespec in typespecs
	       collect 
	       (destructuring-bind (name &optional (type name))
		   (alexandria:ensure-list typespec)
		 (let ((name (alexandria:symbolicate 'boxed- name)))
		   (push `(,name . ,type) types)
		   (generate-boxed-numeric-type name :type type))))

       (macrolet ((later ()
		    (generate-boxer ',(reverse types))))
	 (later)))))

(defun-speedy unbox-array-internal-general (elem-tag elem-index len)
  (declare (type mtag elem-tag) (type mindex elem-index) (type mindex len))
  (let* ((mtagmap (mtagmap elem-tag))
	 (class (mtagmap-class mtagmap))
	 (ilen (mm-metaclass-len class))
	 (instantiator (mtagmap-object-instantiator mtagmap))
	 (array (make-array len)))
    (declare (type mm-object-instantiator instantiator))
    (loop for i below len
	  for index from elem-index by ilen
	  do (setf (aref array i) (funcall (the mm-object-instantiator instantiator) index)))
    array))

(eval-when (:compile-toplevel :load-toplevel)
  (defun generate-boxer (types)
    `(progn
       (defun-speedy box-object (object)
	 (etypecase object
	   ,@(loop for (class . type) in types
		   collect
		   `(,type
		     ,(let* ((class (find-class class)) 
			     (tag (mm-metaclass-tag class)))
			    `(let ((index (mtagmap-alloc (mtagmap ,tag) ,(mm-metaclass-len class))))
			       (setf (d (mpointer ,tag index) 0 ,type) object)
			       (make-mptr ,tag index)))))
	   (symbol (box-symbol object))
	   (string (box-string object))
	   (array (locally
		      (declare (notinline box-array))
		    (box-array object)))
	   (cons (box-cons object))))

       (defun-speedy unbox-array-internal (elem-tag elem-index len)
	 (declare (type mtag elem-tag) (type mindex elem-index) (type mindex len))
	 (case elem-tag
	   ,@(loop for (classname . type) in types
		   for class = (find-class classname)
		   for tag = (mm-metaclass-tag class)
		   collect 
		   `(,tag 
		     (let ((array (make-array len :element-type ',type))
			   (pointer (mpointer ,tag elem-index)))
		       (declare (type (simple-array ,type) array))
		       (loop for i below len
			     do (setf (aref array i) (d pointer i ,type)))
		       array)))
	   (t (unbox-array-internal-general elem-tag elem-index len))))
  

	

       (defun-speedy box-array (object)
	 (assert (not (cdr (array-dimensions object))))
	 (declaim (notinline general-box-array))
	 (etypecase object
	   (simple-array 
	    (typecase object
	      ,@(loop for (class . type) in types
		      collect `((array ,type) (,(specialized-class-array-boxer-name class) object)))
	      (t (general-box-array object))))
	   (array
	    (general-box-array object)))))))




(defmacro define-box-array (array-boxer-name box-class lisp-type &key convertor (array-class 'mm-array))
  `(with-constant-tag-for-class (element-tag ,box-class) 
     (with-constant-tag-for-class (array-tag ,array-class)
      (defun-speedy ,array-boxer-name (array)
	(declare (type (simple-array ,lisp-type (*)) array))
	(let* ((len (length array))
	       (index (mtagmap-alloc (mtagmap element-tag) (* ,(mm-metaclass-len (find-class box-class)) len)))
	       (pointer (mpointer element-tag index)))
	  (loop for i below len do
		(setf (d pointer i ,(if (stored-cffi-type lisp-type) lisp-type 'mptr)) 
		      ,(if convertor
			   `(,convertor (aref array i))
			   `(aref array i))))
	  (let ((barray (mtagmap-alloc (mtagmap array-tag) ,(mm-metaclass-len (find-class array-class)))))
	    (with-pointer-slots (base length)
		((mpointer array-tag barray) ,array-class)
	      (setf base (make-mptr element-tag index)
		    length len)
	      (make-mptr array-tag barray))))))))

(define-box-array general-box-array mm-box t :convertor lisp-object-to-mptr)


(define-boxed-numeric-types
    (byte (unsigned-byte 8))
    double-float
  single-float
  (unsigned (unsigned-byte 64))
  (signed (signed-byte 64)))



