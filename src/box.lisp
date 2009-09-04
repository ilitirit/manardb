(in-package #:manardb)

(with-constant-tag-for-class (tag mm-box)
  (defun-speedy unbox-box (index)
    (with-pointer-slots (ptr)
	((mpointer tag index) mm-box)
      (mptr-to-lisp-object ptr))))

(defun-speedy box-cons (cons)
  ;;; XXX this is a lost cause anyway so don't bother making it efficient?
  (ptr (make-instance 'mm-cons :car (car cons) :cdr (cdr cons))))

(with-constant-tag-for-class (tag mm-cons)
  (check-class-slot-layout mm-cons)

 (defun unbox-cons (index)
   (with-pointer-slots (a b)
       ((mpointer tag index) mm-cons)
     (cons (mptr-to-lisp-object a) (mptr-to-lisp-object b)))))

(defmacro prop-for-mm-symbol (sym)
  `(get ,sym 'mm-symbol))

(defvar *stored-symbols* nil)

(with-constant-tag-for-class (tag mm-symbol)
  (check-class-slot-layout mm-symbol)

  (declaim (ftype (function (symbol) (mptr)) uncached-box-symbol box-symbol))
  (defun uncached-box-symbol (object)
    (declare (type symbol object)
	     (optimize speed))
    (let* 
	((pkg (symbol-package object)) 
	 (mptr (ptr 
		(make-instance 'mm-symbol 
			       :package 
			       (if pkg
				   (package-name pkg)
				   nil)
			       :symbol
			       (symbol-name object)))))
      (assert (not (zerop mptr)))
      (when pkg 
	(push object *stored-symbols*)
	(setf (prop-for-mm-symbol object) mptr))
      mptr))

  (defun-speedy box-symbol (object)
    (declare (type symbol object))
    (cond ((not object)
	   (make-mptr tag 0))
	  (t
	   (or (prop-for-mm-symbol object)
	       (uncached-box-symbol object)))))

 (defun-speedy unbox-symbol (index)
   (unless (zerop index)
     (with-pointer-slots (package-name symbol-name)
	 ((mpointer tag index) mm-symbol)
       (let ((package-name (mptr-to-lisp-object package-name))
	     (symbol-name (mptr-to-lisp-object symbol-name)))
	 (let ((sym
		(if package-name
		    (intern symbol-name (find-package package-name))
		    (make-symbol symbol-name))))
	   (unless (prop-for-mm-symbol sym)
	     (push sym *stored-symbols*)
	     (setf (prop-for-mm-symbol sym) 
		   (make-mptr tag index)))
	   sym))))))

(defun-speedy tag-general-unbox-array (tag index)
  (with-pointer-slots (length base)
    ((mpointer tag index) mm-array)
    (unbox-array-internal (mptr-tag base) (mptr-index base) length)))


(with-constant-tag-for-class (tag mm-array)
  (defun unbox-array (index)
    (tag-general-unbox-array tag index)))

(with-constant-tag-for-class (tag mm-string)
  (defun unbox-string (index)
    (cl-irregsexp.bytestrings:force-string  
     (cl-irregsexp.bytestrings:force-byte-vector 
      (tag-general-unbox-array tag index)))))

(define-box-array internal-box-string boxed-byte (unsigned-byte 8) :array-class mm-string)

(defun-speedy box-string (string)
  (internal-box-string (cl-irregsexp.bytestrings:force-simple-byte-vector string)))

(defun-speedy walk-array (mptr func)
  (macrolet ((base-offset ()
	       (ash (mm-slot-offset 'mm-array 'base) +mtag-bits+)))
   (with-pointer-slots (length base)
       ((mptr-pointer mptr) mm-array)
     (let ((length length))
       (unless (zerop length)
	 (funcall func base (+ mptr (base-offset)) length))))))



;;; XXXX these things are really awful and should be redone much more nicely
(defmacro direct-slot-mptr (class object slot)
  `(with-pointer-slots (,slot)
       ((mm-object-pointer ,object) ,class)
     ,slot))

(defmacro set-direct-slot-mptr (class object slot new-value)
  `(with-pointer-slots (,slot)
       ((mm-object-pointer ,object) ,class)
     (setf ,slot ,new-value)))

(defsetf direct-slot-mptr set-direct-slot-mptr)

(defmacro direct-slot-numeric-maref (class object slot element-type index)
  `(with-pointer-slots (base)
       ((mptr-pointer (direct-slot-mptr ,class ,object ,slot)) marray)
     (d (mptr-pointer base) ,index ,element-type)))

(defmacro set-direct-slot-numeric-maref (class object slot element-type index new-value )
  `(with-pointer-slots (base)
       ((mptr-pointer (direct-slot-mptr ,class ,object ,slot)) marray)
     (setf (d (mptr-pointer base) ,index ,element-type) ,new-value)))

(defsetf direct-slot-numeric-maref set-direct-slot-numeric-maref)

(defun-speedy meq (a b)
  (or (eq a b) 
      (and (typep a 'mm-object) (typep b 'mm-object)
	   (= (ptr a) (ptr b)))))