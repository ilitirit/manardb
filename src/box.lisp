(in-package #:manardb)

(with-constant-tag-for-class (tag mm-box)
  (defun-speedy unbox-box (index)
    (with-pointer-slots (ptr)
	((mpointer tag index) mm-box)
      (mptr-to-lisp-object ptr))))

(defun box-cons (cons)
  ;;; XXX this is terrible anyway so don't bother making it efficient
  (%ptr (make-instance 'mm-cons :car (car cons) :cdr (cdr cons))))

(with-constant-tag-for-class (tag mm-cons)
  (check-class-slot-layout mm-cons)

 (defun unbox-cons (index)
   (with-pointer-slots (a b)
       ((mpointer tag index) mm-cons)
     (cons (mptr-to-lisp-object a) (mptr-to-lisp-object b)))))

(defmacro prop-for-mm-symbol (sym)
  `(get ,sym 'mm-symbol))

(with-constant-tag-for-class (tag mm-symbol)
  (check-class-slot-layout mm-symbol)

  (defun-speedy box-symbol (object)
   (declare (type symbol object))
   (case object
     ((nil) (make-mptr tag 0))
     (t
      (symbol-macrolet ((prop (prop-for-mm-symbol object)))
	(or prop
	    (let ((pkg (symbol-package object)))
	      (setf prop (%ptr 
			  (make-instance 'mm-symbol 
					 :package 
					 (if pkg
					     (package-name pkg)
					     nil)
					 :symbol
					 (symbol-name object))))))))))

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
	   (setf (prop-for-mm-symbol sym) (make-mptr tag index))
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
