(in-package #:manardb)

(defclass mm-metaclass (standard-class)
  ((mtagmap :accessor mm-metaclass-mtagmap :initform nil)
   (tag :reader mm-metaclass-tag :initform nil)
   (len :initform 0 :accessor mm-metaclass-len)
   (object-instantiator :accessor mm-object-instantiator :initarg object-instantiator :initform nil))
  (:documentation "Metaclass for memory mapped objects."))

(defclass mm-object ()
  ((%ptr :type mptr :accessor %ptr :initarg %ptr))
  (:documentation "Base class for all memory mapped objects."))

(defun-speedy mm-object-pointer (mm-object)
  (assert (not (zerop (%ptr mm-object))))
  (mptr-pointer (%ptr mm-object)))

(defmethod shared-initialize :around ((class mm-metaclass)
				      slot-names
				      &rest all-keys
				      &key direct-superclasses
				      &allow-other-keys)
  ;;; Just to make sure that we inherit from mm-object
  (let ((parent (find-class 'mm-object)))
   (labels ((inherits-from (classes)
	      (loop for class in classes
		    thereis (or (subtypep class parent)
				(inherits-from (class-direct-subclasses class))))))
     (let ((all-keys (copy-list all-keys)))
       (setf (getf all-keys :direct-superclasses)
	     (if (inherits-from direct-superclasses)
		 direct-superclasses
		(cons parent direct-superclasses)))
       (apply #'call-next-method class slot-names all-keys)))))

(deftype mm-slot-definition-reader ()
  `(function (mm-object) t))

(deftype mm-slot-definition-writer ()
  `(function (t mm-object) t))

(defgeneric slot-definition-memory-mapped (slotd)
  (:method (slotd)
    (declare (ignorable slotd))))

(defclass mm-slot-definition (slot-definition)   
  ((persistent :initarg :persistent :reader slot-definition-memory-mapped :initform t)))

(defclass mm-effective-slot-definition (mm-slot-definition standard-effective-slot-definition)
  ((offset :initarg :offset :reader mm-slot-definition-offset)
   (writer-function :accessor slot-definition-writer-function)
   (reader-function :accessor slot-definition-reader-function)))

(defclass mm-direct-slot-definition (standard-direct-slot-definition mm-slot-definition)
  ())

(defmethod validate-superclass ((class mm-metaclass) (super standard-class))
  "Memory mapped classes may inherit from ordinary classes."
  t)

(defmethod validate-superclass ((class standard-class) (super mm-metaclass))
  "Ordinary classes may NOT inherit from memory mapped classes."
  nil)

(defmethod slot-definition-allocation ((slotd mm-effective-slot-definition))
  'memory)

(defmethod direct-slot-definition-class ((class mm-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'mm-direct-slot-definition))

(defvar *mop-hack-effective-slot-definition-class* nil) ;; as compute-effective-slot-definition-initargs is not available portably

(defmethod effective-slot-definition-class ((class mm-metaclass) &rest initargs)
  (declare (ignore initargs))
  (or
   *mop-hack-effective-slot-definition-class*
   (call-next-method)))

(defmethod compute-slots :before ((class mm-metaclass))
  (with-slots (len)
      class
    (setf len 0)))

(defmethod compute-effective-slot-definition :around ((class mm-metaclass) name dslotds)
  (declare (ignorable name))
  (let ((last-dslot (first (last (remove-if 'not dslotds)))))
    (let ((*mop-hack-effective-slot-definition-class*
	   (when (slot-definition-memory-mapped last-dslot) 
	       (find-class 'mm-effective-slot-definition))))
      (let ((eslot (call-next-method)))
	(when (slot-definition-memory-mapped eslot)
	  (let ((type (slot-definition-type eslot)))
	   (with-slots (len)
	       class
	     (setf (slot-value eslot 'offset) len)
	     (incf len (stored-type-size type)))))
       eslot))))

(defmethod slot-value-using-class ((class mm-metaclass) (object mm-object) (slotd mm-effective-slot-definition))
  (declare (ignorable class))
  (funcall (the mm-slot-definition-reader (slot-definition-reader-function slotd)) object))

(defmethod (setf slot-value-using-class) (new-value (class mm-metaclass) (object mm-object) (slotd mm-effective-slot-definition))
  (declare (ignorable class))
  (funcall (the mm-slot-definition-writer (slot-definition-writer-function slotd)) new-value object))

(defmethod slot-boundp-using-class ((class mm-metaclass) (object mm-object) (slotd mm-effective-slot-definition))
  (declare (ignorable class object slotd))
  t)
(defmethod slot-makunbound-using-class ((class mm-metaclass) (object mm-object) (slotd mm-effective-slot-definition))
  (declare (ignorable class object slotd))
  (error "Memory mapped slots cannot be unbound."))


