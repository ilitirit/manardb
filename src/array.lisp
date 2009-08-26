(in-package #:manardb)

;; XXX doesn't work on specialised arrays
(defun-speedy marray-ref (marray i)
  (declare (type mindex i))
  (mptr-to-lisp-object (d (mptr-pointer (marray-base marray)) i mptr)))

(defun-speedy (setf marray-ref) (new marray i) 
  (declare (type mindex i))
  (setf (d (mptr-pointer (marray-base marray)) i mptr)
	(lisp-object-to-mptr new))
  new)

(with-constant-tag-for-class (element-tag mm-box) 
  (defun-speedy make-marray (length &key initial-element)
    (let ((marray (make-instance 'marray :length length 
				:base (make-mptr element-tag
						 (mtagmap-alloc (mtagmap element-tag) 
								(* length #.(stored-type-size 'mptr))))))
	  (initial-element (lisp-object-to-mptr initial-element)))
      (loop for i below length do
	    (setf (d (mptr-pointer (marray-base marray)) i mptr) initial-element))
      marray)))


(defclause-sequence in-marray index-of-marray
  :access-fn 'marray-ref
  :size-fn 'marray-length
  :sequence-type 'marray
  :element-type t
  :element-doc-string "Elements of an marray"
  :index-doc-string "Indices of marray")
