(in-package #:manardb)

;; XXX doesn't work on specialised arrays
(defun-speedy marray-ref (marray i)
  (declare (type mindex i))
  (mptr-to-lisp-object (dw (mptr-pointer (marray-base marray)) i)))

(defun-speedy (setf marray-ref) (new marray i) 
  (declare (type mindex i))
  (let ((new (lisp-object-to-mptr new)))
    (setf (dw (mptr-pointer (marray-base marray)) i)
	  new))
  new)

(with-constant-tag-for-class (element-tag mm-box) 
  (defun-speedy make-marray (length &key initial-element (initial-contents nil initial-contents-p))
    (let ((marray (make-instance 'marray :length length 
				:base (make-mptr element-tag
						 (mtagmap-alloc (mtagmap element-tag) 
								(* length #.(stored-type-size 'mptr))))))
	  (initial-element (lisp-object-to-mptr initial-element))
	  (initial-contents (mapcar #'lisp-object-to-mptr initial-contents)))
      (let ((ptr (mptr-pointer (marray-base marray))))
	(if initial-contents-p
	    (loop for i below length 
		  for n in initial-contents
		  do (setf (dw ptr i) n))
	    (loop for i below length do
		  (setf (dw ptr i) initial-element))))
      marray)))


(defclause-sequence in-marray index-of-marray
  :access-fn 'marray-ref
  :size-fn 'marray-length
  :sequence-type 'marray
  :element-type t
  :element-doc-string "Elements of an marray"
  :index-doc-string "Indices of marray")


(defun marray-to-list (marray)
  (when marray
    (iter (for c in-marray marray)
	  (collect c))))
(defun list-to-marray (list)
  (when list
    (make-marray (length list) :initial-contents list)))
