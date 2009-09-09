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
