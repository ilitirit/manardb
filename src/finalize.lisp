(in-package #:manardb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for m across *mtagmaps* 
	when m
	do (setf (mtagmap-object-instantiator m) 
		 (mm-metaclass-object-instantiator (mtagmap-metaclass m)))
	(check-type (mtagmap-object-instantiator m) function)))

(define-lisp-object-to-mptr)

