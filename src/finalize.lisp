(in-package #:manardb)

(defun finalize-all-mtagmaps ()
  (loop for m across *mtagmaps* 
	when m
	do (setf (mtagmap-object-instantiator m) 
		 (mm-metaclass-object-instantiator (mtagmap-class m)))
	(check-type (mtagmap-object-instantiator m) function)))

(defun close-all-mtagmaps ()
  (loop for m across *mtagmaps* do
	(when m (mtagmap-close m))))

(defun open-all-mtagmaps ()
  (finalize-all-mtagmaps)
  (loop for m across *mtagmaps*
	when m do
	(let ((class (mtagmap-class m)))
	  (mtagmap-open m (mm-metaclass-filename class) 
			(* #x100000 (mm-metaclass-len class))))))


(define-lisp-object-to-mptr)

