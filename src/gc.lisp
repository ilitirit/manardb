(in-package #:manardb)

(defun gc-compact (offsets-table)
    (loop for mtagmap across *mtagmaps*
	  for offsets across offsets-table
	  for tag from 0
	  when offsets
	  do
	  (let ((elem-len (mtagmap-elem-len mtagmap)) (last-offset (mtagmap-first-index mtagmap)))
	    (loop for offset across offsets
		  for index from (mtagmap-first-index mtagmap) by elem-len
		  do
		  (unless (zerop offset)
		    (assert (= last-offset offset))
		    (osicat-posix:memmove (mpointer tag offset) (mpointer tag index) elem-len)
		    (setf last-offset (+ offset elem-len))))
	    (setf (mtagmap-next mtagmap) last-offset))))

(defun gc-calc-new-offsets (mtagmap table)
  (when table
    (let ((offsets (make-array (length table) :element-type 'mindex :initial-element 0)) (next (mtagmap-first-index mtagmap)) (elem-len (mtagmap-elem-len mtagmap)))
      (loop for refs across table
	    for i from 0
	    do (when refs
		 (setf (aref offsets i) next)
		 (incf next elem-len)))
      offsets)))

(defun gc-rewrite-pointers-and-compact (refs-table)
  (let ((offsets-table (map 'vector 'gc-calc-new-offsets *mtagmaps* refs-table)))
    (loop for mtagmap across *mtagmaps*
	  for tag from 0
	  for table across refs-table 
	  for offsets across offsets-table
	  when table
	  do (loop for pos from 0
		   for refs across table
		   for new-location across offsets
		   when refs
		   do 
		   (labels ((up (ref)
			      (declare (type mptr ref))
			      (unless (zerop ref)
				(setf (d (mptr-pointer ref) 0 mptr) (make-mptr tag new-location))))) ;;; XXX only write if necessary so that pages are not pointlessly dirtied
		      (typecase refs
			(array
			 (loop for r across refs do (up r)))
			(t
			 (up refs))))))
    (gc-compact offsets-table)))

(defun gc (root-objects-sequence &key verbose (collect-and-compact t) (truncate t))
  (declare (optimize speed))
  (let ((refs-table (map 'vector (lambda (m) 
				   (unless (or (not m) (mtagmap-closed-p m))
				     ;;; also tried with a hash-table but in comparison it is very very slow on Allegro
				     (make-array (mtagmap-count m) :initial-element nil))) 
			 *mtagmaps*)))
    (macrolet ((r (mptr)
		 (check-type mptr symbol)
		 `(aref (aref refs-table (mptr-tag ,mptr)) (mtagmap-elem-pos (mtagmap (mptr-tag ,mptr)) (mptr-index ,mptr)) )
		 ))
	(labels ((add-ref (mptr referrer)
		   (symbol-macrolet ((ref (r mptr)))
		     (let ((rref ref))
		      (typecase rref
			(array
			 (when (zerop referrer)
			   (return-from add-ref))
			 (vector-push-extend referrer rref))
			(null
			 (setf ref referrer))
			(t
			 (cond ((zerop rref)
				(setf ref referrer))
			       ((eql rref referrer))
			       (t
				(setf ref (make-array 2 :adjustable t :fill-pointer 0 
						      :initial-contents (list rref referrer)
						      :element-type 'mptr)))))))))
		 (walk-ref (mptr &optional (referrer 0))
		   (unless (zerop mptr)
		     (let ((first-time (not (r mptr))))
		      (add-ref mptr referrer)
		      (when first-time
			(let ((walker (mtagmap-walker (mtagmap (mptr-tag mptr)))))
			  (when walker
			    (funcall walker mptr #'walk-ref))))))))
	  (iter (for o in-sequence root-objects-sequence)
		(walk-ref o))
	  (when verbose
	    (loop for m across *mtagmaps*
		  for table across refs-table
		  do 
		  (when table
		    (format t "~A total ~D used ~D~&" 
			    (mtagmap-class m) (mtagmap-count m)
			    (count-if-not #'not table)
			    ))))
	  (when collect-and-compact
	    (gc-rewrite-pointers-and-compact refs-table))
	  (when truncate
	    (shrink-all-mmaps))
	  (values)))))

