(in-package #:manardb)

(defun rewrite-gc-walk (root-objects-sequence shared-tables new-mtagmaps)
  (declare (optimize speed))
  (macrolet ((vref (mptr)
	       `(gethash (mptr-index ,mptr) 
			 (aref (the simple-vector visited) (mptr-tag ,mptr)))))
    (iter 
      (for o in-sequence root-objects-sequence)
      (let ((visited (map 'vector (lambda (x table) (or table (when x (make-hash-table :test 'eql)))) 
			  new-mtagmaps shared-tables)))
	(declare (dynamic-extent visited))
	(labels ((walk-ref (mptr referrer)
		   (declare (type mptr mptr))
		   (cond ((zerop mptr) 0)
			 ((vref mptr))
			 (t
			  (let* ((tag (mptr-tag mptr)) 
				 (mtagmap (aref new-mtagmaps tag))
				 (len (mtagmap-elem-len mtagmap))
				 (new-index (mtagmap-alloc mtagmap len))
				 (new-mptr (make-mptr tag new-index))
				 (old-mptr mptr))

			    (osicat-posix:memcpy 
			     (cffi:inc-pointer (mtagmap-ptr mtagmap)
					       new-index)
			     (mptr-pointer mptr)
			     len)
				
			    (unless (zerop referrer) ;; XXX slight hack to stop the hash-tables growing large by only looking at the first element of an array
			      (setf (vref old-mptr) new-mptr))
			    (let ((walker (mtagmap-walker (mtagmap (mptr-tag mptr)))))
			      (when walker
				(labels ((reset-ref (mptr referrer)
					   (cond 
					     ((zerop referrer) (walk-ref mptr referrer))
					     (t
					      (let ((offset (- (mptr-index referrer) (mptr-index old-mptr)))
						    (new-mptr (walk-ref mptr referrer)))
						(setf (d (cffi:inc-pointer (mtagmap-ptr mtagmap)
									   (+ offset new-index)) 0 mptr)
						      new-mptr))))))
				  (declare (dynamic-extent #'reset-ref))
				  (funcall walker mptr #'reset-ref))))
				    
			    new-mptr)))))
	  (declare (dynamic-extent #'walk-ref))
	  (let ((mptr (force-mptr o)))
	    (walk-ref mptr mptr)))))))

(defun rewrite-gc-cleanup (new-mtagmaps new-files)
  (loop for new across new-mtagmaps
	for old across *mtagmaps*
	for new-file in new-files
	do 
	(when new
	  (mtagmap-close new)
	  (let ((old-file (mtagmap-default-filename old)))
	   (mtagmap-close old)
	   (osicat-posix:rename new-file old-file)
	   (mtagmap-open old)))))

(defun rewrite-gc (root-objects-sequence &key verbose shared-classes (base-shared-classes '(mm-symbol mm-string mm-array marray mm-fixed-string)))
  (check-mmap-truncate-okay)
  (let* ((new-mtagmaps
	 (map '(vector mtagmap) (lambda (m)
			   (when (and m (not (mtagmap-closed-p m)))
			     (let ((m (copy-structure m)))
			       (mtagmap-detach m)
			       m)))
	      *mtagmaps*))
	 (shared-tables
	  (make-array (length *mtagmaps*) :initial-element nil))
	(new-files 
	 (loop for m across new-mtagmaps
	       collect 
	       (when m 
		 (make-pathname :type "rewrite" :defaults (mm-metaclass-pathname (mtagmap-class m)))))))
    (flet ((add-shared (seq)
	     (map nil (lambda (x)
			(setf (aref shared-tables (force-tag x)) (make-hash-table :test 'eql))) seq)))
      (add-shared base-shared-classes)
      (add-shared shared-classes))
    
    (unwind-protect
	 (progn
	   (loop for m across new-mtagmaps 
		 for f in new-files
		 do
		 (when m
		   (ignore-errors (delete-file f))
		   (mtagmap-open m :file f :finalize nil)))
	   (rewrite-gc-walk root-objects-sequence shared-tables new-mtagmaps)
	   (when verbose
	     (loop for new across new-mtagmaps
		   for old across *mtagmaps*
		   when new
		   do 
		   (format t "~A before ~D after ~D~&" 
			   (mtagmap-class old) (mtagmap-count old) (mtagmap-count new))))
	   (rewrite-gc-cleanup new-mtagmaps new-files))
      (loop for m across new-mtagmaps 
	    for f in new-files
	    do
	    (when m
	      (mtagmap-close m)
	      (ignore-errors (delete-file f)))))))
