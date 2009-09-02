(in-package #:manardb)

(defun rewrite-gc-walk (root-objects-sequence new-mtagmaps)
  (declare (optimize debug safety))
  (iter (for o in-sequence root-objects-sequence)
	(let ((visited (make-hash-table :test 'eql)))
	  (declare (dynamic-extent visited))
	  (labels ((walk-ref (mptr &optional (referrer 0))
		     (declare (ignore referrer))
		     (cond ((zerop mptr) 0)
			   ((gethash mptr visited))
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
				  (setf (gethash old-mptr visited) new-mptr)
				  (let ((walker (mtagmap-walker (mtagmap (mptr-tag mptr)))))
				    (when walker
				      (labels ((reset-ref (mptr referrer)
						 (cond 
						   ((zerop referrer) (walk-ref mptr))
						   (t
						(let ((offset (- (mptr-index referrer) (mptr-index old-mptr)))
						      (new-mptr (walk-ref mptr)))
						  (setf (d (cffi:inc-pointer (mtagmap-ptr mtagmap)
									     (+ offset new-index)) 0 mptr)
							new-mptr))))))
					(declare (dynamic-extent #'reset-ref))
					(funcall walker mptr #'reset-ref))))
				    
				  new-mptr)))))
	    (walk-ref (force-mptr o))))))

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

(defun rewrite-gc (root-objects-sequence &key verbose)
  (check-mmap-truncate-okay)
  (let* ((new-mtagmaps
	 (map '(vector mtagmap) (lambda (m)
			   (when (and m (not (mtagmap-closed-p m)))
			     (let ((m (copy-structure m)))
			       (mtagmap-detach m)
			       m)))
	      *mtagmaps*))
	(new-files 
	 (loop for m across new-mtagmaps
	       collect 
	       (when m 
		 (make-pathname :type "rewrite" :defaults (mm-metaclass-pathname (mtagmap-class m)))))))
    (unwind-protect
	 (progn
	   (loop for m across new-mtagmaps 
		 for f in new-files
		 do
		 (when m
		   (ignore-errors (delete-file f))
		   (mtagmap-open m :file f :finalize nil)))
	   (rewrite-gc-walk root-objects-sequence new-mtagmaps)
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
