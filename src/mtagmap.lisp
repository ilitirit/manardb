(in-package #:manardb)

(defun-speedy mtagmap-byte (mtagmap index)
  (declare (type mindex index))
  (d (mtagmap-ptr mtagmap) index))

(defun-speedy (setf mtagmap-byte) (val mtagmap index)
  (declare (type mindex index) (type fixnum val))
  (setf (d (mtagmap-ptr mtagmap) index) (logand #xff val)))

(declaim (ftype (function (mtagmap mindex) word) mtagmap-word))
(defun-speedy mtagmap-word (mtagmap windex)
  (declare (type mindex windex))
  (d (mtagmap-ptr mtagmap) windex word))

(declaim (ftype (function (word mtagmap mindex) word) (setf mtagmap-word)))
(defun-speedy (setf mtagmap-word) (val mtagmap windex)
  (declare (type mindex windex))
  (declare (type (unsigned-byte 64) val))
  (setf (d (mtagmap-ptr mtagmap) windex word) val))

(defmacro mtagmap-next (mtagmap)
  `(mtagmap-word ,mtagmap 0))

(defun-speedy mtagmap-first-index (mtagmap)
  (declare (ignore mtagmap))
  +word-length+)
(defun-speedy mtagmap-last-index (mtagmap)
  (mtagmap-next mtagmap))
(defun-speedy mtagmap-elem-len (mtagmap)
  (mm-metaclass-len (mtagmap-metaclass mtagmap)))


(defun make-mtagmap-from-file (file &optional (min-bytes #x1000000))
  (incf min-bytes +word-length+)
  (let ((pagesize (osicat-posix:getpagesize)))
    (setf min-bytes (* pagesize (ceiling min-bytes pagesize))))

  (let ((fd (osicat-posix:open file (logior osicat-posix:O-CREAT osicat-posix:O-RDWR))))
    (unwind-protect
	 (let ((bytes (osicat-posix:stat-size (osicat-posix:fstat fd))))
	   (when (> min-bytes bytes)
	     (osicat-posix:ftruncate fd min-bytes)
	     (setf bytes min-bytes))

	   (assert (>= bytes +word-length+))
	   
	   (let ((ptr (osicat-posix:mmap 
					(cffi:null-pointer) bytes
					(logior osicat-posix:PROT-READ osicat-posix:PROT-WRITE)
					osicat-posix:MAP-SHARED ;; XXX change this?
					fd
					0)))
	     (unwind-protect
		  (let ((mtagmap (make-mtagmap :fd fd 
					 :ptr ptr
					 :len bytes)))
		    (when (zerop (mtagmap-next mtagmap))
		      (setf (mtagmap-next mtagmap) +word-length+))
		    (assert (>= (mtagmap-next mtagmap) (mtagmap-first-index mtagmap)))
		    (setf fd nil ptr nil)
		    mtagmap)
	       (when ptr 
		 (osicat-posix:munmap ptr bytes)))))
      (when fd 
	(osicat-posix:close fd)))))

(defun-speedy mtagmap-alloc (mtagmap bytes)
  (declare (type mindex bytes))
  (symbol-macrolet ((len (mtagmap-len mtagmap)))
    (let ((next (mtagmap-next mtagmap)))
      (loop while (> (+ next bytes) len)
	    do
	    (assert (>= len next))
	    (let ((new-len (* 2 len)))
	      (osicat-posix:ftruncate (mtagmap-fd mtagmap) new-len)
	      (setf (mtagmap-ptr mtagmap)
		    (osicat-posix:mremap (mtagmap-ptr mtagmap) len new-len osicat-posix:MREMAP-MAYMOVE)
		    len new-len)))
      (incf (mtagmap-next mtagmap) bytes)
      next)))

(defun mtagmap-check-read (mtagmap)
  (loop for i below (mtagmap-len mtagmap)
	summing (mtagmap-byte mtagmap i)))

(defun mtagmap-check-invert (mtagmap)
  (loop for i below (mtagmap-len mtagmap)
	for c = (mtagmap-byte mtagmap i)
	do (setf (mtagmap-byte mtagmap i) (lognot c))))

(defun mtagmap-check-write (mtagmap)
  (mtagmap-check-invert mtagmap)
  (mtagmap-check-invert mtagmap))

(defun mtagmap-close (mtagmap)
  (check-type mtagmap mtagmap)
  (let ((fd (mtagmap-fd mtagmap))
	(ptr (mtagmap-ptr mtagmap))
	(len (mtagmap-len mtagmap)))

    (setf (mtagmap-fd mtagmap) -1
	  (mtagmap-len mtagmap) 0
	  (mtagmap-ptr mtagmap) (cffi:null-pointer))

    (unwind-protect
	 (unless (cffi:null-pointer-p ptr)
	   (osicat-posix:munmap ptr len))
      (unless (minusp fd)
	(osicat-posix:close fd))))
  mtagmap)

(defun close-all-mtagmaps ()
  (loop for i below (length *mtagmaps*)
	do 
	(symbol-macrolet ((m (aref *mtagmaps* i))) 
	  (when m (mtagmap-close m) (setf m nil)))))
