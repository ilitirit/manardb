(in-package #:manardb)

(defmmclass mm-fixed-string (mm-string)
  ((cropped-length :type mindex :initform 0))
  (walker walk-array))

(defun mm-fixed-string-uncropper (string original-length)
  (declare (ignore original-length))
  (concatenate 'string string "..."))

(defvar *mm-fixed-string-uncropper* 'mm-fixed-string-uncropper)

(defun mm-fixed-string-value (mfs)
  (with-pointer-slots (cropped-length length)
      ((mm-object-pointer mfs) mm-fixed-string)
    (let ((base-string (cl-irregsexp.bytestrings:force-string  
			  (subseq
			   (cl-irregsexp.bytestrings:force-byte-vector 
			    (tag-general-unbox-array (mptr-tag (%ptr mfs)) (mptr-index (%ptr mfs))))
			   0
			   (min cropped-length length)))))
      (if (> cropped-length length)
	  (funcall *mm-fixed-string-uncropper* base-string cropped-length)
	  base-string))))

(with-constant-tag-for-class (element-tag boxed-byte) 
  (defun-speedy make-mm-fixed-string (length &key value)
    (let ((mfs (make-instance 'mm-fixed-string 
				 :length length 
				 :base (make-mptr element-tag
						  (mtagmap-alloc (mtagmap element-tag) 
								 (* length #.(stored-type-size '(unsigned-byte 8))))))))
      (when value
	(mm-fixed-string-store mfs value))
      mfs)))

(defun mm-fixed-string-store (mfs string)
  (with-pointer-slots (cropped-length length base)
      ((mm-object-pointer mfs) mm-fixed-string)
    (let ((bv (cl-irregsexp.bytestrings:force-byte-vector string)) (ptr (mptr-pointer base)))
      (setf cropped-length (length bv))
      (loop for x across bv
	    for i below length
	    do (setf (d ptr i (unsigned-byte 8)) x))))
  mfs)

(defun (setf mm-fixed-string-value) (string mfs)
  (mm-fixed-string-store mfs string)
  string)

