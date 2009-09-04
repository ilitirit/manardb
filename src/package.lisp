(cl:defpackage #:manardb
  (:export 
   #:mptr
   #:mm-metaclass
   #:mm-object
   #:defmmclass
   #:*mmap-base-pathname*
   #:*mmap-may-allocate*
   #:close-all-mmaps
   #:open-all-mmaps
   #:wipe-all-mmaps
   #:print-all-mmaps
   #:doclass
   #:dosubclasses
   #:retrieve-all-instances
   #:count-all-instances

   #:mptr-to-lisp-object
   #:lisp-object-to-mptr
   #:lisp-object-to-mptr-impl

   #:marray
   #:make-marray
   #:marray-ref
   #:marray-length
   #:index-of-marray
   #:in-marray

   #:gc
   #:rewrite-gc

   #:make-mm-fixed-string
   #:mm-fixed-string-value

   #:with-transaction
   #:use-mmap-dir
   #:clean-mmap-dir

   #:with-object-cache
   #:with-cached-slots

   #:marray-to-list
   #:list-to-marray

   #:meq
   #:direct-slot-numeric-maref ;; XXXX to delete when we have time for something better
   )
;;  #+sbcl (:import-from #:sb-pcl #:reader-function #:writer-function)
  (:use #:iterate #:closer-common-lisp))
