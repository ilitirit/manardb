;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(defpackage :manardb
  (:nicknames :mm)
  (:use :closer-common-lisp :contextl :iterate)
  (:import-from :contextl :layer :layers :process-layered-access-slot-specification)
  (:export
    :*mmap*
    :*mtagmaps-may-mmap*
    :*allocate-base-pathname*

    :lisp-object-to-mptr-impl
    :lisp-object-to-mptr
    :mptr-to-lisp-object
    :mptr
    :meq
    
    :mm-object
    :defmmclass
    :mm-metaclass
    
    :gc
    :rewrite-gc

    :print-all-mmaps
    :close-all-mmaps
    :open-all-mmaps
    :wipe-all-mmaps

    :doclass
    :dosubclasses
    :mm-subclasses
    :count-all-instances
    :retrieve-all-instances

    :marray
    :make-marray
    :marray-ref
    :marray-length
    :marray-to-list
    :list-to-marray
    :index-of-marray
    :in-marray

    :make-mm-fixed-string
    :mm-fixed-string-value

    :ensure-manardb
    :clean-mmap-dir
    :clear-caches
    :clear-caches-hard    
    :with-object-cache
    :with-cached-slots
    :direct-slot-numeric-maref
    
    :mcons
    :mcar
    :mcdr
    :mcadr
    :mcddr
    :mconsp
    :empty
    :emptyp
    :mlist
    :as-list
    :mpush
    :mpop)



