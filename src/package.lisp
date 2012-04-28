;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(defpackage :manardb
  (:nicknames :mm)
  (:use :closer-common-lisp :iterate
    :hu.dwim.def
    :hu.dwim.defclass-star)
  (:export
    :*mtagmaps*
    :*mmap-pathname-defaults*
    :*mmap-base-pathname*
    :*mmap-may-allocate*
    :mptr
    :meq
    :mm-metaclass
    :mm-object
    :defmmclass
    :close-all-mmaps
    :open-all-mmaps
    :wipe-all-mmaps
    :print-all-mmaps
    :doclass
    :dosubclasses
    :mm-subclasses
    :retrieve-all-instances
    :count-all-instances
    :mptr-to-lisp-object
    :lisp-object-to-mptr
    :lisp-object-to-mptr-impl
    :marray
    :make-marray
    :marray-ref
    :marray-length
    :marray-to-list
    :list-to-marray
    :index-of-marray
    :in-marray
    :gc
    :rewrite-gc
    :make-mm-fixed-string
    :mm-fixed-string-value
    :with-transaction
    :use-mmap-dir
    :clean-mmap-dir
    :clear-caches
    :clear-caches-hard    
    :with-object-cache
    :with-cached-slots
    :direct-slot-numeric-maref
    
    ))


