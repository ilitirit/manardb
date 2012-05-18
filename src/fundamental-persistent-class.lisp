;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental-persistent-class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass fundamental-persistent-class (mm-metaclass)
  ()
  (:documentation "The base persistence class serving as the superclass
  of all extended persistent metaclasses implemented in this distribution of
  manardb"))


(defmethod validate-superclass ((class fundamental-persistent-class) (super standard-class))
  t)


(defmacro define-fundamental-persistent-class (name direct-supers direct-slots &rest options)
  "Define a fundamental-persistent-class. This macro is functionally equivalent to the original
  manardb defmmclass, but integrated with the class hierarchy and conformant with the API
  shared by the extensions implemented in this manardb distribution. Automatically adds
  :metaclass FUNDAMENTAL-PERSISTENT-CLASS to options, if it is not present, finalizes the class
  immediately, and puts in an assertion that the class layout in the loaded datastore
  is compatible."
  `(progn
     (eval-when (:load-toplevel :execute :compile-toplevel) 
       (defclass ,name ,direct-supers ,direct-slots 
	 ,@(if (assoc :metaclass options) 
	       options
             `((:metaclass fundamental-persistent-class) ,@options)))
       (ensure-finalize-inheritance ',name))
     (eval-when (:execute)
       (check-class-slot-layout ,name))
     (find-class ',name)))



