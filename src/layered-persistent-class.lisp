;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layered Persistent Access Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass layered-persistent-access-class (layered-access-class
                                            basic-persistent-class
                                            standard-class-in-layer)
  ())

(defclass layered-persistent-direct-slot-definition (layered-direct-slot-definition
                                                      mm-direct-slot-definition
                                                      standard-direct-slot-definition-in-layer)
  ())

(defclass layered-persistent-effective-slot-definition-in-layers
  (mm-effective-slot-definition standard-effective-slot-definition-in-layers)
  ())

(defclass layered-persistent-effective-slot-definition
  (layered-effective-slot-definition-in-layers
    layered-persistent-effective-slot-definition-in-layers)
  ())


(defmethod direct-slot-definition-class ((class layered-persistent-access-class)
                                          &key &allow-other-keys)
  (find-class 'layered-persistent-direct-slot-definition))


(defvar *layered-persistent-effective-slot-definition-class*)


(defmethod effective-slot-definition-class ((class layered-persistent-access-class)
                                             &key &allow-other-keys)
  (if *layered-persistent-effective-slot-definition-class*
    *layered-persistent-effective-slot-definition-class*
    (call-next-method)))


(defmethod compute-effective-slot-definition ((class layered-persistent-access-class) name
                                               direct-slot-definitions)
  (declare (ignore name))
  (let ((*layered-persistent-effective-slot-definition-class*
          (if (some #'slot-definition-layeredp direct-slot-definitions)
            (if (some #'slot-definition-memory-mapped direct-slot-definitions)
              (find-class 'layered-persistent-effective-slot-definition)
              (find-class 'layered-effective-slot-definition-in-layers))
            (when (some #'slot-definition-memory-mapped direct-slot-definitions)
              (find-class 'persistent-effective-slot-definition-in-layers)))))
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Class "LAYERED-PERSISTENT-CLASS"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass layered-persistent-class (partial-class layered-persistent-access-class)
  ()
  (:default-initargs :defining-metaclass 'layered-persistent-access-class))


(ensure-finalize-inheritance (find-class 'layered-persistent-class))


(defmacro define-layered-persistent-class (&whole form name &body options)
  (let* ((layer    (if (member (car options) '(:in-layer :in) :test #'eq)
                     (cadr options)
                     t))
          (options (cond ((member (car options)   '(:in-layer :in) :test #'eq) (cddr options))
                     ((not (listp (car options)))  (error "Illegal option ~S in ~S." (car options)
                                                     form))
                     (t                            options)))
          (form    `(eval-when (:compile-toplevel :load-toplevel :execute)
                      (let ((it (defclass ,name ,(car options)
                                  ,(mapcar #'process-layered-access-slot-specification
                                     (cadr options))
                                  ,@(cddr options)
                                  ,@(unless (assoc :metaclass options)
                                      '((:metaclass layered-persistent-class)))
                                  (:in-layer . ,layer))))
                        (ensure-finalize-inheritance it)             
                        it))))
    #+allegro (if (eq (find-layer layer nil) 't) form
                `(excl:without-redefinition-warnings ,form))
    #+lispworks (if (eq (find-layer layer nil) 't) form
                  `(let ((dspec:*redefinition-action* :quiet)) ,form))
    #-(or allegro lispworks) form))
