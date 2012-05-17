;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;
;;;; See EOF for attribution of the original authors due credit for the original
;;;; CSTM source upon which this implementation is based 

(in-package :manardb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transactional Access Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass transactional-access-class ()
  ())


(defclass transactional-standard-access-class (transactional-access-class
                                                layered-access-class
                                                standard-class-in-layer)
  ())

                                                  
(defgeneric slot-definition-transactionalp (slot)
  (:method ((slot slot-definition)) nil))


(defclass transactional-slot-definition ()                                                
  ((transactionalp :initarg  :transactional
                   :initarg  :transactionalp
                   :initform nil
                   :reader  slot-definition-transactionalp)))


(defclass transactional-standard-direct-slot-definition
  (transactional-slot-definition
    layered-direct-slot-definition
    standard-direct-slot-definition-in-layer)
  ())


(defmethod initialize-instance :around ((slot transactional-slot-definition)
                                         &rest initargs &key transactional transactionalp)
  (declare (dynamic-extent initargs))
  (if (or transactional transactionalp)
    (apply #'call-next-method slot :layered t initargs)
    (call-next-method)))


(defmethod direct-slot-definition-class ((class transactional-standard-access-class)
                                          &rest initargs)
  (declare (ignore initargs))
  (find-class 'transactional-standard-direct-slot-definition))


(defclass transactional-effective-slot-definition ()
  ())


(defclass transactional-standard-effective-slot-definition
  (transactional-effective-slot-definition
    layered-effective-slot-definition-in-layers)
  ())
                                                               

(defmethod slot-definition-transactionalp ((slot transactional-effective-slot-definition))
  t)

(defvar *transactional-standard-effective-slot-definition-class*)


(defmethod effective-slot-definition-class ((class transactional-standard-access-class)
                                             &rest initargs)
  (declare (ignore initargs))
  (if *transactional-standard-effective-slot-definition-class*
    *transactional-standard-effective-slot-definition-class*
    (call-next-method)))


(defmethod compute-effective-slot-definition ((class transactional-standard-access-class) name
                                               direct-slot-definitions)
  (let ((*transactional-standard-effective-slot-definition-class*
          (when (some #'slot-definition-transactionalp direct-slot-definitions)
            (find-class 'transactional-standard-effective-slot-definition))))
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STM Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deflayer stm)

(ensure-active-layer 'stm)

(deflayer isolated-update-mode (stm))
  
(define-layered-function transactional-slot-value (object))

(defvar %unbound% (list '%unbound%))


(defstruct (transactional-slot-content (:constructor %make-transactional-slot-content))
  object (value %unbound%))


(define-layered-method transactional-slot-value ((object transactional-slot-content))
  (let ((value (transactional-slot-content-value object)))
    (if (eq value %unbound%)
      (slot-unbound (class-of object) object 'slot-value)
      value)))


(declaim (inline transactional-slot-content-boundp transactional-slot-content-makunbound))


(defun transactional-slot-content-boundp (object)
  (not (eq (transactional-slot-content-value object) %unbound%)))


(defun transactional-slot-content-makunbound (object)
  (setf (transactional-slot-content-value object) %unbound%)
  (values))


(define-layered-function make-transactional-slot-content (&rest initargs)
  (:method (&rest initargs)
   (declare (dynamic-extent initargs))
   (apply #'%make-transactional-slot-content initargs)))


(defmethod slot-unbound ((class layered-class) (object transactional-slot-content) slot-name)
  (let ((org (transactional-slot-content-object object)))
    (slot-unbound (class-of org) org slot-name)))


(define-layered-function transactional-slot-value-boundp (object)
  (:method ((object transactional-slot-content))
   (transactional-slot-content-boundp object)))


(define-layered-method slot-value-using-layer :in-layer stm :around
  ((class transactional-access-class) object (slot transactional-effective-slot-definition) reader)
  (declare (ignore reader))
  (transactional-slot-value (call-next-method)))


(define-layered-method (setf slot-value-using-layer) :in-layer stm :around
  (new-value (class transactional-access-class) object
    (slot transactional-effective-slot-definition) writer)
  (declare (ignore writer))
  (let* ((new-value (make-transactional-slot-content :object object :value new-value))
         (return-value (call-next-layered-method new-value class object slot writer)))
    (transactional-slot-value return-value)))


(define-layered-method slot-boundp-using-layer :in-layer stm :around
  ((class transactional-access-class) object (slot transactional-effective-slot-definition) reader)
  (declare (ignore reader))
  (let ((bound (call-next-method)))
    (when bound
      (let ((value (with-inactive-layers (stm)
                     (slot-value object (slot-definition-name slot)))))
        (transactional-slot-content-boundp value)))))


(define-layered-method slot-makunbound-using-layer :in-layer stm :around
  ((class transactional-access-class) object (slot transactional-effective-slot-definition) writer)
  (declare (ignore writer))
  (with-inactive-layers (stm)
    (setf (slot-value object (slot-definition-name slot))
          (make-transactional-slot-content :object object)))
  object)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transactional Standard-Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass transactional-standard-class (layered-class transactional-standard-access-class)
  ()
  (:default-initargs :defining-metaclass 'transactional-standard-access-class))


(defmacro define-transactional-standard-class (name supers slots &rest options)
  (let* ((options (remove :metaclass options :key #'first))
          (options  (push '(:metaclass transactional-standard-class) options)))
    `(define-layered-class ,name ,supers ,slots ,@options)))


(defmacro raw-slot-value (object slot-name &body init)
  (let ((new-init (gensym)))
    `(handler-case
       (with-inactive-layers (transaction stm-mode stm)
         (slot-value ,object ,slot-name))
       (unbound-slot ()
         (let ((,new-init (progn ,@init)))
           (with-inactive-layers (transaction stm-mode stm)
             (setf (slot-value ,object ,slot-name) ,new-init)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STM-Mode Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deflayer transaction)
(deflayer stm-mode)

(defvar *big-lock* (bt:make-lock "big lock"))


(define-layered-function call-atomic (thunk)
  (:method (thunk)
    (warn "atomic funcall outside stm strategy layer")
    (bt:with-lock-held (*big-lock*) (funcall thunk))))


(defmacro atomic (&body body)
  `(call-atomic (lambda () ,@body)))


(define-layered-function roll-back ()
  (:method () (error "Invoked roll-back outside of a transaction.")))


(define-layered-function commit-transaction ()
  (:method () (error "Invoked commit-transaction outside of a transaction.")))


(define-condition retry (condition) ())

(declaim (inline retry-transaction))


(defun retry-transaction ()
  (roll-back) (signal 'retry))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deferred-Update-Mode Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deflayer deferred-update-mode (stm-mode))


(defparameter *tries*   5)
(defparameter *timeout* 1.000)


(defstruct transaction
  (process (bt:current-thread))
  (start   (get-internal-real-time))
  (status :active))


(defvar *current-transaction* (make-transaction :status :committed))
(defvar *deferred-read-set*)


(deflayer deferred-update-transaction (transaction))


(define-layered-method roll-back :in-layer deferred-update-transaction ()
  (setf (transaction-status *current-transaction*) :aborted))


(define-layered-method commit-transaction :in-layer deferred-update-transaction ()
  (loop
    :for box :being each hash-key :in *deferred-read-set* :using (hash-value value)
    :for current-value = (svref box 0)
    :unless (eq value current-value) :do (retry-transaction)
    :finally (unless
              #+lispworks
              (system:compare-and-swap-structure-slot  
                (*current-transaction* transaction status)
                :active :committed)
              #+sbcl
              (sb-ext:compare-and-swap (transaction-status *current-transaction*)
                :active :committed)
              (retry-transaction))))


(define-layered-method call-atomic :in-layer deferred-update-mode (thunk)
  (tagbody retry
    (return-from call-atomic
      (let ((*current-transaction* (make-transaction))
             (*deferred-read-set*  (make-hash-table :test #'eq)))
        (with-active-layers (deferred-update-transaction)
          (handler-case (multiple-value-prog1 (funcall thunk)
                          (commit-transaction))
            (retry () (go retry))))))))


(defstruct (deferred-transactional-slot-content (:include transactional-slot-content))
  (most-recent-transaction *current-transaction*)
  old-value)


(declaim (inline most-recent-transaction))


(defun most-recent-transaction (object)
  (deferred-transactional-slot-content-most-recent-transaction object))


(define-layered-method make-transactional-slot-content :in-layer deferred-update-mode
  (&rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'make-deferred-transactional-slot-content initargs))


(declaim (inline transactional-slot-content-old-value))


(defun transactional-slot-content-old-value (object)
  (let ((value (deferred-transactional-slot-content-old-value object)))
    (if (eq value %unbound%)
      (slot-unbound (class-of object) object 'old-value)
      value)))


(declaim (inline (setf transactional-slot-content-old-value)
                 transactional-slot-content-old-value-boundp
                 transactional-slot-content-old-value-makunbound))


(defun (setf transactional-slot-content-old-value) (new-value object)
  (setf (deferred-transactional-slot-content-old-value object) new-value))


(defun transactional-slot-content-old-value-boundp (object)
  (not (eq (deferred-transactional-slot-content-old-value object) %unbound%)))


(defun transactional-slot-content-old-value-makunbound (object)
  (setf (deferred-transactional-slot-content-old-value object) %unbound%)
  (values))


(define-layered-method transactional-slot-value :in-layer deferred-update-mode
  ((object transactional-slot-content))
  (ecase (transaction-status (most-recent-transaction object))
    (:committed (transactional-slot-content-value object))
    (:aborted   (transactional-slot-content-old-value object))
    (:active    (if (eq (most-recent-transaction object) *current-transaction*)
                  (transactional-slot-content-value object)
                  (retry-transaction)))))


(define-layered-method transactional-slot-value-boundp :in-layer deferred-update-mode
  ((object transactional-slot-content))
  (ecase (transaction-status (most-recent-transaction object))
    (:committed (transactional-slot-content-boundp object))
    (:aborted   (transactional-slot-content-old-value-boundp object))
    (:active    (if (eq (most-recent-transaction object) *current-transaction*)
                  (transactional-slot-content-boundp object)
                  (retry-transaction)))))



#-lispworks
(defun thread-wait-with-timeout (timeout thunk)
  (let ((endtime (floor (+ (get-internal-real-time)
                          (* internal-time-units-per-second timeout)))))
    (loop
      :with  result = (funcall thunk)
      :until (or result (> (get-internal-real-time) endtime))
      :do    (progn (bt:thread-yield) (setf result (funcall thunk)))
      :finally (return result))))
  

(define-layered-method slot-value-using-layer :in-layer deferred-update-mode
  ((class transactional-access-class) object (slot transactional-effective-slot-definition) reader)
  (declare (ignore reader))
  (loop
    :with box = (call-next-method)
    :with value = (svref box 0)
    :for  tries :from 1 :to *tries*
    :do
    (ecase (transaction-status (most-recent-transaction value))
      (:committed (return (values value box)))
      (:aborted   (return (values value box)))
      (:active    (cond
                    ((eq (most-recent-transaction value) *current-transaction*)
                      (return (values value box)))                    
                    ((< tries *tries*)
                      (#+lispworks   mp:process-wait-with-timeout
                        #+lispworks  "wait for transaction status"
                        #-lispworks  thread-wait-with-timeout
                        *timeout*
                        (lambda ()
                          (not (eq (transaction-status (most-recent-transaction
                                                         (setq value (svref box 0)))) :active)))))
                    (t
                      (if
                        #+lispworks (> (mp:process-run-time mp:*current-process*)
                                      (mp:process-run-time (transaction-process
                                                             (most-recent-transaction value))))
                        #-lispworks (< (transaction-start *current-transaction*)
                                      (transaction-start (most-recent-transaction value)))
                        (#+lispworks mp:process-interrupt
                          #-lispworks bt:interrupt-thread 
                          (transaction-process (most-recent-transaction value))
                          (lambda ()
                            (when (layer-active-p 'transaction)
                              (retry-transaction))))
                        (retry-transaction))))))))



(define-layered-method (setf slot-value-using-layer) :in-layer deferred-update-mode
  (new-value (class transactional-access-class) object
    (slot transactional-effective-slot-definition) writer)
  (loop
    :with slot-name = (slot-definition-name slot)
    :with box       = (raw-slot-value object slot-name
                        (vector (make-transactional-slot-content :object object)))
    :with value     = (svref box 0)
    :with new-box   = (vector new-value)
    :for  tries :from 1 :to *tries* :do
    (ecase (transaction-status (most-recent-transaction value))
      (:committed
        (return (svref (call-next-layered-method new-box class object slot writer) 0)))
      (:aborted
        (return (svref (call-next-layered-method new-box class object slot writer) 0)))
      (:active
        (cond
          ((eq (most-recent-transaction value) *current-transaction*)
            (return (svref (call-next-layered-method new-box class object slot writer) 0)))
          ((< tries *tries*)
            (#+lispworks mp:process-wait-with-timeout #+lispworks "wait for transaction status"
              #-lispworks thread-wait-with-timeout
              *timeout*
              (lambda ()
                (not (eq (transaction-status (most-recent-transaction (setq value (svref box 0))))
                       :active)))))
          (t (if
               #+lispworks (> (mp:process-run-time mp:*current-process*)
                             (mp:process-run-time (transaction-process
                                                    (most-recent-transaction value))))
               #-lispworks (< (transaction-start *current-transaction*)
                             (transaction-start (most-recent-transaction value)))
               (#+lispworks mp:process-interrupt
                 #-lispworks bt:interrupt-thread
                 (transaction-process (most-recent-transaction value))
                 (lambda () (when (layer-active-p 'transaction)
                              (retry-transaction))))
               (retry-transaction))))))))


(declaim (inline register-deferred-read check-deferred-read unregister-deferred-read))

(defun register-deferred-read (box value)
  (multiple-value-bind  (stored found)
    (gethash box *deferred-read-set*)
    (if found
      (unless (eq stored value) (retry-transaction))
      (setf (gethash box *deferred-read-set*) value))))


(defun check-deferred-read (box value)
  (multiple-value-bind (stored found)
      (gethash box *deferred-read-set*)
    (when found
      (unless (eq stored value) (retry-transaction)))))


(defun unregister-deferred-read (box value)
  (multiple-value-bind (stored found)
    (gethash box *deferred-read-set*)
    (when found
      (unless (eq stored value) (retry-transaction))
      (remhash box *deferred-read-set*))))

                     
(define-layered-method slot-value-using-layer :in-layer deferred-update-transaction
  ((class transactional-access-class) object (slot transactional-effective-slot-definition) reader)
  (declare (ignore reader))
  (multiple-value-bind (slot-value box) (call-next-method)
    (register-deferred-read box slot-value)
    slot-value))


(define-layered-method (setf slot-value-using-layer) :in-layer deferred-update-transaction
  (new-value (class transactional-access-class) object
    (slot transactional-effective-slot-definition) writer)
  (declare (ignore writer))
  (loop
    :with slot-name  = (slot-definition-name slot)
    :with slot-box   = (raw-slot-value object slot-name
                        (vector (make-transactional-slot-content :object object)))
    :with slot-value = (svref slot-box 0)
    :for  tries :from 1 to *tries*
    :do
    (multiple-value-bind (stored found) (gethash slot-box *deferred-read-set*)
      (when found
        (unless (eq stored slot-value) (retry-transaction))))
    (ecase (transaction-status (most-recent-transaction slot-value))
      ((:committed :aborted)
        (if (transactional-slot-value-boundp slot-value)
          (setf (transactional-slot-content-old-value new-value)
            (transactional-slot-value slot-value))
          (transactional-slot-content-old-value-makunbound new-value))
        (cond
          (#+lispworks
            (system:compare-and-swap-simple-vector-slot slot-box 0 slot-value new-value)
            #+sbcl
            (sb-ext:compare-and-swap (svref slot-box 0) slot-value new-value)    
            (remhash slot-box *deferred-read-set*)
            (return new-value))
          (t (setq slot-value (svref slot-box 0)))))
      (:active (cond
                 ((eq (most-recent-transaction slot-value) *current-transaction*)
                       (setf (transactional-slot-content-value slot-value)
                         (transactional-slot-content-value new-value))
                       (remhash slot-box *deferred-read-set*)
                       (return slot-value))
                 ((< tries *tries*)
                   (#+lispworks  mp:process-wait-with-timeout
                     #+lispworks "wait for transaction status"
                     #-lispworks thread-wait-with-timeout
                     *timeout*
                     (lambda ()
                       (not (eq
                              (transaction-status
                                (most-recent-transaction
                                  (setq slot-value (svref slot-box 0))))
                              :active)))))
                 (t
                   (if
                     #+lispworks (> (mp:process-run-time mp:*current-process*)
                                   (mp:process-run-time (transaction-process
                                                          (most-recent-transaction slot-value))))
                     #-lispworks (< (transaction-start *current-transaction*)
                                   (transaction-start (most-recent-transaction slot-value)))
                     (#+lispworks  mp:process-interrupt
                       #-lispworks bt:interrupt-thread
                       (transaction-process (most-recent-transaction slot-value))
                        (lambda ()
                          (when (layer-active-p 'transaction-status)
                                     (retry-transaction))))
                      (retry-transaction))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct-Update-Mode Layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deflayer direct-update-mode (stm-mode))
(deflayer direct-update-transaction (transaction))

(defvar *direct-read-set*)
(defvar *direct-write-set*)


(define-layered-method call-atomic :in-layer direct-update-mode (thunk)
  (tagbody retry (return-from call-atomic
                   (let ((*direct-read-set* (make-hash-table :test #'eq))
                          (*direct-write-set* (make-hash-table :test #'eq)))
                     (with-active-layers (direct-update-transaction)
                       (unwind-protect
                         (handler-case (multiple-value-prog1 (funcall thunk)
                                         (commit-transaction))
                           (retry () (go retry)))
                         (release-locks)))))))


(defstruct (direct-transactional-slot-content (:include transactional-slot-content))
  (version 0)
  (lock
    #+lispworks (mp:make-lock :name "direct transaction lock" :important-p nil)
    #-lispworks (bt:make-lock "direct transaction lock")))


(declaim (inline slot-version (setf slot-version) slot-lock))

(defun slot-version (object)
  (direct-transactional-slot-content-version object))


(defun (setf slot-version) (new-value object)
  (setf (direct-transactional-slot-content-version object) new-value))


(defun slot-lock (object)
  (direct-transactional-slot-content-lock object))


(define-layered-method make-transactional-slot-content :in-layer direct-update-mode (&rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'make-direct-transactional-slot-content initargs))


(declaim (inline register-direct-read register-direct-write))

(defun register-direct-read (slot-value)
  (unless (gethash slot-value *direct-read-set*)
    (setf (gethash slot-value *direct-read-set*)
          (slot-version slot-value))))


(defun register-direct-write (object slot-name slot-value)
  (let* ((lock (slot-lock slot-value))
          (locked
            #+lispworks (mp:process-lock lock "direct-write" 0.001)
            #-lispworks (bt:acquire-lock lock nil)))
    (cond
      (locked (let ((read-version (gethash slot-value *direct-read-set*)))
                (when read-version
                  (if (> (slot-version slot-value) read-version)
                    (retry-transaction)
                    (remhash slot-value *direct-read-set*))))
        (unless (gethash slot-value *direct-write-set*)
          (setf (gethash slot-value *direct-write-set*)
            (cons (with-inactive-layers (transaction)
                    (handler-case (slot-value object slot-name)
                      (unbound-slot () %unbound%)))
              (slot-version slot-value)))))
      (t (sleep *timeout*)
        (retry-transaction)))))


(define-layered-method slot-value-using-layer :in-layer direct-update-transaction
  ((class transactional-access-class) object (slot transactional-effective-slot-definition) reader)
  (declare (ignore reader))
  (let ((value (call-next-method)))
    (register-direct-read value)
    value))


(define-layered-method (setf slot-value-using-layer) :in-layer direct-update-transaction
  (new-value (class transactional-access-class) object
    (slot transactional-effective-slot-definition) writer)
  (declare (ignore new-value writer))
  (let* ((slot-name (slot-definition-name slot))
         (slot-value (raw-slot-value object slot-name
                       (make-transactional-slot-content :object object))))
    (register-direct-write object slot-name slot-value)
    (setf (transactional-slot-content-value slot-value)
          (transactional-slot-content-value new-value))
    slot-value))


(defun release-locks ()
  (loop for slot-value being each hash-key in *direct-write-set*
        for lock = (slot-lock slot-value) do
    #+lispworks (mp:process-unlock lock)
    #-lispworks (bt:release-lock lock)))


(define-layered-method roll-back :in-layer direct-update-transaction ()
  (loop
    for slot-value being each hash-key in *direct-write-set*
    using (hash-value (old-value . old-version))
        unless (> (slot-version slot-value) old-version) do
        (if (eq old-value %unbound%)
          (transactional-slot-content-makunbound slot-value)
          (setf (transactional-slot-content-value slot-value) old-value))))


(define-layered-method commit-transaction :in-layer direct-update-transaction ()
  (loop for slot-value being each hash-key in *direct-read-set* using (hash-value read-version)
        when (> (slot-version slot-value) read-version) do (retry-transaction))
  (loop for slot-value being each hash-key in *direct-write-set* do
        (incf (slot-version slot-value))))


(pushnew :cstm *features*)


#|
Adapted from the original source under the following terms:

Copyright (c) 2009, 2010 Pascal Costanza & Charlotte Herzeel
Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
|#
