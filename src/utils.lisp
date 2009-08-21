(in-package #:manardb)

(defmacro defun-speedy (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       (declare (optimize speed))
       ,@body)))

(defun force-class (class-specifer)
  (typecase class-specifer
    (class class-specifer)
    (t
     (find-class class-specifer))))