(in-package #:manardb)

(defmacro defun-speedy (name lambda-list &body body &environment env)
  (declare (ignorable env))
  `(progn
     (declaim (inline ,name)) 
     #+lispworks ,@(when env `((declaim (notinline ,name)))) 
     ;; Lispworks 5.1 cannot inline things with macrolet lexical scope! 
     (defun ,name ,lambda-list
       (declare (optimize speed))
       ,@body)))

(defun force-class (class-specifer)
  (typecase class-specifer
    (class class-specifer)
    (t
     (find-class class-specifer))))

(defmacro cassert (test-form &optional places string &rest args)
  (declare (ignore places));; XXX
  `(unless ,test-form
     (cerror "Ignore the assertion" ,(or string (format nil "Assertion ~S failed" test-form)) ,@args)))
