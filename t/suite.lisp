(defpackage #:manardb.test
  (:export #:test-all-manardb)
  (:use #:cl #:manardb #:stefil #:iter))

(in-package #:manardb.test)

(defsuite manardb-test)

(defun test-all-manardb (&key (cleanup t) (function 'manardb-test))
  (let ((dir (format nil "/tmp/manardb-test-~36R/" (random most-positive-fixnum)))
	(*mmap-may-allocate* t))
    (use-mmap-dir dir)
    (unwind-protect
	 (funcall function)
      (when cleanup
	(ignore-errors (osicat:delete-directory-and-files dir :if-does-not-exist nil))))))