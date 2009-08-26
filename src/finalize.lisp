(in-package #:manardb)

(defun finalize-all-mmaps ()
  (loop for m across *mtagmaps* 
	when m
	do (mtagmap-finalize m)))

(defun close-all-mmaps ()
  (loop for package in (list-all-packages) do
	(do-all-symbols (sym package)
	  (remf (symbol-plist sym) 'mm-symbol)))
  (loop for m across *mtagmaps* do
	(when m (mtagmap-close m))))

(defun open-all-mmaps ()
  (finalize-all-mmaps)
  (loop for m across *mtagmaps* do
	(when m
	  (when (mtagmap-closed-p m)
	    (mtagmap-open m))
	  (mtagmap-check m))))

(defun shrink-all-mmaps ()
  "Truncate all mmaps to their current sizes"
  (loop for m across *mtagmaps* do
	(when (and m (not (mtagmap-closed-p m)))
	  (mtagmap-shrink m))))

(define-lisp-object-to-mptr)

