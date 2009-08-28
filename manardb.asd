(cl:in-package #:cl-user)

(asdf:defsystem manardb
  :version "0.1.20090821"
  :licence "LLGPL"
  :components
  ((:module :src
	    :components (
			 (:file "package")
			 (:file "widths" :depends-on ("package"))
			 (:file "utils" :depends-on ("package"))
			 (:file "mtagmap" :depends-on ("widths" "struct"))
			 (:file "mop" :depends-on ("struct"))
			 (:file "struct" :depends-on ("utils" "widths"))
			 (:file "class" :depends-on ("mop" "mtagmap"))
			 (:file "types" :depends-on ("class"))
			 (:file "array" :depends-on ("types"))
			 (:file "gc" :depends-on ("finalize"))
			 (:file "box" :depends-on ("types"))
			 (:file "finalize" :depends-on ("box"))
			 (:file "iterator" :depends-on ("class"))
			 (:file "fixed-string" :depends-on ("box"))
			 (:file "transaction" :depends-on ("finalize"))
			 )))
  :depends-on (alexandria osicat iterate closer-mop cl-irregsexp stefil))

(asdf:defsystem manardb.test
  :components
  ((:module :t
	    :components ((:file "suite") 
			 (:file "gc" :depends-on ("tree"))
			 (:file "class" :depends-on ("suite"))
			 (:file "symbol" :depends-on ("suite"))
			 (:file "box" :depends-on ("suite"))
			 (:file "tree" :depends-on ("suite")))
	    ))
  :depends-on (manardb))

