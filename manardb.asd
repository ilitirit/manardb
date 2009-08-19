(cl:in-package #:cl-user)

(asdf:defsystem manardb
  :version "0.1.20090819"
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
			 (:file "box" :depends-on ("basictypes"))
			 (:file "finalize" :depends-on ("box"))
			 (:file "iterator" :depends-on ("class"))
			 ))
   (:module :t
	    :components ((:file "suite") 
			 (:file "class" :depends-on ("suite"))
			 (:file "box" :depends-on ("suite"))
			 (:file "tree" :depends-on ("suite")))
	    ))
  :depends-on (alexandria osicat iterate closer-mop cl-irregsexp stefil))
