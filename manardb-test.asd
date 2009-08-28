(asdf:defsystem manardb-test
  :components
  ((:module :t
	    :components ((:file "suite") 
			 (:file "gc" :depends-on ("tree"))
			 (:file "class" :depends-on ("suite"))
			 (:file "symbol" :depends-on ("suite"))
			 (:file "box" :depends-on ("suite"))
			 (:file "tree" :depends-on ("suite")))
	    ))
  :depends-on (manardb stefil))