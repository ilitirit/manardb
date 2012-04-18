;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :manardb
  :version "0.1.3"
  :licence "LLGPL"
  :depends-on (:alexandria :osicat :iterate :closer-mop :cl-irregsexp)
  :components ((:module :src
                 :components ((:file "package")
                               (:file "widths" :depends-on ("package"))
                               (:file "utils" :depends-on ("package"))
                               (:file "mtagmap" :depends-on ("widths" "struct" "mop"))
                               (:file "mop" :depends-on ("struct"))
                               (:file "struct" :depends-on ("utils" "widths"))
                               (:file "class" :depends-on ("mop" "mtagmap"))
                               (:file "types" :depends-on ("class"))
                               (:file "array" :depends-on ("types"))
                               (:file "gc" :depends-on ("finalize"))
                               (:file "rewrite-gc" :depends-on ("gc"))
                               (:file "box" :depends-on ("types"))
                               (:file "finalize" :depends-on ("box"))
                               (:file "iterator" :depends-on ("class"))
                               (:file "fixed-string" :depends-on ("box"))
                               (:file "transaction" :depends-on ("finalize"))))))




