;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :manardb
  :version    "0.2.0"
  :licence    "LLGPL"
  :author     "John Fremlin"
  :author     "Dan Lentz <danlentz@gmail.com>"
  :maintainer "Dan Lentz <danlentz@gmail.com>"
  :depends-on (:alexandria :osicat :iterate :closer-mop :contextl :cl-irregsexp)
  :components ((:module src :serial t
                 :components ((:static-file "manardb.asd")
                               (:file "package")
                               (:file "widths")  
                               (:file "utils")   
                               (:file "struct")  
                               (:file "mop")     
                               (:file "mtagmap") 
                               (:file "class")   
                               (:file "types")   
                               (:file "iterator")
                               (:file "array")   
                               (:file "box")     
                               (:file "finalize")
                               (:file "filesystem")
                               (:file "fixed-string") 
                               (:file "mcons")        
                               (:file "gc")           
                               (:file "rewrite-gc")
                               (:file "fundamental-persistent-class")
                               (:file "layered-persistent-class")
                               (:file "transactional-standard-class")
;;                               (:file "transactional-persistent-class")
;;                               (:file "standard-persistent-class")
;;                               (:file "mode")
                               ))))






