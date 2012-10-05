;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :manardb
  :version          "0.3.0"
  :description      "ManarDB is a performant Memory-Mapped storage allocation system based on
                    the common-lisp object system meta-object-protocol"
  :long-description "This system defines an enhanced fork of the original manardb distribution,
                     (version designation '0.1.20090911) that provides support for non-linux
                     platforms, compatibility with current releases of the required libraries,
                     updates supporting current lisp platform distributions, and a number of
                     miscellaneous fixes and feature enhancements.  It does not necessarily
                     seek to maintain backward compatibility with the API provided by the
                     original distribution in all cases."
  :licence           "LLGPL"
  :author            "John Fremlin"
  :author            "Dan Lentz <danlentz@gmail.com>"
  :maintainer        "Dan Lentz <danlentz@gmail.com>"
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
                               ))))






