;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)


(asdf:defsystem :manardb-test
  :depends-on (:manardb :hu.dwim.stefil)
  :components ((:module :t
                 :serial t
                 :components ((:file "suite")
                               (:file "tree")
                               (:file "gc")
                               (:file "class")
                               (:file "symbol")
                               (:file "box")))))

