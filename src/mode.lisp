;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :manardb)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facilities for Global STM Mode Activation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun globally-enable-direct-update-mode ()
  (ensure-inactive-layer 'isolated-update-mode)
  (ensure-inactive-layer 'deferred-update-mode)
  (ensure-active-layer 'stm)
  (ensure-active-layer 'direct-update-mode))


(defun globally-enable-deferred-update-mode ()
  (ensure-inactive-layer 'isolated-update-mode)
  (ensure-inactive-layer 'direct-update-mode)
  (ensure-active-layer 'stm)
  (ensure-active-layer 'deferred-update-mode))


(defun globally-enable-isolated-update-mode ()
  (ensure-inactive-layer 'deferred-update-mode)
  (ensure-inactive-layer 'direct-update-mode)
  (ensure-active-layer 'stm)
  (ensure-active-layer 'isolated-update-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIVATION of DEFAULT Globally Active STM Mode 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(globally-enable-direct-update-mode)



