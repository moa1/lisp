;; the real liblightning.lisp is implemented in ~/soft/lightningfn/lisp.

(load "~/quicklisp/setup.lisp")
(ql:quickload :cffi)
(ql:quickload :cl-autowrap)

(defpackage lightningtest
  (:use #:common-lisp))

(in-package :lightningtest)


(setf cffi:*foreign-library-directories* '("/home/toni/soft/lightning-2.1.0/lib/.libs/"))

(cffi:define-foreign-library liblightning
  (:unix (:or "liblightning.so.0" "liblightning.so.0.0.0" "liblightning.so"))
  (t (:default "liblightning")))

(cffi:use-foreign-library liblightning)

;;c2ffi https://codeload.github.com/rpav/c2ffi/zip/master
;;(setf autowrap:*c2ffi-program* "/home/toni/tmp/c2ffi-master/src/c2ffi")
(autowrap:c-include "/home/toni/tmp/lightning-2.1.0/include/lightning.h"
		    :exclude-sources ("/usr/*")
		    :include-sources ("/home/toni/tmp/lightning-2.1.0/include/lightning/jit_x86.h"))

;;(autowrap:c-include "/usr/include/zlib1g.h")


;;(init-jit (cffi:null-pointer))
(init-jit "")
(defparameter *jit-state* (jit-new-state))
(_JIT_PROLOG *jit-state*)
(defparameter *in* (_JIT_ARG *jit-state*))

(_JIT_GETARG_I *jit-state* +_RAX+ *in*)
(_JIT_NEW_NODE_WWW *jit-state* +JIT-CODE-ADDI+ +_RAX+ +_RAX+ 1)
(_JIT_RETR *jit-state* +_RAX+)
(defparameter *incr* (_JIT_EMIT *jit-state*))

(_jit_clear_state *jit-state*)

(defun call-incr (number)
  (cffi:foreign-funcall-pointer (jit-pointer-t-ptr *incr*) (:convention :stdcall) :int number :int))
