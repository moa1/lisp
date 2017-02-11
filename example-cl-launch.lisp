#!/bin/sh
":" ; exec cl-launch -X -- "$0" "$@" || exit 42
(format t "It works!~%")
(write cl-launch:*arguments*) (terpri)
;;(write (read-line))

;;#!/usr/bin/cl-launch -X --init '(format t "foo~%")' --
;;(format t "hello, world. this unexpectedly doesn't work~%")
;;(write cl-launch:*arguments*) (terpri)

