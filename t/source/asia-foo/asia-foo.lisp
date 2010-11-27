(in-package :cl-user)

(defpackage :asia-foo
  (:use :cl)
  (:export :foo))

(in-package :asia-foo)

(defun foo ()
  (print "A test project for ASIA."))
