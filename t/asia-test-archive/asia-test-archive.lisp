(in-package :cl-user)

(defpackage :asia-test-archive
  (:use :cl)
  (:export :foo))

(in-package :asia-test-archive)

(defun foo ()
  (print "A archive test project for ASIA."))
