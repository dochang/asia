(in-package :cl-user)

(defpackage :asia-asdf
  (:use :cl :asdf))

(in-package :asia-asdf)

(defsystem :asia
  :author "Desmond O. Chang <dochang+asia@gmail.com>"
  :maintainer "Desmond O. Chang <dochang+asia@gmail.com>"
  :license "MIT"
  :description "ASDF2 Software Installation Assistant"
  :long-description "ASIA installs Common Lisp projects automatically."
  :depends-on (:asdf #+sbcl :sb-posix)
  :serial t
  :pathname "src/"
  :components ((:module "fad"
                        :serial t
                        :components ((:file "packages")
                                     #+:cormanlisp (:file "corman")
                                     #+:openmcl (:file "openmcl")
                                     (:file "fad")
                                     (:file "test")))
               (:module "pregexp"
                        :components ((:file "pregexp")))
               (:file "package")
               (:file "utils")
               (:file "specials")
               (:file "base")
               (:file "project")
               (:file "installer")
               (:file "asdf")
               (:file "test")))

(defmethod perform :after ((op load-op) (c (eql (find-system :asia))))
  (pushnew :asia *features*)
  (let* ((rcfile (asdf::in-user-configuration-directory "asia.lisp")))
    (when rcfile
      (load rcfile))))

(defmethod perform ((op test-op) (c (eql (find-system :asia))))
  (funcall (intern "TEST" :asia)))
