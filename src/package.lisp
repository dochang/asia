(in-package :cl-user)

(defpackage :asia
  (:documentation "Usage:
;; install manually
> (asia:install-package _PROJECT_)
;; delete an installed project
> (asia:delete-package _PROJECT_)
;; install automatically when loading systems
> (pushnew 'asia:sysdef-asia-search asdf:*system-definition-search-functions*)
")
  (:use :cl :asdf :asia-fad :asia-pregexp)
  (:shadowing-import-from :asia-fad
                          :directory-pathname-p)
  (:import-from :asdf
                :probe-file*
                :delete-file-if-exists
                :get-uid
                :implementation-identifier
                :resolve-location
                :*verbose-out*)
  (:import-from :asia-fad-test
                :*test-counter*
                :assert*)
  (:export
   ;; utilities
   :pathspec
   :location
   ;; special variables
   :*project-manifest*
   :*source-location*
   :*temporary-directory*
   ;; base
   :make-temp-pathname
   :manifest-pathname
   ;; project
   :source-location
   :project-name
   :project-directory
   ;; installer
   :project-installed-p
   :install-project
   :delete-project
   ;; asdf
   :sysdef-asia-search))
