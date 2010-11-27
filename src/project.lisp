(in-package :asia)

(defun source-location ()
  "Returns the real location of the installed source code.  If
*SOURCE-LOCATION* is not nil, returns *SOURCE-LOCATION*; otherwise,
returns the \"source\" subdirectory of *PROJECT-MANIFEST*."
  (or *source-location*
      (location (list *project-manifest* "source") :directory t)))

(defun project-name (project)
  "Returns PROJECT's name.  For a string, returns itself; for a
symbol, returns downcase symbol name; for NIL, singals an error."
  (etypecase project
    (null (error "Project cannot be NIL."))
    (string project)
    (symbol (string-downcase (symbol-name project)))))

(defun project-directory (project)
  "Returns PROJECT's directory."
  (location (list (source-location) (project-name project)) :directory t))

(defun project-ignored-p (project)
  "Tests whether PROJECT is ignored by ASIA."
  (let* ((name (project-name project)))
    (or (probe-file (manifest-pathname "projects" name "ignore"))
        (find name
              (append
               #+sbcl
               '("sb-aclrepl" "sb-bsd-sockets" "sb-cltl2" "sb-concurrency"
                 "sb-cover" "sb-grovel" "sb-introspect" "sb-md5" "sb-posix"
                 "sb-queue" "sb-rotate-byte" "sb-rt" "sb-simple-streams")
               '("asia" "asdf"))
              :key 'project-name :test 'string=))))
