(in-package :asia)

(defun system-project (system)
  "Returns the project which contains SYSTEM."
  (let* ((system-name (asdf:coerce-name system)))
    (or (with-system-file (in system-name "project")
          (read-line in nil nil))
        system-name)))

(defun system-pathname (system &optional project)
  "Returns SYSTEM's pathname."
  (let* ((system-name (asdf:coerce-name system)))
    (merge-pathnames*
     (or (with-system-file (in system-name "pathname")
           (let* ((x (read-line in nil nil)))
             (and x (pathspec x))))
         (pathspec system-name :type "asd"))
     (project-directory (or project (system-project system))))))

(defun sysdef-asia-search (system)
  "ASIA system definition search function.  You should put this
function into ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS* manually."
  (when system
    (let* ((project (system-project system)))
      (and project
           (or (ignore-errors (install-project project))
               (project-installed-p project))
           (probe-file (system-pathname system project))))))
