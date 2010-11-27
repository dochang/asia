(in-package :asia)

(defun make-temp-name ()
  (format nil "ASIA-~A-~A-~A-~A"
          (implementation-identifier) (getpid)
          (get-universal-time) (random #xFFFF)))

(defun make-temp-package ()
  (flet ((%try ()
           (make-package (make-temp-name) :use '(:cl :asdf :asia))))
    (loop :for package := (ignore-errors (%try))
          :when package
          :return package)))

(defun make-temp-pathname (&key type defaults)
  "Generates a unique temporary pathname.  Keyword parameters are the
same as PATHSPEC."
  (location (list *temporary-directory*
                  #+(and (or win32 windows mswindows mingw32) (not cygwin))
                  "asia"
                  #-(and (or win32 windows mswindows mingw32) (not cygwin))
                  (format nil "asia~A" (get-uid))
                  (pathspec (make-temp-name) :type type :defaults defaults))))

(defun manifest-pathname (prefix name pathspec)
  (location (list *project-manifest* prefix name (pathspec pathspec))))

(defmacro with-manifest-file ((stream prefix name pathspec) &body body)
  (with-unique-names (fname)
    `(let* ((,fname (manifest-pathname ,prefix ,name ,pathspec)))
       (when (probe-file ,fname)
         (with-open-file (,stream ,fname)
           ,@body)))))

(defmacro with-system-file ((stream system-name pathspec) &body body)
  `(with-manifest-file (,stream "systems" ,system-name ,pathspec)
     ,@body))

(defmacro with-project-file ((stream project-name pathspec) &body body)
  `(with-manifest-file (,stream "projects" ,project-name ,pathspec)
     ,@body))
