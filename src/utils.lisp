(in-package :asia)

(defmacro with-unique-names (names &body forms)
  "Binds each variable named by a symbol in NAMES to a unique symbol around
FORMS. Each of NAMES must either be either a symbol, or of the form:
 
 (symbol string-designator)

Bare symbols appearing in NAMES are equivalent to:
 
 (symbol symbol)

The string-designator is used as the argument to GENSYM when constructing the
unique symbol the named variable will be bound to."
  (flet ((%trans (name)
           (multiple-value-bind (symbol string)
               (etypecase name
                 (symbol
                    (values name (symbol-name name)))
                 ((cons symbol (cons (or symbol string character) null))
                    (values (first name) (string (second name)))))
             `(,symbol (gensym ,string)))))
    (let ((entries (mapcar #'%trans names)))
      `(let ,entries
         ,@forms))))

(defun emptyp (sequence)
  "Returns true if SEQUENCE is an empty sequence. Signals an error if
SEQUENCE is not a sequence"
  (etypecase sequence
    (list (null sequence))
    (sequence (zerop (length sequence)))))

(defun pathspec (name &key type defaults)
  "This implements the concept of ASDF \"pathname specifier\".

If NAME is a pathname designator except a symbol or string, returns
itself; if NAME is a symbol, treat it as the downcase form of its
symbol-name; if NAME is a string, treat it as the \"pathname
specifier\".

If TYPE is :DIRECTORY or NAME ends with a slash, returns a directory
pathname; if TYPE is a string, it will be the new type component of
the specifier.

The specifier will use the host and device components from DEFAULTS or
*DEFAULT-PATHNAME-DEFAULTS* (if DEFAULTS is NIL).

See ASDF manual 5.3.4 for details."
  (if (typep name '(or pathname symbol string))
      (funcall (if (fboundp 'coerce-pathname)
                   'coerce-pathname
                   'asdf::merge-component-name-type)
               name :type type :defaults defaults)
      (pathspec (pathname name) :type type :defaults defaults)))

(defun location-designator-p (location-designator)
  "Tests whether LOCATION-DESIGNATOR is available.

NOTE: Although NIL and T are available in ASDF, they are unavailable
in ASIA."
  (and (asdf::location-designator-p location-designator)
       (not (typep location-designator 'boolean))))

(defun location (location-designator &key directory wilden)
  "This implements the concept of ASDF location DSL.

If DIRECTORY is true, LOCATION-DESIGNATOR will be treated as a
directory.

If WILDEN is true, LOCATION-DESIGNATOR can be a wildcard.

See ASDF manual 7.4 & 8.3 for the details of LOCATION-DESIGNATOR."
  (if (location-designator-p location-designator)
      (resolve-location location-designator :directory directory :wilden wilden)
      (error "Invalid location designator ~S~%" location-designator)))

#+clisp
(let ((getpid (or (find-symbol "PROCESS-ID" :system)
                  ;; old name prior to 2005-03-01, clisp <= 2.33.2
                  (find-symbol "PROGRAM-ID" :system)
                  #+win32 ; integrated into the above since 2005-02-24
                  (and (find-package :win32) ; optional modules/win32
                       (find-symbol "GetCurrentProcessId" :win32)))))
  (defun %getpid () ; a required interface
    (cond
      (getpid (funcall getpid))
      #+win32 ((ext:getenv "PID")) ; where does that come from?
      (t -1))))

#+lispworks
(defun %getpid ()
  #+win32
  (win32:get-current-process-id)
  #-win32
  (system::getpid))

#+abcl
(defun %getpid ()
  (handler-case
      (let* ((runtime
              (java:jstatic "getRuntime" "java.lang.Runtime"))
             (command
              (java:jnew-array-from-array
               "java.lang.String" #("sh" "-c" "echo $PPID")))
             (runtime-exec-jmethod
              ;; Complicated because java.lang.Runtime.exec() is
              ;; overloaded on a non-primitive type (array of
              ;; java.lang.String), so we have to use the actual
              ;; parameter instance to get java.lang.Class
              (java:jmethod "java.lang.Runtime" "exec"
                            (java:jcall
                             (java:jmethod "java.lang.Object" "getClass")
                             command)))
             (process
              (java:jcall runtime-exec-jmethod runtime command))
             (output
              (java:jcall (java:jmethod "java.lang.Process" "getInputStream")
                          process)))
        (java:jcall (java:jmethod "java.lang.Process" "waitFor")
                    process)
        (loop :with b
              :do (setq b
                        (java:jcall (java:jmethod "java.io.InputStream" "read")
                             output))
              :until (member b '(-1 #x0a))        ; Either EOF or LF
              :collecting (code-char b) :into result
              :finally (return
                         (parse-integer (coerce result 'string)))))
    (t () 0)))

(defun getpid ()
  (or
   #+sbcl
   (sb-posix:getpid)
   #+clozure
   (ccl::getpid)
   #+clisp
   (%getpid)
   #+ecl
   (si:getpid)
   #+cmucl
   (unix:unix-getpid)
   #+lispworks
   (%getpid)
   #+allegro
   (excl::getpid)
   #+abcl
   (%getpid)
   #-(or sbcl clozure clisp ecl cmucl lispworks allegro abcl)
   -1))
