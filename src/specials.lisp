(in-package :asia)

(defvar *project-manifest*
  #+(and (or win32 windows mswindows mingw32) (not cygwin))
  (pathname "C:\\repo51\\")
  #-(and (or win32 windows mswindows mingw32) (not cygwin))
  (let* ((env (getenv "XDG_DATA_HOME"))
         (def (location (list :home ".local" "share") :directory t)))
    (location (list (or env def) "repo51") :directory t))
  "Location of the projects information database.")

(defvar *source-location* nil
  "Location of the installed source code.")

;; For windows user: why not use %TEMP% ?
;;
;; Clozure has a "feature" that changes filename to name\\.tar.gz.  We
;; have to put all the filenames without the quote character in
;; RUN-SHELL-COMMAND.  That means we cannot use any filename which has
;; whitespaces.

(defvar *temporary-directory*
  #+(and (or win32 windows mswindows mingw32) (not cygwin))
  (pathname "C:\\Temp\\")
  #-(and (or win32 windows mswindows mingw32) (not cygwin))
  (location (list (or (getenv "TMPDIR") "/tmp/")) :directory t)
  "Temporary directory.  If it is NIL, uses default value.  On
windows, default is C:\\Temp\\ ; otherwise, default is the value of
environment variable TMPDIR, if TMPDIR is NIL, default is /tmp/ .")
