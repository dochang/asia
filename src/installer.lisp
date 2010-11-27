(in-package :asia)

(defun git-install (url project)
  ;; Debian Git 1:1.5.6.5-3+lenny3 has a bug causes we can't use
  ;; project-directory since it has a trailing slash.
  (run-shell-command "git clone '~A' ~A" url
                     (pathname-as-file (project-directory project))))

(defun svn-install (url project)
  (run-shell-command "svn checkout '~A' ~A" url (project-directory project)))

(defun darcs-install (url project)
  (run-shell-command "darcs get '~A' ~A" url (project-directory project)))

(defun cvs-install (url project)
  (let* ((name (project-name project))
         (fmt "cd ~A && cvs -d '~A' checkout -d '~A' '~A'")
         (pos (position #\# url :from-end t)))
    (multiple-value-bind (cvsroot module)
        (cond ((null pos) (values url name))
              ((= pos (1- (length url))) (values (subseq url 0 pos) name))
              (t (values (subseq url 0 pos) (subseq url (1+ pos)))))
      (run-shell-command fmt (source-location) cvsroot name module))))

(defun tarbomb-p (filename &optional options)
  (rest
   (remove-duplicates
    (with-input-from-string
        (in (with-output-to-string (*verbose-out*)
              (run-shell-command "tar t ~@[~A~] -f ~A" options filename)))
      (read-line in nil nil)
      (loop :for name := (read-line in nil nil)
            :while name
            :collect (let* ((pos (position #\/ name)))
                       (if pos (subseq name 0 pos) name))))
    :test 'equal)))

(defun tar-install (filename project &optional options)
  (run-shell-command "tar x ~:[--strip-components=1~;~] ~@[~A~] -f ~A -C ~A"
                     (tarbomb-p filename) options filename
                     (ensure-directories-exist (project-directory project))))

(defun directory-install (dirname project)
  (let* ((tarfile (make-temp-pathname :type "tar"))
         (dir (pathname-directory dirname))
         (parent (make-pathname :directory (butlast dir) :defaults dirname)))
    (run-shell-command "tar c -C ~A -f ~A ~A" parent tarfile (first (last dir)))
    (tar-install tarfile project)))

(defun local-install (filename project)
  (let* ((type (pathname-type filename))
         (name (pathname-name filename)))
    (cond ((directory-pathname-p filename)
           (directory-install filename project))
          ((and type (string= "tar" type))
           (tar-install filename project))
          ((or (and type (string= "tgz" type))
               (and type (string= "gz" type)
                    name (pregexp-match "\\.tar$" name)))
           (tar-install filename project "-z"))
          ((or (and type (pregexp-match "^(tbz2|tbz|tb2)$" type))
               (and type (pregexp-match "^(bz2|bz)$" type)
                    name (pregexp-match "\\.tar$" name)))
           (tar-install filename project "-j")))))

(defun curl-install (url project)
  (let* ((tmpdir (make-temp-pathname :type :directory)))
    (delete-directory-and-files tmpdir :if-does-not-exist :ignore)
    (ensure-directories-exist tmpdir)
    (run-shell-command "cd ~A && curl -s -J -O '~A'" tmpdir url)
    (local-install (first (list-directory tmpdir)) project)))

(defun git-url-p (url)
  (pregexp-match "^(git|git\\+ssh|ssh\\+git)://|/git(/|$)|\\.git/?$" url))

(defun svn-url-p (url)
  (pregexp-match "^svn://" url))

(defun darcs-url-p (url)
  (pregexp-match "/darcs(/|$)" url))

(defun cvs-url-p (url)
  (pregexp-match "^:pserver:" url))

(defun curl-url-p (url)
  (pregexp-match "^(https?|ftps?|sftp|file)://" url))

(defun guess-backend (url)
  "Guesses the backend name based on URL's pattern."
  (cond ((git-url-p url) 'git-install)
        ((svn-url-p url) 'svn-install)
        ((darcs-url-p url) 'darcs-install)
        ((cvs-url-p url) 'cvs-install)
        ((curl-url-p url) 'curl-install)
        (t 'local-install)))

(defun generic-install (url project)
  (cond ((null url) (error "url cannot be NIL.~%"))
        ((null project) (error "project cannot be NIL.~%"))
        ((or (functionp url) (symbolp url)) (funcall url))
        ((pathnamep url) (local-install url project))
        ((stringp url) (funcall (guess-backend url) url project))
        (t (error 'type-error :datum url
                  :expected-type '(or function pathname string (and symbol (not null)))))))

(defun make-installer (url project)
  (lambda () (generic-install url project)))

(defun project-installed-p (project)
  "Returns true if PROJECT is installed; otherwise, returns false."
  (directory-exists-p (project-directory project)))

(defun %install-project (project url)
  (let* ((*verbose-out* *standard-output*)
         (project-name (project-name project))
         (loc (list *project-manifest* "projects" project-name))
         (*default-pathname-defaults* (location loc :directory t)))
    (funcall (cond (url (make-installer url project))
                   ((with-project-file (in project-name "installer.lisp")
                      (with-standard-io-syntax
                        (let ((package (make-temp-package)))
                          (unwind-protect
                               (let* ((*package* package)
                                      (form (read in nil nil)))
                                 (and form (coerce form 'function)))
                            (delete-package package))))))
                   ((with-project-file (in project-name "url")
                      (let* ((url (read-line in nil nil)))
                        (and url (make-installer url project)))))))))

(defun install-project (project &key url)
  "Installs PROJECT.  If URL is a function, use it to install; if URL
is a string or pathname, use a standard installer; if URL is NIL, this
function will look up the manifest file to get a url or installer.
Returns true if PROJECT is installed; otherwise, returns false."
  (unless project
    (error "Project is NIL.~%"))
  (let* ((name (project-name project))
         (dir (project-directory project))
         (installed nil))
    (when (project-ignored-p project)
      (error "Project ~A is ignored.~%" name))
    (when (project-installed-p project)
      (error "Project ~A is installed.~%" name))
    (loop
      (unwind-protect
           (with-simple-restart (retry "Reinstall ~A." name)
             (%install-project project url)
             (return (setf installed (project-installed-p project))))
        (unless installed
          (delete-directory-and-files dir :if-does-not-exist :ignore))))))

(defun delete-project (project)
  "Deletes PROJECT's directory."
  (unless project
    (error "Project is NIL.~%"))
  (let* ((name (project-name project))
         (dir (project-directory project)))
    (when (project-ignored-p project)
      (error "Project ~A is ignored.~%" name))
    (delete-directory-and-files dir :if-does-not-exist :ignore)))
