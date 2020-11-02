;;;; Common Lisp file:
;;;;                 portable-pathnames.lisp
;;;;
;;;; Portable pathnames library

(in-package :L-portable-pathnames)

(defun component-present-p (value)
  "Tests whether a given component of a  pathname is 'present',  meaning
   neither NIL nor the special value :unspecific.
   NOTE: Implementations are allowed  to return :unspecific  instead  of
   NIL as the value of pathname components in certain situations such as
   when the component isn't used by that implementation."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  "Tests whether a pathname is already in directory form."
  (and (not (component-present-p (pathname-name p)))
       (not (component-present-p (pathname-type p)))
       p))

(defun pathname-as-directory (name)
  "Converts any pathname to a directory form pathname."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
	      (make-pathname
	       :directory (append (or (pathname-directory pathname) (list :relative))
			                      (list (file-namestring pathname)))
	       :name nil
	       :type nil
	       :defaults pathname)
	      pathname)))

;;;===================================================================
;;; In CLISP, DIRECTORY won't return file with no extension unless the
;;; type component of the wildcard is NIL rather than :wild
;;;===================================================================
(defun directory-wildcard (dirname)
  "Takes a pathname in either directory or file form and returns a proper
   wildcard for the given implementation."
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

;;;===================================================================
;;; Not all implementations will return  subdirectories  of the  given
;;; directory. Allegro, SBCL, CMUCL, and LispWorks do. OpenMCL doesn't
;;; by default but will if you pass DIRECTORY a true value via the  im_
;;; plementation-specific keyword argument :directories. CLISP's DIREC_
;;; TORY returns subdirectories only when it's passed a wildcard  path_
;;; name with :wild as the last element of the directory component and
;;; NIL name and type components. In this case, it  returns  only  sub_
;;; directories, so it's necessary to call DIRECTORY twice with  diffe_
;;; rent wildcards and combine the results.
;;;
;;; The implementatios can also differ in whether they return  the  na_
;;; mes of directories in directory or file form. We want LIST-DIRECTO_
;;; RY to always return directory names in directory form  so  we  can
;;; differentiate subdirectories from  regular files based on just the
;;; name. Except for  Allegro, all  the implementations  this  library
;;; will support do  that. Allegro require  the we pass DIRECTORY  the
;;; implementation- specific  keyword  argument :directories-are-files
;;; the value NIL to get it to return directories in file form.
;;;===================================================================
(defun list-directory (dirname)
  "Determines which, if any, files that are present in the file system have
   names matching dirname, and returns a fresh list of pathnames  correspon_
   ding to the truenames of those files."
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "LIST-DIRECTORY not implemented.")))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

;;;===================================================================
;;; Allegro and CMUCL's PROBE-FILE will accept the name of a directory
;;; in either form but, instead of returning a directory form name, it
;;; simply return the name in the same form as  the argumento was  pas_
;;; sed. If passed the name of a nondirectory in directory form,  they
;;; return NIL.
;;; CLISP's PROBE-FILE immediately signals an error if  passed  a name
;;; in  directory  form, regardless  of  whether a file  or  directory
;;; exists with that name. It also signals an error  if  passed a name
;;; in file form that's actually the name of a directory. For  testing
;;; if a directory, CLISP provides it's own function,  PROBE-DIRECTORY.
;;;===================================================================
(defun file-exists-p (pathname)
  "Take a pathname and return an equivalent pathname if the file exists or
   NIL if it doesn't. It accepts the name of a directory in either directo_
   ry or file form but always return a directory form pathname if the file
   exists and is a directory."
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
	     (probe-file (pathname-as-file pathname)))
      (ignore-errors
	     (let ((directory-form (pathname-as-directory pathname)))
	       (when (ext:probe-directory directory-form)
	         directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "FILE-EXISTS-P not implemented."))

(defun pathname-as-file (name)
  "Converts any pathname to a file form pathname."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name)
	      (let* ((directory (pathname-directory pathname))
	             (name-and-type (pathname (first (last directory)))))
	        (make-pathname
	         :directory (butlast directory)
	         :name (pathname-name (pathname name-and-type))
	         :type (pathname-type (pathname name-and-type))
	         :defaults pathname))
	      pathname)))

(defun walk-directory (dirname map-fn &key directories (test (constantly t)))
  "Takes the name of a directory and a function and call the function on
   the pathnames of all the files under the directory,  recursively.  It
   takes the keyword parameters :directories and :test.  When :directori_
   es is T, it will call  the map-fn on the pathnames of directories  as
   well as regular files. The :test argument, when provided, specifies a
   function that's invoked on each pathname before the main function  is;
   The main function will be called only if the test function returns T."
  (labels
      ((walk (name)
	       (cond ((directory-pathname-p name)
		            (when (and directories (funcall test name))
		              (funcall map-fn name))
		            (dolist (x (list-directory name))
		              (walk x)))
	             ((funcall test name)
		            (funcall map-fn name)))))
    (walk (pathname-as-directory dirname))))
