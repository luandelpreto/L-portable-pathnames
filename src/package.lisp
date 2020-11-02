;;;; Common Lisp file:
;;;;                 package.lisp
;;;;
;;;; Package for portable-pathnames

(in-package :cl-user)

(defpackage #:L-portable-pathnames
  (:nicknames #:Lppath
              #:L-prtb-pnames
              #:Lpnames)
  (:use #:common-lisp)
  (:export
   #:list-directory
   #:file-exists-p
   #:directory-pathname-p
   #:file-pathname-p
   #:pathname-as-directory
   #:pathname-as-file
   #:walk-directory
   #:directory-p
   #:file-p))
