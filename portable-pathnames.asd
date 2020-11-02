;;;; ASDF file:
;;;;          portable-pathnames.asd
;;;;

(asdf:defsystem #:L-portable-pathnames
  :description "Common Lisp portable pathnames library"
  :author ("")
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "portable-pathnames")))
