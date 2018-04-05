(in-package :cl-user)
(defpackage epilog-asd
  (:use :cl :asdf))
(in-package :epilog-asd)

(defsystem epilog
  :version "0.1"
  :author "Tomoki Aburatani"
  :license ""
  :depends-on (:anaphora
               :alexandria
               :uiop
               :cl-ppcre)
  :serial t
  :components ((:module "src"
		:components
                ((:file "destruct-operator")
		 (:file "pattern")
		 (:file "base-function")
		 (:file "epilog"))))
  :description "Small Prolog implementation written in Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op epilog-test))))
