(in-package :cl-user)
(defpackage epilog-test
  (:use :cl
        :epilog
        :prove))
(in-package :epilog-test)

;; NOTE: To run this test file, execute `(asdf:test-system :epilog)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
