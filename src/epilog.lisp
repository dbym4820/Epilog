(in-package :cl-user)
(defpackage epilog
  (:use :cl)
  (:import-from :epilog.basic
           :epilog-system
           :make-epilog
	   :clear
	   :epilog-query
	   :epilog-push
	   :fact
	   :pattern-match))
(in-package :epilog)
