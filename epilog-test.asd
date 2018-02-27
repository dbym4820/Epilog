#|
  This file is a part of epilog project.
  Copyright (c) 2018 Tomoki Aburatani (t.aburatani@osakafu-u.net)
|#

(in-package :cl-user)
(defpackage epilog-test-asd
  (:use :cl :asdf))
(in-package :epilog-test-asd)

(defsystem epilog-test
  :author "Tomoki Aburatani"
  :license ""
  :depends-on (:epilog
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "epilog"))))
  :description "Test system for epilog"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
