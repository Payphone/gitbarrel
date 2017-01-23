(in-package :cl-user)
(defpackage gitbarrel-test-asd
  (:use :cl :asdf))
(in-package :gitbarrel-test-asd)

(defsystem gitbarrel-test
  :author "Peyton Farrar"
  :license ""
  :depends-on (:gitbarrel
               :prove)
  :components ((:module "t"
                :components
                ((:file "gitbarrel"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
