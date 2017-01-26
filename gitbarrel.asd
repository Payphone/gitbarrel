(in-package #:cl-user)
(defpackage gitbarrel-asd
  (:use #:cl #:asdf))
(in-package #:gitbarrel-asd)

(defsystem gitbarrel
  :version "0.1"
  :author "Peyton Farrar"
  :license "MIT"
  :depends-on (#:clack
               #:lack
               #:caveman2
               #:envy
               #:cl-ppcre
               #:uiop
               #:git-info
               #:files-and-folders

               ;; HTML Template
               #:djula)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "config"))))
  :description "A Git web interface."
  :in-order-to ((test-op (load-op gitbarrel-test))))
