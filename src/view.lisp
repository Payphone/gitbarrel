(in-package #:cl-user)
(defpackage gitbarrel.view
  (:use #:cl)
  (:import-from #:gitbarrel.config
                #:*template-directory*)
  (:import-from #:caveman2
                #:*response*
                #:response-headers)
  (:import-from #:djula
                #:add-template-directory
                #:compile-template*
                #:render-template*
                #:*djula-execute-package*)
  (:export #:render))
(in-package #:gitbarrel.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

;;
;; Execute package definition

(defpackage gitbarrel.djula
  (:use #:cl)
  (:import-from #:gitbarrel.config
                #:config
                #:appenv
                #:developmentp
                #:productionp)
  (:import-from #:caveman2
                #:url-for))

(setf djula:*djula-execute-package* (find-package :gitbarrel.djula))
