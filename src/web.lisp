(in-package #:cl-user)
(defpackage gitbarrel.web
  (:use #:cl
        #:caveman2
        #:gitbarrel.config
        #:gitbarrel.view
        #:git-info
        #:files-and-folders)
  (:export #:*web*))
(in-package #:gitbarrel.web)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)
(defparameter *repositories* #P"~/git/")
;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

(defroute ("/([\\w]+)/([\\w]+)" :regexp :t) (&key captures)
  (let ((user (first captures))
        (repository (second captures)))
    (render #P "repository.html"
            (list :repository repository
                  :user user
                  :files (mapcar #'name (tracked-files
                                         (merge-paths *repositories*
                                                      (force-directory user)
                                                      repository)))))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
