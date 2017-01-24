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
(defparameter *repositories* (merge-paths *application-root* #P"git/"))

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

(defroute ("/([\\w]+)/([\\w]+)/(.+)" :regexp :t) (&key captures)
  (let ((name (first captures))
        (repository (second captures))
        (file (third captures)))
    (render (merge-paths *repositories*
                         name
                         repository
                         file))))

(defroute ("/([\\w]+)/([\\w]+)" :regexp :t) (&key captures)
  (let ((user (first captures))
        (repository (second captures))
        (file (third captures)))
    (render #P"repository.html"
            (list :repository repository
                  :user user
                  :files (mapcar #'name
                                 (tracked-files
                                  (merge-paths *repositories*
                                               user
                                               repository)))))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
