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
  (let* ((user (first captures))
         (directory (second captures))
         (file (third captures))
         (repository (merge-paths *repositories* user directory)))
    (render #P"repository.html"
            (list :repository repository
                  :user user
                  :files (mapcar #'name (tracked-files repository))
                  :tags (tags repository)))))

;;
;; Error pages

(defun error-reason (error-code)
  (let ((http-error
         '((404 . "Not Found")
           (403 . "Insufficient Permissions"))))
    (cdr (assoc error-code http-error))))

(defmethod on-exception ((app <web>) error-code)
  (declare (ignore app))
  (render "error.html"
          (list :error-code error-code :error-message (error-reason error-code))))
