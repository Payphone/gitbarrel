(in-package #:cl-user)
(defpackage gitbarrel.web
  (:use #:cl
        #:caveman2
        #:gitbarrel.config
        #:gitbarrel.view
        #:peyton-utils
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

(defroute ("/(.+)/(.+)/(.+)" :regexp :t) (&key captures)
  (let* ((user (first captures))
         (directory (second captures))
         (repository (merge-paths *repositories* user directory))
         (file (third captures)))
    (render #P"file.html"
            (list :file (read-file (merge-paths repository file))
                  :files (tracked-files repository)
                  :directory directory
                  :user user))))

(defroute ("/(.+)/(.+)" :regexp :t) (&key captures)
  (let* ((user (first captures))
         (directory (second captures))
         (repository (merge-paths *repositories* user directory)))
    (render #P"repository.html"
            (list :directory directory
                  :user user
                  :files (tracked-files repository)
                  :tags (tags repository)
                  :commits (remove-if-not #'(lambda (c)
                                              (string= "commit" (log-type c)))
                                          (logs repository))))))

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
