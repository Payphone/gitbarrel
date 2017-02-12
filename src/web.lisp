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
(defparameter *.repos* (merge-paths *repositories* #P".repos"))
(defparameter *.index* (merge-paths *repositories* #P".index"))

(defun generate-index ()
  (with-open-file (out *.index*
                      :if-exists :supersede
                      :if-does-not-exist :create
                      :direction :output)
    (format out "~{~S~%~^~}" (mapcar #'(lambda (d) (shorten-directory d "git"))
                                     (list-all-files *repositories* :include-directories nil)))))

(defun generate-repositories ()
  (with-open-file (out *.repos*
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :direction :output)
    (format out "~{~S~%~^~}"
            (mapcar #'(lambda (d) (shorten-directory d "git"))
                    (list-all-files *repositories* :max-depth 2)))))

(defun search-for-file (search-term)
  (let ((term (string-upcase search-term)))
    (with-open-file (in *.index*)
      (loop for path = (read in nil :EOF)
         until (eq path :EOF)
           when (search term (string-upcase (pathname-name path)))
             collect path))))

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

(defroute ("/search" :method :POST) (&key |term|)
  (redirect (cat "/search?term=" |term|)))

(defroute "/search" (&key |term|)
  (render #P"search.html"
          (list :files (search-for-file |term|))))

(defroute "/repositories" ()
  (render #P"user.html"
          (list :files
                (with-open-file (in *.repos*)
                  (loop for file = (read in nil :EOF)
                     until (eq file :EOF)
                       collect file)))))

(defroute ("/(.+)/(.+)/(.+)" :regexp :t) (&key captures)
  (let* ((user (first captures))
         (directory (second captures))
         (repository (merge-paths *repositories* user directory))
         (file (third captures)))
    (if (probe-file (merge-paths repository file))
        (render #P"file.html"
                (list :file (read-file (merge-paths repository file))
                      :files (tracked-files repository)
                      :directory directory
                      :user user))
        (render-error 404))))

(defroute ("/(.+)/(.+)" :regexp :t) (&key captures)
  (let* ((user (first captures))
         (directory (remove #\/ (second captures)))
         (repository (merge-paths *repositories* user directory)))
    (if (probe-file repository)
        (render #P"repository.html"
                (list :directory directory
                      :user user
                      :files (tracked-files repository)
                      :tags (tags repository)
                      :commits (remove-if-not #'(lambda (c)
                                                  (string= "commit" (log-type c)))
                                              (logs repository))))
        (render-error 404))))

(defroute ("/(.+)" :regexp :t) (&key captures)
  (let* ((user (first captures))
         (repositories (list-directory (merge-paths *repositories* user))))
    (if repositories
        (render #P"user.html" (list :files repositories :user user))
        (render-error 404))))

;;
;; Error pages

(defun error-reason (error-code)
  (let ((http-error
         '((404 . "Not Found")
           (403 . "Insufficient Permissions"))))
    (cdr (assoc error-code http-error))))

(defun render-error (code)
  (render "error.html"
          (list :error-code code :error-message (error-reason code))))

(defmethod on-exception ((app <web>) error-code)
  (declare (ignore app))
  (render-error error-code))
