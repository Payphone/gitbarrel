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

(defun generate-index ()
  (with-open-file (in (merge-paths *repositories* "index")
                      :if-exists :supersede
                      :if-does-not-exist :create
                      :direction :output)
    (format in "~{~S~%~^~}" (list-all-files *repositories*))))

(defun search-for-file (search-term)
  (let ((term (string-upcase search-term)))
    (with-open-file (in (merge-paths *repositories* "index"))
      (loop for path = (read in nil :EOF)
         until (eq path :EOF)
           when (search term (string-upcase (pathname-name path)))
         collect (shorten-directory path "git")))))

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

(defroute ("/search" :method :POST) (&key |term|)
  (redirect (cat "/search?term=" |term|)))

(defroute "/search" (&key |term|)
  (render #P"search.html"
          (list :files (search-for-file |term|))))

(defroute "/users" ()
  (render #P"list.html"
          (list :files
                (mapcar #'(lambda (d) (shorten-directory d "git"))
                        (remove-if #'filep (list-directory *repositories*))))))

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
         (on-exception *web* 404))))

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
        (on-exception *web* 404))))

(defroute ("/(.+)" :regexp :t) (&key captures)
  (let* ((user (first captures))
         (repositories (list-directory (merge-paths *repositories* user))))
    (if repositories
        (render #P"list.html"
                (list :files (mapcar #'(lambda (d) (shorten-directory d "git"))
                                     repositories)
                      :user user))
        (on-exception *web* 404))))

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
