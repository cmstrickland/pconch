(in-package :pconch)


(defun category-dir (category)
  "return the path to the directory for rendered content for the
specified category"
  (merge-pathnames (make-pathname :directory `(:relative ,category)) *www-dir*))

(defun target-file-path (category topic)
  "return the path for the rendered content given a category and a topic"
  (merge-pathnames
   (make-pathname  :directory `(:relative ,category) :name topic)
   *www-dir*))

(defun file-link (src dst)
  #-sbcl(handler-case
            (osicat:make-link dst :target src ) (OSICAT-POSIX:EEXIST () nil))
  #+sbcl (sb-posix:symlink src dst))

(defun link-sub-category (subcat cat topic)
  "link the resource specified by category and topic to the resource
specified by subcat and topic, using unix hard links"
  (let ((src (target-file-path cat topic))
        (dst (target-file-path subcat topic)))
    (ensure-directories-exist (category-dir subcat))
    (file-link src dst)))

(defun source-file-path (topic)
  "return the path to the source file for a given topic name"
  (merge-pathnames (make-pathname :directory nil :name topic :type "post")
                   *source-dir*))

(defun template-path (template &key (type "html"))
  "return the path to the named template file"
  (merge-pathnames (make-pathname :directory nil :name template :type type)
                   *template-dir*))

(defun stylesheet-path ()
  (merge-pathnames (make-pathname :directory '(:relative "styles")) *template-dir*))

(defun file-regexp-match-p (f rx)
  (not (null (ppcre:scan rx (namestring f)))))

