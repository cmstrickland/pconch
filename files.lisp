(in-package :pconch)


(defun category-dir (category)
  (merge-pathnames (make-pathname :directory `(:relative ,category)) *www-dir*))

(defun target-file-path (category topic)
  (merge-pathnames
   (make-pathname  :directory `(:relative ,category) :name topic)
   *www-dir*))

(defun link-sub-category (subcat cat topic)
  (let ((src (target-file-path cat topic))
        (dst (target-file-path subcat topic)))
    (ensure-directories-exist (category-dir subcat))
    (osicat:make-link dst :target src :hard t)))

(defun source-file-path (topic)
  (merge-pathnames (make-pathname :directory nil :name topic :type "post")
                   *source-dir*))

(defun template-path (template)
  (merge-pathnames (make-pathname :directory nil :name template :type "html")
                   *template-dir*))
