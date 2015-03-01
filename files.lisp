(in-package :pconch)


(defun target-file-path (category topic)
  (merge-pathnames
   (make-pathname  :directory `(:relative ,category) :name topic)
   *www-dir*))

(defun source-file-path (topic)
  (merge-pathnames (make-pathname :directory nil :name topic :type "post")
                   *source-dir*))

(defun template-path (template)
  (merge-pathnames (make-pathname :directory nil :name template :type "html")
                   *template-dir*))
