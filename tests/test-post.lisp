(in-package :cl-user)
(defpackage pconch-test
  (:use :cl :prove :pconch))
(in-package :pconch-test)

(defparameter *fixtures-directory* #p"./tests/fixtures/")
(defparameter *results-directory* #p "./tests/results/")


(defun fixture-path (f-name)
  (uiop:merge-pathnames* f-name *fixtures-directory*))

(defun results-path (f-name &key (kind :object) )
  (let*  ((root (uiop/pathname:split-name-type f-name))
          (path (cond
                    ((eq  kind :object) (uiop/pathname:make-pathname*
                                         :directory (uiop:unix-namestring *results-directory*)
                                         :name root
                                         :type "store"))
                    ((eq kind :html) (uiop/pathname:make-pathname*
                                      :directory (uiop:unix-namestring *results-directory*)
                                      :name root
                                      :type "html"))
                    (t nil))))
    path))

(defun expect (f-path &key (kind :object))
  (let ((thing (basename f-path)))
    (cond ((eq kind :object)
           (cl-store:restore (results-path thing :kind kind)))
          ((eq kind :html)
           (uiop:read-file-string (results-path thing :kind kind))))))

(defun post-fixture (f-name)
  (with-open-file (f (fixture-path f-name))
    (pconch::read-post f)))


(defun post-write-results (p)
  (let* ((basename (pconch::resource-name p))
         (store-path (results-path basename :kind :object))
         (html-path (results-path basename :kind :html)))
    (cl-store:store p store-path)
    (with-open-file (h html-path :direction :output :if-exists :supersede
                       :if-does-not-exist :create )
      (format h "~A" (pconch::render p)))))

(defun install-post-as-fixture (post-name fixture-name)
  "copy a post from the system source directory called <post-name> to a fixture in
the fixture directory called <fixture-name> and generate a parsed post result for it and a parsed html result for it"
  (let ((source-f (uiop:merge-pathnames* post-name pconch::*source-dir*))
        (dest-f (fixture-path fixture-name))
        (pconch::*source-dir* *fixtures-directory*))
    (uiop:copy-file source-f dest-f)
    (post-write-results (post-fixture fixture-name))))

(defun basename (p)
  (multiple-value-bind (_ _ filename _)
      (uiop/pathname:split-unix-namestring-directory-components
       (uiop/pathname:unix-namestring p))
    (declare (ignore _))
    filename))

;; --- tests are below here


(plan 1)
(subtest "Testing posts from fixtures"
  (let ((fixtures (directory (fixture-path "*"))))
    (dolist (f fixtures)
      (is (post-fixture f) (expect f :kind :object) :test #'pconch::post-equal)
      (is (pconch::render (post-fixture f)) (expect f :kind :html)))))
(finalize)
