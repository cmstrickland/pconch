(in-package :cl-user)
(defpackage pconch-test
  (:use :cl :prove :pconch))
(in-package :pconch-test)

(defparameter *fixtures-directory* #p"./tests/fixtures/")
(defparameter *results-directory* #p "./tests/results/")


(defun fixture-path (f-name)
  (uiop:merge-pathnames* f-name *fixtures-directory*))

(defun post-fixture (f-name)
  (with-open-file (f (fixture-path f-name))
    (pconch::read-post f)))


(defun post-write-results (p)
  (let* ((basename (pconch::resource-name p))
         (store-path (uiop/pathname:make-pathname*
                      :directory (uiop:unix-namestring *results-directory*)
                      :name basename
                      :type "store"))
         (html-path (uiop/pathname:make-pathname*
                     :directory (uiop:unix-namestring *results-directory*)
                     :name basename
                     :type "html")))

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


(subtest "Testing post "
  (ok (not (find 4 '(1 2 3))))
  (is 4 4)
  (isnt 1 #\1))
