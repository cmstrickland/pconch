(in-package :pconch)

(defun parse-header (line)
  "take a line that looks like a header return a list with a keyword
header followed by strings of all the header values "
  (let* ((couple (bisect-string line #\:))
         (k (car couple))
         (v (cdr couple))
         (lv (csv-list v)))
    (if (> (length lv) 1)
        (kvpair (cons k lv))
        (kvpair (cons k (list v))))))

(defun add-raw-content  (line)
  (format nil "content ~a~%" line))

(defun read-post (open-file)
  "parse a .post file
expect a series of headers being a line with a heading and a :
followed by at least one blank line, and then some content"
  (loop for line = (read-line open-file nil :EOF)
     with in-header
     with post = (make-instance 'post)
     initially (push (cons :filename (list (file-namestring open-file)))
                     (headers post))
     until (eq line :EOF) do
       (cond ((blank-line line)  (if in-header
                                     (setf in-header nil)
                                     (setf in-header t)))

             (in-header          (setf (headers post)
                                       (push  (parse-header line) (headers post))))

             (t                  (setf (content post)
                                       (concatenate 'string (content post) line))))
     finally (return post)))



(defclass post ()
  ((headers :accessor headers :initform nil)
   (content :accessor content :initform nil)))

(defgeneric render (post &optional template))
(defgeneric header (post header))
(defgeneric on-topic (post category))
(defgeneric title (post))
(defgeneric url   (post))
(defgeneric resource-name (post))
(defgeneric summary (post &key content-type))
(defgeneric post-type (post))
(defgeneric post-base-tag (post))


(defun summarize-html (post &key (template "post") (selector "article"))
  (let ((lquery:*lquery-master-document*))
    (lquery:$ (initialize (template-path template))
              selector "#content p" (replace-with (content post)))
    (lquery:$ selector (attr :class (post-type post)))
    (lquery:$ selector ".permalink" (attr :href (url post)) (text (title post)))
    (lquery:$ selector  (aref 0) (serialize))))


(defun summarize-rss (post)
  (format nil "<item rdf:about=\"~a\">~%<title>~a</title>~%<link>~a</link>~%<description>FIXME</description>~%<content:encoded><![CDATA[~a]]></content:encoded>~%</item>"
          (url post) (title post) (url post) (content post)))

(defmethod summary ((post post) &key (content-type "html"))
  (cond ((string-equal content-type "html") (summarize-html post))
        ((string-equal content-type "rss")  (summarize-rss post))))

(defmethod resource-name ((post post))
  (first (bisect-string (first (header post :filename)) #\.)))

(defmethod title ((post post))
  (or  (first (header post :title))
       "Untitled"))

(defmethod post-type ((post post))
  (cond
    ((find "links" (append (header post :tags)
                           (header post :category))
           :test #'string-equal ) 'link-post)
    (t 'post)))

(defmethod post-base-tag ((post post))
  (or (first (header post :tags))
      "uncategorized"))

(defmethod url ((post post))
  (let ((base-url (puri:parse-uri *base*)))
    (setf (puri:uri-path base-url)
          (concatenate 'string
                       *prefix*
                       (namestring
                        (make-pathname :directory
                                       `(:relative  ,(post-base-tag post))
                                       :name (resource-name post)))))
    (format nil "~a" base-url)))

(defmethod render ((post post) &optional (template "post"))
  (lquery:$ (initialize (template-path template)))
  (lquery:$ "div#content > p" (replace-with (content post)))
  (lquery:$ "h1#post-heading" (text (first (header post :title))))
  (elt (lquery:$ (serialize)) 0))

(defmethod header ((post post) header)
  (cdr (assoc header (headers post))))

(defmethod on-topic ((post post) category)
  (remove-if-not (lambda (f) (equal category f))
   (append (header post :tags)
           (header post :category))))
