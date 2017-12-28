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
     with headers-finished = nil
     with post = (make-instance 'post)
     initially (push (cons :filename (list (file-namestring open-file)))
                     (headers post))
     until (eq line :EOF) do
       ;; (format t "~a ~a ~a -> ~a ~%"
       ;; 	       (list-length (headers post))
       ;; 	       headers-finished
       ;; 	       (blank-line line)
       ;; 	       line)
       (cond ((blank-line line)      (progn
				       (if (> (list-length (headers post)) 1)
					   (setf headers-finished t))
				       (if headers-finished
					   (setf (content post)
						 (append-line (content post) "")))))

             ((not headers-finished) (setf (headers post)
					   (push  (parse-header line) (headers post))))

             (t                      (setf (content post)
					   (append-line (content post) line))))
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
(defgeneric post-author (post))
(defgeneric post-date (post &key format))
(defgeneric post-categorize (post))
(defgeneric post-tagify (post))
(defgeneric html-content (post))


(defun summarize-html (post &key (template "post") (selector "article"))
  (let ((lquery:*lquery-master-document*))
    (lquery:$ (initialize (template-path template))
              selector "#content section.post-content" (replace-with (html-content post)))
    (lquery:$ selector (attr :class (post-type post)))
    (lquery:$ selector ".permalink" (attr :href (url post)) (text (title post)))
    (lquery:$ selector ".dateline" (text (post-date post :format :short)))
    (lquery:$ "ul.post-attribution"
            (replace-with
             (format nil "<span>posted by <a class=\"p-author h-card\" href=\"\">~a</a></span>~%<span> on <time class=\"dt-published\" datetime=\"~a\">~a</time></span>"
                     (car(post-author post)) (post-date post) (post-date post))))
    ;(lquery:$ selector ".post-attribution .attribute" (text (post-date post)))
    (lquery:$ selector  (aref 0) (serialize))))


(defun summarize-rss (post)
  (let ((item-title (if (eq (post-type post) :short-post) "" (title post))))
    (format nil "<item>~%<title>~a</title>~%<link>~a</link>~%<description><![CDATA[~a]]> </description>~%<pubDate>~a</pubDate>~%<guid>~a</guid>~%</item>"
            item-title (url post) (html-content post) (post-date post :format :rfc822) (url post))))

(defun post-header-getdefault (post hdr dflt)
  (or (header post hdr)
      (list dflt)))

(defmethod post-author (post)
  (post-header-getdefault post :author "cms"))

(defmethod post-date ((post post) &key (format :display))
  (let ((date  
         (car (post-header-getdefault
               post :date
               (car (formatted-date (file-write-date (source-file-path (resource-name post) ))))))))
    (cond ((eq format :display) date)
          ((eq format :short) (subseq date 0 10))
          ((eq format :rfc822) (rfc-formatted-datetime (parse-pconch-datetime date)))
          ((eq format :iso8601) (iso-formatted-datetime (parse-pconch-datetime date))))))

(defmethod html-content ((post post))
  (if (eq (string-upcase (car (header post :format))) "HTML")
      (content post)
      (multiple-value-bind (_ markup) (cl-markdown:markdown (content post) :stream nil)
	markup)))

(DEFMETHOD summary ((post post) &key (content-type "html"))
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
           :test #'string-equal ) :short-post)
    ((find "notes" (append (header post :tags)
                           (header post :category))
           :test #'string-equal) :short-post)
    (t 'post)))

(defmethod post-base-tag ((post post))
  (or (first (header post :tags))
      (first (post-categorize post))))

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
  (lquery:$ "title" (text (title post)))
  (lquery:$ "section.post-content" (replace-with (html-content post)))
  (lquery:$ "a.permalink" (replace-with  (format nil "<h1 class=\"column\">~a</h1>" (title post))))
  (lquery:$ "span.dateline" (text (post-date post :format :short)))
  (lquery:$ "ul.post-attribution"
            (replace-with
             (format nil "<span>posted by <a class=\"p-author h-card\" href=\"\">~a</a></span>~%<span> on <time class=\"dt-published\" datetime=\"~a\">~a</time></span></span>"
                     (car(post-author post)) (post-date post) (post-date post))))
  (lquery:$ ".navigation .menu li"
            (replace-with 
             (format nil "~{~a~}"
                     (mapcar (lambda (c)
                               (format nil "<li class=\"category-menu\">~a</li>" (category-link c)))
                             (post-tagify post)))))
  (elt (lquery:$ (serialize)) 0))

(defmethod header ((post post) header)
  (cdr (assoc header (headers post))))

(defmethod on-topic ((post post) category)
  (remove-if-not (lambda (f) (equal category f))
   (append (header post :tags)
           (post-categorize post))))

(defmethod post-categorize ((post post))
  (post-header-getdefault post :category "uncategorized"))


(defmethod post-tagify ((post post))
  "return a list of all tags attached to the post header concatenated onto 
the result of post-categorize"
  (append (post-categorize post)
         (header post :tags)))
