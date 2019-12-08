(in-package :pconch)


(defun decode-path (req-uri)
  "given a URL string, returns the path with *prefix* removed"
  (namestring
   (let* ((uri-path-list (path-components (quri:uri-path (quri:uri req-uri))))
          (prefix-path-list (path-components *prefix*))
          (reduced-path (remove-prefix prefix-path-list uri-path-list)))
     (uiop:make-pathname* :directory (cons :absolute reduced-path)))))


(defun path-components (pathstring)
  "splits a path string into it's components as a list"
  (let* ((path (uiop:parse-unix-namestring pathstring))
         (name (pathname-name path))
         (dirlist (cdr (pathname-directory path))))
    (if name
        (nconc dirlist (list name))
        dirlist)))



(defun lookup-meta (key meta)
  (cdr (assoc key meta)))

(defun resource-stale (f)
  (let* ((meta (read f))
         (timestamp (lookup-meta :timestamp meta))
         (src       (merge-pathnames  (lookup-meta :original  meta)
                                      *source-dir*)))
    (> (file-write-date src) timestamp)))

(defun valid-resource (path)
  "returns truthily if the requested resource path represents a
serveable resource"
  (with-open-file (f path :if-does-not-exist nil :external-format :utf-8)
    (if f
        (not (resource-stale f))
        nil)))

(defun serve-file (path)
  "Just serve a file over http. First sexp is metadata, second is
content"
  (with-open-file (f path :external-format :utf-8)
    (read f) (read f)))

(defun serve-resource (params)
  "Decode a request for a resource.  if it exists as a serveable file,
serve it. if it exists as a source file but not a serveable file,
publish it and then serve it. Otherwise, 404"
  (let* ((category (getf params :category))
         (topic (getf params :topic))
         (filepath (target-file-path category topic)))
    (cond ((valid-resource filepath) (serve-file filepath))
          ((publish-resource category topic) (serve-file filepath))
          (t (serve-resource-not-found (hunchentoot:request-uri hunchentoot:*request*))))))


(defun publish-file (file path category topic)
  "compile a source file and publish it. This includes linking it into
place as a serveable resource for every secondary category / tag"
  (let ((meta (pairlis '(:version :original :timestamp) (list 1 path (get-universal-time))))
        (post (read-post file)))
    (if (on-topic post category)
        (let ((target (target-file-path category topic)))
          (ensure-directories-exist target)
          (with-open-file (of target
                              :external-format :utf-8
                              :direction :output
                              :if-exists :supersede)
            (prin1 meta of)
            (print (render post) of))
          (mapcar (lambda (f) (link-sub-category f category topic))
                  (remove-if (lambda (f) (equal f category))
                             (append (header post :tags)
                                     (header post :category)))) t))))

(defun publish-resource (category topic)
  "turn a request cat / topic tuple into a call to publish file"
  (let ((path (source-file-path topic)))
    (with-open-file (f path :if-does-not-exist nil :external-format :utf-8)
      (if f (publish-file f path category topic) nil))))

(defun serve-resource-not-found (url)
  "Content and response for 404"
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (format nil "No content found for ~a" url))


(defun index-all-posts ()
  "build a sorted list of post objects for every post in the source directory"
  (sort
   (mapcar (lambda (f)
             (with-open-file (p f :external-format :utf-8)
               (read-post p)))
           (remove-if-not (lambda (f) (file-regexp-match-p f ".*post$"))
                      (uiop:directory-files *source-dir*)))
   #'string>
   :key (lambda (f) (post-date f))))

(defun build-index (&optional category)
  "build an sorted index of posts, perhaps filtered by a category / tag "
  (let ((posts (index-all-posts)))
    (if (not (empty-subject category))
        (remove-if-not (lambda (f) (on-topic f category)) posts)
        posts)))



(defmacro index-paginator-for-uri (kind uri)
  `(if ,kind
       (lquery:$ (inline  (concatenate 'string "div#paginator > ul.menu > li#"
                                       (symbol-name ',kind) "> a"))
                 (attr "hidden" nil "href"
                       (add-params-to-uri ,uri
                                          (pairlis '("start" "end")
                                                   (list (car ,kind) (cadr ,kind))) )))
       (lquery:$ (inline  (concatenate 'string  "div#paginator > ul > li#"
                                       (symbol-name ',kind))) )))


(defun add-params-to-uri (u p)
  (let* ((surl (quri:uri u))
        (params (quri:uri-query-params surl)))
    (dolist (x p)
      (if (assoc (car x) params :test #'equal)
          (setf (cdr (assoc (car x) params :test #'equal)) (cdr x) )
          (push x params)))
    (setf (quri:uri-query-params surl) params)
    (quri:render-uri surl)))


(defun list-sel-extend-with (list-sel content)
  "extend a list element inside the current DOM by adding a new node
just like the last with the supplied html"
  (lquery:$ list-sel
            (first)
            (clone)
            (html content)
            (append-to (lquery:$ list-sel
                                 (first)
                                 (parent)))))

(defun gen-index-content (category range uri)
  "given a category, a range and a page location, build HTML for an index page"
  (let* ((index (build-index category))
             (index-length (length index))
             (range (truncate-range range index-length))
             (prev  (compute-prev range))
             (next  (compute-next range index-length)))
        (if index
            (progn
              (lquery:$ (initialize (template-path "index"))
                        "h1#page-heading" (text *site-title*))
              ;; extend the content list by adding summaries of every entry in range
              (dolist (entry (subseq index (car range) (cadr range)))
                (list-sel-extend-with "ol#index-list > li" (summary entry)))

              ;; remove the example entry from the front of the list
              (lquery:$ "ol#index-list > li" (first) (remove))
              (if category
                  (lquery:$ "title" (text (format nil "Index of ~a" category))))
              (index-paginator-for-uri next uri)
              (index-paginator-for-uri prev uri)
              (lquery:$ (aref 0) (serialize))))))



(defun serve-index (params)
  "serve an index page"
  (let ((range (compute-range (hunchentoot:get-parameters* *last-request*)))
        (category (getf params :category)))
    (unless range
      (hunchentoot::redirect
       (add-params-to-uri (hunchentoot:request-uri*) `(("start" . 0) ("end" . ,*index-pager*)))
        :code 302))
    (clache:with-cache ((cache-key (cache-version) "index" (hunchentoot:request-uri*)) :store *index-cache*)
      (or (gen-index-content category range (hunchentoot:request-uri*))
            (serve-resource-not-found category)))))

(defun with-auth (domain handler params)
  (multiple-value-bind (auth-user auth-password)
      (hunchentoot:authorization)
    (unless (and auth-user auth-password
                 (equal auth-user "cms")
                 (equal auth-password "s3kr3ts"))
      (hunchentoot:require-authorization domain)))
  (funcall handler params))

(defun serve-drafts (params)

  "<h1>Current Drafts</h1>")

(defun serve-feed (params)
  "serve an rss feed"
  (let ((range '(0 20))
        (category (getf params :category))
        (plump:*tag-dispatchers* plump:*xml-tags*))
    (let* ((index (build-index category))
           (index-length (length index))
           (range (truncate-range range index-length)))
      (if index
          (progn
            (lquery:$ (initialize (template-path "index" :type "rss"))
                      "channel > title" (text "beatworm.co.uk"));
            (lquery:$ "channel > description" (text "beatworm blog"))
            (lquery:$ "channel > link" (text "https://beatworm.co.uk/blog/"))
            (lquery:$ "item"
                      (replace-with (reduce
                                     (lambda (a b) (concatenate 'string a b))
                                     (mapcar
                                      (lambda (p) (summary p :content-type "rss"))
                                      (subseq index (car range) (cadr range))))))
            (lquery:$  (aref 0) (serialize)))))))



(defun category-url (category)
  (let
      ((u (quri:uri *base*)))
    (setf
     (quri:uri-path u)
     (concatenate 'string *prefix* category))
    u))

(defun category-link (category)
  ;; FIXME - construct a proper URI
  "return html for a hyperlink to a category index"
  (format nil "<a href=\"~a\" class=\"p-category\">~a</a>" (quri:render-uri (category-url category))
          category))

(defun handler ()
  (let ((router (map-routes '(("/" serve-index)
                              ("/feed/" serve-feed)
                              ("@/pconch/drafts/" serve-drafts)
                              ("/?:category?/*.rss" serve-feed)
                              ("/:category/?" serve-index)
                              ("/:category/:topic/?" serve-resource))))
        (*last-request* hunchentoot:*request*)
        (*script-name* (hunchentoot::script-name*)))
    (multiple-value-bind (response value)
        (myway:dispatch router (decode-path
                                (hunchentoot:request-uri hunchentoot:*request*)))
      response )))
