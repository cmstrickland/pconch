(in-package :pconch)


(defun uri-path (uri)
  "returns the path components of a uri string"
  (cdr (puri:uri-parsed-path (puri:parse-uri uri))))

(defun decode-path (req-uri)
  "removes the prefix path components of a uri string. the prefix path
is defined in *prefix*"
  (let* ((prefix-path (uri-path *prefix*))
         (req-path (uri-path req-uri))
         (path (remove-prefix prefix-path req-path)))
    (if path
        (subseq path 0 (min (length path) 2)))))


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
  (with-open-file (f path :if-does-not-exist nil)
    (if f (not (resource-stale f)) nil)))

(defun serve-resource (path)
  (with-open-file (f path)
    (read f) (read f)))

(defun publish-file (file path category topic)
  (let ((meta (pairlis '(:version :original :timestamp) (list 1 path (get-universal-time))))
        (post (read-post file)))
    (if (on-topic post category)
        (progn
          (with-open-file (of (target-file-path category topic)
                              :direction :output
                              :if-exists :supersede)
            (prin1 meta of)
            (print (render post) of))
          (mapcar (lambda (f) (link-sub-category f category topic))
                  (remove-if (lambda (f) (equal f category))
                             (append (header post :tags)
                                     (header post :category)))) t))))

(defun publish-resource (category topic)
  (let ((path (source-file-path topic)))
    (with-open-file (f path :if-does-not-exist nil)
      (if f (publish-file f path category topic) nil))))

(defun serve-resource-not-found (url)
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (format nil "No content found for ~a" url))


(defun index-all-posts ()
  "build a sorted list of post objects for every post in the source directory"
  (sort
   (mapcar (lambda (f)
             (with-open-file (p f)
               (read-post p)))
           (remove-if-not (lambda (f) (eq (osicat:file-kind f) :regular-file))
                          (osicat:list-directory *source-dir*)))
   #'string>
   :key (lambda (f) (car (header f :date)))))

(defun build-index (&optional category)
  "build an sorted index of posts, perhaps filtered by a category / tag "
  (let ((posts (index-all-posts)))
    (if (not (empty-subject category))
        (remove-if-not (lambda (f) (on-topic f category)) posts)
        posts)))


(defmacro index-paginator (kind)
  `(if ,kind
       (lquery:$ (inline  (concatenate 'string "div#paginator > ul > li#"
                                       (symbol-name ',kind) "> a"))
                 (attr "href"
                       (concatenate 'string
                                    (hunchentoot:script-name*)
                                    (format nil "?start=~a&end=~a"
                                            (car ,kind) (cadr ,kind)))))
       (lquery:$ (inline  (concatenate 'string  "div#paginator > ul > li#"
                                       (symbol-name ',kind))) (remove))))

(defun serve-index (&optional category)
  "serve an index page"
  (let ((range (compute-range (hunchentoot:get-parameters* hunchentoot:*request*))))
    (unless range
      (hunchentoot::redirect  (concatenate 'string (hunchentoot:script-name*)
                                           (format nil "?start=0&end=~a" *index-pager*))
                               :code 301))
    (let* ((index (build-index category))
           (index-length (length index))
           (range (truncate-range range index-length))
           (prev  (compute-prev range))
           (next  (compute-next range index-length)))
      (if index
          (progn
            (lquery:$ (initialize (template-path "index"))
                      "h1#page-heading" (text "beatworm.co.uk"))
            (lquery:$ "ul#index-list > li"
                      (replace-with (reduce
                                     (lambda (a b) (concatenate 'string a b))
                                     (mapcar
                                      #'summary
                                      (subseq index (car range) (cadr range))))))
            (if category
                (lquery:$ "title" (text (format nil "Index of ~a" category))))
            (index-paginator next)
            (index-paginator prev)
            (elt (lquery:$ (serialize)) 0))
          (serve-resource-not-found category)))))

(defun handler ()
  (destructuring-bind (&optional category topic)
      (decode-path (hunchentoot:request-uri hunchentoot:*request*))
    (if (not (empty-subject topic))
        (let ((filepath (target-file-path category topic)))
          (cond ((valid-resource filepath) (serve-resource filepath))
                ((publish-resource category topic) (serve-resource filepath))
                (t (serve-resource-not-found (hunchentoot:request-uri hunchentoot:*request*)))))
        (serve-index category))))
