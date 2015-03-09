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
                                     (header post :category)))) t)
        )))

(defun publish-resource (category topic)
  (let ((path (source-file-path topic)))
    (with-open-file (f path :if-does-not-exist nil)
      (if f (publish-file f path category topic) nil))))

(defun serve-resource-not-found (url)
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (format nil "No content found for ~a" url))



(defun build-index (&optional category)
  (let ((posts
         (sort
          (mapcar (lambda (f)
                    (with-open-file (p f)
                      (read-post p)))
                  (remove-if-not (lambda (f) (eq (osicat:file-kind f) :regular-file))
                                 (osicat:list-directory *source-dir*)))
          #'string>
          :key (lambda (f) (car (header f :date))))))
    (if category
        (remove-if-not (lambda (f) (on-topic f category)) posts)
        posts)))

(defun serve-index (&optional category)
  (lquery:$ (initialize (template-path "index"))
            "ul#index-list > li" (replace-with
                                  (reduce (lambda (a b) (concatenate 'string a b))
                                          (mapcar #'summary (build-index category)))))
  (elt (lquery:$ (serialize)) 0))

(defun handler ()
  (destructuring-bind (&optional category topic)
      (decode-path (hunchentoot:request-uri hunchentoot:*request*))
    (cond
      ((null category) (serve-index))

      ((and category
            (or (null topic) (equal "" topic))) (serve-index category))

      (t (let ((filepath (target-file-path category topic)))
           (cond ((valid-resource filepath) (serve-resource filepath))
                 ((publish-resource category topic) (serve-resource filepath))
                 (t (serve-resource-not-found (hunchentoot:request-uri hunchentoot:*request*)))))))))
