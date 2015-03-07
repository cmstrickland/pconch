(in-package :pconch)


(defun uri-path (uri)
  "returns the path components of a uri string"
  (cdr (puri:uri-parsed-path (puri:parse-uri uri))))

(defun decode-path (req-uri)
  "removes the prefix path components of a uri string. the prefix path
is defined in *prefix*"
  (let ((prefix-path (uri-path *prefix*))
        (req-path (uri-path req-uri)))
    (subseq (remove-prefix prefix-path req-path) 0 2)))


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
        (with-open-file (of (target-file-path category topic)
                            :direction :output
                            :if-exists :supersede)
          (prin1 meta of)
          (print (render post) of) t))))

(defun publish-resource (category topic)
  (let ((path (source-file-path topic)))
    (with-open-file (f path :if-does-not-exist nil)
      (if f (publish-file f path category topic) nil))))

(defun serve-resource-not-found (url)
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (format nil "No content found for ~a" url))

(defun handler ()
  (destructuring-bind (category topic)
      (decode-path (hunchentoot:request-uri hunchentoot:*request*))
    (let ((filepath (target-file-path category topic)))
      (cond ((valid-resource filepath) (serve-resource filepath))
            ((publish-resource category topic) (serve-resource filepath))
            (t (serve-resource-not-found (hunchentoot:request-uri hunchentoot:*request*)))))))
