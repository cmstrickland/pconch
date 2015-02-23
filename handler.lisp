(in-package :pconch)


(defun uri-path (uri)
  (cdr (puri:uri-parsed-path (puri:parse-uri uri))))

(defun decode-path (req-uri)
  (let ((prefix-path (uri-path *prefix*))
        (req-path (uri-path req-uri)))
    (subseq (remove-prefix prefix-path req-path) 0 2)))

(defun target-file-path (category topic)
  (merge-pathnames
   (make-pathname  :directory `(:relative ,category) :name topic)
   *www-dir*))

(defun valid-resource (path)
            nil)

(defun serve-resource (path)
  (with-open-file (f path)
    (read f) (read f)))

(defun publish-resource (category topic)
  nil)

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
