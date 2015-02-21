(in-package :pconch)


(defun uri-path (uri)
  (cdr (puri:uri-parsed-path (puri:parse-uri uri))))

(defun decode-path (req-uri)
  (let ((prefix-path (uri-path *prefix*))
        (req-path (uri-path req-uri)))
    (subseq (remove-prefix prefix-path req-path) 0 2)))

(defun handler ()
  (destructuring-bind (category item)
      (decode-path (hunchentoot:request-uri hunchentoot:*request*))
    (format nil "Looking up ~a in section ~a" item category)))

