(in-package :pconch)


(defun decode-path (req-uri)
  (subseq (puri:uri-parsed-path (puri:parse-uri req-uri)) 1 3))

(defun handler ()
  (destructuring-bind (category item) (decode-path (hunchentoot:request-uri hunchentoot:*request*))
    (format nil "Looking up ~a in section ~a" item category)))

