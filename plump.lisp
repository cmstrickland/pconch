;; extend plump to fit quirks

(defclass pconch-iframe (plump:element) ())

(defmethod plump:serialize-object ((node pconch-iframe))
  (format plump:*stream* "<~a" (plump:tag-name node))
  (plump:serialize (plump:attributes node) plump:*stream*)
  (format plump:*stream* "></~a>" (plump:tag-name node)))


(plump:define-tag-dispatcher (iframe-dispatcher plump:*tag-dispatchers*) (name)
    (string-equal name "iframe")
  (format t "constructing an iframe")
  (let ((attrs (plump:read-attributes)))
    (plump:consume-until (plump:make-matcher (is #\<)))
    (plump:consume-until (plump:make-matcher (is #\>)))
    (plump:consume)
    (plump:append-child plump:*root*
                        (make-instance 'pconch-iframe
                                       :parent plump:*root*
                                       :tag-name "iframe"
                                       :attributes attrs))))





