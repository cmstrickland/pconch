;; hack to overload image tags
(defun cl-markdown::output-image (url title text &optional properties)
  (cond ((not (null url))
         (format cl-markdown::*output-stream*
                 "<img src=\"~A\"~@[ title=\"~A\"~]~@[ alt=\"~A\"~]"
                 url title (first (cl-markdown::ensure-list text)))
         (loop for (key . value) in properties do
              (format cl-markdown::*output-stream* " ~a=\"~a\"" key value))
         (write-string "/>" cl-markdown::*output-stream*)
         (setf cl-markdown::*magic-space-p* nil))
        (t
         )))
