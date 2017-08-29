;; hack to overload image tags
(in-package :cl-markdown)
(defun output-image (url title text &optional properties)
  (cond ((not (null url))
         (format *output-stream*
                 "<img src=\"~A\"~@[ title=\"~A\"~]~@[ alt=\"~A\"~]"
                 url title (first (ensure-list text)))
         (loop for (key . value) in properties do
              (format *output-stream* " ~a=\"~a\"" key value))
         (write-string "/>" *output-stream*)
         (setf *magic-space-p* nil))
        (t
         )))
